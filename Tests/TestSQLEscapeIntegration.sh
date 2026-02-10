#!/bin/bash
# Integration test: verify SQL escaping against real databases
set -euo pipefail

PASS=0
FAIL=0
SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"
TMPDIR=$(mktemp -d)

assert_eq() {
  local test_name="$1" expected="$2" actual="$3"
  if [ "$expected" = "$actual" ]; then
    echo "[PASS] $test_name"
    ((PASS++)) || true
  else
    echo "[FAIL] $test_name"
    echo "  Expected: '$expected'"
    echo "  Actual:   '$actual'"
    ((FAIL++)) || true
  fi
}

assert_contains() {
  local test_name="$1" needle="$2" haystack="$3"
  if echo "$haystack" | grep -qi "$needle"; then
    echo "[PASS] $test_name"
    ((PASS++)) || true
  else
    echo "[FAIL] $test_name"
    echo "  Expected to contain: '$needle'"
    echo "  Actual: '$haystack'"
    ((FAIL++)) || true
  fi
}

fb_exec() {
  docker exec -i firebird-test /usr/local/firebird/bin/isql \
    -user SYSDBA -password masterkey localhost:/firebird/data/testdb 2>&1
}

my_file() {
  # Copy SQL file into container and execute it
  docker cp "$1" mysql-test:/tmp/query.sql
  docker exec mysql-test mariadb -uroot -ptestpass testdb -sN -e "source /tmp/query.sql" 2>&1
}

echo "=== Firebird Integration Tests ==="
echo "Waiting for Firebird..."
for i in $(seq 1 30); do
  if echo "SELECT 1 FROM RDB\$DATABASE;" | fb_exec > /dev/null 2>&1; then
    echo "Firebird ready"; break
  fi; sleep 2
done

# Setup
fb_exec << 'SQL'
CREATE TABLE test_escape (id INTEGER, val VARCHAR(200));
COMMIT;
SQL

# FB1: SQL-standard '' escaping
fb_exec << 'SQL'
INSERT INTO test_escape VALUES (1, 'O''Brien');
COMMIT;
SQL
result=$(echo "SELECT val FROM test_escape WHERE id=1;" | fb_exec | tr -s ' ' | grep "O'" | sed "s/^ *//;s/ *$//")
assert_eq "Firebird: '' escaping produces O'Brien" "O'Brien" "$result"

# FB2: Backslash literal (no escaping needed)
fb_exec << 'SQL'
INSERT INTO test_escape VALUES (2, 'C:\Users');
COMMIT;
SQL
result=$(echo "SELECT val FROM test_escape WHERE id=2;" | fb_exec | tr -s ' ' | grep 'C:' | sed "s/^ *//;s/ *$//")
assert_eq "Firebird: backslash needs no escaping" 'C:\Users' "$result"

# FB3: MySQL-style \' must fail on Firebird
fb3_result=$(echo "INSERT INTO test_escape VALUES (99, 'test\\'value'); COMMIT;" | fb_exec 2>&1 || true)
assert_contains "Firebird: backslash-quote rejected" "error\|statement\|unexpected" "$fb3_result"

echo ""
echo "=== MariaDB Integration Tests ==="
echo "Waiting for MariaDB..."
for i in $(seq 1 30); do
  if docker exec mysql-test mariadb -uroot -ptestpass testdb -e "SELECT 1;" > /dev/null 2>&1; then
    echo "MariaDB ready"; break
  fi; sleep 2
done

# Setup MariaDB
docker exec mysql-test mariadb -uroot -ptestpass testdb -e "CREATE TABLE test_escape (id INT, val VARCHAR(200));"

# Use SQL files to avoid shell escaping issues
# MY1: \' works for quote escaping
cat > "$TMPDIR/my1.sql" << 'SQLEOF'
INSERT INTO test_escape VALUES (1, 'O\'Brien');
SQLEOF
my_file "$TMPDIR/my1.sql"
result=$(docker exec mysql-test mariadb -uroot -ptestpass testdb -sN -e "SELECT val FROM test_escape WHERE id=1;")
assert_eq "MySQL: backslash-quote produces O'Brien" "O'Brien" "$result"

# MY2: \\ needed for literal backslash (compare via HEX to avoid shell escaping)
cat > "$TMPDIR/my2.sql" << 'SQLEOF'
INSERT INTO test_escape VALUES (2, 'C:\\Users');
SQLEOF
my_file "$TMPDIR/my2.sql"
result=$(docker exec mysql-test mariadb -uroot -ptestpass testdb -sN -e "SELECT HEX(val) FROM test_escape WHERE id=2;")
# C:\Users in hex = 433A5C5573657273
assert_eq "MySQL: double-backslash produces C:\\Users" "433A5C5573657273" "$result"

# MY3: Unescaped backslash is swallowed
cat > "$TMPDIR/my3.sql" << 'SQLEOF'
INSERT INTO test_escape VALUES (3, 'C:\Users');
SQLEOF
my_file "$TMPDIR/my3.sql"
result=$(docker exec mysql-test mariadb -uroot -ptestpass testdb -sN -e "SELECT val FROM test_escape WHERE id=3;")
assert_eq "MySQL: unescaped backslash swallowed → C:Users" "C:Users" "$result"

# MY4: The OLD bug — double escaping \\' produces wrong data
# Old code did: O'Brien → O\'Brien → O\\'Brien
# MySQL reads O\\' as: O\ (literal backslash) + ' (end of string) + Brien (syntax error)
cat > "$TMPDIR/my4.sql" << 'SQLEOF'
INSERT INTO test_escape VALUES (4, 'O\\'Brien');
SQLEOF
my4_result=$(my_file "$TMPDIR/my4.sql" 2>&1 || true)
assert_contains "MySQL: double-escaped quote causes error (the bug)" "error\|syntax" "$my4_result"

echo ""
echo "=== SQL Server: String Escaping ==="
echo "Waiting for SQL Server..."
SQLCMD="docker exec mssql-test /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P TestPass123! -C -d tempdb"
for i in $(seq 1 30); do
  if $SQLCMD -Q "SELECT 1" > /dev/null 2>&1; then
    echo "SQL Server ready"; break
  fi; sleep 2
done

$SQLCMD -Q "CREATE TABLE test_escape (id INT, val VARCHAR(200));" > /dev/null

# MS1: '' escaping works (SQL standard)
$SQLCMD -Q "INSERT INTO test_escape VALUES (1, 'O''Brien');" > /dev/null
result=$($SQLCMD -h -1 -Q "SET NOCOUNT ON; SELECT val FROM test_escape WHERE id=1;" | sed 's/ *$//' | head -1)
assert_eq "SQL Server: '' escaping produces O'Brien" "O'Brien" "$result"

# MS2: Backslash is literal (no escaping needed)
$SQLCMD -Q "INSERT INTO test_escape VALUES (2, 'C:\Users');" > /dev/null
result=$($SQLCMD -h -1 -Q "SET NOCOUNT ON; SELECT val FROM test_escape WHERE id=2;" | sed 's/ *$//' | head -1)
assert_eq "SQL Server: backslash is literal (no escaping)" "C:\Users" "$result"

# MS3: \' does NOT work on SQL Server (not MySQL)
ms3_result=$($SQLCMD -Q "INSERT INTO test_escape VALUES (3, 'O\'Brien');" 2>&1 || true)
assert_contains "SQL Server: backslash-quote rejected" "syntax\|error\|Msg\|Unclosed" "$ms3_result"

echo ""
echo "=== Results: $((PASS + FAIL)) tests, $PASS passed, $FAIL failed ==="
rm -rf "$TMPDIR"

[ "$FAIL" -eq 0 ]
