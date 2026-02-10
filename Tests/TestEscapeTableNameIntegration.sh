#!/bin/bash
# Integration test: verify table name escaping against real databases
set -euo pipefail

PASS=0
FAIL=0
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
  docker cp "$1" mysql-test:/tmp/query.sql
  docker exec mysql-test mariadb -uroot -ptestpass testdb -sN -e "source /tmp/query.sql" 2>&1
}

echo "=== Firebird: Table Name Escaping ==="
echo "Waiting for Firebird..."
for i in $(seq 1 30); do
  if echo "SELECT 1 FROM RDB\$DATABASE;" | fb_exec > /dev/null 2>&1; then
    echo "Firebird ready"; break
  fi; sleep 2
done

# FB1: Double-quoted identifier works
fb_exec << 'SQL'
CREATE TABLE "My Table" (id INTEGER);
INSERT INTO "My Table" VALUES (1);
COMMIT;
SQL
result=$(echo 'SELECT id FROM "My Table";' | fb_exec | tr -s ' ' | grep "1" | sed "s/^ *//;s/ *$//" | head -1)
assert_eq "Firebird: double-quoted table name with space works" "1" "$result"

# FB2: Double-quote inside identifier escaped with ""
fb_exec << 'SQL'
CREATE TABLE "My""Table" (id INTEGER);
INSERT INTO "My""Table" VALUES (2);
COMMIT;
SQL
result=$(echo 'SELECT id FROM "My""Table";' | fb_exec | tr -s ' ' | grep "2" | sed "s/^ *//;s/ *$//" | head -1)
assert_eq "Firebird: escaped double-quote in table name works" "2" "$result"

# FB3: Unquoted table name with special chars must fail
fb3_result=$(echo 'CREATE TABLE My Table (id INTEGER);' | fb_exec 2>&1 || true)
assert_contains "Firebird: unquoted name with space rejected" "error\|token\|unknown" "$fb3_result"

echo ""
echo "=== MariaDB: Table Name Escaping ==="
echo "Waiting for MariaDB..."
for i in $(seq 1 30); do
  if docker exec mysql-test mariadb -uroot -ptestpass testdb -e "SELECT 1;" > /dev/null 2>&1; then
    echo "MariaDB ready"; break
  fi; sleep 2
done

# MY1: Backtick-quoted identifier works
cat > "$TMPDIR/my1.sql" << 'SQLEOF'
CREATE TABLE `My Table` (id INT);
INSERT INTO `My Table` VALUES (1);
SQLEOF
my_file "$TMPDIR/my1.sql"
result=$(docker exec mysql-test mariadb -uroot -ptestpass testdb -sN -e "SELECT id FROM \`My Table\`;")
assert_eq "MySQL: backtick-quoted table name with space works" "1" "$result"

# MY2: Backtick inside identifier escaped with ``
cat > "$TMPDIR/my2.sql" << 'SQLEOF'
CREATE TABLE `My``Table` (id INT);
INSERT INTO `My``Table` VALUES (2);
SQLEOF
my_file "$TMPDIR/my2.sql"
cat > "$TMPDIR/my2q.sql" << 'SQLEOF'
SELECT id FROM `My``Table`;
SQLEOF
result=$(my_file "$TMPDIR/my2q.sql")
assert_eq "MySQL: escaped backtick in table name works" "2" "$result"

# MY3: Unquoted table name with space must fail
my3_result=$(docker exec mysql-test mariadb -uroot -ptestpass testdb -e "CREATE TABLE My Table (id INT);" 2>&1 || true)
assert_contains "MySQL: unquoted name with space rejected" "error\|ERROR" "$my3_result"

echo ""
echo "=== SQL Server: Table Name Escaping ==="
echo "Waiting for SQL Server..."
SQLCMD="docker exec mssql-test /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P TestPass123! -C -d tempdb"
for i in $(seq 1 30); do
  if $SQLCMD -Q "SELECT 1" > /dev/null 2>&1; then
    echo "SQL Server ready"; break
  fi; sleep 2
done

# MS1: Bracket-quoted identifier with space
$SQLCMD -Q "CREATE TABLE [My Table] (id INT); INSERT INTO [My Table] VALUES (1);" > /dev/null
result=$($SQLCMD -h -1 -Q "SET NOCOUNT ON; SELECT id FROM [My Table];" | tr -d ' ' | head -1)
assert_eq "SQL Server: bracket-quoted table name with space works" "1" "$result"

# MS2: ] escaped with ]]
$SQLCMD -Q "CREATE TABLE [My]]Table] (id INT); INSERT INTO [My]]Table] VALUES (2);" > /dev/null
result=$($SQLCMD -h -1 -Q "SET NOCOUNT ON; SELECT id FROM [My]]Table];" | tr -d ' ' | head -1)
assert_eq "SQL Server: escaped ] in table name works" "2" "$result"

# MS3: [ inside brackets needs NO escaping (only ] must be escaped)
$SQLCMD -Q "CREATE TABLE [My[Table] (id INT); INSERT INTO [My[Table] VALUES (3);" > /dev/null
result=$($SQLCMD -h -1 -Q "SET NOCOUNT ON; SELECT id FROM [My[Table];" | tr -d ' ' | head -1)
assert_eq "SQL Server: [ needs no escaping inside brackets" "3" "$result"

# MS4: Unquoted name with space must fail
ms4_result=$($SQLCMD -Q "CREATE TABLE My Table (id INT);" 2>&1 || true)
assert_contains "SQL Server: unquoted name with space rejected" "syntax\|error\|Msg" "$ms4_result"

echo ""
echo "=== Results: $((PASS + FAIL)) tests, $PASS passed, $FAIL failed ==="
rm -rf "$TMPDIR"

[ "$FAIL" -eq 0 ]
