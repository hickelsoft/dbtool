unit HS_Mitteilung;

interface

type
  THsMitteilung = record
  public
    class procedure SendeStacktraceAnHickelSOFT(StacktraceFile: string); static;
  end;

implementation

{ THsMitteilung }

class procedure THsMitteilung.SendeStacktraceAnHickelSOFT(StacktraceFile: string);
begin
  // This method is reserved for a future version of DBTool.
  // Note that in case this gets implemented, no stacktraces would be send
  // unless the user gave their explicit permission!!!
end;

end.
