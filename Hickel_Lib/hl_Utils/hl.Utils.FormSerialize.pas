unit hl.Utils.FormSerialize;

interface

uses
  IniFiles, StdCtrls, ExtCtrls, SysUtils, Classes, wwDbDateTimePicker, ComCtrls;

type
  ThlFormSerialization = record
    class function SaveComponent(Component: TComponent): string; static;
    class procedure LoadComponent(Component: TComponent; const data: string); static;
  end;

implementation

uses
  MetaObjVcl(*, wwdblook, DBCtrls, wwdbedit*), Mask, Spin;

{$REGION 'Helferfunktionen'}
function _EncodeStr(S: string): string;
begin
  result := s;
  result := StringReplace(result, '\', '\\', [rfReplaceAll]);
  result := StringReplace(result, #13, '\r', [rfReplaceAll]);
  result := StringReplace(result, #10, '\n', [rfReplaceAll]);
end;

function _DecodeStr(S: string): string;
begin
  result := s;
  result := StringReplace(result, '\r', #13, [rfReplaceAll]);
  result := StringReplace(result, '\n', #10, [rfReplaceAll]);
  result := StringReplace(result, '\\', '\', [rfReplaceAll]);
end;

function iniToStr(ini: TMemIniFile): string;
var
  sl: TStrings;
begin
  sl := TStringList.Create;
  try
    ini.GetStrings(sl);
    result := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

procedure StrToIni(ini: TMemIniFile; const data: string);
var
  sl: TStrings;
begin
  sl := TStringList.Create;
  try
    sl.Text := data;
    ini.SetStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;
{$ENDREGION}

class function ThlFormSerialization.SaveComponent(Component: TComponent): string;
var
  i: Integer;
  ini: TMemIniFile;
  c: TComponent;
begin
  ini := TMemIniFile.Create('');

  try
    for i := Component.ComponentCount - 1 downto 0 do
    begin
      c := Component.Components[i];
      if c is TwwDBCustomDateTimePicker then
      begin
        ini.WriteDateTime('FormUserInput', c.Name+'.DateTime', TwwDBCustomDateTimePicker(c).DateTime);
      end
      else if c is TDateTimePicker then
      begin
        ini.WriteDateTime('FormUserInput', c.Name+'.DateTime', TDateTimePicker(c).DateTime);
      end
      // Ticket 34864
      (*
      else if (c is TCustomEdit) and not (c is TwwDBCustomLookupCombo)
                                 and not (c is TwwDBEdit)
                                 and not (c is TDBLookupCombo)
                                 and not (c is TDBEdit) then
      *)
      else if (c is TEdit) or (c is TMemo) or (c is TMaskEdit) or (c is TSpinEdit) then
      begin
        ini.WriteString('FormUserInput', c.Name+'.Text', _EncodeStr(TCustomEdit(c).Text));
      end
      else if c is TCheckBox then
      begin
        ini.WriteBool('FormUserInput', c.Name+'.Checked', TCheckBox(c).Checked);
      end
      else if c is TRadioGroup then
      begin
        ini.WriteInteger('FormUserInput', c.Name+'.ItemIndex', TRadioGroup(c).ItemIndex);
      end
      else if c is TRadioButton then
      begin
        ini.WriteBool('FormUserInput', c.Name+'.Checked', TRadioButton(c).Checked);
      end
      else if c is TSerializationMetaObj then
      begin
        if Assigned(TSerializationMetaObj(c).OnBeforeSave) then TSerializationMetaObj(c).OnBeforeSave(c);
        ini.WriteString('FormUserInput', c.Name+'.Data', TSerializationMetaObj(c).Data);
        if Assigned(TSerializationMetaObj(c).OnAfterSave) then TSerializationMetaObj(c).OnAfterSave(c);
      end;
    end;

    result := iniToStr(ini);
  finally
    FreeAndNil(ini);
  end;
end;

class procedure ThlFormSerialization.LoadComponent(Component: TComponent; const data: string);
var
  ini: TMemIniFile;
  i: Integer;
  c: TComponent;
begin
  if data = '' then exit; // INI existiert nicht... Daher Form-Defaultwerte lassen ("Erster Programmstart")

  ini := TMemIniFile.Create('');
  try
    StrToIni(ini, data);

    for i := Component.ComponentCount - 1 downto 0 do
    begin
      c := Component.Components[i];
      if c is TwwDBCustomDateTimePicker then
      begin
        // TODO: Hier gibt es ein Problem. Irgendwie speichert er in die Datei '::'  
        TwwDBCustomDateTimePicker(c).DateTime := ini.ReadDateTime('FormUserInput', c.Name+'.DateTime', 0);
      end
      else if c is TDateTimePicker then
      begin
        TDateTimePicker(c).DateTime := ini.ReadDateTime('FormUserInput', c.Name+'.DateTime', 0);
      end
      // Ticket 34864
      (*
      else if (c is TCustomEdit) and not (c is TwwDBCustomLookupCombo)
                                 and not (c is TwwDBEdit)
                                 and not (c is TDBLookupCombo)
                                 and not (c is TDBEdit) then
      *)
      else if (c is TEdit) or (c is TMemo) or (c is TMaskEdit) or (c is TSpinEdit) then
      begin
        TCustomEdit(c).Text := _DecodeStr(ini.ReadString('FormUserInput', c.Name+'.Text', ''));
      end
      else if c is TCheckBox then
      begin
        TCheckBox(c).Checked := ini.ReadBool('FormUserInput', c.Name+'.Checked', false);
      end
      else if c is TRadioGroup then
      begin
        TRadioGroup(c).ItemIndex := ini.ReadInteger('FormUserInput', c.Name+'.ItemIndex', 0);
      end
      else if c is TRadioButton then
      begin
        TRadioButton(c).Checked := ini.ReadBool('FormUserInput', c.Name+'.Checked', false);
      end
      else if c is TSerializationMetaObj then
      begin
        if Assigned(TSerializationMetaObj(c).OnBeforeLoad) then TSerializationMetaObj(c).OnBeforeLoad(c);
        TSerializationMetaObj(c).Data := ini.ReadString('FormUserInput', c.Name+'.Data', '');
        if Assigned(TSerializationMetaObj(c).OnAfterLoad) then TSerializationMetaObj(c).OnAfterLoad(c);
      end;
    end;
  finally
    FreeAndNil(ini);
  end;
end;

end.

