unit EditRTF;

interface

uses
  Windows, Classes, Controls, StdCtrls, Forms, Db, ComCtrls, DBCtrls, ExtCtrls,
  ToolWin, ActnList, ImgList, DBActns, ExtActns, StdActns, System.Actions,
  System.ImageList;

type
  TDLG_EditRTF = class(TForm)
    DBRichEdit1: TDBRichEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    RichEditBold1: TRichEditBold;
    RichEditItalic1: TRichEditItalic;
    RichEditUnderline1: TRichEditUnderline;
    RichEditBullets1: TRichEditBullets;
    RichEditAlignLeft1: TRichEditAlignLeft;
    RichEditAlignRight1: TRichEditAlignRight;
    RichEditAlignCenter1: TRichEditAlignCenter;
    SearchFind1: TSearchFind;
    SearchFindNext1: TSearchFindNext;
    SearchReplace1: TSearchReplace;
    DataSetPost1: TDataSetPost;
    DataSetCancel1: TDataSetCancel;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton16: TToolButton;
    procedure FormKeyPress(Sender: TObject; var Key: char);
  end;
  
var
  DLG_EditRTF: TDLG_EditRTF;

implementation

{$R *.DFM}

procedure TDLG_EditRTF.FormKeyPress(Sender: TObject; var Key: char);
begin
  if ord(Key) = VK_ESCAPE then Close;
end;

end.
