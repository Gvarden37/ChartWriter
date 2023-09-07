unit frame_1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Form_Pen,
  Vcl.Buttons, System.Actions, Vcl.ActnList, Vcl.StdActns, System.ImageList,
  System.UITypes, Vcl.ImgList;

type
  TFrame1 = class(TFrame)
    Label103: TLabel;
    sbLabelFont: TSpeedButton;
    sbDivLines: TSpeedButton;
    Label105: TLabel;
    cbxShowDividerLines: TCheckBox;
    lbDivLinePen: TLabel;
    lbFont: TLabel;
    cbxShowLabels: TCheckBox;
    ActionList1: TActionList;
    acFontDialog: TFontEdit;
    Label1: TLabel;
    eQualifier: TEdit;
    procedure cbxShowLabelsClick(Sender: TObject);
    procedure cbxShowDividerLinesClick(Sender: TObject);
    procedure acFontDialogBeforeExecute(Sender: TObject);
    procedure acFontDialogAccept(Sender: TObject);
    procedure sbDivLinesClick(Sender: TObject);
    procedure eQualifierExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillControls;
  end;
implementation
uses frmMain, ChartWriter;

{$R *.dfm}

procedure TFrame1.cbxShowDividerLinesClick(Sender: TObject);
var
  X : Boolean;
begin
  if form_main.CW.Chart = nil then
    Exit;
  X := cbxShowDividerLines.Checked;
  if Name = 'Names' then
    form_main.CW.NameScale.ShowDividerLines := X
  else if Name = 'V1' then
    form_main.CW.ValueScale1.ShowDividerLines := X
  else
    form_main.CW.ValueScale2.ShowDividerLines := X;
end;

procedure TFrame1.cbxShowLabelsClick(Sender: TObject);
var
  X : Boolean;
begin
  if form_main.CW.Chart = nil then
    Exit;
  X := cbxShowLabels.Checked;
  if Name = 'Names' then
    form_main.CW.NameScale.ShowLabels := X
  else if Name = 'V1' then
    form_main.CW.ValueScale1.ShowLabels := X
  else
    form_main.CW.ValueScale2.ShowLabels := X;
end;

procedure TFrame1.eQualifierExit(Sender: TObject);
begin
  if form_main.CW.Chart = nil then
    Exit;
  if Name = 'Names' then
    form_main.CW.NameScale.Qualifier := eQualifier.Text
  else if Name = 'V1' then
    form_main.CW.ValueScale1.Qualifier := eQualifier.Text
  else
    form_main.CW.ValueScale2.Qualifier := eQualifier.Text;
end;

procedure TFrame1.sbDivLinesClick(Sender: TObject);
begin
  if form_main.CW.Chart = nil then
    Exit;
  if Name = 'Names' then
    form_main.SetPenProps(form_main.CW.NameScale.Pen)
  else if Name = 'V1' then
    form_main.SetPenProps(form_main.CW.ValueScale1.Pen)
  else
    form_main.SetPenProps(form_main.CW.ValueScale2.Pen);
  FillControls;
end;

procedure TFrame1.acFontDialogAccept(Sender: TObject);
var
  F : TFont;
begin
  if form_main.CW.Chart = nil then
    Exit;
  F := acFontDialog.Dialog.Font;
  if Name = 'Names' then
    form_main.CW.NameScale.Font.Assign(F)
  else if Name = 'V1' then
    form_main.CW.ValueScale1.Font.Assign(F)
  else
    form_main.CW.ValueScale2.Font.Assign(F);
  FillControls;
end;

procedure TFrame1.acFontDialogBeforeExecute(Sender: TObject);
var
  F : TFont;
begin
  if form_main.CW.Chart = nil then
    Exit;
  if Name = 'Names' then
    F := form_main.CW.NameScale.Font
  else if Name = 'V1' then
    F := form_main.CW.ValueScale1.Font
  else
    F := form_main.CW.ValueScale2.Font;
  acFontDialog.Dialog.Font.Assign(F);
end;

procedure TFrame1.FillControls;
var
  E : TNotifyEvent;
  S : TCWScale;
  St : string;
begin
  if Name = 'Names' then
    S := form_main.CW.NameScale
  else if Name = 'V1' then
    S := form_main.CW.ValueScale1
  else
    S := form_main.CW.ValueScale2;

  eQualifier.Text := S.Qualifier;

  E := form_main.DisableCBXEvent(cbxShowLabels);
  cbxShowLabels.Checked := S.ShowLabels;
  cbxShowLabels.OnClick := E;

  E := form_main.DisableCBXEvent(cbxShowDividerLines);
  cbxShowDividerLines.Checked := S.ShowDividerLines;
  cbxShowDividerLines.OnClick := E;

  lbFont.Font.Color := S.Font.Color;
  lbFont.Caption := S.Font.Name + ' ' +IntToStr(S.Font.Size);

  lbDivLinePen.Font.Color := S.Pen.Color;
  if S.Pen.Style = psSolid then
    St := St + 'Solid'
  else if S.Pen.Style = psDot then
    St := St + 'Dot'
  else if S.Pen.Style = psDash then
    St := St + 'Dash'
  else
    St := St + 'Solid';
  St := St +  ' ' + IntToStr(S.Pen.Width);
  lbDivLinePen.Caption := St;
end;

end.
