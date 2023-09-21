unit form_Sections;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  System.Actions, Vcl.ActnList, Vcl.StdActns, ChartWriter;

type
  TformSections = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    ActionList1: TActionList;
    acSectionFont: TFontEdit;
    SpeedButton1: TSpeedButton;
    lb_Font: TLabel;
    SpeedButton2: TSpeedButton;
    cbCaptionLayout: TComboBox;
    Label2: TLabel;
    eHorizMargin: TEdit;
    eVertMargin: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    cbxShowLines: TCheckBox;
    cbxProlongedLines: TCheckBox;
    procedure acSectionFontBeforeExecute(Sender: TObject);
    procedure acSectionFontAccept(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure cbCaptionLayoutChange(Sender: TObject);
    procedure eHorizMarginExit(Sender: TObject);
    procedure eVertMarginExit(Sender: TObject);
    procedure cbxShowLinesClick(Sender: TObject);
    procedure cbxProlongedLinesClick(Sender: TObject);
  private
    {Intermediate storage}
    FPen : TPen;
    FFont : TFont;
    FCaptionLayout : TCaptionLayout;
    FHorizMargin, FVertMargin : integer;
    FShowLines : Boolean;
    FProlongedLines : Boolean;
  public
    { Public declarations }
    SectionKind : TCWSectionDefs;
  end;

var
  formSections: TformSections;

implementation
uses frmMain;

{$R *.dfm}

procedure TformSections.acSectionFontAccept(Sender: TObject);
begin
  FFont.Assign(acSectionFont.Dialog.Font);
  lb_Font.Caption := FFont.Name + ' ' + IntToStr(FFont.Size);
  lb_Font.Font.Color := FFont.Color;
end;

procedure TformSections.acSectionFontBeforeExecute(Sender: TObject);
begin
   acSectionFont.Dialog.Font.Assign(FFont);
end;

procedure TformSections.cbCaptionLayoutChange(Sender: TObject);
begin
   FCaptionLayout := TCaptionLayout(cbCaptionLayout.ItemIndex);
end;

procedure TformSections.cbxProlongedLinesClick(Sender: TObject);
begin
  FProlongedLines := cbxProlongedLines.Checked;
end;

procedure TformSections.cbxShowLinesClick(Sender: TObject);
begin
  FShowLines := cbxShowLines.Checked;
end;

procedure TformSections.eHorizMarginExit(Sender: TObject);
begin
   FHorizMargin := StrToInt(eHorizMargin.Text);
end;

procedure TformSections.eVertMarginExit(Sender: TObject);
begin
  FVertMargin := StrToInt(eVertMargin.Text);
end;

procedure TformSections.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    if Modalresult = mrOk then
    begin
      form_main.CW.BeginUpdate;
      SectionKind.Font.Assign(FFont);
      SectionKind.Pen.Assign(FPen);
      SectionKind.CaptionLayout := FCaptionLayout;
      SectionKind.CaptionHorizMargin := FHorizMargin;
      SectionKind.CaptionVertMargin := FVertMargin;
      SectionKind.ShowLines := FShowLines;
      SectionKind.ProlongedLines := FProlongedLines;
      form_main.CW.EndUpdate;
    end;
end;

procedure TformSections.FormCreate(Sender: TObject);
begin
  FPen := TPen.Create;
  FFont := TFont.Create;
end;

procedure TformSections.FormDestroy(Sender: TObject);
begin
  FPen.Free;
  FFont.Free;
end;

procedure TformSections.FormShow(Sender: TObject);
begin
  FPen.Assign(SectionKind.Pen);
  FFont.Assign(SectionKind.Font);
  FCaptionLayout := SectionKind.CaptionLayout;
  FHorizMargin := SectionKind.CaptionHorizMargin;
  FVertMargin := SectionKind.CaptionVertMargin;
  FShowLines := SectionKind.ShowLines;
  cbCaptionLayout.ItemIndex := Ord(FCaptionLayout);
  eHorizMargin.Text := IntToStr(FHorizMargin);
  eVertMargin.Text := IntToStr(FVertMargin);
  cbxShowLines.Checked := FShowLines;

end;

procedure TformSections.SpeedButton2Click(Sender: TObject);
begin
   form_main.SetPenProps(FPen);
end;

end.
