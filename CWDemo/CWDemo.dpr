program CWDemo;

uses
  Vcl.Forms,
  frmMain in 'frmMain.pas' {form_main},
  frame_1 in 'frame_1.pas' {Frame1: TFrame},
  form_Pen in 'form_Pen.pas' {formPen},
  form_Sections in 'form_Sections.pas' {formSections},
  form_data in 'form_data.pas' {frmData},
  form_New in 'form_New.pas' {frm_New};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.HelpFile := '..\Help\ChartWriter.chm';
  Application.Title := 'CWDemo';
  Application.CreateForm(Tform_main, form_main);
  Application.CreateForm(TformPen, formPen);
  Application.CreateForm(TformSections, formSections);
  Application.CreateForm(TfrmData, frmData);
  Application.CreateForm(Tfrm_New, frm_New);
  Application.Run;
end.
