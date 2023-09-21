unit form_Pen;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, ChartWriter;

type
  TformPen = class(TForm)
    eWidth: TEdit;
    cbStyle: TComboBox;
    cbColor: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    bOK: TButton;
    bCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    CW : TChartWriter;
  end;

var
  formPen: TformPen;

implementation

{$R *.dfm}

end.
