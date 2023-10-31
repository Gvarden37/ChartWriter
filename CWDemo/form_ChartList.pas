unit form_ChartList;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmMain, Vcl.StdCtrls, Vcl.Buttons;

type
  Tfrm_ChartList = class(TForm)
    lbCharts: TListBox;
    BitBtn1: TBitBtn;
    procedure lbChartsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_ChartList: Tfrm_ChartList;

implementation

{$R *.dfm}

procedure Tfrm_ChartList.FormCreate(Sender: TObject);
begin
  //form_main.CW.LoadFromFile('C:\Users\47994\Documents\Embarcadero\Studio\Projects\CWDemo\Graphs\CO2Graph.CWR');
  form_main.CW.LoadFromFile('CO2Graph.CWR');
end;

procedure Tfrm_ChartList.lbChartsClick(Sender: TObject);
begin
    form_Main.CW.ChartList.ItemIndex := lbCharts.ItemIndex;
   if form_Main.CW.ChartList.ItemIndex <> -1 then
     form_Main.OnLoadChart;
end;

end.
