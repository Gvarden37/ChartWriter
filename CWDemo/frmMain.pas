unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UiTypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ChartWriter, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, System.Actions, Vcl.ActnList, Vcl.StdActns, System.ImageList,
  Vcl.ImgList, Vcl.ToolWin, frame_1, system.generics.collections, Vcl.CheckLst,
  Vcl.Buttons, form_pen, form_Sections, form_data, form_new, Vcl.Menus,
  Vcl.Samples.Spin, VCl.HtmlHelpViewer, Vcl.ExtDlgs;


type
  TForm_Main = class;

  TGraphControls = class
   private
    FGraph : TCWGraph;
    Frm : TForm_Main;
    procedure DoFill; virtual; abstract;
    procedure Update(Ctrl : TObject); virtual; abstract;
   public
     constructor Create(AGraph : TCWGraph; AForm : TForm_main);
     property Graph : TCWGraph read FGraph write FGraph;
  end;

  TCurveControls = class(TGraphControls)
    private
      FStyleIndex : integer;
      procedure DoFill; override;
      procedure Update(Ctrl : TObject); override;
      procedure SyncSeriesStyles;
  end;

  TBarControls = class(TGraphControls)
    private
      procedure DoFill; override;
      procedure Update(Ctrl : TObject); override;
      procedure ApplyAnimations;
  end;

  TPieControls = class(TGraphControls)
    private
      procedure DoFill; override;
      procedure Update(Ctrl : TObject); override;
  end;



  Tform_main = class(TForm)
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    ImageList1: TImageList;
    FileOpen1: TFileOpen;
    ToolButton1: TToolButton;
    panChart: TPanel;
    panGraph: TPanel;
    CW: TChartWriter;
    pgGraphs: TPageControl;
    Curve: TTabSheet;
    gbCurveStyles: TGroupBox;
    lbStyle: TLabel;
    lbLineStyle: TLabel;
    lbLineWidth: TLabel;
    cbCurveStyle: TComboBox;
    cbLineStyle: TComboBox;
    eLineWidth: TEdit;
    Bar: TTabSheet;
    Pie: TTabSheet;
    pgChart: TPageControl;
    tabChartDefs: TTabSheet;
    pgChartDefs: TPageControl;
    tabChartGeneral: TTabSheet;
    Label16: TLabel;
    eChartTitle: TEdit;
    tabSeries: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    cbChartSeriesColor: TColorBox;
    eSeriesTitle: TEdit;
    cbxChartVisible: TCheckBox;
    lbSeries: TListBox;
    tabItems: TTabSheet;
    lbItems: TListBox;
    tabAxisAttributes: TTabSheet;
    Label15: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    cbAxisOrientation: TComboBox;
    cbAxisColor: TColorBox;
    lbxTextTilting: TCheckListBox;
    cbxGradientWall: TCheckBox;
    eWallWidth: TEdit;
    cbWallColor: TColorBox;
    tabLegends: TTabSheet;
    Label8: TLabel;
    Label9: TLabel;
    cbChartItemColor: TColorBox;
    eItemName: TEdit;
    tabScales: TTabSheet;
    pgScales: TPageControl;
    tabValueScale: TTabSheet;
    tabNameScale: TTabSheet;
    Label14: TLabel;
    lbNamePrecision: TLabel;
    Label22: TLabel;
    cbSpanType: TComboBox;
    eNamePrecision: TEdit;
    cbOverflowAction: TComboBox;
    Label2: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    eHighValue: TEdit;
    eLowValue: TEdit;
    cbxAutoSpan: TCheckBox;
    eValueIntervals: TEdit;
    tabSections: TTabSheet;
    pgSections: TPageControl;
    tabNameSections: TTabSheet;
    tabValueSections: TTabSheet;
    panNameSectionValues: TPanel;
    lbNameSections: TListBox;
    eNStartValue: TEdit;
    eNEndValue: TEdit;
    eNLongCaption: TEdit;
    eNShortCaption: TEdit;
    Label3: TLabel;
    Label5: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    cbAutoSections: TComboBox;
    Label4: TLabel;
    cbTimeTemplate: TComboBox;
    Label26: TLabel;
    Panel1: TPanel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    lbValueSections: TListBox;
    eVStartValue: TEdit;
    eVEndValue: TEdit;
    eVLongCaption: TEdit;
    eVShortCaption: TEdit;
    Label32: TLabel;
    lbLegends: TListBox;
    cbLegAnchoring: TComboBox;
    Label33: TLabel;
    lbxLegContent: TCheckListBox;
    Label34: TLabel;
    cbLegContentFlow: TComboBox;
    Label35: TLabel;
    memLegText: TMemo;
    Label36: TLabel;
    cbxLegVisible: TCheckBox;
    panLegends: TPanel;
    eLegHorizMargins: TEdit;
    Label23: TLabel;
    eLegVertMargins: TEdit;
    Label37: TLabel;
    cbLegAlignment: TComboBox;
    Label38: TLabel;
    cbLegBullets: TComboBox;
    Label39: TLabel;
    tabValueScale2: TTabSheet;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    eHighValue2: TEdit;
    eLowValue2: TEdit;
    cbxAutoSpan2: TCheckBox;
    eValueIntervals2: TEdit;
    cbScale: TComboBox;
    Label44: TLabel;
    Label45: TLabel;
    ePrecision1: TEdit;
    Label46: TLabel;
    ePrecision2: TEdit;
    Label12: TLabel;
    eValuePrecision: TEdit;
    cbxCurveAnimation: TCheckBox;
    cbCurveAnimationSpeed: TComboBox;
    lbCurveAnimationSpeed: TLabel;
    eCurveMinPointSpacing: TEdit;
    eCurveMaxPointSpacing: TEdit;
    Label50: TLabel;
    Label51: TLabel;
    cbxCurveBeaconPoints: TCheckBox;
    cbCurvePointMarkers: TComboBox;
    Label53: TLabel;
    sbFont: TSpeedButton;
    acCurveFont: TFontEdit;
    lbCurveFontName: TLabel;
    eCurveAnimationPause: TEdit;
    Label56: TLabel;
    Label57: TLabel;
    GroupBox1: TGroupBox;
    Label49: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    eCurveBaseLine: TEdit;
    cbNeighborAreaColor: TColorBox;
    cbxAreaOutLine: TCheckBox;
    cbAreaOutLineColor: TColorBox;
    sbApplyOnAll: TSpeedButton;
    acApply: TAction;
    lbApplyOn: TListBox;
    cbBarStyle: TComboBox;
    Label58: TLabel;
    eBarWidth: TEdit;
    Label59: TLabel;
    cbLayout: TComboBox;
    Label61: TLabel;
    eItemSpacing: TEdit;
    eSeriesSpacing: TEdit;
    Label62: TLabel;
    Label63: TLabel;
    eCubeAngle: TEdit;
    eCubeDepth: TEdit;
    Label67: TLabel;
    Label68: TLabel;
    cbxHorizontalBar: TCheckBox;
    GroupBox2: TGroupBox;
    lbxBarOptions: TCheckListBox;
    lbxBarTexts: TCheckListBox;
    Label65: TLabel;
    Label66: TLabel;
    eBarBaselineValue: TEdit;
    gbBarAnimation: TGroupBox;
    lbBarAnimationSpeed: TLabel;
    Label70: TLabel;
    cbBarAnimationSpeed: TComboBox;
    eBarAnimationPause: TEdit;
    lbxBarAnimations: TCheckListBox;
    Label72: TLabel;
    sbBarAnimationApply: TSpeedButton;
    Label64: TLabel;
    acApplyBarAnim: TAction;
    Label60: TLabel;
    cbPieStyle: TComboBox;
    Label69: TLabel;
    Label71: TLabel;
    cbxPieAnimation: TCheckBox;
    cbPieAnimationSpeed: TComboBox;
    ePieAnimationPause: TEdit;
    SpeedButton1: TSpeedButton;
    lbPieFont: TLabel;
    lbxPieOptions: TCheckListBox;
    Label74: TLabel;
    ePieSize: TEdit;
    Label75: TLabel;
    eDoughnutSize: TEdit;
    Label76: TLabel;
    eStartAngle: TEdit;
    Label77: TLabel;
    eSlope: TEdit;
    Label78: TLabel;
    eSliceSpacing: TEdit;
    Label79: TLabel;
    SpeedButton3: TSpeedButton;
    lbPieTitleFont: TLabel;
    eDiscDepth: TEdit;
    Label82: TLabel;
    acPieTitleFont: TFontEdit;
    Label73: TLabel;
    Label81: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label80: TLabel;
    cbxCurveKeepFontColor: TCheckBox;
    cbxPieKeepFontColor: TCheckBox;
    lbCharts: TListBox;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    tab_Commons: TTabSheet;
    cbRulers: TComboBox;
    Label91: TLabel;
    acNLabelFont: TFontEdit;
    acVLabelFont: TFontEdit;
    acChartTitleFont: TFontEdit;
    GroupBox4: TGroupBox;
    eIMargLeft: TEdit;
    eIMargTop: TEdit;
    eIMargRight: TEdit;
    eIMargBottom: TEdit;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    GroupBox5: TGroupBox;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    Label100: TLabel;
    eGmargLeft: TEdit;
    eGMargTop: TEdit;
    eGMargRight: TEdit;
    eGMargBottom: TEdit;
    cbMouseInfo: TComboBox;
    Label101: TLabel;
    sbMoreNSections: TButton;
    cbxNSectionsVisible: TCheckBox;
    cbxVSectionsVisible: TCheckBox;
    sbMoreVSections: TButton;
    Label90: TLabel;
    cbBGColor: TColorBox;
    lbValueQuality: TLabel;
    cbPercentages: TComboBox;
    Shape2: TShape;
    Curve_CO2: TCWCurve;
    Leg_CO2: TCWLegend;
    legCO2Pandemic: TCWLegend;
    acNextPage: TAction;
    ToolButton2: TToolButton;
    acPrevPage: TAction;
    acUnzoom: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    acDashbord: TAction;
    ToolButton5: TToolButton;
    Chart_CO2: TCWSpanChart;
    Sect_Year: TCWNameSectionDefs;
    SpeedButton6: TSpeedButton;
    lbTitleFont: TLabel;
    Label92: TLabel;
    cbTitleAlignment: TComboBox;
    Label102: TLabel;
    lbNLabelFont: TLabel;
    acV2LabelFont: TFontEdit;
    Names: TFrame1;
    V1: TFrame1;
    V2: TFrame1;
    cbxCenterChart: TCheckBox;
    acShowData: TAction;
    ToolButton6: TToolButton;
    acNewChart: TAction;
    ToolButton7: TToolButton;
    FileSaveAs1: TFileSaveAs;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Chart1: TMenuItem;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    Createnewchart1: TMenuItem;
    acShowData1: TMenuItem;
    View1: TMenuItem;
    Viewdashboard1: TMenuItem;
    Nextpage1: TMenuItem;
    Previouspage1: TMenuItem;
    Unzoom1: TMenuItem;
    N1: TMenuItem;
    acAddNameSections: TAction;
    acAddValueSections: TAction;
    Addnamesections1: TMenuItem;
    Addvaluesections1: TMenuItem;
    N2: TMenuItem;
    pmSections: TPopupMenu;
    Addsectiontolist1: TMenuItem;
    Deletesection1: TMenuItem;
    acAddSectionList: TAction;
    acDeleteSectionList: TAction;
    acUpdateSection: TAction;
    Updatesection1: TMenuItem;
    Label1: TLabel;
    pmLegends: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    acAddLegend: TAction;
    acDeleteLegend: TAction;
    eLegPointName: TEdit;
    Label13: TLabel;
    acReload: TAction;
    ToolButton9: TToolButton;
    ToolButton11: TToolButton;
    Reload1: TMenuItem;
    pmCategories: TPopupMenu;
    acAddCategory: TAction;
    acDeleteCategory: TAction;
    acAddCategory1: TMenuItem;
    acDeleteCategory1: TMenuItem;
    acDeleteSeries: TAction;
    acAddSeries: TAction;
    cbSeriesGraphType: TComboBox;
    Label17: TLabel;
    pmSeries: TPopupMenu;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    Label43: TLabel;
    Label103: TLabel;
    seAnimationBooster: TSpinEdit;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    GroupBox3: TGroupBox;
    Label47: TLabel;
    cbxAutoSize: TCheckBox;
    sbBarTextFont: TSpeedButton;
    lbBarFont: TLabel;
    cbxShowQualifier: TCheckBox;
    cbxBarKeepFontColor: TCheckBox;
    Label107: TLabel;
    cbBorders: TComboBox;
    Label48: TLabel;
    Label31: TLabel;
    cbLineShape: TComboBox;
    Label108: TLabel;
    acContract: TAction;
    Contraction1: TMenuItem;
    acResetContraction: TAction;
    Resetcontraction1: TMenuItem;
    N3: TMenuItem;
    sb: TStatusBar;
    imgLabel: TImage;
    Label109: TLabel;
    dlgImage: TOpenPictureDialog;
    SpeedButton2: TSpeedButton;
    tabStats: TTabSheet;
    Label52: TLabel;
    cbCurveStatLine: TComboBox;
    Label110: TLabel;
    Label111: TLabel;
    eSMAPeriods: TEdit;
    eStatLineWidth: TSpinEdit;
    Label112: TLabel;
    tbStatBGBlending: TTrackBar;
    cbxLiveResize: TCheckBox;
    CWBar1: TCWBar;
    cbxImageLabels: TCheckBox;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbSeriesClick(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure cbChartSeriesColorChange(Sender: TObject);
    procedure lbxTextTiltingClickCheck(Sender: TObject);
    procedure cbxLegVisibleClick(Sender: TObject);
    procedure cbxChartVisibleClick(Sender: TObject);
    procedure lbNameSectionsClick(Sender: TObject);
    procedure lbValueSectionsClick(Sender: TObject);
    procedure lbxLegContentClickCheck(Sender: TObject);
    procedure eChartTitleExit(Sender: TObject);
    procedure eSeriesTitleExit(Sender: TObject);
    procedure eHighValueExit(Sender: TObject);
    procedure eLowValueExit(Sender: TObject);
    procedure cbxAutoSpanClick(Sender: TObject);
    procedure eValueIntervalsExit(Sender: TObject);
    procedure eNamePrecisionExit(Sender: TObject);
    procedure cbOverflowActionChange(Sender: TObject);
    procedure cbAxisOrientationChange(Sender: TObject);
    procedure cbAxisColorChange(Sender: TObject);
    procedure cbWallColorChange(Sender: TObject);
    procedure cbxGradientWallClick(Sender: TObject);
    procedure eWallWidthExit(Sender: TObject);
    procedure cbLegAnchoringChange(Sender: TObject);
    procedure cbLegContentFlowChange(Sender: TObject);
    procedure memLegTextExit(Sender: TObject);
    procedure eLegHorizMarginsExit(Sender: TObject);
    procedure eLegVertMarginsExit(Sender: TObject);
    procedure cbLegAlignmentChange(Sender: TObject);
    procedure cbLegBulletsChange(Sender: TObject);
    procedure cbAutoSectionsChange(Sender: TObject);
    procedure cbTimeTemplateChange(Sender: TObject);
    procedure acCurveFontBeforeExecute(Sender: TObject);
    procedure acCurveFontAccept(Sender: TObject);
    procedure cbCurvePointMarkersChange(Sender: TObject);
    procedure cbNeighborAreaColorChange(Sender: TObject);
    procedure eLineWidthExit(Sender: TObject);
    procedure cbxCurveBeaconPointsClick(Sender: TObject);
    procedure acApplyUpdate(Sender: TObject);
    procedure acApplyExecute(Sender: TObject);
    procedure lbApplyOnClick(Sender: TObject);
    procedure lbxBarOptionsClickCheck(Sender: TObject);
    procedure acApplyBarAnimExecute(Sender: TObject);
    procedure acApplyBarAnimUpdate(Sender: TObject);
    procedure acPieTitleFontAccept(Sender: TObject);
    procedure cbPieStyleChange(Sender: TObject);
    procedure cbChartItemColorChange(Sender: TObject);
    procedure eItemNameExit(Sender: TObject);
    procedure cbxCurveKeepFontColorClick(Sender: TObject);
    procedure acPieTitleFontBeforeExecute(Sender: TObject);
    procedure acNLabelFontBeforeExecute(Sender: TObject);
    procedure acNLabelFontAccept(Sender: TObject);
    procedure acVLabelFontBeforeExecute(Sender: TObject);
    procedure acVLabelFontAccept(Sender: TObject);
    procedure acChartTitleFontBeforeExecute(Sender: TObject);
    procedure acChartTitleFontAccept(Sender: TObject);
    procedure cbTitleAlignmentChange(Sender: TObject);
    procedure cbBGColorChange(Sender: TObject);
    procedure cbRulersChange(Sender: TObject);
    procedure cbMouseInfoChange(Sender: TObject);
    procedure eIMargLeftExit(Sender: TObject);
    procedure eIMargTopExit(Sender: TObject);
    procedure eIMargRightExit(Sender: TObject);
    procedure eIMargBottomExit(Sender: TObject);
    procedure eGmargLeftExit(Sender: TObject);
    procedure eGMargTopExit(Sender: TObject);
    procedure eGMargRightExit(Sender: TObject);
    procedure eGMargBottomExit(Sender: TObject);
    procedure sbNameDivLineClick(Sender: TObject);
    procedure sbMoreNSectionsClick(Sender: TObject);
    procedure sbMoreVSectionsClick(Sender: TObject);
    procedure cbxVSectionsVisibleClick(Sender: TObject);
    procedure cbxNSectionsVisibleClick(Sender: TObject);
    procedure lbChartsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure acNextPageExecute(Sender: TObject);
    procedure acUnzoomExecute(Sender: TObject);
    procedure acDashbordExecute(Sender: TObject);
    procedure CWQuerySpace(Sender: TObject; const QueryResult: Integer;
      var Response: TSpaceResponse);
    procedure sbDivLineClick(Sender: TObject);
    procedure sbDivline2Click(Sender: TObject);
    procedure acShowDataExecute(Sender: TObject);
    procedure acNewChartExecute(Sender: TObject);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure acUnzoomUpdate(Sender: TObject);
    procedure acShowDataUpdate(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveAs1Update(Sender: TObject);
    procedure acNextPageUpdate(Sender: TObject);
    procedure acPrevPageUpdate(Sender: TObject);
    procedure acPrevPageExecute(Sender: TObject);
    procedure acAddValueSectionsUpdate(Sender: TObject);
    procedure acAddValueSectionsExecute(Sender: TObject);
    procedure acAddNameSectionsExecute(Sender: TObject);
    procedure acAddNameSectionsUpdate(Sender: TObject);
    procedure acAddSectionListExecute(Sender: TObject);
    procedure acUpdateSectionExecute(Sender: TObject);
    procedure acDeleteSectionListExecute(Sender: TObject);
    procedure acAddSectionListUpdate(Sender: TObject);
    procedure acUpdateSectionUpdate(Sender: TObject);
    procedure acDeleteSectionListUpdate(Sender: TObject);
    procedure acAddLegendExecute(Sender: TObject);
    procedure lbLegendsClick(Sender: TObject);
    procedure eLegPointNameExit(Sender: TObject);
    procedure acDeleteLegendUpdate(Sender: TObject);
    procedure acDeleteLegendExecute(Sender: TObject);
    procedure acReloadExecute(Sender: TObject);
    procedure acReloadUpdate(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acAddCategoryUpdate(Sender: TObject);
    procedure acDeleteCategoryUpdate(Sender: TObject);
    procedure acAddCategoryExecute(Sender: TObject);
    procedure acDeleteCategoryExecute(Sender: TObject);
    procedure acAddSeriesExecute(Sender: TObject);
    procedure acAddSeriesUpdate(Sender: TObject);
    procedure acDeleteSeriesExecute(Sender: TObject);
    procedure acDeleteSeriesUpdate(Sender: TObject);
    procedure cbSeriesGraphTypeChange(Sender: TObject);
    procedure cbxCenterChartClick(Sender: TObject);
    procedure cbCurveStyleChange(Sender: TObject);
    procedure cbBordersChange(Sender: TObject);
    procedure ePrecision2Exit(Sender: TObject);
    procedure ePrecision1Exit(Sender: TObject);
    procedure cbxCurveAnimationClick(Sender: TObject);
    procedure cbxPieAnimationClick(Sender: TObject);
    procedure cbxAutoSizeClick(Sender: TObject);
    procedure CWDataChange(Sender: TObject);
    procedure CWAfterBuildChart(Sender: TObject);
    procedure cbLineShapeChange(Sender: TObject);
    procedure acContractExecute(Sender: TObject);
    procedure acResetContractionExecute(Sender: TObject);
    procedure acResetContractionUpdate(Sender: TObject);
    procedure acContractUpdate(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure SpeedButton2Click(Sender: TObject);
    procedure eSMAPeriodsExit(Sender: TObject);
    procedure eStatLineWidthChange(Sender: TObject);
    procedure tbStatBGBlendingChange(Sender: TObject);
    procedure cbxLiveResizeClick(Sender: TObject);
    procedure cbCurveStatLineChange(Sender: TObject);
    procedure cbxImageLabelsClick(Sender: TObject);
  private
    { Private declarations}
    FGraph1, FGraph2 : TCWGraph;
    FActiveGraphControls : TGraphControls;
    FCurveControls : TCurveControls;
    FBarControls : TBarControls;
    FPieControls : TPieControls;
    FSizing : Boolean;
    function SerIndex : integer;
    function SelGraph : TCWGraph;
    procedure FillChartControls;
    procedure FillChartList;
    procedure SyncSeriesControls(SerNr : integer);
    procedure SyncItemControls(ItmNr : integer);
    procedure SyncSectionValues(AList : TListBox);
    procedure SyncLegends(Indx : integer);
    procedure SetVisibleTabs(GraphsOnly : Boolean = false);
    procedure CO2;
    procedure ChartChange(Sender : TObject);
    procedure ShowData;
    procedure CreateChart;
    function GetBaseName : string;
    procedure EditSectionList(DoInsert : Boolean);
    function CheckSectionValues(DoInsert : Boolean) : Boolean;
    procedure SetLegProps(ALegend : TCWLegend);
    procedure SetLegEnables(ALegend : TCWLegend);
    procedure ResetLegControls;
    procedure WMExitSizeMove(var Message: TWMSize); message WM_EXITSIZEMOVE;
    procedure WMEnterSizeMove(var Message: TWMSize); message WM_ENTERSIZEMOVE;
    procedure SetStates;
    procedure LoadImage;
  public
    { Public declarations }
   procedure SetPenProps(APen : TPen);
   function DisableCBXEvent(ACBX : TObject) : TNotifyEvent;
   procedure OnLoadChart;
  end;

  function CountWholeMonths(const StartDate, EndDate: TDateTime): Integer;

var
  form_main: Tform_main;
  fram : TFrame1;
  CurveFrames : TList<TFrame1>;

implementation

{$R *.dfm}



function CountWholeMonths(const StartDate, EndDate: TDateTime): Integer;
var
  StartYear, StartMonth, StartDay: Word;
  EndYear, EndMonth, EndDay: Word;
  WholeMonths: Integer;
begin
  // Extract year, month, and day components from the start and end dates
  DecodeDate(StartDate, StartYear, StartMonth, StartDay);
  DecodeDate(EndDate, EndYear, EndMonth, EndDay);

  // Calculate the difference in years and months
  WholeMonths := (EndYear - StartYear) * 12 + (EndMonth - StartMonth);

  // Adjust the count if the end day is earlier than the start day
  if EndDay < StartDay then
    Dec(WholeMonths);

  Result := WholeMonths;
end;

procedure TForm_Main.CO2;
var
  sl : TStringList;
  i : integer;
  s, S2 : string;
  P, P2 : integer;
  Res : string;
  Out : TStringList;
  N : integer;
begin
  sl := TStringList.Create;
  Out := TStringList.Create;
  sl.LoadFromFile('C:\Users\47994\Documents\Embarcadero\Studio\Projects\CWDemo\Graphs\CO2.Txt');
  for I := 0 to sl.Count-1 do
  begin
    Res := '';
    S := Trim(sl[i]);
    S := StringReplace(S, '      ', '|', [rfReplaceAll]);
    S := StringReplace(S, '    ', '|', [rfReplaceAll]);
    S := StringReplace(S, '   ', '|', [rfReplaceAll]);
    P := Pos('|', S);
    Res := Copy(S,1, P-1); {Y}
    Delete(S, 1, P);
    P2 := Pos('|', S, P+1);
    P := Pos('|', S);
    S2 := Copy(S,1, P-1); {M}
    if Length(S2) = 1 then
     S2 := '0' + S2;
    S2 := '01.' + S2;
    Res := S2 + '.' + Res;
    Delete(S, 1, P);
    P := Pos('|', S);
    P2 :=  Pos('|', S, P +1);
    S2 := Copy(S,P+1, P2-P-1); {CO2}
    S2 := StringReplace(S2, '.', '', [rfReplaceAll]);
    N := StrToInt(S2) * 10;
    S := IntToStr(N);
    Res := Res + '=' + S; {CO2}
    Out.Add(Res);
  end;
  Out.SaveToFile('C:\Users\47994\Documents\Embarcadero\Studio\Projects\CWDemo\Graphs\CO2Graph3.Txt');
end;


constructor TGraphControls.Create(AGraph: TCWGraph; AForm : TForm_Main);
begin
  FGraph := AGraph;
  Frm :=AForm;
end;


{Curv ctrls}
procedure TCurveControls.DoFill;
var
  G : TCWCurve;
  E : TNotifyEvent;
  i : integer;
begin
   G := Graph as TCWCurve;
   Frm.cbNeighborAreaColor.Selected := G.AreaBrush.Color;
   E := Frm.DisableCBXEvent(Frm.cbxAreaOutLine);
   Frm.cbxAreaOutLine.Checked := G.AreaOutline;
   Frm.cbxAreaOutLine.OnClick := E;
   Frm.cbAreaOutLineColor.Selected := G.AreaOutlineColor;
   Frm.eCurveBaseLine.Text := FloatToStr(G.BaseLineValue);
   Frm.eCurveMinPointSpacing.Text := IntToStr(G.MinPointSpacing);
   Frm.eCurveMaxPointSpacing.Text := IntToStr(G.MaxPointSpacing);
   Frm.cbNeighborAreaColor.Selected := G.AreaBrush.Color;
   E := Frm.DisableCBXEvent(Frm.cbxCurveBeaconPoints);
   Frm.cbxCurveBeaconPoints.Checked := G.BeaconPoints;
   Frm.cbxCurveBeaconPoints.OnClick := E;
   Frm.cbCurvePointMarkers.ItemIndex := Ord(G.PointMarkers);
   Frm.cbLineShape.ItemIndex := Ord(G.LineShape);
   Frm.lbCurveFontName.Caption := G.Font.Name + ' ' + IntToStr(G.Font.Size);
   Frm.lbCurveFontName.Font.Color := G.Font.Color;
   E := Frm.DisableCBXEvent(Frm.cbxCurveAnimation);
   Frm.cbxCurveAnimation.Checked := G.Animation;
   Frm.cbxCurveAnimation.OnClick := E;
   Frm.cbCurveAnimationSpeed.ItemIndex := Ord(G.AnimationSpeed);
   Frm.eCurveAnimationPause.Text := IntToStr(G.AnimationPause);
   Frm.lbApplyOn.Clear;
   Frm.lbApplyOn.Items.Add('All series');
   if Frm.CW.Chart.SeriesDefs.Count > 1 then
   for I := 0 to Frm.CW.Chart.SeriesDefs.Count-1 do
     begin
       if Frm.CW.Chart.SeriesDefs[i].Title <> '' then
        Frm.lbApplyOn.Items.Add(Frm.CW.Chart.SeriesDefs[i].Title)
       else
        Frm.lbApplyOn.Items.Add('Series' + IntToStr(I));
     end;
   Frm.lbApplyOn.ItemIndex := 0;
   FStyleIndex := -1;
   E := Frm.DisableCBXEvent(Frm.cbxCurveKeepFontColor);
   Frm.cbxCurveKeepFontColor.Checked := G.KeepFontColor;
   Frm.cbxCurveKeepFontColor.OnClick := E;
   Frm.lbCurveFontName.Caption := G.Font.Name + ' ' + IntToStr(G.Font.Size);
   Frm.lbCurveFontName.Font.Color := G.Font.Color;
   Frm.cbCurveStatLine.ItemIndex := Ord(G.StatLine);
   Frm.eSMAPeriods.Text := IntToStr(G.SMAPeriods);
   Frm.tbStatBGBlending.Position := G.StatBackgroundBlending;
   Frm.eStatLineWidth.Value := G.StatLineWidth;

end;

procedure TCurveControls.Update(Ctrl : TObject);
var
  G : TCWCurve;
  Tit : string;
  Ind : integer;
  Styl : TCWSeriesStyle;
  DoAnimation : Boolean;
  LW : integer;
  LS: TCurveLineStyle;
  St : TCurveStyle;

  procedure SetStyle;
  begin
     Styl.Style := St;
     Styl.LineStyle := LS;
     Styl.LineWidth := LW;
  end;

begin
   if FRM.CW.IsAnimating then
    Exit;
   Frm.CW.BeginUpdate;
   try
    try
     G := Graph as TCWCurve;
     LW := StrToInt(Frm.eLineWidth.Text);
     LS := TCurveLineStyle(Frm.cbLineStyle.ItemIndex);
     St := TCurveStyle(Frm.cbCurveStyle.ItemIndex);
     if (Ctrl = nil) then
     begin
      try
       if FStyleIndex = -1 then
       begin
         G.SeriesStyles.ApplyOnAll(St, LS, LW);
         Exit;
       end;
       Tit := Frm.lbApplyOn.Items[Frm.lbApplyon.ItemIndex];
       Ind := G.SeriesStyles.IndexOf(Tit);
       if Ind <> -1 then
       begin
          Styl := G.SeriesStyles.Items[Ind];
          SetStyle;
       end
       else if G.SeriesStyles.Count = Graph.Chart.SeriesDefs.Count then
       begin
          Styl := G.SeriesStyles.Items[FStyleIndex];
          SetStyle;
       end
       else
       begin
          if (G.Style <> TCurveStyle(Frm.cbCurveStyle.ItemIndex))
          or (G.LineStyle <> TCurveLineStyle(Frm.cbLineStyle.ItemIndex))
          or (G.LineWidth <> StrToInt(Frm.eLineWidth.Text)) then
          begin
             if Tit <> '' then
             begin
                Styl := G.SeriesStyles.Add;
                Styl.SeriesTitle := Tit;
                try
                  SetStyle;
                except
                  G.SeriesStyles.Delete(Styl.Index);
                  raise;
                end;
             end
             else
             begin

             end;

          end;
       end;
      except
       SyncSeriesStyles;
       raise;
      end;

      Exit;
     end;

     G.AreaBrush.Color := Frm.cbNeighborAreaColor.Selected;
     G.AreaOutline := Frm.cbxAreaOutLine.Checked;
     G.AreaOutlineColor := Frm.cbAreaOutLineColor.Selected;
     G.BaseLineValue := StrToFloat(Frm.eCurveBaseLine.Text);
     G.LineShape := TLineShape(Frm.cbLineShape.ItemIndex);
     G.MinPointSpacing := StrToInt(Frm.eCurveMinPointSpacing.Text);
     G.MaxPointSpacing := StrToInt(Frm.eCurveMaxPointSpacing.Text);
     G.AreaBrush.Color := Frm.cbNeighborAreaColor.Selected;
     G.BeaconPoints := Frm.cbxCurveBeaconPoints.Checked;
     G.PointMarkers := TPointMarkers(Frm.cbCurvePointMarkers.ItemIndex);
     DoAnimation := not G.Animation and Frm.cbxCurveAnimation.Checked;
     G.Animation := Frm.cbxCurveAnimation.Checked;
     G.AnimationSpeed := TAnimationSpeed(Frm.cbCurveAnimationSpeed.ItemIndex);
     G.AnimationPause := StrToInt(Frm.eCurveAnimationPause.Text);
     G.StatLine := TStatLine(Frm.cbCurveStatLine.ItemIndex);
     G.StatBackgroundBlending := Frm.tbStatBGBlending.Position;
     G.StatLineWidth := Frm.eStatLineWidth.Value;
     if Frm.eSMAPeriods.Text <> '' then
     begin
       G.SMAPeriods := StrToInt(Frm.eSMAPeriods.Text);
     end;

     if DoAnimation then
       Frm.CW.RefreshChart;
    except
       Frm.CW.CancelUpdate;
       DoFill;
       raise;
    end;
   finally
     frm.CW.EndUpdate;
   end;
end;

procedure TCurveControls.SyncSeriesStyles;
var
 Ind : integer;
 Styl : TCWSeriesStyle;
 Tit : string;

 procedure SetStyle;
 begin
     Frm.cbCurveStyle.ItemIndex := Ord(Styl.Style);
     Frm.cbLineStyle.ItemIndex := Ord(Styl.LineStyle);
     Frm.eLineWidth.Text := IntToStr(Styl.LineWidth);
 end;

begin
   with Graph as TCWCurve do
   begin
     if FStyleIndex = -1 then
     begin
       Frm.cbCurveStyle.ItemIndex := Ord(Style);
       Frm.cbLineStyle.ItemIndex := Ord(LineStyle);
       Frm.eLineWidth.Text := IntToStr(LineWidth);
       Exit;
     end;
     Tit := Chart.SeriesDefs[FStyleIndex].Title;
     Ind := SeriesStyles.IndexOf(Tit);
     if Ind <> -1 then
     begin
        Styl := SeriesStyles.Items[Ind];
        SetStyle;
     end
     else if SeriesStyles.Count = Graph.Chart.SeriesDefs.Count then
     begin
        Styl := SeriesStyles.Items[FStyleIndex];
        SetStyle;
     end
     else
     begin
        Frm.cbCurveStyle.ItemIndex := Ord(Style);
        Frm.cbLineStyle.ItemIndex := Ord(LineStyle);
        Frm.eLineWidth.Text := IntToStr(LineWidth);
     end;
   end;
end;

{Bar ctrls}
procedure TBarControls.DoFill;
var
  G : TCWBar;
  E : TNotifyEvent;
begin
  G := Graph as TCWBar;
  Frm.cbBarStyle.ItemIndex := Ord(G.BarStyle);
  Frm.cbLayout.ItemIndex := Ord(G.Layout);
  E := Frm.DisableCBXEvent(Frm.cbxHorizontalBar);
  Frm.cbxHorizontalBar.Checked := G.Writer.AxisOrientation in [alLeftBottom, alRightBottom, alLeftTop, alRightTop];
  Frm.cbxHorizontalBar.OnClick := E;
  Frm.eBarWidth.Text := IntToStr(G.BarWidth);
  Frm.eItemSpacing.Text := IntToStr(G.ItemSpacing);
  Frm.eSeriesSpacing.Text := IntToStr(G.SeriesSpacing);
  Frm.lbxBarOptions.Checked[0] := boBaseLine in G.Options;
  Frm.lbxBarOptions.Checked[1] := boOutlines in G.Options;
  Frm.lbxBarOptions.Checked[2] := boText in G.Options;
  Frm.lbxBarOptions.Checked[3] := boBarImages in G.Options;
  Frm.lbxBarTexts.Checked[0] := tcValue in G.TextContents;
  Frm.lbxBarTexts.Checked[1] := tcName in G.TextContents;
  Frm.lbxBarTexts.Checked[2] := tcTitle in G.TextContents;
  Frm.lbxBarTexts.Checked[3] := tcPercentage in G.TextContents;
  Frm.eBarBaselineValue.Text := FloatToStr(G.BaseLineValue);
  E := Frm.DisableCBXEvent(Frm.cbxShowQualifier);
  Frm.cbxShowQualifier.Checked := G.ShowQualifier;
  Frm.cbxShowQualifier.OnClick := E;
  Frm.eCubeAngle.Text := IntToStr(G.CubeAngle);
  Frm.eCubeDepth.Text := IntToStr(G.CubeDepth);
  Frm.lbxBarAnimations.Checked[0] := anFlow in G.Animations;
  Frm.lbxBarAnimations.Checked[1] := anGrow in G.Animations;
  Frm.cbBarAnimationSpeed.ItemIndex := Ord(G.AnimationSpeed);
  Frm.seAnimationBooster.Value := G.AnimationBooster;
  Frm.eBarAnimationPause.Text := IntToStr(G.AnimationPause);
  E := Frm.DisableCBXEvent(Frm.cbxBarKeepFontColor);
  Frm.cbxBarKeepFontColor.Checked := G.KeepFontColor;
  Frm.cbxBarKeepFontColor.OnClick := E;
  Frm.lbBarFont.Caption := G.Font.Name + ' ' + IntToStr(G.Font.Size);
  Frm.lbBarFont.Font.Color := G.Font.Color;
  E := Frm.DisableCBXEvent(Frm.cbxAutoSize);
  Frm.cbxAutoSize.Checked := G.AutoSize;
  Frm.cbxAutoSize.OnClick := E;
  Frm.cbCurveStatLine.ItemIndex := Ord(G.StatLine);
  Frm.eSMAPeriods.Text := IntToStr(G.SMAPeriods);
  Frm.tbStatBGBlending.Position := G.StatBackgroundBlending;
  Frm.eStatLineWidth.Value := G.StatLineWidth;
end;

procedure TBarControls.ApplyAnimations;
var
  G : TCWBar;
  An : TAnimations;
begin
  An := [];
  if Frm.lbxBarAnimations.Checked[0] then An := AN + [anFlow];
  if Frm.lbxBarAnimations.Checked[1] then An := AN + [anGrow];

  G := Graph as TCWBar;
  G.AnimationSpeed := TAnimationSpeed(Frm.cbBarAnimationSpeed.ItemIndex);
  G.AnimationBooster := Frm.seAnimationBooster.Value;
  G.AnimationPause := StrToInt(Frm.eBarAnimationPause.Text);
  G.Animations := An;
  if G.Animations <> [] then
   if Frm.CW.Chart.PerformAnimation <> AN_OK then
    raise Exception.Create('Failed');
end;

procedure TBarControls.Update(Ctrl : TObject);
var
  G : TCWBar;
  Opt : TBarOptions;
  Txt : TTextContents;
begin
  if FRM.CW.IsAnimating then
    Exit;
  FRM.CW.BeginUpdate;
  try
   try
    G := Graph as TCWBar;
    G.BarStyle := TBarStyle(Frm.cbBarStyle.ItemIndex);
    G.Layout := TBarLayout(Frm.cbLayout.ItemIndex);
    if Frm.cbxHorizontalBar.Checked then
      G.Writer.AxisOrientation := alLeftBottom
    else
      G.Writer.AxisOrientation := alBottomLeft;
    G.BarWidth := StrToInt(Frm.eBarWidth.Text);
    G.ItemSpacing := StrToInt(Frm.eItemSpacing.Text);
    G.SeriesSpacing := StrToInt(Frm.eSeriesSpacing.Text);
    Opt := [];
    if Frm.lbxBarOptions.Checked[0] then Opt := Opt + [boBaseLine];
    if Frm.lbxBarOptions.Checked[1] then Opt := Opt + [boOutLines];
    if Frm.lbxBarOptions.Checked[2] then Opt := Opt + [boText];
    if Frm.lbxBarOptions.Checked[3] then Opt := Opt + [boBarImages];
    G.Options := Opt;

    Txt := [];
    if Frm.lbxBarTexts.Checked[0] then Txt := Txt + [tcValue];
    if Frm.lbxBarTexts.Checked[1] then Txt := Txt + [tcName];
    if Frm.lbxBarTexts.Checked[2] then Txt := Txt + [tcTitle];
    if Frm.lbxBarTexts.Checked[3] then Txt := Txt + [tcPercentage];
    G.TextContents := Txt;



    G.BaseLineValue := StrToFloat(Frm.eBarBaselineValue.Text);
    G.ShowQualifier := Frm.cbxShowQualifier.Checked;
    G.CubeAngle := StrToInt(Frm.eCubeAngle.Text);
    G.CubeDepth := StrToInt(Frm.eCubeDepth.Text);

    G.StatLine := TStatLine(Frm.cbCurveStatLine.ItemIndex);
    G.StatBackgroundBlending := Frm.tbStatBGBlending.Position;
    G.StatLineWidth := Frm.eStatLineWidth.Value;
    if Frm.eSMAPeriods.Text <> '' then
     begin
       G.SMAPeriods := StrToInt(Frm.eSMAPeriods.Text);
     end;

   except
     FRM.CW.CancelUpdate;
     DoFill;
     raise;
   end;
  finally
    FRM.CW.EndUpdate;
  end;
end;

{Pie ctrls}
procedure TPieControls.DoFill;
var
  G : TCWPie;
  E : TNotifyEvent;
begin
    G := Graph as TCWPie;
    Frm.cbPieStyle.ItemIndex := Ord(G.Style);
    Frm.ePieSize.Text := IntToStr(G.PieSize);
    Frm.eDoughnutSize.Text := IntToStr(G.DoughnutSize);
    Frm.eStartAngle.Text := IntToStr(G.StartAngle);
    Frm.eSliceSpacing.Text := FloatToStr(G.SliceSpacing);
    Frm.eDiscDepth.Text := IntToStr(G.DiscDepth);
    Frm.eSlope.Text := IntToStr(G.Slope);
    Frm.eValuePrecision.Text := IntToStr(G.ValuePrecision);
    if poPrintPercentages in G.Options then Frm.lbxPieOptions.Checked[0]:= True;
    if poPrintNames in G.Options then Frm.lbxPieOptions.Checked[1]:= True;
    if poPrintValues in G.Options then Frm.lbxPieOptions.Checked[2]:= True;
    if poPrintSeriesTitles in G.Options then Frm.lbxPieOptions.Checked[3]:= True;
    if poPrintTitlesInDoughnut in G.Options then Frm.lbxPieOptions.Checked[4]:= True;
    if poTextBackground in G.Options then Frm.lbxPieOptions.Checked[5]:= True;
    if poPinText in G.Options then Frm.lbxPieOptions.Checked[6]:= True;
    if poClientBorder in G.Options then Frm.lbxPieOptions.Checked[7]:= True;
    E := Frm.DisableCBXEvent(Frm.cbxPieAnimation);
    Frm.cbxPieAnimation.Checked := G.Animation;
    Frm.cbxPieAnimation.OnClick := E;
    Frm.cbPieAnimationSpeed.ItemIndex := Ord(G.AnimationSpeed);
    Frm.ePieAnimationPause.Text := IntToStr(G.AnimationPause);
    E := Frm.DisableCBXEvent(Frm.cbxPieKeepFontColor);
    Frm.cbxPieKeepFontColor.Checked := G.KeepFontColor;
    Frm.cbxPieKeepFontColor.OnClick := E;
    Frm.lbPieFont.Caption := G.Font.Name + ' ' + IntToStr(G.Font.Size);
    Frm.lbPieFont.Font.Color := G.Font.Color;
    Frm.lbPieTitleFont.Caption := G.Font.Name + ' ' + IntToStr(G.SeriesTitleFont.Size);
    Frm.lbPieTitleFont.Font.Color := G.SeriesTitleFont.Color;
end;

procedure TPieControls.Update(Ctrl : TObject);
var
  G : TCWPie;
  Opt : TPieOptions;
  Anim : Boolean;
begin
  if FRM.CW.IsAnimating then
    Exit;
  FRM.CW.BeginUpdate;
  try
   try
    G := Graph as TCWPie;
    G.Style := TPieStyle(Frm.cbPieStyle.ItemIndex);
    G.PieSize := StrToInt(Frm.ePieSize.Text);
    G.DoughnutSize := StrToInt(Frm.eDoughnutSize.Text);
    G.StartAngle := StrToInt(Frm.eStartAngle.Text);
    G.SliceSpacing := StrToFloat(Frm.eSliceSpacing.Text);
    G.DiscDepth := StrToInt(Frm.eDiscDepth.Text);
    G.Slope := StrToInt(Frm.eSlope.Text);
    G.ValuePrecision := StrToInt(Frm.eValuePrecision.Text);
    Opt := [];
    if Frm.lbxPieOptions.Checked[0] then Opt := Opt +  [poPrintPercentages];
    if Frm.lbxPieOptions.Checked[1] then Opt := Opt +  [poPrintNames];
    if Frm.lbxPieOptions.Checked[2] then Opt := Opt +  [poPrintValues];
    if Frm.lbxPieOptions.Checked[3] then Opt := Opt +  [poPrintSeriesTitles];
    if Frm.lbxPieOptions.Checked[4] then Opt := Opt +  [poPrintTitlesInDoughnut];
    if Frm.lbxPieOptions.Checked[5] then Opt := Opt +  [poTextBackground];
    if Frm.lbxPieOptions.Checked[6] then Opt := Opt +  [poPinText];
    if Frm.lbxPieOptions.Checked[7] then Opt := Opt +  [poClientBorder];
    G.Options := Opt;
    Anim := G.Animation;
    G.Animation := Frm.cbxPieAnimation.Checked;
    G.AnimationSpeed := TAnimationSpeed(Frm.cbPieAnimationSpeed.ItemIndex);
    G.AnimationPause :=  StrToInt(Frm.ePieAnimationPause.Text);
    if not Anim and G.Animation then
    begin
      Frm.CW.SetFocus; {To prevent spacebar press affecting the checkbox}
      Frm.CW.RefreshChart;
    end;
   except
     FRM.CW.CancelUpdate;
     DoFill;
     raise;
   end;
  finally
    FRM.CW.EndUpdate;
  end;
end;

procedure TForm_Main.WMExitSizeMove(var Message: TWMSize);
begin
  inherited;
  FSizing := False;
  FillChartControls;
  FActiveGraphControls.DoFill;
  SetStates;
 end;

procedure TForm_Main.WMEnterSizeMove(var Message: TWMSize);
begin
  inherited;
  FSizing := True;
 end;

procedure TForm_main.ChartChange(Sender : TObject);
begin
   if CW.ChartList.IndexOf(CW.Chart, nil) = -1 then
   begin
       CW.ChartList.Add(CW.Chart);
       FillChartList;
       OnLoadChart;
   end;
   lbCharts.ItemIndex := CW.ChartList.ItemIndex;
   if not (CW.Chart is TCWPieChart) then
   begin
    if not (CW.ValueScale1.ValueSpanFromData) then
     CW.BezierMargin := 0
    else
     CW.BezierMargin := 20;
   end;
   if CW.Chart is TCWSpanChart then
   begin
     CW.ValueScale1.MinLabelSpacing := 10;
   end;
end;

procedure TForm_Main.LoadImage;
begin
  if dlgImage.Execute then
  begin
    ImgLabel.Picture.LoadFromFile(dlgImage.FileName);
    CW.Categories.Items[lbItems.ItemIndex].Image.Assign(ImgLabel.Picture);
  end;
end;

procedure TForm_Main.SetStates;
var
  Pst : string;

const
 CIdle = '  Idle';

  function GetCompression : string;
  var
    Cnt : integer;
  begin
    Result := '';
    Cnt := CW.AbsoluteItemCount;
    if Cnt > CW.GraphPrintRect.Width  then
    begin
       Pst := FormatNum(100-(CW.GraphPrintRect.Width / Cnt * 100), 1);
       Result := '  Compressed ' + Pst + '%';
    end
    else
      Result := CIdle;
    end;

begin
  if CW.Chart = nil then
  begin
    sb.SimpleText := '  No active chart';
    Exit;
  end;
  if CW.Count = 0 then
  begin
   sb.SimpleText := '  No data';
   Exit;
  end;
  if CW.OverflowError then
  begin
    sb.SimpleText := '  Overflow error';
    Exit;
  end;
  if CW.IsAxisChart then
  if CW.NameScale.OverflowAction = ovScrolling then
  begin
     if CW.CanScroll(stNext) or  CW.CanScroll(stPrev) then
     begin
      sb.SimpleText := 'Page ' + IntToStr(CW.CurrentPage) + ' / ' + IntToStr(CW.PageCount);
     end
     else
      sb.SimpleText := CIdle;
     Exit;
  end;
  if CW.Chart is TCWspanChart then
  begin
    if CW.NameScale.OverflowAction = ovContraction then
    begin
      if CW.Contraction = 1 then
        sb.SimpleText := cIdle
      else
      begin
        Pst := FormatNum(100-100/CW.Contraction, 1);
        sb.SimpleText := '  Contracted by ' + Pst + '%';
      end;
    end
    else if CW.NameScale.OverflowAction = ovCompression then
    begin
        sb.SimpleText := GetCompression;
    end
    else
      sb.SimpleText := CIdle;
  end
  else if (CW.Chart is TCWGeneralChart) then
  begin
    if (CW.NameScale.OverflowAction = ovCompression) then
      sb.SimpleText := GetCompression
    else
      sb.SimpleText := CIdle
  end
  else
    sb.SimpleText := CIdle;

end;

function Tform_main.SerIndex : integer;
begin
  Result := lbSeries.ItemIndex;
end;

procedure Tform_main.acAddCategoryExecute(Sender: TObject);
var
  Item:TCWCategory;
begin
     Item := CW.Categories.Add;
     Item.CategoryName := 'New category';
     Item.Color := clBlack;
     FillChartControls;
     lbItems.ItemIndex := lbItems.Count-1;
     SyncItemControls(lbItems.ItemIndex);
end;

procedure Tform_main.acAddCategoryUpdate(Sender: TObject);
var
 Enabl : Boolean;
begin
   Enabl := (CW.Count = 0) and (CW.Chart <> nil) and not CW.OverflowError;
   acAddCategory.Enabled  := Enabl;
end;

procedure Tform_main.acAddLegendExecute(Sender: TObject);
var
  Leg : TCWLegend;
  LegItm : TCWLegendItem;
begin
  LegItm := CW.Chart.Legends.Add;
  Leg := TCWLegend.Create(Self);
  LegItm.Legend := Leg;
  SetLegProps(Leg);
  FillChartControls;
  lbLegends.Selected[lbLegends.Count-1] := True;
  SyncLegends(lbLegends.ItemIndex);
end;

procedure Tform_main.acAddNameSectionsExecute(Sender: TObject);
var
 NS : TCWNameSectionDefs;
begin
  NS := TCWNameSectionDefs.Create(Self);
  NS.Name := 'NSection_'+GetBaseName;
  CW.NameSectionDefs := NS;
  panChart.Visible := True;
  pgChartDefs.ActivePage := tabSections;
  tabSections.TabVisible :=True;
  tabNameSections.TabVisible := True;
  pgSections.ActivePage := tabNameSections;
end;

procedure Tform_main.acAddNameSectionsUpdate(Sender: TObject);
begin
 acAddNameSections.Enabled := (CW.Chart <> nil) and CW.IsAxisChart and (CW.NameSectionDefs = nil);
end;

procedure Tform_main.acAddValueSectionsExecute(Sender: TObject);
var
 NS : TCWValueSectionDefs;
begin
  NS := TCWValueSectionDefs.Create(Self);
  NS.Name := 'VSection_'+GetBaseName;
  CW.ValueSectionDefs := NS;
  panChart.Visible := True;
  tabSections.TabVisible :=True;
  tabValueSections.TabVisible := True;
  pgChartDefs.ActivePage := tabSections;
  pgSections.ActivePage := tabValueSections;
end;

procedure Tform_main.acAddValueSectionsUpdate(Sender: TObject);
begin
    acAddValueSections.Enabled := (CW.Chart <> nil) and CW.IsAxisChart and (CW.ValueSectionDefs = nil);
end;

procedure Tform_main.acApplyBarAnimExecute(Sender: TObject);
begin
  FBarControls.ApplyAnimations;
end;

procedure Tform_main.acApplyBarAnimUpdate(Sender: TObject);
var
 Enabl : Boolean;
 AN : TAnimations;
begin
    Enabl := (Selgraph is TCWBar);
    if Enabl then
    begin
     if eBarAnimationPause.Text = '' then
      Enabl := false
     else
     begin
      An := [];
      if lbxBarAnimations.Checked[0] then An := AN + [anFlow];
      if lbxBarAnimations.Checked[1] then An := AN + [anGrow];

      Enabl := (AN <> []) or ((AN = []) and (TCWBar(SelGraph).Animations <> AN));
     end;

    end;
   acApplyBarAnim.Enabled := Enabl;
end;

procedure Tform_main.acApplyExecute(Sender: TObject);
begin
    FCurveControls.Update(nil);
end;

procedure Tform_main.acApplyUpdate(Sender: TObject);
var
  Enabl : Boolean;
  Indx : integer;
  Tit : string;
  LW : integer;
  LS: TCurveLineStyle;
  St : TCurveStyle;
  N : integer;
begin
   if (SelGraph <> nil) and (SelGraph is TCWCurve) then
   with FCurveControls.Graph as TCWCurve do
    begin
        if not TryStrToInt(eLineWidth.Text, N) then
        begin
          acApply.Enabled := false;
          Exit;
        end;

        LW := StrToInt(eLineWidth.Text);
        LS := TCurveLineStyle(cbLineStyle.ItemIndex);
        St := TCurveStyle(cbCurveStyle.ItemIndex);

        if (lbApplyOn.ItemIndex = 0) or (FCurveControls.FStyleIndex = -1) then
        begin
          Enabl := (SeriesStyles.Count > 0);
          if not Enabl then
          Enabl := (LS <> LineStyle)
              or (LW <> LineWidth)
              or (St <> Style);
        end
        else
        begin
           Tit := Chart.SeriesDefs[FCurveControls.FStyleIndex].Title;
           Indx := SeriesStyles.IndexOf(Tit);
           if Indx = -1 then
           begin
             if SeriesStyles.Count = Chart.SeriesDefs.Count then
               Indx := FCurveControls.FStyleIndex;
           end;
           if Indx <> -1 then
           begin
             Enabl := (SeriesStyles.Items[Indx].LineStyle <> LS)
             or (SeriesStyles.Items[Indx].LineWidth <> LW)
             or (SeriesStyles.Items[Indx].Style <> St);
           end
           else
           begin
              Enabl := (LS <> LineStyle)
              or (LW <> LineWidth)
              or (St <> Style);
           end;
        end;

        acApply.Enabled := Enabl;
    end
    else
     acApply.Enabled := false;
end;

procedure Tform_main.acChartTitleFontAccept(Sender: TObject);
begin
   CW.TitleFont.Assign(acChartTitleFont.Dialog.Font);
   FillChartControls;
end;

procedure Tform_main.acChartTitleFontBeforeExecute(Sender: TObject);
begin
   acChartTitleFont.Dialog.Font.Assign(CW.TitleFont);
end;

procedure Tform_main.acContractExecute(Sender: TObject);
var
  Rate : string;
  N : integer;
begin
    Rate := InputBox('Contraction', 'Rate', IntToStr(CW.Contraction));
    if Rate = IntToStr(CW.Contraction) then
      Exit;
    if not TryStrToInt(Rate, N) then
      raise Exception.Create('Must be a number');
    CW.Contraction := N;
    SetStates;
end;

procedure Tform_main.acContractUpdate(Sender: TObject);
begin
  acContract.Enabled := (CW.CanContract = 0);
end;

procedure Tform_main.acCurveFontAccept(Sender: TObject);
begin
     SelGraph.Font.Assign(acCurveFont.Dialog.Font);
     FActiveGraphControls.DoFill;
end;

procedure Tform_main.acCurveFontBeforeExecute(Sender: TObject);
begin
     acCurveFont.Dialog.Font.Assign(TCWCurve(Selgraph).Font);
end;

procedure Tform_main.acDashbordExecute(Sender: TObject);
begin
  panChart.Visible := not PanChart.Visible;
end;

procedure Tform_main.acDeleteCategoryExecute(Sender: TObject);
begin
   CW.Categories.Delete(lbItems.ItemIndex);
   FillChartControls;
   if lbItems.ItemIndex = lbItems.Count then
    lbItems.ItemIndex := lbItems.ItemIndex -1;
   if lbItems.ItemIndex = -1 then
   begin
     eItemName.Text := '';
     cbChartItemColor.Selected := clBlack;
   end;
end;

procedure Tform_main.acDeleteCategoryUpdate(Sender: TObject);
begin
   acAddCategory.Enabled := (CW.Count = 0) and (lbItems.ItemIndex <> -1) and not CW.OverflowError
   and (CW.Chart <> nil);
end;

procedure Tform_main.acDeleteLegendExecute(Sender: TObject);
begin
    CW.Chart.DeleteLegend(lbLegends.ItemIndex);
    FillChartControls;
    if lbLegends.ItemIndex = CW.Chart.Legends.Count then
      lbLegends.ItemIndex := lbLegends.ItemIndex-1;
    if lbLegends.ItemIndex <> -1 then
      SyncLegends(lbLegends.ItemIndex)
    else
      ResetLegControls;
end;

procedure Tform_main.acDeleteLegendUpdate(Sender: TObject);
begin
  acDeleteLegend.Enabled := lbLegends.ItemIndex <> -1;
end;

procedure Tform_main.acDeleteSectionListExecute(Sender: TObject);
var
  SectList : TCWSectionDefs;
  lb : TListBox;
  Indx : integer;
begin
  if pgSections.ActivePage = tabNameSections then
  begin
   SectList := CW.NameSectionDefs;
   lb := lbNameSections;
  end
  else
  begin
   SectList := CW.ValueSectionDefs;
   lb := lbValueSections;
  end;
  Indx := lb.ItemIndex;
  SectList.Sections.Delete(lb.ItemIndex);
  FillChartControls;
  if Indx = SectList.Sections.Count then
    Dec(Indx);
   lb.ItemIndex := Indx;
  if Indx <> -1 then
  begin
    if pgSections.ActivePage = tabNameSections  then
      lbNameSectionsClick(nil)
    else
      lbValueSectionsClick(nil);
  end;
end;

procedure Tform_main.acDeleteSectionListUpdate(Sender: TObject);
begin
  if pgSections.ActivePage = tabNameSections then
    acDeleteSectionList.Enabled := lbNameSections.ItemIndex <> -1
  else
    acDeleteSectionList.Enabled := lbValueSections.ItemIndex <> -1
end;

procedure Tform_main.acDeleteSeriesExecute(Sender: TObject);
begin
   CW.Chart.SeriesDefs.Delete(lbSeries.ItemIndex);
   FillChartControls;
end;

procedure Tform_main.acDeleteSeriesUpdate(Sender: TObject);
var
 Enabl : Boolean;
begin
   Enabl := (CW.Count = 0) and (CW.Chart <> nil) and not CW.OverflowError;
   if Enabl then
     Enabl := (lbSeries.ItemIndex <> -1) and (lbSeries.Count > 1);
   acDeleteSeries.Enabled  := Enabl;
end;

procedure Tform_main.acNewChartExecute(Sender: TObject);
begin
  if frm_new.Showmodal = mrOk then
  begin
    CreateChart;
  end;
  frm_new.Reset;
end;

procedure Tform_main.acNextPageExecute(Sender: TObject);
begin
  CW.NextPage;
  SetStates;
end;

procedure Tform_main.acNextPageUpdate(Sender: TObject);
begin
  acNextPage.Enabled := CW.CanScroll(stNext);
end;

procedure Tform_main.acNLabelFontAccept(Sender: TObject);
begin
    //CW.NameFont.Assign(acNLabelFont.Dialog.Font);
end;

procedure Tform_main.acNLabelFontBeforeExecute(Sender: TObject);
begin
   //acNLabelFont.Dialog.Font.Assign(CW.NameFont);
end;

procedure Tform_main.acPieTitleFontAccept(Sender: TObject);
begin
   with SelGraph as TCWPie do
     SeriesTitleFont.Assign(acPieTitleFont.Dialog.Font);
   FPieControls.DoFill;
end;

procedure Tform_main.acPieTitleFontBeforeExecute(Sender: TObject);
begin
  acPieTitleFont.Dialog.Font.Assign(TCWPie(Selgraph).SeriesTitleFont);
end;

procedure Tform_main.acPrevPageExecute(Sender: TObject);
begin
   CW.PrevPage;
   SetStates;
end;

procedure Tform_main.acPrevPageUpdate(Sender: TObject);
begin
  acPrevPage.Enabled := CW.CanScroll(stPrev);
end;

procedure Tform_main.acReloadExecute(Sender: TObject);
begin
  CW.Chart.Reload;
  SetStates;
end;

procedure Tform_main.acReloadUpdate(Sender: TObject);
begin
    acReload.Enabled := CW.OverflowError;
end;

procedure Tform_main.acResetContractionExecute(Sender: TObject);
begin
  CW.ResetContraction;
end;

procedure Tform_main.acResetContractionUpdate(Sender: TObject);
begin
  acResetContraction.Enabled := (CW.Chart <> nil) and (CW.Chart is TCWSpanChart)
  and not CW.IsContractionIdle;
end;

procedure Tform_main.acShowDataExecute(Sender: TObject);
begin
  ShowData;
end;

procedure Tform_main.acShowDataUpdate(Sender: TObject);
begin
   acShowData.Enabled := CW.Chart <> nil;
end;

procedure Tform_main.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  eItemName.Enabled := (CW.Count = 0) and (CW.Chart <> nil)  and not CW.OverflowError;
  eSeriesTitle.Enabled := eItemName.Enabled;
  cbSeriesGraphType.Enabled := eItemName.Enabled
   and not (CW.Chart is TCWPieChart) and (CW.AlternativeGraph = nil);
  cbScale.Enabled := CW.IsAxisChart and not (CW.Chart is TCWCategoryBarChart);
  cbPercentages.Enabled := eItemName.Enabled;
  eHighValue.Enabled := not cbxAutoSpan.Checked;
  eLowValue.Enabled := eHighValue.Enabled;
  gbBarAnimation.Enabled := (Cw.Chart <> nil) and (CW.Chart.AxisCount = 1);
  cbxCurveAnimation.Enabled := gbBarAnimation.Enabled;
end;

procedure Tform_main.acUnzoomExecute(Sender: TObject);
begin
   CW.Unzoom;
end;

procedure Tform_main.acUnzoomUpdate(Sender: TObject);
begin
   acUnzoom.Enabled := (CW.Chart <> nil) and CW.IsAxisChart and CW.Chart.Zoomed;
end;

function Tform_main.CheckSectionValues(DoInsert : Boolean) : Boolean;
var
  lb : TListBox;
  startV, endV, lCap : TEdit;
  E : single;
  N : integer;
  D : TDateTime;

  function Dup(Sects : TCWSectionItems) : Boolean;
  var
    i : integer;
    begin
      Result := false;
      for i := 0 to Sects.Count-1 do
        begin
           if DoInsert then
           begin
             if SameText(lcap.Text, Sects.Items[i].LongCaption) then
             begin
               Result := True;
               Break;
             end;
             if SameText(startV.Text, Sects.Items[i].StartValue)
             and SameText(EndV.Text, Sects.Items[i].EndValue)then
             begin
               Result := True;
               Break;
             end;
           end
           else
           begin
            if i = lb.ItemIndex then
            begin
              if SameText(lcap.Text, Sects.Items[i].LongCaption)
              and SameText(startV.Text, Sects.Items[i].StartValue)
              and SameText(EndV.Text, Sects.Items[i].EndValue) then
              begin
                Result := True;
                Break;
              end;
            end
            else if SameText(lcap.Text, Sects.Items[i].LongCaption) then
             begin
               Result := True;
               Break;
             end;
           end;
        end;
    end;

begin
  Result := false;
  if pgSections.ActivePage = tabNameSections then
  begin
   lb := lbNameSections;
   if not DoInsert and (lb.ItemIndex = -1) then
   begin
     Exit;
   end;
   startV := eNStartValue;
   endV := eNEndValue;
   lCap := eNLongCaption;
   if cbAutoSections.ItemIndex > 0 then
   begin
     Result := True;
   end
   else if (cbTimeTemplate.ItemIndex > 0) or (CW.SpanType = ntNumberSpan) then
   begin
     Result := TryStrToInt(StartV.Text, N) and TryStrToInt(EndV.Text, N)
     and (Trim(lCap.Text) <> '');
   end
   else if CW.IsTimeSpan then
   begin
     Result := TryStrToDateTime(StartV.Text, D) and TryStrToDateTime(EndV.Text, D)
     and (Trim(lCap.Text) <> '');
   end
   else
   begin
     Result := (Trim(StartV.Text) <> '') and (Trim(EndV.Text) <> '')
     and (Trim(lCap.Text) <> '');
   end;
   if Result then
      Result := not Dup(CW.NameSectionDefs.Sections);
  end
  else if pgSections.ActivePage = tabValueSections then
  begin
    lb := lbValueSections;
    if not DoInsert and (lb.ItemIndex = -1) then
    begin
     Result := false;
     Exit;
    end;
    startV := eVStartValue;
    endV := eVEndValue;
    lCap := eVLongCaption;
    Result := TryStrToFloat(StartV.Text, E) and TryStrToFloat(EndV.Text, E)
     and (Trim(lCap.Text) <> '');
    if Result then
      Result := not Dup(CW.ValueSectionDefs.Sections);
  end;
end;

procedure Tform_main.ResetLegControls;
begin
 eLegHorizMargins.Enabled := True;
 eLegVertMargins.Enabled := eLegHorizMargins.Enabled;
 cbLegAlignment.Enabled := eLegHorizMargins.Enabled;
 cbLegBullets.Enabled := eLegVertMargins.Enabled;
 eLegPointName.Enabled := True;

 eLegHorizMargins.Text := '';
 eLegVertMargins.Text := '';
 cbLegAlignment.ItemIndex := -1;
 cbLegBullets.ItemIndex := -1;
 eLegPointName.Text := '';
 cbLegAnchoring.ItemIndex := 4;
 lbxLegContent.CheckAll(cbUnChecked);
 cbLegContentFlow.ItemIndex := 0;
 memLegText.Lines.Clear;
end;

procedure Tform_main.SetLegEnables(ALegend : TCWLegend);
begin
   eLegHorizMargins.Enabled := ALegend.Anchoring <> anPointInside;
   eLegVertMargins.Enabled := eLegHorizMargins.Enabled;
   cbLegAlignment.Enabled := eLegHorizMargins.Enabled;
   cbLegBullets.Enabled := eLegVertMargins.Enabled;
   //eLegPointName.Enabled := ALegend.Anchoring = anPointInside;
end;

procedure Tform_main.SetLegProps(ALegend : TCWLegend);
var
  Cont : TLegendContents;
begin
 CW.BeginUpdate;
  with ALegend do
  begin
    Alignment := laLeftOrTop;
    Bullets := lbNone;
    Anchoring := anRightOutside;
    Cont := [];
    Contents := [];
    PointName := '';
    Alegend.Border := True;
    ALegend.Brush.Color := clCream;
    Transparency := 128;
    ContentFlow := cfTopBottom;
    Text.Clear;
  end;
 CW.EndUpdate;
end;

procedure Tform_main.EditSectionList(DoInsert : Boolean);
var
  SectList : TCWSectionDefs;
  Sect : TCWSectionItem;
  lb : TListBox;
  startV, endV, lCap, sCap : TEdit;
begin
  if pgSections.ActivePage = tabNameSections then
  begin
   SectList := CW.NameSectionDefs;
   lb := lbNameSections;
   startV := eNStartValue;
   endV := eNEndValue;
   lCap := eNLongCaption;
   sCap := eNShortCaption;
  end
  else
  begin
   SectList := CW.ValueSectionDefs;
   lb := lbValueSections;
   startV := eVStartValue;
   endV := eVEndValue;
   lCap := eVLongCaption;
   sCap := eVShortCaption;
  end;
  if DoInsert then
    Sect := SectList.Sections.Add
  else
    Sect := SectList.Sections.Items[lb.ItemIndex];
  CW.BeginUpdate;
  Sect.StartValue := startV.Text;
  Sect.EndValue := endV.Text;
  Sect.LongCaption := lCap.Text;
  Sect.ShortCaption := sCap.Text;
  CW.EndUpdate;
  FillChartControls;
  if DoInsert then
    lb.ItemIndex := lb.Count-1;
  if pgSections.ActivePage = tabNameSections  then
    lbNameSectionsClick(nil)
  else
    lbValueSectionsClick(nil);
end;

procedure Tform_main.acAddSectionListExecute(Sender: TObject);
begin
  EditSectionList(True);
end;

procedure Tform_main.acAddSectionListUpdate(Sender: TObject);
begin
   acAddSectionList.Enabled := CheckSectionValues(True);
end;

procedure Tform_main.acAddSeriesExecute(Sender: TObject);
var
  G : TCWGraph;
begin
   if (CW.Chart is TCWSpanChart) or (CW.Chart is TCWGeneralChart) then
     G := TCWCurve.Create(Self)
   else if CW.Chart is TCWCategoryBarChart then
     G := TCWBar.Create(Self)
   else
     G := TCWPie.Create(Self);
   if G is TCWPie then
     CW.Chart.AddSeriesDef('New series', G, clBlack, vsNone)
   else
     CW.Chart.AddSeriesDef('New series', G, clBlack, vsValueScale1);
   FillChartControls;
end;

procedure Tform_main.acAddSeriesUpdate(Sender: TObject);
var
 Enabl : Boolean;
begin
   Enabl := (CW.Count = 0) and (CW.Chart <> nil) and not CW.OverflowError;
   acAddSeries.Enabled  := Enabl;
end;

procedure Tform_main.acUpdateSectionExecute(Sender: TObject);
begin
  EditSectionList(false);
end;

procedure Tform_main.acUpdateSectionUpdate(Sender: TObject);
begin
   acUpdateSection.Enabled := CheckSectionValues(False);
end;

procedure Tform_main.acVLabelFontAccept(Sender: TObject);
begin
    if Sender = acVLabelFont then
      CW.ValueScale1.Font.Assign(acVLabelFont.Dialog.Font)
    else if Sender = acV2LabelFont then
      CW.ValueScale2.Font.Assign(acV2LabelFont.Dialog.Font)

end;

procedure Tform_main.acVLabelFontBeforeExecute(Sender: TObject);
begin
  if Sender = acVLabelFont then
    acVLabelFont.Dialog.Font.Assign(CW.ValuesCale1.Font)
  else if Sender = acV2LabelFont then
    acV2LabelFont.Dialog.Font.Assign(CW.ValuesCale2.Font)
end;

procedure Tform_main.Button1Click(Sender: TObject);
var
 Res : integer;
 C : TChartWriter;

begin
  //c := TChartWriter.Create(Curve);
  //c.Parent := Curve;
   //CW.ChartList.Next;
//   Res := CountWholeMonths(StrToDateTime('1.4.2000'), StrToDatetIme('1.5.2003'));
   //Co2;
   //CW.Chart := CWSpanchart1;
   //CW.LoadFromFile('C:\Users\47994\Documents\Embarcadero\Studio\Projects\CWDemo\Graphs\COGraph2.CWR');
  // CW.SaveToFile('C:\Users\47994\Documents\Embarcadero\Studio\Projects\CWDemo\Graphs\COGraph2.CWR', sfRich);
end;

procedure Tform_main.sbMoreNSectionsClick(Sender: TObject);
begin
   formSections.SectionKind := CW.NameSectionDefs;
   formSections.ShowModal;
end;

procedure Tform_main.sbMoreVSectionsClick(Sender: TObject);
begin
   formSections.SectionKind := CW.ValueSectionDefs;
   formSections.ShowModal;
end;

procedure Tform_main.cbAutoSectionsChange(Sender: TObject);
begin
   if CW.NameSectionDefs = nil then
     raise Exception.Create('No name sections defined');
   if cbAutoSections.ItemIndex > 0 then
   begin
       cbTimeTemplate.ItemIndex := 0;
       panNameSectionValues.Enabled := false;
   end
   else
     panNameSectionValues.Enabled := True;
   if CW.Chart <> nil then
   begin
    CW.BeginUpdate;
    try
     if cbAutoSections.ItemIndex > 0 then
       cbTimeTemplateChange(nil);
     CW.NameSectionDefs.AutoSections := TAutoSections(cbAutoSections.ItemIndex);
    finally
      CW.EndUpdate;
    end;
   end;

end;

procedure Tform_main.cbAxisColorChange(Sender: TObject);
begin
  if CW.Chart <> nil then
  begin
    CW.WallBorderColor := cbAxisColor.Selected;
  end;
end;

procedure Tform_main.cbAxisOrientationChange(Sender: TObject);
begin
  if CW.Chart <> nil then
  begin
    CW.AxisOrientation := TAxisOrientation(cbAxisOrientation.ItemIndex);
  end;
end;

procedure Tform_main.cbBGColorChange(Sender: TObject);
begin
    CW.GraphBGColor := cbBGColor.Selected;
end;

procedure Tform_main.cbBordersChange(Sender: TObject);
begin
    CW.GraphBorders := TGraphBorders(cbBorders.ItemIndex);
end;

procedure Tform_main.cbChartItemColorChange(Sender: TObject);
var
  Indx : integer;
begin
  Indx := lbItems.ItemIndex;
  if Indx <> -1 then
  begin
   CW.Categories.Items[Indx].Color := cbChartItemColor.Selected;
   lbItems.Repaint;
  end;
end;

procedure Tform_main.cbChartSeriesColorChange(Sender: TObject);
begin
   CW.Chart.SeriesDefs[SerIndex].Color := cbChartSeriesColor.Selected;
end;

procedure Tform_main.cbCurvePointMarkersChange(Sender: TObject);
begin
   FActiveGraphControls.Update(Sender);
end;

procedure Tform_main.cbCurveStatLineChange(Sender: TObject);
begin
 CW.BeginUpdate;
   with FGraph1 as TCWAxisGraph do
     Statline := TStatLine(cbCurveStatLine.ItemIndex);
   if FGraph2 <> nil then
     with FGraph2 as TCWAxisGraph do
     Statline := TStatLine(cbCurveStatLine.ItemIndex);
 CW.EndUpdate;

end;

procedure Tform_main.cbCurveStyleChange(Sender: TObject);
begin
    if (cbCurveStyle.ItemIndex = 3) and (CW.Count = 1) then
    begin
      ShowMessage('This style cannot be applied on single series charts');
      FCurveControls.SyncSeriesStyles;
    end;
end;

procedure Tform_main.cbLegAlignmentChange(Sender: TObject);
begin
    if CW.Chart <> nil then
   begin
     CW.Chart.Legends.Items[lbLegends.ItemIndex].Legend.Alignment :=
      TLegendAlignment(cbLegAlignment.ItemIndex);
   end;
end;

procedure Tform_main.cbLegAnchoringChange(Sender: TObject);
var
  Indx : integer;
begin
   Indx := lbLegends.ItemIndex;
   if (CW.Chart <> nil) and (CW.Chart.Legends.Count > 0) then
   begin
      if (CW.Chart is TCWPieChart) then
      begin
           if TLegendAnchoring(cbLegAnchoring.ItemIndex) = anPointInside then
           begin
               cbLegAnchoring.ItemIndex := Ord(CW.Chart.Legends.Items[Indx].Legend.Anchoring);
               raise Exception.Create('Point inside is not allowed wih pie charts');
           end;
      end
      else if (cbLegAnchoring.ItemIndex = 9) and (eLegPointName.Text = '') then
      begin
        cbLegAnchoring.ItemIndex := Ord(CW.Chart.Legends.Items[Indx].Legend.Anchoring);
        raise Exception.Create('This option needs a PointName');
      end;

      try
      CW.Chart.Legends.Items[Indx].Legend.Anchoring := TLegendAnchoring(cbLegAnchoring.ItemIndex);
      except
        cbLegAnchoring.ItemIndex := Ord(CW.Chart.Legends.Items[Indx].Legend.Anchoring);
        raise;
      end;
   end;
   SetLegEnables(CW.Chart.Legends.Items[Indx].Legend);
end;

procedure Tform_main.cbLegBulletsChange(Sender: TObject);
begin
if CW.Chart <> nil then
   begin
     CW.Chart.Legends.Items[lbLegends.ItemIndex].Legend.Bullets := TLegendBullets(cbLegBullets.ItemIndex);
   end;
end;

procedure Tform_main.cbLegContentFlowChange(Sender: TObject);
var
  Indx : integer;
begin
   if (CW.Chart <> nil) and (CW.Chart.Legends.Count > 0) then
   begin
      Indx := lbLegends.ItemIndex;
      CW.Chart.Legends.Items[Indx].Legend.ContentFlow := TContentFlow(cbLegContentFlow.ItemIndex);
   end;
end;

procedure Tform_main.cbLineShapeChange(Sender: TObject);
begin
   FCurveControls.Update(cbLineShape);
end;

procedure Tform_main.cbMouseInfoChange(Sender: TObject);
begin
    CW.MouseInfo := TMouseInfo(cbMouseInfo.ItemIndex);
end;

procedure Tform_main.cbNeighborAreaColorChange(Sender: TObject);
begin
   FCurveControls.Update(Sender);
end;

procedure Tform_main.FileOpen1Accept(Sender: TObject);
begin
  CW.LoadFromFile(FileOpen1.Dialog.FileName, True);
  //CW.ChartList.Add(CW.Chart);
  //FillChartList;
  //OnLoadChart;
end;

procedure Tform_main.FileSaveAs1Accept(Sender: TObject);
begin
    CW.SaveToFile(FileSaveAs1.Dialog.FileName, sfRich, ttUnix);
end;

procedure Tform_main.FileSaveAs1Update(Sender: TObject);
begin
    FileSaveAs1.Enabled := CW.Chart <> nil;
end;

procedure Tform_main.FormCreate(Sender: TObject);
begin
//
   FCurveControls := TCurveControls.Create(nil, Self);
   FBarControls := TBarControls.Create(nil, Self);
   FPieControls := TPieControls.Create(nil, Self);
   CW.ChartList.OnChange := ChartChange;
   CW.BezierMargin := 30;
end;

procedure Tform_main.FormDestroy(Sender: TObject);
begin
//
  FCurveControls.Free;
  FBarControls.Free;
  FPieControls.Free;
end;

function Tform_main.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp := True;
end;

procedure Tform_main.lbApplyOnClick(Sender: TObject);
begin
    FCurveControls.FStyleIndex := lbApplyOn.ItemIndex-1;
    FCurveControls.SyncSeriesStyles;
end;

procedure Tform_main.lbChartsClick(Sender: TObject);
begin
   CW.ChartList.ItemIndex := lbCharts.ItemIndex;
   OnLoadChart;
end;

procedure Tform_main.lbItemsClick(Sender: TObject);
begin
  SyncItemControls(lbItems.ItemIndex);
end;

procedure Tform_main.lbItemsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  R : TRect;
  Clr : TColor;
  Nm : string;
begin
   Clr := CW.Categories[Index].Color;
   Nm := CW.Categories[Index].CategoryName;
   with (Control as TListBox).Canvas do
   begin
     if odSelected in State then
     begin
       Brush.Color := clHighlight;
       Brush.Style := bsSolid;
       FillRect(Rect);
       Font.Color := clHighLightText;
     end
     else
     begin
      Brush.Color := clWindow;
      FillRect(Rect);
      Font.Color := clWindowText;
     end;
     Brush.Color := Clr;
     R := Rect;
     R.Top := Rect.Top + 2;
     R.Bottom := Rect.Bottom-2;
     R.Right := R.Left + 14;
     R.Left := Rect.Left + 2;
     Rectangle(R);
     Brush.Style := bsClear;
     TextOut(Rect.Left + 16, Rect.Top, Nm)
   end;
end;

procedure Tform_main.lbLegendsClick(Sender: TObject);
begin
  SyncLegends(lbLegends.ItemIndex);
end;

procedure Tform_main.lbNameSectionsClick(Sender: TObject);
begin
  SyncSectionValues(lbNameSections);
end;

procedure Tform_main.lbSeriesClick(Sender: TObject);
begin
  SyncSeriesControls(lbSeries.ItemIndex);
end;

procedure Tform_main.lbValueSectionsClick(Sender: TObject);
begin
  SyncSectionValues(lbValueSections);
end;

procedure Tform_main.lbxBarOptionsClickCheck(Sender: TObject);
begin
   FBarControls.Update(cbLayout);
end;

procedure Tform_main.lbxLegContentClickCheck(Sender: TObject);
var
  Indx : integer;
  Elems : TLegendContents;
  procedure SetElem(Checked : Boolean; Elem : TLegendContents);
  begin
    if Checked then
      Elems := Elems + Elem
    else
      Elems := Elems - Elem;
  end;
begin
   if (CW.Chart <> nil) and (CW.Chart.Legends.Count > 0) then
   begin
      Indx := lbLegends.ItemIndex;
      Elems := CW.Chart.Legends.Items[Indx].Legend.Contents;

      case lbxLegContent.ItemIndex of
      0: SetElem(lbxLegContent.Checked[0], [coSeriesTitle]);
      1: SetElem(lbxLegContent.Checked[1], [coValue]);
      2: SetElem(lbxLegContent.Checked[2], [coName]);
      3: SetElem(lbxLegContent.Checked[3], [coNameSpan]);
      end;
      CW.Chart.Legends.Items[Indx].Legend.Contents:= Elems;
   end;
end;

procedure Tform_main.lbxTextTiltingClickCheck(Sender: TObject);
var
  Elems : TTextOrientations;
  procedure SetElem(Checked : Boolean; Elem : TTextOrientations);
  begin
    if Checked then
      Elems := Elems + Elem
    else
      Elems := Elems - Elem;
  end;
begin
 if CW.Chart = nil then
     Exit;
 Elems := CW.TextTilting;
 case lbxTextTilting.ItemIndex of
    0: SetElem(lbxTextTilting.Checked[0], [toName]);
    1: SetElem(lbxTextTilting.Checked[1], [toValue]);
    2: SetElem(lbxTextTilting.Checked[2], [toSection]);
 end;
 CW.TextTilting := Elems;
end;

procedure Tform_main.memLegTextExit(Sender: TObject);
var
  Indx : integer;
begin
   if (CW.Chart <> nil) and (CW.Chart.Legends.Count > 0) then
   begin
      Indx := lbLegends.ItemIndex;
      CW.Chart.Legends.Items[Indx].Legend.Text := memLegText.Lines;
   end;
end;

procedure Tform_main.FillChartList;
var
 i : integer;
 CL : TChartList;
begin
   CL := CW.ChartList;
   lbCharts.Clear;
   for I := 0 to cl.Count-1 do
     begin
       lbCharts.Items.AddObject(cl.Items[i].Title, cl.Items[i])
     end;
     lbCharts.ItemIndex := CW.ChartList.ItemIndex;
end;

function TForm_main.SelGraph : TCWGraph;
 function InGraphs(Cl : TClass) : TCWGraph;
 begin
    if (FGraph1 <> nil) and (FGraph1 is Cl) then
      Result := FGraph1
    else if (FGraph2 <> nil) and (FGraph2 is Cl) then
      Result := FGraph2
    else
     Result := nil;
 end;

begin
   if pgGraphs.ActivePage = Curve then
     Result := InGraphs(TCWCurve)
   else if pgGraphs.ActivePage = Bar then
     Result := InGraphs(TCWBar)
   else if pgGraphs.ActivePage = Pie then
     Result := InGraphs(TCWPie)
   else
     Result := nil;
end;

procedure Tform_main.FillChartControls;
var
  i : integer;
  E : TNotifyEvent;

begin
  if CW.Chart <> nil then
  begin
    //SetStates;
  end
  else
  begin
    lbCharts.Clear;
    eChartTitle.Text := '';
    cbBgColor.Selected := clWindow;
    cbBorders.ItemIndex := 0;
    Exit;
  end;
  eChartTitle.Text := CW.Chart.Title;
  lbSeries.Clear;
  for i := 0 to CW.Chart.SeriesDefs.Count-1 do
  begin
    if CW.Chart.SeriesDefs[i].Title = '' then
      lbSeries.Items.Add('Series' + IntToStr(i+1))
    else
     lbSeries.Items.Add(CW.Chart.SeriesDefs[i].Title);
  end;
  lbSeries.Selected[0] := True;
  if CW.Chart is TCWPieChart then
    cbScale.ItemIndex := -1
  else if CW.Chart is TCWCategoryBarChart then
    cbScale.ItemIndex := 1
  else
    cbScale.ItemIndex := Ord(CW.Chart.SeriesDefs[0].ValueScale);

  {Items}
  lbItems.Clear;
  if CW.Chart is TCWCategoryChart then
  for i := 0 to CW.Categories.Count-1 do
  begin
    if CW.Categories[i].CategoryName = '' then
      lbItems.Items.Add('Item' + IntToStr(i+1))
    else
      lbItems.Items.Add(CW.Categories[i].CategoryName);
  end;

  { Scales }
  if CW.IsAxisChart then
  begin
    eHighValue.Text := FormatNum(CW.ValueScale1.ValueHigh, CW.ValueScale1.ValuePrecision);
    eLowValue.Text := FormatNum(CW.ValueScale1.ValueLow, CW.ValueScale1.ValuePrecision);
    eValueIntervals.Text := FormatNum(CW.ValueScale1.ValueIntervals, CW.ValueScale1.ValuePrecision);
    E := DisableCBXEvent(cbxAutoSpan);
    cbxAutoSpan.Checked := CW.ValueScale1.ValueSpanFromData;
    cbxAutoSpan.OnClick := E;
    ePrecision1.Text := IntToStr(CW.ValueScale1.ValuePrecision);
    if not (CW.Chart is TCWCategoryBarChart) then
    begin
      eHighValue2.Text := FormatNum(CW.ValueScale2.ValueHigh, CW.ValueScale2.ValuePrecision);
      eLowValue2.Text := FormatNum(CW.ValueScale2.ValueLow, CW.ValueScale2.ValuePrecision);
      eValueIntervals2.Text := FormatNum(CW.ValueScale2.ValueIntervals, CW.ValueScale2.ValuePrecision);
      E := DisableCBXEvent(cbxAutoSpan2);
      cbxAutoSpan2.Checked := CW.ValueScale2.ValueSpanFromData;
      cbxAutoSpan2.OnClick := E;
      ePrecision2.Text := IntToStr(CW.ValueScale2.ValuePrecision);
    end;

    eNamePrecision.Text := IntToStr(CW.Namescale.NumSpanPrecision);
    cbOverflowAction.ItemIndex := Ord(CW.NameScale.OverflowAction);
    Names.FillControls;
    E := DisableCBXEvent(cbxImageLabels);
    cbxImageLabels.Checked := CW.NameScale.AllowImages;
    cbxImageLabels.OnClick := E;
    V1.FillControls;
    if not (CW.Chart is TCWCategoryBarChart) then
     V2.FillControls;
  end;

  cbSpanType.ItemIndex := 6; { Init to not spanned}
  if CW.Chart is TCWSpanChart then
  with CW.Chart as TCWSpanChart do
  begin
    cbSpanType.ItemIndex := Ord(SpanType);
  end;


  { Axis profile }
  if CW.IsAxisChart then
  begin
    cbAxisOrientation.ItemIndex := Ord(CW.AxisOrientation);
    cbAxisColor.Selected := CW.WallBorderColor;
    if toName in CW.TextTilting then
      lbxTextTilting.Checked[0] := True;
    if toValue in CW.TextTilting then
      lbxTextTilting.Checked[1] := True;
    if toSection in CW.TextTilting then
      lbxTextTilting.Checked[2] := True;
    eWallWidth.Text := IntToStr(CW.WallWidth);
    cbWallColor.Selected := CW.WallColor;
    E := DisableCBXEvent(cbxGradientWall);
    cbxGradientWall.Checked := CW.GradientWall;
    cbxGradientWall.OnClick := E;
    cbBorders.ItemIndex := Ord(CW.GraphBorders);
  end;


  lbTitleFont.Caption := CW.TitleFont.Name + ' ' + IntToStr(CW.TitleFont.Size);
  lbTitleFont.Font.Color := CW.TitleFont.Color;
  cbTitleAlignment.ItemIndex := Ord(CW.TitleAlignment);
  cbBGColor.Selected := CW.GraphBGColor;

  cbRulers.ItemIndex := Ord(TRulers(CW.Rulers));
  cbMouseInfo.ItemIndex := Ord(TMouseInfo(CW.MouseInfo));
  eIMargLeft.Text := IntToStr(CW.Chart.InnerMargins.Left);
  eIMargTop.Text := IntToStr(CW.Chart.InnerMargins.Top);
  eIMargRight.Text := IntToStr(CW.Chart.InnerMargins.Right);
  eIMargBottom.Text := IntToStr(CW.Chart.InnerMargins.Bottom);
  eGMargLeft.Text := IntToStr(CW.Chart.GraphMargins.Left);
  eGMargTop.Text := IntToStr(CW.Chart.GraphMargins.Top);
  eGMargRight.Text := IntToStr(CW.Chart.GraphMargins.Right);
  eGMargBottom.Text := IntToStr(CW.Chart.GraphMargins.Bottom);
  E := DisableCBXEvent(cbxCenterChart);
  cbxCenterChart.Checked := CW.Centered;
  cbxCenterChart.OnClick := E;
  cbxLiveResize.Checked := CW.LiveResize;

  { Sections}
  if CW.IsAxisChart then
  begin
    lbNameSections.Clear;
    if CW.NameSectionDefs <> nil then
    begin
      cbAutoSections.ItemIndex := Ord(CW.NameSectionDefs.AutoSections);
      if cbAutoSections.ItemIndex > 0 then
        panNameSectionValues.Enabled := false;
      cbTimeTemplate.ItemIndex := Ord(CW.NameSectionDefs.DateTimeTemplate);
      E := DisableCBXEvent(cbxVSectionsVisible);
      cbxVSectionsVisible.Checked := CW.NameSectionDefs.Visible;
      cbxVSectionsVisible.OnClick := E;
      E := DisableCBXEvent(cbxNSectionsVisible);
      cbxNSectionsVisible.Checked := CW.NameSectionDefs.Visible;
      cbxNSectionsVisible.OnClick := E;
      for I := 0 to CW.NameSectionDefs.Sections.Count-1 do
        begin
            lbNameSections.Items.Add(CW.NameSectionDefs.Sections.Items[i].LongCaption);
        end;
        if lbNameSections.Count > 0 then
        begin
          lbNameSections.Selected[0] := True;
          SyncSectionValues(lbNameSections);
        end;
    end
    else
    begin
      cbAutoSections.ItemIndex := 0;
      cbTimeTemplate.ItemIndex := 0;
    end;

    lbValueSections.Clear;
    if CW.ValueSectionDefs <> nil then
    begin
      for I := 0 to CW.ValueSectionDefs.Sections.Count-1 do
      begin
          lbValueSections.Items.Add(CW.ValueSectionDefs.Sections.Items[i].LongCaption);
      end;
      if lbValueSections.Count > 0 then
      begin
        lbValueSections.Selected[0] := True;
        SyncSectionValues(lbValueSections);
      end;
    end;
  end;

  {Legends}
  lbLegends.Clear;
  eLegHorizMargins.Text := '';
  eLegVertMargins.Text := '';
  cbLegAlignment.ItemIndex := 0;
  cbLegBullets.ItemIndex := 0;
  cbLegAnchoring.ItemIndex := 4;
  lbxLegContent.CheckAll(cbUnChecked);
  cbLegContentFlow.ItemIndex := 0;
  memLegText.Lines.Clear;

  for I := 0 to CW.Chart.Legends.Count-1 do
  begin
    lbLegends.Items.Add('Legend' + IntToStr(i+1));
  end;
  if (CW.Chart.Legends.Count > 0) then
  begin
   lbLegends.Selected[0] := True;
   SyncLegends(lbLegends.ItemIndex);
  end;

end;

procedure Tform_main.SyncSeriesControls(SerNr: Integer);
var
  E : TNotifyEvent;
begin
  cbChartSeriesColor.Selected := CW.Chart.SeriesDefs[SerNr].Color;
  eSeriesTitle.Text := CW.Chart.SeriesDefs[SerNr].Title;
  E := DisableCBXEvent(cbxChartVisible);
  cbxChartVisible.Checked := CW.Chart.SeriesDefs[SerNr].Visible;
  cbxChartVisible.OnClick := E;
  if CW.Chart.AxisCount = 1 then
    cbScale.ItemIndex := Ord(vsValueScale1)
  else if CW.Chart is TCWPieChart then
    cbScale.ItemIndex := -1
  else
  begin
    cbScale.ItemIndex := Ord(CW.Chart.SeriesDefs[SerNr].ValueScale);
  end;

  if CW.Chart.SeriesDefs[SerNr].Graph is TCWCurve then
    cbSeriesGraphType.ItemIndex := 0
  else if CW.Chart.SeriesDefs[SerNr].Graph is TCWBar then
    cbSeriesGraphType.ItemIndex := 1
  else
   cbSeriesGraphType.ItemIndex := 2;


end;

procedure Tform_main.tbStatBGBlendingChange(Sender: TObject);
begin
  FActiveGraphControls.Update(Sender);
 CW.BeginUpdate;
   with FGraph1 as TCWAxisGraph do
   begin
     if FGraph2 <> nil then
      StatBackgroundBlending := tbStatBGBlending.Position div 2
     else
      StatBackgroundBlending := tbStatBGBlending.Position;
   end;
   if FGraph2 <> nil then
     with FGraph2 as TCWAxisGraph do
     StatBackgroundBlending := tbStatBGBlending.Position div 2;
 CW.EndUpdate;
end;

procedure Tform_main.SyncItemControls(ItmNr: Integer);
begin
  if not (CW.Chart is TCWCategoryChart) then
    Exit;
  if ItmNr > CW.Categories.Count-1 then
    Exit;

  cbChartItemColor.Selected := CW.Categories[ItmNr].Color;
  eItemName.Text := CW.Categories[ItmNr].CategoryName;

  if CW.Categories[ItmNr].Image.Graphic <> nil then
   imgLabel.Picture.Assign(CW.Categories[ItmNr].Image)
  else
   imgLabel.Picture.Graphic := nil;

end;

procedure TForm_main.SyncSectionValues(AList : TListBox);
var
  Indx : integer;
  SVal, EVal, LCap, SCap : TEdit;
  SDef : TCWSectionDefs;
begin
  Indx := AList.ItemIndex;
  if AList = lbNameSections then
  begin
    SVal := eNStartValue;
    EVal := eNEndValue;
    LCap := eNLongCaption;
    SCap := eNShortCaption;
    SDef := CW.NameSectionDefs;
  end
  else
  begin
    SVal := eVStartValue;
    EVal := eVEndValue;
    LCap := eVLongCaption;
    SCap := eVShortCaption;
    SDef := CW.ValueSectionDefs;
  end;
  SVal.Text := SDef.Sections.Items[Indx].StartValue;
  EVal.Text := SDef.Sections.Items[Indx].EndValue;
  LCap.Text := SDef.Sections.Items[Indx].LongCaption;
  SCap.Text := SDef.Sections.Items[Indx].ShortCaption;
end;

procedure TForm_main.SyncLegends(Indx: Integer);
var
  E : TNotifyEvent;
begin
    cbLegAnchoring.ItemIndex := Ord(CW.Chart.Legends.Items[Indx].Legend.Anchoring);
    cbLegContentFlow.ItemIndex := Ord(CW.Chart.Legends.Items[Indx].Legend.ContentFlow);
    if coSeriesTitle in CW.Chart.Legends.Items[Indx].Legend.Contents then
     lbxLegContent.Checked[0] := True;
    if coValue in CW.Chart.Legends.Items[Indx].Legend.Contents then
     lbxLegContent.Checked[1] := True;
    if coName in CW.Chart.Legends.Items[Indx].Legend.Contents then
     lbxLegContent.Checked[2] := True;
    if coNameSpan in CW.Chart.Legends.Items[Indx].Legend.Contents then
     lbxLegContent.Checked[3] := True;
    E := DisableCBXEvent(cbxLegVisible);
    cbxLegVisible.Checked := CW.Chart.Legends.Items[Indx].Legend.Visible;
    cbxLegVisible.OnClick := E;
    eLegHorizMargins.Text := IntToStr(CW.Chart.Legends.Items[Indx].Legend.HorizMargins);
    eLegVertMargins.Text := IntToStr(CW.Chart.Legends.Items[Indx].Legend.VertMargins);
    cbLegBullets.ItemIndex := Ord(CW.Chart.Legends.Items[Indx].Legend.Bullets);
    cbLegAlignment.ItemIndex := Ord(CW.Chart.Legends.Items[Indx].Legend.Alignment);
    memLegText.Lines.Assign(CW.Chart.Legends.Items[Indx].Legend.Text);
    eLegPointName.Text := CW.Chart.Legends.Items[Indx].Legend.PointName;

    SetLegEnables(CW.Chart.Legends.Items[Indx].Legend);
end;

procedure Tform_main.cbOverflowActionChange(Sender: TObject);
begin
   if CW.Chart <> nil then
   try
     CW.NameScale.OverflowAction := TOverflowAction(cbOverflowAction.ItemIndex);
   except
     cbOverflowAction.ItemIndex := Ord(CW.NameScale.Overflowaction);
     raise;
   end;
   SetStates;
end;

procedure Tform_main.cbPieStyleChange(Sender: TObject);
begin
    FActiveGraphControls.Update(nil);
end;

procedure Tform_main.cbRulersChange(Sender: TObject);
begin
   CW.Rulers := TRulers(cbRulers.ItemIndex);
end;

procedure Tform_main.cbSeriesGraphTypeChange(Sender: TObject);
var
  G, GOld :TCWGraph;

begin
   if cbSeriesGraphType.ItemIndex = 0 then
     G := TCWCurve.Create(Self)
   else
     G := TCWBar.Create(Self);
   { Pies can never change}
  GOld := CW.Chart.SeriesDefs[lbSeries.ItemIndex].Graph;
  CW.Chart.SeriesDefs[lbSeries.ItemIndex].Graph := G;
  if GOld = FGraph1 then
    FGraph1 := G
  else
    FGraph2 := G;
  SetVisibleTabs(True);
  if FGraph1 is TCWCurve then
  begin
   FCurveControls.Graph := FGraph1;
   FActiveGraphControls := FCurveControls;
  end
  else
  begin
   FBarControls.Graph := FGraph1;
   FActiveGraphControls := FBarControls;
  end;
  FActiveGraphControls.DoFill;

end;

procedure Tform_main.cbTimeTemplateChange(Sender: TObject);
begin
   if cbTimeTemplate.ItemIndex > 0 then
     cbAutoSections.ItemIndex := 0;
   if CW.Chart <> nil then
   begin
    CW.BeginUpdate;
    try
     if cbTimeTemplate.ItemIndex > 0 then
       cbAutoSectionsChange(nil);
     CW.NameSectionDefs.DateTimeTemplate := TDateTimeTemplate(cbTimeTemplate.ItemIndex);
    finally
      CW.EndUpdate;
    end;
   end;
end;

procedure Tform_main.cbTitleAlignmentChange(Sender: TObject);
begin
    CW.TitleAlignment := TAlignment(cbTitleAlignment.ItemIndex);
end;

procedure Tform_main.cbWallColorChange(Sender: TObject);
begin
    if CW.Chart <> nil then
     CW.WallColor := cbWallColor.Selected;
end;

procedure Tform_main.cbxAutoSizeClick(Sender: TObject);
begin
  with SelGraph as TCWBar do
   AutoSize := TCheckBox(Sender).Checked;
end;

procedure Tform_main.cbxAutoSpanClick(Sender: TObject);
begin
  if CW.Chart <> nil then
     CW.ValueScale1.ValueSpanFromData := cbxAutoSpan.Checked;
end;

procedure Tform_main.cbxCenterChartClick(Sender: TObject);
begin
  CW.Centered := cbxCenterChart.Checked;
end;

procedure Tform_main.cbxChartVisibleClick(Sender: TObject);
var
  Indx : integer;
begin
  Indx := lbSeries.ItemIndex;
  CW.Chart.SeriesDefs[Indx].Visible := cbxChartVisible.Checked;
end;

procedure Tform_main.cbxCurveAnimationClick(Sender: TObject);
begin
   TCWCurve(SelGraph).Animation := cbxCurveAnimation.Checked;
   if TCWCurve(SelGraph).Animation then
    if CW.Chart.PerformAnimation <> AN_OK then
      raise Exception.Create('Failed');
end;

procedure Tform_main.cbxCurveBeaconPointsClick(Sender: TObject);
begin
   FCurveControls.Update(Sender);
end;

procedure Tform_main.cbxGradientWallClick(Sender: TObject);
begin
    if CW.Chart <> nil then
     CW.GradientWall := cbxGradientWall.Checked;
end;

procedure Tform_main.cbxImageLabelsClick(Sender: TObject);
begin
  CW.NameScale.AllowImages := cbxImageLabels.Checked;
end;

procedure Tform_main.cbxCurveKeepFontColorClick(Sender: TObject);
begin
  SelGraph.KeepFontColor := TCheckBox(Sender).Checked;
end;

procedure Tform_main.cbxLegVisibleClick(Sender: TObject);
begin
  CW.Chart.Legends.Items[lbLegends.ItemIndex].Legend.Visible := cbxLegVisible.Checked;
end;

procedure Tform_main.cbxLiveResizeClick(Sender: TObject);
begin
    CW.LiveResize := cbxLiveResize.Checked;
end;

procedure Tform_main.cbxNSectionsVisibleClick(Sender: TObject);
begin
  CW.NameSectionDefs.Visible := cbxNSectionsVisible.Checked;
end;

procedure Tform_main.cbxPieAnimationClick(Sender: TObject);
begin
   TCWPie(SelGraph).Animation := cbxPieAnimation.Checked;
   if TCWPie(SelGraph).Animation then
    if CW.Chart.PerformAnimation <> AN_OK then
      raise Exception.Create('Failed');
end;

procedure Tform_main.cbxVSectionsVisibleClick(Sender: TObject);
begin
   CW.ValueSectionDefs.Visible := cbxVSectionsVisible.Checked;
end;

procedure Tform_main.CWAfterBuildChart(Sender: TObject);
begin
   Exit;
end;

procedure Tform_main.CWDataChange(Sender: TObject);
begin
   if not FSizing then
   begin
     OnLoadChart;
   end;
end;

procedure Tform_main.CWQuerySpace(Sender: TObject; const QueryResult: Integer;
  var Response: TSpaceResponse);
begin
  if QueryResult = -1 then
  begin
    MessageDlg('Sorry, your CW window is too small to render this diagram. ' +
    'Try expanding the window or choosing an overflow action to solve the problem. '+
    'Retry by clicking the Reload button', mtInformation, [mbOK], 0, mbOK);
    Response := srRefuseAbort;
  end
  else
    Response := srAccept;
end;

function TForm_main.DisableCBXEvent(ACBX : TObject) : TNotifyEvent;
begin
  if ACBX is TCheckBox then
  begin
    Result := TCheckBox(ACBX).OnClick;
    TCheckBox(ACBX).OnClick := nil;
  end
  else
  begin
    Result := TCheckListBox(ACBX).OnClickCheck;
    TCheckListBox(ACBX).OnClickCheck := nil;
  end;
end;

procedure TForm_main.SetPenProps(APen : TPen);
begin
     formPen.CW := CW;
     formPen.cbColor.Selected := APen.Color;
     formPen.eWidth.Text := IntToStr(APen.Width);
     if APen.Style = psSolid then
       formPen.cbStyle.ItemIndex := 0
     else if APen.Style = psDot then
       formPen.cbStyle.ItemIndex := 1
     else
       formPen.cbStyle.ItemIndex := 2; {Dash}

     if formPen.ShowModal = mrOK then
     begin
      CW.BeginUpdate;
       APen.Width := StrToInt(formPen.eWidth.Text);
       APen.Color := formPen.cbColor.Selected;
       if formPen.cbStyle.ItemIndex = 0 then
        APen.Style := psSolid
       else if formPen.cbStyle.ItemIndex = 1 then
        APen.Style := psDot
       else
        APen.Style := psDash;
      CW.EndUpdate;
     end;
end;

procedure TForm_main.OnLoadChart;
begin
  CW.Chart.GetUniqueGraphs(FGraph1, FGraph2);
  if FGraph1 is TCWCurve then
  begin
    FCurveControls.Graph := FGraph1;
    FActiveGraphControls := FCurveControls;
    FCurveControls.FStyleIndex := 0;
    FCurveControls.SyncSeriesStyles;
  end
  else if FGraph1 is TCWBar then
  begin
    FBarControls.Graph := FGraph1;
    FActiveGraphControls := FBarControls;
  end
  else if FGraph1 is TCWPie then
  begin
    FActiveGraphControls := FPieControls;
    FPieControls.Graph := FGraph1;
    FGraph1.KeepFontColor := True;
  end;

  if (FGraph2 <> nil)  then
  begin
    if FGraph2 is TCWCurve then
    begin
      FCurveControls.Graph := FGraph2;
      FCurveControls.FStyleIndex := 0;
      FCurveControls.SyncSeriesStyles;
    end
    else if FGraph2 is TCWBar then
    begin
      FBarControls.Graph := FGraph2;
    end
    else if FGraph2 is TCWPie then {Cannot happen}
    begin
      FPieControls.Graph := FGraph2;
      FGraph1.KeepFontColor := True;
    end;
  end;

  FillChartControls;
  SyncSeriesControls(0);
  SyncItemControls(0);
  if lbItems.Count > 0 then
    lbItems.ItemIndex := 0;
  FActiveGraphControls.DoFill;
  SetVisibleTabs;
  if FGraph1 is TCWCurve then
   pgGraphs.ActivePage := Curve
  else if FGraph1 is TCWBar then
   pgGraphs.ActivePage := Bar
  else
   pgGraphs.ActivePage := Pie;
  pgChartDefs.Enabled := True;

  SetStates;
end;

procedure TForm_main.SetVisibleTabs(GraphsOnly : Boolean = false);
begin
  if FGraph1 = nil then
  begin
    pgGraphs.Visible := false;
    Exit;
  end;
  pgGraphs.Visible := True;
  Curve.TabVisible := false;
  Bar.TabVisible := false;
  Pie.TabVisible := false;
  tabStats.TabVisible := false;

  if FGraph1 is TCWCurve then
    Curve.TabVisible := True
  else if FGraph1 is TCWBar then
    Bar.TabVisible := True
  else if FGraph1 is TCWPie then
    Pie.TabVisible := True;

  if FGraph2 <> nil then
  begin
   if FGraph2 is TCWCurve then
    Curve.TabVisible := True
  else if FGraph2 is TCWBar then
    Bar.TabVisible := True
  else if FGraph2 is TCWPie then
    Pie.TabVisible := True;
  end;

  if GraphsOnly then
   Exit;

  tabScales.TabVisible := false;
  tabAxisAttributes.TabVisible := True;
  tabSections.TabVisible := false;
  tabItems.TabVisible := True;
  tabValueScale2.TabVisible := False;
  tabChartGeneral.TabVisible := false;
  tabNameSections.TabVisible := False;
  tabValueSections.TabVisible := False;
  //tabLegends.TabVisible := false;

  if CW.Chart.Legends.Count > 0 then
   tabLegends.TabVisible := True;

  if (CW.Chart is TCWSpanChart) or (CW.Chart is TCWGeneralChart) then
  begin
    tabItems.TabVisible := false;
    tabScales.TabVisible := True;
    tabStats.TabVisible := True;
    if CW.Chart.AxisCount = 2 then
      tabValueScale2.TabVisible := True;

    if (CW.NameSectionDefs <> nil)
      or (CW.ValueSectionDefs <> nil) then
      begin
        tabSections.TabVisible := True;
        if (CW.NameSectionDefs <> nil) then
          tabNameSections.TabVisible := True;
        if (CW.ValueSectionDefs <> nil) then
          tabValueSections.TabVisible := True;
      end;
  end
  else if CW.Chart is TCWCategoryBarChart then
  begin
      tabScales.TabVisible := True;
      if (CW.NameSectionDefs <> nil)
      or (CW.ValueSectionDefs <> nil) then
      begin
        tabSections.TabVisible := True;
        if (CW.NameSectionDefs <> nil) then
          tabNameSections.TabVisible := True;
        if (CW.ValueSectionDefs <> nil) then
          tabValueSections.TabVisible := True;
      end;
  end
  else if CW.Chart is TCWPieChart then
  begin
    tabScales.TabVisible := false;
    tabAxisAttributes.TabVisible := false;
    tabSections.TabVisible := false;
  end;

  tabChartGeneral.TabVisible := True;
  pgChartDefs.ActivePageIndex := 0;
end;

procedure Tform_main.sbDivLineClick(Sender: TObject);
begin
  SetPenProps(CW.ValueScale1.Pen);
end;

procedure Tform_main.sbDivline2Click(Sender: TObject);
begin
 SetPenProps(CW.ValueScale2.Pen);
end;

procedure Tform_main.sbNameDivLineClick(Sender: TObject);
begin
  SetPenProps(CW.NameScale.Pen);
end;

procedure Tform_main.eChartTitleExit(Sender: TObject);
begin
   if CW.Chart <> nil then
     CW.Chart.Title := eChartTitle.Text;
end;

procedure Tform_main.eGMargBottomExit(Sender: TObject);
begin
  CW.Chart.GraphMargins.Bottom := StrToInt(eGMargBottom.Text);
end;

procedure Tform_main.eGmargLeftExit(Sender: TObject);
begin
  CW.Chart.GraphMargins.Left := StrToInt(eGMargLeft.Text);
end;

procedure Tform_main.eGMargRightExit(Sender: TObject);
begin
  CW.Chart.GraphMargins.Right := StrToInt(eGMargRight.Text);
end;

procedure Tform_main.eGMargTopExit(Sender: TObject);
begin
 CW.Chart.GraphMargins.Top := StrToInt(eGMargTop.Text);
end;

procedure Tform_main.eHighValueExit(Sender: TObject);
var
 VScale : TCWValueScale;
begin
   VScale := nil;
   if CW.Chart <> nil then
   begin
    if Sender = eHighValue then
      VScale := CW.ValueScale1
    else if Sender = eHighValue2 then
      VScale := CW.ValueScale2;
    try
     VScale.ValueHigh := StrToFloat(TEdit(Sender).Text);
    except
      TEdit(Sender).Text := FloatToStr(VScale.ValueHigh);
      raise;
    end;
   end;
end;

procedure Tform_main.eIMargBottomExit(Sender: TObject);
begin
  CW.Chart.InnerMargins.Bottom := StrToInt(eIMargBottom.Text);
end;

procedure Tform_main.eIMargLeftExit(Sender: TObject);
begin
  CW.Chart.InnerMargins.Left := StrToInt(eIMargLeft.Text);
end;

procedure Tform_main.eIMargRightExit(Sender: TObject);
begin
  CW.Chart.InnerMargins.Right := StrToInt(eIMargRight.Text);
end;

procedure Tform_main.eIMargTopExit(Sender: TObject);
begin
  CW.Chart.InnerMargins.Top := StrToInt(eIMargTop.Text);
end;

procedure Tform_main.eItemNameExit(Sender: TObject);
var
  Indx : integer;
begin
  Indx := lbItems.ItemIndex;
  if (Indx <>-1) and (Trim(eItemName.Text) <> '') then
  begin
   CW.Categories.Items[Indx].CategoryName := eItemName.Text;
   lbItems.Repaint;
  end;
end;

procedure Tform_main.eLegHorizMarginsExit(Sender: TObject);
begin
  if CW.Chart <> nil then
   begin
     CW.Chart.Legends.Items[lbLegends.ItemIndex].Legend.HorizMargins := StrToInt(eLegHorizMargins.Text);
   end;
end;

procedure Tform_main.eLegPointNameExit(Sender: TObject);
begin
 if (CW.Chart <> nil) and (Trim(eLegPointName.Text) <> '') then
   begin
     CW.Chart.Legends.Items[lbLegends.ItemIndex].Legend.PointName := eLegPointName.Text;
   end;
end;

procedure Tform_main.eLegVertMarginsExit(Sender: TObject);
begin
  if CW.Chart <> nil then
   begin
     CW.Chart.Legends.Items[lbLegends.ItemIndex].Legend.VertMargins := StrToInt(eLegVertMargins.Text);
   end;
end;

procedure Tform_main.eLineWidthExit(Sender: TObject);
begin
  FCurveControls.Update(Sender);
end;

procedure Tform_main.eLowValueExit(Sender: TObject);
var
 VScale : TCWValueScale;
begin
   VScale := nil;
   if CW.Chart <> nil then
   begin
    if Sender = eLowValue then
      VScale := CW.ValueScale1
    else if Sender = eLowValue2 then
      VScale := CW.ValueScale2;
    try
     VScale.ValueLow := StrToFloat(TEdit(Sender).Text);
    except
      TEdit(Sender).Text := FloatToStr(VScale.ValueLow);
      raise;
    end;
   end;
end;

procedure Tform_main.eNamePrecisionExit(Sender: TObject);
begin
  if CW.Chart <> nil then
     CW.NameScale.NumSpanPrecision := StrToInt(eNamePrecision.Text);
end;

procedure Tform_main.ePrecision1Exit(Sender: TObject);
begin
   if CW.Chart <> nil then
   begin
     CW.ValueScale1.ValuePrecision := StrToInt(ePrecision1.Text);
   end;
end;

procedure Tform_main.ePrecision2Exit(Sender: TObject);
begin
  if CW.Chart <> nil then
   begin
     CW.ValueScale2.ValuePrecision := StrToInt(ePrecision2.Text);
   end;
end;

procedure Tform_main.eSeriesTitleExit(Sender: TObject);
begin
   if CW.Chart <> nil then
     CW.Chart.SeriesDefs[lbSeries.ItemIndex].Title := eSeriesTitle.Text;
end;

procedure Tform_main.eSMAPeriodsExit(Sender: TObject);
var
  N : integer;
begin
  if eSMAPeriods.Text = '' then
    Exit;
  CW.BeginUpdate;
  with FGraph1 as TCWAxisGraph do
    SMAPeriods := StrToInt(eSMAPeriods.Text);
   if FGraph2 <> nil then
     with FGraph2 as TCWAxisGraph do
     SMAPeriods := StrToInt(eSMAPeriods.Text);
  CW.EndUpdate;
end;

procedure Tform_main.eStatLineWidthChange(Sender: TObject);
begin
  if eStatlineWidth.Text = '' then
    Exit;
  CW.BeginUpdate;
   with FGraph1 as TCWAxisGraph do
    StatlineWidth := eStatLineWidth.Value;
   if FGraph2 <> nil then
     with FGraph2 as TCWAxisGraph do
     StatlineWidth := eStatLineWidth.Value;
  CW.EndUpdate;
end;

procedure Tform_main.eValueIntervalsExit(Sender: TObject);
var
 VScale : TCWValueScale;
begin
   VScale := nil;
   if CW.Chart <> nil then
   begin
    if Sender = eValueIntervals then
      VScale := CW.ValueScale1
    else if Sender = eValueIntervals2 then
      VScale := CW.ValueScale2;
    try
     VScale.ValueIntervals := StrToFloat(TEdit(Sender).Text);
    except
      TEdit(Sender).Text := FloatToStr(VScale.ValueIntervals);
      raise;
    end;
   end;
   TEdit(Sender).Text := FloatToStr(VScale.ValueIntervals);
end;

procedure Tform_main.eWallWidthExit(Sender: TObject);
begin
   if CW.Chart <> nil then
     CW.WallWidth := StrToInt(eWallWidth.Text);
end;

procedure TForm_main.ShowData;
begin
   frmData.Show;
end;

procedure Tform_main.SpeedButton2Click(Sender: TObject);
begin
  LoadImage;
end;

procedure TForm_main.CreateChart;
var
 C : TCWChart;
 i : integer;
 D : TSeriesData;
 Cat : TCatData;
 G1, G2,  GAlt : string;
 Gr1, Gr2, GrAlt : TCWGraph;
 PieC : TCWPieChart;
 CatC : TCWCategoryBarChart;
 CatItm : TCWCategory;
 Nm : string;
 N : integer;
 NSect : TCWNameSectionDefs;
 VSect : TCWValueSectionDefs;
 L : TCWLegend;
 LItm : TCWLegendItem;

 procedure GetUniques(var G1, G2 : string);
 var
   i : integer;
 begin
     G1 := '';
     G2 := '';
     for I := 0 to frm_New.lbSeries.Count-1 do
      begin
        D := frm_New.lbSeries.Items.Objects[i] as TSeriesData;
        if i = 0 then
        begin
         G1 := D.Graph;
         Continue;
        end;
        if (D.Graph <> G1) or (D.ValueScale = 2) then
        begin
           G2 := D.Graph;
           Break;
        end;
      end;
 end;

 function CreateGraph(G : string) : TCWGraph;
 begin
   Result := nil;
   if G = '' then
     Exit;
   if G = 'Curve' then
     Result := TCWCurve.Create(Self)
   else if G = 'Bar' then
     Result := TCWBar.Create(Self)
   else
     Result := TCWPie.Create(Self)
 end;

begin
  NM := GetBaseName;
  with frm_New do
  begin
      if cbChartType.ItemIndex = C_Span then
        C := TCWSpanChart.Create(Self)
      else if cbChartType.ItemIndex = C_General then
       C := TCWGeneralChart.Create(Self)
      else if cbChartType.ItemIndex = C_Pie then
       C := TCWPieChart.Create(Self)
      else
       C := TCWCategoryBarChart.Create(Self);
      C.Title := eTitle.Text;
      C.Name := 'C_' + NM;
      GetUniques(G1, G2);
      GR1 := CreateGraph(G1);
      Gr1.Name := G1 + '_' + NM;
      Gr2 := CreateGraph(G2);
      if Gr2 <> nil then
        Gr2.Name := G2 + '_' + NM;
      if cbAltGraph.ItemIndex > 0 then
      begin
        GAlt := cbAltGraph.Text;
        GrAlt := CreateGraph(GAlt);
        GrAlt.Name := GAlt + '_Alt' + NM;
        if (C is TCWCategoryBarChart) then
          TCWCategoryBarChart(C).AlternativeGraph := GrAlt
        else if (C is TCWGeneralChart) then
          TCWGeneralChart(C).AlternativeGraph := GrAlt
        else if (C is TCWSpanChart) then
          TCWSpanChart(C).AlternativeGraph := GrAlt;
      end;

      if frm_new.cbSpanType.ItemIndex <> -1 then
        TCWSpanChart(C).SpanType := TSpanType(cbSpanType.ItemIndex);

      if cbxIncludeVSections.Checked and cbxIncludeVSections.Enabled then
      begin
        VSect := TCWValueSectionDefs.Create(Self);
        VSect.Name := 'VSection_' + Nm;

        if (C is TCWCategoryBarChart) then
          TCWCategoryBarChart(C).ValueSectionDefs := VSect
        else if (C is TCWGeneralChart) then
          TCWGeneralChart(C).ValueSectionDefs := VSect
        else if (C is TCWSpanChart) then
          TCWSpanChart(C).ValueSectionDefs := VSect;
      end;
      if cbxIncludeNSections.Checked and cbxIncludeNSections.Enabled then
      begin
        NSect := TCWNameSectionDefs.Create(Self);
        NSect.Name := 'NSection_' + Nm;
        if (C is TCWCategoryBarChart) then
          TCWCategoryBarChart(C).NameSectionDefs := NSect
        else if (C is TCWGeneralChart) then
          TCWGeneralChart(C).NameSectionDefs := NSect
        else if (C is TCWSpanChart) then
          TCWSpanChart(C).NameSectionDefs := NSect;
      end;

      if eNumLegends.Enabled then
      begin
         N := StrToInt(eNumLegends.Text);
         for I := 1 to N do
         begin
            LItm := C.Legends.Add;
            L := TCWLegend.Create(Self);
            L.Name := 'Legend' + IntToStr(i) + '_' + Nm;
            LItm.Legend := L;
         end;
      end;

      FGraph1 := nil;
      FGraph2 := nil;
      for I := 0 to lbSeries.Count-1 do
      begin
        D := lbSeries.Items.Objects[i] as TSeriesData;
        if D.ValueScale = 0 then
        begin
          if D.Graph = G1 then
          begin
            C.AddSeriesDef(D.Title,Gr1, D.Color, vsValueScale1);
          end
          else if (D.Graph = G2) and (G2 <> '')  then
          begin
            C.AddSeriesDef(D.Title, Gr2, D.Color, vsValueScale1);
          end;
        end
        else
        begin
          if D.Graph = G1 then
          begin
            C.AddSeriesDef(D.Title,  Gr1, D.Color, vsValueScale2);
          end
          else if (D.Graph = G2) and (G2 <> '')  then
          begin
            C.AddSeriesDef(D.Title, Gr2, D.Color, vsValueScale2);
          end;
        end;
      end;

      if (cbChartType.ItemIndex in [C_Pie,C_Bar]) and (lbItems.Count > 0) then
      begin
        for I := 0 to lbItems.Count-1 do
          begin
            CatC := nil;
            PieC := nil;
            Cat := lbItems.Items.Objects[i] as TCatData;
            if C is TCWCategoryBarChart then
              CatC := C as TCWCategoryBarChart
            else if C is TCWPieChart then
              PieC := C as TCWPieChart;
            if CatC <> nil then
              CatItm := CatC.Categories.Add
            else
              CatItm := PieC.Categories.Add;
            CatItm.CategoryName := Cat.Name;
            CatItm.Color := Cat.Color;
          end;
      end;
  end;
  CW.Chart := C;
  CW.Chart.GetUniqueGraphs(FGraph1, FGraph2);
  CW.ChartList.Add(CW.Chart);
  FillChartList;
  OnLoadChart;
end;

function TForm_Main.GetBaseName : string;
begin
   Result := IntToStr(Random(9));
   Result := Result + IntToStr(Random(9));
   Result := Result + IntToStr(Random(9));

end;


end.
