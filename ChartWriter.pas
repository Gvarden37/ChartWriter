{**************************************************************************** }
{ Version 1.0.0 2022 }
{ Author: Oivind Muller }
{ }
{ This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
  { }
{ **************************************************************************** }

Unit ChartWriter;

interface

{$R *.res}

uses
  System.SysUtils, System.Generics.Collections, System.Classes, Windows,
  Vcl.Controls,
  System.Types, Vcl.Graphics, Math, Vcl.StdCtrls, System.Messaging,
  Vcl.AppEvnts,
  Messages, GraphUtil, System.UITypes, System.Rtti, GDIPUtil, GDIPapi, GDIPObj,
  Forms, Vcl.ExtCtrls, Data.DB;

const
  NearestNam = True;
  NearestVal = False;
  MatchExact = True;
  MatchAny = False;

  GraphOnly = 1;
  GraphAndLabels = 2;
  All = 3;

  WM_SAVESELBM = wm_user + 200;
  WM_ENDEXEC = wm_user + 201;
  WM_SAVEHIST = wm_user + 202;
  WM_ERROR = wm_user + 203;
  WM_LOADFILE = wm_user + 204;

  c_HookSize = 5;
  c_HookMargin = 3;
  c_HookSpace = c_HookSize + c_HookMargin;
  c_LabelXMarg = 6; { Spaces between labels }
  c_QualifierMargin = 6;

  { TGCFile constants }
  EOF = '[EOF]';
  BOF = '[BOF]';
  Props = '[PROPERTIES]';
  Dta = '[DATA]';
  User = '[USER]';
  Objects = '[OBJECTS]';
  Head = '[HEAD]';
  SignCWR = 'ChartWriter CWR';
  SignCWD = 'ChartWriter CWD';

  { 3d shadows, in percentage darkening }
  d3_TopCube = 10;
  d3_SideCube = 20;
  d3_GradientCylinder = 20;
  d3_GradientStyle = 45;
  d3_TopCylinder = 30;
  d3_Outline = 40;

  leg_BorderMarg = 4;
  leg_PointRad = 6;

  { Property name constants, used with the TGCFileReader }
  C_Chart = 'Chart';
  C_Graph = 'Graph';
  C_Title = 'Title';
  C_TitleAlignment = 'TitleAlignment';
  C_TitleFont = 'TitleFont';
  C_Name = 'Name';
  C_Ident = 'Ident';
  C_Color = 'Color';
  C_ValueAxis = 'ValueAxis';
  C_Visible = 'Visible';
  C_Font = 'Font';
  C_Pen = 'Pen';
  C_Brush = 'Brush';
  C_GraphBGColor = 'GraphBGColor';
  C_NameFont = 'NameFont';
  C_ValueFont = 'ValueFont';
  C_Master = 'Master';
  C_GraphType = 'GraphType';
  C_ValueSpanFromData = 'ValueSpanFromData';
  C_ValueIntervals = 'ValueIntervals';
  C_ValueIntervals2 = 'ValueIntervals2';
  C_ValueLow = 'ValueLow';
  C_ValueHigh = 'ValueHigh';
  C_NameType = 'NameType';
  C_ViewType = 'ViewType';
  C_AutoSections = 'AutoSections';
  C_DateTimeTemplate = 'DateTimeTemplate';
  C_CaptionLayout = 'CaptionLayout';
  C_CaptionHorzMargin = 'CaptionHorzMargin';
  C_CaptionVertMargin = 'CaptionVertMargin';
  C_ProlongedLines = 'ProlongedLines';
  C_ShowLines = 'ShowLines';
  C_StartValue = 'StartValue';
  C_EndValue = 'EndValue';
  C_LongCaption = 'LongCaption';
  C_ShortCaption = 'ShortCaption';
  C_NameSectionDefs = 'NameSectionsDefs';
  C_ValueSectionDefs = 'ValueSectionDefs';
  C_TimeFormat = 'TimeFormat';
  C_AxisOrientation = 'AxisOrientation';
  C_CurveStyle = 'Style';
  C_CurveLineStyle = 'LineStyle';
  C_CurveLineWidth = 'LineWidth';
  C_CurveBaseLineValue = 'BaseLineValue';
  C_CurveStep = 'Step';
  C_SmoothLines = 'SmoothLines';
  C_AreaOutline = 'AreaOutline';
  C_AreaOutlineColor = 'AreaOutlineColor';
  C_UseSeriesStyles = 'UseSeriesStyles';
  C_MinPointSpacing = 'MinPointSpacing';
  C_MaxPointSpacing = 'MaxPointSpacing';
  C_CurveBrush = 'CurveBrush';
  C_BarLayout = 'BarLayout';
  C_BarStyle = 'BarStyle';
  C_BarWidth = 'BarWidth';
  C_CubeAngle = 'CubeAngle';
  C_CubeDepth = 'CubeDepth';
  C_ItemSpacing = 'ItemSpacing';
  C_SeriesSpacing = 'SeriesSpacing';
  C_boBaseLine = 'boBaseLine';
  C_BarBaseLineValue = 'BaseLineValue';
  C_boOutLines = 'boOutLines';
  C_boText = 'boText';
  {Bar text content:}
  C_tcName = 'ctName';
  C_tcValue = 'ctValue';
  C_tcTitle = 'ctTitle';
  C_tcPercentage = 'ctPercentage';
  C_ColorUsage = 'ColorUsage';
  C_ShowQualifier = 'ShowQualifier';
  C_PieSize = 'PieSize';
  C_PieSliceSpacing = 'PieSliceSpacing';
  C_CalcPercentages = 'CalcPercentages';
  C_poPrintPercentages = 'poPrintPercentages';
  C_poPrintNames = 'poPrintNames';
  C_poPrintSeriesTitles = 'poPrintSeriesTitles';
  C_poClientBorder = 'poClientBorder';
  C_PieTitleSpace = 'PieTitleSpace';
  C_Scrollable = 'Scrollable';
  C_MouseTimeFormat = 'MouseTimeFormat';
  C_NumSpanPrecision = 'NumSpanPrecision';
  C_slName = 'slName'; { Opposed section labels }
  C_slValue = 'slValue';
  C_PointWidth = 'PointWidth';
  C_ScrollingBarWidth = 'ScrollingBarWidth';
  C_toName = 'toName'; { Text orientation }
  C_toValue = 'toValue';
  C_TextTiltThreshold = 'TextTiltThreshold';
  C_veNameLabels = 'veNameLables';
  C_veValueLabels = 'veValueLables';
  C_veNameDividerLines = 'veNameDividerLinesLines';
  C_veValueDividerLines = 'veValueDividerLines';
  C_veBaseLine = 'veBaseLine';
  C_UseSeriesCurveStyles = 'UseSeriesCurveStyles';
  C_ValuePrecision = 'ValuePrecision';
  C_slMean = 'slMean';
  C_slMedian = 'slMedian';
  C_slRegression = 'slRegression';
  C_slMode = 'slMode';
  C_DividerLinePen = 'DividerLinePen';
  C_OverflowAction = 'OverflowAction';
  C_WallWidth = 'WallWidth';
  C_GradientWall = 'GradientWall';
  C_WallColor = 'WallColor';
  { Legend props }
  C_Alignment = 'Alignment';
  C_Anchoring = 'Anchoring';
  C_Border = 'Border';
  C_Bullets = 'Bullets';
  C_ColoredText = 'ColoredText';
  C_coSeriesTitle = 'coSeriesTitle';
  C_coValue = 'coValue';
  C_coName = 'coName';
  C_coNameSpan = 'coNameSpan';
  C_ContentFlow = 'ContentFlow';
  C_Enabled = 'Enabled';
  C_HorizMargins = 'HorizMargins';
  C_PointItemIndex = 'PointItemIndex';
  C_PointLocating = 'PointLocating';
  C_poShowConnectionLine = 'poShowConnectionLine';
  C_poThinConnectionLine = 'poThinConnectionLine';
  C_poEnlargePoint = 'poEnlargePoint';
  C_PointSeriesIndex = 'PointSeriesIndex';
  C_PointValue = 'PointValue';
  C_Transparency = 'Transparency';
  C_VertMargins = 'VertMargins';
  C_Text = 'Text';
  C_Qualifier = 'Qualifier';

  { Render error codes }
  R_NoActiveGraph = 1;
  R_NoData = 2;
  R_Updating = 3;
  R_NoChart = 4;
  R_Unknown = 0;

type
  TChartWriter = class;

  TWriterListElement = class
    Writer : TChartWriter;
    Obj : TObject;
  end;
  TWriterList = TObjectList<TWriterListElement>;

  TCWException = class(Exception)
    Errorcode: integer;
  end;

  { Series coordinates }
  TPointArray = TList<TPoint>;

  TTrackBar = record
    BarRect: TRect;
    NormalRect: TRect;
    VisibleRect: TRect;
    SeriesIndex, SeriesItmIndex: integer;
  end;

  TTrackBars = TList<TTrackBar>;

  TPieSlice = record
  private
    FSeriesIndex, FSeriesItmIndex: integer;
  public
    Center: TPoint;
    StartAngel, EndAngel: single;
    Color: TColor;
    Percentage: single;
  end;

  TTrackPies = TList<TPieSlice>;

  TSeriesDef = record
    { Collects info in PointsFromRuler/ProcessRulers and other mouse oriented operations }
    SeriesIndex, SeriesItemIndex: integer;
  end;

  TSeriesDefs = array of TSeriesDef;

  { Mode function return values }
  TModeNumber = record
    Number: string;
    Cnt: integer;
  end;

  TModeNumbers = array of TModeNumber;

  TSeries = class;

  TGraphObject = class
    { Bas class for graph objects. Internal use }
  private
    FWriter: TChartWriter;
    property Writer: TChartWriter read FWriter;
  end;

  { Datasets }
  TData = TObjectList<TSeries>;
  TSeriesItem = class;

  TSeriesItems = TObjectList<TSeriesItem>;

  TSeriesItem = class(TGraphObject)
    { Series items }
  private
    FName: string;
    FOwner: TSeries;
    FItems: TSeriesItems;
    FValue: single;
    FColor : TColor;
    FRealVal: single;
    FSpace: Boolean;
    FLeapDummy: Boolean;
    FRealDate: TDateTime;
    FReminderName: string;
    FVisible : Boolean; {Internal use by CreateXValues}
    FOrigName: string; { Used by save to file }
    function GetPointPos: TPoint;
    function GetName: string;
    function GetDisabled: Boolean;
    function GetIndex: integer;
    function GetItemIndex: integer;
    function GetBarRect: TRect;
    function GetPieSlice: TPieSlice;
    function GetSeriesIndex: integer;
    function GetPst : single;
    function GetVal : single;
    procedure SetColor(Value : TColor);
    property Index: integer read GetIndex; { In span }
  public
    procedure Assign(Source: TSeriesItem);
    property Disabled: Boolean read GetDisabled;
    property Name: string read GetName write FName;
    property Value: single read GetVal write FValue;
    property Color : TColor read FColor write SetColor;
    property PointPos: TPoint read GetPointPos;
    property ItemIndex: integer read GetItemIndex; { 0-based }
    property RealDate: TDateTime read FRealDate;
    property SeriesIndex: integer read GetSeriesIndex;
    property BarRect: TRect read GetBarRect;
    property PieSlice: TPieSlice read GetPieSlice;
    property Pst : single read GetPst;
  End;

  TSectionType = (stSection, stLine);
  TRulers = (ruNames, ruValues, ruBoth, ruNone);
  TAxisType = (atNameAxis, atValueAxis);
  TAxisElement = (veNameLabels, veValueLabels, veNameDividerLines,
    veValueDividerLines, veRulerGuideLines, veBaseLine);
  TAxisElements = set of TAxisElement;
  TGraphBorders = (gbAxis, gbAllSides, gbNone);
  TAutoSections = (autNotUsed, autDays, autDates, autWeeks, autMonths,
    autYears);
  TNameSectionType = (stUndefined, stLiterals, stAutoSections,
    stDateTimeTemplate);
  TCurveStyle = (csLine, csClientArea, csBaseLineArea, csNeighborArea,
    csPoints);
  TPointMarkers = (pmSmallBall, pmBigBall, pmDot, pmText, pmOwnerDraw, pmNone);
  TViewMode = (vmNormal, vmSelecting, vmSelected, vmHinting);
  TBarOption = (boBaseLine, boOutLines, boTruncReminder, boText);
  TBarOptions = set of TBarOption;
  TPieOption = (poPrintPercentages, poPrintNames, poClientBorder, poPrintSeriesTitles);
  TPieOptions = set of TPieOption;
  TBarLayout = (blStacked, blSideBySide);
  TAxisOrientation = (alBottomLeft, alBottomRight, alLeftTop, alTopLeft, alTopRight,
    alRightTop, alLeftBottom, alRightBottom);
  TMouseInfo = (miName, miValue, miBoth, miNone);
  TMousePrecision = (mpHigh, mpMedium, mpLow);
  TLabelKind = (lkName, lkValue, lkValue2, lkNameSection,
    lkValueSection, lkInfo);
  TSectionLabel = (slName, slValue);
  TSectionLabels = set of TSectionLabel;
  TCaptionType = (ctDefined, ctName, ctValue, ctNameAndValue);
  TNameType = (ntDateSpan, ntHourSpan, ntMinuteSpan, ntSecondSpan, ntNumberSpan,
    ntGeneral, ntPieAsync);
  TDateTimeTemplate = (ttNotUsed, ttMonthTemplate, ttWeekTemplate, ttDateTemplate, ttHourTemplate, ttMinuteTemplate,
    ttSecondTemplate, ttLiterals);
  TCaptionLayout = (clSameSideAsLabels, clOppositeSideOfLabels);
  TTextOrientation = (toName, toValue, toSection);
  TTextOrientations = set of TTextOrientation;
  TSectionElement = (seText, seLine);
  TContractionType = (ctExponential, ctIncremental, ctExplicit);
  TState = (stUpdating, stZoomed, stInternalContraction, stUserContraction,
    stInternalAction, stRestoring, stExecuting, stLabelFreqs, stPainting, stActivating,
    stAnimating, stInitAnimation, stAnimationPause, stResumeAnimation,
    stMouseSizing, stLimbo);
  TStates = set of TState;
  TAxisPosition = (apLeft, apTop, apRight, apBottom);
  TLegendContent = (coSeriesTitle, coValue, coName, coNameSpan);
  TLegendContents = set of TLegendContent;
  TLegendBullets = (lbNone, lbSquare, lbCircle, lbLine);
  TLegendAnchoring = (anLeftOutside, anLeftInside, anTopOutside, anTopInside,
    anRightOutside, anRightInside, anBottomOutside, anBottomInside,
    anSeriesOutside, anPointInside);
  TLegendAlignment = (laLeftOrTop, laCenter, laRightOrBottom);
  TContentFlow = (cfTopBottom, cfLeftRight);
  TPointLocating = (plNameValue, plIndexes, plMaxValue, plMinValue, plEvent);
  TPointOption = (poShowConnectionLine, poThinConnectionLine, poEnlargePoint);
  TPointOptions = set of TPointOption;
  TSelectionExec = (seZoom, seEvent, seOther);
  TCurvelineStyle = (lsSolid, lsDot, lsDash);
  TUpdateKind = (ukLabelFreq, ukRestrict, ukScroll, ukPaint);
  TUpdateKinds = Set of TUpdateKind;
  TScrollType = (stNext, stPrev, stNextPage, stPrevPage, stFirst, stLast);
  TRulerPoints = array of integer;
  TStatLine = (slNone, slMean, slMedian, slRegression, slMode);
  TColorUsage = (cuOnSeries, cuOnItems);
  TLineType = (ltCurve, ltMean, ltMedian, ltMode, ltRegression);
  TValueAxisNumber = (vaValueAxis1, vaValueAxis2, vaNone);
  TBarStyle = (bsFlat, bsCube, bsCylinder, bsGradientWidth, bsGradientLength);
  TAnimation = (anFlow, anGrow, anPause);
  TAnimations = set of TAnimation;
  TAnimationSpeed = (asFast, asMediumFast, asMediumSlow, asSlow);
  TOverflowAction = (ovContraction, ovCompression, ovScrolling, ovNone);
  TSaveOptions = (soDataOnly,soDataAndSeriesProps,soAll);
  TTextContent = (tcValue, tcName, tcTitle, tcPercentage);
  TTextContents = set of TTextContent;
  TSpaceResponse =(srAccept, srRefuseAbort, srRefuseExcept);

  TCWBound = record
    StartName, EndName: string;
  end;

  TCWBounds = TList<TCWBound>;

  TAxisObject = class;
  TCWColor = class;
  TCWColors = class;

  TIntPointArray = array of TPoint;

  TInfo = record
    Text: TStringList;
    Items: TList<TSeriesItem>;
    Colors: TList<TColor>;
  end;

  TAnimInfo = record
    { Used with animations }
    NextHorz: integer;
    NextVert: integer;
    NextSeries: integer;
    LastXY: integer;
    LastPt: TPoint;
    Stopped: Boolean;
    Paused: Boolean;
    StartPause: Boolean;
  end;

  TAnimationTuner = record
    Delay: integer;
    { Delay frequences }
    FastFreq: integer;
    MediumFastFreq: integer;
    MediumSlowFreq: integer;
    SlowFreq: integer;
  end;

  TMoveDir = (mdUp, mdLeft, mdRight, mdDown);
  TOverlaps = class
  private
     FRects : TList<TRect>;
     FOuter : TRect;
     FMoveDir : TMoveDir;
  public
    constructor Create(OutBounds : TRect; MoveDir : TMoveDir);
    destructor Destroy; override;
    function GetRect(ARect : TRect) : TRect;
    function Overlapped(ARect : TRect; var OverlappedRect : TRect) : Boolean;
    function MoveIt(ARect : TRect; var MovedRect : TRect) : Boolean;
    procedure Add(ARect : TRect);
  end;



  { TSection ----------------------------------------------------------------- }
  TSection = class(TGraphObject)
  private
    FOwner: TAxisObject;
    FLongCaption: string;
    FShortCaption: string;
    FSectionType: TSectionType;
    FStartVal: string;
    FEndVal: string;
    FIndex: integer;
    FIsAuto: Boolean;
    FIsReduced: Boolean;
    function GetSectionLabelRect: TRect;
    function GetSectionGraphRect: TRect; { Rect within graph }
  public
    constructor Create;
    property StartVal: string read FStartVal;
    property EndVal: string read FEndVal;
    property LongCaption: string read FLongCaption write FLongCaption;
    property ShortCaption: string read FShortCaption write FShortCaption;
    property SectionType: TSectionType read FSectionType;
    property SectionLabelRect: TRect read GetSectionLabelRect;
    property SectionGraphRect: TRect read GetSectionGraphRect;
  end;

  TSections = TObjectList<TSection>;

  TContractionEvent = procedure(Sender: TObject; MaxPoints: integer) of object;
  TDrawPointEvent = procedure(Sender: TObject; SeriesIndex, ItemIndex: integer;
    APosition: TPoint; ACanvas: TCanvas) of object;
  TDrawCurveEvent = procedure(Sender: TObject; SeriesIndex: integer;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawCurveLineEvent = procedure(Sender: TObject; LineType: TLineType;
    FromPoint, ToPoint: TPoint; SeriesIndex, ItemIndex: integer;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawBarEvent = procedure(Sender: TObject; SeriesIndex, ItemIndex: integer;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawPieSliceEvent = procedure(Sender: TObject;
    SeriesIndex, ItemIndex: integer; ACanvas: TCanvas; var Handled: Boolean)
    of object;
  TDrawGraphEvent = procedure(Sender: TObject; Canvas: TCanvas) of object;
  TDrawSectionEvent = procedure(Sender: TObject; Axis: TAxisType;
    APosition: TPoint; ASection: TSection; ASectionElement: TSectionElement;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawLabelEvent = procedure(Sender: TObject; Axis: TAxisType; ALabel: string;
    ATime: TDateTime; APosition: TPoint; ACanvas: TCanvas; var Handled: Boolean)
    of object;
  TDrawLabelsEvent = procedure(Sender: TObject; LabelKind: TLabelKind;
    ACanvas: TCanvas) of object;
  TMouseInfoEvent = procedure(Sender: TObject; Info: TInfo; Position: TPoint;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TMeasureLabelEvent = procedure(Sender: TObject; ALabelKind: TLabelKind;
    var Widest, Tallest: string) of object;
  TMouseItemEvent = procedure(Sender: TObject; SeriesIndex, ItemIndex: integer)
    of object;
  TLocatePointEvent = procedure(Sender: TObject; var SeriesIndex: integer;
    var ItemIndex: integer) of object;
  TQuerySpaceEvent= procedure(Sender: TObject; const QueryResult: integer;
   var Response : TSpaceResponse) of object;
  TOnTitleEvent = procedure(Sender: TObject; const ASeriesIndex: integer;
   var Title : string) of object;

  TCWLegend = class;
  TCWLegends = class;

  TRectInfo = record
    R: TRect;
    P: TPoint;
    Index: integer;
    Leg: TCWLegend;
  end;

  TCWChart = class;

  TLegendRects = TList<TRectInfo>;
  TLegContentList = TObjectList<TStringList>;

  TCWGraph = class(TComponent)
  private
    FWriter: TChartWriter;
    FWID : TWriterListElement;
    FGDIP: TGPGraphics;
    function GetCanvas: TCanvas;
    function GetActiveColor(SeriesIndex, ItemIndex : integer): TColor; virtual;
    function GetSeries(Value : integer) : TSeries;
    function GetWriter : TChartWriter;
    function GetChart : TCWChart;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Canvas: TCanvas read GetCanvas;
    property GDIP: TGPGraphics read FGDIP;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignGraph(Source: TCWGraph); virtual;
    destructor Destroy; override;
    procedure Draw; virtual;
    function InChart(SeriesIndex : integer = -1) : Boolean;
    property ActiveColor[SeriesIndex, ItemIndex : integer] : TColor read GetActiveColor;
    property Series[Index : integer] : TSeries read GetSeries;
    property Chart : TCWChart read GetChart;
    property Writer: TChartWriter read GetWriter;
  published

  end;

  TChartList = TList<TCWChart>;
  TCWNameSectionDefs = class;
  TCWValueSectionDefs = class;
  TCWValueAxis = class;

  TCWAxisGraph = class(TCWGraph)
  private
    FStatLine: TStatLine;
    FMaxPointSpacing: integer;
    FAnimations: TAnimations;
    FAnimationSpeed: TAnimationSpeed;
    FHorzCounter: integer;
    FVertCounter: integer;
    FAnimationBooster: integer;
    procedure SetStatLine(Value: TStatLine);
    function GetMaxPointSpacing: integer; virtual; abstract;
    function GetMinPointSpacing: integer; virtual; abstract;
    function GetValueAxis : TCWValueAxis;
    procedure SetAnimations(Value: TAnimations);
    procedure SetAnimationBooster(Value: integer);
    procedure SetMaxPointSpacing(Value: integer);
    procedure SetMinPointSpacing(Value: integer); virtual; abstract;
  protected
    property MaxPointSpacing: integer read GetMaxPointSpacing
      write SetMaxPointSpacing default 0;
    property MinPointSpacing: integer read GetMinPointSpacing
      write SetMinPointSpacing;
    property Animations: TAnimations read FAnimations write SetAnimations
      default [];
    property AnimationSpeed: TAnimationSpeed read FAnimationSpeed
      write FAnimationSpeed default asMediumFast;
    property AnimationBooster: integer read FAnimationBooster
      write SetAnimationBooster default 0;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignGraph(Source: TCWGraph); override;
    function SeriesCount : integer;
    function QuerySpace : integer; virtual;
    property ValueAxis : TCWValueAxis read GetValueAxis;
  published
    property StatLine: TStatLine read FStatLine write SetStatLine
      default slNone;
  end;

  TCWSeriesStyle = class;

  TCWSeriesStyles = class(TOwnedCollection)
  private
    function GetItem(AIndex: integer): TCWSeriesStyle;
    procedure SetItem(AIndex: integer; const Value: TCWSeriesStyle);
  protected
  public
    function Add: TCWSeriesStyle;
    function GetStyle(ASeries: TSeries): TCWSeriesStyle;
    function IndexOf(const ASeriesTitle: string): integer;
    procedure Assign(Source: TPersistent); override;
    property Items[AIndex: integer]: TCWSeriesStyle read GetItem write SetItem;
  end;

  TCWSeriesStyle = class(TCollectionItem)
  private
    FLineStyle: TCurvelineStyle;
    FStyle: TCurveStyle;
    FLineWidth: integer;
    FSeriesTitle: string;
    procedure SetStyle(Value: TCurveStyle);
    procedure SetLineStyle(Value: TCurvelineStyle);
    procedure SetLineWidth(Value: integer);
    procedure SetSeriesTitle(Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TCurveStyle read FStyle write SetStyle default csLine;
    property LineStyle: TCurvelineStyle read FLineStyle write SetLineStyle
      default lsSolid;
    property LineWidth: integer read FLineWidth write SetLineWidth default 1;
    property SeriesTitle: string read FSeriesTitle write SetSeriesTitle;
  end;

  TCWColors = class(TOwnedCollection)
  private
    function GetItem(AIndex: integer): TCWColor;
    procedure SetItem(AIndex: integer; const Value: TCWColor);
  protected
    procedure Update(Item : TCollectionItem); override;
  public
    function Add: TCWColor;
    procedure Assign(Source: TPersistent); override;
    function IndexOf(const ItemName: string): integer;
    function GetColor(const ItemName: string): TColor;
    procedure SetColor(const ItemName: string; AColor: TColor);
    property Items[AIndex: integer]: TCWColor read GetItem write SetItem;
  end;

  TCWColor = class(TCollectionItem)
  private
    FColor: TColor;
    FItemName: string;
    procedure SetColor(Value: TColor);
    procedure SetItemName(Value: string);
    function GetColor: TColor;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read GetColor write SetColor default clBlack;
    property ItemName: string read FItemName write SetItemName;
  end;

  TCWCurve = class(TCWAxisGraph)
  private
    FBeaconIndex: integer;
    FStyle: TCurveStyle;
    FLineStyle: TCurvelineStyle;
    FLineWidth: integer;
    FStep : Boolean;
    FBaseLineValue: single;
    FPointMarkers: TPointMarkers;
    FPointWidth: integer;
    FMinPointSpacing: integer;
    { Minimun unit space before Contraction takes place }
    FAreaBrush: TBrush; {Neighbor polygon only}
    FAreaOutline : Boolean;
    FAreaOutlineColor : TColor;
    FSmoothLines : Boolean;
    FBeaconPoints: Boolean;
    FSeriesStyles: TCWSeriesStyles;
    FNeighborAreas: array of integer;
    { Keeps track of neighbor poly indexes. See DrawNeighbors proc }
    FAnimation: Boolean;
    FGPen : TGPPen;

    FOnDrawPoint: TDrawPointEvent;
    FOnDrawCurve: TDrawCurveEvent;
    FOnDrawCurveLine: TDrawCurveLineEvent;
    FOnDrawStatLine: TDrawCurveLineEvent;
    FOnMouseEnterPoint: TMouseItemEvent;
    FOnMouseLeavePoint: TMouseItemEvent;

    procedure BrushChanged(Sender: TObject);
    procedure SetStyle(Value: TCurveStyle);
    procedure SetLineStyle(Value: TCurvelineStyle);
    procedure SetLineWidth(Value: integer);
    procedure SetStep(Value : Boolean);
    function GetStyle: TCurveStyle;
    function GetLineStyle: TCurvelineStyle;
    function GetLineWidth: integer;
    function GetActiveStyle(ASeries: TSeries): TCurveStyle;
    function GetActiveLineStyle(ASeries : TSeries): TCurvelineStyle;
    function GetActiveLineWidth(ASeries: TSeries): integer;
    procedure SetActiveStyle(ASeries: TSeries; Value: TCurveStyle);
    procedure SetActiveLineStyle(ASeries : TSeries; Value: TCurvelineStyle);
    procedure SetActiveLineWidth(ASeries: TSeries; Value: integer);
    procedure SetAreaOutlineColor(Value : TColor);
    procedure SetAreaOutline(Value : Boolean);
    procedure SetSmoothLines(Value : Boolean);
    procedure SetBaseLineValue(Value: single);
    procedure SetPointMarkers(Value: TPointMarkers);
    procedure SetPointWidth(Value: integer);
    procedure SetMinPointSpacing(Value: integer); override;
    function GetMinPointSpacing: integer; override;
    procedure SetBeaconPoints(Value: Boolean);
    function GetMaxPointSpacing: integer; override;
    function GetUseSeriesStyles: Boolean;
    function GetStyleIndex(ASeries: TSeries): integer;
    function BeaconsActive: Boolean;
    function GetActiveColor(SeriesIndex, ItemIndex : integer): TColor; override;

    procedure DoDrawNeighbors;
    procedure DrawNeighbors(Serie1, Serie2, NeighborIndex: integer);
    procedure DrawTheLine(ASource : TSeries; Indx: integer; x1, y1, x2, y2: integer; StatLine : Boolean = false);
  public
    procedure Draw; override;
    property UseSeriesStyles: Boolean read GetUseSeriesStyles;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignGraph(Source: TCWGraph); override;
    function QuerySpace : integer; override;
    property ActiveStyle[ASeries: TSeries]: TCurveStyle read GetActiveStyle
      write SetActiveStyle;
    property ActiveLineStyle[ASeries: TSeries]: TCurvelineStyle
      read GetActiveLineStyle write SetActiveLineStyle;
    property ActiveLineWidth[ASeries: TSeries]: integer read GetActiveLineWidth
      write SetActiveLineWidth;
  published
    property MaxPointSpacing;
    property MinPointSpacing default 2;
    property AnimationSpeed;
    property Animation: Boolean read FAnimation write FAnimation default False;
    property AreaOutline : Boolean read FAreaOutline write SetAreaOutline default false;
    property AreaOutlineColor : TColor read FAreaOutlineColor write SetAreaOutlineColor default clBlack;
    property SmoothLines : Boolean read FSmoothLines write SetSmoothLines default true;
    property Style: TCurveStyle read GetStyle write SetStyle default csLine;
    property LineStyle: TCurvelineStyle read GetLineStyle write SetLineStyle
      default lsSolid;
    property LineWidth: integer read GetLineWidth write SetLineWidth default 1;
    property BaseLineValue: single read FBaseLineValue write SetBaseLineValue;
    property PointMarkers: TPointMarkers read FPointMarkers
      write SetPointMarkers default pmNone;
    property PointWidth: integer read FPointWidth write SetPointWidth default 5;
    property AreaBrush: TBrush read FAreaBrush write FAreaBrush;
    property BeaconPoints: Boolean read FBeaconPoints write SetBeaconPoints
      default False;
    property SeriesStyles: TCWSeriesStyles read FSeriesStyles
      write FSeriesStyles;
    property Step : Boolean read FStep write SetStep default false;

    property OnDrawCurve: TDrawCurveEvent read FOnDrawCurve write FOnDrawCurve;
    property OnDrawCurveLine: TDrawCurveLineEvent read FOnDrawCurveLine
      write FOnDrawCurveLine;
    property OnDrawPoint: TDrawPointEvent read FOnDrawPoint write FOnDrawPoint;
    property OnDrawStatLine: TDrawCurveLineEvent read FOnDrawStatLine
      write FOnDrawStatLine;
    property OnMouseEnterPoint: TMouseItemEvent read FOnMouseEnterPoint
      write FOnMouseEnterPoint;
    property OnMouseLeavePoint: TMouseItemEvent read FOnMouseLeavePoint
      write FOnMouseLeavePoint;

  end;

  TCWBar = class(TCWAxisGraph)
  private
    FCubeAngle: integer;
    FCubeDepth: integer;
    FBaseLineValue: single;
    FLayout: TBarLayout;
    FItemSpacing: integer;
    FSeriesSpacing: integer;
    FBarWidth: integer;
    FOptions: TBarOptions;
    FScrollingBarWidth: integer;
    FColorUsage: TColorUsage;
    FOnDrawBar: TDrawBarEvent;
    FOnMouseEnterBar: TMouseItemEvent;
    FOnMouseLeaveBar: TMouseItemEvent;
    FBarStyle: TBarStyle;
    FTextContents : TTextContents;
    FOverlaps : TOverlaps;
    FShowQualifier : Boolean;
    procedure SetOptions(Value: TBarOptions);
    procedure SetBaseLineValue(Value: single);
    procedure SetLayout(Value: TBarLayout);
    procedure SetItemSpacing(Value: integer);
    procedure SetSeriesSpacing(Value: integer);
    procedure SetBarWidth(Value: integer);
    procedure SetScrollingBarWidth(Value: integer);
    function GetScrollingBarWidth : integer;
    function GetMaxPointSpacing: integer; override;
    function GetMinPointSpacing: integer; override;
    procedure SetMinPointSpacing(Value: integer); override;
    procedure SetColorUsage(Value: TColorUsage);
    procedure SetCubeAngle(Value: integer);
    procedure SetCubeDepth(Value: integer);
    procedure SetBarStyle(Value: TBarStyle);
    procedure SetShowQualifier(Value : Boolean);
    procedure SetTextContents(Value : TTextContents);
    function GetBarStyle: TBarStyle;
    function GetBarWidth: integer;
    function GetTextQualifier : string;
    function GetActiveColor(SeriesIndex, ItemIndex :Integer): TColor; override;
    function GetSeriesSpacing: integer;
    procedure Get3DDims(ARect: TRect; var TopLeftPt, TopRightPt, BottomLeftPt,
      BottomRightPt: TPoint);
    procedure Get3DWH(ARect: TRect; var AWidth, AHeight: integer);
    function Get3DBarWidth: integer;
    procedure GetCathesus(ARect : TRect; var HorzCath, VertCath: integer);
    procedure DoDraw;
    function Compressing: Boolean;
    function GetCylinderHatHeight : integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Is3D: Boolean;
    function QuerySpace : integer; override;
    procedure AssignGraph(Source: TCWGraph); override;
    procedure Draw; override;
    function GetBarSpace(var BlockWidth: integer; var Compressed: Boolean;
     var BarWidth : integer): Boolean;
    property MaxPointSpacing;
    property MinPointSpacing;
    property TextQualifier: string read GetTextQualifier;
  published
    property Animations;
    property AnimationBooster;
    property AnimationSpeed;
    property CubeAngle: integer read FCubeAngle write SetCubeAngle default 45;
    property CubeDepth: integer read FCubeDepth write SetCubeDepth default 0;
    property Options: TBarOptions read FOptions write SetOptions default [];
    property BaseLineValue: single read FBaseLineValue write SetBaseLineValue;
    property Layout: TBarLayout read FLayout write SetLayout
      default blSideBySide;
    property ItemSpacing: integer read FItemSpacing write SetItemSpacing
      default 3;
    property SeriesSpacing: integer read GetSeriesSpacing write SetSeriesSpacing
      default 0;
    property ShowQualifier : Boolean read FShowQualifier write SetShowQualifier;
    property BarWidth: integer read GetBarWidth write SetBarWidth default 20;
    property BarStyle: TBarStyle read GetBarStyle write SetBarStyle
      default bsFlat;
    property ScrollingBarWidth: integer read GetScrollingBarWidth
      write SetScrollingBarWidth default 20;
    property ColorUsage: TColorUsage read FColorUsage write SetColorUsage
      default cuOnSeries;
    property TextContents : TTextContents read FTextContents write SetTextContents default [tcValue];
    property OnDrawBar: TDrawBarEvent read FOnDrawBar write FOnDrawBar;
    property OnMouseEnterBar: TMouseItemEvent read FOnMouseEnterBar
      write FOnMouseEnterBar;
    property OnMouseLeaveBar: TMouseItemEvent read FOnMouseLeaveBar
      write FOnMouseLeaveBar;
  end;

  TCWPie = class(TCWGraph)
  private
    FOptions: TPieOptions;
    FSliceSpacing: single;
    FTitleSpace: integer;
    FPieSize: integer;
    FDoughnutSize: integer;
    FNumPaint: integer;
    FOnDrawPieSlice: TDrawPieSliceEvent;
    FOnMouseEnterPieSlice: TMouseItemEvent;
    FOnMouseLeavePieSlice: TMouseItemEvent;
    procedure SetOptions(Value: TPieOptions);
    procedure SetSliceSpacing(Value: single);
    procedure SetPieSize(Value: integer);
    procedure SetDoughnutSize(Value: integer);
    function GetActualSize: integer;
    function GetTitleSpace : integer;
    function GetActiveColor(SeriesIndex, ItemIndex : integer): TColor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw; override;
    procedure AssignGraph(Source: TCWGraph); override;
    property ActualSize: integer read GetActualSize;

  published
    property Options: TPieOptions read FOptions write SetOptions default [];
    property SliceSpacing: single read FSliceSpacing write SetSliceSpacing;
    property PieSize: integer read FPieSize write SetPieSize default 500;
    property DoughnutSize: integer read FDoughnutSize write SetDoughnutSize default 0;

    property OnDrawPieSlice: TDrawPieSliceEvent read FOnDrawPieSlice
      write FOnDrawPieSlice;
    property OnMouseEnterPieSlice: TMouseItemEvent read FOnMouseEnterPieSlice
      write FOnMouseEnterPieSlice;
    property OnMouseLeavePieSlice: TMouseItemEvent read FOnMouseLeavePieSlice
      write FOnMouseLeavePieSlice;

  end;

  { TCWSectionItem ------------------------------------------------------------ }

  TCWSectionDefs = class;

  TCWSectionItem = class(TCollectionItem)
  private
    FStartValue: string;
    FEndValue: string;
    FLongCaption: string;
    FShortCaption: string;
    procedure SetStartValue(Value: string);
    procedure SetEndValue(Value: string);
    procedure SetLongCaption(Value : string);
    procedure SetShortCaption(Value : string);
    function GetWriter: TChartWriter;
    function GetSection: TCWSectionDefs;
    procedure CheckDataType(Value: string);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    property Writer: TChartWriter read GetWriter;
    property Section: TCWSectionDefs read GetSection;
  published
    property LongCaption: string read FLongCaption write SetLongCaption;
    property ShortCaption: string read FShortCaption write SetShortCaption;
    property StartValue: string read FStartValue write SetStartValue;
    property EndValue: string read FEndValue write SetEndValue;
  end;

  { TCWSectionItems ----------------------------------------------------------- }

  TCWSectionItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: integer): TCWSectionItem;
    procedure SetItem(AIndex: integer; const Value: TCWSectionItem);
    procedure CheckEmpties;
  protected
  public
    function Add: TCWSectionItem;
    procedure Assign(Source: TPersistent); override;
    property Items[AIndex: integer]: TCWSectionItem read GetItem write SetItem;
  end;

  { TCWSectionDefs -------------------------------------------------------------- }

  TCWSectionDefs = class(TComponent)
  private
    FSections: TCWSectionItems;
    FCaptionLayout: TCaptionLayout;
    FCaptionHorizMargin: integer;
    FCaptionVertMargin: integer;
    FProlongedLines: Boolean;
    FShowLines: Boolean;
    FFont: TFont;
    FPen: TPen;
    FVisible: Boolean;
    FWID : TWriterListElement;
    procedure SetCaptionHorzMargin(Value: integer);
    procedure SetCaptionVertMargin(Value: integer);
    procedure SetVisible(Value: Boolean);
    procedure SetCaptionLayout(Value : TCaptionLayout);
    procedure SetProlongedLines(Value : Boolean);
    procedure SetShowLines(Value : Boolean);
    function GetSectionType: TNameSectionType;
    function GetWriter : TChartWriter;
    procedure DoCheck;
    procedure GraphChanged(Sender : TObject);
    function GetOwnerChart : TCWChart;
  protected
    property SectionType: TNameSectionType read GetSectionType;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Chart : TCWChart read GetOwnerChart;
    property Writer: TChartWriter read GetWriter;
  published
    property Sections: TCWSectionItems read FSections write FSections;
    property CaptionLayout: TCaptionLayout read FCaptionLayout
      write SetCaptionLayout default clOppositeSideOfLabels;
    property CaptionHorizMargin: integer read FCaptionHorizMargin
      write SetCaptionHorzMargin default 0;
    property CaptionVertMargin: integer read FCaptionVertMargin
      write SetCaptionVertMargin default 0;
    property Font: TFont read FFont write FFont;
    property Pen: TPen read FPen write FPen;
    property ProlongedLines: Boolean read FProlongedLines write SetProlongedLines
      default False;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TCWNameSectionDefs = class(TCWSectionDefs)
  private
    FAutoSections: TAutoSections;
    FDateTimeTemplate: TDateTimeTemplate;
    FCaptionType: TCaptionType;
    procedure SetAutoSections(Value: TAutoSections);
    procedure SetDateTimeTemplate(Value: TDateTimeTemplate);
    procedure CheckConfig;
  public
    property SectionType;
  published
    property DateTimeTemplate: TDateTimeTemplate read FDateTimeTemplate
      write SetDateTimeTemplate default ttNotUsed;
    property AutoSections: TAutoSections read FAutoSections
      write SetAutoSections default autNotUsed;
    property CaptionType: TCaptionType read FCaptionType write FCaptionType
      default ctDefined;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCWValueSectionDefs = class(TCWSectionDefs)
  published
  end;

  { TCWView -------------------------------------------------------------------- }

  TCWSeriesDef = class;

  TCWSeriesDefs = class(TOwnedCollection)
  private
    function GetItem(AIndex: integer): TCWSeriesDef;
    procedure SetItem(AIndex: integer; const Value: TCWSeriesDef);
  protected
      procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
      procedure Update(Item : TCollectionItem); override;
  public
    function Add: TCWSeriesDef;
    function Insert(Index: integer): TCWSeriesDef;
    function IndexOf(const AGraph: TCWGraph): integer;
    function IndexOfTitle(ATitle : string) : integer;
    procedure Assign(Source: TPersistent); override;
    property Items[AIndex: integer]: TCWSeriesDef read GetItem write SetItem; default;
  end;

  TValueAxis = class;

  TCWSeriesDef = class(TCollectionItem)
  private
    FGraph: TCWGraph;
    FTitle : string;
    FColor : TColor;
    FValueAxis: TValueAxisNumber;
    FVisible : Boolean;
    procedure SetGraph(Value: TCWGraph);
    function GetWriter: TChartWriter;
    function GetChart: TCWChart;
    procedure SetVisible(Value : Boolean);
    procedure SetTitle(Value : string);
    procedure SetColor(Value : TColor);
    procedure SetValueAxis(Value: TValueAxisNumber);
    function ActualAxis : TValueAxis;
    function GetTitle: string;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Writer: TChartWriter read GetWriter;
    property Chart : TCWChart read GetChart;
  published
    property Graph: TCWGraph read FGraph write SetGraph;
    property Title: string read GetTitle write SetTitle;
    property Color: TColor read FColor write SetColor default clBlack;
    property ValueAxis : TValueAxisNumber read FValueAxis write SetValueAxis default vaValueAxis1;
    property Visible : Boolean read FVisible write SetVisible default true;
  end;

  TCWLegendItem = class(TCollectionItem)
  private
    FLegend: TCWLegend;
    procedure SetLegend(Value: TCWLegend);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    // procedure Assign(Source: TPersistent); override;
  published
    property Legend: TCWLegend read FLegend write SetLegend;
  end;

  TCWLegends = class(TOwnedCollection)
  private
    function GetItem(AIndex: integer): TCWLegendItem;
    procedure SetItem(AIndex: integer; const Value: TCWLegendItem);
    function WidestLegend(APosition: TAxisPosition): integer;
  protected
    procedure Update(Item : TCollectionItem); override;
  public
    function Add: TCWLegendItem;
    function Insert(Index: integer): TCWLegendItem;
    function IndexOf(ALegend: TCWLegend): integer;
    // procedure Assign(Source: TPersistent); override;
    property Items[AIndex: integer]: TCWLegendItem read GetItem write SetItem;
  end;

  { TCWLegend ------------------------------------------------------------------ }

  TCWLegend = class(TComponent)
  private
    FContents: TLegendContents;
    FContentFlow: TContentFlow;
    FText: TStrings;
    FBullets: TLegendBullets;
    FBorder: Boolean;
    FFont: TFont;
    FBrush: TBrush;
    FWidth: integer;
    FTextWidth: integer;
    FTextHeight: integer;
    FHeight: integer;
    FContentList: TLegContentList;
    FLineHeight: integer;
    FVertMargins: integer;
    FHorizMargins: integer;
    FAnchoring: TLegendAnchoring;
    FAlignment: TLegendAlignment;
    FPointLocating: TPointLocating;
    FPointName: string;
    FPointValue: string;
    FPointSeriesIndex: integer;
    FPointItemIndex: integer;
    FVisible: Boolean;
    FPointOptions: TPointOptions;
    FColoredText: Boolean;
    FTransparency: integer;
    FOnLocatePoint: TLocatePointEvent;
    FWID : TWriterListElement;

    function GetOwnerChart : TCWChart;
    function GetLeft: integer;
    function GetTop: integer;
    function GetCanvas: TCanvas;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetInsideGraph: Boolean;
    procedure SetPointItemIndex(Value: integer);
    procedure SetPointSeriesIndex(Value: integer);
    procedure SetPointValue(Value: string);
    procedure SetPointName(Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetTransparency(Value: integer);
    function AxisPosition: TAxisPosition;
    procedure CreateContent;
    procedure SetText(Value: TStrings);
    function Summary: Boolean;
    property Canvas: TCanvas read GetCanvas;
    property InsideGraph: Boolean read GetInsideGraph;
    function GetWriter : TChartWriter;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Chart: TCWChart read GetOwnerChart;
    property Left: integer read GetLeft;
    property Top: integer read GetTop;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Writer : TChartWriter read GetWriter;
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Draw;
    procedure Assign(Source: TPersistent); override;
  published
    property Anchoring: TLegendAnchoring read FAnchoring write FAnchoring
      default anRightOutside;
    property Contents: TLegendContents read FContents write FContents
      default [coSeriesTitle];
    property ContentFlow: TContentFlow read FContentFlow write FContentFlow
      default cfTopBottom;
    property Visible: Boolean read FVisible write SetVisible default True;
    property HorizMargins: integer read FHorizMargins write FHorizMargins
      default 5;
    property VertMargins: integer read FVertMargins write FVertMargins
      default 5;
    property Text: TStrings read FText write SetText;
    property Bullets: TLegendBullets read FBullets write FBullets
      default lbSquare;
    property Alignment: TLegendAlignment read FAlignment write FAlignment
      default laLeftOrTop;
    property Border: Boolean read FBorder write FBorder default False;
    property Font: TFont read FFont write FFont;
    property Brush: TBrush read FBrush write FBrush;
    property PointName: string read FPointName write SetPointName;
    property PointValue: string read FPointValue write SetPointValue;
    property PointSeriesIndex: integer read FPointSeriesIndex
      write SetPointSeriesIndex default 0;
    property PointItemIndex: integer read FPointItemIndex
      write SetPointItemIndex default 0;
    property PointLocating: TPointLocating read FPointLocating
      write FPointLocating default plNameValue;
    property PointOptions: TPointOptions read FPointOptions write FPointOptions
      default [poShowConnectionLine, poEnlargePoint];
    property Transparency: integer read FTransparency write SetTransparency
      default 255;
    property ColoredText: Boolean read FColoredText write FColoredText
      default True;
    property OnLocatePoint: TLocatePointEvent read FOnLocatePoint
      write FOnLocatePoint;
  end;

  TSeries = class(TGraphObject)
  private
    FSeriesItems: TSeriesItems;
    FPoints: TPointArray;
    FBarPoints: TPointArray;
    FPieState: integer;
    FTrackPies: TTrackPies; { Logs th coords of the pislices }
    FPieRect: TRect;
    FDoughnutRect: TRect;
    FIndent, FExdent: integer;
    FLeapdateCount: integer;
    FStartDate, FEndDate: TDateTime;
    FNegVals : Boolean;
    FMin, FMax, FSum : single;
    FLinkTitle : string;
    function PosFromNamVal(AName: string; AVal: single): TPoint;
    function GetTitle : string;
    function GetItemPoints: TPointArray;
    function GetGraph: TCWGraph;
    function GetVisible: Boolean;
    function GetCount: integer;
    function GetItemCount: integer;
    function GetFirstItem: integer;
    function GetLastItem: integer;
    function GetItems(Index: integer): TSeriesItem;
    function GetPieTitleRect: TRect;
    function GetIndex: integer;
    function GetStartDate: TDateTime;
    function GetEndDate: TDateTime;
    function GetUseItemColors: Boolean;
    function ToDate(Index: integer): TDateTime;
    { The following two define subsets of orig cords. Default 1 means 1:1, then 1:2, etc.
      Resolutions less than 1 uses the average value of the points involved }
    property Points: TPointArray read FPoints write FPoints;
    property SeriesItems: TSeriesItems read FSeriesItems; { Full span }
    { To access the Series items, user must use the SeriesItems property of TChartWriter.
      Seriesitems here works with the full span }
    property FirstItem: integer read GetFirstItem;
    property LastItem: integer read GetLastItem;
    property Count: integer read GetCount; { Full span }

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TSeries; FromIndex, ToIndex: integer);
    procedure ToPercentages;
    procedure ToRealValues;
    function IndexOfName(AName: string): integer; { 0-based }
    function IndexOfDate(Y, M, D: word): integer; { 0-based }
    function IndexOfTime(H, M, S: integer): integer; { 0-based }
    function IndexOfValue(AValue: single): integer; { 0-based }
    function Avg: single;
    function MaxVal: single;
    function MinVal: single;
    function Sum: single;
    function Median: single;
    function Mode: TModeNumbers;
    function AddItem: TSeriesItem;
    function AsStrings(IncludeItemProps: Boolean = False): TStringList;
    property Graph: TCWGraph read GetGraph;
    property ItemCount: integer read GetItemCount; { 0 based }
    property Items[Index: integer]: TSeriesItem read GetItems; { 0 based }
    property ItemPoints: TPointArray read GetItemPoints;
    property Index: integer read GetIndex; { In the Data list }
    property Visible: Boolean read GetVisible;
    property UseItemColors: Boolean read GetUseItemColors;
    property Title: string read GetTitle;
    property PieRect: TRect read FPieRect;
    property DoughnutRect: TRect read FDoughnutRect;
    property PieTitleRect: TRect read GetPieTitleRect;
    property StartDate: TDateTime read GetStartDate write FStartDate;
    property EndDate: TDateTime read GetEndDate write FEndDate;
    { Used with name types that is not ntDateSpan, but when data is extracted
      from a date span. Can be used to create a legend }
  end;

  THistoryItem = class(TGraphObject)
    Series: TData;
    Graph: TCWGraph;
    Chart : TCWChart;
    NameSect: TCWNameSectionDefs;
    ValueSect: TCWValueSectionDefs;
    XUnit, YUnit: integer;
    AxisOrientation: TAxisOrientation;
    States: TStates;
    ZoomStart, ZoomEnd: integer;
    Names: TStringList;
    NameSections: TSections;
    ValueSections: TSections;
    Contraction: integer;
    NameLabelFreq: integer;
    ValueLabelFreq: integer;
    ValueLabelFreq2: integer;
    NameSectionFreq: integer;
    ValueSectionFreq: integer;
    UseNamShortnames: Boolean;
    UseSeriesCurveStyles: Boolean;
    UseValShortnames: Boolean;
    TimeFormat: string;
    MouseTimeFormat: string;
    AutoSections: TAutoSections;
    LowDate: TDateTime;
    HighDate: TDateTime;
    GraphWidth: integer;
    GraphHeight: integer;
    OrigSize: TSize;
    OrigData: TData;
    ContractionBase: TData;
    ScrollIndex: integer;
    HighestVal: integer;
    LowestVal: integer;
    constructor Create;
    destructor Destroy; override;
    procedure Assign;
    procedure AssignSections(ADest, ASource: TSections);
    procedure Restore;
  end;

  THistory = TObjectList<THistoryItem>;

  TRulerHintInfo = record
    Text: TStringList;
    Clrs: array [0 .. 10] of TColor;
    SerIndexes: array [0 .. 10] of integer;
    SerItmIndexes: array [0 .. 10] of integer;
    SerPos: array [0 .. 10] of TPoint;
    SerCount: integer;
  end;

  TYearInfo = record
    Year: integer;
    StartIndex, EndIndex: integer;
    SerIndex: integer;
  end;

  TYearMap = record
    RealYear, LogYear: word;
  end;

  TYears = TList<TYearInfo>;
  TFiles = TObjectList<TStringList>;

  TCWFileReader = class(TGraphObject)
  private
    FDataContent: TFiles;
    FGraphs: TFiles;
    FProps: TStringList;
    function GetSeriesCount: integer;
    function GetItemCount(SeriesIndex: integer): integer;
    function GetPropValue(APropName: string): string;
    function GetItemValue(SeriesIndex, ItemIndex: integer): single;
    function GetItemName(SeriesIndex, ItemIndex: integer): string;
    function GetItemColor(SeriesIndex, ItemIndex: integer): TColor;
    function GetRealDate(SeriesIndex, ItemIndex: integer): TDateTime;
    function GetRealValue(SeriesIndex, ItemIndex: integer): single;
    function GetSeriesTitle(SeriesIndex: integer): string;
    function GetID(SeriesIndex: integer): string;
  public
    constructor Create(AFileName: TFileName; AChartWriter: TChartWriter);
    destructor Destroy; override;
    function GetSeriesStrings(SeriesIndex: integer): TStringList;
    property SeriesCount: integer read GetSeriesCount;
    property ItemCount[SeriesIndex: integer]: integer read GetItemCount;
    property ItemValue[SeriesIndex, ItemIndex: integer]: single
      read GetItemValue;
    property ItemColor[SeriesIndex, ItemIndex: integer]: TColor
      read GetItemColor;
    property ItemRealDate[SeriesIndex, ItemIndex: integer]: TDateTime
      read GetRealDate;
    property ItemRealValue[SeriesIndex, ItemIndex: integer]: single
      read GetRealValue;
    property PropValue[PropName: string]: string read GetPropValue;
    property ItemName[SeriesIndex, ItemIndex: integer]: string read GetItemName;
    property SeriesTitle[SeriesIndex: integer]: string read GetSeriesTitle;
    property ID[SeriesIndex: integer]: string read GetID;
  end;

  TAxisObject = class(TGraphObject)
  private
    FPosition: TAxisPosition;
    FPartner: TAxisObject;
    FLabelTextRect: TRect;
    FLabelHookRect: TRect;
    FGDIP: TGPGraphics;

    function GetCanvas: TCanvas;
    procedure GetGDI;
    function GetGrRect: TRect;
    function GetCount: integer; virtual; abstract;
    function GetLabelFreq: integer; virtual;
    function GetLabelSpace: integer; virtual;
    function GetSectionSpace: integer; virtual;
    function GetSectionCount: integer; virtual;
    function GetSections: TSections; virtual;
    function GetSectionRect(Index: integer): TRect; virtual; { Label text rect }
    function GetUnitSize: integer; virtual;
    function GetLabelRect: TRect;
    function GetFloorPoly: TIntPointArray;
    function GetWallPoly: TIntPointArray;
    function GetFloorRect: TRect;
    function GetWallRect: TRect;
    function Get3DPoly: TIntPointArray;
    function GetWallWidth: integer;
    function GetWallOffset: integer;
    function GetOpposedSections: Boolean;
    function GetSectionSpaceRect: TRect; { Rect of all section labels }
    function GetUnitCount: integer; virtual;
    function GetIsXAxis: Boolean;
    procedure GetAxisHookPos(X, Y: integer; var StartPt, EndPt: TPoint);
    function GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint; virtual;
    procedure Draw;
    procedure DrawAxis;
    procedure Draw3DAxis;
    procedure DrawLabels; virtual; abstract;
    procedure DrawSections;
    property GrRect: TRect read GetGrRect;
    property LabelFreq: integer read GetLabelFreq;
    property LabelRect: TRect read GetLabelRect;
    property SectionSpaceRect: TRect read GetSectionSpaceRect;
    property SectionCount: integer read GetSectionCount;
    property Sections: TSections read GetSections;
    property LabelSpace: integer read GetLabelSpace;
    property OpposedSections: Boolean read GetOpposedSections;
    property SectionSpace: integer read GetSectionSpace;
    property UnitCount: integer read GetUnitCount;
    property UnitSize: integer read GetUnitSize;
    property Canvas: TCanvas read GetCanvas;

  public
    constructor Create;
    destructor Destroy; override;
    property IsXAxis: Boolean read GetIsXAxis;
    property Position: TAxisPosition read FPosition write FPosition;
  end;

  TValueAxis = class(TAxisObject)
  private
    function GetCount: integer; override;
    function GetLabelFreq: integer; override;
    function GetLabelSpace: integer; override;
    function GetSectionSpace: integer; override;
    function GetSectionRect(Index: integer): TRect; override;
    function GetSectionCount: integer; override;
    function GetSections: TSections; override;
    function GetUnitSize: integer; override;
    function GetUnitCount: integer; override;
    function GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint; override;
    function GetInterval: single;
    function GetQualifier: string;
    function GetLow: single; virtual;
    function GetHigh: single; virtual;
    procedure DrawLabels; override;
    function GetExactValuePos( AValue: single) : integer;
    property Interval: single read GetInterval;
    property LowVal: single read GetLow;
    property HighVal: single read GetHigh;
    property Count: integer read GetCount;
  end;

  TValueAxis2 = class(TValueAxis)
  private
    function GetCount: integer; override;
    function GetHigh: single; override;
    function GetLow: single; override;
    function GetLabelFreq: integer; override;
  end;

  TNameAxis = class(TAxisObject)
  private
    function GetCount: integer; override;
    function GetLabelFreq: integer; override;
    function GetLabelSpace: integer; override;
    function GetSectionSpace: integer; override;
    function GetSectionCount: integer; override;
    function GetSectionRect(Index: integer): TRect; override;
    function GetSections: TSections; override;
    function GetUnitSize: integer; override;
    function GetUnitCount: integer; override;
    function GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint; override;
    procedure DrawLabels; override;
    function PosFromDate(ADate: TDateTime): integer;
    function PosFromHour(ADate: TDateTime): integer;
    function PosFromMinute(ADate: TDateTime): integer;
    function PosFromSecond(ADate: TDateTime): integer;
    function PosFromNumber(ANumber: single): integer;
  end;

  TCWMargins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: integer;
    FWriter: TChartWriter;
    procedure SetLeft(Value: integer);
    procedure SetTop(Value: integer);
    procedure SetRight(Value: integer);
    procedure SetBottom(Value: integer);
    function CheckMargin(OldValue, NewValue: integer): Boolean;
    procedure UpdateValue;
  public
    property Writer: TChartWriter read FWriter;
  published
    property Left: integer read FLeft write SetLeft default 0;
    property Top: integer read FTop write SetTop default 0;
    property Right: integer read FRight write SetRight default 0;
    property Bottom: integer read FBottom write SetBottom default 0;
  end;

  TCWValueAxis = class(TPersistent)
  private
    FValueIntervals: single;
    FValueHigh: single;
    FValueLow: single;
    FValueSpanFromData: Boolean;
    FValuePrecision: integer;
    FValueUnit : integer;
    FQualifier : string;
    FQualifierFits : Boolean;
    FOwner : TCWChart;
  private
    function GetWriter : TChartWriter;
    function GetValueCount: integer;
    function GetValueFloatUnit: single;
    procedure SetValueSpanFromData(Value: Boolean);
    procedure SetValueHigh(Value: single);
    procedure SetValueLow(Value: single);
    procedure SetValueIntervals(Value: single);
    procedure SetValuePrecision(Value: integer);
    procedure SetQualifier(Value : string);
    procedure SetHighLow;
  public
    property Writer : TChartWriter read GetWriter;
    property ValueCount: integer read GetValueCount;
    property ValueFloatUnit: single read GetValueFloatUnit;
    procedure SetValueSpan(LowValue, HighValue: single);
    procedure Assign(Source : TPersistent); override;
  published
    property Qualifier : string read FQualifier write SetQualifier;
    property ValuePrecision: integer read FValuePrecision
      write SetValuePrecision default 0;
    property ValueIntervals: single read FValueIntervals
      write SetValueIntervals;
    property ValueHigh: single read FValueHigh write SetValueHigh;
    property ValueLow: single read FValueLow write SetValueLow;
    property ValueSpanFromData: Boolean read FValueSpanFromData
      write SetValueSpanFromData default False;
  end;

  TCWChart = class(TComponent)
  private
    FWriter: TChartWriter;
    FWID : TWriterListElement;
    FNameType: TNameType;
    FItemColors: TCWColors;
    FSeriesDefs : TCWSeriesDefs;
    FValueAxis1 : TCWValueAxis;
    FValueAxis2 : TCWValueAxis;
    FNumSpanPrecision: integer;
    FLegends : TCWLegends;
    FSeriesRects: TLegendRects;
    FPointRects: TLegendRects;
    FSummaryRects: TLegendRects;
    FNameSectionDefs : TCWNameSectionDefs;
    FValueSectionDefs : TCWValueSectionDefs;
    FTitle : string;
    FOverflowAction : TOverFlowAction;
    FValuePrecision : integer;
    FCalcPercentages : Boolean;
    FWallWidth : integer;
    FAxisElements : TAxisElements;
    FAxisOrientation : TAxisOrientation;
    FAxisColor: TColor;
    FWallColor: TColor;
    FGradientWall: Boolean;
    FTextTilting : TTextOrientations;
    FTextTiltThreshold : integer;
    FFileName : TFileName;
    FDataset : TDataset;
    FNameField, FValueFields : string;
    FOnTitle : TOnTitleEvent;
    FOnGetData : TNotifyEvent;
    function GetAxisCount : integer;
    procedure SetWallWidth(Value : integer);
    function GetWallWidth : integer;
    procedure SetAxisOrientation(Value: TAxisOrientation);
    procedure SetAxisElements(Value : TAxisElements);
    procedure SetNameType(Value: TNameType);
    procedure SetWriter(Value: TChartWriter);
    procedure SetNameSectionDefs(Value : TCWNameSectionDefs);
    procedure SetValueSectionDefs(Value : TCWValueSectionDefs);
    procedure SetValuePrecision(Value : integer);
    procedure SetOverflowAction(Value : TOverflowAction);
    procedure SetTitle(Value : string);
    procedure SetWallColor(Value: TColor);
    procedure SetGradientWall(Value: Boolean);
    procedure SetAxisColor(Value: TColor);
    procedure SetTextTilting(Value : TTextOrientations);
    procedure SetTextTiltThreshold(Value : integer);
    procedure SetNumSpanPrecision(Value : integer);
    procedure SetFileName(Value : TFileName);
    procedure SetDataset(Value : TDataset);
    function GetVisibleCount : integer;
    function GetTitle : string;
    function GetWriter : TChartWriter;

    function ValAx1Graph : TCWGraph;
    procedure CreateLegendContent;
    procedure CheckQualifiers;
    procedure DrawLegends;
    procedure DrawTitle;
    procedure DrawQualifiers;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property NameSectionDefs : TCWNameSectionDefs read FNameSectionDefs write SetNameSectionDefs;
    property OverflowAction : TOverflowAction read FOverFlowAction write SetOverflowAction default ovNone;
    property ValueSectionDefs : TCWValueSectionDefs read FValueSectionDefs write SetValueSectionDefs;
    property ValueAxis1 : TCWValueAxis read FValueAxis1 write FValueAxis1;
    property ValueAxis2 : TCWValueAxis read FValueAxis2 write FValueAxis1;
    property ValuePrecision : integer read FValuePrecision write SetValuePrecision default 0;
    property NameType: TNameType read FNameType write SetNameType default ntGeneral;
    property NumSpanPrecision: integer read FNumSpanPrecision write SetNumSpanPrecision default 0;
    property CalcPercentages : Boolean read FCalcPercentages write FCalcPercentages default false;
    property AxisOrientation : TAxisOrientation read FAxisOrienTation write SetAxisOrientation default alBottomLeft;
    property WallWidth : integer read GetWallWidth write SetWallWidth default 0;
    property GradientWall: Boolean read FGradientWall write SetGradientWall
      default False;
    property WallColor: TColor read FWallColor write SetWallColor default clWindow;
    property AxisColor: TColor read FAxisColor write SetAxisColor default clBlack;
    property AxisElements : TAxisElements read FAxisElements write SetAxisElements default [veValueLabels, veNameLabels];
    property TextTilting : TTextOrientations read FTextTilting write SetTextTilting default [];
    property TextTiltThreshold : integer read FTextTiltThreshold write SetTextTiltThreshold default 1;

  public
    property AxisCount : integer read GetAxisCount;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearSeriesDefs;
    procedure SetColor(Index : integer; Usage : TColorUsage; AColor : TColor);
    procedure SetSeriesDef(Index : integer; ATitle : string; AGraph : TCWGraph;
     AColor : TColor; AValueAxis : TValueAxisNumber);
    procedure AddSeriesDef(ATitle : string; AGraph : TCWGraph; AColor : TColor; AValueAxis : TValueAxisNumber);
    procedure ReplaceGraphs(NewGraph : TCWGraph);
    function AllEqual : TCWGraph;
    function GraphCount(AGraph : TCWGraph) : integer;
    function IsActive : Boolean;
    property VisibleCount : integer read GetVisibleCount;
    property Writer: TChartWriter read GetWriter;

  published
    //property Writer: TChartWriter read FWriter write SetWriter;
    property ItemColors: TCWColors read FItemColors write FItemColors;
    property SeriesDefs : TCWSeriesDefs read FSeriesDefs write FSeriesDefs;
    property Legends : TCWLegends read FLegends write FLegends;
    property Title : string read GetTitle write SetTitle;
    property FileName : TFileName read FFileName write SetFileName;
    property Dataset : TDataset read FDataset write SetDataset;
    property NameField : string read FNameField write FNameField;
    property ValueFields : string read FValueFields write FValueFields;
    property OnTitle : TOnTitleEvent read FOnTitle write FOnTitle;
    property OnGetData : TNotifyEvent read FOnGetData write FOnGetData;

  end;

  TCWAxisChart = class(TCWChart)
  published
    property NameSectionDefs;
    property OverflowAction;
    property ValueSectionDefs;
    property ValueAxis1;
    property ValueAxis2;
    property NameType;
    property AxisOrientation;
    property AxisElements;
    property WallWidth;
    property WallColor;
    property AxisColor;
    property GradientWall;
    property TextTilting;
    property TextTiltThreshold;
    property NumSpanPrecision;
  end;

  TCWPieChart = class(TCWChart)
  private
  published
   property ValuePrecision;
   property CalcPercentages;
   property ValueAxis1;
   property AxisElements;
   property OverflowAction;
  end;

  TChartWriter = class(TCustomControl)
  private
    { Private declarations }
    FDsgnData : TObjectList<TStringList>;
    FRightSpace : integer;
    FBottomSpace : integer;
    FChart : TCWChart;
    FActiveValAx : TAxisObject;
    FBorder: Boolean;
    FWallAngle: single;
    FLanguage: String;
    FUpdateKinds: TUpdateKinds; { Used with Begin/EndUpdate. }
    FAppEvent: TApplicationEvents;
    FTimeFormat: string;
    FMouseTimeFormat: string;
    FGraphBorders: TGraphBorders;
    FInnerMargins: TCWMargins;
    FGraphMargins: TCWMargins;
    FKeepHistory: Boolean;
    FLeapdateCount: integer;
    FNameFont: TFont;
    FNumberInterval: integer; { Helps to control equality of series }
    FMouseInfo: TMouseInfo;
    FScrollIndex: integer; { Pointer to data start, when scrollable }
    FScrolling: Boolean;
    FHintSeries: integer; { The Series that displays the mouse move hint }
    FHintItem: integer;
    FLowDate, FHighDate: TDateTime; { Date span in logical( leading) years }
    FMousePrecision: TMousePrecision;
    FRulerX, FRulerY: integer; { Position of ruler }
    FRulers: TRulers;
    FMouseSelect: Boolean;
    FSelectionExec: TSelectionExec;
    { How to transform a selected area into a dynamic section }
    FInternalLeading: integer;
    FYears: TYears;
    FLogYearMap: array of TYearMap;
    FInfoControl: Boolean;
    FSeriesData: TData;
    FChartList : TChartList;
    FOrigData: TData;
    FContractionBase: TData;
    { The series that are used as a bas for computing Contraction }
    FZoomStart, FZoomEnd: integer;
    FValAx: TValueAxis;
    FValAx2: TValueAxis2;
    FNamAx: TNameAxis;
    FValueFont: TFont;
    FTitleFont : TFont;
    FTitleAlignment : TAlignment;
    FCentered : Boolean;

    FViewIndex: integer;
    FTrackBars: TTrackBars;
    { Logs the bar rectangles to make it easy to track them by the mouse }

    FWidestName: String;
    FWidestValueSection: string;
    FWidestNameSection: string;
    FWidestNameShortSection: string;
    FWidestValueShortSection: string;
    FTallestName: string;
    FTallestValueSection: string;
    FTallestNameSection: string;
    FTallestValueShortSection: string;
    FTallestNameShortSection: string;
    FUseNamShortNames: Boolean;
    FUseValShortNames: Boolean;

    FWidestValue: string;
    FWidestValue2: string;
    FTallestValue: string;
    FTallestValue2 : string;
    { Widest/highest text of the Series }
    FHWBM: TBitmap;
    { Helper to compute text dimensions. See GetTextWidth og GetTextHeight }
    FNameUnit: integer;
    { Number of pixels between each point on x axis. Only used to check threasholds.
      Instaed: the computed ValueFloatUnit og NameFloatUnit }
    FNameList: TStringList; { Values of X and Y axis: }
    FContraction: integer;
    FContractionType: TContractionType;
    FBarPoints: Boolean;
    FKeepSelection: Boolean;
    FOrigGraphSize: TSize;
    FGraphBGColor: TColor;
    FBall: Vcl.Graphics.TBitmap;
    FBallSmall: Vcl.Graphics.TBitmap;
    { The following are used with selecting }
    FSelectingRect: TRect;
    FSelStartX: integer;
    FSelstartY: integer;
    FLastMouseX: integer;
    FLastMouseY: integer;
    FMouseDistanceX: integer;
    FMouseDistanceY: integer;
    FOldActive: TCWGraph;
    FInSerInd: integer; { <> -1 if mouse is in this point }
    FInItemInd: integer;
    FStates: TStates;
    FRulerVisible: Boolean;
    FRulerPosition: TPoint;
    FLastMousePos: TPoint;
    { General tracking to determine mouse move direction }
    FNameSections: TSections;
    FValueSections: TSections;
    FNameLabelFreq: integer; { Frequency of label show }
    FValueLabelFreq: integer;
    FValueLabelFreq2: integer;
    FNameSectionFreq: integer;
    FValueSectionFreq: integer;
    FNameLabelSpace: integer; { Label space at x and y }
    FValueLabelSpace: integer;
    FDynaSectStart, FDynaSectEnd: integer;
    FViewMode: TViewMode;
    FHistory: THistory;
    FHintText: string;
    FErrorText: string;
    FHintColor: TColor;
    FLongestSeries : TSeries;
    FPosRect : TRect;  {Static for GetPosrect, used with point computing}
    FSelBM: Vcl.Graphics.TBitmap;
    FAlfaBM: Vcl.Graphics.TBitmap;
    FContractionCount: integer;
    { Debug, picking up endles Contract loop, see RestrictToClient }
    FAnimBM: Vcl.Graphics.TBitmap;
    FAnimInfo: TAnimInfo;
    FDividerLinePen: TPen;
    FTextComputed : Boolean; {Flag to prevent ComputeTextExtent to run twice}
    FNeedsIds : Boolean;

    FCallCounter : integer; {CheckLabelfreqs}

    FAfterDrawGraph: TDrawGraphEvent;
    FOnDrawSection: TDrawSectionEvent;
    FOnDrawLabel: TDrawLabelEvent;
    FOnContraction: TContractionEvent;
    FOnMouseInfo: TMouseInfoEvent;
    FOnSelected: TNotifyEvent;
    FOnMeasureLabel: TMeasureLabelEvent;
    FOnRuler: TNotifyEvent;
    FOnZoom: TNotifyEvent;
    FOnDataChange: TNotifyEvent;
    FOnQuerySpace : TQuerySpaceEvent;

    procedure WMSAVESELBM(var Msg: Tmessage); message WM_SAVESELBM;
    procedure WMSAVEHIST(var Msg: Tmessage); message WM_SAVEHIST;
    procedure WMENDEXEC(var Msg: Tmessage); message WM_ENDEXEC;
    procedure WMERROR(var Msg: Tmessage); message WM_ERROR;
    procedure WMLOADFILE(var Msg : TMessage); message WM_LOADFILE;
    procedure PostHist;
    procedure AppMsg(var Msg: tagMsg; var Handled: Boolean);
    procedure ComputeTextExtent;
    procedure CheckLabelFreqs(ComputePts: Boolean = True);
    function CheckPieValues(Values: TSeriesItems; APie: TCWPie): integer;
    procedure RepaintSelRect;
    procedure AddSeries(ASeries: TSeries; Internal: Boolean = False); overload;
    function AxisOf(Posit: TAxisPosition): TAxisObject;
    procedure CreateAutoSections;
    function GetActiveGraph: TCWGraph;
    function GetScrollPointSpacing: integer;
    function GetContraction: integer;
    function GetCount: integer;
    function GetGraphRect: TRect;
    function GetGraphSpaceRect: TRect;
    function GetGraphWidth: integer;
    function GetGraphHeight: integer;
    function GetGraphLeft: integer;
    function GetGraphTop: integer;
    function GetInfoControl: Boolean;
    function GetIsUpdating: Boolean;
    function GetNameSectionDefs: TCWNameSectionDefs;
    function GetWorkRect: TRect;
    function GetRendered: Boolean;
    function GetSeries(Index: integer): TSeries;
    function GetSeriesItems(SeriesIndex, ItemIndex: integer): TSeriesItem;
    function GetSelRect: TRect;
    function GetTrackBar(ASeriesIndex, AItemIndex: integer;
      var ATrackBar: TTrackBar): Boolean;
    function GetViewMode: TViewMode;
    function GetVisibleCount: integer;
    procedure SetContraction(Value: integer);
    function GetNameCount: integer;
    function GetNames(Index: integer): string;
    function GetMouseTimeFormat: string;
    function GetValuePrecision: integer;
    function GetValueIntervals: single;
    function GetValueHigh: single;
    function GetValueLow: single;
    function GetValueSectionDefs: TCWValueSectionDefs;
    function GetValueSpanFromData: Boolean;
    function GetNumSpanPrecision: integer;
    procedure SetValuePrecision(Value: integer);
    function GetNameFloatUnit: single;
    function GetNameLabelSpace: integer;
    function GetMousePrecision: TMousePrecision;
    function GetNameSectionSpace: integer;
    function GetValueSectionSpace: integer;
    function GetNameLabelRect: TRect;
    function GetSectionHorzMargin(Axis: TAxisType): integer;
    function GetSectionVertMargin(Axis: TAxisType): integer;
    function GetValueLabelRect: TRect;
    function GetValueCount: integer;
    function GetValueFloatUnit: single;
    function GetNameSections(Index: integer): TSection;
    function GetNameSectionCount: integer;
    function GetWallColor : TColor;
    function GetAxisColor : TColor;
    function GetGradientWall : Boolean;
    function GetTextTilting : TTextOrientations;
    function GetTextTiltThreshold : integer;
    function DetectNameType(SeriesIndex: integer): TNameType;
    function GetValueSections(Index: integer): TSection;
    function GetValueSectionCount: integer;
    function GetTextWidth(LabelKind: TLabelKind; Angle: integer = 0)
      : integer; overload;
    function GetTextWidth(LabelKind: TLabelKind; AText: string;
      Angle: integer = 0): integer; overload;
    function GetTextHeight(LabelKind: TLabelKind; Angle: integer = 0): integer;
    function GetWallWidth: integer;
    function GetAxisOrientation : TAxisOrientation;
    procedure SetLabelFont(LabelKind: TLabelKind);
    procedure SetLanguage(Value: string);
    procedure ResetCanvas;
    procedure SaveSelBM;
    procedure SetChart(Value : TCWChart);
    procedure SetBorder(Value: Boolean);
    procedure SetPosition(Orientation: TAxisOrientation);
    procedure SetGraphBorders(Value: TGraphBorders);
    procedure SetInfoControl(Value: Boolean);
    procedure SetTimeFormat(Value: string);
    procedure SetNumSpanPrecision(Value: integer);
    procedure SetNameLabelFreq(Value: integer);
    procedure SetValueLabelFreq(Value: integer);
    procedure SetNameUnit(Value: integer);
    procedure SetValueIntervals(Value: single);
    procedure SetValueLow(Value: single);
    procedure SetValueHigh(Value: single);
    procedure SetValueSpanFromData(Value: Boolean);
    procedure SetHighLow;
    procedure SetGraphBGColor(Value: TColor);
    procedure SetCentered(Value : Boolean);
    procedure SetRulers(Value: TRulers);
    function  GetAxisElements : TAxisElements;
    procedure SetTitleAlignment(Value : TAlignment);
    procedure SetScrollIndex(Value: integer);
    procedure RestrictToClient(Restore: Boolean);
    procedure AssignOrigData;
    procedure CreateXValues;
    function LongestSeries: TSeries;
    function CanScrollTo(AScrollIndex: integer): Boolean;
    procedure ConcludeSelecting(XPos, YPos: integer);
    function IsLeapDate(ASeries: TSeries; ItmIndex: integer): Boolean;
    function PointsFromRuler(Vertical: Boolean; X, Y: integer; var APos: TPoint;
      var SeriesDefs: TSeriesDefs): Boolean;
    procedure ComputePoints(ASeries: TSeries = nil; Recalc: Boolean = False);
    function ComputeTextPos(X, Y: integer; ALabel: string;
      AnAxis: TAxisObject): TPoint;
    procedure DoScrollTo(AScrollIndex: integer);
    procedure DoLoadFiles(AFileName: string; Files: TFiles);
    function SetHintText(X, Y: integer; AGraph: TCWGraph): Boolean; overload;
    function SetHintText(X, Y: integer; SerInd, ItmInd: integer;
      AGraph: TCWGraph): Boolean; overload;
    procedure InternalClear;
    function InState(AState: TState): Boolean;
    function GraphTypeInChart(AGraphType : TClass) : Boolean;
    function InSpan(ASeries: TSeries; LowVal, HighVal: single;
      var ErrorNumber: single): Boolean; overload;
    function InSpan(LowVal, HighVal: single; var ErrorNumber: single)
      : Boolean; overload;
    function RealYearToLogYear(ARealYear: word): word;
    procedure ClearState(AState: TState);
    procedure SetState(AState: TState);
    procedure KeyD(Key: word; Shift: TShiftState);
    procedure KeyU(Key: word; Shift: TShiftState);
    procedure WMERASEBKGND(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseLeave(var Message: TWMMouse); message WM_MOUSELEAVE;
    procedure MakeDateSpan;
    procedure MakeTimeSpan(ANameType: TNameType);
    procedure MakeNumberSpan;
    procedure NormaliseDates(ASeries: TSeries; SerIndx: integer);
    procedure DoSaveToHistory;
    function SeriesIndexOfX(APos: TPoint; var SeriesIndex: integer): integer;
    function SeriesIndexOfY(APos: TPoint; var SeriesIndex: integer): integer;
    function SpaceOf(Position: TAxisPosition): integer;
    function GraphSectionFromPoint(X, Y: integer): integer;
    function XFromName(AValue: string): integer;
    function YFromName(AValue: string): integer;
    function GetPosRect(SeriesCount: integer = -1): TRect;
    function GetGraphPrintRect: TRect;
    procedure GoBackError;
    procedure DrawBorders;
    procedure DoZoom(StartIndex, EndIndex: integer);
    function IsPointVisible(AScrollIndex: integer): Boolean;
    function DoPosInBar(X, Y: integer;
      var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function DoPosInPoint(X, Y: integer; Precision: TMousePrecision;
      var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function DoPosInPieSlice(X, Y: integer;
      var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function GetValPrecision: integer;
    function GetTitleSpace : integer;
    function ValAxFromGraph(AGraph: TCWAxisGraph): TValueAxis;
    function InView(AGraphType: TClass; var Index: integer): TCWGraph; overload;
    function InView(AGraphType: TClass): TCWGraph; overload;
    function IndexOfNearestName(AName: string; StartWith: integer = 0): integer;
    procedure DoContractValues(Rate: integer; ContractType: TContractionType);
    procedure DoRepaint;
    procedure GetProps(PropList: TStringList);
    procedure SetProps(PropList: TStringList);
    procedure LoadFromPropFile(AFileName: TFileName);
    procedure CancelBeacons;
    procedure DoAnimation;
    procedure InitAnimation;
    procedure CreateNameSections;
    procedure CreateValueSections;
    procedure FontChanged(Sender : TObject);
    procedure AddDsgnSeries(ANameType : TNameType; Indx : integer);
    procedure RenderDesigner(Deleted : integer = 0);
    procedure CreateIDS;

    property NameUnit: integer read FNameUnit write SetNameUnit;
    { Not used as property }
    property NameFloatUnit: single read GetNameFloatUnit;
    property NameLabelSpace: integer read GetNameLabelSpace;
    property NameSectionSpace: integer read GetNameSectionSpace;
    property ValueSectionSpace: integer read GetValueSectionSpace;
    property ViewMode: TViewMode read GetViewMode;
    property SelRect: TRect read GetSelRect;
    property WallAngle: single read FWallAngle;

  protected
    { Protected declarations }
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Resize; override;
    procedure Loaded; override;
    property MouseInfoControl: Boolean read GetInfoControl write SetInfoControl;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    AnimationTuner: TAnimationTuner;
    LiveGraphs : Boolean;
    InitDesigner : Boolean;
    DsgnRealData : Boolean;
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    function BaseNameInterval: integer;
    { The original intervals between the names in spanned series, as set by the user.
      When computing Contraction this value is subtracted from the result. }
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure ClearSelection;
    procedure RefreshGraph;
    function CanRender: integer;
    procedure GoBack;
    procedure GoForward;
    function CanGoBack: Boolean;
    function CanGoForWard: Boolean;
    procedure ChangeChart(AChart : TCWChart; Refresh : Boolean = false);
    procedure Execute;
    procedure CheckValspans;
    procedure CheckSeriesDefs;
    procedure ClearHistory;
    procedure ClearObjects;
    function AsBitmap(GraphElement: integer): TBitmap;
    procedure SetChartAs(AChart : TCWChart; AsGraph : TCWGraph);
    procedure ContractValues(Rate: integer; ContractType: TContractionType);
    function IndexOfName(AName: string): integer; { In the names list }
    function IndexOfNameEx(AName: string): integer; { In the orig list }
    function IndexOfDateEx(Y, M, D: word): integer;
    function IndexOfTimeEx(H, M, S: integer): integer;
    function IsTimeSpan(ANameType: TNameType): Boolean;
    function HasWall: Boolean;
    procedure LoadFromDatabase(ADataset : TDataset;
     NameField, ValueFields : string; ATitle : string = ''; DoExecute : Boolean = True);
    procedure LoadFromFile(AFileName: TFileName; DoExecute : Boolean = True);
    procedure SaveToFile(AFileName: TFileName; Options : TSaveOptions = soAll);
    function SpanToSeriesIndex(ASeriesIndex, ASpanIndex: integer): integer;
    function PosFromDate(ADate: TDateTime): integer;
    function PosFromHour(ADate: TDateTime): integer;
    function PosFromMinute(ADate: TDateTime): integer;
    function PosFromSecond(ADate: TDateTime): integer;
    function PosFromNumber(ANumber: single): integer;
    function PosFromValue(AValue: single; AAxis: TValueAxis): integer;
    function PosInNameSection(X, Y: integer; var SectionIndex: integer)
      : Boolean;
    function PosInValueSection(X, Y: integer;
      var SectionIndex: integer): Boolean;
    function MouseInPoint(var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function MouseInBar(var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function MouseInPieSlice(var SeriesIndex, SeriesItemIndex: integer)
      : Boolean;
    function NamValFromPos(const X, Y: integer; var Nam: string;
      var Val: single): Boolean;
    function PosInBar(X, Y: integer;
      var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function PosInPoint(X, Y: integer;
      var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function PosInPieSlice(X, Y: integer;
      var SeriesIndex, SeriesItemIndex: integer): Boolean;

    procedure AddSeries(ASeries: TSeries; ATitle : string = ''); overload;
    procedure AddSeries(ASeries: TSeries; ATitle : string; AGraph : TCWGraph;
     AColor : TColor; AValueAxis : TValueAxisNumber); overload;
    procedure AddSeries(Values: TStringList; ATitle : string = ''); overload;
    procedure AddSeries(Values: TStringList; ATitle : string; AGraph : TCWGraph;
     AColor : TColor; AValueAxis : TValueAxisNumber); overload;
    procedure AddSeries(ADataset: TDataSet; ANameField, ValueFields : string; ATitle : string = ''); overload;

    procedure MoveSeries(FromIndex, ToIndex: integer);
    function AddSection(OnAxis: TAxisType; AStart, AEnd, LongCaption,
      ShortCaption: string; SectionType: TSectionType): TSection;
    procedure DeleteSection(OnAxis: TAxisType; Index: integer);
    procedure ClearSections(OnAxis: TAxisType; Both: Boolean);
    procedure CreateSections;
    procedure RedrawGraphLines;
    function GetScrollable: Boolean;
    procedure GetValueSpan(var HighVal, LowVal: single); overload;
    procedure GetValueSpan(ASeries: TSeries; var HighVal, LowVal: single); overload;
    procedure SetSelection(StartIndex, EndIndex: integer;
      Add: Boolean = False); overload;
    function GetSelection(var StartIndex, EndIndex: integer): Boolean;
    procedure SaveAsImage(GraphElement: integer; AFileName: string);
    procedure SaveToHistory;
    function SeriesFromMouse(var ItemIndex: integer): TSeries;
    procedure Reload;
    function GetFileType(AFileName : string) : integer; {-1 invalid, 0 rich, 1 data}
    function AllVisible: Boolean;
    procedure ScrollTo(AScrollIndex: integer);
    procedure ScrollBy(Delta: integer);
    function CanScroll(ScrollType: TScrollType): Boolean;
    function CanScrollBy(Delta: integer): Boolean;
    procedure First;
    procedure Last;
    procedure NextPage;
    procedure PrevPage;
    procedure NextPoint;
    procedure PrevPoint;
    procedure RepaintGraph;
    function RulerPoints: TRulerPoints;
    procedure HideRuler;
    function SeriesOfTitle(ATitle: string): TSeries;
    procedure SetValueSpan(LowValue, HighValue: single);
    procedure Zoom; overload;
    procedure Zoom(StartIndex, EndIndex: integer); overload;
    procedure Zoom(NameSectionIndex: integer); overload;

    property Rendered: Boolean read GetRendered;
    property Count: integer read GetCount;
    property ChartList: TChartList read FChartList;
    property NameLabelFreq: integer read FNameLabelFreq write SetNameLabelFreq;
    property VisibleCount: integer read GetVisibleCount;
    property Contraction: integer read GetContraction write SetContraction;
    property WorkRect: TRect read GetWorkRect;
    property GraphPrintRect: TRect read GetGraphPrintRect;
    property GraphRect: TRect read GetGraphRect;
    property GraphSpaceRect: TRect read GetGraphSpaceRect;
    property GraphWidth: integer read GetGraphWidth;
    property GraphHeight: integer read GetGraphHeight;
    property GraphLeft: integer read GetGraphLeft;
    property GraphTop: integer read GetGraphTop;
    property NameCount: integer read GetNameCount;
    property Names[Index: integer]: string read GetNames;
    property NameLabelRect: TRect read GetNameLabelRect;
    property NameSections[Index: integer]: TSection read GetNameSections;
    property NameSectionCount: integer read GetNameSectionCount;
    property OldActive: TCWGraph read FOldActive;
    property ScrollIndex: integer read FScrollIndex write SetScrollIndex;
    property ValueSections[Index: integer]: TSection read GetValueSections;
    property ValueSectionCount: integer read GetValueSectionCount;
    property ValueLabelRect: TRect read GetValueLabelRect;
    property Series[Index: integer]: TSeries read GetSeries;
    property SeriesItems[SeriesIndex, ItemIndex: integer]: TSeriesItem
      read GetSeriesItems;
    property Scrollable: Boolean read GetScrollable;
    property IsUpdating: Boolean read GetIsUpdating;
    property ValueLabelFreq: integer read FValueLabelFreq
      write SetValueLabelFreq;
    property ValueLabelFreq2: integer read FValueLabelFreq2
      write FValueLabelFreq2;
    property ValueCount: integer read GetValueCount;
    property ValueIntervals: single read GetValueIntervals
      write SetValueIntervals;
    property ValueHigh: single read GetValueHigh write SetValueHigh;
    property ValueLow: single read GetValueLow write SetValueLow;
    property ValueSpanFromData: Boolean read GetValueSpanFromData
      write SetValueSpanFromData default False;
    property ValuePrecision: integer read GetValuePrecision
      write SetValuePrecision;
    property ValueFloatUnit : single read GetValueFloatUnit;
    property NameSectionDefs: TCWNameSectionDefs read GetNameSectionDefs;
    property ValueSectionDefs: TCWValueSectionDefs read GetValueSectionDefs;
    property NumSpanPrecision: integer read GetNumSpanPrecision
      write SetNumSpanPrecision default 0;
    property ActiveGraph: TCWGraph read GetActiveGraph;
    {Represents the master graph of a possibly mixed chart. If mixed the
    bar will always be the master, while the curve must adapt it's point postions
    to the bar center points}
    property AxisOrientation: TAxisOrientation read GetAxisOrientation;
    property AxisElements: TAxisElements read GetAxisElements;
    property WallWidth: integer read GetWallWidth;
    property AxisColor: TColor read GetAxisColor;
    property WallColor : TColor read GetWallColor;
    property GradientWall : Boolean read GetGradientWall;
    property TextTilting: TTextOrientations read GetTextTilting;
    property TextTiltThreshold: integer read GetTextTiltThreshold;



  published
    { Published declarations }
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StyleElements;

    property Border: Boolean read FBorder write SetBorder default False;
    property Centered : Boolean read FCentered write SetCentered default false;
    property ContractionType: TContractionType read FContractionType
      write FContractionType default ctIncremental;
    property Chart : TCWChart read FChart write SetChart;
    property GraphBGColor: TColor read FGraphBGColor write SetGraphBGColor
      default clWindow;
    property GraphBorders: TGraphBorders read FGraphBorders
      write SetGraphBorders default gbAxis;
    property GraphMargins: TCWMargins read FGraphMargins write FGraphMargins;
    property InnerMargins: TCWMargins read FInnerMargins write FInnerMargins;
    property KeepHistory: Boolean read FKeepHistory write FKeepHistory
      default False;
    property Language: string read FLanguage write SetLanguage;
    property NameFont: TFont read FNameFont write FNameFont;
    property MouseInfo: TMouseInfo read FMouseInfo write FMouseInfo
      default miBoth;
    property MousePrecision: TMousePrecision read GetMousePrecision
      write FMousePrecision default mpHigh;
    property MouseSelect: Boolean read FMouseSelect write FMouseSelect
      default True;
    property MouseTimeFormat: string read GetMouseTimeFormat
      write FMouseTimeFormat;
    property DividerLinePen: TPen read FDividerLinePen write FDividerLinePen;
    property Rulers: TRulers read FRulers write SetRulers default ruNone;
    property SelectionExec: TSelectionExec read FSelectionExec
      write FSelectionExec default seZoom;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property TitleFont : TFont read FTitleFont write FTitleFont;
    property TitleAligment : TAlignment read FTitleAlignment write SetTitleAlignment default taCenter;
    property ValueFont: TFont read FValueFont write FValueFont;

    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property AfterDrawGraph: TDrawGraphEvent read FAfterDrawGraph
      write FAfterDrawGraph;
    property OnDrawSection: TDrawSectionEvent read FOnDrawSection
      write FOnDrawSection;
    property OnDrawLabel: TDrawLabelEvent read FOnDrawLabel write FOnDrawLabel;
    property OnContraction: TContractionEvent read FOnContraction
      write FOnContraction;
    property OnMouseInfo: TMouseInfoEvent read FOnMouseInfo write FOnMouseInfo;
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;
    property OnMeasureLabel: TMeasureLabelEvent read FOnMeasureLabel
      write FOnMeasureLabel;
    property OnRuler: TNotifyEvent read FOnRuler write FOnRuler;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
    property OnQuerySpace : TQuerySpaceEvent read FOnQuerySpace write FOnQuerySpace;
  end;


function qt(AStr: string): string;
function FormatNum(ANumber: single; Precision: integer; ThousandSep : Boolean = false): string;


implementation

uses DateUtils, Dialogs, System.Generics.Defaults, JPEG;

var
  Fmt: TFormatSettings;
  WriterList : TWriterList;
const
  MsgCnt = 109;
  ErrorMsg: array [0 .. MsgCnt - 1] of string = (
    { 0 } 'Value is tied to Series bounds',
    { 1 } 'Not enough space to render this diagram',
    { 2 } 'Not enough space to render a diagram with outlined bars',
    { 3 } 'High value must be higher than Low value',
    { 4 } 'Low value must be lower than High value',
    { 5 } 'Number intervals must be equal',
    { 6 } 'Date and time intervals must be equal',
    { 7 } 'Series are active. You must clear the graph before you can add new series',
    { 8 } 'Using value sections in correlation views ar not allowed. Section ignored.',
    { 9 } 'No data loaded',
    { 10 } 'Value not in span %s',
    { 11 } 'Length of Series must be equal for general purpose data types',
    { 12 } '%s is not present in Series',
    { 13 } '%s must be greater than its predecessor',
    { 14 } 'No series defined',
    { 15 } 'A series must at least contain two values',
    { 16 } 'Minimum 2 series when curve style is neighbor areas',
    { 17 } 'Max 3 series when curve style is NeigborArea',
    { 18 } 'Graph type must be Curve when applying the NeigborArea style',
    { 19 } 'Series must be neighbors.',
    { 20 } '%s index out of bounds',
    { 21 } 'Name cannot be empty',
    { 22 } 'Cannot delete an auto section',
    { 23 } 'Start value greater than end value',
    { 24 } 'Name does not exist %s',
    { 25 } 'First Name must come before last Name',
    { 26 } 'Start date greater than end date',
    { 27 } 'Start number greater than end number',
    { 28 } 'Date out of range %s',
    { 29 } 'Number out of range %s',
    { 30 } 'Graph type must be Curve or Bar',
    { 31 } 'Start value not found',
    { 32 } 'End value not found',
    { 33 } 'Names type can only be set when Series are empty',
    { 34 } 'Name type pieAsync can only be set when graph type is gtPie',
    { 35 } 'Spacing must be between 0 and 50',
    { 36 } 'Cannot Contract general purpose data types',
    { 37 } 'Contraction failed. Insufficient result.',
    { 38 } 'Pie diagrams does not work with spanned name types',
    { 39 } 'No active object',
    { 40 } 'Value sections not allowed in correaltion views. Section ignored',
    { 41 } 'No active graph',
    { 42 } '%s is not a number',
    { 43 } 'Cannot decrease Contraction',
    { 44 } '%s is not a valid CW file',
    { 45 } 'Duplicate series name %s',
    { 46 } 'Note! Only two graphs allowed in a correlation view',
    { 47 } 'View error',
    { 48 } 'View index out of bounds %s',
    { 49 } 'Could not resolve graph type',
    { 50 } 'View is not correctly configured',
    { 51 } 'A correlation view requiers two and only two graphs in the view',
    { 52 } 'A correlation view must have two and only two series.',
    { 53 } 'Number of graphs in a view cannot be less than the number of series',
    { 54 } 'Undefined view member',
    { 55 } 'Nothing to save',
    { 56 } 'Name mismatch',
    { 57 } 'Duplicate names %s',
    { 58 } 'Section type not compatible with name type',
    { 59 } 'Duplicate graphs not allowed',
    { 60 } 'A graph can only be linked to one View',
    { 61 } '%s assignment requires Chart.Writer to be set',
    { 62 } 'Correlation view has no active member',
    { 63 } 'Series spaces too small',
    { 64 } 'Time span cannot be zero',
    { 65 } 'Flow animation can only be applied when layout is Side-By-Side',
    { 66 } 'Cannot apply stacked layout when Animations contains the Flow option',
    { 67 } 'Angle must be in the range of 10..70',
    { 68 } 'Relation values cannot be less than -8',
    { 69 } 'Value must be in the range of 10..90%',
    { 70 } 'Missing section start value',
    { 71 } 'Missing section end value',
    { 72 } 'NameType and DateTimeTemplate are not compliant.',
    { 73 } 'NameType and AutoSection are not compliant.',
    { 74 } '%s is not a valid date / time',
    { 75 } 'Invalid number (%s). Template values must be integers',
    { 76 } 'Section value (%s) out of scope',
    { 77 } 'Insufficient data',
    { 78 } 'Name type and date / time format mismatch',
    { 79 } 'Note that walls can only be shown when name / value axis Orientation is bottom / left',
    { 80 } 'Note that walls cannot be shown when there are no graph borders defined',
    { 81 } 'Pies require the ntGeneral name type',
    { 82 } 'Name type is not compatible with the loaded data',
    { 83 } 'Cannot use compression when layout is side by side',
    { 84 } 'General name types only accepts scolling or no action',
    { 85 } 'Cannot insert pies in an axis chart',
    { 86 } 'Cannot insert axis graphs in a pie chart',
    { 87 } 'Writer not assigned',
    { 88 } 'No active chart',
    { 89 } 'Graph not in chart',
    { 90 } 'Cannot use side by side layout when overflow action is compression',
    { 91 } 'Duplicate Legend',
    { 92 } 'Undefined Graph in Chart',
    { 93 } 'Series titles incomplete in data',
    { 94 } 'Title not found',
    { 95 } 'Conflicting name types',
    { 96 } 'Number of actual and formal series must be alike',
    { 97 } 'Rich chart files cannot be loaded within this context',
    { 98 } 'Could not decide selection. Be sure to pick a real start and end point.',
    { 99 } 'Nothing to zoom',
   { 100 } 'Number of text lines cannot be changed',
   { 101 } 'The file %s does not exist',
   { 102 } 'Missing parameter values',
   { 103 } 'Value fields must either be integers or floats',
   { 104 } 'Name fields must either be date times, integers or strings',
   { 105 } 'Dataset %s is not active',
   { 106 } 'Not a ChartWriter file',
   { 107 } 'Warning: Changing the name type to a spanned type might cause problems rendering the chart.',
   { 108 } 'Cannot share %s among different writers'
    );

  msg_TiedToSeries = 0;
  msg_NoSpace = 1;
  msg_NoOutLinedSpace = 2;
  msg_HighLow = 3;
  msg_LowHigh = 4;
  msg_NumIntervals = 5;
  msg_DateIntervals = 6;
  msg_ActiveSeries = 7;
  msg_Type2Valsections = 8;
  msg_DataEmpty = 9;
  msg_NotInSpan = 10;
  msg_EqualLength = 11;
  msg_NotPresent = 12;
  msg_GreaterPred = 13;
  msg_NoSeries = 14;
  msg_TwoValues = 15;
  msg_NeighborTwoSeries = 16;
  msg_NeighborThreeSeries = 17;
  msg_NeighborCurve = 18;
  msg_NotNeighborSeries = 19;
  msg_IndexBounds = 20;
  msg_NameEmpty = 21;
  msg_DelAutoSect = 22;
  msg_StartEndValue = 23;
  msg_NameNotExists = 24;
  msg_FirstNameLastName = 25;
  msg_StartDateEndDate = 26;
  msg_StartNumEndNum = 27;
  msg_DateOutOfRange = 28;
  msg_NumberOutOfRange = 29;
  msg_GTCurveOrBar = 30;
  msg_StartValNotFound = 31;
  msg_EndValNotFound = 32;
  msg_NameTypeEmptySeries = 33;
  msg_NameTypeAsync = 34;
  msg_ItemSpacing = 35;
  msg_ContractGeneral = 36;
  msg_ContractInsufficient = 37;
  msg_PieSpan = 38;
  msg_NoActiveObject = 39;
  msg_CorrlSection = 40;
  msg_NoActiveGraph = 41;
  msg_InvalidNumber = 42;
  msg_DecreaseContraction = 43;
  msg_FileFormat = 44;
  msg_DupSerName = 45;
  msg_CorrlTwoGraphs = 46;
  msg_ViewError = 47;
  msg_ViewIndexOutOfBounds = 48;
  msg_ResolveGraphType = 49;
  msg_ViewConfig = 50;
  msg_Corrl22Graphs = 51;
  msg_Corrl22Series = 52;
  msg_ViewGraphsLessSeries = 53;
  msg_UndefViewMember = 54;
  msg_NothingToSave = 55;
  msg_NameMismatch = 56;
  msg_DupName = 57;
  msg_SectIncompatible = 58;
  msg_DupGraph = 59;
  msg_GraphOneView = 60;
  msg_NoWriter = 61;
  msg_NoMember = 62;
  msg_SeriesSpacesTooSmall = 63;
  msg_TimeSpanZero = 64;
  msg_FlowSideBySide = 65;
  msg_StackedFlow = 66;
  msg_Angle1070 = 67;
  msg_RelationMin8 = 68;
  msg_DoughnutRange = 69;
  msg_SectStartValue = 70;
  msg_SectEndValue = 71;
  msg_NameTypeTemplate = 72;
  msg_NameTypeAutoSect = 73;
  msg_InvalidDateTime = 74;
  msg_TemplateIntegers = 75;
  msg_SectValueScope = 76;
  msg_InSufficientData = 77;
  msg_NameTypeDtFormatMismatch = 78;
  msg_WallOrientation = 79;
  msg_WallBorder = 80;
  msg_PieGeneral = 81;
  msg_NameTypeData = 82;
  msg_CompressionSideBySide = 83;
  msg_GeneralNameType = 84;
  msg_PieAxisChart = 85;
  msg_AxisPieChart = 86;
  msg_WriterNotAssigned = 87;
  msg_NoActiveChart = 88;
  msg_GraphNotInChart = 89;
  msg_SideBySideCompression = 90;
  msg_DupLegend = 91;
  msg_UndefGraph = 92;
  msg_TitlesIncomplete = 93;
  msg_TitleNotFound = 94;
  msg_NameTypeConflict = 95;
  msg_FormalActual = 96;
  msg_RichDesign = 97;
  msg_UnclearSelection = 98;
  msg_NoZoom = 99;
  msg_HintTextLines = 100;
  msg_NoFile = 101;
  msg_MissingParams = 102;
  msg_ValueDataType = 103;
  msg_NameDataType = 104;
  msg_DSNotActive = 105;
  msg_InvalidFile = 106;
  msg_ChangeSpanned = 107;
  msg_InvalidShare = 108;

type
  trealpoint = record
    X, Y: extended;
  end;

  TRealPointArray = array of trealpoint;

type
  tScaleInfo = record
    ScaleX, ScaleY: extended;
    Offsetx, Offsety: integer;
  end;

{ TOverlaps ---------------------------------------------------}
constructor TOverlaps.Create(OutBounds : TRect; MoveDir : TMoveDir);
begin
    FRects := TList<TRect>.Create;
    FOuter := OutBounds;
    FMoveDir := MoveDir;
end;

destructor TOverlaps.Destroy;
begin
    FRects.Free;
    inherited;
end;

procedure TOverlaps.Add(ARect : TRect);
begin
  FRects.Add(ARect);
end;

function TOverlaps.GetRect(ARect : TRect) : TRect;
begin
   MoveIt(ARect, Result);
end;

function TOverlaps.Overlapped(ARect : TRect; var OverlappedRect : TRect) : Boolean;
var
  i : integer;
begin
   Result := false;
   if not FOuter.Contains(ARect) then
   begin
    Result := True;
    OverlappedRect := FOuter;
    Exit;
   end;
   for I := 0 to FRects.Count-1 do
   begin
     if ARect.IntersectsWith(FRects[i]) then
     begin
        OverlappedRect := FRects[i];
        Result := True;
        Break;
     end;
   end;
end;

function TOverlaps.MoveIt(ARect : TRect; var MovedRect : TRect) : Boolean;
var
  NextMoveDir : TMoveDir;
  MDir :TMoveDir;

  procedure AdjustToOuter(var ARect : TRect);
  var
    H, W : integer;
  begin
      H := ARect.Height;
      W := ARect.Width;
      if ARect.Top < FOuter.Top then
      begin
       ARect.Top := Fouter.Top + 1;
       ARect.Height := H;
      end;
      if ARect.Left < FOUter.Left then
      begin
       ARect.Left := FOuter.Left + 1;
       ARect.Width := W;
      end;
      if ARect.Right > FOuter.Right then
      begin
       ARect.Right := FOUter.Right - 1;
       ARect.Left := ARect.Right-W;
      end;
      if ARect.Bottom > FOuter.Bottom then
      begin
       ARect.Bottom := FOuter.Bottom - 1;
       ARect.Top := ARect.Bottom -H;
      end;
   end;

  function IsOverlapped(ARect : TRect) : Boolean;
  var
    R : TRect;
    Olap : TRect;
  begin
    Result := Overlapped(ARect, Olap);
    MovedRect := ARect;
    if not Result then
      Exit;
    if MDir = mdUp then
    begin
      while Result do
      begin
        ARect.Top := ARect.Top-1;
        ARect.Bottom := ARect.Bottom-1;
        Result := Overlapped(ARect, Olap);
        if Result and (Olap = FOuter) then
        begin
         AdjustToOuter(ARect);
         Result := false;
         Break;
        end;
      end;
    end
    else if MDir = mdLeft then
    begin
      while Result do
      begin
        ARect.Left := ARect.Left-1;
        ARect.Right := ARect.Right-1;
        Result := Overlapped(ARect, Olap);
        if Result and (Olap = FOuter) then
        begin
         AdjustToOuter(ARect);
         Result := false;
         Break;
        end;
      end;
    end
    else if MDir = mdRight then
    begin
      while Overlapped(ARect, R) do
      begin
        ARect.Left := ARect.Left+1;
        ARect.Right := ARect.Right+1;
        Result := Overlapped(ARect, Olap);
        if Result and (Olap = FOuter) then
        begin
         AdjustToOuter(ARect);
         Result := false;
         Break;
        end;
      end;
    end
    else if MDir = mdDown then
    begin
      while Result do
      begin
        ARect.Top := ARect.Top+1;
        ARect.Bottom := ARect.Bottom+1;
        Result := Overlapped(ARect, Olap);
        if Result and (Olap = FOuter) then
        begin
         AdjustToOuter(ARect);
         Result := false;
         Break;
        end;
      end;
    end;
    MovedRect := ARect;
  end;

begin
  if not FOuter.Contains(ARect) then
    AdjustToOuter(ARect);
  NextMoveDir := mdUp;
  case FMoveDir of
    mdUp: NextMoveDir := mdDown;
    mdLeft: NextMoveDir := mdRight;
    mdRight: NextMoveDir := mdLeft;
    mdDown: NextMoveDir := mdUp;
  end;
  MDir := FMoveDir;

  Result := not IsOverlapped(ARect);
  if not Result then
  begin
    MDir := NextMoveDir;
    Result := not IsOverlapped(ARect);
  end;
  Add(MovedRect);

end;

{ General ------------------------------------------------------}


procedure ShowGWError(Errorcode: integer; AddText: string = '');
var
  S: string;
  E: TCWException;
begin
  if Errorcode = -1 then
    S := AddText
  else
  begin
    S := ErrorMsg[Errorcode];
    if AddText <> '' then
      S := Format(S, [AddText]);
  end;
  E := TCWException.Create(S);
  E.Errorcode := Errorcode;
  raise E;
end;

procedure ShowGWMessage(MsgCode : integer; AddText : string = '');
var
  S: string;
begin
  if MsgCode = -1 then
    S := AddText
  else
  begin
    S := ErrorMsg[MsgCode];
    if AddText <> '' then
      S := Format(S, [AddText]);
  end;
  ShowMessage(S);
end;

function IndexOfWList(AWriter : TChartWriter; AnObject : TObject) : integer;
var
  i : integer;
begin
   Result := -1;
   for I := 0 to WriterList.Count-1 do
   begin
      if (WriterList[i].Writer = AWriter)
      and (WriterList[i].Obj = AnObject)  then
      begin
        Result := i;
        Break;
      end;
   end;
end;

function AddToWList(AWriter : TChartWriter; AnObject : TObject) : TWriterListElement;
var
  Sec : TCWSectionDefs;
  Leg : TCWLegend;
  Ch : TCWChart;
  G : TCWGraph;
  ThisID : TWriterListElement;
  Indx : integer;
  i : integer;
  S: string;
begin
  Result := nil;
  if AWriter = nil then
  begin
     Exit;
  end;

  Sec := nil;
  Leg := nil;
  Ch := nil;
  G := nil;

  if AnObject is TCWSectionDefs then
  begin
    Sec := TCWSectionDefs(AnObject);
    ThisID := TCWSectionDefs(AnObject).FWID;
    S := 'Sections (' + Sec.Name + ') ' ;
  end
  else if AnObject is TCWLegend then
  begin
    Leg := TCWLegend(AnObject);
    ThisID := TCWLegend(AnObject).FWID;
    S := 'Legends (' + Leg.Name + ') ';
  end
  else if AnObject is TCWChart then
  begin
    ThisID := TCWChart(AnObject).FWID;
    Ch := TCWChart(AnObject);
    S := 'Charts (' + Ch.Name + ') '
  end
  else if AnObject is TCWGraph then
  begin
    G := TCWGraph(AnObject);
    ThisID := TCWGraph(AnObject).FWID;
    S := 'Graphs (' + G.Name  + ') ';
  end
  else
    ThisID := nil;

  {Objects from other writers using that object?}
  for I := 0 to WriterList.Count-1 do
    begin
      if (WriterList[i].Obj = AnObject)
      and(WriterList[i].Writer <> AWriter)
      then
      begin
        SHowGWError(msg_InvalidShare, S);
      end;
    end;

  Result := ThisID;

  Indx := IndexOfWList(AWriter, AnObject);
  if Indx <> -1 then
  begin
    Exit;
  end;

  if ThisID <> nil then
  begin
    if WriterList.IndexOf(ThisID) <> -1  then
      Exit;
  end;

  Result := TWriterListElement.Create;
  Result.Writer := AWriter;
  Result.Obj := AnObject;
  WriterList.Add(Result);
end;

procedure DeleteFromWList(AWriter : TChartWriter);
var
  i : integer;
  found : boolean;
begin
  repeat
   found := false;
   for I := 0 to WriterList.Count-1 do
     if WriterList[i].Writer = AWriter then
     begin
       WriterList.Delete(I);
       Found := True;
       Break;
     end;
  until not found;
end;

function GetWriterFromWList(AnElement : TWriterListElement) : TChartWriter;
var
 i : integer;
begin
  Result := nil;
  i := WriterList.IndexOf(AnElement);
  if i <> -1 then
    Result := WriterList[i].Writer;
end;




function qt(AStr: string): string;
begin
  Result := '''' + AStr + '''';
end;

function InvertColor(Color: TColor): TColor;
begin
//  Result := RGB(100, 100, 100)
  Color := ColorToRGB(Color);
  if (GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) > 384 then
    Result := RGB(128, 128, 128)
  else
    Result := RGB(200, 200, 200);
end;

procedure DrawAlphaBlend(DestBM: HDC; Rect: TRect; Blend: byte;
  SourceColor: TColor = clBlack);
{ Only used to create the blended selctor style }
var
  Ahdc: HDC; { handle of the DC we will create }
  bf: BLENDFUNCTION; { structure for alpha blending }
  Ahbitmap: HBitmap; { bitmap handle }
  bmi: BITMAPINFO; { bitmap header }
  pvBits: pointer; { pointer to DIB section }
  BMWidth, BMHeight: ULONG; { window width/height }
const
  AC_SRC_ALPHA = $1;

begin
  BMWidth := Rect.Right - Rect.Left;
  BMHeight := Rect.Bottom - Rect.Top;

  if ((BMWidth = 0) and (BMHeight = 0)) then
    Exit;

  { create a DC for our bitmap -- the source DC for AlphaBlend }
  Ahdc := CreateCompatibleDC(DestBM);

  try
    { zero the memory for the bitmap info }
    ZeroMemory(@bmi, sizeof(BITMAPINFO));

    { setup bitmap info }
    bmi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth := BMWidth;
    bmi.bmiHeader.biHeight := BMHeight;
    bmi.bmiHeader.biPlanes := 1;
    bmi.bmiHeader.biBitCount := 32; { four 8-bit components }
    bmi.bmiHeader.biCompression := BI_RGB;
    bmi.bmiHeader.biSizeImage := BMWidth * BMHeight * 4;

    { create our DIB section and select the bitmap into the dc }
    Ahbitmap := CreateDIBSection(Ahdc, bmi, DIB_RGB_COLORS, pvBits, 0, 0);
    try
      SelectObject(Ahdc, Ahbitmap);
      bf.BlendOp := AC_SRC_OVER;
      bf.BlendFlags := 0;
      bf.SourceConstantAlpha := Blend;
      bf.AlphaFormat := 0; { ignore source alpha channel }

      AlphaBlend(DestBM, Rect.Left, Rect.Top, BMWidth, BMHeight, Ahdc, 0, 0,
        BMWidth, BMHeight, bf);

    finally
      DeleteObject(Ahbitmap);
    end;
  finally
    DeleteDC(Ahdc);
  end;
end;

{ ************* LinearLeastSquares ******************* }
function LinearLeastSquares(Data: TRealPointArray;
  var M, B, R: extended): Boolean;
{ Line "Y = mX + b" is linear least squares line for the input array, "data",
  of TRealPoint }
var
  SumX, SumY, SumX2, SumY2, SumXY: extended;
  Sx, Sy: extended;
  n, i: integer;
begin
  Result := True;
  n := Length(Data); { number of points }
  SumX := 0.0;
  SumY := 0.0;
  SumX2 := 0.0;
  SumY2 := 0.0;
  SumXY := 0.0;

  for i := 0 to n - 1 do
    with Data[i] do
    begin
      SumX := SumX + X;
      SumY := SumY + Y;
      SumX2 := SumX2 + X * X;
      SumY2 := SumY2 + Y * Y;
      SumXY := SumXY + X * Y;
    end;

  if (n * SumX2 = SumX * SumX) or (n * SumY2 = SumY * SumY) then
  begin
    // showmessage('LeastSquares() Error - X or Y  values cannot all be the same');
    Result := False;
    M := 0;
    B := 0;
  end
  else
  begin
    M := ((n * SumXY) - (SumX * SumY)) / ((n * SumX2) - (SumX * SumX));
    { Slope M }
    B := (SumY - M * SumX) / n; { Intercept B }
    Sx := sqrt(SumX2 - sqr(SumX) / n);
    Sy := sqrt(SumY2 - sqr(SumY) / n);
    R := (SumXY - SumX * SumY / n) / (Sx * Sy);
    // RSquared:=r*r;
  end;
end;

{ ************* Scalepoint ************* }
function scalePoint(const X, Y: extended; const scaleinfo: tScaleInfo): TPoint;
begin
  with scaleinfo do
  begin
    Result.X := trunc(ScaleX * X + Offsetx);
    Result.Y := trunc(ScaleY * Y + Offsety);
  end;
end;

function ScaledataForPlot(const Data: TRealPointArray;
  imagewidth, imageHeight: integer; var ScaledData: TIntPointArray): tScaleInfo;
{ Function which converts real x,y coordinates to plot coordinates covering the
  range from 10% to 90% of the plot area define by parameters ImageWidth and
  ImageHeight.  I returns the scaling info in a TScaleInfo record which may be passed
  to the scalePoint function to scale additional points if necessary. }
var

  i: integer;
  maxx, minx, maxy, miny: extended;

begin
  setlength(ScaledData, Length(Data));
  with Data[low(Data)] do
  begin
    maxx := X;
    minx := X;
    maxy := Y;
    miny := Y;
  end;
  for i := low(Data) + 1 to high(Data) do
    with Data[i] do
    begin
      if X > maxx then
        maxx := X;
      if X < minx then
        minx := X;
      if Y > maxy then
        maxy := Y;
      if Y < miny then
        miny := Y;
    end;
  with Result do
  begin
    ScaleX := 0.8 * (imagewidth) / (maxx - minx);
    ScaleY := -0.9 * (imageHeight) / (maxy - miny);
    Offsetx := imagewidth div 10 - trunc(ScaleX * minx);
    Offsety := imageHeight - trunc(ScaleY * miny);
  end;
  for i := low(Data) to high(Data) do
    with Data[i] do
      ScaledData[i] := scalePoint(X, Y, Result);
end;


function FormatNum(ANumber: single; Precision: integer; ThousandSep : Boolean = false): string;
var
  S: string;
begin
  S := '0';
  if ThousandSep then
   S := S + ',';
  if Precision = 1 then
    S := S + '.#'
  else if Precision = 2 then
    S := S + '.##'
  else if Precision = 3 then
    S := S + '.###';
  Result := FormatFloat(S, ANumber, Fmt);
end;

procedure SetKeyVal(sl: TStringList; Key, Val: string);
begin
  sl.Add(Key + '=' + Val);
end;

function ExpandDM(AMy: word): string;
begin
  Result := IntToStr(AMy);
  if AMy < 10 then
    Result := '0' + Result;
end;

function MakeGPClr(AColor: TColor; Alpha: integer = 255): TGPColor;
var
  R, G, B: byte;
  Clr: integer;
begin
  Clr := ColorToRgb(AColor);
  R := GetRValue(Clr);
  G := GetGValue(Clr);
  B := GetBValue(Clr);
  if Alpha < 255 then
    Result := MakeColor(Alpha, R, G, B)
  else
    Result := MakeColor(R, G, B);
end;

procedure CreateListFromDelimiter(const ADelimiter: Char; AText: string;
  AList: TStrings);
var
  P: integer;
  S: string;
begin
  AList.Clear;
  P := Pos(ADelimiter, AText);
  while P <> 0 do
  begin
    S := Copy(AText, 1, P - 1);
    Delete(AText, 1, P);
    AList.Add(S);
    P := Pos(ADelimiter, AText);
  end;
  if AText <> '' then
    AList.Add(AText);
end;

function DecodePen(APen: TPen): string;
begin
  Result := '';
  Result := IntToStr(APen.Color) + ',';
  Result := Result + IntToStr(Ord(APen.Style)) + ',';
  Result := Result + IntToStr(APen.Width) + ',';
  Result := Result + IntToStr(Ord(APen.Mode));
end;

procedure EncodePen(APen: TPen; Values: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  CreateListFromDelimiter(',', Values, sl);
  try
    APen.Color := TColor(StrToInt(sl[0]));
    APen.Style := TPenStyle(StrToInt(sl[1]));
    APen.Width := StrToInt(sl[2]);
    APen.Mode := TPenMode(StrToInt(sl[3]));
  finally
    sl.Free;
  end;
end;

function DecodeFont(AFont: TFont): string;
begin
  Result := '';
  Result := IntToStr(AFont.Color) + ',';
  if fsBold in AFont.Style then
    Result := Result + '1|'
  else
    Result := Result + '0|';
  if fsItalic in AFont.Style then
    Result := Result + '1|'
  else
    Result := Result + '0|';
  if fsUnderLine in AFont.Style then
    Result := Result + '1|'
  else
    Result := Result + '0|';
  if fsStrikeOut in AFont.Style then
    Result := Result + '1|'
  else
    Result := Result + '0|';
  Result := Result + ',';
  Result := Result + IntToStr(AFont.Size) + ',';
  Result := Result + AFont.Name + ',';
  Result := Result + IntToStr(AFont.Charset);
end;

procedure EncodeFont(AFont: TFont; Values: string);
var
  sl, sl2: TStringList;
begin
  sl := TStringList.Create;
  sl2 := TStringList.Create;
  CreateListFromDelimiter(',', Values, sl);
  try
    AFont.Color := TColor(StrToInt(sl[0]));
    CreateListFromDelimiter('|', sl[1], sl2);
    AFont.Style := [];
    if sl2[0] = '1' then
      AFont.Style := AFont.Style + [fsBold];
    if sl2[1] = '1' then
      AFont.Style := AFont.Style + [fsItalic];
    if sl2[2] = '1' then
      AFont.Style := AFont.Style + [fsUnderLine];
    if sl2[3] = '1' then
      AFont.Style := AFont.Style + [fsStrikeOut];
    AFont.Size := StrToInt(sl[2]);
    AFont.Name := sl[3];
    AFont.Charset := StrToInt(sl[4]);
  finally
    sl.Free;
    sl2.Free;
  end;
end;

function DecodeBrush(ABrush: TBrush): string;
begin
  Result := '';
  Result := IntToStr(ABrush.Color) + ',';
  Result := Result + IntToStr(Ord(ABrush.Style));
end;

procedure EncodeBrush(ABrush: TBrush; Values: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  CreateListFromDelimiter(',', Values, sl);
  try
    ABrush.Color := TColor(StrToInt(sl[0]));
    ABrush.Style := TBrushStyle(StrToInt(sl[1]));
  finally
    sl.Free;
  end;
end;

function DecodeStrings(Strings: TStrings): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Strings.Count - 1 do
  begin
    Result := Result + Strings[i];
    if i < Strings.Count - 1 then
      Result := Result + '~';
  end;
end;

procedure EncodeStrings(Strings: TStrings; Values: string);
begin
  CreateListFromDelimiter('~', Values, Strings);
end;

function MonthsBetweenEx(const ANow, AThen: TDateTime): integer;
var
  Y, M, D, y2, M2, D2: word;
  RestY, RestY2: integer;
  NumYear: integer;
begin
  DecodeDate(AThen, Y, M, D);
  DecodeDate(ANow, y2, M2, D2);
  if (D = 1) and (D2 = 1) then
  begin
    if Y = y2 then
      Result := M2 - M
    else
    begin
      NumYear := y2 - Y - 1;
      RestY := 12 - M;
      RestY2 := M2;
      Result := RestY + RestY2 + NumYear * 12;
    end;
  end
  else
    Result := MonthsBetween(ANow, AThen);
end;

function DoZero(n: integer): string;
begin
  if n < 10 then
    Result := '0' + IntToStr(n)
  else
    Result := IntToStr(n);
end;

function GetTextDiag(ACanvas: TCanvas; AText: string): integer;
var
  W, H: integer;
begin
  W := ACanvas.TextWidth(AText);
  H := ACanvas.TextHeight(AText);
  Result := round(sqrt(W * W + H * H));
end;

function GetTimeStr(CW: TChartWriter; AFormat: string;
  ATime: TDateTime): string;

  function CompDifferentYears: Boolean;
  var
    i: integer;
    y1: word;
    Ser1: integer;
  begin
    Result := False;
    if CW.FYears.Count = 0 then
      Exit;
    y1 := CW.FYears[0].Year;
    Ser1 := CW.FYears[0].SerIndex;
    for i := 1 to CW.FYears.Count - 1 do
    begin
      if Ser1 <> CW.FYears[i].SerIndex then
      begin
        if y1 <> CW.FYears[i].Year then
        begin
          Result := True;
          Break;
        end;
        Ser1 := CW.FYears[i].SerIndex;
      end;
    end;
  end;

begin
  if AFormat = '' then
    case CW.Chart.NameType of
      ntDateSpan:
        begin
          if CW.Count = 1 then
            AFormat := 'ddddd'
          else
          begin
            if CompDifferentYears then
            begin
              AFormat := 'dd/mm'
            end
            else
              AFormat := 'ddddd';
          end;
        end;
      ntHourSpan:
        AFormat := 'hh';
      ntMinuteSpan:
        AFormat := 'nn';
      ntSecondSpan:
        AFormat := 'ss';
      ntNumberSpan:
        AFormat := '';
    end;
  if AFormat <> '' then
    Result := FormatDateTime(AFormat, ATime, Fmt);
end;

function DetectAngle(LabelKind: TLabelKind; Writer: TChartWriter): integer;
begin
  Result := 0;
  begin
    if LabelKind = lkName then
    begin
      if Writer.FNamAx.IsXAxis then
        Result := Writer.FNameFont.Orientation
    end
    else if LabelKind = lkValue then
    begin
      if Writer.FValAx.IsXAxis then
        Result := Writer.FValueFont.Orientation
    end
    else if (LabelKind = lkValueSection) then
    begin
      if Writer.FValAx.IsXAxis then
      begin
        if Writer.ValueSectionDefs <> nil then
          Result := Writer.ValueSectionDefs.Font.Orientation;
      end;
    end
    else if (LabelKind = lkNameSection) then
    begin
      if Writer.FNamAx.IsXAxis then
      begin
        if Writer.NameSectionDefs <> nil then
          Result := Writer.NameSectionDefs.Font.Orientation;
      end;
    end;
  end;
end;

function IsCtrlDown: Boolean;
begin
  Result := (GetKeyState(VK_CONTROL) < 0);
end;

function IsShiftDown: Boolean;
begin
  Result := (GetKeyState(VK_Shift) < 0);
end;

function IsAltDown: Boolean;
begin
  Result := (GetKeyState(VK_Menu) < 0);
end;

function GetLabelW(ALabel: string; IsWidth: Boolean; ACanvas: TCanvas): integer;
{ Used with ComputeTextExtent and GetTextWidth / GetTextHeight }
var
  sl: TStringList;
  P, P2: integer;
  S: string;
  n: integer;
  i: integer;
begin
  Result := 0;
  P := Pos('[', ALabel);
  if P = 1 then
  begin
    n := 0;
    P2 := Pos(']', ALabel);
    if P2 <> 0 then
    begin
      S := ALabel;
      Delete(S, 1, 1);
      Delete(S, Length(S), 1);
      if TryStrToInt(S, n) then
      begin
        Result := n;
      end
      else
      begin
        if IsWidth then
          Result := ACanvas.TextWidth(ALabel)
        else
          Result := ACanvas.TextHeight(ALabel)
      end;

    end;
  end
  else
  begin
    sl := TStringList.Create;
    sl.Text := ALabel;
    S := '';
    for i := 0 to sl.Count - 1 do
    begin
      if IsWidth then
      begin
        if ACanvas.TextWidth(sl[i]) > Result then
        begin
          Result := ACanvas.TextWidth(sl[i]);
        end;
      end
      else
      begin
        Result := Result + ACanvas.TextHeight(sl[i]);
      end;
    end;
    sl.Free;
  end;
end;

function CheckDateHit(Dt: TDate; Y, M, D: word): Boolean;
var
  YY, MM, DD: word;
  HitY, HitM, HitD: Boolean;
begin
  Result := False;
  DecodeDate(Dt, YY, MM, DD);
  HitY := (Y = 0);
  HitM := (M = 0);
  HitD := (D = 0);
  if HitY and HitM and HitD then
    Exit;
  if not HitY then
    HitY := (YY = Y);
  if not HitM then
    HitM := (M = MM);
  if not HitD then
    HitD := (D = DD);
  Result := HitY and HitM and HitD;
end;

function CheckTimeHit(Dt: TDateTime; H, M, S: integer): Boolean;
var
  HH, MM, SS, Msec: word;
  HitH, HitM, HitS: Boolean;
begin
  Result := False;
  DecodeTime(Dt, HH, MM, SS, Msec);
  HitH := (H < 0);
  HitM := (M < 0);
  HitS := (S < 0);
  if HitH and HitM and HitS then
    Exit;
  if not HitH then
    HitH := (HH = M);
  if not HitM then
    HitM := (M = MM);
  if not HitS then
    HitS := (S = SS);
  Result := HitH and HitM and HitS;
end;

procedure CalcPst(AOwner: TSeries; Itms, ItmsOut: TSeriesItems);
var
  Sm, CheckSm: single;
  i: integer;
  Itm: TSeriesItem;
  V: single;

begin
  Sm := 0;
  for i := 0 to Itms.Count - 1 do
    Sm := Sm + Itms[i].FValue;
  CheckSm := 0;
  for i := 0 to Itms.Count - 1 do
  begin
    if Sm = 0 then
      V := 0
    else
      V := Itms[i].FValue * 100 / Sm;
    Itm := TSeriesItem.Create;
    Itm.FValue := V;
    Itm.FRealVal := V;
    CheckSm := CheckSm + V;
    Itm.Name := Itms[i].Name;
    Itm.FOwner := AOwner;
    Itm.FItems := ItmsOut;
    if CheckSm > 100 then
    begin
      Itm.FValue := Itm.Value - (CheckSm - 100);
      ItmsOut.Add(Itm);
      Break;
    end
    else
    begin
      ItmsOut.Add(Itm);
    end;
  end;
end;

function ChangeColor(InputColor: TColor; Percentage: extended): TColor;
{ From Swiss Delphi center }
var
  H, L, S: word;
  Lint: integer;
begin
  ColorRGBToHLS(ColorToRgb(InputColor), H, L, S);
  Lint := L;
  Lint := Lint - round(L * Percentage / 100);
  if Lint < 0 then
    L := 0
  else if Lint > 240 then
    L := 240
  else
    L := L - round(L * Percentage / 100);
  Result := ColorHLSToRGB(H, L, S);
end;

{ TCWFileReader ------------------------------------------------- }

function TCWFileReader.GetSeriesCount: integer;
begin
  Result := FDataContent.Count;
end;

function TCWFileReader.GetItemCount(SeriesIndex: integer): integer;
begin
  Result := FDataContent[SeriesIndex].Count;
end;

function TCWFileReader.GetPropValue(APropName: string): string;
begin
  Result := FProps.Values[APropName];
end;

function GetValueElement(ValString: string; Index: integer): String;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    CreateListFromDelimiter('|', ValString, sl);
    Result := sl[Index];
  finally
    sl.Free;
  end;
end;

function TCWFileReader.GetItemValue(SeriesIndex, ItemIndex: integer): single;
var
  S: string;
begin
  S := FDataContent[SeriesIndex].ValueFromIndex[ItemIndex];
  Result := 0;
  TryStrToFloat(S, Result, Fmt)
end;

function TCWFileReader.GetItemName(SeriesIndex, ItemIndex: integer): string;
begin
  Result := FDataContent[SeriesIndex].KeyNames[ItemIndex];
end;

function TCWFileReader.GetItemColor(SeriesIndex, ItemIndex: integer): TColor;
var
  S: string;
begin
  S := FDataContent[SeriesIndex].ValueFromIndex[ItemIndex];
  S := GetValueElement(S, 1);
  if S = '' then
    Result := clBlack
  else
    Result := StrToInt(S);
end;

function TCWFileReader.GetRealDate(SeriesIndex, ItemIndex: integer): TDateTime;
{ Not used }
var
  S: string;
begin
  S := FDataContent[SeriesIndex].ValueFromIndex[ItemIndex];
  S := GetValueElement(S, 2);
  Result := 0;
  TryStrToDateTime(S, Result, Fmt)
end;

function TCWFileReader.GetRealValue(SeriesIndex, ItemIndex: integer): single;
{ Not used }
var
  S: string;
begin
  S := FDataContent[SeriesIndex].ValueFromIndex[ItemIndex];
  S := GetValueElement(S, 3);
  Result := 0;
  TryStrToFloat(S, Result, Fmt)
end;

function TCWFileReader.GetSeriesTitle(SeriesIndex: integer): string;
begin
  Result := FDataContent[SeriesIndex].ValueFromIndex[0];
  if Result = '' then
    Result := 'No title';
end;

function TCWFileReader.GetID(SeriesIndex: integer): string;
begin
  Result := FDataContent[SeriesIndex].ValueFromIndex[1];
end;

constructor TCWFileReader.Create(AFileName: TFileName;
  AChartWriter: TChartWriter);
var
  Indx: integer;
  WorkSl: TStringList;
  i: integer;
  sl: TStringList;
begin
  FDataContent := TFiles.Create;
  FGraphs := TFiles.Create;
  FProps := TStringList.Create;
  FProps.DefaultEncoding := TEncoding.Utf8;
  FWriter := AChartWriter;
  WorkSl := TStringList.Create;
  WorkSl.DefaultEncoding := TEncoding.Utf8;
  WorkSl.LoadFromFile(AFileName);
  WorkSl.Delete(0); {Signature}
  Indx := WorkSl.IndexOf(Props);
  try
    if Indx = -1 then
      ShowGWError(msg_FileFormat, AFileName);
    Indx := WorkSl.IndexOf(Dta);
    if Indx <> -1 then
    begin
      for i := 1 to Indx - 1 do
      begin
        FProps.Add(WorkSl[i]);
      end;
      Indx := Indx + 1;

      { Add the series }
      sl := TStringList.Create;
      sl.DefaultEncoding := TEncoding.Utf8;
      repeat
        for i := Indx to WorkSl.Count - 1 do
        begin
          if WorkSl[i] = EOF then
          begin
            if sl.Count > 0 then
              FDataContent.Add(sl);
            Indx := i + 1;
            Break;
          end;
          sl.Add(WorkSl[i]);
        end;
        if (Indx < WorkSl.Count - 1) and (WorkSl[Indx] <> Objects) then
        begin
          sl := TStringList.Create;
          sl.DefaultEncoding := TEncoding.Utf8;
        end;
      until (Indx = WorkSl.Count) or (WorkSl[Indx] = Objects);
    end;

    { Add the objects }
    Indx := WorkSl.IndexOf(Objects);
    if Indx <> -1 then
    begin
      Indx := Indx + 1;
      sl := TStringList.Create;
      sl.DefaultEncoding := TEncoding.Utf8;
      repeat
        for i := Indx to WorkSl.Count - 1 do
        begin
          if WorkSl[i] = EOF then
          begin
            if sl.Count > 0 then
              FGraphs.Add(sl);
            Indx := i + 1;
            Break;
          end;
          sl.Add(WorkSl[i]);
        end;
        if Indx < WorkSl.Count - 1 then
        begin
          sl := TStringList.Create;
          sl.DefaultEncoding := TEncoding.Utf8;
        end;
      until (Indx = WorkSl.Count);
    end;
  finally
    WorkSl.Free;
  end;
end;

function TCWFileReader.GetSeriesStrings(SeriesIndex: integer): TStringList;
var
  i: integer;
  Obj: TObject;
  S: string;
begin
  Result := TStringList.Create;
  if Writer.Chart = nil then
    Exit;
  for i := 0 to ItemCount[SeriesIndex] - 1 do
  begin
    Obj := TObject(ItemColor[SeriesIndex, i]);
    S := ItemName[SeriesIndex, i] + '=' + FormatNum(ItemValue[SeriesIndex, i],
      Writer.GetValPrecision); {Todo:Check}
    Result.AddObject(S, Obj);
  end;
end;

destructor TCWFileReader.Destroy;
begin
  inherited;
  FDataContent.Free;
  FProps.Free;
  FGraphs.Free;
end;

{ TView ---------------------------------------------------------- }

constructor THistoryItem.Create;
begin
  Names := TStringList.Create;
  NameSections := TSections.Create;
  ValueSections := TSections.Create;
  OrigData := TData.Create;
  ContractionBase := TData.Create;
  Series := TData.Create;
end;

destructor THistoryItem.Destroy;
begin
  Series.Free;
  OrigData.Free;
  ContractionBase.Free;
  NameSections.Free;
  ValueSections.Free;
  Names.Free;
end;

procedure THistoryItem.Assign;
var
  i, j: integer;
  SerDest: TSeries;
  Itm: TSeriesItem;

begin
  Chart := Writer.Chart;
  XUnit := Writer.FNameUnit;
  AxisOrientation := Writer.AxisOrientation;
  States := Writer.FStates;
  ZoomStart := Writer.FZoomStart;
  ZoomEnd := Writer.FZoomEnd;
  LowDate := Writer.FLowDate;
  HighDate := Writer.FHighDate;
  TimeFormat := Writer.FTimeFormat;
  Contraction := Writer.FContraction;
  GraphWidth := Writer.GraphWidth;
  GraphHeight := Writer.GraphHeight;
  OrigSize := Writer.FOrigGraphSize;
  NameLabelFreq := Writer.NameLabelFreq;
  ValueLabelFreq := Writer.ValueLabelFreq;
  NameSectionFreq := Writer.FNameSectionFreq;
  ValueSectionFreq := Writer.FValueSectionFreq;
  UseNamShortnames := Writer.FUseNamShortNames;
  UseValShortnames := Writer.FUseValShortNames;
  ScrollIndex := Writer.FScrollIndex;
  Names.Assign(Writer.FNameList);
  AssignSections(NameSections, Writer.FNameSections);
  AssignSections(ValueSections, Writer.FValueSections);
  for i := 0 to Writer.Count - 1 do
  begin
    SerDest := TSeries.Create;
    SerDest.FWriter := FWriter;
    for j := 0 to Writer.FSeriesData[i].FSeriesItems.Count - 1 do
    begin
      Itm := SerDest.AddItem;
      Itm.Assign(Writer.FSeriesData[i].FSeriesItems[j]);
    end;
    SerDest.FIndent := Writer.FSeriesData[i].FIndent;
    SerDest.FExdent := Writer.FSeriesData[i].FExdent;
    SerDest.FMax := Writer.FSeriesData[i].FMax;
    SerDest.FMin := Writer.FSeriesData[i].FMin;
    Series.Add(SerDest);
  end;
  for i := 0 to Writer.FOrigData.Count - 1 do
  begin
    SerDest := TSeries.Create;
    SerDest.Assign(Writer.FOrigData[i], -1, -1);
    OrigData.Add(SerDest);
  end;
  for i := 0 to Writer.FContractionBase.Count - 1 do
  begin
    SerDest := TSeries.Create;
    SerDest.Assign(Writer.FContractionBase[i], -1, -1);
    ContractionBase.Add(SerDest);
  end;
end;

procedure THistoryItem.AssignSections(ADest, ASource: TSections);
var
  i: integer;
  Sect: TSection;
begin
  ADest.Clear;
  for i := 0 to ASource.Count - 1 do
  begin
    Sect := TSection.Create;
    Sect.FWriter := FWriter;
    Sect.FIndex := ASource[i].FIndex;
    Sect.FOwner := ASource[i].FOwner;
    Sect.FLongCaption := ASource[i].FLongCaption;
    Sect.FShortCaption := ASource[i].FShortCaption;
    Sect.FSectionType := ASource[i].FSectionType;
    Sect.FStartVal := ASource[i].FStartVal;
    Sect.FEndVal := ASource[i].FEndVal;
    ADest.Add(Sect);
  end;
end;

procedure THistoryItem.Restore;
var
  i, j: integer;
  SerDest: TSeries;
  Itm: TSeriesItem;
begin
  Writer.FKeepHistory := False;
  Writer.InternalClear;
  Writer.FKeepHistory := True;
  Writer.FChart := Chart;
  Writer.FRulerVisible := False;
  Writer.FNameList.Assign(Names);
  AssignSections(Writer.FNameSections, NameSections);
  AssignSections(Writer.FValueSections, ValueSections);
  Writer.SetPosition(AxisOrientation);
  Writer.FStates := States;
  Writer.FZoomStart := ZoomStart;
  Writer.FZoomEnd := ZoomEnd;
  Writer.FTimeFormat := TimeFormat;
  Writer.FNameLabelFreq := NameLabelFreq;
  Writer.FValueLabelFreq := ValueLabelFreq;
  Writer.FNameSectionFreq := NameSectionFreq;
  Writer.FValueSectionFreq := ValueSectionFreq;
  Writer.FUseNamShortNames := UseNamShortnames;
  Writer.FUseValShortNames := UseValShortnames;
  Writer.FHighDate := HighDate;
  Writer.FLowDate := LowDate;
  Writer.FContraction := 1;
  Writer.FScrollIndex := ScrollIndex;
  for i := 0 to Series.Count - 1 do
  begin
    SerDest := TSeries.Create;
    SerDest.FWriter := FWriter;
    for j := 0 to Series[i].FSeriesItems.Count - 1 do
    begin
      Itm := SerDest.AddItem;
      Itm.Assign(Series[i].FSeriesItems[j]);
      Itm.FOwner := SerDest;
      Itm.FItems := SerDest.FSeriesItems;
    end;

    SerDest.FIndent := Series[i].FIndent;
    SerDest.FExdent := Series[i].FExdent;
    SerDest.FMax := Series[i].FMax;
    SerDest.FMin := Series[i].FMin;
    Writer.FSeriesData.Add(SerDest);
  end;
  Writer.FNameUnit := XUnit;
  Writer.FContraction := Contraction;
  Writer.FOrigGraphSize := OrigSize;
  Writer.FOrigData.Clear;
  for i := 0 to OrigData.Count - 1 do
  begin
    SerDest := TSeries.Create;
    SerDest.Assign(OrigData[i], -1, -1);
    Writer.FOrigData.Add(SerDest);
  end;
  Writer.FContractionBase.Clear;
  for i := 0 to ContractionBase.Count - 1 do
  begin
    SerDest := TSeries.Create;
    SerDest.Assign(ContractionBase[i], -1, -1);
    Writer.FContractionBase.Add(SerDest);
  end;
  Writer.FLongestSeries := Writer.LongestSeries;
  Writer.SetState(stRestoring);
end;

{ TSeriesItem --------------------------------------------------- }

function TSeriesItem.GetPointPos: TPoint;
begin
  Result := Writer.FSeriesData[SeriesIndex].ItemPoints
    [Index - Writer.FSeriesData[SeriesIndex].FirstItem];
end;

function TSeriesItem.GetName: string;
begin
  Result := FName;
end;

function TSeriesItem.GetIndex: integer;
begin
  Result := FItems.IndexOf(Self);
end;

function TSeriesItem.GetDisabled: Boolean;
begin
  Result := FLeapDummy;
end;

function TSeriesItem.GetItemIndex: integer;
begin
  Result := GetIndex - FOwner.FIndent;
end;

function TSeriesItem.GetBarRect: TRect;
var
  i: integer;
begin
  for i := 0 to Writer.FTrackBars.Count - 1 do
  begin
    if (Writer.FTrackBars[i].SeriesItmIndex = Index) and
      (Writer.FTrackBars[i].SeriesIndex = FOwner.Index) then
    begin
      Result := Writer.FTrackBars[i].VisibleRect;
      Break;
    end;
  end;
end;

function TSeriesItem.GetPieSlice: TPieSlice;
var
  i: integer;
begin
  for i := 0 to FOwner.FTrackPies.Count - 1 do
  begin
    if (FOwner.FTrackPies[i].FSeriesItmIndex = Index) and
      (FOwner.FTrackPies[i].FSeriesIndex = FOwner.Index) then
    begin
      Result := FOwner.FTrackPies[i];
      Break;
    end;
  end;
end;

function TSeriesItem.GetSeriesIndex: integer;
begin
  Result := FOwner.Index;
end;

function TSeriesItem.GetPst : single;
begin
  Result := FValue / FOwner.FSum * 100;
end;

function TSeriesItem.GetVal : single;
begin
  Result := FValue;
end;

procedure TSeriesItem.SetColor(Value : TColor);
begin
  if FColor = Color then
    Exit;
  FColor := Value;
  if Index > Writer.Chart.ItemColors.Count-1 then
    Writer.DoRepaint;
end;

procedure TSeriesItem.Assign(Source: TSeriesItem);
begin
  FName := Source.FName;
  FOrigName := Source.FOrigName;
  FReminderName := Source.FReminderName;
  FOwner := Source.FOwner;
  FValue := Source.FValue;
  FRealVal := Source.FRealVal;
  FSpace := Source.FSpace;
  FLeapDummy := Source.FLeapDummy;
  FVisible := Source.FVisible;
  FRealDate := Source.FRealDate;
  FWriter := Source.Writer;
  FColor := Source.FColor;
end;

{ TAxisObject ---------------------------------------------------- }

constructor TAxisObject.Create;
begin

end;

destructor TAxisObject.Destroy;
begin
  inherited;
end;

function TAxisObject.GetCanvas: TCanvas;
begin
  if Writer = nil then
    Result := nil
  else
    Result := Writer.Canvas;
end;

procedure TAxisObject.GetGDI;
begin
  FGDIP := TGPGraphics.Create(Canvas.Handle);
  FGDIP.SetSmoothingMode(SmoothingModeAntiAlias);
  FGDIP.SetInterpolationMode(InterpolationMode.InterpolationModeHighQuality);
  FGDIP.SetCompositingQuality(CompositingQuality.CompositingQualityHighQuality);
end;

function TAxisObject.GetGrRect;
begin
  Result := Writer.GraphRect;
end;

function TAxisObject.GetLabelFreq: integer;
begin
  Result := 1;
end;

function TAxisObject.GetLabelSpace: integer;
begin
  Result := 0;
end;

function TAxisObject.GetSectionSpace: integer;
begin
  Result := 0;
end;

function TAxisObject.GetSectionCount: integer;
begin
  Result := 0;
end;

function TAxisObject.GetSectionRect(Index: integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TAxisObject.GetSections: TSections;
begin
  Result := nil;
end;

function TAxisObject.GetLabelRect: TRect;
var
  L, T, R, B: integer;
  HookL, HookT, HookR, HookB: integer;
  TxtL, TxtT, TxtR, TxtB: integer;
begin
  T := 0;
  L := 0;
  R := 0;
  B := 0;
  HookL := 0;
  HookT := 0;
  HookR := 0;
  HookB := 0;
  TxtT := 0;
  TxtL := 0;
  TxtR := 0;
  TxtB := 0;
  if Position in [apTop, apBottom] then
  begin
    L := Writer.GraphRect.Left;
    R := Writer.GraphRect.Right;
    HookL := L;
    TxtL := L;
    HookR := R;
    TxtR := R;
  end
  else if Position in [apLeft, apRight] then
  begin
    B := Writer.GraphRect.Bottom + GetWallOffset;
    T := Writer.GraphRect.Top + GetWallOffset;
    HookB := B;
    TxtB := B;
    HookT := T;
    TxtT := T;
  end;

  if Position = apTop then
  begin
    T := Writer.GraphRect.Top - LabelSpace;
    B := T + LabelSpace;
    HookB := B;
    HookT := B - c_HookSpace;
    TxtB := HookT;
    TxtT := T;
  end
  else if Position = apBottom then
  begin
    B := Writer.GraphRect.Bottom + LabelSpace + GetFloorRect.Height;
    T := B - LabelSpace;
    HookB := T + c_HookSpace;
    HookT := T;
    TxtT := HookB;
    TxtB := B;

  end
  else if Position = apLeft then
  begin
    L := Writer.GraphRect.Left - LabelSpace - GetWallWidth;
    R := L + LabelSpace;
    HookR := R;
    HookL := R - c_HookSpace;
    TxtR := HookL;
    TxtL := L;
  end
  else if Position = apRight then
  begin
    L := Writer.GraphRect.Right;
    R := L + LabelSpace;
    HookL := L;
    HookR := L + c_HookSpace;
    TxtL := HookR;
    TxtR := R;
  end;
  Result := Rect(L, T, R, B);
  FLabelTextRect := Rect(TxtL, TxtT, TxtR, TxtB);
  FLabelHookRect := Rect(HookL, HookT, HookR, HookB);

end;

function TAxisObject.GetFloorRect: TRect;
var
  P: TIntPointArray;
begin
  P := GetWallPoly;
  Result.Top := GrRect.Bottom;
  Result.Left := P[0].X;
  Result.Bottom := P[3].Y;
  Result.Right := GrRect.Right;
end;

function TAxisObject.GetWallRect: TRect;
var
  a: single;
  Y: single;
begin
  a := DegToRad(Writer.WallAngle);
  Y := GrRect.Bottom + Writer.WallWidth * Sin(a);
  Result.Top := GrRect.Top;
  Result.Left := GrRect.Left - Writer.WallWidth;
  Result.Bottom := round(Y);
  Result.Right := GrRect.Left;
end;

function TAxisObject.GetWallPoly: TIntPointArray;
var
  P1, P2, P3, P4: TPoint;
  Y: single;
  a: single;
  R: TRect;
begin
  if Writer = nil then
    Exit;
  a := DegToRad(Writer.WallAngle);
  Y := GrRect.Top + Writer.WallWidth * Sin(a);
  R := GetWallRect;
  P1 := Point(R.Left, round(Y));
  P2 := GrRect.TopLeft;
  P3 := Point(GrRect.Left, GrRect.Bottom);
  P4 := Point(GrRect.Left - Writer.WallWidth, R.Bottom);
  setlength(Result, 4);
  Result[0] := P1;
  Result[1] := P2;
  Result[2] := P3;
  Result[3] := P4;
end;

function TAxisObject.Get3DPoly: TIntPointArray;
begin
  if IsXAxis then
    Result := GetFloorPoly
  else
    Result := GetWallPoly;
end;

function TAxisObject.GetWallWidth: integer;
begin
  Result := 0;
  if Writer <> nil then
    Result := Writer.WallWidth;
end;

function TAxisObject.GetFloorPoly: TIntPointArray;
var
  P1, P2, P3, P4: TPoint;
  X: single;
  R: TRect;
begin
  R := GetFloorRect;
  P1 := Point(R.Left, R.Bottom);
  X := R.Right - Writer.WallWidth;
  P2 := Point(round(X), R.Top + R.Height);
  P3 := Point(GrRect.Right, GrRect.Bottom);
  P4 := Point(GrRect.Left, GrRect.Bottom);
  setlength(Result, 4);
  Result[0] := P1;
  Result[1] := P2;
  Result[2] := P3;
  Result[3] := P4;
end;

function TAxisObject.GetWallOffset: integer;
var
  P: TIntPointArray;
  Y: integer;
begin
  P := GetWallPoly;
  Y := P[0].Y;
  Result := Y - GrRect.Top;
end;

function TAxisObject.GetOpposedSections: Boolean;
begin
  Result := False;
  if Self is TNameAxis then
  begin
    if Writer.NameSectionDefs <> nil then
      Result := Writer.NameSectionDefs.FCaptionLayout = clOppositeSideOfLabels;
  end
  else
  begin
    if Writer.ValueSectionDefs <> nil then
      Result := Writer.ValueSectionDefs.FCaptionLayout = clOppositeSideOfLabels;
  end;
end;

function TAxisObject.GetSectionSpaceRect: TRect;
var
  L, T, R, B: integer;
begin
  L := 0;
  T := 0;
  R := 0;
  B := 0;
  if Position in [apTop, apBottom] then
  begin
    L := Writer.GraphRect.Left;
    R := Writer.GraphRect.Right;
  end
  else if Position in [apLeft, apRight] then
  begin
    T := Writer.GraphRect.Top;
    B := Writer.GraphRect.Bottom;
  end;

  if Position = apTop then
  begin
    if OpposedSections then
    begin
      T := Writer.GraphRect.Bottom;
      B := T + SectionSpace;
    end
    else
    begin
      B := LabelRect.Top;
      T := B - SectionSpace;
    end;
  end

  else if Position = apBottom then
  begin
    if OpposedSections then
    begin
      B := Writer.GraphRect.Top;
      T := B - SectionSpace;
    end
    else
    begin
      T := LabelRect.Bottom;
      B := T + SectionSpace;
    end;
  end

  else if Position = apLeft then
  begin
    if OpposedSections then
    begin
      L := Writer.GraphRect.Right;
      R := L + SectionSpace;
    end
    else
    begin
      R := LabelRect.Left;
      L := R - SectionSpace;
    end;
  end

  else if Position = apRight then
  begin
    if OpposedSections then
    begin
      R := Writer.GraphRect.Left;
      L := R - SectionSpace;
    end
    else
    begin
      L := LabelRect.Right;
      R := L + SectionSpace;
    end;
  end;
  Result := Rect(L, T, R, B);
end;

function TAxisObject.GetUnitSize: integer;
begin
  Result := 0;
end;

function TAxisObject.GetUnitCount: integer;
begin
  Result := 0;
end;

function TAxisObject.GetIsXAxis: Boolean;
begin
  Result := Position in [apTop, apBottom];
end;

procedure TAxisObject.GetAxisHookPos(X, Y: integer; var StartPt, EndPt: TPoint);
var
  R: TRect;
begin
  R := FLabelHookRect;
  if Position = apTop then
  begin
    StartPt := Point(X, R.Bottom - c_HookSize);
    EndPt := Point(X, R.Bottom);
  end
  else if Position = apLeft then
  begin
    StartPt := Point(R.Right - c_HookSize, Y);
    EndPt := Point(R.Right, Y);
  end
  else if Position = apRight then
  begin
    StartPt := Point(R.Left, Y);
    EndPt := Point(R.Left + c_HookSize, Y);
  end
  else { Bottom }
  begin
    StartPt := Point(X, R.Top);
    EndPt := Point(X, R.Top + c_HookSize);
  end;
end;

function TAxisObject.GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint;
begin
  Result := Point(0, 0);
end;

procedure TAxisObject.DrawSections;
var
  i: integer;
  Gr: TRect;
  Sz: TSize;
  X, Y: integer;
  Nm: string;
  SR: TRect;
  UseShorts: Boolean;
  Fits: Boolean;
  Prolong: Boolean;
  TextRects: array of TRect;

  function DoEvent(P: TPoint; AElement: TSectionElement): Boolean;
  var
    Ax: TAxisType;
  begin
    Result := False;
    if Self is TNameAxis then
      Ax := atNameAxis
    else
      Ax := atValueAxis;
    if Assigned(Writer.FOnDrawSection) then
    begin
      Writer.FOnDrawSection(Writer, Ax, P, Sections[i], AElement,
        Canvas, Result);
    end;
  end;

  function GetTextSize(AText: string): TSize;
  begin
    if Self is TNameAxis then
    begin
      Result.cx := Writer.GetTextWidth(lkNameSection, AText);
      Result.cy := Writer.GetTextHeight(lkNameSection)
    end
    else
    begin
      Result.cx := Writer.GetTextWidth(lkValueSection, AText);
      Result.cy := Writer.GetTextHeight(lkValueSection)
    end;
  end;

  function LinesVisible: Boolean;
  begin
    if Self is TValueAxis then
    begin
      Result := Writer.Chart.ValueSectionDefs.ShowLines;
    end
    else
    begin
      Result := Writer.Chart.NameSectionDefs.ShowLines
    end;
  end;

  procedure DrawProlongedLine;
  begin
    if IsXAxis then
    begin
      Canvas.MoveTo(SR.Left, SR.Top);
      Canvas.LineTo(SR.Left, SR.Bottom);
    end
    else
    begin
      Canvas.MoveTo(SR.Left, SR.Top);
      Canvas.LineTo(SR.Right, SR.Top);
    end;
  end;

  procedure SetCanvas;
  begin
    if Self is TValueAxis then
    begin
      Canvas.Pen.Assign(Writer.Chart.ValueSectionDefs.Pen);
      Writer.SetLabelFont(lkValueSection)
    end
    else
    begin
      Canvas.Pen.Assign(Writer.Chart.NameSectionDefs.Pen);
      Writer.SetLabelFont(lkNameSection)
    end;
  end;

  procedure LogTextRect(R: TRect; Txt: string);
  var
    R2: TRect;
    W: integer;
  begin
    if Self is TValueAxis then
    begin
      W := Writer.GetTextWidth(lkValueSection, Txt);
    end
    else
    begin
      W := Writer.GetTextWidth(lkNameSection, Txt);
    end;
    R2.Left := R.CenterPoint.X - W div 2;
    R2.Right := R.CenterPoint.X + W div 2;
    R2.Top := R.Top;
    R2.Bottom := R.Bottom;
    setlength(TextRects, Length(TextRects) + 1);
    TextRects[High(TextRects)] := R2;
  end;

  function TextFits(R: TRect; var Txt: string): Boolean;
  var
    i: integer;
    R2: TRect;
    W: integer;
    S: string;
  begin
    Result := True;
    if Self is TValueAxis then
    begin
      W := Writer.GetTextWidth(lkValueSection, Txt);
    end
    else
    begin
      W := Writer.GetTextWidth(lkNameSection, Txt);
    end;
    if W > SR.Width then
    begin
      S := Txt;
      while (W > SR.Width) and (length(S) > 2) do
      begin
        Delete(S, Length(S), 1);
        W := Writer.GetTextWidth(lkNameSection, S);
      end;
      if (W > SR.Width) or (S = '') then
      begin
        Result := False;
        Exit;
      end
      else
        Txt := S;
    end;
    R2.Left := R.CenterPoint.X - W div 2;
    R2.Right := R.CenterPoint.X + W div 2;
    R2.Top := R.Top;
    R2.Bottom := R.Bottom;
    for i := 0 to High(TextRects) do
    begin
      if TextRects[i].IntersectsWith(R2) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

begin
  Gr := GrRect;
  if ((Self is TValueAxis) and (Writer.Chart.ValueSectionDefs = nil)) or
    ((Self is TNameAxis) and (Writer.Chart.NameSectionDefs = nil)) then
    Exit;

  if ((Self is TValueAxis) and not Writer.Chart.ValueSectionDefs.Visible) or
    ((Self is TNameAxis) and not Writer.Chart.NameSectionDefs.Visible) then
    Exit;

  if Self is TValueAxis then
  begin
    SetCanvas;
    Prolong := Writer.Chart.ValueSectionDefs.ProlongedLines;
  end
  else
  begin
    SetCanvas;
    Prolong := Writer.Chart.NameSectionDefs.ProlongedLines;
  end;
  Finalize(TextRects);

  try
    for i := 0 to SectionCount - 1 do
    begin
      SR := GetSectionRect(i);
      if (SR.Width = 0) and (SR.Height = 0) then
        Continue;

      Canvas.Brush.Style := bsClear;
      if LinesVisible or (Sections[i].SectionType = stLine) then
      begin
        if not DoEvent(Point(0, 0), seLine) then
        begin
          if IsXAxis then
          begin { Vert }
            if (SR.Left <> Gr.Left) then
            begin
              Canvas.MoveTo(SR.Left, Gr.Top);
              Canvas.LineTo(SR.Left, Gr.Bottom);
            end;
            if (SR.Right <> Gr.Right) and not(SR.Right = SR.Left) then
            begin
              Canvas.MoveTo(SR.Right, Gr.Top);
              Canvas.LineTo(SR.Right, Gr.Bottom);
            end;
          end
          else
          begin { Horz }
            if SR.Top <> Gr.Top then
            begin
              Canvas.MoveTo(Gr.Left, SR.Top);
              Canvas.LineTo(Gr.Right, SR.Top);
            end;
            if (SR.Bottom <> Gr.Bottom) and not(SR.Bottom = SR.Top) then
            begin
              Canvas.MoveTo(Gr.Left, SR.Bottom);
              Canvas.LineTo(Gr.Right, SR.Bottom);
            end;
          end;
          if Prolong then
            DrawProlongedLine;
        end;
      end;
      if Writer.InState(stAnimating) then
        Continue;

      if Self is TNameAxis then
        UseShorts := Writer.FUseNamShortNames
      else
        UseShorts := Writer.FUseValShortNames;

      if UseShorts then
        Nm := Sections[i].ShortCaption
      else
        Nm := Sections[i].LongCaption;
      Sz := GetTextSize(Nm);

      X := SR.CenterPoint.X - (Sz.cx div 2);
      Y := SR.CenterPoint.Y - (Sz.cy div 2);

      if DoEvent(Point(X, Y), seText) then
      begin
        SetCanvas;
        Continue;
      end;

      if Self is TNameAxis then
      begin
        if (i Mod Writer.FNameSectionFreq = 0) then
        begin
          Fits := TextFits(SR, Nm);
          if not Fits and (Sections[i].ShortCaption <> Nm) then
          begin
            Nm := Sections[i].ShortCaption;
            Fits := TextFits(SR, Nm);
            X := SR.CenterPoint.X - (Canvas.TextWidth(Nm) div 2)
          end;

          if Fits or (Sections[i].SectionType = stLine) then
          begin
            Canvas.TextOut(X, Y, Nm);
            if Sections[i].SectionType = stSection then
              LogTextRect(SR, Nm);
          end;
        end;
      end
      else
      begin
        if (i Mod Writer.FValueSectionFreq = 0) then
        begin
          Fits := TextFits(SR, Nm);
          if not Fits and (Sections[i].ShortCaption <> Nm) then
          begin
            Nm := Sections[i].ShortCaption;
            Fits := TextFits(SR, Nm);
            X := SR.CenterPoint.X - (Canvas.TextWidth(Nm) div 2)
          end;

          if Fits or (Sections[i].SectionType = stLine) then
          begin
            Canvas.TextOut(X, Y, Nm);
            LogTextRect(SR, Nm);
          end;
        end;
      end;
      if Assigned(Writer.FOnDrawSection) then
        SetCanvas; { Might have been changed in event }
    end;
  finally

  end;
  Writer.ResetCanvas;
end;

procedure TAxisObject.Draw3DAxis;
var
  GPts: TPointDynArray;
  P: TIntPointArray;
  GPen: TGPPen;
  GBrush: TGPSolidBrush;
  GradBrush: TGPLinearGradientBrush;
  Clr1, Clr2: TGPColor;
  R: TRect;
begin

  setlength(GPts, 4);
  P := Get3DPoly;
  if not IsXAxis then
    R := GetWallRect
  else
    R := GetFloorRect;
  GradBrush := nil;
  GBrush := nil;
  GPen := TGPPen.Create(MakeGPClr(Writer.AxisColor));
  if not Writer.GradientWall then
    GBrush := TGPSolidBrush.Create(MakeGPClr(Writer.WallColor))
  else
  begin
    Clr1 := MakeGPClr(Writer.WallColor);
    Clr2 := MakeGPClr(Writer.GraphBGColor);

    if not IsXAxis then
      GradBrush := TGPLinearGradientBrush.Create(MakeRect(R), Clr1, Clr2,
        LinearGradientModeHorizontal)
    else
      GradBrush := TGPLinearGradientBrush.Create(MakeRect(R), Clr2, Clr1,
        LinearGradientModeVertical);
  end;
  GetGDI;
  try
    GPts[0] := MakePoint(P[0].X, P[0].Y);
    GPts[1] := MakePoint(P[1].X, P[1].Y);
    GPts[2] := MakePoint(P[2].X, P[2].Y);
    GPts[3] := MakePoint(P[3].X, P[3].Y);
    if Writer.GradientWall then
      FGDIP.FillPolygon(GradBrush, PGPPoint(@GPts[0]), 4)
    else
      FGDIP.FillPolygon(GBrush, PGPPoint(@GPts[0]), 4);
    FGDIP.DrawPolygon(GPen, PGPPoint(@GPts[0]), 4);

  finally
    GPen.Free;
    if Writer.GradientWall then
      GradBrush.Free
    else
      GBrush.Free;
    FGDIP.Free;
  end;
end;

procedure TAxisObject.DrawAxis;
var
  R: TRect;
begin
  if Writer.GraphBorders = gbNone then
    Exit;
  if Writer.HasWall then
  begin
    Draw3DAxis;
    Exit;
  end;
  if (Self is TNameAxis) and not(veNameLabels in Writer.AxisElements) then
    Exit;
  if (Self is TValueAxis) and not(veValueLabels in Writer.AxisElements) then
    Exit;

  R := Writer.GraphRect;
  with Canvas do
  begin
    Pen.Color := Writer.AxisColor;
    if Position = apLeft then
    begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
    end
    else if Position = apTop then
    begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Top);
    end
    else if Position = apRight then
    begin
      MoveTo(R.Right, R.Top);
      LineTo(R.Right, R.Bottom);
    end
    else if Position = apBottom then
    begin
      MoveTo(R.Left, R.Bottom);
      LineTo(R.Right, R.Bottom);
    end
  end;
  Writer.ResetCanvas;
end;

procedure TAxisObject.Draw;
begin
  if (Writer.ActiveGraph is TCWAxisGraph) then
  begin
    if Self is TValueAxis then
     Writer.FActiveValAx := Self;
    DrawAxis;
    if Writer.FSeriesData.Count <> 0 then
    begin
      DrawLabels;
      DrawSections;
    end;
  end;
end;

{ TValueAxis ---------------------------------------------------- }

function TValueAxis.GetExactValuePos(AValue: single)
  : integer;
{ Returns the X or Y coordinate (depending on the axis orientation) of the
  Value axis, computetd from AValue }
var
  Un: single;
  ValSpan: single;
  P: single;
  R: TRect;
  StartPos : integer;
begin
  ValSpan := Writer.ValueHigh - Writer.ValueLow;
  P := AValue - Writer.ValueLow; { Get the "index" in the span }
  if ValSpan = 0 then
    Un := 0
  else
    Un := abs(P) / ValSpan;
  R := Writer.FPosRect;

  if IsXAxis then
  begin
    StartPos := R.Left;
    Result := Round(StartPos + Un * R.Width);
  end
  else
  begin
    StartPos := R.Bottom;
    Result := Round(StartPos - (Un * R.Height));
  end;
end;

function TValueAxis.GetLabelFreq: integer;
begin
  Result := Writer.ValueLabelFreq;
end;

function TValueAxis.GetCount: integer;
begin
  Result := Writer.ValueCount
end;

function TValueAxis.GetLabelSpace: integer;
var
  W, H: integer;
  lk: TLabelKind;
  QW : integer;
  VS : string;
begin
  if not(veValueLabels in Writer.AxisElements) then
  begin
    Result := 0;
    Exit;
  end;
  QW := 0;
  Result := Writer.FValueLabelSpace;
  if Self is TValueAxis2 then
  begin
    VS := Writer.Chart.ValueAxis2.Qualifier;
    lk := lkValue2;
  end
  else
  begin
    VS := Writer.Chart.ValueAxis1.Qualifier;
    lk := lkValue;
  end;
  if not IsXAxis then
  begin
    if VS <> '' then
      QW := Writer.GetTextWidth(lk, VS);
    W := Writer.GetTextWidth(lk);
    if W <> 0 then
    begin
      Result := W + c_LabelXMarg + c_HookSize;
    end;
    if QW > W then  {Draw vertically}
      Result := Result + Writer.GetTextHeight(lk) + c_QualifierMargin*2;
  end
  else
  begin
    H := Writer.GetTextHeight(lk);
    W := Writer.GetTextWidth(lk);
    if VS <> '' then
      QW := Writer.GetTextWidth(lk, VS);
    if H <> 0 then
    begin
      Result := H + c_HookSize;
    end;
    if QW > W then
      Result := Result + H + c_QualifierMargin*2;
  end;
end;

function TValueAxis.GetSectionSpace: integer;
begin
  if Writer.Chart.ValueSectionDefs = nil then
    Result := 0
  else if not Writer.Chart.ValueSectionDefs.Visible then
    Result := 0
  else
    Result := Writer.ValueSectionSpace;
end;

function TValueAxis.GetSectionRect(Index: integer): TRect;
var
  Sect: TSection;
  AStartPos, AEndPos: integer;
  SVal, EVal: single;

  procedure swap;
  var
    tmp: integer;
  begin
    tmp := AStartPos;
    AStartPos := AEndPos;
    AEndPos := tmp;
  end;

begin
  Result := Rect(0, 0, 0, 0);
  if not Writer.Chart.ValueSectionDefs.Visible then
    Exit;
  Sect := Sections[Index];
  SVal := StrToFloat(Sect.FStartVal);
  EVal := StrToFloat(Sect.FEndVal);

  if not ((SVal >= Writer.ValueLow) and (SVal <= Writer.ValueHigh))
  and not ((EVal >= Writer.ValueLow) and (EVal <= Writer.ValueHigh)) then
   Exit;

  if (SVal < Writer.ValueLow) and (EVal > Writer.ValueHigh) then
    Exit;

  if SVal < Writer.ValueLow then
    SVal := Writer.ValueLow;

  if EVal > Writer.ValueHigh then
    EVal := Writer.ValueHigh;

  AStartPos := GetExactValuePos(SVal);
  AEndPos := GetExactValuePos(EVal);
  if AStartPos > AEndPos then
    swap;

  if IsXAxis then
  begin
    if AStartPos < GrRect.Left then
      AStartPos := GrRect.Left;
    if AEndPos > GrRect.Right then
      AEndPos := GrRect.Right;
    Result := Rect(AStartPos, SectionSpaceRect.Top, AEndPos,
      SectionSpaceRect.Bottom);
  end
  else
  begin
    if AEndPos > GrRect.Bottom then
      AEndPos := GrRect.Bottom;
    if AStartPos < GrRect.Top then
      AStartPos := GrRect.Top;
    Result := Rect(SectionSpaceRect.Left, AStartPos,
      SectionSpaceRect.Right, AEndPos);
  end;
end;

function TValueAxis.GetUnitSize: integer;
begin
  if Self is TValueAxis2 then
   Result := Writer.Chart.FValueAxis2.FValueUnit
  else
   Result := Writer.Chart.FValueAxis1.FValueUnit
end;

function TValueAxis.GetUnitCount: integer;
begin
  Result := 0;
  begin
    if Writer.Chart <> nil then
    begin
      if Self is TValueAxis2 then
        Result := Writer.Chart.ValueAxis2.ValueCount
      else
        Result := Writer.Chart.ValueAxis1.ValueCount
    end;
  end;
end;

function TValueAxis.GetQualifier : string;
begin
    if Self is TValueAxis2 then
      Result := Writer.Chart.ValueAxis2.Qualifier
    else
      Result := Writer.Chart.ValueAxis1.Qualifier
end;

function TValueAxis.GetInterval: single;
begin
    if Self is TValueAxis2 then
      Result := Writer.Chart.ValueAxis2.ValueIntervals
    else
      Result := Writer.Chart.ValueAxis1.ValueIntervals
end;

function TValueAxis.GetLow: single;
begin
   if Self is TValueAxis2 then
      Result := Writer.Chart.ValueAxis2.ValueLow
    else
      Result := Writer.Chart.ValueAxis1.ValueLow
end;

function TValueAxis.GetHigh: single;
begin
   if Self is TValueAxis2 then
      Result := Writer.Chart.ValueAxis2.ValueHigh
    else
      Result := Writer.Chart.ValueAxis1.ValueHigh
end;

function TValueAxis.GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint;
begin
  Result := Writer.ComputeTextPos(X, Y, ALabel, Self);
end;

procedure TValueAxis.DrawLabels;
var
  i: integer;
  Y: integer;
  X: integer;
  S: string;
  E: single;
  PointLnStart, PointLnEnd: TPoint;
  LabelPos: TPoint;
  Handled: Boolean;
  LastPos: TPoint;
  Prec: integer;
  Line3DStart, Line3DEnd: TPoint;
  SPL, SPT, SPR, SPB : integer;
  Q : string;

  procedure DoWrite;
  var
    Dt: TDateTime;
    YOk, XOk: Boolean;
  begin
    if Assigned(Writer.FOnDrawLabel) then
    begin
      Dt := 0;
      Handled := False;
      Writer.FOnDrawLabel(Writer, atValueAxis, S, Dt,
        LabelPos, Canvas, Handled);
      if not Handled then
      begin
        Canvas.TextOut(LabelPos.X, LabelPos.Y, S);
      end;
      { Handled should only be true when text is drawn by user. }
      Writer.ResetCanvas;
    end
    else
    begin
      YOk := (LabelPos.Y >= Writer.GraphRect.Top) or (SPT > 0);
      if YOk then
        YOk := (LabelPos.Y <= Writer.GraphRect.Bottom) or (SPB > 0);
      XOk := (LabelPos.X >= Writer.GraphRect.Left) or (SPL > 0);
      if XOk then
        XOk := (LabelPos.X <= Writer.GraphRect.Right) or (SPR > 0);
      if YOk and XOk then
        Canvas.TextOut(LabelPos.X, LabelPos.Y, S);
    end;
  end;

  procedure DrawLine;
  var
    GPen: TGPPen;
  begin
    if (Writer.GraphBorders = gbAxis) or (Writer.GraphBorders = gbAllSides) then
    begin
      if not Writer.HasWall then
      begin
        Canvas.MoveTo(PointLnStart.X, PointLnStart.Y);
        Canvas.LineTo(PointLnEnd.X, PointLnEnd.Y);
      end
      else
      begin
        GPen := TGPPen.Create(MakeGPClr(Writer.AxisColor));
        FGDIP.DrawLine(GPen, Line3DStart.X, Line3DStart.Y, Line3DEnd.X,
          Line3DEnd.Y);
      end;
      if veValueDividerLines in Writer.AxisElements then
      begin
        Canvas.Pen.Assign(Writer.FDividerLinePen);
        if IsXAxis then
        begin
          if Writer.HasWall then
            PointLnStart.Y := GrRect.Top - c_HookSize;
          Canvas.MoveTo(PointLnStart.X, PointLnStart.Y + c_HookSize);
          Canvas.LineTo(PointLnStart.X, GrRect.Top);
        end
        else
        begin
          if Writer.HasWall then
            PointLnStart.X := GrRect.Left;
          Canvas.MoveTo(PointLnStart.X, PointLnStart.Y);
          Canvas.LineTo(GrRect.Right, PointLnStart.Y);
        end;
      end;
    end;
  end;

begin
  if not(veValueLabels in Writer.AxisElements) then
    Exit;
  X := LabelRect.Left;
  E := LowVal;
  i := 0;
  if IsXAxis then
  begin
    Y := LabelRect.Top;
  end
  else
  begin
    Y := LabelRect.Bottom;
  end;

  if Writer.HasWall then
    GetGDI;
  Writer.SetLabelFont(lkValue);
  SPT := Writer.SpaceOf(apTop);
  SPL := Writer.SpaceOf(apLeft);
  SPR := Writer.SpaceOf(apRight);
  SPB := Writer.SpaceOf(apBottom);
  try
    Prec := Writer.ValuePrecision;
    while (E <= HighVal) do
    begin
      if i = 0 then
      begin
        Q:= GetQualifier;
        if Q <> '' then
        begin
          if Writer.GetTextWidth(lkValue, Q) <=  Writer.GetTextWidth(lkValue) then
          begin
           E := E + Interval;
           inc(i);
           Continue;
          end;
        end;
      end;
      S := FormatNum(E, Prec, True);
      if IsXAxis then
      begin
        X := GetExactValuePos(E);
        GetAxisHookPos(X, Y, PointLnStart, PointLnEnd);
        if Writer.HasWall then
          X := X - Writer.WallWidth;
      end
      else
      begin
        Y := GetExactValuePos(E);
        GetAxisHookPos(X, Y, PointLnStart, PointLnEnd);
        if Writer.HasWall then
        begin
          Y := Y + GetWallOffset;
          Line3DStart.X := PointLnStart.X + c_HookSize;
          Line3DStart.Y := Y;
          Line3DEnd.X := PointLnStart.X + Writer.WallWidth + c_HookSize;
          Line3DEnd.Y := Line3DStart.Y - GetWallOffset;
        end;
      end;
      try
        if i mod LabelFreq = 0 then
        begin
          Canvas.Brush.Style := bsClear;
          LabelPos := GetLabelCenterPos(S, X, Y);
          DoWrite;
          DrawLine;
        end;
      finally
        E := E + Interval;
        LastPos := LabelPos;
        inc(i);
      end;
    end;
    Writer.ResetCanvas;
  finally
    if Writer.HasWall then
      FGDIP.Free;
  end;
end;

function TValueAxis.GetSections: TSections;
begin
  Result := Writer.FValueSections;
end;

function TValueAxis.GetSectionCount: integer;
begin
  Result := Writer.FValueSections.Count;
end;

{ TvalueAxis2 ------------------------------------------------------ }

function TValueAxis2.GetCount: integer;
begin
  Result := Writer.Chart.ValueAxis2.ValueCount;
end;

function TValueAxis2.GetHigh: single;
begin
  Result := Writer.Chart.ValueAxis2.ValueHigh;
end;

function TValueAxis2.GetLow: single;
begin
  Result := Writer.Chart.ValueAxis2.ValueLow;
end;

function TValueAxis2.GetLabelFreq: integer;
begin
  Result := Writer.FValueLabelFreq2;
end;

{ TNameAxis ------------------------------------------------------ }

function TNameAxis.GetCount: integer;
begin
  Result := Writer.NameCount;
end;

function TNameAxis.GetLabelFreq: integer;
begin
  Result := Writer.NameLabelFreq;
end;

function TNameAxis.GetLabelSpace: integer;
begin
  Result := Writer.NameLabelSpace;
end;

function TNameAxis.GetSectionSpace: integer;
begin
  if Writer.Chart.NameSectionDefs = nil then
    Result := 0
  else if not Writer.Chart.NameSectionDefs.Visible then
    Result := 0
  else
    Result := Writer.NameSectionSpace;
end;

function TNameAxis.GetSectionCount: integer;
begin
  Result := Writer.FNameSections.Count;
end;

function TNameAxis.GetSectionRect(Index: integer): TRect;
var
  Sect: TSection;
  AStartPos, AEndPos: integer;
  EVal, SVal: string;

  procedure swap;
  var
    tmp: integer;
  begin
    tmp := AStartPos;
    AStartPos := AEndPos;
    AEndPos := tmp;
  end;

begin
  Result := Rect(0,0,0,0);
  if not Writer.Chart.NameSectionDefs.Visible then
    Exit;
  Sect := Sections[Index];
  SVal := Sect.FStartVal;
  EVal := Sect.FEndVal;

  if IsXAxis then
  begin
    begin
      AStartPos := Writer.XFromName(SVal);
      AEndPos := Writer.XFromName(EVal);
      if AStartPos > AEndPos then
        swap;
      Result := Rect(AStartPos, SectionSpaceRect.Top, AEndPos,
        SectionSpaceRect.Bottom);
    end;
  end
  else
  begin
    AStartPos := Writer.YFromName(SVal);
    AEndPos := Writer.YFromName(EVal);
    if AStartPos > AEndPos then
      swap;
    Result := Rect(SectionSpaceRect.Left, AStartPos,
      SectionSpaceRect.Right, AEndPos);
  end;
end;

function TNameAxis.GetSections: TSections;
begin
  Result := Writer.FNameSections;
end;

function TNameAxis.GetUnitSize: integer;
begin
  Result := Writer.NameUnit;
end;

function TNameAxis.GetUnitCount: integer;
begin
  Result := Writer.NameCount;
end;

function TNameAxis.GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint;
begin
  Result := Writer.ComputeTextPos(X, Y, ALabel, Self);
end;

function TNameAxis.PosFromDate(ADate: TDateTime): integer;
var
  NumDays, Indx: integer;
  Dt1, Dt2: TDateTime;
  Y, y2, M, D: word;
  Leap: Boolean;
  R : TRect;
begin
  Result := 0;
  if Writer.FNameList.Count = 0 then
    Exit;

  Dt1 := StrToDate(Writer.Names[0], Fmt);
  if ADate < Dt1 then
    ADate := Dt1;
  DecodeDate(Dt1, Y, M, D);
  Leap := IsLeapYear(Y) and (M <= 2) and (D <= 29);
  Dt2 := StrToDate(Writer.Names[Writer.FNameList.Count - 1], Fmt);
  if ADate > Dt2 then
    ADate := Dt2;
  DecodeDate(Dt2, y2, M, D);
  NumDays := DaysBetween(Dt2, Dt1);
  if Leap and (y2 > Y) then
    inc(NumDays);
  Indx := DaysBetween(Dt1, ADate);
  R := Writer.FPosRect;
  if IsXAxis then
  begin
    Result := R.Left + round((Indx / NumDays) * R.Width)
  end
  else
    Result := R.Top + round((Indx / NumDays) * R.Height);
end;

function TNameAxis.PosFromHour(ADate: TDateTime): integer;
var
  NumHours, Indx: integer;
  Dt1, Dt2: TDateTime;
  R : TRect;
begin
  Result := 0;
  if Writer.FNameList.Count = 0 then
    Exit;

  Dt1 := StrToDateTime(Writer.Names[0], Fmt);
  Dt2 := StrToDateTime(Writer.Names[Writer.FNameList.Count - 1], Fmt);
  if ADate > Dt2 then
    ADate := Dt2
  else if ADate < Dt1 then
    ADate := Dt1;
  NumHours := HoursBetween(Dt2, Dt1);
  Indx := HoursBetween(Dt1, ADate);
  R := Writer.FPosRect;
  if IsXAxis then
    Result := R.Left + round((Indx / NumHours) * R.Width)
  else
    Result := Writer.GraphRect.Top +
      round((Indx / NumHours) * R.Height);
end;

function TNameAxis.PosFromMinute(ADate: TDateTime): integer;
var
  NumMinutes, Indx: integer;
  Dt1, Dt2: TDateTime;
  R : TRect;
begin
  Result := 0;
  if Writer.FNameList.Count = 0 then
    Exit;

  Dt1 := StrToDateTime(Writer.Names[0], Fmt);
  Dt2 := StrToDateTime(Writer.Names[Writer.FNameList.Count - 1], Fmt);
  if ADate > Dt2 then
    ADate := Dt2
  else if ADate < Dt1 then
    ADate := Dt1;
  NumMinutes := MinutesBetween(Dt2, Dt1);
  Indx := MinutesBetween(Dt1, ADate);
  R := Writer.FPosRect;
  if IsXAxis then
    Result := R.Left + round((Indx / NumMinutes) * R.Width)
  else
    Result := R.Top + round((Indx / NumMinutes) * R.Height);
end;

function TNameAxis.PosFromSecond(ADate: TDateTime): integer;
var
  NumSecs, Indx: integer;
  Dt1, Dt2: TDateTime;
  R : TRect;
begin
  Result := 0;
  if Writer.FNameList.Count = 0 then
    Exit;

  Dt1 := StrToDateTime(Writer.Names[0], Fmt);
  Dt2 := StrToDateTime(Writer.Names[Writer.FNameList.Count - 1], Fmt);
  if ADate > Dt2 then
    ADate := Dt2
  else if ADate < Dt1 then
    ADate := Dt1;
  NumSecs := SecondsBetween(Dt2, Dt1);
  Indx := SecondsBetween(Dt1, ADate);
  R := Writer.FPosRect;
  if IsXAxis then
    Result := R.Left + round((Indx / NumSecs) * R.Width)
  else
    Result := R.Top + round((Indx / NumSecs) * R.Height);
end;

function TNameAxis.PosFromNumber(ANumber: single): integer;
var
  Indx: single;
  Span: single;
  N1, N2: single;
  R : TRect;
begin
  Result := 0;
  if Writer.FNameList.Count = 0 then
    Exit;

  N1 := StrToFloat(Writer.Names[0], Fmt);
  N2 := StrToFloat(Writer.Names[Writer.FNameList.Count - 1], Fmt);
  if ANumber > N2 then
    ANumber := N2
  else if ANumber < N1 then
    ANumber := N1;
  Span := N2 - N1;
  Indx := ANumber - N1;
  R := Writer.FPosRect;
  if IsXAxis then
    Result := R.Left + round(Indx / Span * R.Width)
  else
    Result := R.Top + round(Indx / Span * R.Height);
end;

procedure TNameAxis.DrawLabels;
var
  Y: integer;
  X: integer;
  S: string;
  i, j: integer;
  R: TRect;
  PointLnStart, PointLnEnd: TPoint;
  LabelPos: TPoint;
  W: integer;
  Reduct: integer;
  TotSpace: integer;
  sl: TStringList;
  LblY: integer;
  Handled: Boolean;
  P: integer;
  Messy: Boolean;
  LastPenPos: integer;
  IsReminder: Boolean;
  Line3DStart, Line3DEnd: TPoint;
  GPen: TGPPen;

  procedure DoReduction(Width: integer);
  var
    E: single;
  begin
    E := Width / UnitCount;
    W := Ceil(E);
    TotSpace := Ceil(E) * UnitCount;
    Reduct := TotSpace - Width;
    if Reduct <> 0 then
      Reduct := UnitCount div Reduct;
  end;

  function GetReminderName(Indx: integer): string;
  var
    i: integer;
  begin
    Result := '';
    if Writer.Contraction = 1 then
      Exit;
    for i := 0 to Writer.FSeriesData.Count - 1 do
    begin
      if (Indx >= Writer.FSeriesData[i].FirstItem) and
        (Indx <= Writer.FSeriesData[i].LastItem) then
      begin
        Result := Writer.FSeriesData[i].FSeriesItems[Indx].FReminderName;
        if Result = '' then
          Break;
      end;
    end;
  end;

  function GetItem(Indx: integer): TSeriesItem;
  var
    i: integer;
  begin
    Result := nil;
    for i := 0 to Writer.FSeriesData.Count - 1 do
    begin
      if (Indx >= Writer.FSeriesData[i].FirstItem) and
        (Indx <= Writer.FSeriesData[i].LastItem) then
      begin
        Result := Writer.FSeriesData[i].FSeriesItems[Indx];
        Break;
      end;
    end;
  end;

begin
  if not(veNameLabels in Writer.AxisElements) then
    Exit;
  R := Writer.GraphRect;
  X := LabelRect.Left;
  Y := LabelRect.Top;
  sl := TStringList.Create;
  if Writer.HasWall then
    GetGDI;
  Writer.SetLabelFont(lkName);
  try
    LastPenPos := -1;
    for i := 0 to UnitCount - 1 do
    begin
      if (i mod Writer.NameLabelFreq <> 0) then
      begin
        LastPenPos := -1;
        Continue;
      end;
      S := GetReminderName(i);
      IsReminder := S <> '';
      if S = '' then
        S := Writer.Names[i];
      if IsXAxis then
      begin
        if not IsReminder then
          X := Writer.XFromName(S)
        else
          X := Writer.XFromName(Writer.Names[i]);
        GetAxisHookPos(X, Y, PointLnStart, PointLnEnd);
        if Writer.HasWall then
        begin
          { The hooks }
          X := X - Writer.WallWidth;
          R := GetFloorRect;
          Line3DStart.X := X;
          Line3DStart.Y := R.Bottom;
          Line3DEnd.X := PointLnStart.X;
          Line3DEnd.Y := R.Top;
        end;
      end
      else
      begin
        if not IsReminder then
          Y := Writer.YFromName(S)
        else
          Y := Writer.YFromName(Writer.Names[i]);
        GetAxisHookPos(X, Y, PointLnStart, PointLnEnd);
        Y := Y + GetWallOffset;
      end;
      try
          CreateListFromDelimiter('|', S, sl);
          S := sl[0];
          if Writer.IsTimeSpan(Writer.Chart.NameType) then
            S := GetTimeStr(Writer, Writer.TimeFormat, StrToDateTime(S, Fmt));
          LabelPos := GetLabelCenterPos(S, X, Y);

          if (i > 0) and (LastPenPos <> -1) and (Writer.NameFont.Orientation = 0)
          then
          begin
            if Writer.FNamAx.IsXAxis then
              Messy := (LabelPos.X - 3 < LastPenPos)
            else
              Messy := (LabelPos.Y - 3 < LastPenPos);
            if Messy then
            begin
              { No space available }
              LastPenPos := -1;
              Continue;
            end;
          end;

          LblY := LabelPos.Y;
          Canvas.Brush.Style := bsClear;
          for j := 0 to sl.Count - 1 do
          begin
            S := sl[j];
            if Writer.IsTimeSpan(Writer.Chart.NameType) then
              S := GetTimeStr(Writer, Writer.TimeFormat, StrToDateTime(S, Fmt))
            else if (Writer.Chart.NameType = ntNumberSpan) and
              (Writer.NumSpanPrecision > 0) then
            begin
              P := Length(S) - Writer.NumSpanPrecision;
              if P = 0 then
                S := '0,' + S
              else if P >= 1 then
                Insert(',', S, P + 1);
            end;

            LabelPos := GetLabelCenterPos(S, X, Y);
            if Assigned(Writer.FOnDrawLabel) then
            begin
              Handled := False;
              Writer.FOnDrawLabel(Writer, atNameAxis, S,
                GetItem(i).RealDate, LabelPos, Canvas, Handled);
              if not Handled then
              begin
                Canvas.TextOut(LabelPos.X, LblY, S);
                LastPenPos := Canvas.PenPos.X;
              end
              else
                LastPenPos := -1;
              { Handled should only be true when text is drawn by user. }
              Writer.ResetCanvas;
            end
            else
            begin
              Canvas.TextOut(LabelPos.X, LblY, S);
              if Writer.FNamAx.IsXAxis then
                LastPenPos := Canvas.PenPos.X
              else
                LastPenPos := Canvas.PenPos.Y;
            end;
            inc(LblY, Writer.GetTextHeight(lkName));
          end;
          if ((Writer.GraphBorders = gbAxis) or
            (Writer.GraphBorders = gbAllSides)) then
          { Hooks }
          begin
            if not Writer.HasWall then
            begin
              Canvas.MoveTo(PointLnStart.X, PointLnStart.Y);
              Canvas.LineTo(PointLnEnd.X, PointLnEnd.Y);
            end
            else
            begin
              GPen := TGPPen.Create(MakeGPClr(Writer.AxisColor));
              try
                FGDIP.DrawLine(GPen, Line3DStart.X, Line3DStart.Y, Line3DEnd.X,
                  Line3DEnd.Y);
              finally
                GPen.Free;
              end;

            end;
          end;
          if (veNameDividerLines in Writer.AxisElements) and (i > 0) then
          begin
            Canvas.Pen.Assign(Writer.FDividerLinePen);
            if IsXAxis then
            begin
              if Writer.HasWall then
                Canvas.MoveTo(PointLnStart.X, GrRect.Bottom)
              else
                Canvas.MoveTo(PointLnStart.X, PointLnStart.Y + c_HookSize);
              Canvas.LineTo(PointLnStart.X, GrRect.Top);
            end
            else
            begin
              Canvas.MoveTo(PointLnStart.X + c_HookSize, PointLnStart.Y);
              Canvas.LineTo(GrRect.Right, PointLnStart.Y);
            end;
          end;
      finally
      end;
    end;
  finally
    if Writer.HasWall then
      FGDIP.Free;
    sl.Free;
  end;
  Writer.ResetCanvas;
end;

{ TSection ------------------------------------------------------- }

constructor TSection.Create;
begin
  //
end;

function TSection.GetSectionLabelRect: TRect;
begin
  Result := FOwner.GetSectionRect(FIndex)
end;

function TSection.GetSectionGraphRect: TRect;
begin
  if FOwner.IsXAxis then
    Result := Rect(FOwner.GetSectionRect(FIndex).Left, Writer.GraphRect.Top,
      FOwner.GetSectionRect(FIndex).Right, Writer.GraphRect.Bottom)
  else
    Result := Rect(Writer.GraphRect.Left, FOwner.GetSectionRect(FIndex).Top,
      Writer.GraphRect.Right, FOwner.GetSectionRect(FIndex).Bottom);
end;

{ TCWValueAxis-----------------------------------------------------------}

function TCWValueAxis.GetWriter : TChartWriter;
begin
  Result := FOwner.Writer;
end;

procedure TCWValueAxis.Assign(Source : TPersistent);
begin
  if Source is TCWValueAxis then
  begin
    FValueIntervals := TCWValueAxis(Source).ValueIntervals;
    FValueHigh := TCWValueAxis(Source).ValueHigh;
    FValueLow := TCWValueAxis(Source).ValueLow;
    FValueSpanFromData := TCWValueAxis(Source).ValueSpanFromData;
    FValuePrecision := TCWValueAxis(Source).ValuePrecision;
  end
  else
    inherited Assign(Source)
end;

function TCWValueAxis.GetValueFloatUnit: single;
begin
  if ValueCount = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if Writer.FValAx.IsXAxis then
  begin
    Result := Writer.GraphSpaceRect.Width / ValueCount;
    { Only called by RestrictToClinet}
  end
  else
  begin
    Result := Writer.GraphSpaceRect.Height / ValueCount;
  end;
end;

function TCWValueAxis.GetValueCount: integer;
{ Counts the number og whole YUnits }
begin
  Result := floor((ValueHigh - ValueLow + 1) / FValueIntervals);
end;

procedure TCWValueAxis.SetValueSpanFromData(Value: Boolean);
begin
  if Value = FValueSpanFromData then
    Exit;
  FValueSpanFromData := Value;

  if not(csLoading in FOwner.componentState) and FOwner.IsActive then
    with Writer do
    begin
      begin
        if Value then
          Self.SetHighLow
        else
          Self.SetValueSpan(ValueLow, ValueHigh);
        RestrictToClient(True);
        CheckLabelFreqs;
        DoRepaint;
      end;
    end;
end;

procedure TCWValueAxis.SetValueHigh(Value: single);
var
  ErrNumber: single;
  Insp: Boolean;
begin
  if (Value = FValueHigh) then
    Exit;
  if Value <= FValueLow then
    ShowGWError(msg_LowHigh);
  if not(csLoading in FOwner.componentState) and FOwner.IsActive then
    begin
      if ValueSpanFromData then
        Exit;
      Insp := Writer.InSpan(ValueLow, Value, ErrNumber);

      if not Insp then
      begin
        ShowGWError(msg_NotInSpan, '(' + FormatNum(Value,
          ValuePrecision) + ')');
      end;
    end;
  FValueHigh := Value;
  if FOwner.IsActive then
    Writer.RefreshGraph;
end;

procedure TCWValueAxis.SetValueLow(Value: single);
var
  ErrNumber: single;
  Insp: Boolean;
begin
  if (Value = FValueLow) then
    Exit;
  if Value >= FValueHigh then
    ShowGWError(msg_LowHigh);
  if not(csLoading in FOwner.componentState) and FOwner.IsActive then
    begin
      if ValueSpanFromData then
        Exit;
      Insp := Writer.InSpan(Value, ValueHigh, ErrNumber);

      if not Insp then
      begin
        ShowGWError(msg_NotInSpan, '(' + FormatNum(Value, ValuePrecision) + ')');
      end;
    end;

  FValueLow := Value;
  if FOwner.IsActive then
    Writer.RefreshGraph;
end;

procedure TCWValueAxis.SetValueIntervals(Value: single);
begin
  if Value = FValueIntervals then
    Exit;
  if (Value < 0.1) or (Value = FValueIntervals) then
    Exit;
  FValueIntervals := Value;
  if FOwner.IsActive and not(csLoading in FOwner.componentState) then
    Writer.RefreshGraph;
end;

procedure TCWValueAxis.SetValuePrecision(Value: integer);
begin
  if Value = FValuePrecision then
    Exit;
  if (Value < 0) or (Value > 3) then
    Exit;
  FValuePrecision := Value;
  if FOwner.IsActive and not(csLoading in FOwner.componentState) then
    Writer.DoRepaint;
end;

procedure TCWValueAxis.SetHighLow;
begin
  if not FOwner.IsActive then
    Exit;
  if Fowner.Writer.Count = 0 then
    Exit;
  Writer.GetValueSpan(FValueHigh, FValueLow);
end;

procedure TCWValueAxis.SetValueSpan(LowValue, HighValue: single);
var
  ErrNumber: single;
  Insp: Boolean;
begin
  if ValueSpanFromData then
    ShowGWError(msg_TiedToSeries);
  if LowValue >= HighValue then
    ShowGWError(msg_LowHigh);
  if (FValueHigh = HighValue) and (FValueLow = LowValue) then
  begin
    Exit;
  end;
  if FOwner.IsActive then
  begin
    Insp := Writer.InSpan(LowValue, HighValue, ErrNumber);
    if not Insp then
    begin
      ShowGWError(msg_NotInSpan, '(' + FormatNum(ErrNumber,
        ValuePrecision) + ')');
    end;
  end;

  FValueHigh := HighValue;
  FValueLow := LowValue;
  if FOwner.Writer.Count = 0 then
    Exit;
  if FOwner.IsActive then
    with Writer do
    begin
      RefreshGraph;
    end;
end;

procedure TCWValueAxis.SetQualifier(Value : string);
begin
  Value := Trim(Value);
  if Value = FQualifier then
    Exit;
  FQualifier := Value;
  if Fowner.IsActive and not(csLoading in FOwner.componentState) then
    Writer.RefreshGraph;
end;

{ TCWChart ---------------------------------------------------------------}

constructor TCWChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemColors := TCWColors.Create(Self, TCWColor);
  FSeriesDefs := TCWSeriesDefs.Create(Self, TCWSeriesDef);
  FLegends := TCWLegends.Create(Self, TCWLegendItem);
  FValueAxis1 := TCWValueAxis.Create;
  FNameType := ntGeneral;
  FOverflowAction := ovNone;
  with FValueAxis1 do
  begin
    FValueHigh := 1;
    FValueLow := 0;
    FValueIntervals := 1;
    FValuePrecision := 0;
    FOwner := Self;
  end;
  FValueAxis2 := TCWValueAxis.Create;
  with FValueAxis2 do
  begin
    FValueHigh := 1;
    FValueLow := 0;
    FValueIntervals := 1;
    FValuePrecision := 0;
    FOwner := Self;
  end;
  FSeriesRects := TLegendRects.Create;
  FPointRects := TLegendRects.Create;
  FSummaryRects := TLegendRects.Create;
  FWallColor := clWindow;
  FAxisColor := clBlack;
  FAxisElements := [veNameLabels, veValueLabels];
  FTextTilting := [];
  FTextTiltThreshold := 1;
end;

destructor TCWChart.Destroy;
begin
  FItemColors.Free;
  FSeriesDefs.Free;
  FValueAxis1.Free;
  FValueAxis2.Free;
  FLegends.Free;
  FSeriesRects.Free;
  FPointRects.Free;
  FSummaryRects.Free;
  inherited;
end;

procedure TCWChart.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (csDestroying in componentState) then
  begin
    inherited;
    Exit;
  end;

  if (Operation = opRemove) then
  begin
     if (AComponent is TCWNameSectionDefs) then
      FNameSectionDefs := nil
     else if (AComponent is TCWValueSectionDefs) then
      FValueSectionDefs := nil;
  end;
  inherited;
end;

function TCWChart.GetAxisCount : integer;
var
 i : integer;
 Has1, Has2 : Boolean;
begin
   Result := 0;
   Has1 := false;
   Has2 := false;
   for I := 0 to FSeriesDefs.Count-1 do
   begin
      if (FSeriesDefs.Items[i].ValueAxis = vaValueAxis1) and
      (FSeriesDefs.Items[i].Graph is TCWAxisGraph) then
        Has1 := True
      else if (FSeriesDefs.Items[i].Graph is TCWAxisGraph) then
        Has2 := True;
   end;
   if Has1 and Has2 then
     Result := 2
   else if Has1 then
     Result := 1;
end;

function TCWChart.GetWallWidth: integer;
begin
  Result := 0;
  if Writer = nil then
    Exit;
  if Writer.HasWall then
    Result := FWallWidth;
end;

procedure TCWChart.SetWallWidth(Value: integer);
begin
  if Value = FWallWidth then
    Exit;
  if (Value < 0) or (Value > 300) then
    Exit;
  if (Value <> 0) and (Value < 10) then
    Exit;

  FWallWidth := Value;

  if (csLoading in ComponentState) or (Writer = nil) then
    Exit;

  if FWallWidth > 0 then
  begin
    if not Writer.HasWall then
    begin
      if AxisOrientation <> alBottomLeft then
        ShowGWMessage(msg_WallOrientation)
      else if Writer.GraphBorders = gbNone then
        ShowGWMessage(msg_WallBorder);
    end;
  end;
  Writer.RefreshGraph;
end;

procedure TCWChart.SetNameType(Value: TNameType);
var
  NT : TNameType;
  G :TCWGraph;
begin
  if csLoading in ComponentState then
  begin
    FNametype := Value;
    Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
      Exit;
  end;
  //if not (csLoading in ComponentState) then
  // if (Writer = nil) then
  //  ShowGWError(msg_WriterNotAssigned);

  if Writer <> nil then
   if Writer.DsgnRealData then
    Exit;

  if Value = FNameType then
    Exit;
  if (SeriesDefs.Count > 0) and (SeriesDefs[0].Graph is TCWPie) and (Value <> ntGeneral) then
    ShowGWError(msg_PieGeneral);
  if IsActive and (Writer.Count > 0) then
  begin
     NT := Writer.DetectNameType(0);
     if (NT <> Value) and (Value <> ntGeneral) and not (Writer.ActiveGraph is TCWCurve) then
       ShowGWError(msg_NameTypeData);
  end;

  if (Writer <> nil) and (SeriesDefs.Count > 0) and (Value <> ntGeneral) then
  begin
   G := AllEqual;
   if (G <> nil) and not (G is TCWCurve) then
     ShowGWMessage(msg_ChangeSpanned);
  end;


  FNameType := Value;
  if FNameType = ntGeneral then
  begin
    if not (OverflowAction in [ovScrolling, ovNone]) then
      FOverflowAction := ovNone;
  end;
  if csLoading in ComponentState then
    Exit;
  if ISactive and (SeriesDefs.Count > 0) then
  begin
    Writer.RenderDesigner;
  end;
end;

procedure TCWChart.SetWriter(Value: TChartWriter);
var
  Indx : integer;
  Old : TChartWriter;
  W : TChartWriter;
begin
  if Value = FWriter then
    Exit;
  if csLoading in ComponentState then
  begin
    FWriter := Value;
    Writer.FChartList.Add(Self);
    if Value <> nil then
      FreeNotification(Value);
    Exit;
  end;
  if IsActive then
   W := Writer
  else
   W := nil;

  Old := FWriter;
  FWriter := Value;
  if FWriter <> nil then
  begin
    FreeNotification(Value);
    Writer.FChartList.Add(Self);
    Writer.RefreshGraph;
  end
  else
  begin
    Indx := Old.ChartList.IndexOf(Self);
    if Indx <> -1 then
      Old.ChartList.Delete(Indx);
    if W <> nil then
    begin
      W.Chart := nil;
    end;

    if Old <> nil then
     Old.RefreshGraph;
  end;

end;

procedure TCWChart.SetTitle(Value: string);
begin
  if Value = FTitle then
    Exit;
  Value := Trim(Value);
  if (csLoading in componentState) then
  begin
    FTitle := Value;
    Exit;
  end;
  FTitle := Value;
  if IsActive then
  begin
    Writer.RestrictToClient(False);
    Writer.CheckLabelFreqs;
    Writer.DoRepaint;
  end;
end;

function TCWChart.GetTitle : string;
begin
  Result := FTitle;
  if Assigned(FOnTitle) then
  begin
    FOnTitle(Self, -1, Result);
  end;
end;

function TCWChart.GetWriter : TChartWriter;
begin
  Result := GetWriterFromWList(FWID);
end;

procedure TCWChart.SetNameSectionDefs(Value: TCWNameSectionDefs);
var
  FOld: TCWNameSectionDefs;
begin
  if Value = NameSectionDefs then
    Exit;
  if csLoading in ComponentState then
  begin
    FNameSectionDefs := Value;
    Value.FWID := AddToWList(Writer, Value);
    Exit;
  end;
  FOld := FNameSectionDefs;
  FNameSectionDefs := Value;
  if Value <> nil then
  begin
    try
      NameSectionDefs.CheckConfig;
    except
      FNameSectionDefs := FOld;
      raise;
    end;
  end;
  if Value <> nil then
  begin
    Value.FWID := AddToWList(Writer, Value);
  end;
  if IsActive and (Value <> nil)
  and not(csLoading in componentState) then
  begin
    Writer.CreateNameSections;
    Writer.RefreshGraph;
  end
  else if IsActive and (Value = nil)
  and not(csLoading in componentState) then
     Writer.RefreshGraph;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCWChart.SetValueSectionDefs(Value: TCWValueSectionDefs);
var
  Old : TCWValueSectionDefs;
begin
  if Value = ValueSectionDefs then
    Exit;

  if csLoading in ComponentState then
  begin
    FValueSectionDefs := Value;
    Value.FWID := AddToWList(Writer, Value);
    Exit;
  end;
  Old := FValueSectionDefs;
  FValueSectionDefs := Value;
  if Value <> nil then
  begin
    Value.FWID := AddToWList(Writer,  Value);
  end;

  if IsActive and (Value <> nil)
  and not(csLoading in componentState)
  and (AxisCount = 1) then
  begin
    Writer.CreateValueSections;
    Writer.RefreshGraph;
  end
  else if IsActive and (Value = nil)
  and not(csLoading in componentState) then
    Writer.RefreshGraph;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCWChart.SetOverflowAction(Value : TOverflowAction);
begin
  if Value = FOverflowAction then
    Exit;
  if Writer <> nil then
  if (Writer.ActiveGraph is TCWBar) and not (csLoading in ComponentState) then
  begin
     if (TCWBar(Self).Layout = blSideBySide) and (Value = ovCompression) then
      ShowGWError(msg_CompressionSideBySide);
  end;
  if (NameType = ntGeneral) and not (csLoading in ComponentState) then
  begin
    if Value in [ovCompression, ovContraction] then
      ShowGWError(msg_GeneralNameType);

  end;
  FOverflowAction := Value;
  if IsActive then
  begin
      if csDesigning in ComponentState then
        Writer.RenderDesigner
      else
        Writer.Reload;
  end;
end;

procedure TCWChart.SetValuePrecision(Value : integer);
begin
if Value = FValuePrecision then
    Exit;
  if (Value < 0) or (Value > 3) then
    Exit;
  FValuePrecision := Value;
  if IsActive and not(csLoading in ComponentState) then
    Writer.DoRepaint;
end;

procedure TCWChart.SetAxisColor(Value: TColor);
begin
  if Value = FAxisColor then
    Exit;
  FAxisColor := Value;
  if IsActive then
  begin
      Writer.DoRepaint;
  end;
end;

procedure TCWChart.SetGradientWall(Value: Boolean);
begin
  if Value = FGradientWall then
    Exit;
  FGradientWall := Value;
  if IsActive then
   Writer.DoRepaint;
end;

procedure TCWChart.SetWallColor(Value: TColor);
begin
  if Value = FWallColor then
    Exit;
  FWallColor := Value;
  if IsActive then
    Writer.DoRepaint;
end;

procedure TCWChart.DrawLegends;
var
  i: integer;
begin
  FSeriesRects.Clear;
  FPointRects.Clear;
  FSummaryRects.Clear;
  for i := 0 to Legends.Count - 1 do
    if Legends.Items[i].FLegend <> nil then
    begin
      Legends.Items[i].FLegend.Draw;
    end;
end;

procedure TCWChart.DrawTitle;
var
  X, Y : integer;
  R : TRect;
begin
   if (Writer = nil) or (Writer.Chart = nil) or (Writer.Chart.Title = '') then
     Exit;
   R := Writer.GraphRect;
   Writer.Canvas.Brush.Style := bsClear;
   Writer.Canvas.Font.Assign(Writer.TitleFont);
   Y := Writer.WorkRect.Top + 5;
   X := 0;
   case Writer.TitleAligment of
     taLeftJustify: X := R.Left;
     taRightJustify: X := R.Right - Writer.Canvas.TextWidth(Title);
     taCenter: X := R.CenterPoint.X - Writer.Canvas.TextWidth(Title) div 2;
   end;
   Writer.Canvas.TextOut(X, Y, Title);
end;

procedure TCWChart.DrawQualifiers;
var
 S1, S2 : string;
 R : TRect;
 X, Y : integer;
 H, W : integer;
 Orient : integer;

 function GetRect(Ax : TCWValueAxis) : TRect;
 var
   GR : TRect;
   AxObj : TAxisobject;

 begin
     GR := Writer.GraphRect;
     if Ax = ValueAxis1 then
      AxObj := Writer.FValAx
     else
      AxObj := Writer.FValAx2;
     if not AxObj.IsXAxis then
     begin
       if Ax.FQualifierFits then
       begin
          Result.Top := AxObj.LabelRect.Top;
          Result.Bottom := Result.Top + Writer.GettextHeight(lkValue) + c_QualifierMargin*2;
          Result.Left := AxObj.LabelRect.Left;
          Result.Right := AxObj.LabelRect.Right;
       end
       else
       begin
         Result.Top := GR.Top;
         Result.Bottom := GR.Bottom;
         if AxObj.Position = apLeft then
         begin
           Result.Left := AxObj.LabelRect.Left;
           Result.Right := AxObj.LabelRect.Right- Writer.GetTextWidth(lkValue)-c_HookSize;
         end
         else
         begin
           Result.Right := AxObj.LabelRect.Right;
           Result.Left := AxObj.LabelRect.Left+ Writer.GetTextWidth(lkValue)+c_HookSize;
         end;
       end;
     end
     else
     begin
       if Ax.FQualifierFits then
       begin
          Result.Left := AxObj.LabelRect.Left;
          Result.Right := Result.Left + Writer.GettextWidth(lkValue) + c_QualifierMargin*2;
          Result.Top := AxObj.LabelRect.Top;
          Result.Bottom := AxObj.LabelRect.Bottom;
       end
       else
       begin
         Result.Left := GR.Left;
         Result.Right := GR.Right;
         if AxObj.Position = apTop then
         begin
           Result.Top := AxObj.LabelRect.Top;
           Result.Bottom := AxObj.LabelRect.Bottom - Writer.GetTextHeight(lkValue) - c_HookSize;
         end
         else
         begin
           Result.Bottom := AxObj.LabelRect.Bottom;
           Result.Top := AxObj.LabelRect.Top + Writer.GetTextHeight(lkValue) + c_HookSize;
         end;
       end;
     end;
 end;

begin
    Writer.Canvas.Font.Assign(Writer.ValueFont);
    S1 := ValueAxis1.FQualifier;
    S2 := ValueAxis2.FQualifier;
    if (S1 = '') and (S2 = '') then
      Exit;
    Orient := Writer.Canvas.Font.Orientation;
    if S1 <> '' then
    begin
      H := Writer.GetTextHeight(lkValue);
      W := Writer.GetTextWidth(lkValue, S1);
      R := GetRect(ValueAxis1);
      if ValueAxis1.FQualifierFits then
      begin
        X := R.CenterPoint.X - W div 2;
        Y := R.Top;
        if Writer.FValAx.Position = apBottom then
        begin
         X := R.Left;
         Y := R.CenterPoint.Y - H div 2 + c_HookMargin;
        end
        else if Writer.FValAx.Position = apTop then
        begin
         X := R.Left;
         Y := R.CenterPoint.Y - H div 2 - c_HookMargin;
        end;
        Writer.Canvas.TextOut(X, Y, S1);
      end
      else
      begin
         if not Writer.FValAx.IsXAxis then
         begin
           Writer.Canvas.Font.Orientation := 900;
           X := R.CenterPoint.X - H div 2;
           Y := R.CenterPoint.Y + W div 2;
         end
         else
         begin
           X := R.CenterPoint.X - W div 2;
           Y := R.CenterPoint.Y - H div 2;
         end;
         Writer.Canvas.TextOut(X, Y, S1);
         Writer.Canvas.Font.Orientation := Orient;
      end;
    end;


    if S2 <> '' then
    begin
      H := Writer.GetTextHeight(lkValue);
      W := Writer.GetTextWidth(lkValue, S2);
      R := GetRect(ValueAxis2);
      if ValueAxis2.FQualifierFits then
      begin
        if Writer.FValAx2.Position = apBottom then
        begin
          X := R.Left;
          Y := R.CenterPoint.Y - H div 2 + c_HookMargin;
        end
        else
        begin
          X := R.Left;
          Y := R.CenterPoint.Y - H div 2 - c_HookMargin;
        end;
        Writer.Canvas.TextOut(X, Y, S2);
      end
      else
      begin
         if not Writer.FValAx2.IsXAxis then
         begin
           Orient := Writer.Canvas.Font.Orientation;
           Writer.Canvas.Font.Orientation := 900;
           X := R.CenterPoint.X - H div 2;
           Y := R.CenterPoint.Y + W div 2;
         end
         else
         begin
          X := R.CenterPoint.X - W div 2;
          Y := R.CenterPoint.Y - H div 2;
         end;
         Writer.Canvas.TextOut(X, Y, S2);
         Writer.Canvas.Font.Orientation := Orient;
      end;
    end;
end;

procedure TCWChart.CreateLegendContent;
var
  i: integer;
begin
  for i := 0 to Legends.Count - 1 do
    if Legends.Items[i].FLegend <> nil then
    begin
      Legends.Items[i].FLegend.CreateContent;
    end;
end;

procedure TCWChart.CheckQualifiers;
var
  S1, S2 : string;
  L1, L2 : integer;
  H : integer;
begin
    ValueAxis1.FQualifierFits := false;
    ValueAxis2.FQualifierFits := false;
    S2 := '';
    S1 := ValueAxis1.Qualifier ;
    if AxisCount = 2 then
      S2 := ValueAxis2.Qualifier;
    if (S1 = '') and (S2 = '') then
      Exit;
    Writer.ComputeTextExtent;
    Writer.FTextComputed := True;

    L1 := Writer.GetTextWidth(lkValue, S1);
    L2 := Writer.GetTextWidth(lkValue);

    if (S1 <> '') and (L1 <= L2) then
    begin
      if Writer.FValAx.IsXAxis then
      begin
        if Writer.FGraphMargins.Left < L1 then
        begin
         Writer.FGraphMargins.Left := L1 + c_QualifierMargin;
        end;
      end
      else
      begin
        H := Writer.GetTextHeight(lkValue) + c_QualifierMargin;
        if Writer.FGraphMargins.Top < H then
          Writer.FGraphMargins.Top := H;
      end;
      ValueAxis1.FQualifierFits := True;
    end;

    L1 := Writer.GetTextWidth(lkValue2, S2);
    L2 := Writer.GetTextWidth(lkValue2);

    if (S2 <> '') and (L1 <= L2) then
    begin
      if Writer.FValAx.IsXAxis then
      begin
        if Writer.FGraphMargins.Left < L1 then
         Writer.FGraphMargins.Left := L1 + c_QualifierMargin;
      end
      else
      begin
        H := Writer.GetTextHeight(lkValue2) + c_QualifierMargin;
        if Writer.FGraphMargins.Top < H then
          Writer.FGraphMargins.Top := H;
      end;
      ValueAxis2.FQualifierFits := True;
    end;
end;

function TCWChart.ValAx1Graph : TCWGraph;
var
  i : integer;
begin
  Result := nil;
  for I := 0 to FseriesDefs.Count-1 do
    if FseriesDefs.Items[i].ValueAxis = vaValueAxis2 then
    begin
      Result := FseriesDefs.Items[i].Graph;
    end;
end;

procedure TCWChart.SetColor(Index : integer; Usage : TColorUsage; AColor : TColor);
begin
  if Usage = cuOnSeries then
    FSeriesDefs.Items[Index].FColor := AColor
  else
    ItemColors.Items[Index].FColor := AColor;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWChart.SetAxisOrientation(Value: TAxisOrientation);
begin
  if Value = FAxisOrientation then
    Exit;
  FAxisOrientation := Value;
  if Writer <> nil then
    Writer.SetPosition(Value);
  if not IsActive or (csLoading in ComponentState) then
    Exit;
  Writer.FDynaSectStart := -1;
  Writer.FDynaSectEnd := -1;
  Writer.FViewMode := vmNormal;
  Writer.RefreshGraph;
end;

procedure TCWChart.SetAxisElements(Value: TAxisElements);
begin
  if Value = FAxisElements then
    Exit;
  FAxisElements := Value;
  if IsActive and not (csLoading in ComponentState) then
  begin
   Writer.RestrictToClient(False);
   Writer.CheckLabelFreqs;
   Writer.DoRepaint;
  end;

end;

procedure TCWChart.ClearSeriesDefs;
begin
    FSeriesDefs.Clear;
end;

procedure TCWChart.SetSeriesDef(Index : integer; ATitle : string; AGraph : TCWGraph; AColor : TColor; AValueAxis : TValueAxisNumber);
var
  SD: TCWSeriesDef;
begin
   SD := FSeriesDefs.Items[Index];
   SD.FGraph := AGraph;
   SD.FColor := AColor;
   SD.FValueAxis := AValueAxis;
   SD.FTitle := ATitle;
end;

procedure TCWChart.AddSeriesDef(ATitle : string; AGraph : TCWGraph; AColor : TColor; AValueAxis : TValueAxisNumber);
var
  SD : TCWSeriesDef;
begin
   SD := FSeriesDefs.Add;
   SD.FGraph := AGraph;
   SD.FColor := AColor;
   SD.FValueAxis := AValueAxis;
   SD.FTitle := ATitle;
end;

procedure TCWChart.ReplaceGraphs(NewGraph : TCWGraph);
var
  i : integer;
begin
  if (Self is TCWAxisChart) and (NewGraph is TCWPie) then
    ShowGWError(msg_PieAxisChart)
  else if (Self is TCWPieChart) and (NewGraph is TCWCurve) then// (NewGraph is TCWAxisGraph) then
    ShowGWError(msg_AxisPieChart);
  for I := 0 to FSeriesDefs.Count-1 do
  begin
    FSeriesDefs.Items[i].Graph := NewGraph;
    if Self is TCWPieChart then
    begin
      if NewGraph is TCWBar then
        FSeriesDefs.Items[i].FValueAxis := vaValueAxis1
      else
        FSeriesDefs.Items[i].FValueAxis := vaNone
      end;
  end;
  if (Self is TCWPieChart) and (NewGraph is TCWBar) then
  begin
    ValueAxis1.ValueSpanFromData := True;
    ValueAxis1.SetHighLow;
  end;
end;

function TCWChart.AllEqual : TCWGraph;
var
  i : integer;
  VT : TValueAxisNumber;
begin
  Result := nil;
  if FSeriesDefs.Count = 0 then
    Exit;
  Result := FSeriesDefs.Items[0].Graph;
  if Result = nil then
    Exit;
  VT := FSeriesDefs.Items[0].ValueAxis;
  Result := FSeriesDefs.Items[0].Graph;
  for I := 1 to SeriesDefs.Count-1 do
  begin
    if (VT <> FSeriesDefs.Items[i].ValueAxis)
    or (Result <> FSeriesDefs.Items[i].Graph) then
    begin
      Result := nil;
      Break;
    end;
  end;
end;

function TCWChart.GraphCount(AGraph : TCWGraph) : integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FseriesDefs.Count-1 do
  begin
     if FseriesDefs.Items[i].Graph = AGraph then
       inc(Result);
  end;
end;

function TCWChart.IsActive : Boolean;
begin
   Result := (Writer <> nil) and (Self = Writer.Chart);
end;

function TCWChart.GetVisibleCount : integer;
var
 i : integer;
begin
  Result := 0;
  for i := 0 to SeriesDefs.Count - 1 do
    if (SeriesDefs[i].Visible) then
      inc(Result);

end;

procedure TCWChart.SetTextTilting(Value: TTextOrientations);
begin
  if Value = FTextTilting then
    Exit;
  FTextTilting := Value;
  if not(csLoading in componentState) and IsActive then
  begin
    Writer.CheckLabelFreqs(False);
    Writer.RefreshGraph;
  end;
end;

procedure TCWChart.SetTextTiltThreshold(Value: integer);
begin
  if Value = FTextTiltThreshold then
    Exit;
  if (Value < 1) or (Value > 100) then
    Exit;
  FTextTiltThreshold := Value;
  if not(csLoading in componentState) and IsActive then
  begin
    Writer.CheckLabelFreqs(False);
    Writer.RefreshGraph;
  end;
end;

procedure TCWChart.SetNumSpanPrecision(Value: integer);
begin
  if Value = FNumSpanPrecision then
    Exit;
  FNumSpanPrecision := Value;
  if IsActive then
  begin
    Writer.RefreshGraph;
  end;
end;

procedure TCWChart.SetFileName(Value : TFileName);
begin
  Value := Trim(Value);
  if SameText(Value, FFileName) then
    Exit;
  FFileName := Value;
  if FFileName <> '' then
    FDataset := nil;
end;

procedure TCWChart.SetDataset(Value : TDataset);
begin
  if Value = FDataset then
    Exit;
  FDataset := Value;
  if Value <> nil then
    FFilename := '';
end;

{ TCWGraph ------------------------------------------------------------- }

constructor TCWGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FreeNotification(AOwner);
end;

destructor TCWGraph.Destroy;
begin
  inherited;
end;

function TCWGraph.GetCanvas: TCanvas;
begin
  if Writer <> nil then
  begin
    Result := Writer.Canvas
  end
  else
    Result := nil;
end;

procedure TCWGraph.Draw;
begin
  if not(Self is TCWCurve) then
  begin
    FGDIP := TGPGraphics.Create(Canvas.Handle);
    FGDIP.SetSmoothingMode(SmoothingModeAntiAlias);
    FGDIP.SetInterpolationMode(InterpolationMode.InterpolationModeHighQuality);
    FGDIP.SetCompositingQuality
      (CompositingQuality.CompositingQualityHighQuality);
  end;
  if Writer.Chart.ValAx1Graph = self then
      Writer.FActiveValAx := Writer.FValAx2
    else
      Writer.FActiveValAx := Writer.FValAx;
end;

function TCWGraph.InChart(SeriesIndex: Integer = -1) : Boolean;
var
  i : integer;
begin
  Result := false;
  if (Writer = nil) or (Writer.Chart = nil) then
    Exit;
  if SeriesIndex = -1 then
  begin
    for I := 0 to Writer.Chart.SeriesDefs.Count-1 do
    begin
      Result := Writer.Chart.SeriesDefs[i].Graph = Self;
      if Result then
        Break;
    end;
  end
  else
  begin
   if (SeriesIndex > Writer.Count-1) or (SeriesIndex < 0)  then
     Exit;
   Result := Writer.Chart.FSeriesDefs.Items[SeriesIndex].Graph = Self;
  end;
end;

procedure TCWGraph.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (csDestroying in componentState) then
  begin
    inherited;
    Exit;
  end;
  if (Operation = opRemove) and (AComponent = Writer) then
    FWriter := nil;
  inherited;
end;

procedure TCWGraph.AssignGraph(Source: TCWGraph);
begin
  FWriter := Source.FWriter;
end;

function TCWGraph.GetSeries(Value : integer) : TSeries;
begin
  Result := nil;
  if (Writer <> nil) and (Writer.Chart <> nil) then
    Result := Writer.Series[Value];
end;

function TCWGraph.GetActiveColor(SeriesIndex, ItemIndex : integer): TColor;
begin
  Result := clBlack;
end;

function TCWGraph.GetWriter : TChartWriter;
begin
  Result := GetWriterFromWList(FWID);
end;

function TCWGraph.GetChart : TCWChart;
begin
  Result := nil;
  if Writer <> nil then
    Result := Writer.Chart;
end;

{ TCWAxisGraph ----------------------------------------------------- }

constructor TCWAxisGraph.Create(AOwner: TComponent);
begin
  FAnimationSpeed := asMediumFast;
  inherited Create(AOwner);
end;

procedure TCWAxisGraph.AssignGraph(Source: TCWGraph);
begin
  if Source is TCWAxisGraph then
  begin
    FStatLine := TCWAxisGraph(Source).StatLine;
    FMaxPointSpacing := TCWAxisGraph(Source).MaxPointSpacing;
  end;
  inherited;
end;

function TCWAxisGraph.SeriesCount : integer;
var
  i : integer;
  D, D2: TCWSeriesDef;
begin
  {Returns the number of entries in Chart.SeriesDefs using the same axis}
  Result := 0;
  if not InChart then
    Exit;
  D := nil;
  for I := 0 to Writer.Chart.SeriesDefs.Count-1 do
  begin
    if (Writer.Chart.SeriesDefs[i].Graph = self) and
    Writer.Chart.SeriesDefs[i].Visible then
    begin
      D := Writer.Chart.SeriesDefs[i];
      Break;
    end
  end;
  if D <> nil then
  for I := 0 to Writer.Chart.SeriesDefs.Count-1 do
  begin
     D2 := Writer.Chart.SeriesDefs[i];
     if (D2.Graph = D.Graph) and (D2.ValueAxis = D.ValueAxis) and D2.Visible then
       Inc(Result);
  end;

end;

function TCWAxisGraph.QuerySpace : integer;
begin
   Result := 0;
   if Writer = nil then
    ShowGWError(msg_WriterNotAssigned);
   if Writer.Chart = nil then
    ShowGWError(msg_NoActiveChart);
   if Writer.Count = 0 then
    ShowGWError(msg_DataEmpty);
   if not InChart then
    ShowGWError(msg_GraphNotInChart);
end;

function TCWAxisGraph.GetValueAxis : TCWValueAxis;
var
  Ser : integer;
begin
  Result := nil;
  if InChart then
  begin
     Ser := Writer.Chart.SeriesDefs.IndexOf(Self);
     if Writer.Chart.SeriesDefs[Ser].FValueAxis = vaValueAxis1 then
       Result := Writer.Chart.ValueAxis1
     else
       Result := Writer.Chart.ValueAxis2;
  end;
end;


procedure TCWAxisGraph.SetAnimations(Value: TAnimations);
begin
  if Self is TCWBar then
    with Self as TCWBar do
      if (anFlow in Value) and (FLayout = blStacked) and
        not(csLoading in componentState) then
        ShowGWError(msg_FlowSideBySide);
  FAnimations := Value;
end;

procedure TCWAxisGraph.SetAnimationBooster(Value: integer);
begin
  if (Value < -5) or (Value > 5) then
    Exit;
  FAnimationBooster := Value;
end;

procedure TCWAxisGraph.SetMaxPointSpacing(Value: integer);
begin
  if (Value = FMaxPointSpacing) or (Value < 0) or (Value > 100) then
    Exit;
  if Self is TCWBar then
    Exit;
  FMaxPointSpacing := Value;
  if Writer = nil then
    Exit;
  if not(csLoading in componentState)
  then
    with Writer do
    begin
      if Scrollable then
      begin
        FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
        ScrollTo(Writer.FScrollIndex);
      end
      else
      begin
        RefreshGraph;
      end;
    end;
end;

procedure TCWAxisGraph.SetStatLine(Value: TStatLine);
begin
  if Value = FStatLine then
    Exit;
  FStatLine := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

{ TCWSeriesStyle --------------------------------------------------------- }

constructor TCWSeriesStyle.Create(Collection: TCollection);
begin
  FLineWidth := 1;
  inherited;
end;

procedure TCWSeriesStyle.Assign(Source: TPersistent);
var
  wSrc: TCWSeriesStyle;
begin
  if Source is TCWSeriesStyle then
  begin
    wSrc := TCWSeriesStyle(Source);
    FLineStyle := wSrc.LineStyle;
    FLineWidth := wSrc.LineWidth;
    FStyle := wSrc.Style;
    FSeriesTitle := wSrc.SeriesTitle;
  end
  else
    inherited;
end;

function TCWSeriesStyle.GetDisplayName: string;
begin
  if FSeriesTitle <> '' then
    Result := FSeriesTitle
  else
    Result := 'Unnamed';
end;

procedure TCWSeriesStyle.SetStyle(Value: TCurveStyle);
begin
  FStyle := Value;
  if TCWCurve(Collection.Owner).Writer <> nil then
    TCWCurve(Collection.Owner).Writer.DoRepaint;
end;

procedure TCWSeriesStyle.SetLineStyle(Value: TCurvelineStyle);
begin
  FLineStyle := Value;
  if TCWCurve(Collection.Owner).Writer <> nil then
    TCWCurve(Collection.Owner).Writer.DoRepaint;
end;

procedure TCWSeriesStyle.SetLineWidth(Value: integer);
begin
  if Value < 1 then
    Exit;
  FLineWidth := Value;
  if TCWCurve(Collection.Owner).Writer <> nil then
    TCWCurve(Collection.Owner).Writer.DoRepaint;
end;

procedure TCWSeriesStyle.SetSeriesTitle(Value: string);
var
  i: integer;
begin
  if Trim(Value) = '' then
  begin
    FSeriesTitle := '';
    Exit;
  end;
  for i := 0 to Collection.Count - 1 do
  begin
    if i <> Index then
    begin
      if SameText(Value, TCWSeriesStyles(Collection).Items[i].FSeriesTitle) then
        ShowGWError(msg_DupSerName, '(' + Value + ')');
    end;
  end;
  FSeriesTitle := Value;
end;

{ TCWSeriesStyles ------------------------------------------------ }

function TCWSeriesStyles.GetItem(AIndex: integer): TCWSeriesStyle;
begin
  Result := TCWSeriesStyle(inherited Items[AIndex]);
end;

procedure TCWSeriesStyles.SetItem(AIndex: integer; const Value: TCWSeriesStyle);
begin
  inherited SetItem(AIndex, Value);
end;

function TCWSeriesStyles.IndexOf(const ASeriesTitle: string): integer;
var
  i: integer;
begin
  Result := -1;
  if Trim(ASeriesTitle) = '' then
    Exit;
  for i := 0 to Count - 1 do
  begin
    if SameText(ASeriesTitle, Items[i].SeriesTitle) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TCWSeriesStyles.Add: TCWSeriesStyle;
begin
  Result := TCWSeriesStyle(inherited Add);
end;

procedure TCWSeriesStyles.Assign(Source: TPersistent);
var
  wSrc: TCWSeriesStyles;
  loop: integer;
begin
  if (Source is TCWSeriesStyles) then
  begin
    wSrc := TCWSeriesStyles(Source);
    Clear;
    for loop := 0 to wSrc.Count - 1 do
      Add.Assign(wSrc.Items[loop]);
  end
  else
    inherited;
end;

function TCWSeriesStyles.GetStyle(ASeries: TSeries): TCWSeriesStyle;
var
  i: integer;
begin
  Result := nil;
  if not TCWCurve(Owner).UseSeriesStyles then
    Exit;

  if Trim(ASeries.Title) = '' then
  begin
    if Count >= TCWCurve(Owner).Writer.Count then
      Result := Items[ASeries.Index];
    Exit;
  end;
  for i := 0 to Count - 1 do
  begin
    if SameText(ASeries.Title, Items[i].SeriesTitle) then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

{ TCWColor ------------------------------------------------------- }

constructor TCWColor.Create(Collection: TCollection);
begin
  inherited;
end;

procedure TCWColor.SetColor(Value: TColor);
var
  C : TCWChart;
begin
  if FColor = Value then
    Exit;
  FColor := Value;
  if Collection.Owner is TCWChart then
    C := Collection.Owner as TCWChart
  else
    C := nil;
  if (C <> nil) and (C.Writer <> nil) and not (csLoading in C.componentState) then
  begin
     C.Writer.DoRepaint;
  end;
end;

function TCWColor.GetColor: TColor;
begin
    Result := ColorToRgb(FColor);
end;

function TCWColor.GetDisplayName: string;
begin
  if FItemName = '' then
    Result := 'Unnamed: ' + ColorToString(FColor)
  else
    Result := FItemName + ': ' + ColorToString(FColor);
end;

procedure TCWColor.SetItemName(Value: string);
var
  i: integer;
begin
  if Trim(Value) = '' then
  begin
    FItemName := '';
    Exit;
  end;
  for i := 0 to Collection.Count - 1 do
  begin
    if i <> Index then
    begin
      if SameText(Value, TCWColors(Collection).Items[i].FItemName) then
        ShowGWError(msg_DupSerName);
    end;
  end;
  FItemName := Value;
  if TCWChart(Collection.Owner).IsActive then
  begin
    if csDesigning in TCWChart(Collection.Owner).ComponentState then
      TCWChart(Collection.Owner).Writer.RenderDesigner()
    else
      TCWChart(Collection.Owner).Writer.DoRepaint;
  end;
end;

procedure TCWColor.Assign(Source: TPersistent);
var
  wSrc: TCWColor;
begin
  if Source is TCWColor then
  begin
    wSrc := TCWColor(Source);
    FColor := wSrc.Color;
    FItemName := wSrc.FItemName;
  end
  else
    inherited;
end;

{ TCWColors------------------------------------------------------- }

function TCWColors.GetItem(AIndex: integer): TCWColor;
begin
  Result := TCWColor(inherited Items[AIndex]);
end;

procedure TCWColors.SetItem(AIndex: integer; const Value: TCWColor);
begin
  inherited SetItem(AIndex, Value);
end;

procedure TCWColors.Update(Item : TCollectionItem);
begin
  if (csDesigning in TCWChart(Owner).ComponentState)
  and TCWChart(Owner).IsActive then
   TCWChart(Owner).Writer.RenderDesigner;
end;

function TCWColors.Add: TCWColor;
begin
  Result := TCWColor(inherited Add);
end;

procedure TCWColors.Assign(Source: TPersistent);
var
  wSrc: TCWColors;
  loop: integer;
begin
  if (Source is TCWColors) then
  begin
    wSrc := TCWColors(Source);
    Clear;
    for loop := 0 to wSrc.Count - 1 do
      Add.Assign(wSrc.Items[loop]);
  end
  else
    inherited;
end;

function TCWColors.IndexOf(const ItemName: string): integer;
var
  i: integer;
begin
  Result := -1;
  if Trim(ItemName) = '' then
    Exit;
  for i := 0 to Count - 1 do
  begin
    if SameText(ItemName, Items[i].ItemName) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TCWColors.GetColor(const ItemName: string): TColor;
var
  Indx: integer;
begin
  Result := clBlack;
  Indx := IndexOf(ItemName);
  if Indx <> -1 then
    Result := Items[Indx].Color;
end;

procedure TCWColors.SetColor(const ItemName: string; AColor: TColor);
var
  Indx: integer;
  c: TCWColor;
begin
  Indx := -1;
  if Trim(ItemName) <> '' then
    Indx := IndexOf(ItemName);
  if Indx <> -1 then
    Items[Indx].Color := AColor
  else
  begin
    c := Add;
    c.FItemName := ItemName;
    c.FColor := AColor;
  end;
  with Owner as TCWGraph do
    Writer.DoRepaint;
end;

{ TCWCurve  ------------------------------------------------ }

constructor TCWCurve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSeriesStyles := TCWSeriesStyles.Create(Self, TCWSeriesStyle);
  FAreaBrush := TBrush.Create;
  FAreaBrush.OnChange := BrushChanged;
  FLineWidth := 1;
  FPointWidth := 5;
  FPointMarkers := pmNone;
  FMinPointSpacing := 2;
  FSmoothLines := True;
  FBeaconIndex := -1;
end;

destructor TCWCurve.Destroy;
begin
  FAreaBrush.Free;
  FSeriesStyles.Free;
  inherited;
end;

function TCWCurve.QuerySpace : integer;
var
 MinSpace : integer;
 W : integer;
 Cnt : integer;
begin
   Result := inherited;
   MinSpace := MinPointSpacing;
   Cnt := Writer.LongestSeries.Count;
   if Writer.FNamAx.IsXAxis then
     W := Writer.GraphPrintRect.Width
   else
     W := Writer.GraphPrintRect.Height;
   if MinSpace * Cnt > W  then
   begin
     MinSpace := MinSpace-1;
     while (MinSpace * Cnt > W) and (Minspace > 0)  do
     begin
        Dec(MinSpace);
     end;
     if MinSpace = 0 then
       Result := -1
     else if MinPointSpacing > MinSpace then
       Result := MinSpace;
   end;
end;

procedure TCWCurve.AssignGraph(Source: TCWGraph);
begin
  if Source is TCWCurve then
  begin
    FBeaconIndex := TCWCurve(Source).FBeaconIndex;
    FStyle := TCWCurve(Source).Style;
    FLineStyle := TCWCurve(Source).LineStyle;
    FLineWidth := TCWCurve(Source).LineWidth;
    FBaseLineValue := TCWCurve(Source).BaseLineValue;
    FPointMarkers := TCWCurve(Source).PointMarkers;
    FPointWidth := TCWCurve(Source).PointWidth;
    FMinPointSpacing := TCWCurve(Source).MinPointSpacing;
    { Minimun unit space before Contraction takes place }
    FAreaBrush := TBrush.Create;
    FAreaBrush.Assign(TCWCurve(Source).AreaBrush);
    FBeaconPoints := TCWCurve(Source).BeaconPoints;
    FSeriesStyles := TCWSeriesStyles.Create(Self, TCWSeriesStyle);
    FSeriesStyles.Assign(TCWCurve(Source).SeriesStyles);
  end;
  inherited;
end;

procedure TCWCurve.BrushChanged(Sender: TObject);
begin
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetStyle(Value: TCurveStyle);
begin
  if (csLoading in componentState) then
  begin
    FStyle := Value;
    Exit;
  end;
  if Value = FStyle then
    Exit;
  FStyle := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetLineStyle(Value: TCurvelineStyle);
begin
  if (csLoading in componentState) then
  begin
    FLineStyle := Value;
    Exit;
  end;
  if FLineStyle = Value then
    Exit;
  FLineStyle := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetLineWidth(Value: integer);
begin
  if (csLoading in componentState) then
  begin
    FLineWidth := Value;
    Exit;
  end;
  if (LineWidth = Value) or (Value < 1) then
    Exit;
  FLineWidth := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetStep(Value : Boolean);
begin
  if (csLoading in componentState) then
  begin
    FStep := Value;
    Exit;
  end;
  if (FStep = Value) then
    Exit;
  FStep := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWCurve.GetStyle: TCurveStyle;
begin
  Result := FStyle;
end;

function TCWCurve.GetLineStyle: TCurvelineStyle;
begin
  Result := FLineStyle;
end;

function TCWCurve.GetLineWidth: integer;
begin
  Result := FLineWidth;
end;

function TCWCurve.GetActiveStyle(ASeries: TSeries): TCurveStyle;
var
  Indx: integer;
begin
  Indx := GetStyleIndex(ASeries);
  if Indx <> -1 then
    Result := SeriesStyles.Items[Indx].FStyle
  else
    Result := FStyle;
end;

function TCWCurve.GetActiveLineStyle(ASeries : TSeries): TCurvelineStyle;
var
  Indx: integer;
begin
  Indx := GetStyleIndex(ASeries);
  if Indx <> -1 then
    Result := SeriesStyles.Items[Indx].FLineStyle
  else
    Result := FLineStyle;
end;

function TCWCurve.GetActiveLineWidth(ASeries: TSeries): integer;
var
  Indx: integer;
begin
  Indx := GetStyleIndex(ASeries);
  if Indx <> -1 then
    Result := SeriesStyles.Items[Indx].FLineWidth
  else
    Result := FLineWidth;
end;

procedure TCWCurve.SetActiveStyle(ASeries: TSeries; Value: TCurveStyle);
var
  Indx: integer;
begin
  if Value = ActiveStyle[ASeries] then
    Exit;
  Indx := GetStyleIndex(ASeries);
  if Indx <> -1 then
    SeriesStyles.Items[Indx].FStyle := Value
  else
    FStyle := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetActiveLineStyle(ASeries : TSeries; Value: TCurvelineStyle);
var
  Indx: integer;
begin
  if ActiveLineStyle[ASeries] = Value then
  begin
    Exit;
  end;
  Indx := GetStyleIndex(ASeries);
  if Indx <> -1 then
  begin
      SeriesStyles.Items[Indx].FLineStyle := Value;
  end
  else
  begin
    FLineStyle := Value;
  end;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetActiveLineWidth(ASeries: TSeries; Value: integer);
var
  Indx: integer;
begin
  if (ActiveLineWidth[ASeries] = Value) or (Value < 1) or (Value > 3) then
    Exit;
  Indx := GetStyleIndex(ASeries);
  if Indx <> -1 then
    SeriesStyles.Items[Indx].FLineWidth := Value
  else
    FLineWidth := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWCurve.GetUseSeriesStyles: Boolean;
begin
  Result := SeriesStyles.Count > 0;
end;

function TCWCurve.GetStyleIndex(ASeries: TSeries): integer;
var
 S : string;
begin
  Result := -1;
  if (ASeries = nil) then
    Exit;
  begin
    S := Writer.Chart.SeriesDefs[ASeries.Index].FTitle;
    if S <> '' then
    begin
      Result := SeriesStyles.IndexOf(S);
    end
    else
    begin
      if SeriesStyles.Count >= Writer.Count then
        Result := ASeries.Index;
    end;
  end;
end;

procedure TCWCurve.SetBaseLineValue(Value: single);
begin
  if (FBaseLineValue = Value) then
    Exit;
  FBaseLineValue := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetAreaOutlineColor(Value : TColor);
begin
  if (FAreaOutlineColor = Value) then
    Exit;
  FAreaOutlineColor := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetAreaOutline(Value : Boolean);
begin
  if (FAreaOutline = Value) then
    Exit;
  FAreaOutline := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetSmoothLines(Value : Boolean);
begin
  if (FSmoothLines = Value) then
    Exit;
  FSmoothLines := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetPointMarkers(Value: TPointMarkers);
begin
  if Value = FPointMarkers then
    Exit;
  FPointMarkers := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetPointWidth(Value: integer);
begin
  if (Value < 3) then
    Exit;
  FPointWidth := Value;
end;

procedure TCWCurve.SetMinPointSpacing(Value: integer);
begin
  if Value = FMinPointSpacing then
    Exit;
  if (Value < 0) or (Value > 50) then
    Exit;
  FMinPointSpacing := Value;
  if Writer = nil then
    Exit;
  if not(csLoading in componentState)
  then
  begin
    if Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.ScrollTo(Writer.FScrollIndex);
    end
    else
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

function TCWCurve.GetMinPointSpacing;
begin
  Result := FMinPointSpacing;
end;

procedure TCWCurve.SetBeaconPoints(Value: Boolean);
begin
  if Value = FBeaconPoints then
    Exit;
  FBeaconPoints := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWCurve.GetActiveColor(SeriesIndex, ItemIndex : integer): TColor;
begin
  Result := Writer.Chart.FSeriesDefs.Items[SeriesIndex].Color;
end;

function TCWCurve.BeaconsActive: Boolean;
begin
  Result := FBeaconPoints and (Style = csLine) and
    (Writer.ViewMode = vmHinting);
end;

function TCWCurve.GetMaxPointSpacing: integer;
begin
  Result := 0;
  if not((Writer = nil) or (Writer.Scrollable)) then
    Result := FMaxPointSpacing;
end;

procedure TCWCurve.DrawTheLine(ASource : TSeries; Indx: integer;
 x1, y1, x2, y2: integer; StatLine : Boolean = false);
var
   Clr: TColor;
   CStyl: TCurvelineStyle;
   W: integer;
begin
    if Indx = -1 then
      CStyl := lsDot
    else
      CStyl := ActiveLineStyle[ASource];
    W := ActiveLineWidth[ASource];
    if FAreaOutline and (ActiveStyle[Asource] = csClientArea) then
      Clr := FAreaOutlineColor
    else
      Clr := ActiveColor[ASource.Index, -1];
    FGPen.SetColor(MakeGPClr(clr));
    FGPen.SetWidth(W);
    if CStyl = lsDot then
      FGPen.SetDashStyle(DashStyle.DashStyleDot)
    else if CStyl = lsDash then
      FGPen.SetDashStyle(DashStyle.DashStyleDash)
    else
      FGPen.SetDashStyle(DashStyle.DashStyleSolid);

    if Step and not StatLine and not (ActiveStyle[Asource] = csNeighborArea)then
    begin
      GDIP.DrawLine(FGPen,x1, y1, x2, y1);
      GDIP.DrawLine(FGPen,x2, y1, x2, y2);
    end
    else
    begin
      if SmoothLines then
       GDIP.DrawLine(FGPen,x1, y1, x2, y2 )
      else
      begin
        Canvas.Pen.Width := W;
        Canvas.Pen.Color := Clr;
        if CStyl = lsDot then
          Canvas.Pen.Style := psDot
        else if CStyl = lsDash then
          Canvas.Pen.Style := psDash
        else
          Canvas.Pen.Style := psSolid;
        Canvas.MoveTo(x1, y1);
        Canvas.LineTo(x2, y2);
      end;
    end;
end;

procedure TCWCurve.Draw;
type
  TOrigProps = record
    LWidth: integer;
    LStyl: TCurvelineStyle;
    CStyl: TCurveStyle;
    Clr: TColor;
    Styl: TCWSeriesStyle;
    NeedsRestore: Boolean;
  end;

var
  i: integer;
  OrigProps: TOrigProps;
  Anim: Boolean;
  SeriesFinished: Boolean;
  loop: integer;
  LoopCnt: integer;

  procedure DoDraw(ASource: TSeries);
  var
    i: integer;
    P: TPoint;
    LastPt: TPoint;
    S: string;
    R: TRect;
    Sz: TSize;
    Cs: TCurveStyle;
    Stp : Boolean;
    SerInd, ItmInd: integer;
    Styl: TCurvelineStyle;
    Points: TPointArray;
    IsHandled: Boolean;

    procedure SetOrigProps;
    begin
      if OrigProps.NeedsRestore then
        Exit;
      OrigProps.Clr := ActiveColor[ASource.Index, -1];
      OrigProps.Styl := SeriesStyles.GetStyle(ASource);
      if OrigProps.Styl <> nil then
      begin
        OrigProps.LStyl := OrigProps.Styl.FLineStyle;
        OrigProps.LWidth := OrigProps.Styl.FLineWidth;
        OrigProps.CStyl := OrigProps.Styl.FStyle;
      end
      else
      begin
        OrigProps.LStyl := LineStyle;
        OrigProps.LWidth := LineWidth;
        OrigProps.CStyl := Style;
      end;
      OrigProps.NeedsRestore := True;
    end;

    procedure RestoreOrigProps;
    begin
      if not OrigProps.NeedsRestore then
        Exit;
      Writer.Chart.FSeriesDefs.Items[ASource.Index].FColor := OrigProps.Clr;
      if OrigProps.Styl <> nil then
      begin
        OrigProps.Styl.FLineStyle := OrigProps.LStyl;
        OrigProps.Styl.FLineWidth := OrigProps.LWidth;
        OrigProps.Styl.FStyle := OrigProps.CStyl;
      end
      else
      begin
        FLineStyle := OrigProps.LStyl;
        FLineWidth := OrigProps.LWidth;
        FStyle := OrigProps.CStyl;
      end;
      OrigProps.NeedsRestore := False;
    end;

    procedure DrawArea;
    var
      i: integer;
      Pts: array of TPoint;
      Cnt: integer;
      BaseLine: integer;
      BV: single;

    begin
      if Stp then
        setlength(Pts, (ASource.LastItem - ASource.FirstItem) * 2 + 3)
      else
       setlength(Pts, ASource.LastItem - ASource.FirstItem + 1 + 3);
      Pts[0] := P;
      LastPt := P;
      if Cs = csBaseLineArea then
      begin
        BV := BaseLineValue;
        begin
          if (BV < Writer.ValueLow) or (BV > Writer.ValueHigh) then
            BV := MaxInt;
        end;
        if BV <> MaxInt then
          BaseLine := Writer.PosFromValue(BV, Writer.ValAxFromGraph(Self))
        else
        begin
          if Writer.FNamAx.IsXAxis then
            BaseLine := Writer.GetGraphPrintRect.Bottom
          else
            BaseLine := Writer.GetGraphPrintRect.Left;
        end;
      end
      else
      begin
        if Writer.FNamAx.IsXAxis then
          BaseLine := Writer.GetGraphPrintRect.Bottom
        else
          BaseLine := Writer.GetGraphPrintRect.Left
      end;
      try
        Cnt := 1;
        for i := ASource.FirstItem + 1 to ASource.LastItem do
        begin
          if Stp then
          begin
            P.X := Points[i - ASource.FirstItem].X;
            Pts[Cnt] := P;
            Inc(Cnt);
          end;
          P := Points[i - ASource.FirstItem];
          Pts[Cnt] := P;
          inc(Cnt);
        end;
        if Writer.FNamAx.IsXAxis then
        begin
          Pts[High(Pts) - 2] := Point(P.X, BaseLine);
          Pts[High(Pts) - 1] := Point(Pts[0].X, BaseLine);
        end
        else
        begin
          Pts[High(Pts) - 2] := Point(BaseLine, P.Y);
          Pts[High(Pts) - 1] := Point(BaseLine, Pts[0].Y);
        end;
        Pts[High(Pts)] := Pts[0];

        Canvas.Brush.Assign(AreaBrush);
        Canvas.Brush.Color := ActiveColor[ASource.Index, -1];
        Canvas.Pen.Color := Canvas.Brush.Color;

        Canvas.Polygon(Pts);
        P := Points[0];
        LastPt := P;
        for i := 1 to Points.Count - 1 do
        begin
          P := Points[i];
          DrawTheLine(Asource, 0, LastPt.X, LastPt.Y, P.X, P.Y);
          DrawTheLine(ASource, 0, LastPt.X + 1, LastPt.Y, P.X + 1, P.Y);
          LastPt := P;
        end;

        if (veBaseLine in Writer.AxisElements) and (Cs = csBaseLineArea) then
        begin
          Canvas.Pen.Color := clBlack;
          if Writer.FNamAx.IsXAxis then
          begin
            Canvas.MoveTo(Writer.GetGraphPrintRect.Left, BaseLine);
            Canvas.LineTo(Writer.GetGraphPrintRect.Right, BaseLine);
          end
          else
          begin
            Canvas.MoveTo(BaseLine, Writer.GetGraphPrintRect.Top);
            Canvas.LineTo(BaseLine, Writer.GetGraphPrintRect.Bottom);
          end
        end;
      finally

      end;
      Writer.ResetCanvas;
    end;

    function DoLineEvent(LineType: TLineType; ItemIndex: integer;
      Event: TDrawCurveLineEvent; StartPoint, EndPoint: TPoint): Boolean;
    begin
      if Writer.InState(stAnimating) then
        Exit;
      Result := False;
      if not Assigned(Event) or
        ((Assigned(FOnDrawCurve)) and (LineType = ltCurve))
      then { Cannot do both }
        Exit;
      SetOrigProps;
      Event(ASource, LineType, Point(StartPoint.X, StartPoint.Y),
        Point(EndPoint.X, EndPoint.Y), ASource.Index, ItemIndex,
        Canvas, Result);
    end;

    procedure DrawThePoints(AIndex: integer);
    var
      PIndx: integer;
      Clr: TColor;
    begin
      PIndx := AIndex - ASource.FirstItem;
      if (PointMarkers <> pmNone) and (ActiveStyle[ASource] in [csLine, csPoints]) then
      begin
        if BeaconPoints and (FBeaconIndex = -1) then
        begin
          if not Writer.DoPosInPoint(Writer.FRulerX, Writer.FRulerY,
            Writer.MousePrecision, SerInd, ItmInd) then
            Exit;
          if not(SerInd = ASource.Index) or not(AIndex = ItmInd) then
            Exit;
        end;
        if PointMarkers = pmOwnerDraw then
        begin
          if Assigned(FOnDrawPoint) then
          begin
            FOnDrawPoint(Self, ASource.Index, AIndex, Points[PIndx], Canvas);
          end;
        end
        else if PointMarkers = pmBigBall then
          Canvas.Draw(Points[PIndx].X - 4, Points[PIndx].Y - 4, Writer.FBall)
        else if PointMarkers = pmSmallBall then
          Canvas.Draw(Points[PIndx].X - 3, Points[PIndx].Y - 3,
            Writer.FBallSmall)
        else if PointMarkers = pmText then
        begin
          S := FormatNum(ASource.FSeriesItems[AIndex].Value,
            Writer.ValuePrecision);
          Sz := Canvas.TextExtent(S);
          Clr := ActiveColor[ASource.Index, -1];
          R.Left := Points[PIndx].X;
          R.Right := Points[PIndx].X + Sz.cx;
          R.Top := Points[PIndx].Y - (Sz.cy div 2) - 6;
          R.Bottom := Points[PIndx].Y + (Sz.cy div 2) - 6;
          Canvas.Font.Color := Clr;
          Canvas.Pen.Color := clBlack;
          Canvas.Brush.Style := bsClear;
          Canvas.Brush.Color := Writer.GraphBGColor;
          Canvas.TextRect(R, R.Left, R.Top, S);
          Canvas.Pen.Color := Clr;
        end

        else
        begin
          Clr := ActiveColor[ASource.Index, -1];
          Canvas.Pixels[Points[PIndx].X - 1, Points[PIndx].Y - 1] := Clr;
          Canvas.Pixels[Points[PIndx].X, Points[PIndx].Y - 1] := Clr;
          Canvas.Pixels[Points[PIndx].X + 1, Points[PIndx].Y - 1] := Clr;

          Canvas.Pixels[Points[PIndx].X - 1, Points[PIndx].Y] := Clr;
          Canvas.Pixels[Points[PIndx].X, Points[PIndx].Y] := Clr;
          Canvas.Pixels[Points[PIndx].X + 1, Points[PIndx].Y] := Clr;

          Canvas.Pixels[Points[PIndx].X - 1, Points[PIndx].Y + 1] := Clr;
          Canvas.Pixels[Points[PIndx].X, Points[PIndx].Y + 1] := Clr;
          Canvas.Pixels[Points[PIndx].X + 1, Points[PIndx].Y + 1] := Clr;
        end;
      end;
    end;

    procedure SetStatLinePen;
    begin
      if ActiveLineStyle[ASource] = lsSolid then
        Canvas.Pen.Style := psSolid
      else if ActiveLineStyle[ASource] = lsDot then
        Canvas.Pen.Style := psDot
      else
        Canvas.Pen.Style := psDash;
      Canvas.Pen.Width := ActiveLineWidth[ASource];
      Canvas.Pen.Color := ActiveColor[ASource.Index, -1];
    end;

    procedure DrawRegressionLine;
    var
      xzero, xmax: extended;
      P: TPoint;
      pStart, PEnd: TPoint;
      i: integer;
      X, Y: real;
      Data: TRealPointArray;
      ScaledData: TIntPointArray;
      RP: trealpoint;
      SI: tScaleInfo;
      M, B, R: extended;
      LocalGR: TRect;
      YMin, YMax: integer;

    begin
      if not((Writer.IsTimeSpan(Writer.Chart.NameType) or
        (Writer.Chart.NameType = ntNumberSpan)) and (ActiveStyle[ASource] = csLine))
      then
        Exit;
      if not Writer.FNamAx.IsXAxis then
        Exit;

      YMin := MaxInt;
      YMax := -MaxInt;

      for i := 0 to Points.Count - 1 do
      begin
        if Points[i].Y < YMin then
          YMin := Points[i].Y;
        if Points[i].Y > YMax then
          YMax := Points[i].Y;
      end;

      LocalGR := Rect(Writer.GetGraphPrintRect.Left, YMin,
        Writer.GetGraphPrintRect.Right, YMax);

      setlength(Data, ASource.ItemCount);
      for i := 0 to Points.Count - 1 do
      begin
        X := Points[i].X - Writer.GetGraphPrintRect.Left;
        Y := Points[i].Y - YMin;
        Y := LocalGR.Height - Y;
        RP.X := X;
        RP.Y := Y;
        Data[i] := RP;
      end;
      LinearLeastSquares(Data, M, B, R);
      SI := ScaledataForPlot(Data, LocalGR.Width, LocalGR.Height, ScaledData);

      // Dist := Writer.GraphRect.Top;
      { for I := 0 to High(ScaledData) do
        (*Testing*)
        begin
        if I > 0 then
        begin
        X1 := ScaledData[i-1].X;
        Y1 := ScaledData[i-1].Y;
        X2 := ScaledData[i].X;
        Y2 := ScaledData[i].Y;
        DrawTheLine(-1, X1, Y1, X2, Y2);
        end;
        end; }

      Canvas.Pen.Color := ActiveColor[ASource.Index, -1];
      with SI do
      begin
        xzero := -Offsetx / ScaleX;
        P := scalePoint(xzero, M * xzero + B, SI);
        pStart.X := P.X + Writer.GetGraphPrintRect.Left;
        pStart.Y := P.Y + YMin;
        xmax := (Writer.GetGraphPrintRect.Width - Offsetx) / ScaleX;
        P := scalePoint(xmax, M * xmax + B, SI);
        PEnd.X := P.X + Writer.GetGraphPrintRect.Left;
        PEnd.Y := P.Y + YMin;
        if not DoLineEvent(ltRegression, -1, FOnDrawStatLine, pStart, PEnd) then
          DrawTheLine(ASource, -1, pStart.X, pStart.Y, PEnd.X, PEnd.Y, True);
      end;
    end;

    procedure DrawMeanLine;
    var
      MeanLine: single;
      P: integer;
      R: TRect;
    begin
      R := Writer.GetGraphPrintRect;
      if not Writer.IsTimeSpan(Writer.Chart.NameType) and
      not (Writer.Chart.NameType = ntNumberSpan)
      then
        Exit;
      if not Writer.FNamAx.IsXAxis then
        Exit;
      MeanLine := ASource.Avg;
      P := Writer.PosFromValue(MeanLine, Writer.ValAxFromGraph(Self));
      SetStatLinePen;
      if not DoLineEvent(ltMean, -1, FOnDrawStatLine, Point(R.Left, P),
        Point(R.Right, P)) then
      begin
        Canvas.MoveTo(R.Left, P);
        Canvas.LineTo(R.Right, P);
        Canvas.Font.Color := ActiveColor[ASource.Index, -1];
        Canvas.Brush.Color := Writer.GraphBGColor;
        Canvas.TextOut(R.Left + 3, P + 3, FormatNum(MeanLine, 1));
      end;
    end;

    procedure DrawMedianLine;
    var
      MedianLine: single;
      P: integer;
      R: TRect;
    begin
      R := Writer.GetGraphPrintRect;
      if not((Writer.IsTimeSpan(Writer.Chart.NameType) or
        (Writer.Chart.NameType = ntNumberSpan)) and (ActiveStyle[ASource] = csLine))
      then
        Exit;
      if not Writer.FNamAx.IsXAxis then
        Exit;
      MedianLine := ASource.Median;
      P := Writer.PosFromValue(MedianLine, Writer.ValAxFromGraph(Self));

      SetStatLinePen;
      if not DoLineEvent(ltMedian, -1, FOnDrawStatLine, Point(R.Left, P),
        Point(R.Right, P)) then
      begin
        Canvas.MoveTo(R.Left, P);
        Canvas.LineTo(R.Right, P);
        Canvas.Font.Color := ActiveColor[ASource.Index, -1];
        Canvas.Brush.Color := Writer.GraphBGColor;
        Canvas.TextOut(R.Left + 3, P + 3, FormatNum(MedianLine, 1));
      end;
    end;

    procedure DrawModeLines;
    var
      Nums: TModeNumbers;
      i: integer;
      P: integer;
      R: TRect;
    begin
      R := Writer.GetGraphPrintRect;
      Nums := ASource.Mode;
      SetStatLinePen;
      for i := 0 to High(Nums) do
      begin
        P := Writer.PosFromValue(StrToFloat(Nums[i].Number, Fmt),
          Writer.ValAxFromGraph(Self));
        if not DoLineEvent(ltMode, -1, FOnDrawStatLine, Point(R.Left, P),
          Point(R.Right, P)) then
        begin
          Canvas.MoveTo(R.Left, P);
          Canvas.LineTo(R.Right, P);
          Canvas.Font.Color := ActiveColor[ASource.index, -1];
          Canvas.Brush.Color := Writer.GraphBGColor;
          Canvas.TextOut(R.Left + 3, P + 3, Nums[i].Number + '(' +
            IntToStr(Nums[i].Cnt) + ')');
        end;
      end;
    end;

    procedure DrawStatLines;
    begin
      Styl := ActiveLineStyle[ASource];
      try
        ActiveLineStyle[ASource] := lsDash;
        if slRegression = StatLine then
        begin
          DrawRegressionLine;
        end
        else if slMean = StatLine then
        begin
          DrawMeanLine;
        end
        else if slMedian = StatLine then
        begin
          DrawMedianLine;
        end
        else if slMode = StatLine then
        begin
          DrawModeLines;
        end;
        RestoreOrigProps;
      finally
        ActiveLineStyle[ASource] := Styl;
      end;
    end;

  begin
    RestoreOrigProps;
    if Anim and not(ActiveStyle[ASource] = csLine) and
      not(ActiveStyle[ASource] = csPoints) then
      Cs := csLine
    else
      Cs := ActiveStyle[ASource];
    if ActiveStyle[ASource] = csNeighborArea then
      Stp := false
    else
      Stp := Step;
    if (Cs = csBaseLineArea) and
      ((BaseLineValue < Writer.ValueLow) or (BaseLineValue > Writer.ValueHigh)) then
      Cs := csClientArea;

    if (cs = csNeighborArea) and ((Writer.VisibleCount < 2) or (Writer.Chart.GraphCount(Self) < 2)) then
      Cs := csLine;

    if Writer.GraphTypeInChart(TCWBar) then
      Points := ASource.FBarPoints
    else
      Points := ASource.FPoints;
    if (ASource.ItemCount = 0) or (Points.Count = 0) then
      Exit;
    if Assigned(FOnDrawCurve) then
    begin
      IsHandled := False;
      SetOrigProps;
      if not Anim then
        FOnDrawCurve(Self, ASource.Index, Canvas, IsHandled);
      if IsHandled then
      begin
        RestoreOrigProps;
        Writer.ResetCanvas;
        Exit;
      end;
    end;

    if (Cs = csNeighborArea) then
    { Drawn at the end of the paint prcess }
    begin
      setlength(FNeighborAreas, Length(FNeighborAreas) + 1);
      FNeighborAreas[High(FNeighborAreas)] := ASource.Index;
      Exit;
    end;
    if (FBeaconIndex <> -1) and not(Writer.FViewMode = vmSelecting) then
    begin
      DrawThePoints(FBeaconIndex);
      Exit;
    end;
    if Anim and (Writer.FAnimInfo.NextHorz > 0) then
      P := Writer.FAnimInfo.LastPt
    else
      P := Points[0];
    Canvas.MoveTo(P.X, P.Y);
    LastPt := P;

    if (Cs = csClientArea) or (Cs = csBaseLineArea) then
    begin
      DrawArea;
    end
    else
    begin
      for i := ASource.FirstItem + 1 to ASource.LastItem do
      begin
        if Writer.InState(stAnimating) and (i < Writer.FAnimInfo.NextHorz) then
          Continue;
        P := Points[i - ASource.FirstItem];
        if ASource.FSeriesItems[i].FLeapDummy then
        begin
          Continue;
        end;
        if not Assigned(FOnDrawCurve) and not Anim and not(Cs in [csPoints]) then
        begin
          if not DoLineEvent(ltCurve, i - ASource.FirstItem, FOnDrawCurveLine,
            LastPt, P) then
            DrawTheLine(ASource, i, LastPt.X, LastPt.Y, P.X, P.Y);
          RestoreOrigProps;
        end
        else if not(Cs in [csPoints]) then
          DrawTheLine(ASource, i, LastPt.X, LastPt.Y, P.X, P.Y);
        LastPt := P;
        DrawThePoints(i - 1);
        if Anim then
        begin
          inc(Writer.FAnimInfo.NextHorz);
          Writer.FAnimInfo.LastPt := P;
          if i = ASource.LastItem then
            SeriesFinished := True;
          Break;
        end;
      end;
    end;
    RestoreOrigProps;
    if (StatLine <> slNone) and not Anim then
    begin
      DrawStatLines;
    end;

    if (PointMarkers <> pmNone) and (ActiveStyle[ASource] = csLine) and
      not Writer.InState(stAnimating) then
      DrawThePoints(ASource.LastItem);
    Writer.ResetCanvas;
  end;

begin
  if not InChart then
    Exit;
  inherited;
  Anim := False;
  SeriesFinished := False;
  loop := 0;
  LoopCnt := Writer.Count - 1;
  if Writer.InState(stAnimating) then
  begin
    Anim := True;
    loop := Writer.FAnimInfo.NextSeries;
    LoopCnt := loop;
  end;
  OrigProps.NeedsRestore := False;
  Finalize(FNeighborAreas);
  if Writer <> nil then
  begin
    FGDIP := TGPGraphics.Create(Canvas.Handle);
    FGDIP.SetSmoothingMode(SmoothingModeAntiAlias);
    FGDIP.SetInterpolationMode(InterpolationMode.InterpolationModeHighQuality);
    FGDIP.SetCompositingQuality(CompositingQuality.CompositingQualityHighQuality);
    FGPEN := TGPPen.Create(0);
    try
    for i := loop to LoopCnt do
    begin
      if not InChart(i) then
        Continue;
      if Writer.Chart.FSeriesDefs.Items[i].Graph <> nil then
        if (Writer.Series[i].Visible) and (Writer.Chart.FSeriesDefs.Items[i].Graph = Self) then
        begin
          DoDraw(Series[i]);
        end;
      if Anim and SeriesFinished then
      begin
        Writer.FAnimInfo.NextHorz := 0;
        inc(Writer.FAnimInfo.NextSeries);
        if (i = Writer.Count - 1) then
          Writer.ClearState(stAnimating);
        Break;
      end;
    end;
    if not Anim then
      DoDrawNeighbors;
    finally
      FGDIP.Free;
      FGPEN.Free;
    end;
  end;

end;

procedure TCWCurve.DoDrawNeighbors;
var
  i, j, Indx: integer;
  HasNeighborPoly: Boolean;
begin
  i := 0;
  Indx := 1;
  HasNeighborPoly := False;
  while True do
  begin
    for j := 0 to High(FNeighborAreas) do
    begin
      if i = FNeighborAreas[j] then
      begin
        HasNeighborPoly := True;
        Break;
      end;
    end;

    if not((Writer.Series[i].Visible) and HasNeighborPoly
     and (Writer.Series[i].Graph = Self))
    then
    begin
      inc(i);
      inc(Indx);
      if i > Writer.Count - 2 then
        Break
      else
        Continue;
    end;
    DrawNeighbors(i, i + 1, Indx);
    inc(i);
    inc(Indx);
    HasNeighborPoly := False;
    if i > Writer.Count - 2 then
      Break;
  end;
end;

procedure TCWCurve.DrawNeighbors(Serie1, Serie2, NeighborIndex: integer);
var
  tmp: integer;
  FirstIndx, LastIndx: integer;
  Pts, Pts1, Pts2: array of TPoint;
  Pt: TPoint;
  i: integer;
  Cnt: integer;
  Clr1, Clr2: TColor;
  GP1, GP2 : TPointArray;
begin
  if Writer = nil then
    Exit;
  if Writer.VisibleCount < 2 then
    Exit;
  if Writer.Chart.GraphCount(Self) < 2 then
    ShowGWError(msg_NeighborTwoSeries);
  if Writer.Chart.GraphCount(Self) > 3 then
    ShowGWError(msg_NeighborThreeSeries);
  if abs(Serie1 - Serie2) <> 1 then
    ShowGWError(msg_NotNeighborSeries);
  if Serie1 > Serie2 then
  begin
    tmp := Serie1;
    Serie1 := Serie2;
    Serie2 := tmp;
  end;
  if Writer.Series[Serie1].FIndent > Writer.Series[Serie2].FIndent then
    FirstIndx := Writer.Series[Serie1].FIndent
  else
    FirstIndx := Writer.Series[Serie2].FIndent;

  if Writer.Series[Serie1].FExdent > Writer.Series[Serie2].FExdent then
    LastIndx := Writer.Series[Serie1].LastItem
  else
    LastIndx := Writer.Series[Serie2].LastItem;
  setlength(Pts1, (LastIndx - FirstIndx + 1));
  setlength(Pts2, (LastIndx - FirstIndx + 1));
  Clr1 := ActiveColor[Serie1,-1];
  Clr2 := ActiveColor[Serie2,-1];
  i := -1;
  if Writer.InView(TCWBar, i) <> nil then
  begin
   GP1 :=  Writer.Series[Serie1].FBarPoints;
   GP2 :=  Writer.Series[Serie2].FBarPoints;
  end
  else
  begin
   GP1 :=  Writer.Series[Serie1].FPoints;
   GP2 :=  Writer.Series[Serie2].FPoints;
  end;

  for i := FirstIndx to LastIndx do
  begin
    Pt := GP1[i - Writer.Series[Serie1].FIndent];
    Pts1[i - Writer.Series[Serie1].FIndent] := Pt;
  end;
  Cnt := 0;
  for i := LastIndx downto FirstIndx do
  begin
    Pt := GP2[i - Writer.Series[Serie2].FIndent];
    Pts2[Cnt] := Pt;
    inc(Cnt);
  end;
  setlength(Pts, Length(Pts1) + Length(Pts2));

  for i := 0 to Length(Pts1) - 1 do
    Pts[i] := Pts1[i];

  for i := Length(Pts2) - 1 downto 0 do
    Pts[i + Length(Pts1)] := Pts2[i];

  Canvas.Brush.Assign(AreaBrush);
  if NeighborIndex = 1 then
    Canvas.Pen.Color := Clr1
  else
    Canvas.Pen.Color := Clr2;
  Canvas.Polygon(Pts);

  for i := 0 to High(Pts1) do
  begin
    if i > 0 then
    begin
      DrawTheLine(Writer.Series[Serie1], 0, Pts1[i - 1].X, Pts1[i - 1].Y, Pts1[i].X,
        Pts1[i].Y);
    end;
  end;

  for i := 0 to High(Pts2) do
  begin
    if i > 0 then
    begin
      DrawTheLine(Writer.Series[Serie2], 0, Pts2[i - 1].X, Pts2[i - 1].Y, Pts2[i].X,
        Pts2[i].Y);
    end;
  end;

  Writer.ResetCanvas;
end;

{ TCWBar ------------------------------------------------------------ }

constructor TCWBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarWidth := 20;
  FItemSpacing := 3;
  FScrollingBarWidth := 20;
  FAnimationBooster := 0;
  FLayout := blSideBySide;
  FTextContents := [tcValue];
  FCubeAngle := 45;
end;

destructor TCWBar.Destroy;
begin
  inherited;
end;

function TCWBar.Is3D: Boolean;
begin
  if (FBarStyle = bsFlat) or
    (FBarStyle in [bsGradientWidth, bsGradientLength]) then
    Result := False
  else if BarWidth <= 10 then
    Result := False
  else if (FBarStyle = bsCube) and (CubeAngle = 0) then
    Result := false
  else
    Result := FBarStyle in [bsCube, bsCylinder];
  if (Writer <> nil) and Result then
  begin
    Result := Writer.FNamAx.IsXAxis and not(boBaseLine in Options);
  end;
end;

function TCWBar.QuerySpace : integer;
var
  BWidth, W : integer;
  Cpr : Boolean;
begin
  Result := inherited;
  if BarWidth = 0 then
    Exit;
  BWidth := BarWidth;
  FBarWidth := 0;
  try
    if not GetBarSpace(W, Cpr, Result) then
    begin
      Result := -1;
      FBarWidth := BWidth;
      Exit;
    end
    else
    begin
      if Result > BWidth then
        Result := 0;

    end;
  finally
    FBarWidth := BWidth;
  end;

end;

procedure TCWBar.AssignGraph(Source: TCWGraph);
begin
  if Source is TCWBar then
  begin
    FBaseLineValue := TCWBar(Source).BaseLineValue;
    FLayout := TCWBar(Source).Layout;
    FItemSpacing := TCWBar(Source).ItemSpacing;
    FSeriesSpacing := TCWBar(Source).SeriesSpacing;
    FBarStyle := TCWBar(Source).BarStyle;
    FBarWidth := TCWBar(Source).BarWidth;
    FCubeAngle := TCWBar(Source).CubeAngle;
    FCubeDepth := TCWBar(Source).CubeDepth;
    FOptions := TCWBar(Source).Options;
    FScrollingBarWidth := TCWBar(Source).ScrollingBarWidth;
    FColorUsage := TCWBar(Source).ColorUsage;
    FAnimations := TCWBar(Source).Animations;
    FAnimationSpeed := TCWBar(Source).AnimationSpeed;
    FAnimationBooster := TCWBar(Source).AnimationBooster;
  end;
  inherited;
end;

procedure TCWBar.SetOptions(Value: TBarOptions);
begin
  if Value = FOptions then
    Exit;
  FOptions := Value;
  if Writer = nil then
    Exit;
  Writer.DoRepaint;
end;

procedure TCWBar.SetBaseLineValue(Value: single);
begin
  if FBaseLineValue = Value then
    Exit;
  FBaseLineValue := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWBar.SetLayout(Value: TBarLayout);
begin
  if Value = FLayout then
    Exit;
  if (Writer <> nil) and InChart then
  if (Writer.Chart.FOverflowAction = ovCompression) and (Value = blSideBySide)
   then
    ShowGWError(msg_SideBySideCompression);
  if (Value = blStacked) and (anFlow in Animations) and
    not(csLoading in componentState) then
    ShowGWError(msg_StackedFlow);
  FLayout := Value;
  if (Writer = nil) or (Writer.Chart = nil) then
    Exit;
  if Writer.Count = 0 then
    Exit;
  if not(csLoading in componentState)
  then
  begin
    if Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

procedure TCWBar.SetItemSpacing(Value: integer);
begin
  if (Value = FItemSpacing) then
    Exit;
  if (Value < 0) then
    ShowGWError(msg_ItemSpacing);
  FItemSpacing := Value;
  if Writer = nil then
    Exit;
  if not(csLoading in componentState)
  then
  begin
    if Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

procedure TCWBar.SetSeriesSpacing(Value: integer);
begin
  if (Value = FSeriesSpacing) then
    Exit;
  if csLoading in componentState then
  begin
    FSeriesSpacing := Value;
    Exit;
  end;
  FSeriesSpacing := Value;
  if Writer = nil then
    Exit;
  if not(csLoading in componentState)
  then
  begin
    if Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

procedure TCWBar.SetBarWidth(Value: integer);
begin
  if Value = FBarWidth then
    Exit;
  if Value < 0 then
    Value := 0;
  FBarWidth := Value;
  if CubeDepth > BarWidth * 2 then
    FCubeDepth := BarWidth * 2;
  if Writer = nil then
    Exit;
  if not(csLoading in componentState)
  then
  begin
    if Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else if not Writer.InState(stExecuting) then
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

function TCWBar.GetBarWidth: integer;
begin
  if (Writer <> nil) and Writer.Scrollable then
    Result := ScrollingBarWidth
  else
    Result := FBarWidth;
end;

function TCWBar.GetTextQualifier : string;
begin
   Result := '';
   if ValueAxis <> nil then
     Result := ValueAxis.Qualifier;
end;

function TCWBar.GetScrollingBarWidth : integer;
begin
  if FBarWidth = 0 then
    Result := FScrollingBarWidth
  else
    Result := FBarWidth;
end;

procedure TCWBar.SetScrollingBarWidth(Value: integer);
begin
  if Value < 4 then
    Exit;
  FScrollingBarWidth := Value;
  if BarWidth > 0 then {Alike}
  begin
    Exit;
  end;
  if Writer = nil then
    Exit;
  if not(csLoading in componentState)
  then
  begin
    if Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

function TCWBar.GetMaxPointSpacing: integer;
var
  BWidth: integer;
begin
  Result := 0;
  if Writer = nil then
    Exit;
  if not(BarStyle = bsCube) then
    BWidth := BarWidth
  else
    BWidth := Get3DBarWidth;
  if (BWidth <> 0) then
  begin
    if (Layout = blSideBySide) and (Writer.Count > 1) then
    begin
      Result := (BWidth * Writer.Count) + ItemSpacing +
        (SeriesSpacing * (Writer.Count - 1));
    end
    else
      Result := BWidth + ItemSpacing;
  end
  else if (BWidth = 0) then
  begin
    Result := 0;
  end;
end;

function TCWBar.GetMinPointSpacing: integer;
var
  Cpr: Boolean;
  B : integer;
begin
  if GetBarSpace(Result, Cpr, B) then
    Result := 0;
end;

procedure TCWBar.SetMinPointSpacing(Value: integer);
begin
  //
end;

procedure TCWBar.GetCathesus(ARect : TRect; var HorzCath, VertCath: integer);
var
  a: extended;
  A2: extended;
  D: integer;
  qt: single;
begin
  if CubeDepth = 0 then
    D := BarWidth
  else if CubeDepth < 0 then
  begin
    qt := BarWidth / 4;
    D := round(qt * abs(CubeDepth));
  end
  else
    D := CubeDepth;
  if D > BarWidth * 2 then
    D := BarWidth * 2;
  a := DegToRad(CubeAngle);
  A2 := DegToRad(90 - CubeAngle);
  HorzCath := round(Cos(a) * D);
  VertCath := round(Cos(A2) * D);
end;

procedure TCWBar.Get3DDims(ARect: TRect; var TopLeftPt, TopRightPt,
  BottomLeftPt, BottomRightPt: TPoint);
var
  Y, X, Top: integer;
begin
  GetCathesus(ARect, X, Y);
  Top := ARect.Top - Y;
  TopLeftPt := Point(ARect.Left + X, Top);
  TopRightPt := Point(ARect.Right + X, Top);
  BottomRightPt := Point(TopRightPt.X, ARect.Bottom - Y);
  BottomLeftPt := Point(ARect.Right, ARect.Bottom);
end;

procedure TCWBar.Get3DWH(ARect: TRect; var AWidth, AHeight: integer);
var
  TopL, TopR, BotR, BotL: TPoint;
begin
  Get3DDims(ARect, TopL, TopR, BotL, BotR);
  AWidth := TopR.X - ARect.Left;
  AHeight := ARect.Top - TopR.Y;
end;

function TCWBar.Get3DBarWidth: integer;
var
  W: integer;
  H: integer;
  R: TRect;
begin
  if Writer.Scrollable then
  begin
    W := ScrollingBarWidth;
  end
  else
    W := FBarWidth;
  if BarStyle = bsCube then
  begin
    R := Rect(0, 0, BarWidth, 0);
    Get3DWH(R, Result, H);
  end
  else
    Result := W;
end;

procedure TCWBar.SetColorUsage(Value: TColorUsage);
begin
  if Value <> FColorUsage then
  begin
    FColorUsage := Value;
    if InChart then
      Writer.DoRepaint;
  end;
end;

procedure TCWBar.SetTextContents(Value: TTextContents);
begin
  if Value = FTextContents then
    Exit;
  FTextContents := Value;
  if not (csLoading in ComponentState) and InChart then
    Writer.DoRepaint;
end;

procedure TCWBar.SetShowQualifier(Value : Boolean);
begin
  if Value = FShowQualifier then
    Exit;
  FShowQualifier := Value;
  if not (csLoading in ComponentState) and InChart then
    Writer.DoRepaint;
end;

procedure TCWBar.SetCubeAngle(Value: integer);
begin
  if Value = FCubeAngle then
    Exit;
  if (Value < 10) or (Value > 70) then
    ShowGWError(msg_Angle1070);
  FCubeAngle := Value;
  if not(csLoading in componentState)
  then
  begin
    if InChart and Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else if InChart then

    begin
      Writer.RefreshGraph;
    end;
  end;
end;

procedure TCWBar.SetCubeDepth(Value: integer);
begin
  if Value = FCubeDepth then
    Exit;
  if (Value < -8) then
      Value := -8;
  FCubeDepth := Value;
  if not(csLoading in componentState)
  then
  begin
    if InChart and Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else if Inchart then
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

procedure TCWBar.SetBarStyle(Value: TBarStyle);
begin
  if Value = FBarStyle then
    Exit;
  if BarWidth = 0 then
    Exit;
  FBarStyle := Value;
  if not(csLoading in componentState)
  then
  begin
    if Inchart and Writer.Scrollable then
    begin
      Writer.FUpdateKinds := Writer.FUpdateKinds + [ukScroll];
      Writer.DoScrollTo(Writer.FScrollIndex);
    end
    else if InChart then
    begin
      Writer.RefreshGraph;
    end;
  end;
end;

function TCWBar.GetBarStyle: TBarStyle;
begin
  if not Is3D and not(FBarStyle in [bsGradientLength, bsGradientWidth])
  then
    Result := bsFlat
  else
    Result := FBarStyle;
end;

function TCWBar.GetActiveColor(SeriesIndex, ItemIndex : integer): TColor;
var
  R, G, B : Byte;
  S : string;
  Indx : integer;
begin
  if ColorUsage = cuOnSeries then
    Result := Writer.Chart.FSeriesDefs.Items[SeriesIndex].Color
  else
  begin
    if ItemIndex > Writer.Chart.ItemColors.Count-1 then
    begin
      if csDesigning in ComponentState then
      begin
        R := Random(255);
        G := Random(255);
        B := Random(255);
        Result := RGB(R,G,B);
      end
      else
       Result := Writer.Series[SeriesIndex].SeriesItems[ItemIndex].Color;
    end
    else
    begin
      S := Writer.Series[SeriesIndex].SeriesItems[ItemIndex].FName;
      Indx := Writer.Chart.ItemColors.IndexOf(S);
      if Indx <> -1 then
        Result := Writer.Chart.ItemColors.Items[Indx].Color
      else
        Result := Writer.Chart.ItemColors.Items[ItemIndex].Color;
    end;
  end;
end;

function TCWBar.GetSeriesSpacing: integer;
begin
  Result := FSeriesSpacing;
end;

function TCWBar.Compressing: Boolean;
begin
  Result := InChart and (Writer.Chart.OverflowAction = ovCompression)
  and (Writer.Count = 1);
end;

function TCWBar.GetCylinderHatHeight : integer;
begin
  Result := Ceil(BarWidth * 0.2);
end;

procedure TCWBar.DoDraw;
type
  TBarPosit = record
    X: integer;
    Y: integer;
    SerInd: integer;
    SerCount: integer;
  end;

  TBarPosits = TList<TBarPosit>;

var
  i, ItmNumber: integer;
  BarIndx: integer;
  Posits: TBarPosits;
  Pt: TBarPosit;
  BrkLine: integer;
  R: TRect;
  VertChanged, VertFinished: Boolean;
  WInt: integer;
  Wdth: integer;
  UseBrk: Boolean;
  BS: integer;
  Width3D: integer;
  Handled: Boolean;
  LastXY: integer;
  LastBar: TRect;
  LastColor: TColor;
  UsebaseLine: Boolean;
  SelColor: TColor;
  GPen: TGPPen;
  GBrush: TGPSolidBrush;
  VertAnim, HorzAnim: Boolean;
  LastRect: TRect;
  ExcludeEvent: Boolean;

  function GRect: TRect;
  begin
    Result := Writer.FPosRect;
  end;

  function ItmClrsMultiSeries : Boolean;
  begin
    Result := (Writer.Count > 0) and (Layout = blStacked) and (ColorUsage = cuOnItems);
  end;

  procedure SetTheColor(Clr: TColor; Which: integer = 0);
  var
    R, G, B: byte;
  begin
    Clr := ColorToRgb(Clr);
    R := GetRValue(Clr);
    G := GetGValue(Clr);
    B := GetBValue(Clr);
    if (Which = 0) or (Which = 1) then
      GBrush := TGPSolidBrush.Create(MakeColor(255, R, G, B));
    if (Which = 0) or (Which = 2) then
      GPen := TGPPen.Create(MakeColor(255, R, G, B), 0);
  end;

  function DoTrack(BrkLine: integer; OutPutIndex: integer; ARect: TRect;
    SerIndx, SerItmIndx: integer): TTrackBar;
  var
    Tb: TTrackBar;
    BothOver, BothUnder: Boolean;
  begin
    if Writer.InState(stAnimating) then
      Exit;
    if (OutPutIndex > 0) and (Layout = blStacked) then
    begin
      Tb := Writer.FTrackBars[Writer.FTrackBars.Count - 1];
      BothOver := False;
      BothUnder := False;
      if UsebaseLine then
      begin
        if (Writer.FNamAx.IsXAxis) then
        begin
          BothOver := (Tb.VisibleRect.Bottom = BrkLine) and
            (ARect.Bottom = BrkLine);
          BothUnder := (Tb.VisibleRect.Top = BrkLine) and (ARect.Top = BrkLine);
        end
        else
        begin
          BothOver := (Tb.VisibleRect.Left = BrkLine) and
            (ARect.Left = BrkLine);
          BothUnder := (Tb.VisibleRect.Right = BrkLine) and
            (ARect.Right = BrkLine);
        end;
      end;
      if (Writer.FNamAx.IsXAxis) then
      begin
        if not UsebaseLine or BothOver then
        begin
          Tb.VisibleRect.Bottom := ARect.Top;
          Writer.FTrackBars[Writer.FTrackBars.Count - 1] := Tb;
        end
        else if BothUnder then
        begin
          Tb.VisibleRect.Top := ARect.Bottom;
          Writer.FTrackBars[Writer.FTrackBars.Count - 1] := Tb;
        end;
      end
      else
      begin
        if not UsebaseLine or BothOver then
        begin
          Tb.VisibleRect.Left := ARect.Right;
          Writer.FTrackBars[Writer.FTrackBars.Count - 1] := Tb;
        end
        else if BothUnder then
        begin
          Tb.VisibleRect.Right := ARect.Left;
          Writer.FTrackBars[Writer.FTrackBars.Count - 1] := Tb;
        end;
      end;
    end;

    Tb.VisibleRect := ARect;
    Tb.SeriesIndex := SerIndx;
    Tb.SeriesItmIndex := SerItmIndx;
    Writer.FTrackBars.Add(Tb);
    Result := Tb;
  end;

  procedure DrawBarText(ARect: TRect; SerInd, ItmIndex: integer);
  var
    S, Q: string;
    sl : TStringList;
    W, H: integer;
    X, Y: integer;
    V, P: single;
    Gr: TRect;
    TLeft, TRight, BLeft, BRight : TPoint;
  const
    Marg = 3;

  function Stacked : Boolean;
  begin
    Result := (Layout = blStacked) and (Writer.Count > 1);
  end;

  procedure SetTextWidth;
  begin
      if Writer.GetTextWidth(lkInfo, S) > W then
       W := Writer.GetTextWidth(lkInfo, S);
  end;

  procedure DoDraw(R : TRect);
  var
    i : integer;
  begin
    H := Writer.GetTextHeight(lkInfo);
    Y := R.Top;
    for I := 0 to sl.Count-1 do
    begin
      W := Writer.GetTextWidth(lkInfo, Sl[i]);
      X := R.CenterPoint.X - W div 2;
      Canvas.TextOut(X, Y, Sl[i]);
      inc(Y, H);
    end;
  end;

  begin
    if not(boText in Options) then
      Exit;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
    if Writer.InState(stAnimating) and not(Animations = [anFlow]) and
      not(Animations = [anFlow, anPause]) then
    begin
      if not Writer.InState(stAnimationPause) and not VertFinished then
      begin
        Exit;
      end;
    end;

    if ShowQualifier then
      Q := ' ' + TextQualifier
    else
      Q := '';
    V := Writer.SeriesItems[SerInd, ItmIndex].Value;
    W := 0;
    if tcTitle in TextContents then
    begin
     S := Writer.Chart.FSeriesDefs[SerInd].Title;
     sl.Add(S);
     SetTextWidth;
    end;
    if tcName in TextContents then
    begin
     S := Writer.SeriesItems[SerInd, ItmIndex].Name;
     sl.Add(S);
     SetTextWidth;
    end;
    if tcValue in TextContents then
    begin
     S := FormatNum(V, Writer.ValuePrecision, True) + Q;
     sl.Add(S);
     SetTextWidth;
    end;
    if tcPercentage in TextContents then
    begin
     P := Writer.SeriesItems[SerInd, ItmIndex].Pst;
     S := FormatNum(P, Writer.ValuePrecision, True) + '%';
     sl.Add(S);
     SetTextWidth;
    end;

    H := Writer.GetTextHeight(lkInfo) * sl.Count;
    Canvas.Font.Color := InvertColor(Canvas.Brush.Color);
    Canvas.Brush.Style := bsClear;
    Gr := Writer.GetGraphPrintRect;

    if (BarStyle = bsCube) and not Stacked then
    begin
     Get3DDims(ARect, TLeft, TRight, BLeft, BRight);
     ARect.Top := ARect.Top + (TRight.Y-ARect.Top);
     ARect.Right := ARect.Right + (TRight.X - ARect.Right);
    end
    else if (BarStyle = bsCylinder) and not Stacked then
    begin
     ARect.Top := ARect.Top - GetCylinderHatHeight;
    end;

    if Writer.FNamAx.IsXAxis then
    begin
      if not(boBaseLine in Options) or
        ((boBaseLine in Options) and (V >= BaseLineValue)) then
      begin
        if (ARect.Top - Marg - H < Writer.GraphRect.Top) or Stacked then
        begin
          Y := ARect.Top + Marg;
        end
        else
        begin
          Y := ARect.Top - Marg - H;
          Canvas.Font.Color := InvertColor(Writer.GraphBGColor);
        end;
      end
      else
      begin
        if (ARect.Bottom + H + Marg > Writer.GraphRect.Bottom) then
        begin
          Y := ARect.Bottom - Marg - H;
        end
        else
        begin
          Y := ARect.Bottom + Marg;
          Canvas.Font.Color := InvertColor(Writer.GraphBGColor);
        end;
      end;
      if (Y > Writer.GraphRect.Top) and (Y < Writer.GraphRect.Bottom) then
      begin
        GR := ARect;
        GR.Left := ARect.CenterPoint.X - W div 2;
        GR.Right := Gr.Left + W;
        GR.Top := Y;
        GR.Bottom := GR.Top + H;
        GR := FOverlaps.GetRect(GR);
        DoDraw(Gr);
      end;
    end
    else
    begin
      Y := ARect.CenterPoint.Y - H div 2;
      if not(boBaseLine in Options) or
        ((boBaseLine in Options) and (V >= BaseLineValue)) then
      begin
        if (ARect.Right + W + Marg < Writer.GraphRect.Right) and not Stacked  then
        begin
          X := ARect.Right + Marg;
          Canvas.Font.Color := InvertColor(Writer.GraphBGColor);
        end
        else
        begin
          X := ARect.Right - Marg-W;
        end;
      end
      else
      begin
        if ARect.Left - W - Marg > Writer.GraphRect.Left then
        begin
          X := ARect.Left - W - Marg;
          Canvas.Font.Color := InvertColor(Writer.GraphBGColor);
        end
        else
          X := ARect.Left + Marg
      end;
      if (X > Writer.GraphRect.Left) and (X < Writer.GraphRect.Right) then
      begin
        GR := ARect;
        GR.Left := X;
        GR.Right := X + W;
        GR.Top := ARect.CenterPoint.Y - H div 2;
        GR.Bottom := GR.Top + H;
        GR := FOverlaps.GetRect(GR);
        DoDraw(Gr);
      end;
    end;
    finally
      sl.Free;
    end;
  end;

  procedure DrawGradRectangle(ARect: TRect; Color: TColor);
  var
    EndClr: TColor;
    Pt: TPoint;
    R: TRect;
    Dir: TGradientDirection;
    GradBrush: TGPLinearGradientBrush;
  begin

    if BarStyle = bsGradientLength then
      EndClr := ChangeColor(Color, d3_GradientStyle)
    else
      EndClr := ChangeColor(Color, d3_GradientCylinder);

    Pt := ARect.CenterPoint;
    if Writer.FNamAx.IsXAxis then
    begin
      if BarStyle = bsGradientLength then
        Dir := gdVertical
      else
        Dir := gdHorizontal;
      R := ARect;
      if BarStyle <> bsCylinder then
        R.Right := Pt.X
      else
      begin
        GradBrush := TGPLinearGradientBrush.Create(MakeRect(R),
          MakeGPClr(Color), MakeGPClr(EndClr), LinearGradientModeHorizontal);
        GDIP.FillRectangle(GradBrush, MakeRect(R));
        if (boOutLines in Options) or ItmClrsMultiseries  then
        begin
          SetTheColor(Color, 2);
          GDIP.DrawLine(GPen, MakePoint(R.Left, R.Top),
            MakePoint(R.Left, R.Bottom));
          GDIP.DrawLine(GPen, MakePoint(R.Right, R.Top),
            MakePoint(R.Right, R.Bottom));
          GPen.Free;
        end;
        GradBrush.Free;
        Exit;
      end;
      if BarStyle = bsGradientLength then
        GradientFillCanvas(Canvas, EndClr, Color, ARect, Dir)
      else
        GradientFillCanvas(Canvas, EndClr, Color, R, Dir);

      R := ARect;
      R.Left := Pt.X;
      if BarStyle = bsGradientLength then
        GradientFillCanvas(Canvas, Color, EndClr, ARect, Dir)
      else
        GradientFillCanvas(Canvas, Color, EndClr, R, Dir);
    end
    else
    begin
      if BarStyle = bsGradientLength then
        Dir := gdHorizontal
      else
        Dir := gdVertical;
      R := ARect;
      R.Bottom := Pt.Y;
      GradientFillCanvas(Canvas, EndClr, Color, R, Dir);
      R := ARect;
      R.Top := Pt.X;
      GradientFillCanvas(Canvas, Color, EndClr, R, Dir);
    end;
    if (boOutLines in Options) or ItmClrsMultiseries then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := ChangeColor(Color, d3_Outline);
      Canvas.Rectangle(ARect);
      Canvas.Brush.Style := bsSolid;
    end;
  end;

  procedure DrawCylinder(ARect: TRect; Color: TColor; FirstBar: Boolean;
    SerIndx: integer);
  var
    Clr: TColor;
    GPClr1, GPClr2: TGPColor;
    R, TopR: TRect;
    GradBrush: TGPLinearGradientBrush;
    D3Inc: integer;

    procedure DrawOutLine(AClr: TColor; Ellips: Boolean);
    begin
      Clr := ChangeColor(AClr, d3_Outline);
      SetTheColor(Clr, 2);
      if Ellips then
        GDIP.DrawEllipse(GPen, MakeRect(R))
      else
        GDIP.DrawArc(GPen, MakeRect(R), 0, 180);
      GPen.Free;
    end;

    procedure DrawFirstTop(Colr: TColor; Rct: TRect);
    var
      Clr: TColor;
    begin
      Clr := ChangeColor(Colr, d3_TopCylinder);
      SetTheColor(Clr);
      Dec(Rct.Right);
      Inc(Rct.Left);
      GDIP.FillEllipse(GBrush, MakeRect(Rct));
      GDIP.DrawEllipse(GPen, MakeRect(Rct));
      GBrush.Free;
      GPen.Free;
      if (boOutLines in Options) or ItmClrsMultiseries then
      begin
        DrawOutLine(Colr, True);
      end;
    end;

  begin
    D3Inc := 0;
    if Writer.HasWall then
    begin
      D3Inc := GetCylinderHatHeight; //d3_CylinderHeight;
    end;
    inc(ARect.Bottom, D3Inc);
    R := ARect;
    R.Bottom := Writer.GetGraphPrintRect.Bottom - 1 + D3Inc;
    R.Top := R.Bottom - GetCylinderHatHeight;;
    Dec(R.Right);
    Clr := Color;
    SetTheColor(Clr, 2);
    GPClr1 := MakeGPClr(Color);
    Clr := ChangeColor(Color, d3_GradientCylinder);
    GPClr2 := MakeGPClr(Clr);
    GradBrush := TGPLinearGradientBrush.Create(MakePoint(R.Left, 10),
      MakePoint(R.Right, 10), GPClr1, GPClr2);
    GDIP.FillPie(GradBrush, MakeRect(R), 0, 180);
    GradBrush.Free;
    GPen.Free;

    if (boOutLines in Options) or ItmClrsMultiseries then
    begin
      DrawOutLine(Color, False);
    end;

    R := ARect;
    R.Top := R.Top - GetCylinderHatHeight div 2;
    R.Bottom := R.Bottom - GetCylinderHatHeight div 2;

    DrawGradRectangle(R, Color);
    LastRect := R;

    R.Top := R.Top - GetCylinderHatHeight div 2;
    R.Bottom := R.Top + GetCylinderHatHeight;
    TopR := R;
    if FirstBar then
    begin
      DrawFirstTop(Color, R);
    end
    else
    begin
      R.Top := R.Top - 1;
      if Layout = blStacked then
      begin
        if VertFinished or not Writer.InState(stAnimating) then
        begin
          GPClr1 := MakeGPClr(LastColor);
          Clr := ChangeColor(LastColor, d3_GradientCylinder);
          GPClr2 := MakeGPClr(Clr);

          GradBrush := TGPLinearGradientBrush.Create(MakePoint(R.Left, 10),
            MakePoint(R.Right, 10), GPClr1, GPClr2);
          GDIP.FillPie(GradBrush, MakeRect(R), 0, 180);
          GradBrush.Free;
          if (boOutLines in Options) or ItmClrsMultiseries then
          begin
            DrawOutLine(LastColor, False);
          end;
        end
        else
        begin
          Clr := ChangeColor(Color, d3_GradientCylinder);
          SetTheColor(Clr, 1);
          GDIP.FillEllipse(GBrush, MakeRect(R));
          GBrush.Free;
          if (boOutLines in Options) or ItmClrsMultiseries then
          begin
            DrawOutLine(Color, True);
          end;

        end;
      end;
    end;
  end;

  procedure DrawCube(var ABarRect: TRect; Color: TColor; FirstBar: Boolean;
    SerIndx: integer);
  var
    TopL, TopR, BotR, BotL: TPoint;
    Clr: TColor;
    GPts: TPointDynArray;
    PrintTop: Boolean;
    H: integer;
    D3Inc: integer;
  begin
    if Writer.HasWall then
    begin
      Get3DDims(ABarRect, TopL, TopR, BotL, BotR);
      H := ABarRect.Top - TopR.Y;
      D3Inc := H;
      inc(ABarRect.Bottom, D3Inc);
    end;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(ABarRect);
    Get3DDims(ABarRect, TopL, TopR, BotL, BotR);
    setlength(GPts, 4);
    Clr := ChangeColor(Color, d3_TopCube);
    if Layout = blStacked then
    begin
      if Animations = [anGrow] then
        PrintTop := FirstBar or
          (not VertFinished and Writer.InState(stAnimating))
      else
        PrintTop := FirstBar;
    end
    else
      PrintTop := FirstBar;
    if PrintTop then
    begin
      SetTheColor(Clr);
      try
        GPts[0] := MakePoint(ABarRect.Left, ABarRect.Top);
        GPts[1] := MakePoint(TopL.X, TopL.Y);
        GPts[2] := MakePoint(TopR.X, TopR.Y);
        GPts[3] := MakePoint(ABarRect.Right, ABarRect.Top);
        GDIP.FillPolygon(GBrush, PGPPoint(@GPts[0]), 4);
        GDIP.DrawPolygon(GPen, PGPPoint(@GPts[0]), 4);
      finally
        GPen.Free;
        GBrush.Free;
      end;
    end;

    Clr := ChangeColor(Color, d3_SideCube);
    SetTheColor(Clr);
    try
      GPts[0] := MakePoint(ABarRect.Right - 1, ABarRect.Top);
      GPts[1] := MakePoint(TopR.X, TopR.Y);
      GPts[2] := MakePoint(BotR.X, BotR.Y);
      GPts[3] := MakePoint(BotL.X - 1, BotL.Y - 1);
      GDIP.FillPolygon(GBrush, PGPPoint(@GPts[0]), 4);
      GDIP.DrawPolygon(GPen, PGPPoint(@GPts[0]), 4);
    finally
      GPen.Free;
      GBrush.Free;
    end;

    if ((boOutLines in Options) or ItmClrsMultiseries) then
    begin
      Clr := ChangeColor(Color, d3_Outline);
      SetTheColor(Clr);
      try
        GDIP.DrawLine(GPen, ABarRect.Left, ABarRect.Top, ABarRect.Right,
          ABarRect.Top);
        if FirstBar then
        begin
         GDIP.DrawLine(GPen, ABarRect.Left, ABarRect.Top, TopL.X, TopL.Y);
         GDIP.DrawLine(GPen, TopL.X, TopL.Y, TopR.X, TopR.Y);
         GDIP.DrawLine(GPen, TopR.X, TopR.Y, BotR.X,BotR.Y);
        end;
        GDIP.DrawLine(GPen, BotR.X, BotR.Y, BotL.X, BotL.Y);
        GDIP.DrawLine(GPen, ABarRect.Right, ABarRect.Top, TopR.X, TopR.Y);
        GDIP.DrawLine(GPen, ABarRect.Left, ABarRect.Bottom, ABarRect.Right, ABarRect.Bottom);
        GDIP.DrawLine(GPen, ABarRect.Left, ABarRect.Top, ABarRect.Left, ABarRect.Bottom);
        GDIP.DrawLine(GPen, ABarRect.Right, ABarRect.Top, ABarRect.Right, ABarRect.Bottom);
      finally
        GPen.Free;
        GBrush.Free;
      end;
    end;
  end;

  function DrawSideBySideBars(BrkLine: integer; OutPutIndex: integer;
    BarRect: TRect; SerInd, ItmNumber: integer): Boolean;
  var
    W: integer;
    Pt: TPoint;
    Block: integer;
    W3D: integer;
  begin
    Result := True;
    W := Wdth;
    W3D := 0;
    Pt := Point(Posits[OutPutIndex].X, Posits[OutPutIndex].Y);
    if BarStyle = bsCube then
    begin
      W3D := Get3DBarWidth;
      Block := (W3D * Writer.Count) + (SeriesSpacing * (Writer.Count - 1));
    end
    else
      Block := (W * Writer.Count) + (SeriesSpacing * (Writer.Count - 1));
    if Writer.FNamAx.IsXAxis then
    begin
      if (OutPutIndex > 0) then
      begin
        if BarStyle = bsCube then
          BarRect.Left := LastXY + W3D + SeriesSpacing
        else
          BarRect.Left := LastXY + W + SeriesSpacing;
        if VertAnim and Writer.InState(stAnimating) and HorzAnim then
        begin
          if VertFinished then
            LastXY := BarRect.Left
        end
        else
          LastXY := BarRect.Left;
      end
      else
      begin
        if BarStyle = bsCube then
          BarRect.Left := Pt.X - (Block div 2) + (Width3D div 2)
        else
          BarRect.Left := Pt.X - (Block div 2) + (W div 2);
        LastXY := BarRect.Left;
      end;
      BarRect.Right := BarRect.Left + W;
    end
    else
    begin
      if (OutPutIndex > 0) then
      begin
        BarRect.Top := LastXY + W + SeriesSpacing;
        if VertAnim and Writer.InState(stAnimating) and HorzAnim then
        begin
          if VertFinished then
            LastXY := BarRect.Top
        end
        else
          LastXY := BarRect.Top;
      end
      else
      begin
        BarRect.Top := Pt.Y - (Block div 2) + (W div 2);
        LastXY := BarRect.Top;
      end;
      BarRect.Bottom := BarRect.Top + W;
    end;

    Writer.FAnimInfo.LastXY := LastXY;

    LastBar := BarRect;
    if BarStyle <> bsCube then
      DoTrack(BrkLine, 0, BarRect, SerInd, ItmNumber);
    Handled := False;

    if Assigned(FOnDrawBar) and not ExcludeEvent then
    begin
      FOnDrawBar(Self, SerInd, ItmNumber, Canvas, Handled);
      if SelColor <> ActiveColor[SerInd, ItmNumber] then
      begin
        Canvas.Brush.Color := ActiveColor[SerInd, ItmNumber];
        SelColor := Canvas.Brush.Color;
      end;
    end;
    if not Handled then
    begin
      if BarStyle = bsCylinder then
        DrawCylinder(BarRect, Canvas.Brush.Color, True, OutPutIndex)
      else if BarStyle in [bsGradientLength, bsGradientWidth] then
        DrawGradRectangle(BarRect, Canvas.Brush.Color)
      else if BarStyle = bsCube then
      begin
        DrawCube(BarRect, Canvas.Brush.Color, True, OutPutIndex);
        DoTrack(BrkLine, 0, BarRect, SerInd, ItmNumber);
      end
      else
        Canvas.Rectangle(BarRect);
    end;
  end;

  function CheckSpace: Boolean;
  var
    W3: integer;
    Block: integer;
    Cpr: Boolean;
    B : integer;
  begin
    Result := True;
    GetBarSpace(WInt, Cpr, B);
    if (Layout = blSideBySide) and not Compressing then
    begin
      W3 := Get3DBarWidth;
      Block := (W3 * Writer.Count) + (SeriesSpacing * (Writer.Count - 1));
      Result := Block >= W3;
    end;
  end;

  function GetNext(Indx: integer; Posits: TBarPosits;
    var ItmNumber: integer): Boolean;
  { The next item }
  var
    i, j: integer;
    Itm: TSeriesItem;
    NextPtIndex: integer;
    Added: Boolean;
    Pt: TPoint;
    P: TBarPosit;
    Indt: integer;
  begin
    Result := False;
    Posits.Clear;
    for i := 0 to Writer.Count - 1 do
    begin
      if not(Series[i].Visible) or not InChart(i)
      then
        Continue;
      Indt := Series[i].FIndent;
      if Indt + (Indx - Indt) > Series[i].LastItem then
        Continue;
      ItmNumber := Indt + (Indx - Indt);
      NextPtIndex := Indx - Series[i].FIndent;
      if (NextPtIndex < 0) or (NextPtIndex > Series[i].FBarPoints.Count - 1) then
        Continue;
      Itm := Series[i].FSeriesItems[ItmNumber];
      if Itm.FLeapDummy then
      begin
        Result := True;
        Continue;
      end;
      Pt := Series[i].FBarPoints[NextPtIndex];
      if Writer.FNamAx.IsXAxis then
      begin
        if BarStyle = bsCube then
          Pt.X := Pt.X - (Width3D div 2)
        else
          Pt.X := Pt.X - (Wdth div 2);
      end
      else
        Pt.Y := Pt.Y - (Wdth div 2);
      P.X := Pt.X;
      P.Y := Pt.Y;
      P.SerInd := i;
      Added := False;
      if Writer.FNamAx.IsXAxis then
        for j := 0 to Posits.Count - 1 do
        begin
          { Sort overlaps by height/width. Draw tallest first }
          if UsebaseLine and (Pt.Y > BrkLine) then
          begin
            if (Pt.Y > Posits[j].Y) and (Layout = blStacked) then
            begin
              Posits.Insert(j, P);
              Added := True;
              Break;
            end;
          end
          else
          begin
            if (Pt.Y < Posits[j].Y) and (Layout = blStacked) then
            begin
              Posits.Insert(j, P);
              Added := True;
              Break;
            end;
          end;
        end
      else
        for j := 0 to Posits.Count - 1 do
        begin
          if UsebaseLine and (Pt.X < BrkLine) then
          begin
            if (Pt.X < Posits[j].X) and (Layout = blStacked) then
            begin
              Posits.Insert(j, P);
              Added := True;
              Break;
            end;
          end
          else
          begin
            if (Pt.X > Posits[j].X) and (Layout = blStacked) then
            begin
              Posits.Insert(j, P);
              Added := True;
              Break;
            end;
          end;
        end;
      if not Added then
        Posits.Add(P);
      Result := True;
    end;
  end;

  function GetAnimRect(ARect: TRect): TRect;
  { Sets print line i vertical animations }
  begin
    Result := ARect;
    if Writer.FNamAx.IsXAxis then
    begin
      if (boBaseLine in Options) and (ARect.Top + 1 > BrkLine) then
      begin
        if Result.Top + Writer.FAnimInfo.NextVert < Result.Bottom then
        begin
          Result.Bottom := Result.Top + Writer.FAnimInfo.NextVert;
          if Result.Bottom > ARect.Bottom then
            Result.Bottom := ARect.Bottom;
          VertChanged := True;
          VertFinished := Result.Bottom = ARect.Bottom;
        end
        else if HorzAnim then
          VertFinished := True;
      end
      else if Result.Bottom - Writer.FAnimInfo.NextVert > Result.Top then
      begin
        Result.Top := Result.Bottom - Writer.FAnimInfo.NextVert;
        if Result.Top < ARect.Top then
          Result.Top := ARect.Top;
        VertChanged := True;
        VertFinished := Result.Top = ARect.Top;
      end
      else
      begin
        VertFinished := True;
      end;
    end
    else
    begin
      if (boBaseLine in Options) and (ARect.Right - 1 < BrkLine) then
      begin
        if Result.Right - Writer.FAnimInfo.NextVert > Result.Left then
        begin
          Result.Left := Result.Right - Writer.FAnimInfo.NextVert;
          if Result.Left < ARect.Left then
            Result.Left := ARect.Left;
          VertChanged := True;
          VertFinished := Result.Left = ARect.Left;
        end
        else if HorzAnim then
          VertFinished := True;
      end
      else if Result.Left + Writer.FAnimInfo.NextVert < Result.Right then
      begin
        Result.Right := Result.Left + Writer.FAnimInfo.NextVert;
        if Result.Right > ARect.Right then
          Result.Right := ARect.Right;
        VertChanged := True;
        VertFinished := Result.Right = ARect.Right;
      end
      else
      begin
        VertFinished := True;
      end;
    end

  end;

  procedure DrawAnimLine;
  var
    R: TRect;
  begin
    R := Writer.GetGraphPrintRect;
    R.Bottom := Writer.FAnimInfo.NextVert;
    Dec(Writer.FAnimInfo.NextVert, 2);
    R.Top := R.Bottom - 2;
    if Writer.FAnimInfo.NextVert < Writer.GetGraphPrintRect.Top then
    begin
      Writer.ClearState(stAnimating);
      Writer.FAnimInfo.Paused := False;
    end;
    if Writer.InState(stAnimating) then
    begin
      Writer.ClearState(stAnimating);
      Canvas.CopyRect(R, Writer.FAnimBM.Canvas, R);
      Writer.SetState(stAnimating);
    end;
  end;

{ Main proc Draw bars }
begin
  if Writer = nil then
    Exit;
  if not InChart then
    Exit;
  Writer.FTrackBars.Clear;
  BrkLine := Writer.PosFromValue(BaseLineValue, Writer.ValAxFromGraph(Self));
  UsebaseLine := (boBaseLine in Options) and (BaseLineValue > Writer.ValueLow) and
    (BaseLineValue < Writer.ValueHigh);
  Canvas.Brush.Style := bsSolid;
  BS := ItemSpacing;
  if (Layout = blSideBySide) and not Compressing then
    BS := BS + (SeriesSpacing * (Writer.Count - 1));

  if Compressing then
  begin
    CheckSpace;
    WInt := WInt - BS;
    if WInt < 1 then
      WInt := 1
  end
  else if BarWidth = 0 then
  begin
    CheckSpace;
    WInt := WInt - BS;
    if Layout = blSideBySide then
    begin
      WInt := round(WInt / Writer.Count);
    end;
  end
  else
  begin
    if not CheckSpace then
    begin
      ShowGWError(msg_SeriesSpacesTooSmall);
    end;
    Width3D := WInt - BS; { The size of the series block }
    WInt := BarWidth;
  end;

  if Writer.FNamAx.IsXAxis then
  begin
    R.Left := GRect.Left;
  end
  else
  begin
    R.Top := GRect.Top;
  end;

  Wdth := WInt;

  UseBrk := UsebaseLine;

  BarIndx := 0;

  FHorzCounter := 0;
  FVertCounter := 0;
  VertAnim := (anGrow in Animations);
  HorzAnim := (anFlow in Animations);
  { VerAnim and HorzAnim use for shorts }
  Writer.FAnimInfo.Stopped := False;
  Writer.FAnimInfo.Paused := Writer.FAnimInfo.StartPause;

  if Writer.FAnimInfo.Paused then
  begin
    Writer.FAnimInfo.StartPause := False;
    Exit;
  end;

  ExcludeEvent := Writer.InState(stAnimating);

  if VertAnim and Writer.InState(stAnimating) and
    not Writer.InState(stInitAnimation) then
  begin
    { Clear GraphRect to render anti aliasing correctly }
    Writer.FAnimInfo.NextVert := Writer.FAnimInfo.NextVert +
      AnimationBooster + 1;
    if not HorzAnim then
    begin
      LastBar := Writer.GetGraphPrintRect;
      inc(LastBar.Left);
      Canvas.Brush.Color := Writer.GraphBGColor;
      Canvas.FillRect(LastBar);
      Writer.FNamAx.DrawSections;
      Writer.FValAx.DrawSections;
      Writer.DrawBorders;
    end;
  end;
  Posits := TBarPosits.Create;
  try
    VertChanged := False;
    VertFinished := False;
    while GetNext(BarIndx, Posits, ItmNumber) do
    begin
      for i := 0 to Posits.Count - 1 do
      begin
        if Writer.InState(stAnimating) and not Compressing then
        begin
          LastXY := Writer.FAnimInfo.LastXY;
          if (FHorzCounter < Writer.FAnimInfo.NextHorz) and HorzAnim then
          begin
            inc(FHorzCounter);
            Continue;
          end
          else if HorzAnim then
          begin
            if VertAnim then
            begin
              if VertFinished then
                Writer.FAnimInfo.NextHorz := FHorzCounter + 1;
            end
            else
              Writer.FAnimInfo.NextHorz := FHorzCounter + 1;
            Writer.FAnimInfo.Stopped := True;
          end;
        end;

        SelColor := ActiveColor[Posits[i].SerInd, ItmNumber];
        Canvas.Brush.Color := SelColor;
        if (boOutLines in Options) or ItmClrsMultiseries then
        begin
          Canvas.Pen.Color := ChangeColor(SelColor, d3_Outline)
        end
        else
          Canvas.Pen.Color := Canvas.Brush.Color;

        if BarIndx > 0 then
        begin
          if Writer.FNamAx.IsXAxis then
          begin
            Pt := Posits[i];
          end
          else
          begin
            Pt := Posits[i];
          end;
        end;
        if Writer.FNamAx.IsXAxis then
        begin
          if UseBrk and (Posits[i].Y > BrkLine) then
          begin
            R.Top := BrkLine;
            R.Bottom := Posits[i].Y;
          end
          else
          begin
            if UseBrk then
            begin
              R.Bottom := BrkLine;
              R.Top := Posits[i].Y;
            end
            else
            begin
              R.Bottom := GRect.Bottom;
              R.Top := Posits[i].Y;
            end;
          end;
          R.Left := Posits[i].X;
          R.Right := R.Left + Wdth;
        end
        else
        begin
          if UseBrk and (Posits[i].X > BrkLine) then
          begin
            R.Left := BrkLine;
            R.Right := Posits[i].X;
          end
          else
          begin
            if UseBrk then
            begin
              R.Left := Posits[i].X;
              R.Right := BrkLine;
            end
            else
            begin
              R.Left := GRect.Left;
              R.Right := Posits[i].X
            end;
          end;
          R.Top := Posits[i].Y;
          R.Bottom := R.Top + Wdth;
        end;
        if not (Writer.FNamAx.IsXAxis) and (R.Width <> 0)  then
          inc(R.Left);

        if (R.Height = 0)
          and (Writer.Series[Posits[i].SerInd].FSeriesItems[ItmNumber].Value > 0) then
           Dec(R.Top);
        if (R.Width = 0)
          and (Writer.Series[Posits[i].SerInd].FSeriesItems[ItmNumber].Value > 0) then
           Inc(R.Right);

        if VertAnim and Writer.InState(stAnimating) then
        begin
          R := GetAnimRect(R);
          if VertFinished and HorzAnim then
          begin
            Writer.FAnimInfo.NextHorz := FHorzCounter + 1;
            Writer.FAnimInfo.NextVert := 0;
            Writer.FAnimInfo.Paused := True;
          end;
        end
        else if Writer.InState(stAnimating) then
          Writer.FAnimInfo.Paused := True;

        if (Layout = blSideBySide) and (Writer.VisibleCount > 1) and
          (SeriesCount > 1) and not Compressing then
        begin
          if not DrawSideBySideBars(BrkLine, i, R, Posits[i].SerInd, ItmNumber)
          then
            Exit;
        end
        else
        begin
          Handled := False;
          if BarStyle <> bsCube then
           DoTrack(BrkLine, i, R, Posits[i].SerInd, ItmNumber);
          if Assigned(FOnDrawBar) and not ExcludeEvent then
          begin
            FOnDrawBar(Self, Posits[i].SerInd, ItmNumber, Canvas, Handled);
            if ColorUsage = cuOnItems then
            if SelColor <> ActiveColor[Posits[i].SerInd, ItmNumber] then
            begin
              Canvas.Brush.Color := ActiveColor[Posits[i].SerInd, ItmNumber];
              SelColor := Canvas.Brush.Color;
            end;
          end;

          if not Handled or Writer.FAnimInfo.Stopped then
          begin
            if (BarStyle = bsFlat) or Compressing then
              Canvas.Rectangle(R)
            else if BarStyle = bsCylinder then
              DrawCylinder(R, Canvas.Brush.Color, i = 0, i)
            else if (BarStyle in [bsGradientWidth, bsGradientLength]) then
              DrawGradRectangle(R, Canvas.Brush.Color)
            else
            begin
              DrawCube(R, Canvas.Brush.Color, i = 0, i);
              DoTrack(BrkLine, i, R, Posits[i].SerInd, ItmNumber);
            end;
          end;
        end;

        LastColor := SelColor;
        if Writer.FAnimInfo.Stopped then
          Break;
        Writer.ResetCanvas;
      end; { End posit loop }
      if (Posits.Count > 0) and (Layout = blStacked) then
      begin
        if Writer.FNamAx.IsXAxis then
          LastXY := Posits[0].X
        else
          LastXY := Posits[0].Y;
      end;
      inc(BarIndx);
      Wdth := WInt;
      if Writer.FAnimInfo.Stopped then
        Break;
    end; { GetNex Loop }
  finally
    if HorzAnim and not VertAnim then
    begin
      if Writer.InState(stAnimating) and not Writer.FAnimInfo.Stopped then
      begin
        Writer.ClearState(stAnimating);
        Writer.FAnimInfo.Paused := False;
      end;
    end

    else if VertAnim and not HorzAnim then
    begin
      if not VertChanged then
      begin
        Writer.ClearState(stAnimating);
        Writer.FAnimInfo.Paused := False;
      end;
    end
    else if VertAnim then
    begin
      if Writer.InState(stAnimating) and not Writer.FAnimInfo.Stopped then
      begin
        Writer.ClearState(stAnimating);
        Writer.FAnimInfo.Paused := False;
      end;
    end;;

    Posits.Free;
  end;
  if (boText in Options) and (TextContents <> []) then
  for i := 0 to Writer.FTrackBars.Count - 1 do
  begin
    DrawBarText(Writer.FTrackBars[i].VisibleRect,
     Writer.FTrackBars[i].SeriesIndex, Writer.FTrackBars[i].SeriesItmIndex);
  end;

  if UseBrk and (veBaseLine in Writer.AxisElements) then
  begin
    Canvas.Pen.Color := clBlack;
    if Writer.FNamAx.IsXAxis then
    begin
      Canvas.MoveTo(Writer.GetGraphPrintRect.Left, BrkLine);
      Canvas.LineTo(Writer.GetGraphPrintRect.Right, BrkLine);
    end
    else
    begin
      Canvas.MoveTo(BrkLine, Writer.GetGraphPrintRect.Top);
      Canvas.LineTo(BrkLine, Writer.GetGraphPrintRect.Bottom);
    end
  end;
  Writer.ResetCanvas;
end;

procedure TCWBar.Draw;
begin
  inherited Draw;
  try
    if Writer.FNamAx.IsXAxis then
      FOverlaps := TOverlaps.Create(Writer.GraphRect, mdUp)
    else
     FOverlaps := TOverlaps.Create(Writer.GraphRect, mdRight);
    DoDraw;
  finally
    FGDIP.Free;
    FOverlaps.Free;
  end;
end;

function TCWBar.GetBarSpace(var BlockWidth: integer;
  var Compressed: Boolean; var BarWidth : integer): Boolean;
{ Checks if there is enough space to render the diagram within then current graph rectangle
  given the current bar width and spacing. The BlocWidth param returns the required block
  width (bar width + spacing). If this is too big the function returns false. }
var
  GrWidth: integer;
  BlockSize: single;
  BlockCount: integer;
  OutLineMin: integer;
  RegularMin: integer;
  SideBySideMin: integer;
  W: integer;
  Spacing: integer;
const
  { The minimum width when ChartWriter.BarWidth = 0 }
  c_OutlineMin = 4;
  c_SideBySideMin = 4;
  c_RegularMin = 2;

  function RealDataCount: integer;
  begin
      Result := SeriesCount;
  end;

begin
  Result := True;
  if SeriesCount = 0 then
    Exit;
  if (Layout = blSideBySide) and (RealDataCount > 1) then
    Spacing := ItemSpacing + (SeriesSpacing * (RealDataCount - 1))
  else
    Spacing := ItemSpacing;

  W := Get3DBarWidth; { If not 3d returns 2d width }

  OutLineMin := c_OutlineMin;
  RegularMin := c_RegularMin;
  SideBySideMin := c_SideBySideMin;
  if Writer.FNamAx.IsXAxis then
    GrWidth := Writer.GetGraphPrintRect.Width
  else
    GrWidth := Writer.GetGraphPrintRect.Height;
  if Writer.FLongestSeries = nil then
    Writer.FLongestSeries := Writer.LongestSeries;
  BlockCount := Writer.FLongestSeries.FSeriesItems.Count;
  { The number of blocks that the graph rectangle can hold }
  BlockSize := (GrWidth / BlockCount);
  { The size of a block. This includes the bar itself and its spacing }

  if W > 0 then
  begin
    OutLineMin := W;
    RegularMin := W;
    SideBySideMin := W;
    if (Layout = blSideBySide) and (RealDataCount > 1) then
    begin
      BlockWidth := OutLineMin * RealDataCount + Spacing;
    end
    else
      BlockWidth := W + Spacing; { Fixed }
    if (BlockSize < BlockWidth) then
    { Not enough space }
    begin
      Result := False;
    end;
  end
  else
  begin
    BlockWidth := round(BlockSize); { Block size in whole units }
  end;

  BarWidth := BlockWidth - Spacing;
  { The width of the bar itself. If side by side layout, this includes all the bars }

  if (boOutLines in Options)
  { An outlined bar must be at least 4 pixels wide if not fixed }
    and (BarWidth < OutLineMin) then
  begin
    Result := False;
    if (Layout = blSideBySide) and (RealDataCount > 1) then
    begin
      BlockWidth := OutLineMin * RealDataCount + Spacing
      { Return the blockwidth ncessary for holding the bars.
        Used with Contraction }
    end
    else
      BlockWidth := OutLineMin + Spacing;
  end
  else if BarWidth < RegularMin then
  begin
    Result := False;
    BlockWidth := RegularMin + Spacing;
  end;

  if (Layout = blSideBySide) and (SeriesCount > 1) then
  begin
    BarWidth := round(BarWidth / RealDataCount);
    { The indivdual bar widths in a side by side layout }
    if BarWidth < SideBySideMin then
    { Side by side cannot be less than 4 or fixed, else looks unfriendly }
    begin
      Result := False;
      BlockWidth := SideBySideMin * RealDataCount + Spacing;
    end;
  end;

  if (Writer.Chart.OverflowAction = ovCompression) and not Result then
  begin
    Result := True;
    Compressed := True;
    BlockWidth := round(Writer.GraphPrintRect.Width / Writer.NameCount);
    if BlockWidth = 0 then
      BlockWidth := 1;

  end;
end;

{ TCWPie ------------------------------------------------------------ }

constructor TCWPie.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPieSize := 500;
end;

destructor TCWPie.Destroy;
begin
  inherited;
end;

procedure TCWPie.AssignGraph(Source: TCWGraph);
begin
  if Source is TCWPie then
  begin
    FOptions := TCWPie(Source).Options;
    FSliceSpacing := TCWPie(Source).SliceSpacing;
    FTitleSpace := TCWPie(Source).FTitleSpace;
    FPieSize := TCWPie(Source).PieSize;
  end;
  inherited;
end;

procedure TCWPie.SetOptions(Value: TPieOptions);
begin
  if FOptions = Value then
    Exit;
  FOptions := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetSliceSpacing(Value: single);
begin
  if (Value > 2) or (Value < 0) or (Value = FSliceSpacing) then
    Exit;
  FSliceSpacing := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetPieSize(Value: integer);
begin
  if (Value < 100) or (Value = FPieSize) then
    Exit;
  FPieSize := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetDoughnutSize(Value: integer);
begin
  if Value = FDoughnutSize then
    Exit;
  if (Value <> 0) then
  begin
    if not (Value in [10 .. 90]) then
      ShowGWError(msg_DoughnutRange);
  end;
  FDoughnutSize := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.Draw;
var
  i: integer;
  PSize: integer;
  StartPt: TPoint;
const
  Spacing = 10;

  procedure DoDraw(ASource: TSeries);
  var
    GPen: TGPPen;
    GBrush: TGPSolidBrush;
    SeritIndx: integer;

    procedure SetTheColor(Clr: TColor);
    var
      R, G, B: byte;
    begin
      Clr := ColorToRgb(Clr);
      R := GetRValue(Clr);
      G := GetGValue(Clr);
      B := GetBValue(Clr);
      GBrush := TGPSolidBrush.Create(MakeColor(255, R, G, B));
      GPen := TGPPen.Create(MakeColor(255, R, G, B), 0);
    end;

    procedure DrawPieSlice(Serit: TSeriesItems; ItmIndex: integer;
      Angle: single; LastAngle: single; DrawRect: TRect; var Center: TPoint;
      Pst: single);
    var
      L: integer;
      Ang: single;
      TP: TPieSlice;
      RealItmIndex: integer;
      Factor: single;
    begin
      try
        if ASource.FPieState <> 0 then
        begin
          SetTheColor(clGray);
        end
        else if Serit[ItmIndex].FSpace then
        begin
          SetTheColor(Writer.GraphBGColor);
        end
        else
        begin
          SetTheColor(ActiveColor[ASource.Index, SeritIndx]);
        end;
        GDIP.FillPie(GBrush, MakeRect(DrawRect.Left, DrawRect.Top,
          DrawRect.Width, DrawRect.Height), LastAngle, Angle);
        GDIP.DrawPie(GPen, MakeRect(DrawRect.Left, DrawRect.Top, DrawRect.Width,
          DrawRect.Height), LastAngle, Angle);
      finally
        GBrush.Free;
        GPen.Free;
      end;

      if ASource.FPieState <> 0 then
        Exit;

      if SliceSpacing <> 0 then
        RealItmIndex := ASource.FTrackPies.Count
      else
        RealItmIndex := ItmIndex;

      if not Serit[ItmIndex].FSpace then
      begin
        TP.EndAngel := LastAngle + Angle;
        TP.StartAngel := LastAngle;
        TP.Color := ActiveColor[ASource.Index, SeritIndx];
        TP.FSeriesIndex := ASource.Index;
        TP.FSeriesItmIndex := RealItmIndex;
        TP.Percentage := Pst;
      end;

      if ASource.FPieState <> 0 then
      begin
        Center := Point(DrawRect.Width div 2, DrawRect.Height div 2);
      end
      else
      begin
        L := DrawRect.Width div 2;
        Angle := LastAngle + (Angle / 2);
        Ang := DegToRad(Angle);
        if FDoughnutSize > 0 then
        begin
          Factor := 2 - 1 / 100 * (FDoughnutSize + 10);
          Center.X := DrawRect.Left + L + round(L / Factor * Cos(Ang));
          Center.Y := DrawRect.Top + L + round(L / Factor * Sin(Ang));
        end
        else
        begin
          Center.X := DrawRect.Left + L + round((L / 2) * Cos(Ang));
          Center.Y := DrawRect.Top + L + round((L / 2) * Sin(Ang));
        end;
      end;

      if not Serit[ItmIndex].FSpace then
      begin
        TP.Center := Center;
        TP.Color := ActiveColor[ASource.Index,SeritIndx];
        ASource.FTrackPies.Add(TP);
      end;
      Writer.ResetCanvas;
    end; { End DrawPieSlice }

    procedure CreateSpaces(Items: TSeriesItems);
    var
      Sp: single;
      Temp: TSeriesItems;
      i: integer;
      Itm: TSeriesItem;
    begin
      Sp := SliceSpacing;
      Temp := TSeriesItems.Create(False);
      for i := 0 to Items.Count - 1 do
      begin
        Itm := TSeriesItem.Create;
        Itm.Assign(Items[i]);
        Itm.FValue := Itm.FValue - Sp;
        Itm.FSpace := False;
        Temp.Add(Itm);

        Itm := TSeriesItem.Create;
        Itm.Assign(Items[i]);
        Itm.FValue := Sp;
        Itm.FSpace := True;
        Itm.FName := '';
        Temp.Add(Itm);
      end;

      Items.Clear;
      Items.AddRange(Temp);
      Temp.Free;
    end; { End CraeteSpaces }

    procedure DrawPie;
    var
      i: integer;
      Pst: single;
      a: single;
      LastAngl: single;
      R: TRect;
      DrawRect: TRect;
      Center: TPoint;
      S: string;
      Serit: TSeriesItems;
      Handled: Boolean;
      Err: Boolean;

      procedure Error;
      var
        TX: TSize;
      begin
        if Serit = nil then
          Exit;

        DrawPieSlice(Serit, 0, 360, 0, DrawRect, Center, 0);
        if ASource.FPieState = -1 then
          S := 'Pie data contains negative numbers'
        else if ASource.FPieState = 1 then
          S := 'Percentage exceedes 100'
        else if ASource.FPieState = -1 then
          S := 'One or more value are negative'
        else if ASource.FPieState = 2 then
          S := 'Too many slices (max 36)'
        else if ASource.FPieState = 3 then
          S := 'Slice too smaal';
        TX := Canvas.TextExtent(S);
        Canvas.Brush.Style := bsClear;
        Canvas.Font.Color := clWhite;
        Canvas.TextOut(DrawRect.CenterPoint.X - TX.cx div 2, DrawRect.CenterPoint.Y - TX.cy div 2, S);
        Err := True;
      end;

      procedure AssignItems(AOwner: TSeries; Source: TSeriesItems;
        ItmsOut: TSeriesItems);
      var
        i: integer;
        Itm: TSeriesItem;
      begin
        for i := 0 to Source.Count - 1 do
        begin
          Itm := TSeriesItem.Create;
          Itm.Value := Source[i].Value;
          Itm.FRealVal := Source[i].FRealVal;
          Itm.Name := Source[i].Name;
          Itm.FOwner := AOwner;
          ItmsOut.Add(Itm);
        end;
      end;

      procedure DrawText(Indx: integer);
      var
        TX: TSize;
        ItmIndx: integer;
        Clr: TColor;
        Itm: TSeriesItem;

      begin
        ItmIndx := ASource.FTrackPies[Indx].FSeriesItmIndex;
        Center := ASource.FTrackPies[Indx].Center;
        Clr := ASource.FTrackPies[Indx].Color;
        Clr := InvertColor(Clr);
        Canvas.Font.Color := Clr;
        Canvas.Font.Size := 8;
        Canvas.Brush.Style := bsClear;
        S := '';
        Itm := ASource.FSeriesItems[ItmIndx];
        if Itm.Value = 0 then
          Exit;
        if (poPrintNames in Options) then
          S := Itm.Name;
        if ((poPrintPercentages in Options)) or Assigned(FOnDrawPieSlice) then
        begin
          S := S + ' ' + FormatNum(ASource.FTrackPies[Indx].Percentage,
            Writer.ValuePrecision) + '%';
        end;
        TX := Canvas.TextExtent(S);
        Handled := False;
        if Assigned(FOnDrawPieSlice) then
        begin
          FOnDrawPieSlice(Self, ASource.Index, Indx, Canvas, Handled);
        end;
        if not Handled and (S <> '') then
        begin
          Canvas.TextOut(Center.X - TX.cx div 2, Center.Y - TX.cy div 2, S);
        end;

      end;

      procedure DrawDoughnut(PieRect: TRect);
      var
        wd, L, T: integer;
      begin
        wd := round(PieRect.Width * FDoughnutSize / 100);
        L := PieRect.CenterPoint.X - wd div 2;
        T := PieRect.CenterPoint.Y - wd div 2;
        SetTheColor(Writer.GraphBGColor);
        GDIP.FillEllipse(GBrush, MakeRect(L, T, wd, wd));
        GDIP.DrawEllipse(GPen, MakeRect(L, T, wd, wd));
        ASource.FDoughnutRect := Rect(L, T, wd, wd);
      end;

      procedure SetDrawRect;
      var
        L, T: integer;
      begin
        if R.Width >= R.Height then
        begin
          L := StartPt.X + (FNumPaint - 1) * PSize + (FNumPaint - 1) * Spacing;
          T := StartPt.Y;
        end
        else
        begin

          L := StartPt.X;
          T := StartPt.Y + (FNumPaint - 1) * PSize + (FNumPaint - 1) * Spacing +
            FNumPaint * FTitleSpace;
        end;

        DrawRect.Left := L;
        DrawRect.Top := T;
        DrawRect.Right := DrawRect.Left + PSize;
        DrawRect.Bottom := T + PSize;
        ASource.FPieRect := DrawRect;
      end;

    begin { Draw Pie }
      R := Writer.GraphRect;
      ASource.FTrackPies.Clear;
      LastAngl := 0;
      Serit := nil;

      Serit := TSeriesItems.Create;
      if TCWPieChart(Writer.Chart).CalcPercentages then
        CalcPst(ASource, ASource.FSeriesItems, Serit)
      else
        AssignItems(ASource, ASource.FSeriesItems, Serit);
      SetDrawRect;
      if SliceSpacing > 0 then
      begin
        CreateSpaces(Serit);
      end;

      try
        Err := False;
        if ASource.FPieState in [0, 3] then
        begin
          { Draw the slices }
          if ASource.FPieState = 3 then
            ASource.FPieState := 0;
          SeritIndx := 0;
          for i := 0 to Serit.Count - 1 do
          begin
            Pst := Serit[i].Value;
            a := Pst * 360 / 100;
            if (floor(a + LastAngl) > 360) then
            begin
              ASource.FPieState := 3;
              Error;
              Break;
            end;
            DrawPieSlice(Serit, i, a, LastAngl, DrawRect, Center,
              Pst + SliceSpacing);

            if not Serit[i].FSpace then
            begin
              inc(SeritIndx);
            end;
            LastAngl := LastAngl + a;
          end;
          for i := 0 to ASource.FTrackPies.Count - 1 do
          begin
            DrawText(i);
          end;
        end
        else
        begin
          Error;
        end;
        if FDoughnutSize > 0 then
          DrawDoughnut(DrawRect);
      finally
        Serit.Free;
      end;

      Writer.ResetCanvas;
    end; { End DrawPie }

  begin { DoDraw }
    DrawPie;
  end;

  procedure ComputeSize;
  { Compute the pie size. Original value cannot be held if space not allows }
  var
    Cnt: integer;
    R: TRect;
    MaxS, MaxW, MaxH: integer;
    TotWidth: integer;

  begin
    Cnt := Writer.VisibleCount;
    R := Writer.GraphRect;
    if R.Width >= R.Height then
    begin
      MaxW := R.Width div Cnt - (Spacing * Cnt);
      MaxH := R.Height - Spacing * 2 + FTitleSpace;
      if MaxW > MaxH then
        MaxS := MaxH
      else
        MaxS := MaxW;
      PSize := FPieSize;
      if PSize > MaxS then
        PSize := MaxS;
      TotWidth := PSize * Cnt + Cnt * Spacing - Spacing;
      StartPt.X := R.Left + (R.Width - TotWidth) div 2;
      StartPt.Y := R.CenterPoint.Y - PSize div 2 + FTitleSpace;
    end
    else
    begin
      MaxW := R.Width - (Spacing * Cnt);
      MaxH := R.Height div Cnt - Spacing * 2 - FTitleSpace;
      if MaxW > MaxH then
        MaxS := MaxH
      else
        MaxS := MaxW;
      PSize := FPieSize;
      if PSize > MaxS then
        PSize := MaxS;
      TotWidth := PSize * Cnt + Cnt * (Spacing + FTitleSpace) - Spacing;
      StartPt.Y := R.Top + (R.Height - TotWidth) div 2;
      StartPt.X := R.CenterPoint.X - PSize div 2;
    end;
  end;

  procedure DrawTitle(SeriesIndex : integer);
  var
    R : TRect;
    S : string;
    X, Y : integer;
  begin
    R := Writer.Series[SeriesIndex].PieTitleRect;
    S := Writer.Series[SeriesIndex].Title;
    X := R.CenterPoint.X - Canvas.TextWidth(S) div 2;
    Y := R.Top + 5;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(X, Y, S);
  end;

begin
  if Writer = nil then
    Exit;
  if not InChart then
    Exit;
  FTitleSpace := GetTitleSpace;
  FNumPaint := 0;
  inherited Draw;
  ComputeSize;
  try
    for i := 0 to Writer.Count - 1 do
      if (Series[i].Visible) and InChart(i) then
      begin
        inc(FNumPaint);
        DoDraw(Series[i]);
        if (poPrintSeriesTitles in Options) and (FTitleSpace > 0) and (Writer.Series[i].Title <> '') then
          DrawTitle(i);
      end;
  finally
    FGDIP.Free;
  end;
end;

function TCWPie.GetActiveColor(SeriesIndex, ItemIndex : integer): TColor;
var
  R , G, B : Byte;
  S : string;
  Indx : integer;
begin
  Result := clBlack;
  if Writer = nil then
   Exit;
  if ItemIndex > Writer.Chart.ItemColors.Count-1 then
  begin
    if csDesigning in ComponentState then
    begin
      R := Random(255);
      G := Random(255);
      B := Random(255);
      Result := RGB(R, G, B);
    end
    else
     Result := Writer.FSeriesData[SeriesIndex].SeriesItems[ItemIndex].Color;
  end
  else
  begin
   S := Writer.Series[SeriesIndex].SeriesItems[ItemIndex].FName;
   Indx := Writer.Chart.ItemColors.IndexOf(S);
   if Indx <> -1 then
     Result := Writer.Chart.ItemColors.Items[Indx].Color
   else
     Result := Writer.Chart.ItemColors.Items[ItemIndex].Color;
  end;
end;

function TCWPie.GetActualSize: integer;
begin
  Result := FPieSize;
  if (Writer.Count > 0) then
    Result := Series[0].PieRect.Width;
end;

function TCWPie.GetTitleSpace : integer;
var
 i : integer;
 F : TFont;
 H : integer;
begin
  Result := 0;
  if not (poPrintSeriesTitles in Options) then
    Exit;
  F := TFont.Create;
  try
   F.Assign(Canvas.Font);
   Canvas.Font.Assign(Writer.NameFont);

   for I := 0 to Writer.Chart.SeriesDefs.Count-1 do
   begin
     if Writer.Chart.SeriesDefs[i].Title <> '' then
     begin
       H := Canvas.TextHeight(Writer.Chart.SeriesDefs[i].Title) + 10;
       if H > Result then
         Result := H;
     end;
   end;
  finally
    Canvas.Font.Assign(F);
    F.Free;
  end;

end;

{ TCWSeriesDef ---------------------------------------------------- }

constructor TCWSeriesDef.Create(Collection: TCollection);
begin
  FVisible := True;
  inherited;
end;

procedure TCWSeriesDef.Assign(Source: TPersistent);
var
  wSrc: TCWSeriesDef;
begin
  if Source is TCWSeriesDef then
  begin
    wSrc := TCWSeriesDef(Source);
    FGraph := wSrc.Graph;
    FTitle := wSrc.Title;
    FColor := wSrc.Color;
    FValueAxis := wSrc.ValueAxis;
  end
  else
    inherited;
end;

function TCWSeriesDef.GetWriter: TChartWriter;
begin
  Result := TCWChart(Collection.Owner).Writer;
end;

function TCWSeriesDef.GetChart: TCWChart;
begin
  Result := TCWChart(Collection.Owner);
end;

function TCWSeriesDef.ActualAxis : TValueAxis;
begin
  Result := nil;
  if Writer = nil then
   Exit;
  if ValueAxis = vaValueAxis1 then
    Result := Writer.FValAx
  else
    Result := Writer.FValAx2;
end;

procedure TCWSeriesDef.SetVisible(Value : Boolean);
begin
   if Value = FVisible then
    Exit;
   FVisible := Value;
   if Writer = nil then
     Exit;
   if not (csLoading in Chart.ComponentState) then
   begin
    Writer.FActiveValAx := ActualAxis;
    if Writer.ValueSpanFromData then
    begin
      Writer.SetHighLow;
    end;
    Writer.RefreshGraph;
   end;
end;

procedure TCWSeriesDef.SetValueAxis(Value: TValueAxisNumber);
var
 i : integer;
 OneFound : Boolean;
begin
  if FValueAxis = Value then
    Exit;
  if Chart is TCWPieChart then
  begin
    if TCWSeriesDefs(Collection).Items[0].Graph is TCWPie then
      Value := vaNone
    else
      Value := vaValueAxis1;
    FValueAxis := value;
    Exit {Always vNone or va1}
  end
  else if Value = vaNone then
    Exit;
  if Value = vaValueAxis2 then
  begin
    OneFound := false;
    with Collection as TCWSeriesDefs do
    for I := 0 to Count-1 do
     if (Items[i].ValueAxis = vaValueAxis1) and (I <> Index) then
       OneFound := True;
    if not OneFound then
      Value := vaValueAxis1;
  end;
  FValueAxis := Value;
  if Writer = nil then
    Exit;
  if not (csLoading in Chart.ComponentState) then
   begin
    Writer.RefreshGraph;
   end;
end;

procedure TCWSeriesDef.SetColor(Value : TColor);
begin
  if Value = FColor then
   Exit;
  FColor := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWSeriesDef.GetDisplayName: string;
begin
  if Title <> '' then
    Result := Title
  else if FGraph <> nil then
    Result := FGraph.Name
  else
    inherited;
end;

procedure TCWSeriesDef.SetGraph(Value: TCWGraph);
var
  i: integer;
begin
  if FGraph = Value then
    Exit;
  if csLoading in Chart.componentState then
  begin
    FGraph := Value;
    Value.FWID := AddToWList(Chart.Writer,  Value);
    Exit;
  end;
  if (Value is TCWPie) and (Chart.NameType <> ntGeneral) then
    ShowGWError(msg_PieGeneral);
  if (Chart is TCWAxisChart) and (Value is TCWPie) then
    Exit
  else if (Chart is TCWPieChart) then
  begin
  if (Value is TCWCurve)
  or ((Value is TCWBar) and (TCWBar(Value).ColorUsage <> cuOnItems)) then
    Exit;
  end;

  Value.FWID := AddToWList(Chart.Writer,  Value);

  FGraph := Value;
  if Value is TCWPie then
  begin
    with Collection as TCWSeriesDefs do
    for I := 0 to Count-1 do
     //if not (Items[i].Graph = Value) then
     begin
       Items[i].FGraph := Value;
       Items[i].FValueAxis := vaNone;
     end;
  end
  else if Value is TCWBar then
  begin
    with Collection as TCWSeriesDefs do
    begin
      for I := 0 to Count-1 do
      begin
      if Chart is TCWPieChart then
      begin
        if Value is TCWBar then
        begin
          Items[i].FGraph := Value;
          Items[i].FValueAxis := vaValueAxis1;
        end;
      end
      else
       if (Items[i].Graph is TCWBar) and not (Items[i].Graph = Value)then
         Items[i].FGraph := Value;
      end;
    end;
  end
  else
  begin
    with Collection as TCWSeriesDefs do
    for I := 0 to Count-1 do
     if not (Items[i].Graph is TCWAxisGraph) then
       Items[i].FGraph := Value;
  end;
  if (FGraph = nil) and (Writer <> nil) then
  begin
    Writer.Invalidate;
  end;
  if Chart.IsActive then
  begin
    if csDesigning in Writer.ComponentState then
    begin
      Writer.RenderDesigner;
    end
    else
      Writer.RefreshGraph;
    end;
end;

procedure TCWSeriesDef.SetTitle(Value : string);
begin
  if Value = FTitle then
   Exit;
  FTitle := Value;
  if Chart.IsActive and not (csLoading in Writer.ComponentState) then
  begin
    if Chart.Legends <> nil then
      Writer.RefreshGraph;
  end;
end;

function TCWSeriesDef.GetTitle : string;
begin
   Result := FTitle;
   if (Chart = nil) or not Assigned(Chart.FOnTitle) then
     Exit;
   Chart.FOnTitle(Chart, Index, Result);
end;

{ TCWSeriesDefs --------------------------------------------------- }

function TCWSeriesDefs.GetItem(AIndex: integer): TCWSeriesDef;
begin
  Result := TCWSeriesDef(inherited Items[AIndex]);
end;

procedure TCWSeriesDefs.SetItem(AIndex: integer; const Value: TCWSeriesDef);
begin
  inherited SetItem(AIndex, Value);
end;

procedure TCWSeriesDefs.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  if (Action =  cnAdded) and (TCWChart(Owner) is TCWPieChart) then
    with Item as TCWSeriesDef do
      FValueAxis := vaNone;
  inherited;
end;

procedure TCWSeriesDefs.Update(Item : TCollectionItem);
begin
  if (csLoading in TCWChart(Owner).ComponentState)
  or (csDestroying in TCWChart(Owner).ComponentState) then
    Exit;

  if not TCWChart(Owner).IsActive then
   Exit;

  if (csDesigning in TCWChart(Owner).ComponentState) then
    TCWChart(Owner).Writer.RenderDesigner
  else
    TCWChart(Owner).Writer.RefreshGraph;
end;

function TCWSeriesDefs.Add: TCWSeriesDef;
begin
  Result := TCWSeriesDef(inherited Add);
end;

function TCWSeriesDefs.Insert(Index: integer): TCWSeriesDef;
begin
  Result := TCWSeriesDef(inherited Insert(Index));
end;

procedure TCWSeriesDefs.Assign(Source: TPersistent);
var
  wSrc: TCWSeriesDefs;
  loop: integer;
begin
  if (Source is TCWSeriesDefs) then
  begin
    wSrc := TCWSeriesDefs(Source);
    Clear;
    for loop := 0 to wSrc.Count - 1 do
      Add.Assign(wSrc.Items[loop]);
  end
  else
    inherited;
end;

function TCWSeriesDefs.IndexOf(const AGraph: TCWGraph): integer;
var
  loop: integer;
begin
  Result := -1;
  loop := 0;
  while (Result = -1) and (loop < Count) do
  begin
    if Items[loop].Graph = AGraph then
      Result := loop
    else
      inc(loop);
  end;
end;

function TCWSeriesDefs.IndexOfTitle(ATitle : string) : integer;
var
  loop: integer;
begin
  Result := -1;
  loop := 0;
  while (Result = -1) and (loop < Count) do
  begin
    if SameText(Items[loop].Title, ATitle) then
      Result := loop
    else
      inc(loop);
  end;
end;

{ TCWSectionDefs ----------------------------------------------------- }

constructor TCWSectionDefs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSections := TCWSectionItems.Create(Self, TCWSectionItem);
  FFont := TFont.Create;
  FPen := TPen.Create;
  FPen.Color := clSilver;
  FShowLines := True;
  FVisible := True;
  FCaptionLayout := clOppositeSideOfLabels;
  FPen.OnChange := GraphChanged;
  FFont.OnChange := GraphChanged;
end;

destructor TCWSectionDefs.Destroy;
begin
  FSections.Free;
  FFont.Free;
  FPen.Free;
  inherited;
end;

procedure TCWSectionDefs.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if csDestroying in ComponentState then
  begin
    inherited;
    Exit;
  end;
  if Operation <> opRemove then
  begin
    inherited;
    Exit;
  end;
end;

(*procedure TCWSectionDefs.SetWriter(Value : TChartWriter);
begin
  if Value = FWriter then
    Exit;
  FWriter := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;*)

function TCWSectionDefs.GetWriter : TChartWriter;
begin
  Result := GetWriterFromWList(FWID);
//  Result := nil;
//  if Chart <> nil then
//    Result := Chart.Writer;
end;

function TCWSectionDefs.GetSectionType: TNameSectionType;
begin
  Result := stUndefined;
  if Self is TCWValueSectionDefs then
  begin
    if Sections.Count > 0 then
      Result := stLiterals;
  end
  else
    with Self as TCWNameSectionDefs do
    begin
      if AutoSections <> autNotUsed then
        Result := stAutoSections
      else if (DateTimeTemplate = ttLiterals) and (Sections.Count > 0) then
      begin
          Result := stLiterals;
      end
      else if (Sections.Count > 0 ) and (DateTimeTemplate <> ttNotUsed) then
        Result := stDateTimeTemplate;
    end;
end;

function TCWSectionDefs.GetOwnerChart : TCWChart;
begin
  Result := nil;
  if Writer <> nil then
    Result := Writer.Chart;
end;

procedure TCWSectionDefs.SetCaptionHorzMargin(Value: integer);
begin
  if Value < 0 then
    Exit;
  if Value > 50 then
    Exit;
  FCaptionHorizMargin := Value;
  if (Writer <> nil) and not(csLoading in Writer.componentState) then
  begin
    Writer.RefreshGraph;
  end;
end;

procedure TCWSectionDefs.SetCaptionVertMargin(Value: integer);
begin
  if Value < 0 then
    Exit;
  if Value > 50 then
    Exit;
  FCaptionVertMargin := Value;
  if (Writer <> nil) and not (csLoading in Writer.ComponentState) then
    Writer.RefreshGraph;
end;

procedure TCWSectionDefs.SetVisible(Value: Boolean);
begin
  if Value = FVisible then
    Exit;
  FVisible := Value;
  if Writer <> nil then
  begin
    if csLoading in Writer.ComponentState then
     Exit;
    if not FVisible then
    begin
      if Self is TCWNameSectionDefs then
        Writer.ClearSections(atNameAxis, False)
      else
        Writer.ClearSections(atValueAxis, False);
    end;
    Writer.RefreshGraph;
  end;
end;

procedure TCWSectionDefs.SetProlongedLines(Value : Boolean);
begin
  if Value = FProlongedLines then
   Exit;
  FProlongedLines := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWSectionDefs.SetShowLines(Value : Boolean);
begin
  if Value = FShowLines then
   Exit;
  FShowLines := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWSectionDefs.GraphChanged(Sender : TObject);
begin
  if csLoading in ComponentState then
    Exit;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWSectionDefs.SetCaptionLayout(Value : TCaptionLayout);
begin
  if Value = FCaptionLayout then
    Exit;
  FCaptionLayout := Value;
  if Writer <> nil then
  begin
   if csLoading in Writer.ComponentState then
     Exit;
   Writer.RefreshGraph;
  end;

end;

procedure TCWSectionDefs.DoCheck;
var
  i: integer;
begin
  if SectionType = stUndefined then
    Exit;

  Sections.CheckEmpties;
  for i := 0 to Sections.Count - 1 do
    begin
      Sections.Items[i].CheckDataType(Sections.Items[i].FStartValue);
      Sections.Items[i].CheckDataType(Sections.Items[i].FEndValue);
    end;
end;

{ TCWSectionItems --------------------------------------------------- }

function TCWSectionItems.Add: TCWSectionItem;
begin
  Result := TCWSectionItem(inherited Add);
end;

function TCWSectionItems.GetItem(AIndex: integer): TCWSectionItem;
begin
  Result := TCWSectionItem(inherited Items[AIndex]);
end;

procedure TCWSectionItems.SetItem(AIndex: integer; const Value: TCWSectionItem);
begin
  inherited SetItem(AIndex, Value);
end;

procedure TCWSectionItems.CheckEmpties;
var
  i: integer;
  Itm: TCWSectionItem;
  Err1, Err2: Boolean;
begin
  Err1 := False;
  Err2 := False;
  if Owner is TCWNameSectionDefs then
  begin
    if (TCWNameSectionDefs(Owner).SectionType = stAutoSections) or
      (TCWNameSectionDefs(Owner).SectionType = stUndefined) then
      Exit;
  end;

  for i := 0 to Count - 1 do
  begin
    Itm := Items[i];
    if (Itm.FStartValue = '') then
    begin
      Err1 := True;
      Break;
    end;
    if (Itm.FEndValue = '') then
    begin
      Err2 := True;
      Break;
    end;
  end;
  if Err1 then
    ShowGWError(msg_SectStartValue)
  else if Err2 then
    ShowGWError(msg_SectEndValue);
end;

procedure TCWSectionItems.Assign(Source: TPersistent);
var
  wSrc: TCWSectionItems;
  loop: integer;
begin
  if (Source is TCWSectionItems) then
  begin
    wSrc := TCWSectionItems(Source);
    Clear;
    for loop := 0 to wSrc.Count - 1 do
      Add.Assign(wSrc.Items[loop]);
  end
  else
    inherited;
end;

{ TCWNameSectionDefs ---------------------------------------------- }

constructor TCWNameSectionDefs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSections := autNotUsed;
end;

procedure TCWNameSectionDefs.CheckConfig;
var
  Err, Err2: Boolean;
  Templ: TDateTimeTemplate;
  Aut: TAutoSections;
  Nt: TNameType;
begin

  if (Writer = nil) or (csLoading in componentState) or
    (SectionType = stLiterals) then
    Exit;
  if Writer.Chart = nil  then
    Exit;
  if not Writer.IsTimeSpan(Writer.Chart.NameType) then
    Exit;
  if SectionType = stUndefined then
    Exit;
  Err := False;
  Err2 := False;
  Nt := Writer.Chart.NameType;

  if SectionType = stDateTimeTemplate then
  begin
    Templ := DateTimeTemplate;
    if Templ = ttNotUsed then
      Exit;
    { These nt spans lack time information }
    if Nt = ntDateSpan then
    begin
      case Templ of
        ttHourTemplate, ttMinuteTemplate, ttSecondTemplate:
          Err := True;
      end;
    end
    else if Nt = ntHourSpan then
    begin
      case Templ of
        ttMinuteTemplate, ttSecondTemplate:
          Err := True;
      end;
    end
    else if Nt = ntMinuteSpan then
    begin
      case Templ of
        ttSecondTemplate:
          Err := True;
      end;
    end;

  end
  else if SectionType = stAutoSections then
  begin
    Aut := TCWNameSectionDefs(Self).AutoSections;
    if Aut = autNotUsed then
      Exit;
    if Nt = ntHourSpan then
    begin
      case Aut of
        { Not practical, ok with weeks }
        autMonths:
          Err2 := True;
        autYears:
          Err2 := True;
      end;
    end
    else if Nt in [ntMinuteSpan, ntSecondSpan] then
    begin
      case Aut of
        { Not practical }
        autWeeks:
          Err2 := True;
        autMonths:
          Err2 := True;
        autYears:
          Err2 := True;
      end;
    end;
  end;

  if Err then
  begin
    ShowGWError(msg_NameTypeTemplate)
  end
  else if Err2 then
  begin
    ShowGWError(msg_NameTypeAutoSect);
  end;
end;

procedure TCWNameSectionDefs.SetDateTimeTemplate(Value: TDateTimeTemplate);
begin
  if FDateTimeTemplate = Value then
    Exit;
  FDateTimeTemplate := Value;
  if FDateTimeTemplate <> ttNotUsed then
    FAutoSections := autNotUsed;
  if Writer <> nil then
    Writer.RefreshGraph;
end;

procedure TCWNameSectionDefs.SetAutoSections(Value: TAutoSections);
begin
  if FAutoSections = Value then
    Exit;
    FAutoSections := Value;
  if FAutoSections <> autNotUsed then
    FDateTimeTemplate := ttNotUsed;
  if Writer <> nil then
    Writer.RefreshGraph;
end;

{ TCWSectionItem ------------------------------------------------- }

procedure TCWSectionItem.Assign(Source: TPersistent);
var
  wSrc: TCWSectionItem;
begin
  if Source is TCWSectionItem then
  begin
    wSrc := TCWSectionItem(Source);
    FStartValue := wSrc.StartValue;
    FEndValue := wSrc.EndValue;
    FLongCaption := wSrc.LongCaption;
    FShortCaption := wSrc.ShortCaption;
  end
  else
    inherited;
end;

function TCWSectionItem.GetDisplayName: string;
begin
  if LongCaption = '' then
    Result := inherited
  else
    Result := LongCaption;
end;

procedure TCWSectionItem.CheckDataType(Value: string);
var
  Dt: TDateTime;
  n: integer;
  E: single;
  Err: Boolean;
begin
 try
  if (Writer = nil) or (csLoading in Section.componentState) then
    Exit;
  if Section is TCWValueSectionDefs then
  begin
    if Section is TCWValueSectionDefs then
    begin
      if not TryStrToFloat(Value, E, Fmt) then
        ShowGWError(msg_InvalidNumber, Value);
    end
  end
  else if Section.Writer.IsTimeSpan(Writer.Chart.NameType)
  and (Section.SectionType = stLiterals) then
  begin
      if not TryStrToDateTime(Value, Dt, Fmt) then
        ShowGWError(msg_InvalidDateTime, Value);
  end
  else if Writer.Chart.NameType = ntNumberSpan then
  begin
      if not TryStrToInt(Value, n) then
        ShowGWError(msg_InvalidNumber, Value);
  end
  else if Section.SectionType = stDateTimeTemplate then
  begin
    Err := False;
    if not TryStrToInt(Value, n) then
      ShowGWError(msg_TemplateIntegers, Value);
    with Section as TCWNameSectionDefs do
      case DateTimeTemplate of
        ttMonthTemplate:
          Err := ((n < 1) or (n > 12));
        ttWeekTemplate:
          Err := ((n < 1) or (n > 53));
        ttDateTemplate:
          Err := ((n < 1) or (n > 366));
        ttHourTemplate:
          Err := ((n < 0) or (n > 23));
        ttMinuteTemplate:
          Err := ((n < 0) or (n > 59));
        ttSecondTemplate:
          Err := ((n < 0) or (n > 59));
      end;
    if Err then
      ShowGWError(msg_SectValueScope, Value);
  end;
 except
  raise;
 end;
end;

procedure TCWSectionItem.SetStartValue(Value: string);
begin
  if (Writer = nil) or (csLoading in Section.componentState) then
  begin
    FStartValue := Value;
    Exit;
  end;
  CheckDataType(Value);
  FStartValue := Value;
  if Section is TCWNameSectionDefs then
    TCWNameSectionDefs(Section).FDateTimeTemplate := ttNotUsed;
end;

procedure TCWSectionItem.SetEndValue(Value: string);
begin
  if (Writer = nil) or (csLoading in Section.componentState) then
  begin
    FEndValue := Value;
    Exit;
  end;
  CheckDataType(Value);
  FEndValue := Value;
  if Section is TCWNameSectionDefs then
    TCWNameSectionDefs(Section).FDateTimeTemplate := ttNotUsed;
end;

procedure TCWSectionItem.SetLongCaption(Value : string);
begin
  if FLongCaption = Value then
    Exit;
  FLongCaption := Value;
  if Section is TCWNameSectionDefs then
    TCWNameSectionDefs(Section).FDateTimeTemplate := ttNotUsed;
end;

procedure TCWSectionItem.SetShortCaption(Value : string);
begin
  if FShortCaption = Value then
    Exit;
  FShortCaption := Value;
  if Section is TCWNameSectionDefs then
    TCWNameSectionDefs(Section).FDateTimeTemplate := ttNotUsed;
end;

function TCWSectionItem.GetWriter: TChartWriter;
begin
  Result := TCWSectionDefs(Collection.Owner).Writer;
end;

function TCWSectionItem.GetSection: TCWSectionDefs;
begin
  Result := Collection.Owner as TCWSectionDefs;
end;

{ TCWLegends collection ------------------------------------------- }

function TCWLegends.GetItem(AIndex: integer): TCWLegendItem;
begin
  Result := TCWLegendItem(inherited Items[AIndex]);
end;

procedure TCWLegends.SetItem(AIndex: integer; const Value: TCWLegendItem);
begin
  inherited SetItem(AIndex, Value);
end;

function TCWLegends.Add: TCWLegendItem;
begin
  Result := TCWLegendItem(inherited Add);
end;

function TCWLegends.Insert(Index: integer): TCWLegendItem;
begin
  Result := TCWLegendItem(inherited Insert(Index));
end;

function TCWLegends.IndexOf(ALegend: TCWLegend): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if ALegend = Items[i].Legend then
    begin
      Result := i;
      Break;
    end;
end;

function TCWLegends.WidestLegend(APosition: TAxisPosition): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].FLegend <> nil) and (Items[i].FLegend.AxisPosition = APosition)
      and Items[i].FLegend.Visible then
    begin
      if APosition in [apRight, apLeft] then
      begin
        if Items[i].FLegend.InsideGraph then
          Continue;
        if Items[i].FLegend.FWidth > Result then
          Result := Items[i].FLegend.FWidth;
      end
      else
      begin
        if Items[i].FLegend.InsideGraph then
          Continue;
        if Items[i].FLegend.FHeight > Result then
          Result := Items[i].FLegend.FHeight;
      end;
    end;
  end;
end;

procedure TCWLegends.Update(Item : TCollectionItem);
begin
  with Owner as TCWChart do
    if Writer <> nil then
      Writer.RefreshGraph;
  inherited;
end;

{ TCWLegendItem -------------------------------------------------- }

constructor TCWLegendItem.Create(Collection: TCollection);
begin
  inherited;
end;

procedure TCWLegendItem.SetLegend(Value: TCWLegend);
var
  i: integer;
  L: TCWLegendItem;
  Lgs : TCWLegends;
  C : TCWChart;
begin

  Lgs := Collection as TCWLegends;
  C := Lgs.Owner as TCWChart;

  if csLoading in C.ComponentState then
  begin
    FLegend := Value;
    FLegend.FWID := AddToWList(C.Writer,  Value);
    Exit;
  end;

  if Value <> nil then
    for i := 0 to Collection.Count - 1 do
    begin
      L := TCWLegendItem(Collection.Items[i]);
      if L.Legend = Value then
        ShowGWError(msg_DupLegend);
    end;

  FLegend := Value;
  FLegend.FWID := AddToWList(C.Writer,  Value);

  if FLegend <> nil then
  if FLegend.Writer <> nil then
  begin
    FLegend.Writer.RefreshGraph;
  end;

end;

function TCWLegendItem.GetDisplayName: string;
begin
  if FLegend <> nil then
    Result := FLegend.Name
  else
    Result := inherited;
end;

{ TCWLegend ------------------------------------------------------- }

constructor TCWLegend.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FVertMargins := 5;
  FHorizMargins := 5;
  FContents := [coSeriesTitle];
  FBullets := lbSquare;
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FContentList := TLegContentList.Create;
  FText := TStringList.Create;
  FAnchoring := anRightOutside;
  FPointOptions := [poShowConnectionLine, poEnlargePoint];
  FColoredText := True;
  FTransparency := 255;
  FVisible := True;
end;

destructor TCWLegend.Destroy;
begin
  Font.Free;
  Brush.Free;
  FContentList.Free;
  FText.Free;
  inherited;
end;

procedure TCWLegend.Assign(Source: TPersistent);
var
  L: TCWLegend;
begin
  if not(Source is TCWLegend) then
    inherited
  else
  begin
    L := Source as TCWLegend;
    FContents := L.Contents;
    FContentFlow := L.ContentFlow;
    FText.Assign(L.Text);
    FBullets := L.Bullets;
    FBorder := L.Border;
    FFont.Assign(L.Font);
    FBrush.Assign(L.Brush);
    FWidth := L.FWidth;
    FTextWidth := L.FTextWidth;
    FTextHeight := L.FTextHeight;
    FHeight := L.FHeight;
    FContentList.Clear;
    FLineHeight := L.FLineHeight;
    FVertMargins := L.VertMargins;
    FHorizMargins := L.HorizMargins;
    FAnchoring := L.Anchoring;
    FAlignment := L.Alignment;
    FPointLocating := PointLocating;
    FPointName := L.PointName;
    FPointValue := L.PointValue;
    FPointSeriesIndex := L.PointSeriesIndex;
    FPointItemIndex := L.PointItemIndex;
    FVisible := L.Visible;
  end;
end;

procedure TCWLegend.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if csDestroying in ComponentState then
  begin
    inherited;
    Exit;
  end;
  if Operation <> opRemove then
  begin
    inherited;
    Exit;
  end;
end;

function TCWLegend.GetCanvas;
begin
  Result := nil;
  if Writer <> nil then
    Result := Writer.Canvas;
end;

function TCWLegend.GetLeft: integer;
  function GetRelLeft: integer;
  var
    AL: TLegendAlignment;
  begin
    AL := Alignment;
    if AL = laLeftOrTop then
      Result := Writer.GraphRect.Left
    else if AL = laRightOrBottom then
      Result := Writer.GraphRect.Right - FWidth
    else
    begin
      Result := Writer.GraphRect.CenterPoint.X - FWidth div 2;
    end;
  end;

begin
  Result := 0;
  if Writer = nil then
    Exit;
  case Anchoring of
    anLeftInside, anLeftOutside:
      begin
        if InsideGraph then
          Result := Writer.GraphRect.Left
        else
          Result := Writer.WorkRect.Left
      end;
    anTopInside, anTopOutside:
      Result := GetRelLeft;
    anRightInside, anRightOutside:
      if InsideGraph then
        Result := Writer.GraphRect.Right - FWidth
      else
        Result := Writer.WorkRect.Right - Chart.Legends.WidestLegend(AxisPosition);
    anBottomInside, anBottomOutside:
      Result := GetRelLeft;
    anSeriesOutside:
      if Writer.FNamAx.IsXAxis then
        Result := Writer.WorkRect.Right - Chart.Legends.WidestLegend(AxisPosition);
    anPointInside:
      ;
  end;
end;

function TCWLegend.GetTop: integer;

  function GetRelTop: integer;
  var
    AL: TLegendAlignment;
  begin
    AL := Alignment;
    if AL = laLeftOrTop then
      Result := Writer.GraphRect.Top
    else if AL = laRightOrBottom then
      Result := Writer.GraphRect.Bottom - FHeight
    else
      Result := Writer.GraphRect.CenterPoint.Y - FHeight div 2;
  end;

begin
  Result := 0;
  if Writer = nil then
    Exit;
  case Anchoring of
    anLeftInside, anLeftOutside:
      begin
        Result := GetRelTop;
      end;
    anTopInside, anTopOutside:
      begin
        if InsideGraph then
          Result := Writer.GraphRect.Top
        else
          Result := Writer.WorkRect.Top + Writer.GetTitleSpace;
      end;
    anRightInside, anRightOutside:
      begin
        Result := GetRelTop;
      end;
    anBottomInside, anBottomOutside:
      begin
        if InsideGraph then
          Result := Writer.GraphRect.Bottom - FHeight
        else
          Result := Writer.WorkRect.Bottom - FHeight;
      end;
    anSeriesOutside:
      Result := Writer.WorkRect.Bottom - FHeight;
  end;
end;

function TCWLegend.GetWidth: integer;
begin
  Result := FWidth;
end;

function TCWLegend.GetHeight: integer;
begin
  Result := FHeight;
end;

function TCWLegend.GetInsideGraph: Boolean;
begin
  Result := Anchoring in [anLeftInside, anTopInside, anRightInside,
    anBottomInside, anPointInside];
end;

procedure TCWLegend.SetPointItemIndex(Value: integer);
begin
  if Value < 0 then
    Exit;
  FPointItemIndex := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWLegend.GetWriter : TChartWriter;
begin
  Result := GetWriterFromWList(FWID);
  //Result := nil;
  //if FChart <> nil then
  //  Result := FChart.Writer;
end;

procedure TCWLegend.SetVisible(Value: Boolean);
begin
  if Value = FVisible then
    Exit;
  FVisible := Value;
  if not(csLoading in componentState) and (Writer <> nil) then
  begin
    Writer.RefreshGraph
  end;
end;

procedure TCWLegend.SetTransparency(Value: integer);
begin
  if (Value < 0) or (Value > 255) then
    Exit;
  if Value = FTransparency then
    Exit;
  FTransparency := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWLegend.SetPointSeriesIndex(Value: integer);
begin
  if Value < -1 then
    Value := -1;
  FPointSeriesIndex := Value;
end;

procedure TCWLegend.SetPointValue(Value: string);
var
  E: single;
begin
  if not TryStrToFloat(Value, E, Fmt) then
    raise Exception.Create('Invalid floating point value');
  FPointValue := Value;
end;

procedure TCWLegend.SetPointName(Value: string);
begin
  Value := Trim(Value);
  FPointName := Value;
end;

function TCWLegend.GetOwnerChart: TCWChart;
begin
  Result := nil;
  if Writer <> nil then
    Result := Writer.Chart;
end;

function TCWLegend.AxisPosition: TAxisPosition;
begin
  Result := apLeft;
  case Anchoring of
    anLeftOutside, anLeftInside:
      Result := apLeft;
    anTopOutside, anTopInside:
      Result := apTop;
    anRightOutside, anRightInside:
      Result := apRight;
    anBottomOutside, anBottomInside:
      Result := apBottom;
    anPointInside:
      Result := apRight;
    anSeriesOutside:
      if Writer.FNamAx.IsXAxis then
        Result := apRight
      else
        Result := apBottom;
  end;
end;

procedure TCWLegend.SetText(Value: TStrings);
begin
  FText.Assign(Value);
end;

function TCWLegend.Summary: Boolean;
begin
  Result := Anchoring in [anLeftOutside, anLeftInside, anTopOutside,
    anTopInside, anRightOutside, anRightInside, anBottomOutside,
    anBottomInside];
end;

procedure TCWLegend.CreateContent;
var
  i, j: integer;
  sl: TStringList;
  D1, D2: string;
  W: integer;
  BullWidth: integer;
  BullMarg: integer;
  Elems: TLegendContents;
  ElemFlow: TContentFlow;
  TM: TTextMetric;
  Leading: integer;
  SerIndx, ItmIndx: integer;
  OverflowHorz, OverflowVert: Boolean;
  MaxMinVal: single;
  UseItmClrs: Boolean;
  ClrTag: string;
  S: string;

  function GetPointValue(AValue: string): integer;
  var
    i: integer;
    S: string;
  begin
    Result := -1;
    for i := 0 to Writer.Count - 1 do
    begin
      S := FormatNum(Writer.FSeriesData[i].FSeriesItems[ItmIndx].Value,
        Writer.ValuePrecision);
      if AValue = S then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

  procedure GetNamValIndexes(AName: string; AValue: string);
  begin
    ItmIndx := -1;
    SerIndx := -1;
    ItmIndx := Writer.IndexOfName(AName);
    SerIndx := GetPointValue(AValue);
    if SerIndx = -1 then
      ItmIndx := -1;
    if ItmIndx = -1 then
      SerIndx := -1;
  end;

  procedure ComputeText(Indx: integer);
  var
    i: integer;
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      sl.Assign(Text);
      if OverflowVert then
        Exit;
      if (sl.Count > 0) then
      begin
        for i := 0 to sl.Count - 1 do
        begin
          W := Canvas.TextWidth(sl[i]);
          if W > FWidth then
            FWidth := W;
          FHeight := FHeight + FLineHeight;
          if not Summary then
            FContentList[Indx].Add(sl[i]);
        end;
      end;
    finally
      sl.Free;
    end;
  end;

  function PointMatch(SerIndx, ItmIndx: integer): Boolean;
  var
    NamOk, ValOK: Boolean;
    S: string;
  begin
    Result := True;
    if (Anchoring = anPointInside) and (PointLocating in [plIndexes, plEvent])
    then
    begin
      if (ItmIndx <> -1) then
      begin
        if FPointSeriesIndex = -1 then
          Result := ItmIndx = FPointItemIndex
        else
          Result := (ItmIndx = FPointItemIndex) and
            (SerIndx = FPointSeriesIndex);
      end
      else if FPointSeriesIndex <> -1 then
        Result := SerIndx = FPointSeriesIndex; { Not allowed, actually }
    end
    else if (Anchoring = anPointInside) and (PointLocating = plNameValue) then
    begin
      if PointName <> '' then
      begin
        if (ItmIndx = -1) and (PointName <> '') then
          ItmIndx := Writer.IndexOfName(PointName);
        if ItmIndx = -1 then
          NamOk := False
        else
          NamOk := Writer.FSeriesData[SerIndx].FSeriesItems[ItmIndx].Name = PointName;
      end
      else
        NamOk := True;

      if PointValue <> '' then
      begin
        if ItmIndx = -1 then
        begin
          ItmIndx := Writer.FSeriesData[SerIndx].IndexOfValue
            (StrToFloat(PointValue, Fmt));
          ValOK := ItmIndx <> -1;
        end
        else
        begin
          S := FormatNum(Writer.FSeriesData[SerIndx].FSeriesItems[ItmIndx].Value,
            Writer.ValuePrecision);
          ValOK := S = PointValue;
        end;
      end
      else
        ValOK := True;
      Result := ValOK and NamOk;
      if Result then
      begin
        FPointItemIndex := ItmIndx;
        FPointSeriesIndex := SerIndx;
      end;
    end
    else if (Anchoring = anPointInside) and
      (PointLocating in [plMaxValue, plMinValue]) then
    begin
      if ItmIndx = -1 then
        Exit;
      S := FormatNum(Writer.FSeriesData[SerIndx].FSeriesItems[ItmIndx].Value,
        Writer.ValuePrecision);
      if FPointSeriesIndex = -1 then
      begin
        if PointLocating = plMaxValue then
        begin
          MaxMinVal := Writer.FSeriesData[SerIndx].MaxVal;
        end
        else
          MaxMinVal := Writer.FSeriesData[SerIndx].MinVal;
      end;
      Result := S = FormatNum(MaxMinVal, Writer.ValuePrecision);
    end;
  end;

begin
  if not Visible then
    Exit;
  if (Anchoring = anSeriesOutside) then
    if not Writer.FNamAx.IsXAxis then
      Exit; { Only horiz layout curves supported for this anchoring }
  FWidth := 0;
  FHeight := 0;
  if Writer = nil then
    Exit;
  FContentList.Clear;
  for i := 0 to Writer.Count - 1 do
  begin
    sl := TStringList.Create;
    FContentList.Add(sl);
  end;

  Elems := Contents;
  ElemFlow := ContentFlow;
  if Anchoring = anSeriesOutside then
  begin
    ElemFlow := cfTopBottom;
  end;

  Canvas.Font.Assign(Font);
  FLineHeight := Canvas.TextHeight('X');
  GetTextMetrics(Canvas.Handle, TM);
  Leading := TM.tmInternalLeading;
  inc(FLineHeight, 2);
  if (ElemFlow = cfLeftRight) and not(Elems = []) then
    FHeight := FLineHeight;
  W := 0;
  if Bullets <> lbNone then
  begin
    BullWidth := FLineHeight - Leading;
    BullMarg := 5
  end
  else
  begin
    BullWidth := 0;
    BullMarg := 0;
  end;

  if (Anchoring = anPointInside) and (PointLocating = plEvent) then
  begin
    if not Assigned(FOnLocatePoint) then
      Exit;
    FPointSeriesIndex := -1;
    FPointItemIndex := -1;
    FOnLocatePoint(Self, FPointSeriesIndex, FPointItemIndex);
    if (FPointSeriesIndex < -1) then
      Exit;
    if FPointSeriesIndex > Writer.Count - 1 then
      Exit;
    if FPointItemIndex < 0 then
      Exit;
    if FPointItemIndex > Writer.NameCount - 1 then
      Exit;
  end
  else if (Anchoring = anPointInside) and (PointLocating = plNameValue) then
  begin
    FPointSeriesIndex := -1;
    FPointItemIndex := -1;
  end
  else if (Anchoring = anPointInside) and (PointLocating = plIndexes) then
  begin
    if (FPointSeriesIndex > Writer.Count - 1) or
      (FPointItemIndex > Writer.NameCount - 1) then
      Exit;
  end
  else if (Anchoring = anPointInside) and
    (PointLocating in [plMaxValue, plMinValue]) then
  begin
    if FPointSeriesIndex <> -1 then
    begin
      if PointLocating = plMaxValue then
        MaxMinVal := Writer.FSeriesData[FPointSeriesIndex].MaxVal
      else
        MaxMinVal := Writer.FSeriesData[FPointSeriesIndex].MinVal;
    end;
  end;

  OverflowHorz := False;
  OverflowVert := False;

  for i := 0 to Writer.Count - 1 do
  begin
    if not Writer.Chart.SeriesDefs.Items[i].Visible then
      Continue;
    if (Anchoring = anPointInside) and
      (PointLocating in [plIndexes, plMaxValue, plMinValue, plEvent]) and
      (i <> FPointSeriesIndex) and (FPointSeriesIndex <> -1) then
      Continue;

    { Series title and time span do not repeat (like values and names) }
    if (coSeriesTitle in Elems) then
    begin
        if not PointMatch(i, -1) then
          Continue;
        FContentList[i].Add(Writer.FSeriesData[i].Title);
        W := Canvas.TextWidth(Writer.FSeriesData[i].Title);
        if ElemFlow = cfTopBottom then
        begin
          { In point anchoring, height is computed when drawing }
          FHeight := FHeight + FLineHeight;
          if W > FWidth then
            FWidth := W;
        end
        else
          FWidth := FWidth + W + BullWidth + BullMarg + 5;
    end;

    if (coNameSpan in Elems) and (Writer.IsTimeSpan(Writer.Chart.NameType) or
      (Writer.Chart.NameType = ntNumberSpan)) then
    begin
      if not PointMatch(i, -1) then
        Continue;

      if Writer.IsTimeSpan(Writer.Chart.NameType) then
      begin
        D1 := DateTimeToStr(Writer.FSeriesData[i].FSeriesItems[0].RealDate, Fmt);
        D2 := DateTimeToStr(Writer.FSeriesData[i].FSeriesItems
          [Writer.FSeriesData[i].FSeriesItems.Count - 1].RealDate, Fmt);
        FContentList[i].Add(D1 + ' - ' + D2);
        W := Canvas.TextWidth(D1 + ' - ' + D2);
      end
      else if Writer.Chart.NameType = ntNumberSpan then
      begin
        D1 := Writer.FSeriesData[i].FSeriesItems[0].FOrigName;
        D2 := Writer.FSeriesData[i].FSeriesItems[Writer.FSeriesData[i].FSeriesItems.Count -
          1].FOrigName;
        FContentList[i].Add(D1 + ' - ' + D2);
        W := Canvas.TextWidth(D1 + ' - ' + D2);
      end;
      if (ElemFlow = cfTopBottom) then
      begin
        if W > FWidth then
          FWidth := W;
        FHeight := FHeight + FLineHeight;
      end
      else
        FWidth := FWidth + W + 5;
    end;

    sl := FContentList[i];
    UseItmClrs := Writer.FSeriesData[i].UseItemColors;

    if (coName in Elems) or (coValue in Elems) then
    begin
      for j := 0 to Writer.FSeriesData[i].FSeriesItems.Count - 1 do
      begin
        if Anchoring = anSeriesOutside then
          if j < Writer.FSeriesData[i].FSeriesItems.Count - 1 then
            Continue;

        if not PointMatch(i, j) then
          Continue;

        if not(Anchoring in [anPointInside, anSeriesOutside]) then
        begin
          if (Top + FHeight + FLineHeight > Writer.GraphRect.Bottom) and
            (ElemFlow = cfTopBottom) then
          begin
            if sl.Count > 0 then
              sl[sl.Count - 1] := '...'
            else
              sl.Add('...');
            OverflowVert := True;
            Break;
          end
          else if (Left + FWidth > Writer.GraphRect.Right)
            and (ElemFlow = cfLeftRight) then
          begin
            if sl.Count > 0 then
              sl[sl.Count - 1] := '...'
            else
              sl.Add('...');

            OverflowHorz := True;
            Break;
          end;
        end;

        ClrTag := '';
        if UseItmClrs then
          ClrTag := '[' + IntToStr(j) + ']';

        if coName in Elems then
        begin
          sl.Add(ClrTag + Writer.FSeriesData[i].FSeriesItems[j].Name);
        end;
        if coValue in Elems then
        begin
          if not(coName in Elems) then
            sl.Add(ClrTag + FormatNum(Writer.FSeriesData[i].FSeriesItems[j].Value,
              Writer.ValuePrecision, True))
          else
            sl[sl.Count - 1] := sl[sl.Count - 1] + ': ' +
              FormatNum(Writer.FSeriesData[i].FSeriesItems[j].Value,
              Writer.ValuePrecision, True);
        end;
        if ClrTag <> '' then
        begin
          S := sl[sl.Count - 1];
          Delete(S, 1, Length(ClrTag));
        end
        else
          S := sl[sl.Count - 1];
        W := Canvas.TextWidth(S);
        if ElemFlow = cfTopBottom then
        begin
          if W > FWidth then
            FWidth := W;
          FHeight := FHeight + FLineHeight;
        end
        else
        begin
          if Chart.FSeriesDefs.Items[i].Graph is TCWBar then
          begin
            if TCWBar(Chart.FSeriesDefs.Items[i].Graph).ColorUsage = cuOnItems then
              FWidth := FWidth + W + BullWidth + BullMarg + 5
            else
              FWidth := FWidth + W + 5;
          end
          else if Chart.FSeriesDefs.Items[i].Graph is TCWPie then
          begin
            FWidth := FWidth + W + BullWidth + BullMarg + 5;
          end
          else
            FWidth := FWidth + W + 5;
        end;

        if (Anchoring in [anPointInside, anSeriesOutside]) then
          Break; { Only one point per series }

      end;
    end;
    if not Summary then
    begin
      if (Anchoring = anPointInside) and
        (PointLocating in [plNameValue, plIndexes, plMaxValue, plMinValue,
        plEvent]) then
      begin
        if (i = FPointSeriesIndex) or (FPointSeriesIndex = -1) then
          ComputeText(i);
      end
      else
        ComputeText(i);
    end;
    if OverflowHorz or OverflowVert then
      Break;
  end;

  if Summary then
    ComputeText(-1);
  FTextWidth := FWidth;
  FTextHeight := FHeight;
  if Summary or (Anchoring = anSeriesOutside) then
    FWidth := FWidth + FHorizMargins * 2;
  if FBullets <> lbNone then
    FWidth := FWidth + BullWidth + 5;
  if Summary or (Anchoring = anSeriesOutside) then
    FHeight := FHeight + FVertMargins * 2;
  if FBorder then
  begin
    FWidth := FWidth + leg_BorderMarg * 2;
    FHeight := FHeight + leg_BorderMarg * 2;
  end;
end;

procedure TCWLegend.Draw;
var
  i, j: integer;
  Y, H, X: integer;
  R: TRect;
  Tb: TTrackBar;
  BrdMarg, BullMarg, BullWidth: integer;
  BullRect: TRect;
  L, W: integer;
  Clr, LastColor: TColor;
  GDIP: TGPGraphics;
  GPen: TGPPen;
  GBrush: TGPSolidBrush;
  BulletDrawn: Boolean;
  P: TPoint;
  MaxMinVal: single;
  Elems: TLegendContents;
  ElemFlow: TContentFlow;
  Leading: integer;
  TM: TTextMetric;
  Points: TPointArray;
  PtLineLength: integer;
  ConnPoint: TPoint;
  RInfo: TRectInfo;
  Computed: Boolean;
  PRect: TRect;
  Full: Boolean;
  Indx: integer;
  sl: TStringList;
  SContent: string;
  S: string;
  UseItmClrMode, UseItmClr: Boolean;

  function GetBullRect(L, T: integer): TRect;
  var
    c: integer;
  begin
    Result := Rect(0, 0, 0, 0);
    c := T + (FLineHeight div 2); { Center of text }
    T := c - BullWidth div 2;
    case Bullets of
      lbSquare, lbCircle:
        Result := Rect(L, T, L + BullWidth, T + BullWidth);
      lbLine:
        Result := Rect(L, c - 1, L + BullWidth, c + 3);
    end;
  end;

  function GetColor(SerIndx, ItemIndx: integer): TColor;
  begin
    if Chart.FSeriesDefs.Items[SerIndx].Graph is TCWBar then
      with Chart.FSeriesDefs.Items[SerIndx].Graph as TCWBar do
      begin
        if ColorUsage = cuOnSeries then
          Result := Chart.FSeriesDefs.Items[SerIndx].Color
        else
        begin
          Result := Chart.ItemColors.Items[ItemIndx].Color;
        end;
      end
    else if Chart.FSeriesDefs.Items[SerIndx].Graph is TCWPie then
      with Chart.FSeriesDefs.Items[SerIndx].Graph as TCWPie do
      begin
        Result := Chart.ItemColors.Items[ItemIndx].Color;
      end
    else
      Result := Chart.FSeriesDefs.Items[SerIndx].Color;
  end;

  procedure CheckOverlaps;
  var
    R1, R2, Gr: TRect;
    i, j: integer;
    Overflow: integer;
    RI: TRectInfo;
    SumRects: TLegendRects;
    Del: Boolean;
    Rects: TLegendRects;

    procedure swap(RI1, RI2: TRectInfo; Ind1, Ind2: integer);
    var
      tmp: TRect;
      Rc1, Rc2: TRect;
      Ind: integer;
    begin
      Rc1 := RI1.R;
      Rc2 := RI2.R;
      tmp := Rc1;
      Rc1 := Rc2;
      Rc2 := tmp;
      RI1.R := Rc1;
      RI2.R := Rc2;
      Ind := RI1.Index;
      RI1.Index := RI2.Index;
      RI2.Index := Ind;
      Rects[Ind1] := RI1;
      Rects[Ind2] := RI2;
    end;

  begin
    Gr := Writer.GraphRect;
    Rects := Chart.FSeriesRects;
    repeat
      Overflow := 0;
      if Rects[0].R.Top <= Gr.Top then
      begin
        RI := Rects[0];
        RI.R.Location := Point(Rects[0].R.Left, Gr.Top + 1);
        Rects[0] := RI;
      end;

      if Rects.Count > 1 then
      begin
        for i := 0 to Rects.Count - 1 do
        begin
          for j := 0 to Rects.Count - 1 do
          begin
            if i = j then
              Continue;
            R1 := Rects[i].R;
            R2 := Rects[j].R;
            if R1.IntersectsWith(R2) then
            begin
              R2.Location := Point(R2.Left, R1.Bottom + 1);
              RI := Rects[j];
              RI.R := R2;
              Rects[j] := RI;
              if Rects[j].P.Y < Rects[i].P.Y then
              begin
                swap(Rects[i], Rects[j], i, j);
              end;
            end;
          end;
        end;
      end
      else
        R2 := Rects[0].R;

      if (R2.Bottom > Gr.Bottom) then
      begin
        Overflow := R2.Bottom - Gr.Bottom;
        if Rects[0].R.Top - Overflow > Gr.Top then { Enough space, lift up }
        begin
          for i := 0 to Rects.Count - 1 do
          begin
            RI := Rects[i];
            RI.R.Location := Point(Rects[i].R.Left, Rects[i].R.Top - Overflow);
            Rects[i] := RI;
          end;
          Overflow := 0;
        end
        else
        begin
          Rects.Delete(Rects.Count - 1);
          if Rects.Count > 0 then
          begin
            RI := Rects[Rects.Count - 1];
            RI.R.Location := Point(RI.R.Left, RI.R.Top - Overflow);
            if RI.R.Top < Gr.Top then
            begin
              Rects.Delete(Rects.Count - 1);
              if Rects.Count > 0 then
              begin
                RI := Rects[Rects.Count - 1];
                RI.R.Location := Point(RI.R.Left, RI.R.Top - Overflow);
              end;
            end;
            if Rects.Count > 0 then
              Rects[Rects.Count - 1] := RI;
          end;

        end;
      end;
      if Rects.Count = 0 then
        Break;
    until Overflow = 0;

    SumRects := Chart.FSummaryRects;
    repeat
      Del := False;
      for i := 0 to Rects.Count - 1 do
      begin
        for j := 0 to SumRects.Count - 1 do
        begin
          if Rects[i].R.IntersectsWith(SumRects[j].R) then
          begin
            Rects.Delete(i);
            Del := True;
            Break;
          end;
        end;
        if Del then
          Break;
      end;
    until not Del;

  end;

  procedure SetWH(Indx: integer);
  var
    i: integer;
    W: integer;
  begin
    { Width is set by CreateContent. Height is the sum of the series. Must be
      broken up in individual heights }
    FHeight := 0;
    FTextWidth := 0;
    for i := 0 to FContentList[Indx].Count - 1 do
    begin
      W := Canvas.TextWidth(FContentList[Indx][i]);
      if W > FTextWidth then { Used to measure the width of the rects }
      begin
        FTextWidth := W;
      end;
      inc(FHeight, FLineHeight);
    end;
    FHeight := FHeight + BrdMarg * 2;
  end;

  function GetPoints(Indx: integer): TPointArray;
  begin
    if Writer.GraphTypeInChart(TCWBar) then
      Result := Writer.FSeriesData[Indx].FBarPoints
    else
      Result := Writer.FSeriesData[Indx].FPoints;
  end;

  function GetPointRect(APoint: TPoint): TRect;
  var
    OK: Boolean;
    procedure SetWH;
    begin
      Result.Right := Result.Left + FWidth;
      Result.Bottom := Result.Top + FHeight;
    end;

    procedure SetConnPoint(Indx: integer);
    begin
      if Indx in [1, 2, 8] then
        { Tops, connect to center bottoms }
        ConnPoint := Point(Result.CenterPoint.X, Result.Bottom)
      else if Indx = 3 then
        { Middle right }
        ConnPoint := Point(Result.Left, Result.CenterPoint.Y)
      else if Indx = 7 then
        { Middle left }
        ConnPoint := Point(Result.Right, Result.CenterPoint.Y)
      else { Bottoms }
        ConnPoint := Point(Result.CenterPoint.X, Result.Top)
    end;

    function Overlaps(ARect: TRect): Boolean;
    var
      i: integer;
      R: TRect;
      P: TPoint;
    begin
      Result := False;
      for i := 0 to Chart.FPointRects.Count - 1 do
      begin
        R := Chart.FPointRects[i].R;
        if ARect.IntersectsWith(R) then
        begin
          Result := True;
          Break;
        end;
        P := Chart.FPointRects[i].P;
        PRect := Rect(P.X - leg_PointRad, P.Y - leg_PointRad,
          P.X + leg_PointRad, P.Y + leg_PointRad);
        if ARect.IntersectsWith(PRect) then
        begin
          Result := True;
          Break;
        end;
      end;
      for i := 0 to Chart.FSummaryRects.Count - 1 do
      begin
        R := Chart.FSummaryRects[i].R;
        if ARect.IntersectsWith(R) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;

    function GetCath(AHypo: integer): integer;
    begin
      Result := round(sqrt((PtLineLength * PtLineLength) / 2));
    end;

  begin
    { Try top }
    OK := False;
    Result.Left := APoint.X - FWidth div 2;
    Result.Top := APoint.Y - PtLineLength - FHeight;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(1);
      Exit;
    end;

    { Try topright }
    Result.Left := APoint.X + GetCath(PtLineLength);
    Result.Top := APoint.Y - PtLineLength - FHeight;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(2);
      Exit;
    end;

    { Try right }
    Result.Left := APoint.X + PtLineLength;
    Result.Top := APoint.Y - FHeight div 2;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(3);
      Exit;
    end;

    { Try rightbottom }
    Result.Left := APoint.X + GetCath(PtLineLength);
    Result.Top := APoint.Y + PtLineLength;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(4);
      Exit;
    end;

    { Try bottom }
    Result.Left := APoint.X - FWidth div 2;
    Result.Top := APoint.Y + PtLineLength;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(5);
      Exit;
    end;

    { Try bottomleft }
    Result.Left := APoint.X - GetCath(PtLineLength) - FWidth;
    Result.Top := APoint.Y + PtLineLength;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(6);
      Exit;
    end;

    { Try left }
    Result.Left := APoint.X - PtLineLength - FWidth;
    Result.Top := APoint.Y - FHeight div 2;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(7);
      Exit;
    end;

    { Try lefttop }
    Result.Left := APoint.X - GetCath(PtLineLength) - FWidth;
    Result.Top := APoint.Y - PtLineLength - FHeight;
    SetWH;
    if Writer.GraphRect.Contains(Result) and not Overlaps(Result) then
    begin
      SetConnPoint(8);
      Exit;
    end;

    if not OK then
      Result := Rect(0, 0, 0, 0);
  end;

  function GetSummaryRect(AProposal: TRect): TRect;
  var
    R: TRect;
    i: integer;
    Rects: TLegendRects;
    SpaceHoriz, SpaceVert: integer;
    Located, NoSpace: Boolean;
    P1, P2: integer;

    procedure GetSpace;
    var
      i: integer;
    begin
      SpaceHoriz := 0;
      SpaceVert := 0;
      for i := 0 to Rects.Count - 1 do
      begin
        if Rects[i].Leg.Anchoring = Anchoring then
        begin
          SpaceHoriz := SpaceHoriz +
            (Writer.GraphRect.Width - Rects[i].R.Width);
          SpaceVert := SpaceVert +
            (Writer.GraphRect.Height - Rects[i].R.Height);
        end;
      end;
    end;

  begin
    Result := Rect(0, 0, 0, 0);
    if InsideGraph then
    begin
      if not Writer.GraphRect.Contains(AProposal) then
      begin
        Exit;
      end;
    end;
    R.Left := Left;
    R.Top := Top;
    R.Width := Width;
    R.Height := Height;
    NoSpace := False;
    Rects := Chart.FSummaryRects;
    repeat
      Located := True;
      for i := 0 to Rects.Count - 1 do
      begin
        if AProposal.IntersectsWith(Rects[i].R) then
        begin
          GetSpace;
          if AxisPosition in [apRight, apLeft] then
          begin
            if SpaceVert > AProposal.Height then
            begin
              if Rects[i].Leg.Alignment = laCenter then
              begin
                P1 := Writer.GraphRect.Bottom - AProposal.Height - 1;
                P2 := Writer.GraphRect.Top;
              end
              else
              begin
                P1 := Rects[i].R.Bottom + 1;
                P2 := Rects[i].R.Top - AProposal.Height - 1;
              end;

              { Space below? }
              AProposal.Location := Point(AProposal.Left, P1);
              if AProposal.Bottom > Writer.GraphRect.Bottom then
              begin
                { Space abow? }
                AProposal.Location := Point(AProposal.Left, P2);
                if AProposal.Top < Writer.GraphRect.Top then
                begin
                  Located := False;
                  NoSpace := True;
                  Break;
                end;
              end;
            end
            else
            begin
              Located := False;
              NoSpace := True;
              Break;
            end
          end
          else
          begin
            if SpaceHoriz > AProposal.Width then
            begin
              if Rects[i].Leg.Alignment = laCenter then
              begin
                P1 := Writer.GraphRect.Right - AProposal.Width - 1;
                P2 := Writer.GraphRect.Left;
              end
              else
              begin
                P1 := Rects[i].R.Right + 1;
                P2 := Rects[i].R.Left - AProposal.Width - 1;
              end;

              { Space right? }
              AProposal.Location := Point(P1, AProposal.Top);
              if AProposal.Left <= Rects[i].R.Right then
              begin
                { Space left? }
                AProposal.Location := Point(P2, Result.Top);
                if AProposal.Right >= Rects[i].R.Left then
                begin
                  Located := False;
                  NoSpace := True;
                  Break;
                end;
              end;
            end
            else
            begin
              Located := False;
              NoSpace := True;
              Break;
            end
          end;
        end;
      end;
    until Located or NoSpace;
    if Located then
      Result := AProposal
    else
      Exit;

    Rects := Chart.FSeriesRects;
    for i := 0 to Rects.Count - 1 do
    begin
      if Result.IntersectsWith(Rects[i].R) then
      begin
        Result := Rect(0, 0, 0, 0);
        Break;
      end;
    end;

    if Result.IsEmpty then
      Exit;

    if InsideGraph then
    begin
      if not Writer.GraphRect.Contains(AProposal) then
      begin
        Result := Rect(0, 0, 0, 0);
        Exit;
      end;

      Rects := Chart.FPointRects;
      for i := 0 to Rects.Count - 1 do
      begin
        if Result.IntersectsWith(Rects[i].R) then
        begin
          Result := Rect(0, 0, 0, 0);
          Break;
        end;
      end;
    end;
  end;

  function GetSeriesRect(Index: integer; var Pt: TPoint): TRect;
  var
    i: integer;
    Rects: TLegendRects;
  begin
    Result := Rect(0, 0, 0, 0);
    Rects := Chart.FSeriesRects;
    for i := 0 to Rects.Count - 1 do
      if Index = Rects[i].Index then
      begin
        Result := Rects[i].R;
        Pt := Rects[i].P;
        Break;
      end;

  end;

  procedure DrawBorder;
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(R);
  end;

  procedure AddRect(RInfo: TRectInfo);
  var
    i: integer;
    Added: Boolean;
    Rects: TLegendRects;
  begin
    RInfo.Leg := Self;

    if Anchoring = anPointInside then
    begin
      Rects := Chart.FPointRects;
    end
    else if Anchoring = anSeriesOutside then
      Rects := Chart.FSeriesRects
    else
      Rects := Chart.FSummaryRects;

    Added := False;
    for i := 0 to Rects.Count - 1 do
    begin
      if RInfo.R.Top < Rects[i].R.Top then
      begin
        if i = Rects.Count - 1 then
          Rects.Add(RInfo)
        else
          Rects.Insert(i, RInfo);
        Added := True;
        Break;
      end;
    end;
    if not Added then
      Rects.Add(RInfo);
  end;

  function GetFirstMaxMinVal(SerIndex: integer; Val: single): integer;
  var
    S, S2: string;
    i: integer;
  begin
    Result := -1;
    S := FormatNum(Val, Writer.ValuePrecision);
    for i := 0 to Writer.FSeriesData[SerIndex].Count - 1 do
    begin
      S2 := FormatNum(Writer.FSeriesData[SerIndex].FSeriesItems[i].Value,
        Writer.ValuePrecision);
      if S2 = S then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

begin
  if not Visible then
    Exit;
  if (Anchoring = anSeriesOutside) then
    if not Writer.FNamAx.IsXAxis then
      Exit; { Only horiz layout curves supported for this anchoring }

  PtLineLength := 60;
  if Writer = nil then
    Exit;
  if Border then
    BrdMarg := leg_BorderMarg
  else
    BrdMarg := 0;

  ElemFlow := ContentFlow;
  if Anchoring = anSeriesOutside then
  begin
    ElemFlow := cfTopBottom;
  end;

  GetTextMetrics(Canvas.Handle, TM);
  Leading := TM.tmInternalLeading;

  if Bullets <> lbNone then
  begin
    BullMarg := 5;
    BullWidth := FLineHeight - Leading - Round(FLineHeight * 20 / 100);
  end
  else
  begin
    BullMarg := 0;
    BullWidth := 0;
  end;
  X := 0;
  W :=0;
  if Anchoring in [anLeftOutside .. anBottomInside] then
  begin
    R := Rect(Left + HorizMargins, Top + FVertMargins,
      Left + HorizMargins + FTextWidth + BrdMarg * 2 + BullMarg + BullWidth,
      Top + FVertMargins + FTextHeight + BrdMarg * 2);
    R := GetSummaryRect(R);
    X := R.Left;
    if R.IsEmpty then
      Exit;
    RInfo.R := R;
    RInfo.P := Point(0, 0); { Not used, actually }
    AddRect(RInfo);
  end;

  GPen := nil;
  GBrush := nil;
  GDIP := nil;
  if (Bullets <> lbNone) or (Anchoring in [anSeriesOutside, anPointInside]) or
    (Transparency < 255) then
  begin
    GDIP := TGPGraphics.Create(Canvas.Handle);
    GDIP.SetSmoothingMode(SmoothingModeAntiAlias);
    GDIP.SetInterpolationMode(InterpolationMode.InterpolationModeHighQuality);
    GDIP.SetCompositingQuality
      (CompositingQuality.CompositingQualityHighQuality);
    GPen := TGPPen.Create(MakeGPClr(clBlack), 2);
    GBrush := TGPSolidBrush.Create(MakeGPClr(clBlue));
  end;

  try
    if Anchoring in [anSeriesOutside, anPointInside] then
    begin
      Points := GetPoints(0);
      if Points.Count = 0 then
      begin
        Abort;
      end;
    end;
    Canvas.Brush.Assign(Brush);
    Canvas.Font.Assign(Font);
    if Anchoring in [anLeftOutside .. anBottomInside] then
    begin
      if Transparency < 255 then
      begin
        GBrush.SetColor(MakeGPClr(Brush.Color, Transparency));
        GDIP.FillRectangle(GBrush, MakeRect(R));
      end
      else
        Canvas.FillRect(R);
    end;
    Y := R.Top + BrdMarg;
    L := Left;
    LastColor := RGB(0, 0, 1);
    Computed := True;
    Elems := Contents;
    repeat
      if Anchoring = anSeriesOutside then
      begin
        Computed := not Computed;
        if Computed then
          CheckOverlaps;
      end;
      Full := False;
      for i := 0 to FContentList.Count - 1 do
      begin
        if FContentList[i].Count = 0 then
          Continue;
        Canvas.Brush.Assign(Brush);
        if Anchoring = anSeriesOutside then
        begin
          if not Computed then
          begin
            Points := GetPoints(i);
            P := Points[Points.Count - 1];

            if (Chart.FSeriesDefs.Items[i].Graph is TCWBar) and
              (TCWBar(Chart.FSeriesDefs.Items[i].Graph).Layout = blSideBySide) then
            begin
              if not Writer.GetTrackBar(i, Points.Count - 1, Tb) then
                Continue;
              if Writer.FNamAx.IsXAxis then
                P.X := Tb.VisibleRect.CenterPoint.X
              else
                P.Y := Tb.VisibleRect.CenterPoint.Y;
            end;
            Y := P.Y;
            SetWH(i);
            R := Rect(L + HorizMargins, Y - FHeight div 2,
              L + FTextWidth + BullMarg + BullWidth + HorizMargins + BrdMarg *
              2, Y + FHeight div 2);
            RInfo.R := R;
            RInfo.P := P;
            RInfo.Index := i;
            AddRect(RInfo);
            Continue;
          end
          else
          begin
            R := GetSeriesRect(i, P);
          end;
          Y := R.Top + BrdMarg;
          X := R.Left;
          ConnPoint.X := R.Left;
          ConnPoint.Y := R.CenterPoint.Y;
          Canvas.Brush.Assign(Brush);
          if Transparency < 255 then
          begin
            GBrush.SetColor(MakeGPClr(Brush.Color, Transparency));
            GDIP.FillRectangle(GBrush, MakeRect(R));
          end
          else
            Canvas.FillRect(R);
        end
        else if Anchoring = anPointInside then
        begin
          if PointLocating in [plMaxValue, plMinValue] then
          begin
            if FPointSeriesIndex = -1 then
              Indx := i
            else
              Indx := FPointSeriesIndex;
            if Indx <> i then
              Continue;
            if PointLocating = plMaxValue then
              MaxMinVal := Writer.FSeriesData[Indx].MaxVal
            else
              MaxMinVal := Writer.FSeriesData[Indx].MinVal;
            FPointItemIndex := GetFirstMaxMinVal(Indx, MaxMinVal);
          end;

          Points := GetPoints(i);
          P := Points[PointItemIndex];
          if (Chart.FSeriesDefs.Items[i].Graph is TCWBar) and
            (TCWBar(Chart.FSeriesDefs.Items[i].Graph).Layout = blSideBySide) then
          begin
            if not Writer.GetTrackBar(i, PointItemIndex, Tb) then
              Continue;
            if Writer.FNamAx.IsXAxis then
              P.X := Tb.VisibleRect.CenterPoint.X
            else
              P.Y := Tb.VisibleRect.CenterPoint.Y;
          end;
          BullWidth := 0;
          BullMarg := 0;
          SetWH(i);
          R := GetPointRect(P);
          if not R.IsEmpty then
          begin
            RInfo.R := R;
            RInfo.P := P;
            RInfo.Index := i;
            AddRect(RInfo);
            Y := R.Top + BrdMarg;
            X := R.Left;
            if Transparency < 255 then
            begin
              GBrush.SetColor(MakeGPClr(Brush.Color, Transparency));
              GDIP.FillRectangle(GBrush, MakeRect(R));
            end
            else
              Canvas.FillRect(R);
          end
          else
            Continue;
        end;

        UseItmClrMode := Writer.FSeriesData[i].UseItemColors;
        for j := 0 to FContentList[i].Count - 1 do
        begin
          BulletDrawn := False;
          UseItmClr := False;
          SContent := FContentList[i][j];
          if Pos('[', SContent) = 1 then
          begin
            Indx := Pos(']', SContent, 2);
            S := Copy(SContent, 1, Indx);
            Delete(SContent, 1, Indx);
            Delete(S, 1, 1);
            Delete(S, Length(S), 1);
            Indx := StrToInt(S);
            UseItmClr := True;
          end
          else
          begin
            Indx := j;
          end;
          Clr := GetColor(i, Indx);

          if (UseItmClr and (BullMarg <> 0) and UseItmClrMode)

            or ((Clr <> LastColor) and (BullMarg <> 0)) and
            not(UseItmClrMode and not UseItmClr)

          then
          begin
            GPen.SetWidth(1);
            BullRect := GetBullRect(X + BrdMarg, Y);
            Canvas.Brush.Color := GetColor(i, Indx);
            if Bullets in [lbSquare, lbLine] then
            begin
              if Bullets = lbLine then
                Canvas.Pen.Color := Canvas.Brush.Color;
              Canvas.Rectangle(BullRect);
              Canvas.Pen.Color := clBlack;
            end
            else
            begin
              GBrush.SetColor(MakeGPClr(GetColor(i, Indx)));
              GDIP.FillEllipse(GBrush, MakeRect(BullRect));
              GDIP.DrawEllipse(GPen, MakeRect(BullRect));
            end;
            LastColor := Canvas.Brush.Color;
            BulletDrawn := True;
          end
          else if (UseItmClr and (BullMarg = 0) and UseItmClrMode and
            ColoredText) or ((BullMarg = 0) and ColoredText and
            (((Clr <> LastColor) and UseItmClr) or ((Clr <> LastColor) and
            not UseItmClrMode))) then
          begin
            Canvas.Font.Color := GetColor(i, Indx);
            LastColor := Canvas.Font.Color;
          end
          else
            Canvas.Font.Color := Font.Color;
          Canvas.Brush.Style := bsClear;
          if ElemFlow = cfLeftRight then
          begin
            if BulletDrawn then
            begin
              X := X + BullMarg + BullWidth + BrdMarg;
            end;
            W := Canvas.TextWidth(SContent);
            Canvas.TextOut(X, Y, SContent);
          end
          else
          begin
            if Anchoring = anPointInside then
              L := R.Left + BrdMarg
            else
              L := X + BrdMarg + BullMarg + BullWidth;

            Canvas.TextOut(L, Y, SContent);
            if (Anchoring in [anSeriesOutside, anPointInside]) and (j = 0) then
            begin
              if (poThinConnectionLine in PointOptions) then
                GPen.SetWidth(1)
              else
                GPen.SetWidth(2);
              if (poShowConnectionLine in PointOptions) then
                GDIP.DrawLine(GPen, ConnPoint.X, ConnPoint.Y, P.X, P.Y);
              if (Anchoring = anPointInside) and (poEnlargePoint in PointOptions)
              then
              begin
                PRect := Rect(P.X - leg_PointRad, P.Y - leg_PointRad,
                  P.X + leg_PointRad, P.Y + leg_PointRad);
                GBrush.SetColor(MakeGPClr(GetColor(i, j)));
                GDIP.FillEllipse(GBrush, MakeRect(PRect));
                GPen.SetColor(MakeGPClr(clBlack));
                GPen.SetWidth(1);
                GDIP.DrawEllipse(GPen, MakeRect(PRect));

                GBrush.SetColor(MakeGPClr(clWhite));
                PRect := Rect(P.X - 2, P.Y - 2, P.X + 2, P.Y + 2);
                GDIP.FillEllipse(GBrush, MakeRect(PRect));
                GPen.SetColor(MakeGPClr(clBlack));
              end;
            end;
          end;
          if ElemFlow = cfLeftRight then
            inc(X, W + 5)
          else
          begin
            inc(Y, FLineHeight);
          end;
          if AxisPosition in [apLeft, apRight] then
          begin
            if Y > Writer.WorkRect.Height then
              Break;
          end
          else
          begin
            if L > Writer.WorkRect.Width then
              Break;
          end;
        end; { J }
        if Border and not Summary then
          DrawBorder;
      end; { I }
    until Computed;

    Canvas.Font.Color := Font.Color;
    Full := SContent = '...';
    if (Text.Count > 0) and Summary and not Full then
    begin
      sl := TStringList.Create;
      try
        sl.Assign(Text);
        if (ElemFlow = cfLeftRight) and (Elems <> []) then
          inc(Y, FLineHeight);
        for i := 0 to sl.Count - 1 do
        begin
          if Y + FLineHeight > Writer.GraphRect.Bottom then
          begin
            Canvas.TextOut(R.Left + BrdMarg, Y, '...');
            Break;
          end
          else
            Canvas.TextOut(R.Left + BrdMarg, Y, sl[i]);
          inc(Y, FLineHeight);

        end;
      finally
        sl.Free;
      end;
    end;

    if Summary and Border then
      DrawBorder;
  finally
    if (Bullets <> lbNone) or (Transparency < 255) or
      (Anchoring in [anSeriesOutside, anPointInside]) then
    begin
      GDIP.Free;
      GPen.Free;
      GBrush.Free;
    end;
  end;
end;

{ TSeries -------------------------------------------------------- }

constructor TSeries.Create;
begin
  FSeriesItems := TSeriesItems.Create;
  FPoints := TPointArray.Create;
  FBarPoints := TPointArray.Create;
  FTrackPies := TTrackPies.Create;
  FPieState := 0;
end;

Destructor TSeries.Destroy;
begin
  FSeriesItems.Free;
  FPoints.Free;
  FBarPoints.Free;
  FTrackPies.Free;
  inherited;
end;

procedure TSeries.Assign(ASource: TSeries; FromIndex, ToIndex: integer);
var
  i: integer;
  Itm: TSeriesItem;
  AFrom, ATo: integer;

begin
  if FromIndex = -1 then
  begin
    AFrom := 0;
    ATo := ASource.Count - 1;
  end
  else
  begin
    AFrom := FromIndex;
    ATo := ToIndex;
  end;

  if FromIndex = -1 then
  begin
    FIndent := ASource.FIndent;
    FExdent := ASource.FExdent;
  end
  else
  begin
    FIndent := 0;
    FExdent := 0;
  end;

  for i := AFrom to ATo do
  begin
    if (i > ASource.LastItem) and (FromIndex <> -1) then
    begin
      inc(FExdent);
    end;
    if (i < ASource.FirstItem) and (FromIndex <> -1) then
    begin
      inc(FIndent);
    end;
    Itm := AddItem;
    Itm.Assign(ASource.FSeriesItems[i]);
    Itm.FOwner := Self;
  end;

  FMax := ASource.FMax;
  FMin := ASource.FMin;
  FLeapdateCount := ASource.FLeapdateCount;
  FWriter := ASource.FWriter;
end;

function TSeries.Avg: single;
begin
  Result := FSum / ItemCount;
end;

function TSeries.MaxVal: single;
begin
  Result := FMax;
end;

function TSeries.MinVal: single;
begin
  Result := FMin;
end;

function TSeries.Sum: single;
begin
  Result := FSum;
end;

function TSeries.Mode: TModeNumbers;
var
  Ser: TSeries;
  i: integer;
  sl: TStringList;
  S, S2: string;
  Cnt, MaxCnt: integer;
  NumCount: TModeNumber;
begin
  Ser := TSeries.Create;
  try
    Ser.Assign(Self, -1, -1);
    Ser.FSeriesItems.Sort(TComparer<TSeriesItem>.Construct(
      function(const a, B: TSeriesItem): integer
      begin
        if a.FValue = B.FValue then
          Result := 0
        else if a.FValue > B.FValue then
          Result := 1
        else
          Result := -1;
      end));

    sl := TStringList.Create;
    try
      for i := 0 to Ser.ItemCount - 1 do
      begin
        S := FormatNum(Ser.Items[i].FValue, Writer.ValuePrecision);
        if i > 0 then
        begin
          S2 := FormatNum(Ser.Items[i - 1].FValue, Writer.ValuePrecision);
          if S = S2 then
          begin
            sl.Add(S);
          end;
        end;
      end;

      if sl.Count = 0 then
        Exit;
      S := sl[0];
      Cnt := 1;
      MaxCnt := 0;
      for i := 1 to sl.Count - 1 do
      begin
        S2 := sl[i];
        if S2 = S then
        begin
          inc(Cnt);
        end
        else
        begin
          if Cnt < MaxCnt then
          begin
            S := sl[i];
            Continue;
          end
          else if Cnt > MaxCnt then
          begin
            Finalize(Result);
            MaxCnt := Cnt;
            Cnt := 1;
          end;
          setlength(Result, Length(Result) + 1);
          NumCount.Number := S;
          NumCount.Cnt := MaxCnt;
          Result[High(Result)] := NumCount;
          S := sl[i];
        end;
      end;
    finally
      sl.Free;
    end;

  finally
    Ser.Free;
  end;
end;

function TSeries.Median: single;
var
  Ser: TSeries;
  i: integer;
  V1, V2: single;
begin
  Ser := TSeries.Create;
  try
    Ser.Assign(Self, -1, -1);
    Ser.FSeriesItems.Sort(TComparer<TSeriesItem>.Construct(
      function(const a, B: TSeriesItem): integer
      begin
        if a.FValue = B.FValue then
          Result := 0
        else if a.FValue > B.FValue then
          Result := 1
        else
          Result := -1;
      end));

    if Odd(Ser.Count) then
      Result := Ser.Items[Ser.Count div 2].FValue
    else
    begin
      i := Ser.Count div 2;
      V1 := Items[i].FValue;
      V2 := Items[i + 1].FValue;
      Result := (V1 + V2) / 2;
    end;
  finally
    Ser.Free;
  end;
end;

function TSeries.PosFromNamVal(AName: string; AVal: single): TPoint;
var
  X, Y: integer;
  i: integer;
  StartFrom: integer;
  Un: single;
  Dt: TDateTime;
begin
  Result := Point(0, 0);
  if Writer.FNameList.Count = 0 then
    Exit;

  if Writer.Chart.NameType = ntGeneral then
  begin
      Un := Writer.NameFloatUnit;
      X := -1;
      if Writer.FNamAx.Position = apLeft then
      begin
        StartFrom := Writer.GetPosRect.Top;
      end
      else if Writer.FNamAx.Position = apTop then
      begin
        StartFrom := Writer.GetPosRect.Left;
      end
      else if Writer.FNamAx.Position = apBottom then
      begin
        StartFrom := Writer.GetPosRect.Left;
      end
      else // Right
      begin
        StartFrom := Writer.GetPosRect.Top;
      end;

      for i := 0 to Writer.FNameList.Count - 1 do
      begin
        if SameText(Writer.Names[i], AName) then
        begin
          X := floor(StartFrom + (i * Un));
          Break;
        end;
      end;
      if X = -1 then
      begin
        Writer.GoBackError;
        ShowGWError(msg_NotPresent, '(' + AName + ')');
      end
  end
  else if Writer.IsTimeSpan(Writer.Chart.NameType) then
  begin
    Dt := StrToDateTime(AName, Fmt);
    if Writer.Chart.NameType = ntDateSpan then
      X := Writer.PosFromDate(Dt)
    else if Writer.Chart.NameType = ntHourSpan then
      X := Writer.PosFromHour(Dt)
    else if Writer.Chart.NameType = ntMinuteSpan then
      X := Writer.PosFromMinute(Dt)
    else if Writer.Chart.NameType = ntSecondSpan then
      X := Writer.PosFromSecond(Dt)
    else
      X := -1;
    if X = -1 then
    begin
      Writer.GoBackError;
      ShowGWError(msg_NotPresent, '(' + AName + ')');
    end;
  end
  else
  begin
    X := Writer.PosFromNumber(StrToFloat(AName, Fmt));
    if X = -1 then
    begin
      Writer.GoBackError;
      ShowGWError(msg_NotPresent, '(' + AName + ')');
    end;
  end;

  Y := Writer.PosFromValue(AVal, Writer.ValAxFromGraph(TCWAxisGraph(Graph)));
  if Writer.FValAx.IsXAxis then
    Result := System.Classes.Point(Y, X)
  else
    Result := System.Classes.Point(X, Y);
end;

function TSeries.GetTitle : string;
begin
  Result := '';
  if Index = -1 then
    Exit;
  if (Writer = nil) or (Writer.Chart = nil) or (Writer.Chart.SeriesDefs.Count-1 < Index) then
   Exit;
  Result := Writer.Chart.FSeriesDefs.Items[Index].Title;
end;

function TSeries.GetVisible: Boolean;
begin
  if Index = -1 then
    Result := false
  else
   Result := Writer.Chart.FSeriesDefs.Items[Index].Visible;
end;

function TSeries.AddItem: TSeriesItem;
begin
  Result := TSeriesItem.Create;
  Result.FOwner := Self;
  Result.FItems := FSeriesItems;
  FSeriesItems.Add(Result);
end;

function TSeries.AsStrings(IncludeItemProps: Boolean = False): TStringList;
var
  i: integer;
  Itm: TSeriesItem;
  S: string;
  Dt: TDateTime;
  UxTime: int64;
  STime: string;
  V: single;
begin
  Result := TStringList.Create;
  Result.DefaultEncoding := TEncoding.Utf8;
  for i := 0 to ItemCount - 1 do
  begin
    Itm := Items[i];
    if Itm.FLeapDummy then
      Continue;
    V := Itm.FValue * 1000;
    S := FormatNum(V, Writer.ValuePrecision);
    if Writer.IsTimeSpan(Writer.Chart.NameType) then
    begin
      Dt := StrToDateTime(Itm.FOrigName, Fmt);
      UxTime := DateTimeToUnix(Dt);
      STime := IntToStr(UxTime);
      S := STime + '=' + S;
    end
    else
      S := Itm.FOrigName + '=' + S;
    Result.Add(S);
  end;
end;

function TSeries.GetItemPoints: TPointArray;
begin
  if Graph is TCWBar then
    Result := FBarPoints
  else if Graph is TCWCurve then
  begin
    if Writer.GraphTypeInChart(TCWBar) then
      Result := FBarPoints
    else
      Result := FPoints
  end
  else
    Result := nil;
end;

function TSeries.GetGraph: TCWGraph;
begin
  Result := nil;
  if Writer.Chart <> nil then
   Result := Writer.Chart.FSeriesDefs.Items[Index].Graph;
  if Result = nil then
    ShowGWError(msg_ResolveGraphType);
end;

function TSeries.GetUseItemColors: Boolean;
begin
  Result := false;
  if Graph is TCWPie then
    Result := True
  else
  begin
    if Graph is TCWCurve then
      Result := False
    else if (Graph is TCWBar) then
      Result := (TCWBar(Graph).ColorUsage = cuOnItems)
    else if (Graph is TCWPie) then
      Result := True;
  end;
end;

function TSeries.GetPieTitleRect: TRect;
begin
  Result := FPieRect;
  Result.Bottom := FPieRect.Top;
  Result.Top := Result.Bottom - TCWPie(Graph).FTitleSpace;
end;

function TSeries.GetStartDate: TDateTime;
begin
  if (ItemCount > 1) and (Items[0].RealDate <> 0) then
    Result := Items[0].RealDate
  else
    Result := FStartDate;
end;

function TSeries.GetEndDate: TDateTime;
begin
  if (ItemCount > 1) and (Items[ItemCount - 1].RealDate <> 0)  then
    Result := Items[ItemCount - 1].RealDate
  else
    Result := FEndDate;
end;

function TSeries.IndexOfName(AName: string): integer;
var
  i: integer;
  S: string;
begin
  Result := -1;
  for i := 0 to FSeriesItems.Count - 1 do
  begin
    S := FSeriesItems[i].FName;
    if SameText(S, AName) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TSeries.IndexOfValue(AValue: single): integer;
{ Returns first found }
var
  i: integer;
  S, S2: string;
begin
  Result := -1;
  S := FormatNum(AValue, Writer.ValuePrecision);
  for i := 0 to FSeriesItems.Count - 1 do
  begin
    S2 := FormatNum(FSeriesItems[i].FValue, Writer.ValuePrecision);
    if S = S2 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TSeries.IndexOfDate(Y, M, D: word): integer;
var
  i: integer;
  Dt: TDate;
begin
  Result := -1;
  if not Writer.IsTimeSpan(Writer.Chart.NameType) then
    Exit;
  for i := 0 to ItemCount - 1 do
  begin
    Dt := Items[i].FRealDate;
    if CheckDateHit(Dt, Y, M, D) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TSeries.IndexOfTime(H, M, S: integer): integer;
var
  i: integer;
  Dt: TDateTime;
begin
  Result := -1;
  if not Writer.IsTimeSpan(Writer.Chart.NameType) then
    Exit;
  for i := 0 to ItemCount - 1 do
  begin
    Dt := Items[i].FRealDate;
    if CheckTimeHit(Dt, H, M, S) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TSeries.GetFirstItem: integer;
begin
  Result := FIndent;
end;

function TSeries.GetLastItem: integer;
begin
  Result := Count - 1 - FExdent;
end;

function TSeries.GetItems(Index: integer): TSeriesItem;
begin
  Result := FSeriesItems[FirstItem + Index];
end;

function TSeries.GetIndex: integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Writer.Count - 1 do
  begin
    if Writer.FSeriesData[i] = Self then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TSeries.ToDate(Index: integer): TDateTime;
begin
  Result := StrToDateTime(FSeriesItems[Index].FName, Fmt);
end;

procedure TSeries.ToPercentages;
var
  i : integer;
begin
  if FNegVals then
    Exit;
  for I := FirstItem to LastItem do
  begin
    Items[i].FValue := Items[i].Pst;
  end;
end;

procedure TSeries.ToRealValues;
var
  i : integer;
begin
  if FNegVals then
    Exit;
  for I := FirstItem to LastItem do
  begin
    Items[i].FValue := Items[i].FRealVal;
  end;
end;

function TSeries.GetCount: integer;
begin
  Result := FSeriesItems.Count;
end;

function TSeries.GetItemCount: integer;
begin
  Result := LastItem - FirstItem + 1;
end;

{ TCWMargins -------------------------------------------------- }

function TCWMargins.CheckMargin(OldValue, NewValue: integer): Boolean;
begin
  Result := False;
  if OldValue = NewValue then
    Exit;
  if NewValue < 0 then
    Exit;
  Result := True;
end;

procedure TCWMargins.UpdateValue;
begin
  if not(csLoading in Writer.componentState) then
  begin
    Writer.RefreshGraph;
  end;
end;

procedure TCWMargins.SetLeft(Value: integer);
begin
  if not CheckMargin(FLeft, Value) then
    Exit;
  FLeft := Value;
  UpdateValue;
end;

procedure TCWMargins.SetTop(Value: integer);
begin
  if not CheckMargin(FTop, Value) then
    Exit;
  FTop := Value;
  UpdateValue;
end;

procedure TCWMargins.SetRight(Value: integer);
begin
  if not CheckMargin(FRight, Value) then
    Exit;
  FRight := Value;
  Writer.FOrigGraphSize.cx := FRight;
  UpdateValue;
end;

procedure TCWMargins.SetBottom(Value: integer);
begin
  if not CheckMargin(FBottom, Value) then
    Exit;
  FBottom := Value;
  Writer.FOrigGraphSize.cy := FBottom;
  UpdateValue;
end;

{ TChartWriter -------------------------------------------------- }

procedure TChartWriter.WMSAVESELBM(var Msg: Tmessage);
begin
  SaveSelBM;
  if not MouseInfoControl then
    if (FViewMode = vmNormal) then
      FViewMode := vmHinting;
end;

procedure TChartWriter.WMENDEXEC(var Msg: Tmessage);
begin
  ClearState(stInternalContraction);
  ClearState(stExecuting);
end;

procedure TChartWriter.WMSAVEHIST(var Msg: Tmessage);
begin
  if InState(stRestoring) then
  begin
    ClearState(stRestoring);
    Exit;
  end;
  DoSaveToHistory;
end;

procedure TChartWriter.WMERROR(var Msg: Tmessage);
begin
  if Assigned(FOnContraction) then
  begin
    FOnContraction(Self, Msg.WParam);
  end
  else
    ShowGWError(-1, FErrorText);
end;

procedure TChartWriter.WMLOADFILE(var Msg: Tmessage);
begin
 if csDesigning in ComponentState then
   Exit;
 try
  if FNeedsIds then
  begin
    CreateIds;
    Chart.FWID := AddToWList(Self, Chart);
    FNeedsIds := false;
  end;
  if Assigned(Chart) and (Chart.FileName <> '')  then
  begin
    if not FileExists(Chart.FileName) then
      ShowGWError(msg_NoFile, Chart.FileName);
    if Assigned(Chart.FOnGetData) then
     Chart.FOnGetData(Chart);
    if GetFileType(Chart.FileName) <> 1 then
      ShowGWError(msg_RichDesign);
    LoadFromFile(Chart.FileName);
  end
  else if Assigned(Chart) and (Chart.Dataset <> nil)
  and (Chart.NameField <> '') and (Chart.ValueFields <> '') then
  begin
    if not Chart.Dataset.Active then
      ShowGWError(msg_DsNotActive, Chart.Dataset.Name);
    if Assigned(Chart.FOnGetData) then
     Chart.FOnGetData(Chart);
    Clear;
    AddSeries(Chart.Dataset, Chart.NameField, Chart.ValueFields);
    Execute;
  end
  else
  begin
     if Assigned(Chart) and Assigned(FChart.FOnGetData) then
     begin
       Clear;
       FChart.FOnGetData(Chart)
     end
  end;
 except
   GoBackError;
   raise;
 end;

end;

procedure TChartWriter.PostHist;
var
  Msg: TMsg;
begin
  if InState(stRestoring) then
    Exit;
  PeekMessage(Msg, Handle, WM_SAVEHIST, WM_SAVEHIST, PM_REMOVE);
  PostMessage(Handle, WM_SAVEHIST, 0, 0);
end;

procedure TChartWriter.AppMsg(var Msg: tagMsg; var Handled: Boolean);
var
  Pt: TPoint;
  Sft: TShiftState;
begin
  Pt := ScreenToClient(MOuse.CursorPos);
  if not Windows.PtInRect(ClientRect, Pt) then
    Exit;
  Sft := [];
  if Msg.Message = WM_KEYDOWN then
  begin
    if IsCtrlDown then
      Sft := Sft + [ssCtrl];
    if IsShiftDown then
      Sft := Sft + [ssShift];
    if IsAltDown then
      Sft := Sft + [ssAlt];
    KeyD(Msg.WParam, Sft);
  end
  else if Msg.Message = WM_KEYUP then
  begin
    if IsCtrlDown then
      Sft := Sft + [ssCtrl];
    if IsShiftDown then
      Sft := Sft + [ssShift];
    if IsAltDown then
      Sft := Sft + [ssAlt];
    KeyU(Msg.WParam, Sft);
  end;

end;

constructor TChartWriter.Create(AComponent: TComponent);
var
  Clr: TColor;
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable, csPannable, csOpaque];
  DoubleBuffered := True;
  if csDesigning in ComponentState then
    FDsgnData := TObjectList<TStringList>.Create;
  FSeriesData := TData.Create;
  FChartList := TChartList.Create;
  FOrigData := TData.Create;
  FContractionBase := TData.Create;
  FNameList := TStringList.Create;
  FNameSections := TSections.Create;
  FValueSections := TSections.Create;
  FHistory := THistory.Create;
  FNameFont := TFont.Create;
  FNameFont.Assign(Font);
  FNameFont.OnChange := FontChanged;
  FValueFont := TFont.Create;
  FValueFont.Assign(Font);
  FValueFont.OnChange := FontChanged;
  FTitleFont := TFont.Create;
  FTitleFont.Assign(Font);
  FTitleFont.Size := 12;
  FTitleFont.Color := clBlue;
  FTitleFont.OnChange := FontChanged;
  FTitleAlignment := taCenter;
  FTrackBars := TTrackBars.Create;
  FHWBM := TBitmap.Create;
  FDividerLinePen := TPen.Create;
  FYears := TYears.Create;
  FMouseInfo := miBoth;
  FRulers := ruNone;
  FViewIndex := -1;
  Width := 500;
  Height := 500;
  FNameUnit := 40;
  FNameLabelSpace := 25;
  FValueLabelSpace := 40;
  FMouseSelect := True;
  FInfoControl := True;
  FNameLabelFreq := 1;
  FValueLabelFreq := 1;
  FContraction := 1;
  FContractionType := ctIncremental;
  FDynaSectStart := -1;
  FDynaSectEnd := -1;
  FWallAngle := 90;
  FLastMousePos := Point(0, 0);
  FValAx := TValueAxis.Create;
  FValAx.FWriter := Self;
  FValAx2 := TValueAxis2.Create;
  FActiveValAx := FValAx;
  FValAx2.FWriter := Self;
  FNamAx := TNameAxis.Create;
  FNamAx.FWriter := Self;
  FValAx.Position := apLeft;
  FValAx2.Position := apRight;
  FNamAx.Position := apBottom;
  FValAx.FPartner := FNamAx;
  FNamAx.FPartner := FValAx;
  FSelBM := Vcl.Graphics.TBitmap.Create;
  FAlfaBM := Vcl.Graphics.TBitmap.Create;
  FLastMouseX := MaxInt;
  FGraphBGColor := clWindow;
  FInnerMargins := TCWMargins.Create;
  FInnerMargins.FWriter := Self;
  FGraphMargins := TCWMargins.Create;
  FGraphMargins.FWriter := Self;

  with AnimationTuner do
  begin
    Delay := 10;
    FastFreq := 0;
    MediumFastFreq := 20;
    MediumSlowFreq := 5;
    SlowFreq := 1;
  end;

  FBall := Vcl.Graphics.TBitmap.Create;
  FBall.Width := 8;
  FBall.Height := 8;
  FBall.Canvas.Brush.Color := RGB(153, 180, 209);
  FBall.Canvas.FillRect(Rect(0, 0, 8, 8));
  Clr := RGB(223, 231, 240);
  FAnimBM := Vcl.Graphics.TBitmap.Create;
  FAppEvent := TApplicationEvents.Create(Self);
  FAppEvent.OnMessage := AppMsg;
  with FBall.Canvas do
  begin
    Pen.Color := Clr;
    Pixels[0, 0] := Clr;
    Pixels[1, 0] := Clr;
    Pixels[6, 0] := Clr;
    Pixels[7, 0] := Clr;
    Pixels[0, 1] := Clr;
    Pixels[0, 6] := Clr;
    Pixels[0, 7] := Clr;
    Pixels[1, 7] := Clr;
    Pixels[7, 6] := Clr;
    Pixels[7, 7] := Clr;
    Pixels[6, 7] := Clr;
    Pixels[6, 0] := Clr;
    Pixels[7, 0] := Clr;
    Pixels[7, 1] := Clr;

    Clr := RGB(154, 201, 243);
    Pixels[2, 0] := Clr;
    Pixels[5, 0] := Clr;
    Pixels[0, 2] := Clr;
    Pixels[7, 2] := Clr;
    Pixels[0, 5] := Clr;
    Pixels[7, 5] := Clr;
    Pixels[2, 7] := Clr;
    Pixels[5, 7] := Clr;

    Clr := RGB(166, 190, 215);
    Pixels[4, 2] := Clr;
    Pixels[5, 2] := Clr;
    Pixels[4, 3] := Clr;
    Pixels[5, 3] := Clr;
  end;
  FBall.Transparent := True;
  FBall.TransparentColor := RGB(223, 231, 240);

  FBallSmall := Vcl.Graphics.TBitmap.Create;
  FBallSmall.Width := 7;
  FBallSmall.Height := 7;
  FBallSmall.Canvas.Brush.Color := RGB(0, 163, 255);
  FBallSmall.Canvas.FillRect(Rect(0, 0, 7, 7));
  Clr := RGB(240, 240, 240);
  with FBallSmall.Canvas do
  begin
    Pixels[0, 0] := Clr;
    Pixels[0, 6] := Clr;
    Pixels[6, 6] := Clr;
    Pixels[6, 0] := Clr;

    Clr := RGB(187, 222, 243);
    Pixels[1, 0] := Clr;
    Pixels[5, 0] := Clr;
    Pixels[0, 1] := Clr;
    Pixels[6, 1] := Clr;
    Pixels[0, 5] := Clr;
    Pixels[6, 5] := Clr;
    Pixels[1, 6] := Clr;
    Pixels[5, 6] := Clr;

    Clr := RGB(54, 180, 252);
    Pixels[2, 0] := Clr;
    Pixels[4, 0] := Clr;
    Pixels[0, 2] := Clr;
    Pixels[6, 2] := Clr;
    Pixels[0, 4] := Clr;
    Pixels[6, 4] := Clr;
    Pixels[2, 6] := Clr;
    Pixels[4, 6] := Clr;

    Clr := RGB(62, 186, 255);
    Pixels[1, 1] := Clr;
    Pixels[3, 1] := Clr;
    Pixels[1, 3] := Clr;
    Pixels[3, 3] := Clr;

    Clr := RGB(142, 215, 255);
    Pixels[2, 1] := Clr;
    Pixels[1, 2] := Clr;
    Pixels[2, 2] := Clr;
    Pixels[3, 2] := Clr;
    Pixels[2, 3] := Clr;
  end;
  FBallSmall.Transparent := True;
  FBallSmall.TransparentColor := RGB(240, 240, 240);
  ResetCanvas;
end;

procedure TChartWriter.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FBorder then
    Params.Style := Params.Style or ws_Border;
end;

procedure TChartWriter.SetBorder(Value: Boolean);
begin
  if Value = FBorder then
    Exit;
  FBorder := Value;
  RecreateWnd;
end;

destructor TChartWriter.Destroy;

begin
  if csDesigning in ComponentState then
   FDsgnData.Free;
  FChartList.Free;
  FValAx.Free;
  FNamAx.Free;
  FValAx2.Free;
  FSeriesData.Free;
  FOrigData.Free;
  FContractionBase.Free;
  FNameList.Free;
  FBall.Free;
  FBallSmall.Free;
  FNameSections.Free;
  FDividerLinePen.Free;
  FValueSections.Free;
  FHistory.Free;
  FYears.Free;
  FTrackBars.Free;
  FSelBM.Free;
  FAlfaBM.Free;
  FAnimBM.Free;
  FNameFont.Free;
  FValueFont.Free;
  FTitleFont.Free;
  FHWBM.Free;
  FInnerMargins.Free;
  FGraphMargins.Free;
  inherited;
end;

function TChartWriter.InState(AState: TState): Boolean;
begin
  Result := (AState in FStates);
end;

function TChartWriter.GraphTypeInChart(AGraphType : TClass) : Boolean;
var
 i : integer;
begin
  Result := false;
  if Chart = nil then
    Exit;
  for I := 0 to Chart.FSeriesDefs.Count-1 do
  begin
    if (Chart.FSeriesDefs.Items[i].Graph is AGraphType) and Chart.FSeriesDefs.Items[i].Visible then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TChartWriter.InSpan(ASeries: TSeries; LowVal, HighVal: single;
var ErrorNumber: single): Boolean;
begin
  Result := True;
  ErrorNumber := 0;
  if Chart = nil then
    Exit;

  if InState(stInternalAction) or ValueSpanFromData then
    Exit;
  if (LowVal > ASeries.FMin) or (HighVal < ASeries.FMax) then
  begin
    if LowVal < ASeries.FMin then
      ErrorNumber := LowVal
    else
      ErrorNumber := HighVal;
    Result := False;
  end;
end;

function TChartWriter.InSpan(LowVal, HighVal: single;
var ErrorNumber: single): Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
  begin
    Result := InSpan(FSeriesData[i], LowVal, HighVal, ErrorNumber);
    if not Result then
      Break;
  end;
end;

function TChartWriter.RealYearToLogYear(ARealYear: word): word;
var
  i: integer;
begin
  Result := ARealYear;
  for i := 0 to High(FLogYearMap) do
    if FLogYearMap[i].RealYear = ARealYear then
    begin
      Result := FLogYearMap[i].LogYear;
      Break;
    end;
end;

procedure TChartWriter.RefreshGraph;
begin
  if not(csLoading in componentState) then
  begin
    FUpdateKinds := FUpdateKinds + [ukRestrict, ukLabelFreq];
    RestrictToClient(False);
    CheckLabelFreqs;
    DoRepaint;
  end;
end;

procedure TChartWriter.DoAnimation;
var
  Cnt: integer;
  Boost: integer;
  LS: TSeries;
  Delay: integer;
  Freq: integer;

  function GetSpeed: integer;
  begin
    Result := 0;
    case TCWAxisGraph(ActiveGraph).AnimationSpeed of
      asFast:
        Result := AnimationTuner.FastFreq;
      asMediumFast:
        Result := AnimationTuner.MediumFastFreq;
      asMediumSlow:
        Result := AnimationTuner.MediumSlowFreq;
      asSlow:
        Result := AnimationTuner.SlowFreq;
    end;
  end;

begin
  Delay := AnimationTuner.Delay;
  LS := FLongestSeries;
  if ActiveGraph is TCWCurve then
    TCWCurve(ActiveGraph).Animations := [anFlow];

  SetState(stInitAnimation);
  ClearState(stAnimating);
  Repaint; { Draw labels and lines, but not the graph, to the screen }
  ClearState(stInitAnimation);
  SetState(stAnimating); { Run }
  Freq := GetSpeed;
  Cnt := 0;

  if ActiveGraph is TCWBar then
    Boost := TCWAxisGraph(ActiveGraph).AnimationBooster
  else
    Boost := 0;

  if Boost < 0 then
    Delay := Delay + Boost * 10;

  while InState(stAnimating) do
  begin
    Repaint;
    if anGrow in TCWAxisGraph(ActiveGraph).Animations then
    begin
      if Freq > 0 then
        if Cnt mod Freq = 0 then
          Sleep(Delay);
    end
    else
    begin
      if Boost = 0 then
      begin
        if Freq > 0 then
        begin
          if Cnt mod Freq = 0 then
            Sleep(Delay);
        end
        else
          Sleep(0);
      end
      else
      begin
        if Boost > 0 then
        begin
          if Freq = 0 then
            Sleep(0)
          else if Cnt mod (Freq + Boost) = 0 then
            Sleep(Delay);
        end
        else
          Sleep(Delay);
      end;
    end;
    inc(Cnt);
    if ActiveGraph is TCWBar then
      if (anPause in TCWBar(ActiveGraph).Animations) and FAnimInfo.Paused and
        (TCWBar(ActiveGraph).Layout = blSideBySide) and
        (FAnimInfo.NextHorz < Count * LS.Count) then
      begin
        SetState(stAnimationPause);
        SaveSelBM;
        while InState(stAnimationPause) do
        begin
          Application.ProcessMessages;
        end;
      end;
  end;
  ClearState(stAnimationPause);
  ClearState(stResumeAnimation);
  Repaint;
end;

procedure TChartWriter.InitAnimation;
begin
  if csDesigning in ComponentState then
    Exit;
  FAnimInfo.NextHorz := 0;
  FAnimInfo.NextVert := 0;
  FAnimInfo.NextSeries := 0;
  FAnimInfo.LastXY := 0;
  FAnimInfo.Stopped := False;
  FAnimInfo.StartPause := True;
  SetState(stAnimating);
end;

procedure TChartWriter.ClearState(AState: TState);
begin
  Exclude(FStates, AState);
end;

procedure TChartWriter.SetState(AState: TState);
begin
  Include(FStates, AState);
end;

function TChartWriter.BaseNameInterval: integer;
var
  Dt1, Dt2: TDateTime;
  Itm1, Itm2: TSeriesItem;
begin
  Result := 1;
  if Chart = nil then
   Exit;
  if FOrigData.Count = 0 then
    Exit;
  Itm1 := FOrigData[0].FSeriesItems[0];
  Itm2 := FOrigData[0].FSeriesItems[1];
  if IsTimeSpan(Chart.NameType) then
  begin
    Dt1 := StrToDateTime(Itm1.FName, Fmt);
    Dt2 := StrToDateTime(Itm2.FName, Fmt);
    case Chart.NameType of
      ntDateSpan:
        Result := DaysBetween(Dt2, Dt1);
      ntHourSpan:
        Result := HoursBetween(Dt2, Dt1);
      ntMinuteSpan:
        Result := MinutesBetween(Dt2, Dt1);
      ntSecondSpan:
        Result := SecondsBetween(Dt2, Dt1);
      ntNumberSpan:
        Result := StrToInt(Itm2.FName) - StrToInt(Itm1.FName)
    end;
  end;
end;

procedure TChartWriter.Clear;
begin
  InternalClear;
  FNameSections.Clear;
  FValueSections.Clear;
  FOrigData.Clear;
  ClearState(stZoomed);
  FContractionBase.Clear;
  FViewMode := vmNormal;
  Invalidate;
end;

procedure TChartWriter.ClearSelection;
begin
  FViewMode := vmNormal;
  FDynaSectStart := -1;
  FDynaSectEnd := -1;
  FKeepSelection := False;
  if not InState(stAnimating) then
    DoRepaint;
end;

procedure TChartWriter.InternalClear;
begin
  FSeriesData.Clear;
  FLongestSeries := nil;
  ClearSelection;
  FNameList.Clear;
  FViewMode := vmNormal;
  FDynaSectStart := -1;
  FDynaSectEnd := -1;
  FNumberInterval := 0;
  FLeapdateCount := 0;
  FScrollIndex := -1;
  ClearState(stUpdating);
  FYears.Clear;
  if not InState(stAnimating) and not (csDestroying in ComponentState) then
    DoRepaint;
end;

procedure TChartWriter.BeginUpdate;
begin
  if InState(stActivating) then
    Exit;
  SetState(stUpdating);
  FUpdateKinds := [];
end;

procedure TChartWriter.EndUpdate;
begin
  if InState(stActivating) then
    Exit;
  { Prevent user to set this state when OnActivateGraph is executing }
  if not InState(stUpdating) then
    Exit;
  if FUpdateKinds = [] then
  begin
    ClearState(stUpdating);
    Exit;
  end;
  ClearState(stUpdating);
  if ukRestrict in FUpdateKinds then
    RestrictToClient(False);
  if ukLabelFreq in FUpdateKinds then
    CheckLabelFreqs;
  if ukScroll in FUpdateKinds then
    DoScrollTo(FScrollIndex);
  FUpdateKinds := [];
  DoRepaint;
end;

procedure TChartWriter.GoBackError;
var
  Msg: TMsg;
begin
  InternalClear;
  PeekMessage(Msg, Handle, WM_SAVEHIST, WM_SAVEHIST, PM_REMOVE);
  DoRepaint;
end;

procedure TChartWriter.GoBack;
begin
  if not CanGoBack then
    Exit;
  FViewIndex := FViewIndex - 1;
  SetState(stRestoring);
  FHistory[FViewIndex].Restore;
  RestrictToClient(True);
  CheckLabelFreqs;
  ClearState(stRestoring);
  if Assigned(FOnDataChange) then
    FOnDataChange(Self);
  DoRepaint;
end;

procedure TChartWriter.GoForward;
begin
  if not CanGoForWard then
    Exit;
  FViewIndex := FViewIndex + 1;
  SetState(stRestoring);
  FHistory[FViewIndex].Restore;
  RestrictToClient(True);
  CheckLabelFreqs;
  ClearState(stRestoring);
  if Assigned(FOnDataChange) then
    FOnDataChange(Self);
  DoRepaint;
end;

function TChartWriter.CanGoBack: Boolean;
begin
  Result := False;
  if csDestroying in componentState then
    Exit;
  Result := KeepHistory and (FHistory.Count > 1);
  if not Result then
    Exit;
  Result := (FViewIndex - 1 >= 0);
end;

function TChartWriter.CanGoForWard: Boolean;
begin
  Result := False;
  if csDestroying in componentState then
    Exit;
  Result := KeepHistory and (FHistory.Count > 1);
  if not Result then
    Exit;
  Result := (FViewIndex + 1 <= FHistory.Count - 1);
end;

procedure TChartWriter.DoSaveToHistory;
begin
  if csDesigning in componentState then
    Exit;
  if not KeepHistory then
    Exit;
  SaveToHistory;
end;

procedure TChartWriter.Reload;
var
  i: integer;
  Ser: TSeries;
begin
  if (csLoading in componentState) then
    Exit;
  if Count = 0 then
    Exit;
  FSeriesData.Clear;
  for i := 0 to FOrigData.Count - 1 do
  begin
    Ser := TSeries.Create;
    Ser.Assign(FOrigData[i], -1, -1);
    FSeriesData.Add(Ser);
  end;
  Execute;
end;

procedure TChartWriter.SaveToHistory;
var
  V: THistoryItem;
begin
  if InState(stUpdating) or (FSeriesData.Count = 0) then
    Exit;
  V := THistoryItem.Create;
  V.FWriter := Self;
  V.Assign;
  FHistory.Add(V);
  FViewIndex := FHistory.Count - 1;
end;

function TChartWriter.IsPointVisible(AScrollIndex: integer): Boolean;
var
  EndPt: integer;
begin
  EndPt := FScrollIndex + FSeriesData[0].FSeriesItems.Count;
  Result := (AScrollIndex <= EndPt) and (AScrollIndex >= FScrollIndex);
end;

function TChartWriter.CanScroll(ScrollType: TScrollType): Boolean;
begin
  Result := False;
  if csDestroying in componentState then
    Exit;
  if FOrigData.Count = 0 then
    Exit;
  case ScrollType of
    stNext, stNextPage, stLast:
      Result := not IsPointVisible(FOrigData[0].FSeriesItems.Count);
    stPrev, stPrevPage, stFirst:
      Result := not IsPointVisible(0);
  end;
end;

function TChartWriter.CanScrollBy(Delta: integer): Boolean;
begin
  Result := False;
  if Delta = 0 then
    Exit;
  Delta := FScrollIndex + Delta;
  Result := CanScrollTo(Delta);
end;

function TChartWriter.CanScrollTo(AScrollIndex: integer): Boolean;
begin
  Result := False;
  if csDestroying in componentState then
    Exit;
  if FOrigData.Count = 0 then
    Exit;
  if AScrollIndex = FScrollIndex then
    Exit
  else if not Scrollable then
    Exit
  else if AScrollIndex < 0 then
    Exit
  else if AScrollIndex > FScrollIndex then
  begin
    if IsPointVisible(FOrigData[0].Count - 1) then
      Exit;
  end
  else if AScrollIndex < FScrollIndex then
  begin
    if IsPointVisible(0) then
      Exit;
  end;
  Result := True;
end;

function TChartWriter.AllVisible: Boolean;
begin
  Result := IsPointVisible(FOrigData[0].Count - 1) and IsPointVisible(0)
end;

procedure TChartWriter.SetScrollIndex(Value: integer);
begin
  if Value = FScrollIndex then
    Exit;
  if not CanScrollTo(Value) then
    Exit;
  DoScrollTo(Value);
end;

function TChartWriter.GetScrollPointSpacing: integer;
var
  G: TCWGraph;
  Cpr: Boolean;
  B : integer;
begin
  Result := 10;
  G := ActiveGraph;
  if G is TCWBar then
    with G as TCWBar do
    begin
      GetBarSpace(Result, Cpr, B);
    end
  else if G is TCWCurve then
    with G as TCWCurve do
      Result := MinPointSpacing;
end;

procedure TChartWriter.DoScrollTo(AScrollIndex: integer);
var
  i, j: integer;
  Ser, NewSer: TSeries;
  TheSeries: TList<TSeries>;
  Cnt: integer;
  Itm: TSeriesItem;
  APointSpacing: integer;
  PIndx: integer;
begin
  if not (ActiveGraph is TCWAxisGraph) then
    Exit;
  if FSeriesData.Count = 0 then
    Exit;
  if InState(stUpdating) then
    Exit;
  FScrolling := True;
  ClearState(stZoomed);
  APointSpacing := GetScrollPointSpacing;
  PIndx := FScrollIndex;

  TheSeries := TList<TSeries>.Create;
  try
    for i := 0 to FOrigData.Count - 1 do
    begin
      Ser := FOrigData[i];
      NewSer := TSeries.Create;
      NewSer.FWriter := Self;
      NewSer.FIndent := 0;
      NewSer.FExdent := 0;
      if FNamAx.IsXAxis then
        Cnt := round(GraphRect.Width / APointSpacing)
      else
        Cnt := round(GraphRect.Height / APointSpacing);
      for j := AScrollIndex to AScrollIndex + Cnt - 1 do
      begin
        if j < Ser.FirstItem then
          Continue;
        if j > Ser.LastItem then
          Break;
        Itm := NewSer.AddItem;
        Itm.Assign(Ser.FSeriesItems[j]);
        Itm.FItems := NewSer.FSeriesItems;
        Itm.FOwner := NewSer;

      end;
      if NewSer.Count = 0 then
      begin
        NewSer.Free;
        Exit;
      end;
      TheSeries.Add(NewSer);
    end;
    InternalClear;
    SetState(stInternalAction);
    try
      for i := 0 to TheSeries.Count - 1 do
      begin
        AddSeries(TheSeries[i], True);
      end;
      FScrollIndex := AScrollIndex;
      FContractionBase.Clear;
      Execute;
    finally
      ClearState(stInternalAction);
    end;
  finally
    TheSeries.Free;
    FScrolling := False;
    if InState(stAnimating) and (PIndx <> -1) then
      DoAnimation;
  end;
end;

procedure TChartWriter.ScrollTo(AScrollIndex: integer);
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  if AScrollIndex <> -1 then
    if not CanScrollTo(AScrollIndex) then
      Exit;
  if AScrollIndex = -1 then
    AScrollIndex := 0; { Initial from RestrictToClient }
  DoScrollTo(AScrollIndex);
end;

procedure TChartWriter.ScrollBy(Delta: integer);
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  if Delta = 0 then
    Exit;
  Delta := FScrollIndex + Delta;
  ScrollTo(Delta);
end;

procedure TChartWriter.First;
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  ScrollTo(0);
end;

procedure TChartWriter.Last;
var
  W: integer;
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  if FNamAx.IsXAxis then
    W := GetPosRect.Width
  else
    W := GetPosRect.Height;
  ScrollTo(FOrigData[0].Count - floor(W / GetScrollPointSpacing));
end;

procedure TChartWriter.NextPage;
var
  W: integer;
  Delta: integer;
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  FRightSpace := FOrigGraphSize.cx;
  FBottomSpace := FOrigGraphSize.cy;
  if FNamAx.IsXAxis then
  begin
    W := GraphRect.Width;
  end
  else
  begin
    W := GraphRect.Height;
  end;
  Delta := FScrollIndex + round(W / GetScrollPointSpacing);
  if Delta + round(W / GetScrollPointSpacing) > FOrigData[0].Count - 1 then
  begin
    Delta := FOrigData[0].Count - round(W / GetScrollPointSpacing);
    if Delta < 0 then
      Delta := 0;
  end;
  ScrollTo(Delta);
end;

procedure TChartWriter.PrevPage;
var
  W: integer;
  Delta: integer;
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  if FNamAx.IsXAxis then
     W := GraphRect.Width
  else
    W := GraphRect.Height;
  Delta := FScrollIndex - round(W / GetScrollPointSpacing);
  if Delta < 0 then
    Delta := 0;
  ScrollTo(Delta);
end;

procedure TChartWriter.NextPoint;
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  ScrollTo(FScrollIndex + 1);
end;

procedure TChartWriter.PrevPoint;
begin
  if not Scrollable or (ActiveGraph is TCWPie) or (FSeriesData.Count = 0) then
    Exit;
  ScrollTo(FScrollIndex - 1);
end;

function TChartWriter.AsBitmap(GraphElement: integer): TBitmap;
var
  RSource, RDest: TRect;
begin
  Result := TBitmap.Create;
  if GraphElement = GraphOnly then
  begin
    RSource := GraphRect;
  end
  else if GraphElement = GraphAndLabels then
  begin
    RSource := WorkRect;
  end
  else
  begin
    RSource := ClientRect;
  end;
  inc(RSource.Right);
  inc(RSource.Bottom);
  Result.SetSize(RSource.Width, RSource.Height);
  RDest := Rect(0, 0, Result.Width, Result.Height);
  Result.Canvas.CopyRect(RDest, Canvas, RSource);
end;

procedure TChartWriter.RepaintGraph;
begin
  DoRepaint;
end;

procedure TChartWriter.SaveAsImage(GraphElement: integer; AFileName: string);
var
  BM: TBitmap;
  j: TJPEGImage;
begin
  BM := AsBitmap(GraphElement);
  j := TJPEGImage.Create;
  try
    j.Assign(BM);
    j.SaveToFile(AFileName);
  finally
    BM.Free;
    j.Free;
  end;
end;

function TChartWriter.GetActiveGraph: TCWGraph;
{In mixed charts the bar will always be the master, while the curve must
adapt it's point postions to the bar center points. See ComputePoints}
var
  i : integer;
  D : TCWSeriesDefs;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  Result := Chart.AllEqual;
  if Result <> nil then
    Exit;
  D := Chart.FSeriesDefs;
  for I := 0 to D.Count-1 do
  begin
    if D.Items[i].Graph is TCWBar then {Bar has 1. prio}
    begin
      Result := D.Items[i].Graph;
      Break;
    end
    else
      Result := D.Items[i].Graph;
  end;
end;

procedure TChartWriter.CreateAutoSections;
var
  FIntActn: Boolean;

  procedure SetMonthSections;
  var
    St, Nd: string;
    FirstD, LastD, ThisD: TDateTime;
    M, D, Y: word;
    SectStart, SectEnd: string;
    MN, SN: string;

    function MonthName(MonthNum: integer): string;
    begin
      Result := Fmt.LongMonthNames[MonthNum];
    end;

    function ShortMonthName(MonthNum: integer): string;
    begin
      Result := Fmt.ShortMonthNames[MonthNum];
    end;

    procedure FindFirst;
    begin
      while (D <> 1) and (ThisD < LastD) do
      begin
        ThisD := ThisD + 1;
        DecodeDate(ThisD, Y, M, D);
      end;
      if M = 1 then
        M := 13;
      MN := MonthName(M - 1);
      SN := ShortMonthName(M - 1);
    end;

  begin
    St := Names[0];
    Nd := FNameList[FNameList.Count - 1];
    FirstD := DateOf(StrToDateTime(St, Fmt));
    LastD := DateOf(StrToDateTime(Nd, Fmt));
    ThisD := FirstD;
    SectStart := DateToStr(FirstD, Fmt);
    DecodeDate(ThisD, Y, M, D);
    if (MonthOf(LastD) = MonthOf(FirstD)) and (YearOf(LastD) = YearOf(FirstD))
    then
    begin
      SectEnd := DateToStr(LastD, Fmt);
      MN := MonthName(M);
      AddSection(atNameAxis, SectStart, SectEnd, MN, ShortMonthName(M),
        stSection).FIsAuto := True;
      Exit;
    end;
    ThisD := ThisD + 1;
    DecodeDate(ThisD, Y, M, D);
    FindFirst;
    SectEnd := DateToStr(ThisD, Fmt);
    AddSection(atNameAxis, SectStart, SectEnd, MN, SN, stSection)
      .FIsAuto := True;
    SectStart := SectEnd;

    repeat
      ThisD := ThisD + 27;
      if ThisD >= LastD then
      begin
        ThisD := LastD;
        SectEnd := DateToStr(ThisD, Fmt);
        DecodeDate(ThisD, Y, M, D);
        MN := MonthName(M);
        SN := ShortMonthName(M);
      end
      else
      begin
        DecodeDate(ThisD, Y, M, D);
        FindFirst;
        SectEnd := DateToStr(ThisD, Fmt);
        if ThisD = LastD then
        begin
          if DaysBetween(ThisD, StrToDate(Nd, Fmt)) = 0 then
          begin
            MN := MonthName(MonthOf(LastD - 1));
            SN := ShortMonthName(MonthOf(LastD - 1));
          end
          else
          begin
            MN := MonthName(MonthOf(ThisD));
            SN := ShortMonthName(MonthOf(ThisD));

          end;
        end;
      end;
      AddSection(atNameAxis, SectStart, SectEnd, MN, SN, stSection)
        .FIsAuto := True;
      SectStart := SectEnd;
    until ThisD = LastD;
  end;

  procedure SetYearSections;
  var
    St, Nd: string;
    FirstD, LastD, ThisD: TDateTime;
    SectStart, SectEnd: string;
    NY: integer;

    procedure FindFirst(ANowYear: integer);
    begin
      NY := YearOf(FirstD);
      while NY = ANowYear do
      begin
        ThisD := ThisD + 1;
        NY := YearOf(ThisD);
      end;
      ThisD := ThisD - 1;
      NY := YearOf(ThisD);
    end;

  begin
    St := Names[0];
    Nd := Names[FNameList.Count - 1];
    FirstD := StrToDateTime(St, Fmt);
    LastD := StrToDateTime(Nd, Fmt);
    ThisD := FirstD;
    SectStart := DateToStr(FirstD, Fmt);
    if (YearOf(LastD) = YearOf(FirstD)) then
    begin
      SectEnd := DateToStr(LastD, Fmt);
      AddSection(atNameAxis, SectStart, SectEnd, IntToStr(YearOf(LastD)), '',
        stSection).FIsAuto := True;
      Exit;
    end;
    ThisD := ThisD + 1;
    FindFirst(YearOf(ThisD));
    SectEnd := DateToStr(ThisD, Fmt);
    AddSection(atNameAxis, SectStart, SectEnd, IntToStr(YearOf(ThisD)), '',
      stSection).FIsAuto := True;
    SectStart := SectEnd;

    repeat
      ThisD := ThisD + DaysInAYear(YearOf(ThisD + 1));
      if ThisD >= LastD then
      begin
        ThisD := LastD;
        SectEnd := DateToStr(ThisD, Fmt);
      end
      else
      begin
        SectEnd := DateToStr(ThisD, Fmt);
      end;
      AddSection(atNameAxis, SectStart, SectEnd, IntToStr(YearOf(ThisD)), '',
        stSection).FIsAuto := True;
      SectStart := SectEnd;
    until ThisD = LastD;
  end;

  procedure SetWeekSections;
  var
    St, Nd: string;
    FirstD, LastD, ThisD: TDateTime;
    SectStart, SectEnd: string;
    WN: string;
    NW: integer;

    function WeekName(WeekNum: integer): string;
    begin
      Result := IntToStr(WeekNum);
    end;

    procedure FindFirst(ANowWeek: integer);
    begin
      NW := WeekOf(FirstD);
      while NW = ANowWeek do
      begin
        ThisD := ThisD + 1;
        NW := WeekOf(ThisD);
      end;
      ThisD := ThisD - 1;
      NW := WeekOf(ThisD);

    end;

  begin
    St := Names[0];
    Nd := Names[FNameList.Count - 1];
    FirstD := StrToDateTime(St, Fmt);
    LastD := StrToDateTime(Nd, Fmt);
    ThisD := FirstD;
    SectStart := DateTimeToStr(FirstD, Fmt);
    if (WeekOf(LastD) = WeekOf(FirstD)) and (YearOf(LastD) = YearOf(FirstD))
    then
    begin
      WN := WeekName(WeekOf(LastD));
      SectEnd := DateTimeToStr(LastD, Fmt);
      AddSection(atNameAxis, SectStart, SectEnd, WN, IntToStr(WeekOf(LastD)),
        stSection).FIsAuto := True;
      Exit;
    end;
    ThisD := ThisD + 1;
    FindFirst(WeekOf(ThisD));
    WN := WeekName(NW);
    SectEnd := DateTimeToStr(ThisD, Fmt);
    AddSection(atNameAxis, SectStart, SectEnd, WN, IntToStr(NW), stSection)
      .FIsAuto := True;
    SectStart := SectEnd;

    repeat
      ThisD := ThisD + 7;
      if ThisD >= LastD then
      begin
        ThisD := LastD;
        WN := WeekName(WeekOf(ThisD));
        SectEnd := DateTimeToStr(ThisD, Fmt);
      end
      else
      begin
        WN := WeekName(WeekOf(ThisD));
        SectEnd := DateTimeToStr(ThisD, Fmt);
      end;
      AddSection(atNameAxis, SectStart, SectEnd, WN, IntToStr(WeekOf(ThisD)),
        stSection).FIsAuto := True;
      SectStart := SectEnd;
    until ThisD = LastD;
  end;

  procedure SetDaySections;
  var
    St, Nd: string;
    FirstD, LastD, ThisD: TDateTime;
    SectStart, SectEnd: string;
    WN, WN2: string;
    NW: integer;
    Cap1, Cap2: string;

    procedure SetCaps(Dt: TDateTime);
    begin
      if NameSectionDefs.AutoSections = autDays then
      begin
        Cap1 := WN;
        Cap2 := WN2
      end
      else
      begin
        Cap1 := DateToStr(Dt, Fmt);
        Cap2 := '';
      end;

    end;

    function DayName(Long: Boolean; ADate: TDateTime): string;
    begin
      if Long then
      begin
        Result := FormatDateTime('dddd', ADate, Fmt);
      end
      else
      begin
        Result := FormatDateTime('ddd', ADate, Fmt)
      end;
    end;

    procedure FindFirst(ANowDay: integer);
    begin
      NW := DayOfTheWeek(FirstD);
      while NW = ANowDay do
      begin
        ThisD := ThisD + EncodeTime(1, 0, 0, 0);
        NW := DayOfTheWeek(ThisD);
      end;
    end;

  begin
    St := Names[0];
    Nd := Names[FNameList.Count - 1];
    FirstD := StrToDateTime(St, Fmt);
    LastD := StrToDateTime(Nd, Fmt);
    ThisD := FirstD;
    SectStart := DateTimeToStr(FirstD, Fmt);
    if LastD = FirstD then
    begin
      WN := DayName(True, LastD);
      WN2 := DayName(False, LastD);
      SetCaps(LastD);
      SectEnd := DateTimeToStr(LastD, Fmt);
      AddSection(atNameAxis, SectStart, SectEnd, Cap1, Cap2, stSection)
        .FIsAuto := True;
      Exit;
    end;

    WN := DayName(True, ThisD);
    WN2 := DayName(False, ThisD);
    SetCaps(ThisD);
    FindFirst(DayOfTheWeek(ThisD)); { Now we are past the first date }
    SectEnd := DateTimeToStr(ThisD, Fmt);
    AddSection(atNameAxis, SectStart, SectEnd, Cap1, Cap2, stSection)
      .FIsAuto := True;
    SectStart := SectEnd;

    repeat
      WN := DayName(True, ThisD);
      WN2 := DayName(False, ThisD);
      SetCaps(ThisD);
      ThisD := ThisD + 1;
      if ThisD >= LastD then
        SectEnd := DateTimeToStr(LastD, Fmt)
      else
        SectEnd := DateTimeToStr(ThisD, Fmt);
      AddSection(atNameAxis, SectStart, SectEnd, Cap1, Cap2, stSection)
        .FIsAuto := True;
      SectStart := SectEnd;
    until ThisD >= LastD;
  end;

begin
  if not IsTimeSpan(Chart.NameType) then
    Exit;
  if FSeriesData.Count = 0 then
    Exit;
  if NameSectionDefs.FAutoSections = autNotUsed then
    Exit;
  FNameSections.Clear;
  FIntActn := InState(stInternalAction);
  SetState(stInternalAction);
  try
    case NameSectionDefs.FAutoSections of
      autWeeks:
        SetWeekSections;
      autMonths:
        SetMonthSections;
      autYears:
        SetYearSections;
      autDays, autDates:
        SetDaySections;
    end;
  finally
    if FIntActn then
      SetState(stInternalAction)
    else
      ClearState(stInternalAction);
  end;
end;

procedure TChartWriter.ChangeChart(AChart : TCWChart; Refresh : Boolean = false);
begin
  if AChart = Chart then
    Exit;
  Chart := AChart;
  {if Refresh then
  begin
    RefreshGraph;
    PostHist;
  end;}
end;

procedure TChartWriter.CheckSeriesDefs;
var
  i : integer;
begin
  for I := 0 to Chart.SeriesDefs.Count-1 do
    begin
      if Chart.SeriesDefs.Items[i].Graph = nil then
      begin
        ShowGWError(msg_UndefGraph);
      end;
      if (Chart.SeriesDefs.Items[i].Graph is TCWBar)
      and (TCWBar(Chart.SeriesDefs.Items[i].Graph).Layout = blSideBySide)
      and (Chart.OverflowAction = ovCompression)
      and (Count > 1)
      then
        ShowGWError(msg_SideBySideCompression);
    end;
end;

procedure TChartWriter.CheckValspans;
var
  i: integer;
  ErrNumber: single;
  Itm : TCWSeriesDef;
  H1, L1, H2, L2 : single;
  DoH1, DoH2 : Boolean;
begin
   H1 := -Maxint;
   L1 := Maxint;
   H2 := -Maxint;
   L2 := Maxint;
   DoH1 := false;
   DoH2 := false;
   if Chart = nil then
     Exit;
   for I := 0 to Chart.FSeriesDefs.Count-1 do
   begin
     Itm := Chart.FSeriesDefs.Items[i];
     if Itm.ValueAxis = vaValueAxis1 then
     begin
       if Chart.ValueAxis1.ValueSpanFromData
       or not InSpan(Series[i],Chart.ValueAxis1.FValueLow, Chart.ValueAxis1.FValueHigh, ErrNumber) then
       begin
         if FSeriesData[i].FMax > H1 then
           H1 := FSeriesData[i].FMax;
         if FSeriesData[i].FMin < L1 then
           L1 := FSeriesData[i].FMin;
         DoH1 := True;
       end
     end
     else
     begin
       if Chart.ValueAxis2.ValueSpanFromData
       or not InSpan(Series[i],Chart.ValueAxis2.FValueLow, Chart.ValueAxis2.FValueHigh, ErrNumber) then
       begin
         begin
         if FSeriesData[i].FMax > H2 then
           H2 := FSeriesData[i].FMax;
         if FSeriesData[i].FMin < L2 then
           L2 := FSeriesData[i].FMin;
         DoH2 := True;
       end
      end;
     end;
   end;

   if DoH1 then
   begin
       if Chart.ValueAxis1.FValueHigh < H1 then
         Chart.ValueAxis1.FValueHigh := H1;
       if Chart.ValueAxis1.FValueLow > L1 then
         Chart.ValueAxis1.FValueLow := L1;
   end;
   if DoH2 then
   begin
       if Chart.ValueAxis2.FValueHigh < H2 then
         Chart.ValueAxis2.FValueHigh := H2;
       if Chart.ValueAxis2.FValueLow > L2 then
         Chart.ValueAxis2.FValueLow := L2;
   end;
end;

procedure TChartWriter.Execute;
var
  i, j: integer;
  LastSerInd: integer;
  LastYear: integer;
  Ser: TSeries;
  SerItm: TSeriesItem;
  Nt: TNameType;

  procedure CheckFlow(Items: TSeriesItems);
  var
    i: integer;
    LastD: TDateTime;
    LastN: integer;
    Dt: TDateTime;
    n: integer;
  begin
    if (Chart.NameType = ntGeneral) then
      Exit;
    if FNumberInterval = 0 then
      Exit;
    try
      LastD := 0;
      LastN := 0;
      if IsTimeSpan(Chart.NameType) then
        LastD := StrToDateTime(Items[0].FName, Fmt)
      else
        LastN := StrToInt(Items[0].FName);

      for i := 1 to Items.Count - 1 do
      begin
        if Chart.NameType = ntDateSpan then
        begin
          Dt := StrToDate(Items[i].FName, Fmt);
          if Dt < LastD then
          begin
            ShowGWError(msg_GreaterPred, '(Date ' + Items[i].FName + ')');
          end;
          if (DaysBetween(Dt, LastD) <> FNumberInterval) and
            not InState(stInternalAction) then
            ShowGWError(msg_DateIntervals);
          LastD := StrToDate(Items[i].FName, Fmt)
        end
        else if IsTimeSpan(Chart.NameType) then
        begin
          Dt := StrToDateTime(Items[i].FName, Fmt);
          if Dt < LastD then
          begin
            ShowGWError(msg_GreaterPred, '(Date ' + Items[i].FName + ')');
          end;
          if Chart.NameType = ntHourSpan then
            n := HoursBetween(Dt, LastD)
          else if Chart.NameType = ntMinuteSpan then
            n := MinutesBetween(Dt, LastD)
          else
            n := SecondsBetween(Dt, LastD);

          if (n <> FNumberInterval) and not InState(stInternalAction) then
            ShowGWError(msg_DateIntervals);
          LastD := StrToDateTime(Items[i].FName, Fmt)
        end
        else if Chart.NameType = ntNumberSpan then
        begin
          n := StrToInt(Items[i].FName);
          if n < LastN then
          begin
            ShowGWError(msg_GreaterPred, '( Number ' + Items[i].FName + ')');
          end;
          if (n - LastN <> FNumberInterval) and not InState(stInternalAction)
          then
            ShowGWError(msg_NumIntervals);
          LastN := StrToInt(Items[i].FName)
        end;
      end;
    except
      raise
    end;
  end;

  procedure InsertLeapDummies;
  var
    i, j: integer;
    Y, M, D: word;
    InsertionCount: integer;
    Insertions: array [1 .. 10] of integer;
    Dt: TDateTime;
    SerItm: TSeriesItem;
    Found: Boolean;

    function InLeapYear(ThisYear: word; SerInd: integer): Boolean;
    var
      i, j: integer;
      Y, M, D: word;
    begin
      Result := False;
      for i := 0 to FSeriesData.Count - 1 do
      begin
        if i = SerInd then
          Continue;
        for j := 0 to FSeriesData[i].Count - 1 do
        begin
          DecodeDate(FSeriesData[i].ToDate(j), Y, M, D);
          if IsLeapYear(Y) and (Y > ThisYear) then
          begin
            Result := True;
            Break;
          end;
        end;
        if Result then
          Break;
      end;
    end;

  begin
    for i := 0 to FSeriesData.Count - 1 do
    begin
      InsertionCount := 0;
      Found := False;
      for j := 0 to FSeriesData[i].Count - 1 do
      begin
        Dt := FSeriesData[i].ToDate(j);
        DecodeDate(Dt, Y, M, D);
        if IsLeapYear(Y) or InLeapYear(Y, i) then
          Continue;
        if ((M = 2) and (D = 28) and (j = 0)) then
        begin
          inc(InsertionCount);
          Insertions[InsertionCount] := j + 1;
          Found := True;
        end
        else if ((M = 3) and (D = 1) and (j > 0)) and not Found then
        begin
          inc(InsertionCount);
          Insertions[InsertionCount] := j;
          Found := True;
        end;
      end;

      for j := 1 to InsertionCount do
      begin
        if Insertions[j] = FSeriesData[i].Count - 1 then
        begin
          SerItm := FSeriesData[i].AddItem;
          SerItm.Assign(FSeriesData[i].FSeriesItems[Insertions[j]]);
        end
        else
        begin
          SerItm := TSeriesItem.Create;
          SerItm.Assign(FSeriesData[i].FSeriesItems[Insertions[j]]);
          FSeriesData[i].FSeriesItems.Insert(Insertions[j], SerItm);
        end;
        SerItm.FLeapDummy := True;
      end;
    end;
  end;

  procedure LinkTitles;
  var
    i : integer;
    S1, S2 : string;
    Indx : integer;
    Moved : Boolean;
  begin
    if Count = 1 then
      Exit;
    S1 := FSeriesData[0].FLinkTitle;
    for I := 1 to Count-1 do
    begin
       S2 := FSeriesData[I].FLinkTitle;
       if ((S2='') and (S1<>''))
       or ((S2<>'') and (S1='')) then
         ShowGWError(msg_TitlesIncomplete);
    end;

    if S1 = '' then
      Exit;

    repeat
      Moved := false;
      for I := 0 to Count-1 do
      begin
         Indx := Chart.SeriesDefs.IndexOfTitle(FSeriesData[I].FLinkTitle);
         if Indx = -1 then
           ShowGWError(msg_TitleNotFound);
         if Indx <> i then
         begin
           FSeriesData.Move(I, Indx);
           Moved := True;
           Break;
         end;
      end;
    until not Moved;
  end;

begin
  if (FSeriesData.Count = 0) then
  begin
    Clear;
    ShowGWError(msg_NoSeries);
  end;
  try
    if not InState(stExecuting) then
    begin
      SetState(stExecuting);
      PostMessage(Handle, WM_ENDEXEC, 0, 0);
    end;

    CheckSeriesDefs;
    if Chart.FSeriesDefs.Count <> FSeriesData.Count then
      ShowGWError(msg_FormalActual);

    LinkTitles;

    FLeapdateCount := 0;
    if not FScrolling then
      for i := 0 to FSeriesData.Count - 1 do
      begin
        if FSeriesData[i].Count = 1 then
        begin
          ShowGWError(msg_TwoValues);
        end;
      end;

    Nt := ntGeneral;
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Ser := FSeriesData[i];
      if i = 0 then
        Nt := DetectNameType(i)
      else if DetectNameType(i) <> Nt then
      begin
        ShowGWError(msg_NameTypeConflict);
      end;

      try
        CheckFlow(Ser.FSeriesItems);
      except
        Chart.FNameType := ntGeneral;
      end;
      if Chart.NameType = ntDateSpan then
      begin
        NormaliseDates(Ser, i);
      end
      else
        for j := 0 to FSeriesData[i].Count - 1 do
        begin
          SerItm := FSeriesData[i].FSeriesItems[j];
          SerItm.FVisible := True;
        end;
    end;

    if FLeapdateCount > 0 then
      InsertLeapDummies;

    FLongestSeries := LongestSeries;

    CreateXValues;
    if Chart.NameType = ntDateSpan then
    begin
      LastSerInd := FYears[0].SerIndex;
      LastYear := FYears[0].Year;
      for i := 0 to FYears.Count - 1 do
      begin
        if (FYears[i].SerIndex <> LastSerInd) and (FYears[i].Year <> LastYear)
        then
        begin
          Break;
        end
        else
        begin
          LastSerInd := FYears[i].SerIndex;
          LastYear := FYears[i].Year;
        end;
      end;
    end;

    CheckValspans;
    ComputeTextExtent;

    FRightSpace := FOrigGraphSize.cx;
    FBottomSpace := FOrigGraphSize.cy;

    ClearState(stUpdating);
    if not InState(stInternalAction) then
    begin
      FScrollIndex := -1;
    end;

    if (not FScrolling and (FHistory.Count = 0)) or not InState(stInternalAction)
    then
      PostHist;

    if (FContractionBase.Count = 0) and (Chart.NameType <> ntGeneral) then
    begin
      if InState(stZoomed) then
        for i := 0 to FSeriesData.Count - 1 do
        begin
          Ser := TSeries.Create;
          Ser.Assign(FSeriesData[i], -1, -1);
          FContractionBase.Add(Ser);
        end
        else
        for i := 0 to FOrigData.Count - 1 do
        begin
          Ser := TSeries.Create;
          Ser.Assign(FOrigData[i], -1, -1);
          FContractionBase.Add(Ser);
        end;
    end;
    RestrictToClient(False);
    CheckLabelFreqs;
    ClearState(stUpdating);
    FInSerInd := -1;
    FInItemInd := -1;
    FRulerVisible := False;
    CancelBeacons;
    if Assigned(FOnDataChange) then
      FOnDataChange(Self);
    Invalidate;
  except
    GoBackError;
    raise;
  end;
end;

procedure TChartWriter.WMMouseLeave(var Message: TWMMouse);
begin
  FHintText := '';
  Repaint;
end;

procedure TChartWriter.ClearHistory;
begin
  FHistory.Clear;
  FViewIndex := -1;
  DoSaveToHistory;
end;

procedure TChartWriter.ClearObjects;
var
  i: integer;
begin
  if Chart = nil then
    Exit;
  for I := 0 to Chart.Legends.Count-1 do
    begin
       if Chart.Legends.Items[i].Legend <> nil then
        Chart.Legends.Items[i].Legend.Free;
    end;

  if Chart.ValueSectionDefs <> nil then
    Chart.ValueSectionDefs.Free;
  if Chart.NameSectionDefs <> nil then
    Chart.NameSectionDefs.Free;
  Chart.Free;
end;

procedure TChartWriter.RedrawGraphLines;
var
  i: integer;

  procedure DoDrawSectionLine(ASection: TSection; Ax: TAxisObject);
  var
    SR, Gr: TRect;
    Handled: Boolean;
    AxType: TAxisType;
    Pn: TPen;
  begin
    Gr := GraphRect;
    SR := ASection.SectionLabelRect;
    Handled := False;
    if Ax = FNamAx then
    begin
      Pn := NameSectionDefs.Pen;
    end
    else
      Pn := ValueSectionDefs.Pen;
    Canvas.Pen.Color := Pn.Color;
    Canvas.Pen.Style := Pn.Style;
    Canvas.Pen.Width := Pn.Width;

    if Assigned(FOnDrawSection) then
    begin
      if Ax is TNameAxis then
        AxType := atNameAxis
      else
        AxType := atValueAxis;
      FOnDrawSection(Self, AxType, Point(0, 0), ASection, seLine,
        Canvas, Handled);
      if Handled then
        Exit;
    end;
    if Ax.IsXAxis then
    begin { Vert }
      if (SR.Left <> Gr.Left) then
      begin
        Canvas.MoveTo(SR.Left, Gr.Top);
        Canvas.LineTo(SR.Left, Gr.Bottom);
        Canvas.MoveTo(SR.Right, Gr.Top);
        Canvas.LineTo(SR.Right, Gr.Bottom);
      end;
    end
    else
    begin { Horz }
      Canvas.MoveTo(Gr.Left, SR.Top);
      Canvas.LineTo(Gr.Right, SR.Top);
      Canvas.MoveTo(Gr.Left, SR.Bottom);
      Canvas.LineTo(Gr.Right, SR.Bottom);
    end;
  end;

begin
  { Be sure to redraw the graph lines. These might have been damaged by the polygon }
  Exit;
  if ActiveGraph is TCWPie then
    Exit;
  FNamAx.DrawAxis;
  FValAx.DrawAxis;
  DrawBorders;
  for i := 0 to FNameSections.Count - 1 do
  begin
    if FNameSections[i].FSectionType = stLine then
    begin
      DoDrawSectionLine(FNameSections[i], FNamAx);
    end;
  end;
  for i := 0 to FValueSections.Count - 1 do
  begin
    if FValueSections[i].FSectionType = stLine then
    begin
      DoDrawSectionLine(FValueSections[i], FValAx);
    end;
  end;
  ResetCanvas;
end;

function TChartWriter.PointsFromRuler(Vertical: Boolean; X, Y: integer;
var APos: TPoint; var SeriesDefs: TSeriesDefs): Boolean;
{ Finds all points along the name axis that are within a given space of xy }
var
  i, j, k: integer;
  Ser: TSeries;
  Gr: TRect;
  Pt: TPoint;
  UnInt: integer;
  TempDefs: TSeriesDefs;
  SerInd, ItmInd: integer;
  Found: Boolean;
  Exclusions: array [1 .. 100] of integer;
  ExclusionCount: integer;
  Legal: Boolean;
  ExclIndex: integer;
  G: TCWGraph;
  BWidth: integer;
  Cpr: Boolean;
  B : integer;

  function Hit(SerInd: integer): Boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to High(SeriesDefs) do
      if SeriesDefs[i].SeriesIndex = SerInd then
      begin
        Result := True;
        Break;
      end;
  end;

  procedure GetNearest(var SerInd, ItmInd: integer);
  var
    i: integer;
    P: TPoint;
    XDist, YDist, Dist: single;
  begin
    Dist := MaxInt;
    for i := 0 to High(TempDefs) do
    begin
      try
        P := FSeriesData[TempDefs[i].SeriesIndex].FSeriesItems
          [TempDefs[i].SeriesItemIndex].PointPos;
        XDist := Power(abs(X - P.X), 2);
        YDist := Power(abs(Y - P.Y), 2);
        if XDist + YDist < Dist then
        begin
          Dist := XDist + YDist;
          ItmInd := TempDefs[i].SeriesItemIndex;
          SerInd := TempDefs[i].SeriesIndex;
        end
        else
        begin
          inc(ExclusionCount);
          Exclusions[ExclusionCount] := TempDefs[i].SeriesItemIndex;
        end;
      except
        Sleep(0);
        raise;
      end;
    end;

  end;

begin
  Result := false;
  if Chart = nil then
    Exit;

  G := InView(TCWCurve);
  if G = nil then
  begin
    G := InView(TCWBar);
    if G <> nil then
      with G as TCWBar do
        GetBarSpace(BWidth, Cpr, B)
  end;
  if (FSeriesData.Count = 0) or (G = nil) then
  begin
    Result := False;
    Exit;
  end;
  Gr := GraphRect;
  Pt := Point(X, Y);
  ExclusionCount := 0;

  UnInt := round(NameFloatUnit / 2) + 1;

  if Vertical then
  begin
    inc(Gr.Bottom);
    if MousePrecision = mpHigh then
    begin
      if G is TCWCurve then
      begin
        Gr.Left := Pt.X - TCWCurve(G).PointWidth;
        Gr.Right := Pt.X + TCWCurve(G).PointWidth;
      end
      else
      begin
        Gr.Left := Pt.X - BWidth;
        Gr.Right := Pt.X + BWidth;
      end

    end
    else
    begin
      Gr.Left := Pt.X - UnInt;
      Gr.Right := Pt.X + UnInt
    end
  end
  else
  begin
    inc(Gr.Right);
    if MousePrecision = mpHigh then
    begin
      if G is TCWCurve then
      begin
        Gr.Top := Pt.Y - TCWCurve(G).PointWidth;
        Gr.Bottom := Pt.Y + TCWCurve(G).PointWidth;
      end
      else
      begin
        Gr.Top := Pt.Y - BWidth;
        Gr.Bottom := Pt.Y + BWidth;
      end

    end
    else
    begin
      Gr.Top := Pt.Y - UnInt;
      Gr.Bottom := Pt.Y + UnInt;
    end;
    { Low is excluded }
  end;

  for j := 0 to FSeriesData.Count - 1 do
  { Collect points. There might be more than one per series that qualify }
  begin
    Ser := FSeriesData[j];
    if not Ser.Visible then
      Continue;
    Found := False;
    for i := 0 to Ser.ItemPoints.Count - 1 do
    begin
      Pt := Ser.ItemPoints[i];
      if Ser.FSeriesItems[i + Ser.FIndent].FLeapDummy then
        Continue;
      if (i + Ser.FIndent > Ser.LastItem) or (i + Ser.FIndent < Ser.FirstItem)
      then
        Continue;
      if PtInRect(Gr, Pt) and not Hit(j) { already found? } then
      begin
        setlength(TempDefs, Length(TempDefs) + 1);
        TempDefs[High(TempDefs)].SeriesIndex := j;
        TempDefs[High(TempDefs)].SeriesItemIndex := i + FSeriesData[j].FIndent;
        Found := True;
      end;
    end;

    if not Found then
      Continue;
    { Pick the nearest one from the series }
    GetNearest(SerInd, ItmInd);
    setlength(SeriesDefs, Length(SeriesDefs) + 1);
    SeriesDefs[High(SeriesDefs)].SeriesIndex := SerInd;
    if Length(SeriesDefs) > 1 then
      { Use the first one found for all series }
      SeriesDefs[High(SeriesDefs)].SeriesItemIndex :=
        SeriesDefs[0].SeriesItemIndex
    else
      SeriesDefs[High(SeriesDefs)].SeriesItemIndex := ItmInd;
    Finalize(TempDefs);

  end;

  Result := Length(SeriesDefs) > 0;
  if not Result then
    Exit;
  Legal := False;
  ExclIndex := 0;
  { Check legals. In multiple series with uneven lengths, there might be a risk that
    a point does not have an actual position. The Exclusions array saves points that
    have failed by the GeatNearest check. Fall back of one of these if one of the first choice
    points are illegal }
  if Result then
    while not Legal do
    begin
      Legal := True;
      for i := 0 to High(SeriesDefs) do
      begin
        Ser := FSeriesData[SeriesDefs[i].SeriesIndex];
        ItmInd := SeriesDefs[i].SeriesItemIndex;
        if (ItmInd > Ser.LastItem) or (ItmInd < Ser.FirstItem) then
        { Illegal point. Not within range of span }
        begin
          Legal := False;
          Break;
        end;
      end;
      if not Legal then
      begin
        inc(ExclIndex);
        if ExclIndex > ExclusionCount then
        begin
          Result := False;
          Break;
        end;
        for k := 0 to High(SeriesDefs) do
          SeriesDefs[k].SeriesItemIndex := Exclusions[ExclIndex];
      end;
    end;
end;

function TChartWriter.SetHintText(X, Y: integer; SerInd, ItmInd: integer;
AGraph: TCWGraph): Boolean;
var
  R: TRect;
  S: string;
  G: TCWGraph;

  function GetMultiLine(Nm: string): string;
  begin
    Result := Nm;
  end;

  procedure DoSetText;
  var
    SerItm: TSeriesItem;
    MI: TMouseInfo;
    Prec: integer;
  begin
    FHintText := '';
    Prec := ValuePrecision;
    if ViewMode = vmSelecting then
      MI := miName
    else
      MI := MouseInfo;
    try
      if ItmInd > FSeriesData[SerInd].FSeriesItems.Count - 1 then
        ItmInd := FSeriesData[SerInd].FSeriesItems.Count - 1;
      SerItm := FSeriesData[SerInd].FSeriesItems[ItmInd];
    except
      Exit;
    end;
    case MI of
      miName:
        if IsTimeSpan(Chart.NameType) then
          FHintText := GetTimeStr(Self, MouseTimeFormat, SerItm.RealDate)
        else
          FHintText := GetMultiLine(SerItm.FName);
      miValue:
        FHintText := FormatNum(SerItm.Value, Prec, True);
      miBoth:
        begin
          if IsTimeSpan(Chart.NameType) then
            S := GetTimeStr(Self, MouseTimeFormat, SerItm.RealDate)
          else
            S := GetMultiLine(SerItm.FName);
          if (S <> '') and (ViewMode <> vmSelecting) then
            S := S + ': ' + FormatNum(SerItm.Value, Prec, True);
          FHintText := S;
        end;
      miNone:
        ;
    end;
    FHintSeries := SerInd;
    FHintItem := ItmInd;
    G := FSeriesData[SerInd].Graph;
    if (G <> nil) then //and not(G is TCWPie) then
    begin
      FHintColor := G.ActiveColor[SerInd, ItmInd];
    end;
    //else
    //  FHintColor := Chart.ItemColors.Items[ItmInd].Color;
  end;

begin
  Result := False;
  FHintText := '';
  R := GraphRect;

  if AGraph <> nil then
    G := AGraph
  else
    G := nil;
  if (G is TCWBar) then
  begin
    if SerInd = -1 then
    begin
      if DoPosInBar(X, Y, SerInd, ItmInd) then
      begin
        DoSetText;
        Result := True;
      end;
    end
    else
    begin
      DoSetText;
      Result := True;
    end;
    Exit;
  end
  else if ActiveGraph is TCWPie then
  begin
    if DoPosInPieSlice(X, Y, SerInd, ItmInd) then
    begin
      DoSetText;
      Result := True;
    end;
  end
  else
  begin
    if (MousePrecision = mpHigh) and not(ViewMode = vmSelecting) then
    begin
      if SerInd = -1 then
      begin
        if DoPosInPoint(X, Y, MousePrecision, SerInd, ItmInd) then
        begin
          DoSetText;
          Result := True;
        end;
      end
      else
      begin
        DoSetText;
        Result := True;
      end;
    end
    else
    begin
      if SerInd = -1 then
      begin
        if DoPosInPoint(X, Y, MousePrecision, SerInd, ItmInd) then
        begin
          DoSetText;
          Result := True;
        end;
      end
      else
      begin
        DoSetText;
        Result := True;
      end;
    end;
  end;
end;

function TChartWriter.SetHintText(X, Y: integer; AGraph: TCWGraph): Boolean;
begin
  Result := SetHintText(X, Y, -1, -1, AGraph);
end;

procedure TChartWriter.MouseMove(Shift: TShiftState; X, Y: integer);
var
  R: TRect;
  SerInd, ItmInd: integer;
  Indx: integer;
  G, Hit: TCWGraph;

  function DoEnterLeaveEvent(AGraphType: TCWGraph;
  AEnterEvent, ALeaveEvent: TMouseItemEvent): Boolean;
  begin
    Result := False;
    if AGraphType is TCWCurve then
      Result := MouseInPoint(SerInd, ItmInd)
    else if AGraphType is TCWBar then
      Result := MouseInBar(SerInd, ItmInd)
    else if AGraphType is TCWPie then
      Result := MouseInPieSlice(SerInd, ItmInd)
    else
      Exit;

    if Result then
    begin
      if (SerInd <> FInSerInd) or (ItmInd <> FInItemInd) then
      begin
        if FInSerInd <> -1 then
        begin
          if Assigned(ALeaveEvent) then
            ALeaveEvent(AGraphType, FInSerInd, FInItemInd);
          FInSerInd := SerInd;
          FInItemInd := ItmInd;
          if Assigned(AEnterEvent) then
            AEnterEvent(AGraphType, FInSerInd, FInItemInd);
        end
        else
        begin
          FInSerInd := SerInd;
          FInItemInd := ItmInd;
          if Assigned(AEnterEvent) then
            AEnterEvent(AGraphType, FInSerInd, FInItemInd);
        end;
      end;
    end
    else
    begin
      if FInSerInd <> -1 then
      begin
        if Assigned(ALeaveEvent) then
          ALeaveEvent(AGraphType, FInSerInd, FInItemInd);
      end;
      FInSerInd := -1;
      FInItemInd := -1;
    end;
  end;

begin
  if InState(stAnimationPause) then
    Exit;
  FHintText := '';
  if (FSeriesData.Count = 0) or InState(stUpdating) or InState(stInternalAction) then
    Exit;
  R := GraphRect;
  Hit := nil;
  if ViewMode in [vmHinting, vmNormal] then
  begin
    Indx := -1;
    G := InView(TCWBar, Indx);
    while G <> nil do
    begin
      if DoEnterLeaveEvent(G, TCWBar(G).FOnMouseEnterBar,
        TCWBar(G).FOnMouseLeaveBar) then
        Hit := G;
      G := InView(TCWBar, Indx);
    end;

    Indx := -1;
    G := InView(TCWCurve, Indx);
    while G <> nil do
    begin
      if DoEnterLeaveEvent(G, TCWCurve(G).FOnMouseEnterPoint,
        TCWCurve(G).FOnMouseLeavePoint) then
        Hit := G;
      G := InView(TCWCurve, Indx);
    end;

    if (ActiveGraph is TCWPie) then
    begin
      if DoEnterLeaveEvent(ActiveGraph, TCWPie(ActiveGraph)
        .FOnMouseEnterPieSlice, TCWPie(ActiveGraph).FOnMouseLeavePieSlice) then
        Hit := ActiveGraph;
    end;
  end;
  if (ViewMode = vmHinting) then
  begin
    SetHintText(X, Y, Hit);
    Repaint;
    inherited;
    Exit;
  end
  { Selecting }

  else if ViewMode = vmSelecting then
  begin
    if FNamAx.IsXAxis then
    begin
      if (X < R.Left) then
        X := R.Left
      else if (X > R.Right) then
        X := R.Right;
    end
    else
    begin
      if (Y < R.Top) then
        Y := R.Top
      else if (Y > R.Bottom) then
        Y := R.Bottom;
    end;

    FMouseDistanceX := FMouseDistanceX + (X - FLastMouseX);
    FMouseDistanceY := FMouseDistanceY + (Y - FLastMouseY);

    FLastMouseX := X;
    FLastMouseY := Y;
    if (FNamAx.IsXAxis) then
      SerInd := SeriesIndexOfX(Point(X, Y), SerInd)
    else
      SerInd := SeriesIndexOfY(Point(X, Y), SerInd);
    if SerInd <> -1 then
    begin
      SetHintText(0, 0, 0, SerInd, nil);
      RepaintSelRect;
    end;
  end;
  inherited;
end;

procedure TChartWriter.MouseDown(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
var
  Indx: integer;
  R: TRect;
  SerInd, ItmInd: integer;
  Pt: TPoint;
  G: TCWGraph;

  function IsSelected: Boolean;
  begin
    Result := (FDynaSectStart <> -1) and (FDynaSectEnd <> -1);
  end;

begin
  if InState(stAnimationPause) then
  begin
    ClearState(stAnimationPause);
    SetState(stResumeAnimation);
    Exit;
  end;
  SetFocus;
  if (FSeriesData.Count = 0) or InState(stUpdating) then
  begin
    inherited;
    Exit;
  end;

  if (ViewMode = vmSelected) then
  begin
    if not FKeepSelection then
      ClearSelection;
    Inherited;
    Exit;
  end;

  if (Shift <> [ssCtrl, ssLeft]) and MouseInfoControl then
  begin
    ClearSelection;
    Inherited;
    Exit;
  end;

  if DoPosInPoint(X, Y, MousePrecision, SerInd, ItmInd) then
  begin
    Pt := FSeriesData[SerInd].FSeriesItems[ItmInd].PointPos;
    Pt.Y := Y;
  end
  else
    Pt := Point(X, Y);

  G := InView(TCWCurve);
  if (G is TCWCurve) and (Rulers <> ruNone) and
    (((Shift = [ssCtrl, ssLeft]) and MouseInfoControl) or
    ((Shift = [ssLeft]) and not MouseInfoControl)) then
  begin
    FRulerVisible := True;
    FRulerPosition := Pt;
    FRulerX := Pt.X;
    FRulerY := Pt.Y;
    FViewMode := vmNormal;
    if Assigned(FOnRuler) then
      FOnRuler(Self);
  end;

  FViewMode := vmNormal;
  if (((Shift = [ssCtrl, ssLeft]) and MouseInfoControl) or
    ((Shift = [ssLeft]) and not MouseInfoControl)) and MouseSelect and
    not(ActiveGraph is TCWPie) then
  begin
    R := GraphRect;
    if not Windows.PtInRect(R, System.Classes.Point(X, Y)) then
    begin
      ClearSelection;
      Inherited;
      Exit;
    end;

    SerInd := -1;
    Indx := SeriesIndexOfX(Pt, SerInd);
    if Indx = -1 then
      Exit;
    FHintText := Names[Indx];
    X := Pt.X;
    Y := Pt.Y;
    Pt := ClientToScreen(Pt);
    SetCursorPos(Pt.X, Pt.Y);
    FLastMouseX := X;
    FLastMouseY := Y;
    FSelStartX := X;
    FSelstartY := Y;
    FMouseDistanceX := 0;
    FMouseDistanceY := 0;
    FSelectingRect.Width := 0;
    FSelectingRect.Height := 0;
    FViewMode := vmSelecting;
    SaveSelBM;
    inherited;
    Exit;
  end;

  if not IsSelected then
  begin
    FDynaSectStart := -1;
    FDynaSectEnd := -1;
  end;

  Indx := GraphSectionFromPoint(X, Y);
  if Indx = -1 then
  begin
    ClearSelection;
  end;
  inherited;
end;

function TChartWriter.IsLeapDate(ASeries: TSeries; ItmIndex: integer): Boolean;
var
  Dt1, Dt2: TDateTime;
  Y, M, D: word;
  k: integer;
begin
  Result := False;
  if Chart.NameType <> ntDateSpan then
    Exit;

  Dt1 := StrToDate(ASeries.SeriesItems[ItmIndex].FName, Fmt);
  DecodeDate(Dt1, Y, M, D);
  if not IsLeapYear(Y) then
  begin
    for k := 0 to FSeriesData.Count - 1 do
    begin
      if FSeriesData[k] = ASeries then
        Continue;
      if ItmIndex <= FSeriesData[k].LastItem then
      begin
        Dt2 := StrToDate(FSeriesData[k].SeriesItems[ItmIndex].FName, Fmt);
        DecodeDate(Dt2, Y, M, D);
        if IsLeapYear(Y) then
        begin
          IsLeapDate := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TChartWriter.CancelBeacons;
var
  i: integer;
begin
  for I := 0 to Chart.FSeriesDefs.Count-1 do
  begin
    if Chart.FSeriesDefs[i].Graph is TCWCurve then
     TCWCurve(Chart.FSeriesDefs[i].Graph).FBeaconIndex := -1;
  end;
end;

procedure TChartWriter.ConcludeSelecting(XPos, YPos: integer);
var
  tmp: integer;
  ASeries, ItmInd: integer;
  Pt: TPoint;

  procedure DoCancel;
  begin
    FDynaSectEnd := -1;
    FDynaSectStart := -1;
    FViewMode := vmNormal;
    FHintText := '';
    CancelBeacons;
    Repaint;
  end;

begin
  { XPos, YPos is cursor pos }
  ASeries := -1;
  if XPos > FSelectingRect.Left then
    XPos := FSelectingRect.Right
  else
    XPos := FSelectingRect.Left;
  if abs(FSelectingRect.Left - FSelectingRect.Right) < round(NameFloatUnit) div 2
  then
  begin
    DoCancel;
    Exit;
  end;

  if DoPosInPoint(XPos, YPos, MousePrecision, ASeries, ItmInd) then
  begin
    Pt := FSeriesData[ASeries].FSeriesItems[ItmInd].PointPos;
  end
  else
    Pt := Point(XPos, YPos);
  if FNamAx.IsXAxis then
  begin
    FDynaSectStart := SeriesIndexOfX(Point(FSelStartX, FSelstartY), ASeries);
    FDynaSectEnd := SeriesIndexOfX(Pt, ASeries);
  end
  else
  begin
    FDynaSectStart := SeriesIndexOfY(Point(FSelStartX, FSelstartY), ASeries);
    FDynaSectEnd := SeriesIndexOfY(Pt, ASeries);
  end;
  if ((FDynaSectStart = -1) or (FDynaSectEnd = -1)) or
    (FDynaSectStart = FDynaSectEnd) then
  begin
    if FDynaSectStart <> FDynaSectEnd then
      ShowGWMessage(msg_UnclearSelection)
    else
      ShowGWMessage(msg_NoZoom);
    DoCancel;
    Exit;
  end;

  if FDynaSectStart > FDynaSectEnd then
  begin
    tmp := FDynaSectStart;
    FDynaSectStart := FDynaSectEnd;
    FDynaSectEnd := tmp;
  end;
  if FSelectionExec = seZoom then
  begin
    Zoom;
  end
  else if (FSelectionExec = seEvent) and Assigned(FOnSelected) then
  begin
    FViewMode := vmSelected;
    FOnSelected(Self);
  end
  else
    FViewMode := vmSelected;
  Invalidate;
end;

procedure TChartWriter.MouseUp(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
begin
  ClearState(stMouseSizing);
  if (FSeriesData.Count = 0) or InState(stUpdating) or InState(stInternalAction) then
    Exit;
  FHintText := '';
  if (ViewMode = vmSelecting) then { Mouse slide select }
  begin
    ConcludeSelecting(X, Y);
  end
  else if ssCtrl in Shift then
    Invalidate;
  inherited;
end;

procedure TChartWriter.KeyD(Key: word; Shift: TShiftState);
var
  MP: TPoint;
begin
  if InState(stAnimationPause) then
  begin
    if (Key = VK_Space) then
    begin
      ClearState(stAnimationPause);
      SetState(stResumeAnimation);
    end
    else if Key = vk_Escape then
    begin
      ClearState(stAnimationPause);
      ClearState(stAnimating);
    end;

    Exit;
  end;
  if (FSeriesData.Count = 0) or InState(stUpdating) or InState(stInternalAction) then
    Exit;
  if (Key = vkEscape) and (ViewMode = vmSelected) then
    ClearSelection;
  if (ViewMode = vmSelecting) or (ViewMode = vmHinting) or
    (ViewMode = vmSelected) then
  begin
    inherited;
    Exit;
  end;
  if (Shift = [ssCtrl]) and MouseInfoControl then
  begin
    MP := ScreenToClient(MOuse.CursorPos);
    FViewMode := vmHinting;
    SetHintText(MP.X, MP.Y, nil);
    Repaint;
  end;
  Inherited;
end;

procedure TChartWriter.KeyU(Key: word; Shift: TShiftState);
var
  MP: TPoint;
begin
  if (FSeriesData.Count = 0) or InState(stUpdating) or InState(stInternalAction) then
    Exit;
  if not MouseInfoControl then
    Exit;
  FHintText := '';
  if ViewMode = vmHinting then
  begin
    FViewMode := vmNormal;
  end
  else if ViewMode = vmSelecting then
  begin
    MP := ScreenToClient(MOuse.CursorPos);
    ConcludeSelecting(MP.X, MP.Y);
  end;
  FRulerX := -1;
  FRulerY := -1;
  CancelBeacons;
  Repaint;
  inherited;
end;

procedure TChartWriter.WMERASEBKGND(var Msg: TWMEraseBkgnd);
begin
  if ViewMode = vmSelected then
    Msg.Result := 1;
end;

procedure TChartWriter.Resize;
begin
  if not InState(stMouseSizing) and not (csLoading in ComponentState)
  and (Count > 0) then
  begin
    RestrictToClient(False);
    if not InState(stLimbo) then
      CheckLabelFreqs;
    DoRepaint;
  end;
  inherited;
end;

procedure TChartWriter.Loaded;
begin
  inherited;
end;

procedure TChartWriter.Notification(AComponent: TComponent;
Operation: TOperation);
var
  Indx: integer;
  i : integer;
begin
  if csDestroying in ComponentState then
  begin
    inherited;
    Exit;
  end;
  if Operation <> opRemove then
  begin
    inherited;
    Exit;
  end;
  if AComponent is TCWChart then
  begin
    Indx := ChartList.IndexOf(AComponent as TCWChart);
    if Indx <> -1 then
      ChartList.Delete(Indx);
    if FChart = AComponent then
    begin
     FChart := nil;
    end;
  end
  else if (AComponent is TCWLegend) and (Chart <> nil) then
  begin
     for i := 0 to Chart.Legends.Count-1 do
       if Chart.Legends.Items[i].Legend = AComponent then
       begin
         Chart.Legends.Items[i].FLegend := nil;
         Break;
       end;
  end;
end;

procedure TChartWriter.DoZoom(StartIndex, EndIndex: integer);
var
  i, j: integer;
  Ser: TSeries;
  Series: TList<TSeries>;
  NewSer: TSeries;
  SerItm: TSeriesItem;
  TheSer: TSeries;
  S: string;
  StInd, NdInd: integer;
  PIndex: integer;
begin
  if (ActiveGraph is TCWPie) then
  begin
    Exit;
  end;
  StInd := 0;
  NdInd := 0;
  FViewMode := vmNormal;
  FHintText := '';
  Series := TList<TSeries>.Create;
  for i := 0 to FSeriesData.Count - 1 do
  begin
    if not FSeriesData[i].Visible then
      Continue;
    Ser := FSeriesData[i];
    NewSer := TSeries.Create;
    NewSer.FLeapdateCount := Ser.FLeapdateCount;
    NewSer.FWriter := Self;
    NewSer.FIndent := 0;
    NewSer.FExdent := 0;

    TheSer := FOrigData[i];
    if Ser.FSeriesItems[StartIndex].FReminderName <> '' then
      S := Ser.FSeriesItems[StartIndex].FReminderName
    else
      S := Ser.FSeriesItems[StartIndex].FName;
    StInd := TheSer.IndexOfName(S);
    if Ser.FSeriesItems[EndIndex].FReminderName <> '' then
      S := Ser.FSeriesItems[EndIndex].FReminderName
    else
      S := Ser.FSeriesItems[EndIndex].FName;
    NdInd := TheSer.IndexOfName(S);

    for j := StInd to NdInd do
    begin
      if (j > TheSer.LastItem) or (j < TheSer.FirstItem) then
      begin
        Sleep(0);
        Continue;
      end;
      SerItm := NewSer.AddItem;
      SerItm.Assign(TheSer.FSeriesItems[j]);
      SerItm.FOwner := NewSer;
      SerItm.FItems := NewSer.FSeriesItems;
    end;
    if NewSer.Count > 0 then
      Series.Add(NewSer)
    else
    begin
      NewSer.Free;
      Continue;
    end;
  end;
  PIndex := FScrollIndex;
  InternalClear;
  SetState(stInternalAction);
  try
    for i := 0 to Series.Count - 1 do
    begin
      AddSeries(Series[i], True);
    end;
    SetState(stZoomed);
    FZoomStart := StInd;
    FZoomEnd := NdInd;
    FContractionBase.Clear;
    Execute;
  finally
    ClearState(stInternalAction);
    FScrollIndex := PIndex;
  end;
  Series.Free;
  FRulerX := -1;
  FRulerY := -1;
  PostHist;
  if ActiveGraph is TCWBar then
  begin
    DoRepaint;
  end
  else
    Repaint;
  if Assigned(FOnZoom) then
    FOnZoom(Self);
end;

procedure TChartWriter.Zoom;
begin
  if (FDynaSectStart <> -1) and (FDynaSectEnd <> -1) then
    DoZoom(FDynaSectStart, FDynaSectEnd);
end;

procedure TChartWriter.Zoom(StartIndex, EndIndex: integer);
begin
  if (StartIndex < 0) or (StartIndex > FNameList.Count - 1) then
    ShowGWError(msg_IndexBounds, 'Start ');
  if (EndIndex < 0) or (EndIndex > FNameList.Count - 1) then
    ShowGWError(msg_IndexBounds, 'End ');
  if abs(StartIndex - EndIndex) = 0 then
    Exit;
  if StartIndex > EndIndex then
    ShowGWError(msg_IndexBounds, 'Start ');
  DoZoom(StartIndex, EndIndex);
end;

procedure TChartWriter.Zoom(NameSectionIndex: integer);
var
  St, Nd: integer;
begin
  if (NameSectionIndex > NameSectionCount - 1) or (NameSectionIndex < 0) then
    ShowGWError(msg_IndexBounds, 'Section ');
  St := IndexOfName(NameSections[NameSectionIndex].StartVal);
  Nd := IndexOfName(NameSections[NameSectionIndex].EndVal);
  Zoom(St, Nd);

end;

function TChartWriter.ComputeTextPos(X, Y: integer; ALabel: string;
AnAxis: TAxisObject): TPoint;
var
  R: TRect;
  AFont: TFont;
  LabelKind: TLabelKind;
  n: integer;
  QualFits : Boolean;
  QualF : string;
begin
  R := GraphRect;
  if AnAxis is TNameAxis then
  begin
    LabelKind := lkName;
    AFont := FNameFont;
  end
  else
  begin
    if AnAxis is TValueAxis2 then
      LabelKind := lkValue2
    else
      LabelKind := lkValue;
    AFont := FValueFont
  end;

  if AnAxis.IsXAxis then
  begin
    if AnAxis.Position = apBottom then
    begin
      if AFont.Orientation = 0 then
      begin
        Y := AnAxis.FLabelTextRect.Top - FInternalLeading;
      end
      else if AFont.Orientation = 900 then
      begin
        Y := AnAxis.FLabelTextRect.Bottom - FInternalLeading;
      end
      else if AFont.Orientation = 450 then
      begin
        n := GetTextDiag(Canvas, ALabel);
        n := round(sqrt(n * n / 2));
        Y := AnAxis.FLabelTextRect.Top + n - c_HookSpace - 2;
        { 2 to ensure it fits the space. Rounding is inexact }
        X := X - GetTextWidth(LabelKind);
      end;
    end
    else if AnAxis.Position = apTop then
    begin
      if AFont.Orientation = 0 then
      begin
        Y := AnAxis.FLabelTextRect.Bottom - GetTextHeight(LabelKind) +
          FInternalLeading;
      end
      else if AFont.Orientation = 450 then
      begin
        n := GetTextDiag(Canvas, ALabel);
        n := round(sqrt(n * n / 2));
        Y := AnAxis.FLabelTextRect.Top + n - c_HookSpace + 3;
        { 3 to ensure it fits the space. Rounding is inexact }
      end
      else
      begin
        Y := AnAxis.FLabelTextRect.Bottom + FInternalLeading;
      end;
    end;

    if (AFont.Orientation = 0) then
      Result := Point(X - (GetTextWidth(LabelKind, ALabel) div 2), Y)
    else
      Result := Point(X, Y);

    { If falls outside client area, try to press it inside }
    if AnAxis is TNameAxis then
    begin
      if (X <= ClientRect.Left) and (FNameUnit > Canvas.TextWidth(ALabel) + 3)
      then
        X := ClientRect.Left
      else if (X >= ClientRect.Right - Canvas.TextWidth(ALabel)) and
        (FNameUnit > Canvas.TextWidth(ALabel) + 3) then
        X := ClientRect.Right - Canvas.TextWidth(ALabel) - 3;
      Result := Point(X, Y);
    end;
  end
  else {YAxis}
  begin
    if AnAxis.Position = apRight then
    begin
      QualFits := True;
      if AnAxis = FValAx then
      begin
       if Chart.ValueAxis1.FQualifier <> '' then
       begin
        QualFits := Chart.ValueAxis1.FQualifierFits;
        QualF := Chart.ValueAxis1.FQualifier;
       end;
      end
      else
      begin
       if Chart.ValueAxis2.FQualifier <> '' then
       begin
        QualFits := Chart.ValueAxis2.FQualifierFits;
        QualF := Chart.ValueAxis2.FQualifier;
       end;
      end;
      if QualFits then
      begin
        if AnAxis.Position = apRight then
         X := AnAxis.FLabelTextRect.Left
        else
         X := AnAxis.FLabelTextRect.Right - GetTextWidth(LabelKind, ALabel) - c_HookSize
      end
      else
      begin
        if AnAxis.Position = apRight then
          X := AnAxis.FLabelTextRect.Left
        else
         X := AnAxis.FLabelTextRect.Right - GetTextHeight(LabelKind)
         - GetTextWidth(LabelKind, ALabel) - c_QualifierMargin - c_HookSize
      end;
    end
    else
    begin
      if AnAxis.Position = apRight then
       X := AnAxis.FLabelTextRect.Left
      else
        X := AnAxis.FLabelTextRect.Right - GetTextWidth(LabelKind, ALabel);
    end;
    Result := Point(X, (Y - GetTextHeight(LabelKind) div 2));

    if AnAxis is TNameAxis then
    begin
      if (Y <= ClientRect.Top) and (FNameUnit > Canvas.TextHeight(ALabel) + 3)
      then
        Y := ClientRect.Top
      else if (Y >= ClientRect.Bottom - Canvas.TextHeight(ALabel)) and
        (FNameUnit > Canvas.TextHeight(ALabel) + 3) then
        Y := ClientRect.Bottom - Canvas.TextHeight(ALabel) - 3;
      Result := Point(X, Y);
    end;
  end;
end;

procedure TChartWriter.ComputePoints(ASeries: TSeries = nil;
Recalc: Boolean = False);
var
  i, j: integer;
  Adds: integer;
  LastPt: TPoint;
  LastRealPt: TPoint;
  Space: integer;
  SpaceSet: Boolean;
  DoDiff: Boolean;
  Cpr: Boolean;
  Un : single;
  B : integer;

  procedure AdjustBarPoint;
  var
    ThisPt: TPoint;
    Diff: integer;
  begin
    if Un < 1 then
      Exit;
    ThisPt := FSeriesData[i].FBarPoints[FSeriesData[i].FBarPoints.Count - 1];
    if FNamAx.IsXAxis then
    begin
      Diff := ThisPt.X - LastPt.X
    end
    else
      Diff := ThisPt.Y - LastPt.Y;
    DoDiff := True;
    if (Diff <> Space) and DoDiff then
    begin
      Diff := Space - Diff;
      if FNamAx.IsXAxis then
        ThisPt.X := ThisPt.X + Diff
      else
        ThisPt.Y := ThisPt.Y + Diff;
      FSeriesData[i].FBarPoints[FSeriesData[i].FBarPoints.Count - 1] := ThisPt;
      Adds := Diff;
    end;
  end;

  function CheckMinSpace: Boolean;
  var
    ThisPt: TPoint;
    MinSpace: integer;
  begin
    Result := True;
    if Un < 1 then
      Exit;
    ThisPt := FSeriesData[i].FBarPoints[FSeriesData[i].FBarPoints.Count - 1];
    if FNamAx.IsXAxis then
      MinSpace := ThisPt.X - LastRealPt.X
    else
      MinSpace := ThisPt.Y - LastRealPt.Y;
    if MinSpace < Space then
    begin
      Space := MinSpace;
      Result := False;
    end;
  end;

{ Recalc is not used. Should be removed? }

begin
  if ActiveGraph is TCWPie then
    Exit;
  if InView(TCWBar) = nil then
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if Chart.FSeriesDefs.Items[i].ValueAxis = vaValueAxis1 then
        FActiveValAx := FValax
      else
        FActiveValAx := FValax2;
      FSeriesData[i].FPoints.Clear;
      for j := FSeriesData[i].FirstItem to FSeriesData[i].LastItem do
      begin
        FSeriesData[i].FPoints.Add(FSeriesData[i].PosFromNamVal(FSeriesData[i].FSeriesItems[j]
          .FName, FSeriesData[i].FSeriesItems[j].Value));
      end;
    end
  else
  begin
    FBarPoints := True;
    Un := NameFloatUnit;
    Adds := 0;

    TCWBar(ActiveGraph).GetBarSpace(Space, Cpr, B);
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if Chart.FSeriesDefs.Items[i].ValueAxis in [vaValueAxis1, vaNone] then
        FActiveValAx := FValax
      else
        FActiveValAx := FValax2;
      repeat
        SpaceSet := True;
        FSeriesData[i].FBarPoints.Clear;
        for j := FSeriesData[i].FirstItem to FSeriesData[i].LastItem do
        begin
          FSeriesData[i].FBarPoints.Add
            (FSeriesData[i].PosFromNamVal(FSeriesData[i].FSeriesItems[j].FName,
            FSeriesData[i].FSeriesItems[j].Value));
          if j > FSeriesData[i].FirstItem then
          begin
            if not CheckMinSpace then
            begin
              Adds := 0;
              SpaceSet := False;
              Break;
            end;
          end;
          LastRealPt := FSeriesData[i].FBarPoints[FSeriesData[i].FBarPoints.Count - 1];
          if j > FSeriesData[i].FirstItem then
          begin
            AdjustBarPoint;
          end;
          LastPt := FSeriesData[i].FBarPoints[FSeriesData[i].FBarPoints.Count - 1];
        end;
      until SpaceSet;
    end;

    FBarPoints := False;
    if (Adds <> 0) and not Recalc then
    begin
      if FNamAx.IsXAxis then
        FRightSpace := FRightSpace - Adds
      else
        FBottomSpace := FBottomSpace - Adds;
      CheckLabelFreqs(False);
      FPosRect := GetPosRect;
      if Centered and not Recalc then {Recalc according to new posrect. Only bars}
        ComputePoints(ASeries, True);

    end;
  end;
end;

procedure TChartWriter.AssignOrigData;
var
  OrigSer: TSeries;
  i: integer;
begin
  if (Chart.NameType in [ntGeneral]) or
    InState(stInternalContraction) or Scrollable or InState(stZoomed) or
    InState(stUserContraction) then
  begin
    Exit;
  end;

  if FSeriesData[0].FSeriesItems.Count = FOrigData[0].FSeriesItems.Count then
    Exit;

  FSeriesData.Clear;
  for i := 0 to FOrigData.Count - 1 do
  begin
    OrigSer := TSeries.Create;
    OrigSer.Assign(FOrigData[i], -1, -1);
    FSeriesData.Add(OrigSer);
  end;
  FLongestSeries := LongestSeries;
  FNameList.Clear;
  for i := 0 to FLongestSeries.FSeriesItems.Count - 1 do
  begin
    FNameList.Add(FLongestSeries.FSeriesItems[i].FName)
  end;

end;

procedure TChartWriter.RestrictToClient(Restore: Boolean);
{ Ensures that all points fit within graph area. If not, Contractes or scrolls the
  series }
var
  Space: integer;
  Un: single;
  Cnt: integer;
  AMaxSpace: integer;
  Ax: TAxisObject;
  Rate: integer;
  FirstTime: Boolean;
  MinSpace: integer;
  MaxPoints: integer;
  G: TCWAxisGraph;
  HasContracted: Boolean;
  Compr : Boolean;

  function GetContraction(Axis: TAxisType): integer;
  var
    ThisCount: single;
    Spacing: integer;
    NewCount: single;
    MaxSpace: integer;
  begin
    Result := 1;
    MaxSpace := AMaxSpace;
    if Axis = atNameAxis then
    begin
      ThisCount := FNameList.Count;
      Spacing := G.MinPointSpacing;
      NewCount := MaxSpace / Spacing;
      if NewCount = 0 then
      begin
        GoBackError;
        ShowGWError(msg_NoSpace);
      end;
      Result := round(ThisCount / NewCount);
    end;
  end;

  procedure SetSpaces(IsValue: Boolean);
  begin
    if IsValue then
    begin
       Ax := FValAx;
    end
    else
      Ax := FNamAx;
    if Ax.IsXAxis then
    begin
      AMaxSpace := GraphRect.Width;
      Space := round((FNameList.Count * NameFloatUnit));
    end
    else
    begin
      AMaxSpace := GraphRect.Height;
      Space := round(ValueCount * ValueFloatUnit);
    end;
  end;

  function CheckValueSpace: Boolean;
  var
    i: integer;
    VS : TCWValueAxis;
  begin
    Result := True;

    for I := 0 to Chart.AxisCount-1 do
    begin
      if I = 0 then
      begin
       FActiveValAx := FValAx;
       VS := Chart.FValueAxis1;
      end
      else
      begin
       FActiveValAx := FValAx2;
       VS := Chart.FValueAxis2;
      end;
      SetSpaces(True);
      Cnt := ValueCount;
      if Cnt = 0 then
      begin
        Result := False;
        GoBackError;
        ShowGWError(msg_NoSpace);
      end;
      VS.FValueUnit := Trunc(AMaxSpace / Cnt);
      if VS.FValueUnit <= 1 then
      { Increase intervals to make it fit}
      begin
        while VS.FValueUnit <= 1 do
        begin
          begin
            ValueIntervals := ValueIntervals + 1;
            Cnt := ValueCount;
            VS.FValueUnit := round(AMaxSpace / Cnt);
          end;
        end;
      end;
    end;
  end;

  procedure CheckMaxSpacing;
  var
    W: integer;
  begin
    if G is TCWBar then
      Exit;
    if (NameFloatUnit > G.MaxPointSpacing) and (G.MaxPointSpacing <> 0) then
    begin
      if FNamAx.IsXAxis then
      begin
        W := GraphWidth - (FNameList.Count - 1) * G.MaxPointSpacing;
        FRightSpace := FRightSpace + W;
      end
      else
      begin
        W := GraphHeight - (FNameList.Count - 1) * G.MaxPointSpacing;
        FBottomSpace := FBottomSpace + W;
      end;
      SetSpaces(False);
      FNameUnit := round(AMaxSpace / FNameList.Count);
    end;
  end;

  function IsInLimbo: Boolean;
  var
    Cpr: Boolean;
    B, B2: integer;
  begin
    Result := False;
    if csDesigning in ComponentState then
      Exit;
    if G = nil then
      Exit;
    if G is TCWBar then
    begin
      TCWBar(G).GetBarSpace(B, Cpr, B2);
      B := B * 3;
    end
    else
    begin
      B := G.MinPointSpacing;
    end;
    if B < 75 then
      B := 75;
    if FNamAx.IsXAxis then
      Result := (Width - SpaceOf(apLeft) - InnerMargins.Left -
      SpaceOf(apRight)) < B;
    if not Result then
      Result := (Height - SpaceOf(apTop) - InnerMargins.Top -
        SpaceOf(apBottom)) < 75;

  end;

  procedure DoQuery;
  var
    i : integer;
    G : TCWAxisGraph;
    QR : integer;
    Response : TSpaceResponse;
  begin
    if not (ActiveGraph is TCWAxisGraph) or Scrollable then
      Exit;
    if (ActiveGraph is TCWCurve) and (TCWCurve(ActiveGraph).MinPointSpacing = 0) then
      Exit;
    if (Chart.NameType = ntGeneral) or
    (Chart.OverflowAction = ovNone) then
    for I := 0 to Chart.SeriesDefs.Count-1 do
    begin
      if Chart.SeriesDefs[i].Graph is TCWAxisGraph then
      begin
         Response := srRefuseExcept;
         G := Chart.SeriesDefs[i].Graph as TCWAxisGraph;
         QR := G.QuerySpace;
         if (QR <> 0) then //and not (csDesigning in ComponentState) then
           FOnQuerySpace(G, QR, Response)
         (*else if QR <> 0 then
         begin
           if QR = -1 then
           begin
             Response := srRefuseExcept;
           end
           else
             Response := srAccept;
         end*)
         else
           Exit;
         if (Response = srAccept) and not (QR = -1) then
         begin
           if G is TCWBar then
            TCWBar(G).FBarWidth := QR
           else
            TCWCurve(G).FMinPointSpacing := QR;
         end
         else if Response = srRefuseExcept then
           ShowGWError(msg_NoSpace)
         else
           Abort;
      end;
    end;
  end;


{ Computes the base for the graphrect.
  The unit sizes ar in whole numbers, though float values are used in actutal
  processing. }

begin
  ClearState(stLimbo); { Space to small, draws a static image }

  HasContracted := False;
  G := nil;
  if not(ActiveGraph is TCWPie) then
    G := ActiveGraph as TCWAxisGraph;

  if FSeriesData.Count = 0 then
    Exit;

  if IsInLimbo then
  begin
   SetState(stLimbo);
   Exit;
  end;

  if ActiveGraph is TCWPie then
  begin
    Chart.CreateLegendContent;
  end;

  if (G = nil) or (ActiveGraph is TCWPie) then
    Exit;
  Canvas.Font.Assign(Font);

  if InState(stUpdating) then
    Exit;

  FRightSpace := FOrigGraphSize.cx;
  FBottomSpace := FOrigGraphSize.cy;

  if not InState(stRestoring) then
    AssignOrigData;

  Chart.CheckQualifiers;
  CreateSections;
  Chart.CreateLegendContent;

  if (Assigned(FOnQuerySpace)
  and not (csDesigning in ComponentState)) then // and InState(stExecuting) then
  begin
     DoQuery;
  end;


  SetSpaces(False);
  Un := AMaxSpace / (FNameList.Count-1);
  if Un < 1 then
  begin
    if (G is TCWCurve) and (TCWCurve(G).FMinPointSpacing = 0) then
      Un := 0;
  end;
  Compr := false;
  if G is TCWBar then
   Compr := TCWBar(G).Compressing
  else if G is TCWCurve then
    Compr := (Chart.OverflowAction = ovCompression);

  MinSpace := G.MinPointSpacing;
  if Compr or
    ((Un >= MinSpace) and not((Contraction > 1) and Scrollable)) then
  begin
    FNameUnit := round(Un);
    if Un > 0 then
      CheckMaxSpacing;
    FContractionCount := 0;
  end
  else if Scrollable then
  begin
    ScrollTo(FScrollIndex);
    FNameUnit := round(Un);
  end
  else
  begin { Not enough space}
    if Chart.NameType = ntGeneral then
    begin
      GoBackError;
      ShowGWError(msg_NoSpace); { No Contraction of ntGeneral }
    end;
    if (Chart.OverflowAction = ovNone) then
    begin
      if FNamAx.IsXAxis then
        MaxPoints := GraphRect.Width div G.MinPointSpacing
      else
        MaxPoints := GraphRect.Height div G.MinPointSpacing;
      GoBackError;
      FErrorText := ErrorMsg[msg_NoSpace];
      { Maxpoints sent as param on OnOverflow event}
      PostMessage(Handle, WM_ERROR, MaxPoints, 0);
      Exit;
    end;
    { Internal Contraction always uses avg }
    FirstTime := not InState(stInternalContraction);
    SetState(stInternalContraction);

    Rate := GetContraction(atNameAxis);
    if Rate = Contraction then
      inc(Rate);

    if FirstTime then
      DoContractValues(Rate, ctExplicit)
    else
      DoContractValues(Rate, ctIncremental);
    HasContracted := True;
    inc(FContractionCount);
    if FContractionCount > 100 then
    begin
      raise TCWException.Create('Contract loop error ' + IntTostr(Rate));
    end;
  end;
  CheckValueSpace;
  if not HasContracted and (ActiveGraph is TCWAxisGraph) and (Chart.AllEqual <> nil) then
  begin
    if ActiveGraph is TCWBar then
    begin
      if (TCWBar(ActiveGraph).FAnimations <> [anPause]) and
        (TCWBar(ActiveGraph).FAnimations <> []) then
      begin
        InitAnimation;
      end;
    end
    else if (ActiveGraph is TCWCurve) and TCWCurve(ActiveGraph).Animation then
      InitAnimation;
  end;
  FPosRect := GetPosRect;

end;

procedure DrawDsgnSample(Writer: TChartWriter);
var
  R, R2, DrawRect: TRect;
  BM: TBitmap;
  PSize: integer;
begin
  R := Writer.GraphRect;
  R.Left := R.Left + 4;
  R.Right := R.Right - 4;
  R.Top := R.Top + 4;
  R.Bottom := R.Bottom - 4;
  if Writer.ActiveGraph = nil then
    PSize := 500
  else
    PSize := TCWPie(Writer.ActiveGraph).FPieSize;
  if PSize > Min(R.Width, R.Height) then
    PSize := Min(R.Width, R.Height);
  BM := TBitmap.Create;
  BM.SetSize(PSize + 8, PSize + 8);
  DrawRect.Left := 4;
  DrawRect.Top := 4;
  DrawRect.Bottom := DrawRect.Top + PSize;
  DrawRect.Right := DrawRect.Left + PSize;
  R2 := DrawRect;
  BM.Canvas.Brush.Color := Writer.GraphBGColor;
  BM.Canvas.FillRect(Rect(0, 0, BM.Width, BM.Height));
  BM.Canvas.Ellipse(DrawRect);
  DrawRect.Right := BM.Width;
  DrawRect.Left := BM.Width - 4;
  DrawRect.Top := 0;
  DrawRect.Bottom := BM.Height;
  BM.Canvas.FillRect(DrawRect);
  BM.Canvas.Pen.Color := clBlack;
  BM.Canvas.MoveTo(R2.Left, R2.Top + PSize div 2);
  BM.Canvas.LineTo(R2.Right, R2.Top + PSize div 2);
  BM.Canvas.MoveTo(R2.Left + PSize div 2, R2.Top);
  BM.Canvas.LineTo(R2.Left + PSize div 2, R2.Bottom);
  Writer.Canvas.Draw((R.Left + R.Width div 2) - (PSize div 2) - 4,
    (R.Top + R.Height div 2) - (PSize div 2) - 4, BM);
  BM.Free;
end;

procedure TChartWriter.DrawBorders;
var
  R: TRect;

begin
  if ActiveGraph is TCWPie then
  begin
    if poClientBorder in TCWPie(ActiveGraph).Options then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(GraphRect);
    end;
  end
  else if GraphBorders = gbAllSides then
  begin
    R := GraphRect;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Style := bsClear;
    inc(R.Bottom);
    inc(R.Right);
    Canvas.Rectangle(R);
  end;
  ResetCanvas;
end;

procedure TChartWriter.Paint;
var
  R: TRect;
  W, W2: integer;
  MP: TPoint;
  i: integer;

  procedure RedrawPoints;
  var
    Ser: TSeries;
    ItmInd: integer;
  begin
    if ActiveGraph is TCWPie then
      Exit;
    begin
      Ser := SeriesFromMouse(ItmInd); { 0-based }
      if Ser <> nil then
      begin
        if Ser.Graph is TCWCurve then
          with Ser.Graph as TCWCurve do
          begin
            if BeaconsActive then
            begin
              FBeaconIndex := ItmInd + Ser.FIndent;
              Draw;
            end;
          end;
      end;
    end;
  end;

  function ShowRuler: Boolean;
  var
    Pt: TPoint;
  begin
    Result := False;
    Canvas.Pen.Color := clBlack;
    Pt := ScreenToClient(MOuse.CursorPos);
    if not PtInRect(GraphRect, Pt) then
      Exit;
    Result := True;
    if (FRulers = ruNames) or (FRulers = ruBoth) then
    begin
      if FNamAx.IsXAxis then
      begin
        Canvas.MoveTo(Pt.X, GraphRect.Top);
        Canvas.LineTo(Pt.X, GraphRect.Bottom);
      end
      else
      begin
        Canvas.MoveTo(GraphRect.Left, Pt.Y);
        Canvas.LineTo(GraphRect.Right, Pt.Y);
      end;
    end;
    if (FRulers = ruValues) or (FRulers = ruBoth) then
    begin
      if FNamAx.IsXAxis then
      begin
        Canvas.MoveTo(GraphRect.Left, Pt.Y);
        Canvas.LineTo(GraphRect.Right, Pt.Y);
      end
      else
      begin
        Canvas.MoveTo(Pt.X, GraphRect.Top);
        Canvas.LineTo(Pt.X, GraphRect.Bottom);
      end;
    end;

    FRulerY := Pt.Y;
    FRulerX := Pt.X;
  end;

  procedure DisplayHint(APos: TPoint; SerIndex: integer;
  KeepBackGround: Boolean = False);
  var
    Handled: Boolean;
    Info: TInfo;
    Itm: TSeriesItem;
    G: TCWGraph;

    function GetHintRect: TRect;
    var
      W, H: integer;
      Add: integer;
    begin
      G := InView(TCWCurve);
      if (G <> nil) and (TCWCurve(G).PointMarkers <> pmNone) then
        Add := 6
      else
        Add := 0;
      W := GetTextWidth(lkInfo, FHintText) + 15;
      H := GetTextHeight(lkInfo);
      Result := Rect(APos.X - (W div 2), APos.Y - H - Add,
        APos.X - (W div 2) + W, APos.Y - Add);
      W := Result.Width;
      H := Result.Height;

      if Result.Top < GraphRect.Top then
      begin
        Result.Top := GraphRect.Top;
        Result.Bottom := R.Top + H;
      end
      else if Result.Bottom > GraphRect.Bottom then
      begin
        Result.Bottom := GraphRect.Bottom;
        Result.Top := Result.Bottom - H;
      end;

      if Result.Left < GraphRect.Left then
      begin
        Result.Left := GraphRect.Left;
        Result.Right := Result.Left + W;
      end
      else if Result.Right > GraphRect.Right then
      begin
        Result.Right := GraphRect.Right;
        Result.Left := Result.Right - W;
      end;
    end;

  begin
    if ActiveGraph = nil then
      Exit;
    if not KeepBackGround then
    begin
      Canvas.Draw(ClientRect.Left, ClientRect.Top, FSelBM);
      RedrawPoints;
    end;
    if (FHintText = '') and not Assigned(FOnMouseInfo) then
    begin
      RedrawGraphLines;
      Exit;
    end;
    Canvas.Font.Color := clBlack;
    Canvas.Pen.Color := clSilver;
    if Assigned(FOnMouseInfo) then
    begin
      Info.Items := TList<TSeriesItem>.Create;
      Info.Colors := TList<TColor>.Create;
      Info.Text := TStringList.Create;
      try
        Itm := TSeriesItem.Create;
        Itm.Assign(FSeriesData[FHintSeries].FSeriesItems[FHintItem]);
        Info.Items.Add(Itm);
        Info.Text.Add(FHintText);
        Info.Colors.Add(FHintColor);
        Handled := False;
        R := GetHintRect;
        FOnMouseInfo(Self, Info, Point(R.Left - 2, R.Top - 1), Canvas, Handled);
        if Handled then
        begin
          ResetCanvas;
          RedrawGraphLines;
          Exit;
        end
        else
        begin
          FHintText := Info.Text[0];
        end;
      finally
        Info.Items.Free;
        Info.Colors.Free;
        Info.Text.Free;
      end;
    end;
    R := GetHintRect;
    Dec(R.Left, 2);
    inc(R.Right, 2);
    inc(R.Bottom);
    Dec(R.Top);
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := GraphBGColor;
    Canvas.Rectangle(R);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(R.Left + 15, R.Top + 1, FHintText);
    Canvas.Brush.Color := FHintColor;
    Canvas.Pen.Color := clBlack;

    Canvas.Rectangle(
    Rect(R.Left + 3, R.Top + 3, R.Left + 12, R.Bottom -3));
    ResetCanvas;
    RedrawGraphLines;
  end;

  procedure DisplayRulerHint(HintInfo: TRulerHintInfo; RulX, RulY: integer);
  var
    i: integer;
    W, H: integer;
    R: TRect;
    S: string;
    TotHeight: integer;
    MP: TPoint;
    Handled: Boolean;
    Ser: TSeries;
    Info: TInfo;
    V: single;

    procedure FlipRight;
    begin
      R := Rect(RulX + 4, GraphRect.Top + 3, RulX + W,
        GraphRect.Top + (H * HintInfo.Text.Count) + 2);
    end;

    procedure FlipLeft;
    begin
      R := Rect(RulX - W - 1, GraphRect.Top + 3, RulX - 1,
        GraphRect.Top + (H * HintInfo.Text.Count) + 2)
    end;

    procedure FlipTop;
    begin
      R := Rect(GraphRect.Right - W, RulY - TotHeight, GraphRect.Right, RulY);
    end;

    procedure FlipBottom;
    begin
      R := Rect(GraphRect.Right - W, RulY + 1, GraphRect.Right,
        RulY + TotHeight + 1);
    end;

    function ValueFromPos(APos: TPoint): single;
    var
      HW: integer;
    begin
      if FNamAx.IsXAxis then
      begin
        APos.Y := APos.Y - GraphRect.Top;
        HW := GraphRect.Height;
        APos.Y := GraphRect.Height - APos.Y;
        Result := (ValueHigh - ValueLow) * APos.Y / HW + ValueLow;

      end
      else
      begin
        APos.X := APos.X - GraphRect.Left;
        HW := GraphRect.Width;
        APos.X := GraphRect.Width - APos.X;
        Result := (ValueHigh - ValueLow) * APos.X / HW - ValueHigh;
        Result := -Result;
      end;
    end;

    procedure SetRect;
    var
      i: integer;
    begin
      W := 0;
      H := 0;
      MP := ScreenToClient(MOuse.CursorPos);
      for i := 0 to HintInfo.Text.Count - 1 do
      begin
        if GetTextWidth(lkInfo, HintInfo.Text[i]) > W then
        begin
          W := GetTextWidth(lkInfo, HintInfo.Text[i]);
        end;
        if GetTextHeight(lkInfo) > H then
        begin
          H := GetTextHeight(lkInfo);
        end;
        if FSeriesData[HintInfo.SerIndexes[i]].Graph is TCWCurve then
          with FSeriesData[HintInfo.SerIndexes[i]].Graph as TCWCurve do
          begin
            if BeaconPoints then
            begin
              FBeaconIndex := HintInfo.SerItmIndexes[i];
              Draw;
            end;
          end;
      end;
      W := W + 15;
      if FNamAx.IsXAxis then
      begin
        if MP.X > FLastMousePos.X then
        begin
          if RulX - W >= GraphRect.Left then
            FlipLeft
          else
            FlipRight;
        end
        else
        begin
          if RulX + W <= GraphRect.Right then
          begin
            FlipRight;
          end
          else
            FlipLeft;
        end;
      end
      else
      begin
        TotHeight := H * HintInfo.Text.Count;
        if MP.Y > FLastMousePos.Y then
        begin
          if RulY - TotHeight >= GraphRect.Top then
            FlipTop
          else
            FlipBottom;
        end
        else
        begin
          if RulY + TotHeight <= GraphRect.Bottom then
            FlipBottom
          else
            FlipTop;
        end;
      end;
    end;

  begin
    Canvas.Draw(ClientRect.Left, ClientRect.Top, FSelBM);
    if not ShowRuler then
      HintInfo.SerCount := 0;
    if HintInfo.SerCount = 0 then
    begin
      RedrawGraphLines;
      Exit;
    end;
    if Rulers = ruValues then
    begin
      if (MouseInfo = miName) then
      begin
        RedrawGraphLines;
        Exit;
      end;
      MP := ScreenToClient(MOuse.CursorPos);
      V := ValueFromPos(MP);
      S := FormatNum(V, ValuePrecision);

      if FNamAx.IsXAxis then
      begin
        Canvas.TextOut(GraphRect.Left + 3, MP.Y + 3, S);
      end
      else
      begin
        Canvas.TextOut(MP.X + 3, GraphRect.Top + 3, S);
      end;
      RedrawGraphLines;
      Exit;
    end;

    Handled := False;
    if Assigned(FOnMouseInfo) then
    begin
      Info.Items := TList<TSeriesItem>.Create;
      Info.Colors := TList<TColor>.Create;
      Info.Text := TStringList.Create;
      try
        for i := 0 to HintInfo.Text.Count - 1 do
        begin
          Ser := FSeriesData[HintInfo.SerIndexes[i]];
          Info.Items.Add(Ser.FSeriesItems[HintInfo.SerItmIndexes[i]]);
          Info.Text.Add(HintInfo.Text[i]);
          Info.Colors.Add(HintInfo.Clrs[i]);
        end;
        SetRect;
        FOnMouseInfo(Self, Info, R.TopLeft, Canvas, Handled);
        if not Handled then
        begin
          if Info.Text.Count <> HintInfo.Text.Count then
            ShowGWMessage(msg_HintTextLines)
          else
            HintInfo.Text.Assign(Info.Text);
          for i := 0 to HintInfo.Text.Count - 1 do
            HintInfo.Clrs[i] := Info.Colors[i];
        end;
      finally
        Info.Items.Free;
        Info.Colors.Free;
        Info.Text.Free;
      end;
    end;

    SetRect;
    if not Handled then
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := GraphBGColor;
      Canvas.FillRect(R);
    end;
    for i := 0 to HintInfo.Text.Count - 1 do
    begin
      if not Handled then
      begin
        S := HintInfo.Text[i];
        Canvas.Font.Color := clBlack;
        Canvas.Brush.Color := GraphBGColor;
        Canvas.TextOut(R.Left+15, R.Top + (H * i), S);
        Canvas.Brush.Color := HintInfo.Clrs[i];
        Canvas.Pen.Color := clBlack;
        Canvas.Rectangle(
          Rect(R.Left + 3, R.Top+(H*i) + 3, R.Left + 12,
          R.Top + (H*I) + GetTextHeight(lkInfo)-3));
        ResetCanvas;
      end;
      if (veRulerGuideLines in AxisElements) then
      begin
        Canvas.Pen.Color := Canvas.Font.Color;
        if FNamAx.IsXAxis then
        begin
          Canvas.MoveTo(GraphRect.Left, HintInfo.SerPos[i].Y);
          Canvas.LineTo(GraphRect.Right, HintInfo.SerPos[i].Y);
        end
        else
        begin
          Canvas.MoveTo(HintInfo.SerPos[i].X, GraphRect.Top);
          Canvas.LineTo(HintInfo.SerPos[i].X, GraphRect.Bottom);
        end
      end;
    end;
    ResetCanvas;
    FLastMousePos := MP;
    RedrawGraphLines;
  end;

  procedure ProcessRuler;
  var
    Pt: TPoint;
    Res: Boolean;
    Cnt: integer;
    Inf: TRulerHintInfo;
    Defs: TSeriesDefs;
    Def: TSeriesDef;
    i: integer;
    Prec: TMousePrecision;
  begin
    if ViewMode = vmSelecting then
    begin
      DisplayHint(MP, -1);
      Exit;
    end;

    Cnt := 0;
    Res := ShowRuler;

    Finalize(Defs);
    if Res then
    begin
      Prec := FMousePrecision;
      if MouseInfo <> miNone then
      begin
        if Rulers = ruValues then
        begin
          FMousePrecision := mpLow;
        end;
        Res := PointsFromRuler(FNamAx.IsXAxis, FRulerX, FRulerY, Pt, Defs);
        if Rulers = ruValues then
          FMousePrecision := Prec;
      end
      else
        Res := False;
    end;
    if not Res then
    begin
      Inf.SerCount := 0;
      DisplayRulerHint(Inf, FRulerX, FRulerY);
      Exit;
    end;
    Inf.Text := TStringList.Create;

    Inf.SerCount := 0;

    for i := 0 to High(Defs) do
    begin
      Def := Defs[i];
      begin
        Pt := FSeriesData[Def.SeriesIndex].ItemPoints
          [Def.SeriesItemIndex - FSeriesData[Def.SeriesIndex].FirstItem];
        SetHintText(0, 0, Def.SeriesIndex, Def.SeriesItemIndex, nil);
        Inf.Text.Add(FHintText);
        Inf.SerPos[Cnt] := Pt;
        Inf.SerItmIndexes[Cnt] := Def.SeriesItemIndex;
        Inf.Clrs[i] := FHintColor;
        Inf.SerIndexes[Cnt] := FHintSeries;
        inc(Inf.SerCount);
        FHintSeries := Def.SeriesIndex;
        inc(Cnt)
      end;
    end;
    DisplayRulerHint(Inf, FRulerX, FRulerY);
    Inf.Text.Free;
    if Assigned(FOnRuler) then
      FOnRuler(Self);
  end;

  procedure DisplaySelecting;
  begin
    Canvas.Draw(ClientRect.Left, ClientRect.Top, FSelBM);
    R.Top := GraphRect.Top;
    R.Bottom := GraphRect.Bottom;
    if FNamAx.IsXAxis then
    begin
      R.Left := FSelStartX;
      R.Right := FSelStartX + FMouseDistanceX;
      if R.Width = 0 then
        Exit;
      W := R.Width;
      W2 := W;
      if R.Width < 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Left + abs(W);
      end;
    end
    else
    begin
      R.Top := FSelstartY;
      R.Bottom := FSelstartY + FMouseDistanceY;
      if R.Height = 0 then
        Exit;
      W := R.Height;
      W2 := W;
      if R.Height < 0 then
      begin
        R.Top := R.Bottom;
        R.Bottom := R.Top + abs(W);
      end;
    end;
    FSelectingRect := R;
    FAlfaBM.SetSize(R.Width, R.Height);
    FAlfaBM.Canvas.CopyRect(Rect(0, 0, FAlfaBM.Width, FAlfaBM.Height),
      Canvas, R);
    DrawAlphaBlend(FAlfaBM.Canvas.Handle, Rect(0, 0, FAlfaBM.Width,
      FAlfaBM.Height), 64);
    Canvas.Draw(R.Left, R.Top, FAlfaBM);
    W := GetTextWidth(lkInfo, FHintText);
    if FNamAx.IsXAxis then
    begin
      if W2 >= 0 then
        Canvas.TextOut(R.Right - W - 2, R.Height div 2, FHintText)
      else
        Canvas.TextOut(R.Left + 2, R.Height div 2, FHintText);
    end
    else
    begin
      if W2 >= 0 then
        Canvas.TextOut(R.Left + (R.Width div 2) - (W div 2),
          R.Bottom - Canvas.TextHeight(FHintText) - 2, FHintText)
      else
        Canvas.TextOut(R.Left + (R.Width div 2) - (W div 2), R.Top, FHintText);
    end
  end;

  procedure DrawEssentials;
  begin
    Chart.DrawTitle;
    DrawBorders;
    if ActiveGraph is TCWPie then
      Exit;
    FNamAx.DrawAxis;
    FValAx.DrawAxis;
    if Chart <> nil then
    begin
     if Chart.AxisCount = 2 then
      FValAx2.DrawAxis;
    end;
  end;

begin
  if (Chart = nil)  then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    Exit;
  end;

  try
    if (Chart <> nil) and (Count > 0) then
    begin
      for I := 0 to Chart.SeriesDefs.Count-1 do
      begin
        if Chart.SeriesDefs[i].Graph.Writer = nil then
        begin
          GoBackError;
          ShowGWError(msg_WriterNotAssigned);
        end;
      end;

    end;
    if (csDesigning in ComponentState) and not DsgnRealData then
    begin
     if not InitDesigner and LiveGraphs then
     begin
       Canvas.Brush.Color := Color;
       Canvas.FillRect(ClientRect);
       try
         RenderDesigner;
       finally
         InitDesigner := True;
       end;
       Exit;
     end
     else if (csDesigning in Componentstate) and not LiveGraphs then
     begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
      R := GraphRect;
      Canvas.Brush.Color := GraphBGColor;
      Canvas.FillRect(R);
      DrawEssentials;
      Exit;
     end;
    end;

    SetState(stPainting);
    if (Chart = nil) or InState(stMouseSizing) or InState(stLimbo) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
      Canvas.Draw(0, 0, FSelBM);
      ClearState(stPainting);
      Exit;
    end;
    try
      if InState(stAnimating) and not InState(stInitAnimation) then
      begin
        if InState(stAnimationPause) or InState(stResumeAnimation) then
        begin
          Canvas.Draw(0, 0, FSelBM);
          ClearState(stResumeAnimation);
          if InState(stAnimationPause) then
            Exit;
        end;
        for I := 0 to Chart.SeriesDefs.Count-1 do
         Chart.SeriesDefs[i].Graph.Draw;
        DrawBorders;
        Exit;
      end;
      if ViewMode = vmSelected then
      { Selected draw the saved bitmap (selBM) and do not need running
        the whole paint procedure }
      begin
        R := SelRect;
        Canvas.Draw(ClientRect.Left, ClientRect.Top, FSelBM);
        FAlfaBM.SetSize(R.Width, R.Height);

        FAlfaBM.Canvas.CopyRect(Rect(0, 0, FAlfaBM.Width, FAlfaBM.Height),
          Canvas, R);
        DrawAlphaBlend(FAlfaBM.Canvas.Handle, Rect(0, 0, FAlfaBM.Width,
          FAlfaBM.Height), 64);
        Canvas.Draw(R.Left, R.Top, FAlfaBM);
        Exit;
      end;
      ResetCanvas;
      Canvas.FillRect(ClientRect);

      if InState(stUpdating) then
      begin
        if Assigned(FAfterDrawGraph) then
          FAfterDrawGraph(Self, Canvas);
      end;

      R := GraphRect;
      Canvas.Brush.Color := GraphBGColor;
      Canvas.FillRect(R);

      if InState(stUpdating) then
        Exit;

      if (FSeriesData.Count = 0) then
      begin
        DrawEssentials;
        Exit;
      end;

      { Selecting or Hinting draw the saved bitmap (selBM) and do not need running
        the whole paint procedure }
      if ViewMode = vmSelecting then
      begin
        DisplaySelecting;
        Exit;
      end
      else if (ViewMode = vmHinting) then
      { The anchor ruler state does not react to hinting }
      begin
        if not(csDesigning in componentState) then
        begin
          if (Rulers <> ruNone) and (InView(TCWAxisGraph) <> nil) then
          begin
            ProcessRuler
          end
          else
          begin
            MP := ScreenToClient(MOuse.CursorPos);
            DisplayHint(MP, -1);
          end;
        end;
        Exit;
      end;

      { Draws axis lines, labels and sections }
      DrawBorders;
      if not (ActiveGraph is TCWPie) then
      begin
       Canvas.Brush.Color := Color;
       FValAx.Draw;
       if Chart.ValAx1Graph <> nil  then
         FValAx2.Draw;
       FNamAx.Draw;
      end;

      if InState(stInitAnimation) and not InState(stAnimating) then
      begin
        Chart.DrawTitle;
        if Assigned(FAfterDrawGraph) then
          FAfterDrawGraph(Self, Canvas);
        Exit;
      end;

      if Count > 0 then
      begin
        for I := 0 to Chart.Seriesdefs.Count-1 do
        begin
          Chart.SeriesDefs[i].Graph.Draw;
        end;
        Chart.DrawLegends;
        Chart.DrawTitle;
        Chart.DrawQualifiers;
      end;

      if Assigned(FAfterDrawGraph) then
        FAfterDrawGraph(Self, Canvas);

      if not MouseInfoControl then
        PostMessage(Handle, WM_SAVESELBM, 0, 0);
      RedrawGraphLines;
      if ActiveGraph <> nil then
        SaveSelBM;
    finally
      ClearState(stPainting);
    end;
  except
    GoBackError;
    raise;
  end;
end;

procedure TChartWriter.ComputeTextExtent;
var
  S: string;
  Ser: TSeries;
  i: integer;
  n, N2: integer;
  W: integer;
  W2: integer;
  E: single;
  Interv : single;

  function DoEvent(LabelKind: TLabelKind): integer;
  var
    WLbl, HLbl: string;
    WS, HS: string;

    function GetLbl(IsWidth: Boolean): String;
    begin
      if LabelKind = lkValue then
      begin
        if IsWidth then
          Result := FWidestValue
        else
          Result := FTallestValue;
      end
      else if LabelKind = lkValue2 then
      begin
        if IsWidth then
          Result := FWidestValue2
        else
          Result := FTallestValue2;
      end
      else if LabelKind = lkName then
      begin
        if IsWidth then
          Result := FWidestName
        else
          Result := FTallestName
      end
      else if LabelKind = lkValueSection then
      begin
        if IsWidth then
          Result := FWidestValueSection
        else
          Result := FTallestValueSection
      end
      else
      begin
        if IsWidth then
          Result := FWidestNameSection
        else
          Result := FTallestNameSection
      end;
    end;

    procedure SetLbl(Lbl: string; IsWidth: Boolean);
    begin
      if LabelKind = lkValue then
      begin
        if IsWidth then
          FWidestValue := Lbl
        else
          FTallestValue := Lbl
      end
      else if LabelKind = lkValue2 then
      begin
        if IsWidth then
          FWidestValue2 := Lbl
        else
          FTallestValue2 := Lbl
      end
      else if LabelKind = lkName then
      begin
        if IsWidth then
          FWidestName := Lbl
        else
          FTallestName := Lbl;
      end
      else if LabelKind = lkValueSection then
      begin
        if IsWidth then
          FWidestValueSection := Lbl
        else
          FTallestValueSection := Lbl
      end
      else
      begin
        if IsWidth then
          FWidestNameSection := Lbl
        else
          FTallestNameSection := Lbl;
      end;
    end;

  begin
    Result := 0;
    if ActiveGraph = nil then
      Exit;
    if ((toName in TextTilting) and (LabelKind = lkName)) or
      ((toValue in TextTilting) and (LabelKind = lkValue)) or
      ((toSection in TextTilting) and (LabelKind = lkValueSection)) or
      ((toSection in TextTilting) and (LabelKind = lkNameSection)) then
      Exit;
    if Assigned(FOnMeasureLabel) then
    begin
      WLbl := GetLbl(True);
      HLbl := GetLbl(False);
      WS := WLbl;
      HS := HLbl;
      FOnMeasureLabel(Self, LabelKind, WLbl, HLbl);
      Result := GetLabelW(WLbl, True, Canvas);
      if Result > Canvas.TextWidth(WS) then
        SetLbl(WLbl, True);
      Result := GetLabelW(HLbl, False, Canvas);
      if Result > Canvas.TextHeight(HS) then
        SetLbl(WLbl, False);
    end;
  end;

begin
  if FTextComputed then
  begin
    FTextComputed := false;
    Exit;
  end;
  begin
    if ActiveGraph = nil then
      Exit;

    FTallestNameSection := 'XY';
    FTallestValueSection := 'XY';
    FTallestNameShortSection := 'XY';
    FTallestValueShortSection := 'XY';
    FTallestValue := 'XY';
    FTallestValue2 := 'XY';

    FWidestNameShortSection := '';
    FWidestValueShortSection := '';
    FTallestValue := 'XY';

    Ser := FLongestSeries;
    if Ser = nil then
      Exit;
    Canvas.Font.Assign(Font);
    { Compute text width and height }

    N2 := 0;
    n := 0;
    W := 0;

    FActiveValAx := FValAx;
    Interv := Int(ValueHigh / 100);
    if Interv < 1 then
      Interv := 1;
    if ValueIntervals <  InterV then
    begin
     Chart.ValueAxis1.FValueIntervals := Interv;
    end;

    E := ValueLow;
    while E <= ValueHigh do
    begin
      S := FormatNum(E, ValuePrecision, True);
      if Canvas.TextWidth(S) > W then
      begin
        FWidestValue := S;
        W := Canvas.TextWidth(S);
      end;
      E := E + ValueIntervals;
    end;
    DoEvent(lkValue);

    FActiveValAx := FValAx2;
    Interv := Int(ValueHigh / 100);
    if Interv < 1 then
      Interv := 1;
    if ValueIntervals <  InterV then
    begin
     Chart.ValueAxis2.FValueIntervals := Interv;
    end;

    W := 0;
    E := ValueLow;
    while E <= ValueHigh do
    begin
      S := FormatNum(E, ValuePrecision, True);
      if Canvas.TextWidth(S) > W then
      begin
        FWidestValue2 := S;
        W := Canvas.TextWidth(S);
      end;
      E := E + ValueIntervals;
    end;
    DoEvent(lkValue2);
    FActiveValAx := FValAx;

    for i := Ser.FirstItem to Ser.LastItem do
    begin
      S := Ser.FSeriesItems[i].FName;

      if IsTimeSpan(Chart.NameType) then
        S := GetTimeStr(Self, TimeFormat, StrToDateTime(S, Fmt));

      if Canvas.TextWidth(S) > n then
      begin
        FWidestName := S;
        n := Canvas.TextWidth(S);
      end;

      if Canvas.TextHeight('XY') > N2 then
      begin
        FTallestName := 'XY';
        N2 := Canvas.TextHeight('XY');
      end;
    end;
    DoEvent(lkName);
  end;

  W2 := 0;
  W := 0;
  for i := 0 to FValueSections.Count - 1 do
  begin
    S := FValueSections[i].LongCaption;
    if Canvas.TextWidth(S) > W then
    begin
      FWidestValueSection := S;
      W := Canvas.TextWidth(S);
    end;

    S := FValueSections[i].ShortCaption;
    if Canvas.TextWidth(S) > W2 then
    begin
      FWidestValueShortSection := S;
      W2 := Canvas.TextWidth(S);
    end;
  end;
  DoEvent(lkValueSection);

  W2 := 0;
  W := 0;
  for i := 0 to FNameSections.Count - 1 do
  begin
    S := FNameSections[i].LongCaption;
    if Canvas.TextWidth(S) > W then
    begin
      FWidestNameSection := S;
      W := Canvas.TextWidth(S);
    end;
    S := FNameSections[i].ShortCaption;
    if Canvas.TextWidth(S) > W2 then
    begin
      FWidestNameShortSection := S;
      W2 := Canvas.TextWidth(S);
    end;
  end;
  DoEvent(lkNameSection);
end;

procedure TChartWriter.CheckLabelFreqs(ComputePts: Boolean = True);
var
  Num: single;

  function GetFreq(Ax: TAxisObject; Count: integer; LblKind: TLabelKind;
  Angle: integer): integer;
  var
    n: integer;
  begin
    if Ax.IsXAxis then
    begin
      n := GetTextWidth(LblKind, Angle) + c_LabelXMarg;
      if n = 0 then
      begin
        Result := 1;
        Exit;
      end;
      Num := GraphPrintRect.Width / n;
    end
    else
    begin
      n := GetTextHeight(LblKind, Angle);
      if n = 0 then
      begin
        Result := 1;
        Exit;
      end;
      Num := GraphPrintRect.Height / n;
    end;
    if Num = 0 then
      Result := 1
    else
      Result := Ceil(Count / Num);

    if (Result > 1) and (((LblKind = lkNameSection) and not FUseNamShortNames)
      or ((LblKind = lkValueSection) and not FUseValShortNames))

      and Ax.IsXAxis then
    begin
      if LblKind = lkNameSection then
      begin
        if FWidestNameShortSection <> '' then
        begin
          FUseNamShortNames := True;
          Result := GetFreq(Ax, Count, lkNameSection, Angle);
        end;
      end
      else
      begin
        if FWidestValueShortSection <> '' then
        begin
          FUseValShortNames := True;
          Result := GetFreq(Ax, Count, lkNameSection, Angle);
        end;
      end;
    end;

  end;

  function GetOrient(Ax: TAxisObject; Count: integer; LblKind: TLabelKind;
  IsOriented: Boolean): integer;
  var
    Angl: integer;
    Res: integer;
  begin
    Angl := 0;
    Result := GetFreq(Ax, Count, LblKind, 0);
    if not IsOriented or not Ax.IsXAxis then
    begin
      Exit;
    end;
    if (Result > TextTiltThreshold) then
    begin
      Res := Result;
      Result := GetFreq(Ax, Count, LblKind, 450);
      Angl := 450;
      if Result > TextTiltThreshold then
      begin
        Result := GetFreq(Ax, Count, LblKind, 900);
        Angl := 900;
      end;
      if Result = Res then
      begin
        Angl := 0;
      end;
    end;

    case LblKind of
      lkName:
        if Ax.IsXAxis then
          FNameFont.Orientation := Angl;
      lkValue:
        if Ax.IsXAxis then
          FValueFont.Orientation := Angl;
      lkNameSection:
        if (NameSectionDefs <> nil) and Ax.IsXAxis then
          NameSectionDefs.Font.Orientation := Angl;
      lkValueSection:
        if (ValueSectionDefs <> nil) and Ax.IsXAxis then
          ValueSectionDefs.Font.Orientation := Angl;
    end;
  end;

begin
  if (Count = 0) or InState(stUpdating) or (ActiveGraph = nil) or
    (ActiveGraph is TCWPie) then
    Exit;
  inc(FCallCounter);
  SetState(stLabelFreqs);
  try

  ComputeTextExtent;
  if ComputePts then
  begin
    ComputePoints;
  end;

  FNameFont.Orientation := 0;
  FValueFont.Orientation := 0;
  if (NameSectionDefs <> nil) then
    NameSectionDefs.Font.Orientation := 0;
  if (ValueSectionDefs <> nil) then
    ValueSectionDefs.Font.Orientation := 0;

  FUseNamShortNames := False;
  FUseValShortNames := False;
  FNameLabelFreq := GetOrient(FNamAx, NameCount, lkName,
    (toName in TextTilting));
  FValueLabelFreq := GetOrient(FValAx, FValAx.Count, lkValue,
    (toValue in TextTilting));
  if Chart.ValAx1Graph <> nil then
  begin
    FValueLabelFreq2 := GetOrient(FValAx2, FValAx2.Count, lkValue2,
      (toValue in TextTilting));
  end;
  FNameSectionFreq := GetOrient(FNamAx, FNameSections.Count, lkNameSection,
    (toSection in TextTilting));
  FValueSectionFreq := GetOrient(FValAx, FValueSections.Count, lkValueSection,
    (toSection in TextTilting));
  finally
    dec(FCallCounter);
    if FCallCounter = 0 then
      ClearState(stLabelFreqs);
  end;
end;

function TChartWriter.GetCount: integer;
begin
  Result := FSeriesData.Count;
end;

function TChartWriter.GetContraction: integer;
var
  i: integer;
  Dt1, Dt2: TDateTime;
begin
  Result := 0;
  Dt1 := Now; { To keep the compiler satisfied }
  Dt2 := Now;
  for i := 0 to FSeriesData.Count - 1 do
  begin
    if IsTimeSpan(Chart.NameType) then
    begin
      Dt1 := FSeriesData[i].ToDate(0);
      Dt2 := FSeriesData[i].ToDate(1);
    end;
    if FSeriesData[i].Count > 1 then
    begin
      if Chart.NameType = ntDateSpan then
        Result := DaysBetween(Dt1, Dt2)
      else if Chart.NameType = ntHourSpan then
        Result := HoursBetween(Dt1, Dt2)
      else if Chart.NameType = ntMinuteSpan then
        Result := MinutesBetween(Dt1, Dt2)
      else if Chart.NameType = ntSecondSpan then
        Result := SecondsBetween(Dt1, Dt2)
      else if Chart.NameType = ntNumberSpan then
        Result := (StrToInt(FSeriesData[i].FSeriesItems[1].FName) -
          StrToInt(FSeriesData[i].FSeriesItems[0].FName))
      else
      begin
        Result := 1;
      end;
      Break;
    end;
  end;
  if BaseNameInterval <> 0 then
    Result := Result div BaseNameInterval;
  if Result = 0 then
    Result := 1;
end;

function TChartWriter.AxisOf(Posit: TAxisPosition): TAxisObject;
begin
  if FNamAx.Position = Posit then
    Result := FNamAx
  else if FValAx.Position = Posit then
    Result := FValAx
  else if ((Chart <> nil) and (Chart.ValAx1Graph <> nil)) and (FValAx2.Position = Posit) then
    Result := FValAx2
  else
    Result := nil;
end;

function TChartWriter.GetVisibleCount: integer;
begin
  Result := 0;
  if Chart <> nil then
    Result := Chart.VisibleCount;
end;

function TChartWriter.CheckPieValues(Values: TSeriesItems;
APie: TCWPie): integer;
var
  Acc: single;
  i: integer;
begin
  Result := 0;
  if Chart = nil then
    Exit;
  Acc := 0;
  if Values.Count > 36 then
  begin
    Result := 2;
    Exit;
  end;
  for i := 0 to Values.Count - 1 do
  begin
    if Values[i].Value < 0 then
    begin
      Result := -1
    end;
    Acc := Acc + Values[i].Value;
  end;
  if (Acc > 100) and not TCWPieChart(Chart).CalcPercentages then
    Result := 1;
end;

procedure TChartWriter.NormaliseDates(ASeries: TSeries; SerIndx: integer);
var
  Y, M, D: word;
  i: integer;
  SerItm: TSeriesItem;
  LastY: integer;
  YInfo: TYearInfo;
  Dt: TDateTime;

begin
  LastY := 0;
  for i := 0 to ASeries.Count - 1 do
  begin
    Dt := StrToDate(ASeries.FSeriesItems[i].FName, Fmt);
    DecodeDate(Dt, Y, M, D);
    if IsLeapYear(Y) then
    begin
      inc(FLeapdateCount);
    end;
  end;

  for i := 0 to ASeries.Count - 1 do
  begin
    SerItm := ASeries.FSeriesItems[i];
    Dt := StrToDate(SerItm.FName, Fmt);
    SerItm.FName := DateToStr(Dt, Fmt); { Full date string }
    SerItm.FVisible := True;
    SerItm.FLeapDummy := False;
    DecodeDate(Dt, Y, M, D);
    if Y <> LastY then
    begin
      YInfo.Year := Y;
      YInfo.SerIndex := SerIndx;
      FYears.Add(YInfo);
      LastY := Y;
    end;

  end;
end;

procedure CheckNumberEquality(ASeries: TSeries; Writer: TChartWriter);
var
  Span: integer;
begin
  if Writer.InState(stInternalAction) then
    Exit;
  Span := StrToInt(ASeries.FSeriesItems[1].FName) -
    StrToInt(ASeries.FSeriesItems[0].FName);
  if Writer.FNumberInterval <> 0 then
  begin
    if Span <> Writer.FNumberInterval then
    begin
      Writer.FSeriesData.Delete(Writer.FSeriesData.Count - 1);
      ShowGWError(msg_NumIntervals);
    end;
  end
  else
    Writer.FNumberInterval := Span;
end;

procedure CheckDateEquality(ASeries: TSeries; Writer: TChartWriter);
var
  Span: integer;
  Dt: TDateTime;
begin
  if Writer.InState(stInternalAction) then
    Exit;
  if not TryStrToDate(ASeries.FSeriesItems[1].FName, Dt, Fmt) then
    ShowGWError(msg_NameTypeDtFormatMismatch);
  Span := DaysBetween(StrToDate(ASeries.FSeriesItems[1].FName, Fmt),
    StrToDate(ASeries.FSeriesItems[0].FName, Fmt));
  if Span = 0 then
    ShowGWError(msg_TimeSpanZero);
  if Writer.FNumberInterval <> 0 then
  begin
    if Span <> Writer.FNumberInterval then
    begin
      Writer.FSeriesData.Delete(Writer.FSeriesData.Count - 1);
      ShowGWError(msg_DateIntervals);
    end;
  end
  else
    Writer.FNumberInterval := Span;
end;

procedure CheckTimeEquality(ASeries: TSeries; Writer: TChartWriter);
var
  Span: integer;
  Dt1, Dt2: TDateTime;
begin
  if Writer.InState(stInternalAction) then
    Exit;
  Dt1 := StrToDateTime(ASeries.FSeriesItems[1].FName, Fmt);
  Dt2 := StrToDateTime(ASeries.FSeriesItems[0].FName, Fmt);
  if Writer.Chart.NameType = ntHourSpan then
    Span := HoursBetween(Dt1, Dt2)
  else if Writer.Chart.NameType = ntMinuteSpan then
    Span := MinutesBetween(Dt1, Dt2)
  else
    Span := SecondsBetween(Dt1, Dt2);
  if Span = 0 then
    ShowGWError(msg_TimeSpanZero);
  if Writer.FNumberInterval <> 0 then
  begin
    if Span <> Writer.FNumberInterval then
    begin
      Writer.FSeriesData.Delete(Writer.FSeriesData.Count - 1);
      ShowGWError(msg_DateIntervals);
    end;
  end
  else
    Writer.FNumberInterval := Span;
end;

procedure CheckGeneralEquality(ASeries: TSeries; Writer: TChartWriter);
begin
  if Writer.FNumberInterval = 0 then
    Writer.FNumberInterval := ASeries.Count
  else if ASeries.Count <> Writer.FNumberInterval then
  begin
    Writer.FSeriesData.Delete(Writer.FSeriesData.Count - 1);
    ShowGWError(msg_EqualLength);
  end;
  if Writer.ActiveGraph is TCWPie then
    ASeries.FPieState := Writer.CheckPieValues(ASeries.FSeriesItems,
      TCWPie(Writer.ActiveGraph));
end;

function TChartWriter.GetTitleSpace : integer;
var
  F : TFont;
begin
  Result := 0;
  if (Chart = nil) or (Chart.Title = '') then
   Exit;
  F := TFont.Create;
  try
   F.Assign(Canvas.Font);
   Canvas.Font.Assign(TitleFont);
   Result := Canvas.TextHeight(Chart.Title) + 13; {Fixed margins}
  finally
    Canvas.Font.Assign(F);
    F.Free;
  end;

end;

function TChartWriter.GetValPrecision: integer;
begin
  Result := ValuePrecision; // Only file reader calls this
end;

function TChartWriter.ValAxFromGraph(AGraph: TCWAxisGraph): TValueAxis;
var
  Indx: integer;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  Indx := Chart.FseriesDefs.IndexOf(AGraph);
  if Indx <> -1 then
  begin
   if Chart.FseriesDefs.Items[Indx].ValueAxis = vaValueAxis2 then
     Result := FValax2
   else
    Result := FValAx;
  end;

end;

function TChartWriter.InView(AGraphType: TClass; var Index: integer): TCWGraph;
var
  i: integer;
begin
  Result := nil;
  if Chart = nil then
   Exit;
  begin
    for i := 0 to Chart.FSeriesDefs.Count - 1 do
      if (Chart.FSeriesDefs[i].FGraph is AGraphType) and (i > Index)
      and Chart.FSeriesDefs[i].Visible then
      begin
        Result := Chart.FSeriesDefs[i].FGraph;
        inc(Index);
        Break;
      end;
  end;
end;

function TChartWriter.InView(AGraphType: TClass): TCWGraph;
var
  Indx: integer;
begin
  Indx := -1;
  Result := InView(AGraphType, Indx);
end;

function TChartWriter.IndexOfNearestName(AName: string;
StartWith: integer = 0): integer;
var
  i: integer;
  n, N2: Double;
begin
  Result := -1;
  if (Chart.NameType = ntGeneral) then
    Exit;
  if IsTimeSpan(Chart.NameType) then
  begin
    n := StrToDateTime(AName, Fmt);
  end
  else
    n := StrToFloat(AName, Fmt);

  for i := StartWith to FNameList.Count - 1 do
  begin
    if IsTimeSpan(Chart.NameType) then
    begin
      N2 := StrToDateTime(FNameList[i], Fmt);
    end
    else
      N2 := StrToFloat(FNameList[i], Fmt);
    if (N2 > n) then
    begin
      if IsTimeSpan(Chart.NameType) and (i > 0) then
      begin
        N2 := StrToDateTime(FNameList[i - 1], Fmt);
        if N2 > n then
        begin
          Break;
        end;
      end;
      Result := i - 1;
      Break;
    end
    else if n = N2 then
    begin
      if IsTimeSpan(Chart.NameType) and (i > 0) then
      begin
        N2 := StrToDateTime(FNameList[i - 1], Fmt);
        if N2 > n then
        begin
          Break;
        end;
      end;
      Result := i;
      Break;
    end;
  end;
end;

function TChartWriter.GetFileType(AFileName : string) : integer;
var
  sl : TstringList;
begin
  Result := -1;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    if sl.IndexOf(SignCWR) = 0 then
    begin
       Result := 0;
    end
    else if sl.IndexOf(SignCWD) = 0 then
       Result := 1
    else
      ShowGWError(msg_InvalidFile);
  finally
    sl.Free;
  end;
end;

procedure TChartWriter.LoadFromPropFile(AFileName: TFileName);
type
  TGraphName = record
     Nam : string;
     G : TCWGraph;
  end;
var
  PropFile: TCWFileReader;
  i, j: integer;
  Ser: TSeries;
  Itm: TSeriesItem;
  UxTime: int64;
  STime: string;
  Dt: TDateTime;
  V: single;
  ObjList: array of TGraphName;

  function SetCompName(AName : string) : string;
  var
    Fix : integer;
  begin
    Result := AName;
    if Owner.FindComponent(AName) = nil then
      Exit;
    Fix := 1;
    while Owner.FindComponent(AName + IntToStr(Fix)) <> nil do
    begin
      inc(Fix);
    end;
    Result := AName + IntToStr(Fix);
  end;

  procedure SetColors(Colors: TCWColors; Props: TStringList);
  var
    i, Indx, Indx2: integer;
    sl: TStringList;
    Clr: TCWColor;
  begin
    Indx2 := 0;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
      Indx := Props.IndexOf('{ITEMCOLOR' + IntToStr(0) + '}');
      while Indx <> -1 do
      begin
        sl.Clear;
        for i := 1 to 2 do
        begin
          sl.Add(Props[Indx + i]);
        end;
        Clr := Colors.Add;
        Clr.FItemName := sl.Values[C_Ident];
        Clr.FColor := TColor(StrToInt(sl.Values[C_Color]));
        inc(Indx2);
        Indx := Props.IndexOf('{ITEMCOLOR' + IntToStr(Indx2) + '}');
      end;
    finally
      sl.Free;
    end;
  end;

  procedure SetLegends(Legends: TCWLegends; Props: TStringList);
  var
    i, Indx, Indx2: integer;
    sl: TStringList;
    Leg: TCWLegend;
    LegItm: TCWLegendItem;
  begin
    Indx2 := 0;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
      Indx := Props.IndexOf('{LEGEND' + IntToStr(0) + '}');
      while Indx <> -1 do
      begin
        sl.Clear;
        for i := 1 to 25 do
        begin
          sl.Add(Props[Indx + i]);
        end;
        if sl.Values[C_Name] = '' then { Title leg. Is creted internally }
        begin
          inc(Indx2);
          Indx := Props.IndexOf('{LEGEND' + IntToStr(Indx2) + '}');
          Continue;
        end;

        LegItm := Legends.Add;
        Leg := TCWLegend.Create(Owner);
        LegItm.Legend := Leg;
        Leg.Name := SetCompName(sl.Values[C_Name]);
        Leg.FAlignment := TLegendAlignment(StrToInt(sl.Values[C_Alignment]));
        Leg.FAnchoring := TLegendAnchoring(StrToInt(sl.Values[C_Anchoring]));
        Leg.FBorder := Boolean(StrToInt(sl.Values[C_Border]));
        Leg.FBullets := TLegendBullets(StrToInt(sl.Values[C_Bullets]));
        Leg.FColoredText := Boolean(StrToInt(sl.Values[C_ColoredText]));
        Leg.FContents := [];
        if Boolean(StrToInt(sl.Values[C_coSeriesTitle])) then
          Leg.FContents := Leg.FContents + [coSeriesTitle];
        if Boolean(StrToInt(sl.Values[C_coValue])) then
          Leg.FContents := Leg.FContents + [coValue];
        if Boolean(StrToInt(sl.Values[C_coName])) then
          Leg.FContents := Leg.FContents + [coName];
        if Boolean(StrToInt(sl.Values[C_coNameSpan])) then
          Leg.FContents := Leg.FContents + [coNameSpan];
        Leg.FContentFlow := TContentFlow(StrToInt(sl.Values[C_ContentFlow]));
        Leg.FVisible := Boolean(StrToInt(sl.Values[C_Enabled]));
        Leg.FHorizMargins := StrToInt(sl.Values[C_HorizMargins]);
        Leg.FPointItemIndex := StrToInt(sl.Values[C_PointItemIndex]);
        Leg.FPointLocating :=
          TPointLocating(StrToInt(sl.Values[C_PointLocating]));
        Leg.FPointOptions := [];
        if Boolean(StrToInt(sl.Values[C_poShowConnectionLine])) then
          Leg.FPointOptions := Leg.FPointOptions + [poShowConnectionLine];
        if Boolean(StrToInt(sl.Values[C_poThinConnectionLine])) then
          Leg.FPointOptions := Leg.FPointOptions + [poThinConnectionLine];
        if Boolean(StrToInt(sl.Values[C_poEnlargePoint])) then
          Leg.FPointOptions := Leg.FPointOptions + [poEnlargePoint];
        Leg.FPointSeriesIndex := StrToInt(sl.Values[C_PointSeriesIndex]);
        Leg.FPointValue := sl.Values[C_PointValue];
        Leg.FTransparency := StrToInt(sl.Values[C_Transparency]);
        Leg.FVertMargins := StrToInt(sl.Values[C_VertMargins]);
        Leg.Text.Clear;
        EncodeStrings(Leg.Text, sl.Values[C_Text]);
        EncodeFont(Leg.Font, sl.Values[C_Font]);
        EncodeBrush(Leg.Brush, sl.Values[C_Brush]);
        inc(Indx2);
        Indx := Props.IndexOf('{LEGEND' + IntToStr(Indx2) + '}');
      end;
    finally
      sl.Free;
    end;
  end;

  procedure SetValSpans(Props: TStringList; Ax: TCWValueAxis);
  begin
    Ax.FValuePrecision := StrToInt(Props.Values[C_ValuePrecision]);
    Ax.FValueSpanFromData :=
      Boolean(Ord(StrToInt(Props.Values[C_ValueSpanFromData])));
    Ax.FValueIntervals := StrToFloat(Props.Values[C_ValueIntervals], Fmt);
    Ax.FValueLow := StrToFloat(Props.Values[C_ValueLow], Fmt);
    Ax.FValueHigh := StrToFloat(Props.Values[C_ValueHigh], Fmt);
    Ax.FQualifier := Props.Values[C_Qualifier];
  end;

  procedure CreateBar(Props: TStringList);
  var
    B: TCWBar;
    O : TGraphName;
  begin
    B := TCWBar.Create(Owner);
    B.Name := SetCompName(Props.Values[C_Name]);
    SetLength(ObjList, Length(ObjList) + 1);
    O.G := B;
    O.Nam := B.Name;
    ObjList[High(ObjList)] := O;
    B.FWriter := Self;
    B.FBaseLineValue := StrToFloat(Props.Values[C_BarBaseLineValue], Fmt);
    B.FItemSpacing := StrToInt(Props.Values[C_ItemSpacing]);
    B.FSeriesSpacing := StrToInt(Props.Values[C_SeriesSpacing]);
    B.FBarWidth := StrToInt(Props.Values[C_BarWidth]);
    B.FCubeDepth := StrToInt(Props.Values[C_CubeDepth]);
    B.FCubeAngle := StrToInt(Props.Values[C_CubeAngle]);
    B.FBarStyle := TBarStyle(StrToInt(Props.Values[C_BarStyle]));
    B.FLayout := TBarLayout(StrToInt(Props.Values[C_BarLayout]));
    B.FColorUsage := TColorUsage(StrToInt(Props.Values[C_ColorUsage]));
    B.FShowQualifier := Boolean(StrToInt(Props.Values[C_ShowQualifier]));
    B.FOptions := [];
    if Boolean(StrToInt(Props.Values[C_boBaseLine])) then
      B.FOptions := B.FOptions + [boBaseLine];
    if Boolean(StrToInt(Props.Values[C_boOutLines])) then
      B.FOptions := B.FOptions + [boOutLines];
    if Boolean(StrToInt(Props.Values[C_boText])) then
      B.FOptions := B.FOptions + [boText];

    B.FTextContents := [];
    if Boolean(StrToInt(Props.Values[C_tcName])) then
      B.FTextContents := B.FTextContents + [tcName];
    if Boolean(StrToInt(Props.Values[C_tcValue])) then
      B.FTextContents := B.FTextContents + [tcValue];
    if Boolean(StrToInt(Props.Values[C_tcTitle])) then
      B.FTextContents := B.FTextContents + [tcTitle];
    if Boolean(StrToInt(Props.Values[C_tcPercentage])) then
      B.FTextContents := B.FTextContents + [tcPercentage];
  end;

  procedure CreatePie(Props: TStringList);
  var
    P: TCWPie;
    O : TGraphName;
  begin
    P := TCWPie.Create(Owner);
    P.Name := SetCompName(Props.Values[C_Name]);
    SetLength(ObjList, Length(ObjList) + 1);
    O.G := P;
    O.Nam := P.Name;
    ObjList[High(ObjList)] := O;
    P.FWriter := Self;
    P.FOptions := [];
    if Boolean(StrToInt(Props.Values[C_poPrintNames])) then
      P.FOptions := P.FOptions + [poPrintNames];
    if Boolean(StrToInt(Props.Values[C_poPrintPercentages])) then
      P.FOptions := P.FOptions + [poPrintPercentages];
    if Boolean(StrToInt(Props.Values[C_poClientBorder])) then
      P.FOptions := P.FOptions + [poClientBorder];
    if Boolean(StrToInt(Props.Values[C_poPrintSeriesTitles])) then
      P.FOptions := P.FOptions + [poPrintSeriesTitles];
    P.FSliceSpacing := StrToFloat(Props.Values[C_PieSliceSpacing], Fmt);
    P.FPieSize := StrToInt(Props.Values[C_PieSize]);
  end;

  procedure CreateCurve(Props: TStringList);
  var
    c: TCWCurve;
    Indx, Indx2: integer;
    i: integer;
    sl: TStringList;
    St: TCWSeriesStyle;
    O : TGraphName;
  begin
    c := TCWCurve.Create(Owner);
    c.Name := SetCompName(Props.Values[C_Name]);
    SetLength(ObjList, Length(ObjList) + 1);
    O.G := C;
    O.Nam := C.Name;
    ObjList[High(ObjList)] := O;
    c.FWriter := Self;
    c.FStyle := TCurveStyle(StrToInt(Props.Values[C_CurveStyle]));
    c.FLineStyle := TCurvelineStyle(StrToInt(Props.Values[C_CurveLineStyle]));
    c.FLineWidth := StrToInt(Props.Values[C_CurveLineWidth]);
    c.FStep := Boolean(StrToInt(Props.Values[C_CurveStep]));
    c.FSmoothLines := Boolean(StrToInt(Props.Values[C_SmoothLines]));
    c.FAreaOutline := Boolean(StrToInt(Props.Values[C_AreaOutLine]));
    c.FAreaOutlineColor := TColor(StrToInt(Props.Values[C_AreaOutLineColor]));
    c.FBaseLineValue := StrToFloat(Props.Values[C_CurveBaseLineValue], Fmt);
    c.FMinPointSpacing := StrToInt(Props.Values[C_MinPointSpacing]);
    c.FMaxPointSpacing := StrToInt(Props.Values[C_MaxPointSpacing]);
    EncodeBrush(c.FAreaBrush, Props.Values[C_CurveBrush]);

    Indx2 := 0;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
      Indx := Props.IndexOf('{STYLE' + IntToStr(0) + '}');
      while Indx <> -1 do
      begin
        sl.Clear;
        for i := 1 to 4 do
        begin
          sl.Add(Props[Indx + i]);
        end;
        St := c.SeriesStyles.Add;
        St.FSeriesTitle := sl.Values[C_Ident];
        St.FLineStyle := TCurvelineStyle(StrToInt(sl.Values[C_CurveLineStyle]));
        St.FStyle := TCurveStyle(StrToInt(sl.Values[C_CurveStyle]));
        St.FLineWidth := StrToInt(sl.Values[C_CurveLineWidth]);
        inc(Indx2);
        Indx := Props.IndexOf('{STYLE' + IntToStr(Indx2) + '}');
      end;
    finally
      sl.Free;
    end;
  end;

  procedure SetSects(Props: TStringList; SD: TCWSectionDefs);
  var
    i, Indx, Indx2: integer;
    sl: TStringList;
    S: TCWSectionItem;
  begin
    Indx2 := 0;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
      Indx := Props.IndexOf('{SECTION' + IntToStr(0) + '}');
      while Indx <> -1 do
      begin
        sl.Clear;
        for i := 1 to 4 do
        begin
          sl.Add(Props[Indx + i]);
        end;
        S := SD.FSections.Add;
        S.FStartValue := sl.Values[C_StartValue];
        S.FEndValue := sl.Values[C_EndValue];
        S.FLongCaption := sl.Values[C_LongCaption];
        S.FShortCaption := sl.Values[C_ShortCaption];
        inc(Indx2);
        Indx := Props.IndexOf('{SECTION' + IntToStr(Indx2) + '}');
      end;
    finally
      sl.Free;
    end;
  end;

  procedure CreateNameSects(Props: TStringList);
  var
    NS: TCWNameSectionDefs;
  begin
    NS := TCWNameSectionDefs.Create(Chart);
    NS.Name := SetCompName(Props.Values[C_Name]);
    SetLength(ObjList, Length(ObjList) + 1);
    NS.FAutoSections := TAutoSections(StrToInt(Props.Values[C_AutoSections]));
    NS.FDateTimeTemplate := TDateTimeTemplate
      (StrToInt(Props.Values[C_DateTimeTemplate]));
    NS.FCaptionLayout := TCaptionLayout
      (StrToInt(Props.Values[C_CaptionLayout]));
    NS.FCaptionHorizMargin := StrToInt(Props.Values[C_CaptionHorzMargin]);
    NS.FCaptionVertMargin := StrToInt(Props.Values[C_CaptionVertMargin]);
    EncodeFont(NS.Font, Props.Values[C_Font]);
    EncodePen(NS.Pen, Props.Values[C_Pen]);
    NS.FShowLines := Boolean(StrToInt(Props.Values[C_ShowLines]));
    NS.FProlongedLines := Boolean(StrToInt(Props.Values[C_ProlongedLines]));
    SetSects(Props, NS);
    Chart.FNameSectionDefs := NS;
  end;

  procedure CreateValueSects(Props: TStringList);
  var
    VS: TCWValueSectionDefs;
  begin
    VS := TCWValueSectionDefs.Create(Chart);
    VS.Name := SetCompName(Props.Values[C_Name]);
    SetLength(ObjList, Length(ObjList) + 1);
    VS.FCaptionLayout := TCaptionLayout
      (StrToInt(Props.Values[C_CaptionLayout]));
    VS.FCaptionHorizMargin := StrToInt(Props.Values[C_CaptionHorzMargin]);
    VS.FCaptionVertMargin := StrToInt(Props.Values[C_CaptionVertMargin]);
    EncodeFont(VS.Font, Props.Values[C_Font]);
    EncodePen(VS.Pen, Props.Values[C_Pen]);
    VS.FShowLines := Boolean(StrToInt(Props.Values[C_ShowLines]));
    VS.FProlongedLines := Boolean(StrToInt(Props.Values[C_ProlongedLines]));
    SetSects(Props, VS);
    Chart.FValueSectionDefs := VS;
  end;

  function GetGraphByName(AName: string): TCWGraph;
  var
    i: integer;
    Fix : integer;
  begin
    Result := nil;
    for i := 0 to High(ObjList) do
    begin
      if ObjList[i].Nam = AName then
      begin
        Result := ObjList[i].G;
        Break;
      end
      else
      begin
        Fix := 1;
        while (AName + IntToStr(Fix)) <> ObjList[i].Nam do
        begin
          inc(Fix);
          if Fix = 10 then
            Break;
        end;
        if (AName + IntToStr(Fix)) = ObjList[i].Nam  then
          Result := ObjList[i].G;
      end;
    end;
  end;

  procedure CreateLegends(Props: TStringList);
  begin
    SetLegends(Chart.Flegends, Props);
  end;

  procedure CreateChart(Props: TStringList);
  var
    C : TCWChart;
    Indx, Indx2 : integer;
    sl : TStringList;
    SD : TCWSeriesDef;
    i : integer;
  begin
    C := FChart;
    C.Name := SetCompName(Props.Values[C_Name]);
    C.FTitle := Props.Values[C_Title];
    C.FOverflowAction := TOverflowAction
      (StrToInt(Props.Values[C_OverflowAction]));
    C.FValuePrecision := StrToInt(Props.Values[C_ValuePrecision]);
    C.FNumSpanPrecision := StrToInt(Props.Values[C_NumSpanPrecision]);
    //C.Writer := Self;
    C.FNameType := TNameType(StrToInt(Props.Values[C_NameType]));
    C.FCalcPercentages := Boolean(StrToInt(Props.Values[C_CalcPercentages]));
    C.FAxisOrientation := TAxisOrientation
      (StrToInt(Props.Values[C_AxisOrientation]));
    C.FWallWidth := StrToInt(Props.Values[C_WallWidth]);
    C.FTextTiltThreshold := StrToInt(Props.Values[C_TextTiltThreshold]);

    if Boolean(StrToInt(Props.Values[C_veBaseLine])) then
      C.FAxisElements := C.FAxisElements + [veBaseLine]
    else
      C.FAxisElements := C.FAxisElements - [veBaseLine];

    if Boolean(StrToInt(Props.Values[C_veNameLabels])) then
      C.FAxisElements := C.FAxisElements + [veNameLabels]
    else
      C.FAxisElements := C.FAxisElements - [veNameLabels];

    if Boolean(StrToInt(Props.Values[C_veValueLabels])) then
      C.FAxisElements := C.FAxisElements + [veValueLabels]
    else
      C.FAxisElements := C.FAxisElements - [veValueLabels];

    if Boolean(StrToInt(Props.Values[C_veNameDividerLines])) then
      C.FAxisElements := C.FAxisElements + [veNameDividerLines]
    else
      C.FAxisElements := C.FAxisElements - [veNameDividerLines];

    if Boolean(StrToInt(Props.Values[C_veValueDividerLines])) then
      C.FAxisElements := C.FAxisElements + [veValueDividerLines]
    else
      C.FAxisElements := C.FAxisElements - [veValueDividerLines];

    if Boolean(StrToInt(Props.Values[C_toName])) then
      C.FTextTilting := C.FTextTilting + [toName]
    else
      C.FTextTilting := C.FTextTilting - [toValue];
    if Boolean(StrToInt(Props.Values[C_toValue])) then
      C.FTextTilting := C.FTextTilting + [toValue]
    else
      C.FTextTilting := C.FTextTilting - [toValue];

    SetColors(C.ItemColors, Props);
    SetValSpans(Props, C.ValueAxis1);
    SetValSpans(Props, C.ValueAxis2);

    Indx2 := 0;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
      Indx := Props.IndexOf('{SERIES' + IntToStr(0) + '}');
      while Indx <> -1 do
      begin
        sl.Clear;
        for i := 1 to 5 do
        begin
          sl.Add(Props[Indx + i]);
        end;
        SD := c.SeriesDefs.Add;
        SD.FColor := TColor(StrToInt(sl.Values[C_Color]));
        SD.FValueAxis :=  TValueAxisNumber(StrToInt(sl.Values[C_ValueAxis]));
        SD.FVisible := Boolean(StrToInt(sl.Values[C_Visible]));
        SD.FTitle := sl.Values[C_Title];
        SD.FGraph := GetGraphByName(sl.Values[C_Graph]);
        inc(Indx2);
        Indx := Props.IndexOf('{SERIES' + IntToStr(Indx2) + '}');
      end;
    finally
      sl.Free;
    end;
  end;

begin
  PropFile := TCWFileReader.Create(AFileName, Self);
  try
    Clear;
    ClearHistory;

    { Create objects }
    FChart := TCWChart.Create(Self);
    for i := 0 to PropFile.FGraphs.Count - 1 do
    begin
      if PropFile.FGraphs[i][0] = '[CURVE]' then
        CreateCurve(PropFile.FGraphs[i])
      else if PropFile.FGraphs[i][0] = '[BAR]' then
        CreateBar(PropFile.FGraphs[i])
      else if PropFile.FGraphs[i][0] = '[PIE]' then
        CreatePie(PropFile.FGraphs[i])
      else if PropFile.FGraphs[i][0] = '[NAMESECTIONS]' then
        CreateNameSects(PropFile.FGraphs[i])
      else if PropFile.FGraphs[i][0] = '[VALUESECTIONS]' then
        CreateValueSects(PropFile.FGraphs[i])
      else if PropFile.FGraphs[i][0] = '[LEGENDS]' then
        CreateLegends(PropFile.FGraphs[i])
      else if (PropFile.FGraphs[i][0] = '[CHART]') then
      begin
        CreateChart(PropFile.FGraphs[i]);
      end;
    end;

    SetProps(PropFile.FProps); { General props }
    for i := 0 to PropFile.SeriesCount - 1 do
    begin
      Ser := TSeries.Create;
      SetState(stInternalAction);
      try
        for j := 0 to PropFile.ItemCount[i] - 1 do
        begin
          Itm := Ser.AddItem;
          V := PropFile.ItemValue[i, j] / 1000;
          Itm.FValue := V;
          if IsTimeSpan(Chart.NameType) then
          begin
            UxTime := StrToInt(PropFile.ItemName[i, j]);
            Dt := UnixToDateTime(UxTime);
            STime := DateToStr(Dt, Fmt);
            Itm.FName := STime;
          end
          else
            Itm.FName := PropFile.ItemName[i, j];
        end;
        AddSeries(Ser, False);
      finally
        ClearState(stInternalAction);
      end;
    end;
  finally
    PropFile.Free;
  end;
end;

procedure TChartWriter.LoadFromFile(AFileName: TFileName; DoExecute : Boolean = True);
var
  Files: TFiles;
  i: integer;

  function GetTimeInterval(T1, T2: TDateTime; ANameType: TNameType): integer;
  begin
    Result := 0;
    case ANameType of
      ntDateSpan:
        Result := DaysBetween(T2, T1);
      ntHourSpan:
        Result := HoursBetween(T2, T1);
      ntMinuteSpan:
        Result := MinutesBetween(T2, T1);
      ntSecondSpan:
        Result := SecondsBetween(T2, T1);
    end;
  end;

begin
  { Check file type }
  if GetFileType(AFileName) = 0 then
  begin
    if csDesigning in ComponentState then
      ShowGWError(msg_RichDesign);
    LoadFromPropFile(AFileName);
    if DoExecute then
      Execute;
    Exit;
  end;
  if (Chart = nil) or (ActiveGraph=nil) then
    ShowGWError(msg_NoActiveGraph);
  Files := TFiles.Create;
  SetState(stPainting);  { Aborts paint method}
  try
    DoLoadFiles(AFileName, Files);
    if Files.Count = 0 then
      Exit;
    Clear;
    for i := 0 to Files.Count - 1 do
    begin
      AddSeries(Files[i]);
    end;
  finally
    Files.Free;
    ClearState(stPainting);  { Aborts paint method}
  end;
  if DoExecute then
   Execute;
end;

procedure TChartWriter.DoLoadFiles(AFileName: string; Files: TFiles);
var
  WorkSl: TStringList;
  S: string;
  FirstIndex: integer;
  i: integer;
  TimSpan : Boolean;
  Indx, Indx2 : integer;
  Clr : TCWColor;
  Cnt : integer;
  Chk :Boolean;

  function DecodeUX(S: string): string;
  var
    P: integer;
    S2: string;
    n: integer;
  begin
    P := Pos('=', S);
    S2 := Copy(S, 1, P - 1);
    n := StrToInt(S2);
    S2 := DateTimeToStr(UnixToDateTime(n), Fmt);
    Delete(S, 1, P - 1);
    Result := S2 + S;
  end;

  procedure AddFile(FromIndex, ToIndex: integer);
  var
    i: integer;
    sl: TStringList;
    S: string;
    S2: string;
    Dt: TDateTime;
    V: single;

  begin
    if ToIndex - FromIndex < 2 then
      Exit;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    for i := FromIndex to ToIndex do
    begin
      S2 := WorkSl.ValueFromIndex[i];
      V := StrToFloat(S2, Fmt) / 1000;
      WorkSl.ValueFromIndex[i] := FormatNum(V, 3);
      S := WorkSl[i];

      if TimSpan then
      begin
        S2 := WorkSl.KeyNames[i];
        if not TryStrToDateTime(S2, Dt, Fmt) then
          try
            S := DecodeUX(S);
          except
            ShowGWError(msg_InvalidDateTime, S);
          end;
      end;
      sl.Add(S);
    end;
    Files.Add(sl);
  end;

  function GetValue(S : string) : string;
  var
    P : integer;
  begin
     P := Pos('=', S);
     Delete(S, 1, P);
     Result := S;
  end;

  function GetColor(ClrStr : string) : TColor;
  var
    R, G, B : Byte;
    sl : TStringList;
  begin
    S := GetValue(ClrStr);
    sl := TStringList.Create;
    try
    begin
       CreateListFromDelimiter(',', S, sl);
       R := StrToInt(sl[0]);
       G := StrToInt(sl[1]);
       B := StrToInt(sl[2]);
       Result := RGB(R, G, B);
    end;
    finally
      sl.Free;
    end;
  end;

begin
  WorkSl := TStringList.Create;
  WorkSl.DefaultEncoding := TEncoding.Utf8;
  Chk := false;
  try
   repeat
    Chk := not Chk;
    WorkSl.LoadFromFile(AFileName);
    if WorkSL.Count = 0 then
      Exit;
    WorkSL.Delete(0); {Signature}
    while Trim(WorkSl[0]) = '' do
      WorkSl.Delete(0);
    while Trim(WorkSl[WorkSl.Count-1]) = '' do
      WorkSl.Delete(WorkSl.Count-1);
    if WorkSL.Count = 0 then
      Exit;

    Indx := WorkSl.IndexOf('[TIMESPAN]');
    TimSpan := (Indx <> -1);
    Indx := WorkSl.IndexOf('[MAINTITLE]');
    if Indx <> -1 then
    begin
      Indx2 := WorkSl.IndexOf('[ENDMAINTITLE]');
      if Indx2 = -1 then
        ShowGWError(msg_FileFormat, AFileName);
      if not Chk then
       Chart.FTitle := GetValue(WorkSl[Indx + 1]);
    end;
    Indx := WorkSl.IndexOf('[SERIESTITLES]');
    if Indx <> -1 then
    begin
      Indx2 := WorkSl.IndexOf('[ENDSERIESTITLES]');
      if Indx2 = -1 then
       ShowGWError(msg_FileFormat, AFileName);
      if not Chk then
      begin
        Cnt := 0;
        for I := Indx+1 to Indx2-1 do
        begin
         Chart.FSeriesDefs[Cnt].FTitle := GetValue(WorkSL[i]);
         Inc(Cnt);
        end;
      end;
    end;

    Indx := WorkSl.IndexOf('[COLORS]');
    if Indx <> -1 then
    begin
      Indx2 := WorkSl.IndexOf('[ENDCOLORS]');
      if Indx2 = -1 then
        ShowGWError(msg_FileFormat, AFileName);
      if not Chk then
      begin
        Cnt := 0;
        for I := Indx+1 to Indx2-1 do
        begin
         Chart.FSeriesDefs[Cnt].FColor := GetColor(WorkSL[i]);
         Inc(Cnt);
        end;
      end;
    end;

    Indx := WorkSl.IndexOf('[ITEMCOLORS]');
    if Indx <> -1 then
    begin
      Indx2 := WorkSl.IndexOf('[ENDITEMCOLORS]');
      if Indx2 = -1 then
       ShowGWError(msg_FileFormat, AFileName);
      if not Chk then
      begin
        Chart.ItemColors.Clear;
        for I := Indx+1 to Indx2-1 do
        begin
         Clr := Chart.ItemColors.Add;
         Clr.FColor := GetColor(WorkSL[i]);
        end;
      end;
    end;

    Indx := WorkSl.IndexOf('[DATA]');
    Inc(Indx);
    if (Indx = 0) and TimSpan  then
      Inc(Indx);
    FirstIndex := Indx;
    for i := Indx to WorkSl.Count - 1 do
    begin
      S := WorkSl[i];
      if S = '' then
      begin
        if not Chk then
        begin
          AddFile(FirstIndex, i - 1);
          FirstIndex := i + 1;
        end;
      end
      else
      begin
        if Pos('=', S) = 0 then
          ShowGWError(msg_FileFormat, AFileName);
      end;
    end;
    if not Chk then
     AddFile(FirstIndex, WorkSl.Count - 1);
   until not Chk;
  finally
    WorkSl.Free;
  end;
end;

procedure TChartWriter.GetProps(PropList: TStringList);
var
  S: string;

  procedure SetTiltOpt(AnOption: TTextOrientation; AValue: string);
  begin
    S := AValue + '=0';
    if AnOption in TextTilting then
    begin
      S := AValue + '=1';
    end;
    PropList.Add(S);
  end;

begin
  PropList.Add(Props);
  S := C_TimeFormat + '=' + TimeFormat;
  PropList.Add(S);
  S := C_Chart + '=' + Chart.Name;
  PropList.Add(S);
  S := C_MouseTimeFormat + '=' + FMouseTimeFormat;
  PropList.Add(S);
  S := C_NameFont + '=' + DecodeFont(NameFont);
  PropList.Add(S);
  S := C_ValueFont + '=' + DecodeFont(ValueFont);
  PropList.Add(S);
  S := C_TitleFont + '=' + DecodeFont(TitleFont);
  PropList.Add(S);
  S := C_TitleAlignment + '=' + IntToStr(Ord(FTitleAlignment));
  PropList.Add(S);
  S := C_DividerLinePen + '=' + DecodePen(DividerLinePen);
  PropList.Add(S);
  S := C_GraphBGColor + '=' + IntToStr(FGraphBGColor);
  PropList.Add(S);
end;

procedure TChartWriter.SetProps(PropList: TStringList);
var
  i: integer;

begin
  for i := 0 to PropList.Count - 1 do
  begin
    if PropList.Names[i] = C_TimeFormat then
      FTimeFormat := PropList.ValueFromIndex[i];

    if PropList.Names[i] = C_MouseTimeFormat then
      FMouseTimeFormat := PropList.ValueFromIndex[i];

    if PropList.Names[i] = C_NameFont then
      EncodeFont(NameFont, PropList.ValueFromIndex[i]);

    if PropList.Names[i] = C_DividerLinePen then
      EncodePen(DividerLinePen, PropList.ValueFromIndex[i]);

    if PropList.Names[i] = C_GraphBGColor then
      FGraphBGColor := TColor(StrToInt(PropList.ValueFromIndex[i]));
  end;
end;

procedure TChartWriter.LoadFromDatabase(ADataset : TDataset;
 NameField, ValueFields : string; ATitle : string = ''; DoExecute : Boolean = True);
begin
  Clear;
  AddSeries(ADataSet, NameField, ValueFields, ATitle);
  if DoExecute then
    Execute;
end;

procedure TChartWriter.SaveToFile(AFileName: TFileName; Options : TSaveOptions);
{ Saves data only }
var
  i: integer;
  sl, saveSL: TStringList;
  G: TCWGraph;
  CCnt, BCnt, PCnt: integer;
  S: string;

  procedure SetBarOpt(AnOption: TBarOption; BarOptions: TBarOptions;
  AValue: string);
  begin
    S := AValue + '=0';
    if AnOption in BarOptions then
    begin
      S := AValue + '=1';
    end;
    saveSL.Add(S);
  end;

  procedure SetBarTextOpt(AnOption: TTextContent; TextOptions: TTextContents;
  AValue: string);
  begin
    S := AValue + '=0';
    if AnOption in TextOptions then
    begin
      S := AValue + '=1';
    end;
    saveSL.Add(S);
  end;

  procedure SetPieOpt(AnOption: TPieOption; PieOptions: TPieOptions;
  AValue: string);
  begin
    S := AValue + '=0';
    if AnOption in PieOptions then
    begin
      S := AValue + '=1';
    end;
    saveSL.Add(S);
  end;

  procedure SetLegContentOpt(AnOption: TLegendContent;
  ContentOptions: TLegendContents; AValue: string);
  begin
    S := AValue + '=0';
    if AnOption in ContentOptions then
    begin
      S := AValue + '=1';
    end;
    saveSL.Add(S);
  end;

  procedure SetLegPointOpt(AnOption: TPointOption; PointOptions: TPointOptions;
  AValue: string);
  begin
    S := AValue + '=0';
    if AnOption in PointOptions then
    begin
      S := AValue + '=1';
    end;
    saveSL.Add(S);
  end;

  procedure GetTitle;
  begin
    SetKeyVal(saveSL, C_Title, Chart.Title);
  end;

  procedure GetValSpans(Valspan: TCWValueAxis; Num : integer);
  begin
    saveSL.Add('{VALUEAXIS' + IntToStr(Num) + '}');
    SetKeyVal(saveSL, C_ValueSpanFromData, IntToStr(Ord(ValSpan.ValueSpanFromData)));
    SetKeyVal(saveSL, C_ValueIntervals, FormatNum(ValSpan.ValueIntervals,
      ValSpan.ValuePrecision));
    SetKeyVal(saveSL, C_ValueHigh, FormatNum(ValSpan.ValueHigh, ValSpan.ValuePrecision));
    SetKeyVal(saveSL, C_ValueLow, FormatNum(ValSpan.ValueLow, ValSpan.ValuePrecision));
    SetKeyVal(saveSL, C_ValuePrecision, IntToStr(ValSpan.ValuePrecision));
    SetKeyVal(saveSL, C_Qualifier, ValSpan.FQualifier);
  end;

  procedure GetSeriesDefs;
  var
    i : integer;
  begin
     for I := 0 to Chart.SeriesDefs.Count-1 do
     begin
       saveSL.Add('{SERIES' + IntToStr(i) + '}');
       SetKeyVal(saveSL, C_Graph, Chart.SeriesDefs.Items[i].Graph.Name);
       SetKeyVal(saveSL, C_Title, Chart.SeriesDefs.Items[i].Title);
       SetKeyVal(saveSL, C_Color, IntToStr(Chart.SeriesDefs.Items[i].Color));
       SetKeyVal(saveSL, C_ValueAxis, IntToStr(Ord(Chart.SeriesDefs.Items[i].ValueAxis)));
       SetKeyVal(saveSL, C_Visible, IntToStr(Ord(Chart.SeriesDefs.Items[i].Visible)));
     end;
  end;

  procedure GetColors(Clrs: TCWColors);
  var
    i: integer;
  begin
    for i := 0 to Clrs.Count - 1 do
    begin
      saveSL.Add('{ITEMCOLOR' + IntToStr(i) + '}');
      SetKeyVal(saveSL, C_Ident, Clrs.Items[i].ItemName);
      SetKeyVal(saveSL, C_Color, IntToStr(Clrs.Items[i].Color));
    end;
  end;

  procedure GetLegends;
  var
    i: integer;
    Leg: TCWLegend;
    S: string;
  begin
    saveSL.Add('[LEGENDS]');
    for i := 0 to Chart.Legends.Count - 1 do
    begin
      saveSL.Add('{LEGEND' + IntToStr(i) + '}');
      Leg := Chart.Legends.Items[i].Legend;
      SetKeyVal(saveSL, C_Name, Leg.Name);
      SetKeyVal(saveSL, C_Alignment, IntToStr(Ord(Leg.Alignment)));
      SetKeyVal(saveSL, C_Anchoring, IntToStr(Ord(Leg.Anchoring)));
      SetKeyVal(saveSL, C_Border, IntToStr(Ord(Leg.Border)));
      SetKeyVal(saveSL, C_Bullets, IntToStr(Ord(Leg.Bullets)));
      SetKeyVal(saveSL, C_ColoredText, IntToStr(Ord(Leg.ColoredText)));
      SetLegContentOpt(coSeriesTitle, Leg.Contents, C_coSeriesTitle);
      SetLegContentOpt(coValue, Leg.Contents, C_coValue);
      SetLegContentOpt(coName, Leg.Contents, C_coName);
      SetLegContentOpt(coNameSpan, Leg.Contents, C_coNameSpan);
      SetKeyVal(saveSL, C_ContentFlow, IntToStr(Ord(Leg.ContentFlow)));
      SetKeyVal(saveSL, C_Enabled, IntToStr(Ord(Leg.Visible)));
      SetKeyVal(saveSL, C_HorizMargins, IntToStr(Leg.HorizMargins));
      SetKeyVal(saveSL, C_PointItemIndex, IntToStr(Leg.PointItemIndex));
      SetKeyVal(saveSL, C_PointLocating, IntToStr(Ord(Leg.PointLocating)));
      SetLegPointOpt(poShowConnectionLine, Leg.PointOptions,
        C_poShowConnectionLine);
      SetLegPointOpt(poThinConnectionLine, Leg.PointOptions,
        C_poThinConnectionLine);
      SetLegPointOpt(poEnlargePoint, Leg.PointOptions, C_poEnlargePoint);
      SetKeyVal(saveSL, C_PointSeriesIndex, IntToStr(Leg.PointSeriesIndex));
      SetKeyVal(saveSL, C_PointValue, Leg.PointValue);
      SetKeyVal(saveSL, C_Transparency, IntToStr(Ord(Leg.Transparency)));
      SetKeyVal(saveSL, C_VertMargins, IntToStr(Leg.VertMargins));
      S := DecodeStrings(Leg.Text);
      SetKeyVal(saveSL, C_Text, S);
      S := DecodeBrush(Leg.Brush);
      SetKeyVal(saveSL, C_Brush, S);
      S := DecodeFont(Leg.Font);
      SetKeyVal(saveSL, C_Font, S);
    end;
  end;

  procedure GetSections(S: TCWSectionDefs);
  var
    i: integer;
  begin
    for i := 0 to S.Sections.Count - 1 do
    begin
      saveSL.Add('{SECTION' + IntToStr(i) + '}');
      SetKeyVal(saveSL, C_StartValue, S.Sections.Items[i].StartValue);
      SetKeyVal(saveSL, C_EndValue, S.Sections.Items[i].EndValue);
      SetKeyVal(saveSL, C_LongCaption, S.Sections.Items[i].LongCaption);
      SetKeyVal(saveSL, C_ShortCaption, S.Sections.Items[i].ShortCaption);
    end;
  end;

  procedure GetNameSectionProps(NS: TCWNameSectionDefs);
  begin
    saveSL.Add('[NAMESECTIONS]');
    SetKeyVal(saveSL, C_Name, NS.Name);
    SetKeyVal(saveSL, C_AutoSections, IntToStr(Ord(NS.AutoSections)));
    SetKeyVal(saveSL, C_DateTimeTemplate, IntToStr(Ord(NS.DateTimeTemplate)));
    SetKeyVal(saveSL, C_CaptionLayout, IntToStr(Ord(NS.CaptionLayout)));
    SetKeyVal(saveSL, C_CaptionHorzMargin,
      IntToStr(Ord(NS.CaptionHorizMargin)));
    SetKeyVal(saveSL, C_CaptionVertMargin, IntToStr(Ord(NS.CaptionVertMargin)));
    SetKeyVal(saveSL, C_Font, DecodeFont(NS.Font));
    SetKeyVal(saveSL, C_Pen, DecodePen(NS.Pen));
    SetKeyVal(saveSL, C_ProlongedLines, IntToStr(Ord(NS.ProlongedLines)));
    SetKeyVal(saveSL, C_ShowLines, IntToStr(Ord(NS.ShowLines)));
    GetSections(NS);
  end;

  procedure GetValueSectionProps(VS: TCWValueSectionDefs);
  begin
    saveSL.Add('[VALUESECTIONS]');
    SetKeyVal(saveSL, C_Name, VS.Name);
    SetKeyVal(saveSL, C_CaptionLayout, IntToStr(Ord(VS.CaptionLayout)));
    SetKeyVal(saveSL, C_CaptionHorzMargin,
      IntToStr(Ord(VS.FCaptionHorizMargin)));
    SetKeyVal(saveSL, C_CaptionVertMargin,
      IntToStr(Ord(VS.FCaptionVertMargin)));
    SetKeyVal(saveSL, C_Font, DecodeFont(VS.Font));
    SetKeyVal(saveSL, C_Pen, DecodePen(VS.Pen));
    SetKeyVal(saveSL, C_ProlongedLines, IntToStr(Ord(VS.ProlongedLines)));
    SetKeyVal(saveSL, C_ShowLines, IntToStr(Ord(VS.ShowLines)));
    GetSections(VS);
  end;

  procedure GetCurveProps(c: TCWCurve);
  var
    i: integer;
    S: string;
  begin
    saveSL.Add('[CURVE]');
    inc(CCnt);
    SetKeyVal(saveSL, C_Name, c.Name);
    SetKeyVal(saveSL, C_CurveLineStyle, IntToStr(Ord(c.LineStyle)));
    SetKeyVal(saveSL, C_CurveStyle, IntToStr(Ord(c.Style)));
    SetKeyVal(saveSL, C_CurveLineWidth, IntToStr(c.LineWidth));
    SetKeyVal(saveSL, C_CurveStep, IntToStr(Ord(c.Step)));
    SetKeyVal(saveSL, C_SmoothLines, IntToStr(Ord(c.SmoothLines)));
    SetKeyVal(saveSL, C_AreaOutline, IntToStr(Ord(c.AreaOutLine)));
    SetKeyVal(saveSL, C_AreaOutlineColor, IntToStr(Ord(c.AreaOutlineColor)));
    SetKeyVal(saveSL, C_UseSeriesStyles, IntToStr(Ord(c.UseSeriesStyles)));
    SetKeyVal(saveSL, C_CurveBaseLineValue, FormatNum(c.BaseLineValue,
      ValuePrecision));
    SetKeyVal(saveSL, C_MinPointSpacing, IntToStr(c.MinPointSpacing));
    SetKeyVal(saveSL, C_MaxPointSpacing, IntToStr(c.MaxPointSpacing));

    S := DecodeBrush(c.FAreaBrush);
    SetKeyVal(saveSL, C_CurveBrush, S);
    for i := 0 to c.SeriesStyles.Count - 1 do
    begin
      saveSL.Add('{STYLE' + IntToStr(i) + '}');
      SetKeyVal(saveSL, C_Ident, c.SeriesStyles.Items[i].SeriesTitle);
      SetKeyVal(saveSL, C_CurveStyle,
        IntToStr(Ord(c.SeriesStyles.Items[i].Style)));
      SetKeyVal(saveSL, C_CurveLineStyle,
        IntToStr(Ord(c.SeriesStyles.Items[i].LineStyle)));
      SetKeyVal(saveSL, C_CurveLineWidth,
        IntToStr(Ord(c.SeriesStyles.Items[i].LineWidth)));
    end;
  end;

  procedure GetBarProps(B: TCWBar);
  begin
    saveSL.Add('[BAR]');
    inc(BCnt);
    SetKeyVal(saveSL, C_Name, B.Name);
    SetKeyVal(saveSL, C_BarWidth, IntToStr(B.BarWidth));
    SetKeyVal(saveSL, C_ItemSpacing, IntToStr(B.ItemSpacing));
    SetKeyVal(saveSL, C_SeriesSpacing, IntToStr(B.SeriesSpacing));
    SetKeyVal(saveSL, C_BarLayout, IntToStr(Ord(B.Layout)));
    SetKeyVal(saveSL, C_BarStyle, IntToStr(Ord(B.BarStyle)));
    SetKeyVal(saveSL, C_BarBaseLineValue, FloatToStr(B.BaseLineValue));
    SetKeyVal(saveSL, C_CubeAngle, IntToStr(B.CubeAngle));
    SetKeyVal(saveSL, C_CubeDepth, IntToStr(B.CubeDepth));
    SetKeyVal(saveSL, C_ColorUsage, IntToStr(Ord(B.ColorUsage)));
    SetKeyVal(saveSL, C_ShowQualifier, IntToStr(Ord(B.ShowQualifier)));
    SetBarOpt(boBaseLine, B.Options, C_boBaseLine);
    SetBarOpt(boOutLines, B.Options, C_boOutLines);
    SetBarOpt(boText, B.Options, C_boText);
    SetBarTextOpt(tcName, B.TextContents, C_tcName);
    SetBarTextOpt(tcValue, B.TextContents, C_tcValue);
    SetBarTextOpt(tcTitle, B.TextContents, C_tcTitle);
    SetBarTextOpt(tcPercentage, B.TextContents, C_tcPercentage);
  end;

  procedure GetPieProps(P: TCWPie);
  begin
    saveSL.Add('[PIE]');
    inc(PCnt);
    SetKeyVal(saveSL, C_Name, P.Name);
    SetKeyVal(saveSL, C_PieSize, IntToStr(P.PieSize));
    SetKeyVal(saveSL, C_PieSliceSpacing, FormatNum(P.SliceSpacing, 1));
    SetPieOpt(poPrintPercentages, P.Options, C_poPrintPercentages);
    SetPieOpt(poPrintNames, P.Options, C_poPrintNames);
    SetPieOpt(poClientBorder, P.Options, C_poClientBorder);
    SetPieOpt(poPrintSeriesTitles, P.Options, C_poPrintSeriesTitles);
  end;

  procedure DoSaveData;
  var
    i: integer;
    R, Gr, B : Byte;
    Clr : TColor;
  begin
    saveSL := TStringList.Create;
    saveSL.DefaultEncoding := TEncoding.Utf8;
    Savesl.Add(SignCWD);
    try
     if IsTimeSpan(Chart.NameType) then
      Savesl.Add('[TIMESPAN]');

     if Options = soDataAndSeriesProps then
     begin
        Savesl.Add('[MAINTITLE]');
        Savesl.Add('TITLE=' + Chart.Title);
        Savesl.Add('[ENDMAINTITLE]');

        Savesl.Add('[SERIESTITLES]');
        for i := 0 to Chart.SeriesDefs.Count-1 do
        begin
           Savesl.Add('TITLE=' + Chart.SeriesDefs.Items[i].Title);
        end;
        Savesl.Add('[ENDSERIESTITLES]');

        SaveSl.Add('[COLORS]');
        for i := 0 to Chart.SeriesDefs.Count-1 do
        begin
           Clr := Chart.SeriesDefs.Items[i].FColor;
           R := GetRValue(Clr);
           Gr := GetGValue(Clr);
           B := GetBValue(Clr);
           SaveSl.Add(IntToStr(R) + ',' + IntToStr(Gr) + ',' + IntToStr(B));
        end;
        if Chart.SeriesDefs.Count > 0 then
        begin
          SaveSL.Add('[ENDCOLORS]');
        end;

        if Chart.ItemColors.Count > 0 then
         Savesl.Add('[ITEMCOLORS]');
        for i := 0 to Chart.ItemColors.Count-1 do
        begin
           Clr := Chart.ItemColors.Items[i].FColor;
           R := GetRValue(Clr);
           Gr := GetGValue(Clr);
           B := GetBValue(Clr);
           Savesl.Add(IntToStr(R) + ',' + IntToStr(Gr) + ',' + IntToStr(B));
        end;
        if Chart.ItemColors.Count > 0 then
        begin
          Savesl.Add('[ENDITEMCOLORS]');
        end;
        Savesl.Add('[DATA]');
     end;

     for i := 0 to FOrigData.Count - 1 do
     begin
        sl := FOrigData[i].AsStrings(False);
        saveSL.AddStrings(sl);
        if i < FOrigData.Count-1 then
         saveSL.Add('');
        sl.Free;
     end;
     SaveSl.SaveToFile(AFileName);
    finally
      saveSL.Free;
    end;
  end;

begin
  if FSeriesData.Count = 0 then
    ShowGWError(msg_NothingToSave);
  if Options in [soDataOnly, soDataAndSeriesProps] then
  begin
    DoSaveData;
    Exit;
  end;

  if ActiveGraph = nil then
    ShowGWError(msg_NoActiveGraph);
  saveSL := TStringList.Create;
  saveSL.DefaultEncoding := TEncoding.Utf8;

  { Get general props }
  saveSL.Add(SignCWR);
  GetProps(saveSL);
  saveSL.Add('[Data]');

  { Get series data }
  try
    for i := 0 to FOrigData.Count - 1 do
    begin
      sl := FOrigData[i].AsStrings(False);
      saveSL.AddStrings(sl);
      saveSL.Add(EOF);
      sl.Free;
    end;

    { Get graph objects }
    BCnt := 0;
    CCnt := 0;
    PCnt := 0;
    saveSL.Add(Objects);
    for i := 0 to Chart.SeriesDefs.Count - 1 do
    begin
      G := Chart.SeriesDefs[i].Graph;
      if G.InChart then
      begin
        if G is TCWCurve then
          GetCurveProps(G as TCWCurve)
        else if G is TCWBar then
          GetBarProps(G as TCWBar)
        else if G is TCWPie then
          GetPieProps(G as TCWPie);
        saveSL.Add(EOF);
      end;
    end;
    if Chart.Legends.Count > 0 then
    begin
     GetLegends;
     saveSL.Add(EOF);
    end;
    if Assigned(Chart.NameSectionDefs) then
    begin
     GetNameSectionProps(Chart.NameSectionDefs);
     saveSL.Add(EOF);
    end;
    if Assigned(Chart.ValueSectionDefs) then
    begin
     GetValueSectionProps(Chart.ValueSectionDefs);
     saveSL.Add(EOF);
    end;
    saveSL.Add('[CHART]');
    saveSL.Add(C_Name + '=' + Chart.Name);
    saveSL.Add(C_Title + '=' + Chart.Title);
    saveSL.Add(C_Nametype + '=' + IntToStr(Ord(Chart.FNameType)));
    saveSL.Add(C_OverflowAction + '=' + IntToStr(Ord(Chart.FOverflowAction)));
    saveSL.Add(C_ValuePrecision + '=' + IntToStr(Chart.FValuePrecision));
    saveSL.Add(C_NumSpanPrecision + '=' + IntToStr(Chart.FNumSpanPrecision));
    saveSL.Add(C_CalcPercentages + '=' + IntToStr(Ord(Chart.FCalcPercentages)));
    saveSL.Add(C_AxisOrientation + '=' + IntToStr(Ord(Chart.FAxisOrientation)));
    saveSL.Add(C_WallWidth + '=' + IntToStr(Chart.FWallWidth));
    saveSL.Add(C_TextTiltThreshold + '=' + IntToStr(Chart.FTextTiltThreshold));
    if Chart.NameSectionDefs <> nil then
      saveSL.Add(C_NameSectionDefs + '=' + Chart.NameSectionDefs.Name);
    if Chart.ValueSectionDefs <> nil then
    saveSL.Add(C_ValueSectionDefs + '=' + Chart.ValueSectionDefs.Name);

    S := C_veBaseLine +'=0';
    if veBaseLine in Chart.AxisElements then
      S := C_veBaseLine +'=1';
    saveSL.Add(S);

    S := C_veNameLabels +'=0';
    if veNameLabels in Chart.AxisElements then
      S := C_veNameLabels +'=1';
    saveSL.Add(S);

    S := C_veValueLabels +'=0';
    if veValueLabels in Chart.AxisElements then
      S := C_veValueLabels +'=1';
    saveSL.Add(S);

    S := C_veNameDividerLines +'=0';
    if veNameDividerLines in Chart.AxisElements then
      S := C_veNameDividerLines +'=1';
    saveSL.Add(S);

    S := C_veValueDividerLines +'=0';
    if veValueDividerLines in Chart.AxisElements then
      S := C_veValueDividerLines +'=1';
    saveSL.Add(S);

    S := C_toName +'=0';
    if toName in Chart.FTextTilting then
      S := C_toName +'=1';
    saveSL.Add(S);

    S := C_toValue +'=0';
    if toValue in Chart.FTextTilting then
      S := C_toValue +'=1';
    saveSL.Add(S);

    GetSeriesDefs;
    GetColors(Chart.ItemColors);
    GetValspans(Chart.ValueAxis1, 1);
    GetValspans(Chart.ValueAxis2, 2);
    saveSL.Add(EOF);

    saveSL.SaveToFile(AFileName);
  finally
    saveSL.Free;
  end;
end;

function TChartWriter.GetScrollable: Boolean;
begin
  Result := False;
  if (Chart = nil) then
    Exit;
  if (ActiveGraph = nil) or (ActiveGraph is TCWPie) then
    Exit;
  Result := Chart.OverflowAction = ovScrolling;
end;

procedure TChartWriter.GetValueSpan(var HighVal, LowVal: single);
var
  i: integer;
begin
  if Count = 0 then
  begin
    HighVal := 1;
    LowVal := 0;
    Exit;
  end;
  HighVal := -MaxInt;
  LowVal := MaxInt;
  for i := 0 to Count - 1 do
  begin
    if FSeriesData[i].FMax > HighVal then
      HighVal := FSeriesData[i].FMax;
    if FSeriesData[i].FMin < LowVal then
      LowVal := FSeriesData[i].FMin;
  end;
end;

procedure TChartWriter.GetValueSpan(ASeries: TSeries;
var HighVal, LowVal: single);
begin
  HighVal := ASeries.FMax;
  LowVal := ASeries.FMin;
end;

procedure TChartWriter.AddSeries(ASeries: TSeries; ATitle : string = '');
begin
  ASeries.FLinkTitle := ATitle;
  AddSeries(ASeries, False);
end;

procedure TChartWriter.AddSeries(ASeries: TSeries; Internal: Boolean = False);
var
  i: integer;
  SerItm: TSeriesItem;

  procedure DoError(M: integer);
  begin
    ASeries.Free;
    GoBackError;
    ShowGWError(M);
  end;

begin
  if ASeries.Count = 0 then
    DoError(msg_DataEmpty);
  if not InState(stUpdating) and (FSeriesData.Count > 0) then
    DoError(msg_ActiveSeries);
  if (Chart = nil) or (ActiveGraph = nil) then
    DoError(msg_NoActiveGraph);

  FNumberInterval := 0;
  ASeries.FMax := -MaxInt;
  ASeries.FMin := MaxInt;

  ASeries.FWriter := Self;
  for i := 0 to ASeries.Count - 1 do
  begin
    SerItm := ASeries.FSeriesItems[i];
    SerItm.FOwner := ASeries;
    SerItm.FWriter := Self;
    SerItm.FRealVal := SerItm.FValue;

    if SerItm.FValue > ASeries.FMax then
      ASeries.FMax := SerItm.FValue;
    if SerItm.FValue < ASeries.FMin then
      ASeries.FMin := SerItm.FValue;
    ASeries.FSum := ASeries.FSum + SerItm.FValue;
    if SerItm.FValue < 0 then
      ASeries.FNegVals := True;
    if not Internal then
    begin
      SerItm.FOrigName := SerItm.FName;
      TryStrToDateTime(SerItm.FName, SerItm.FRealDate, Fmt);
    end;
  end;
  FSeriesData.Add(ASeries);
  Chart.FNameType := DetectNameType(FSeriesData.Count - 1);
  try
    if (Chart.NameType = ntNumberSpan) and (ASeries.Count > 1) then
    begin
      CheckNumberEquality(ASeries, Self);
    end
    else if (Chart.NameType = ntDateSpan) and (ASeries.Count > 1) then
    begin
      CheckDateEquality(ASeries, Self);
    end
    else if IsTimeSpan(Chart.NameType) then
      CheckTimeEquality(ASeries, Self)
    else if (Chart.NameType = ntGeneral) then
      CheckGeneralEquality(ASeries, Self);
  except
    Chart.FNameType := ntGeneral;
  end;
   SetState(stUpdating);
end;

procedure TChartWriter.AddSeries(Values: TStringList; ATitle : string = '');
var
  i: integer;
  Crd: TSeriesItem;
  Ser: TSeries;
begin
  if Values.Count = 0 then
    ShowGWError(msg_DataEmpty);
  if not InState(stUpdating) and (FSeriesData.Count > 0) then
    ShowGWError(msg_ActiveSeries);
  if (Chart = nil) or (ActiveGraph = nil) then
    ShowGWError(msg_NoActiveGraph);

  FNumberInterval := 0;
  Ser := TSeries.Create;
  Ser.FWriter := Self;
  begin
    Ser.FMax := -MaxInt;
    Ser.FMin := MaxInt;
  end;
  for i := 0 to Values.Count - 1 do
  begin
    Crd := TSeriesItem.Create;
    Crd.FName := Values.Names[i];
    Crd.FOrigName := Crd.FName;
    Crd.FOwner := Ser;
    Crd.FItems := Ser.FSeriesItems;
    Crd.FWriter := Self;
    Crd.FLeapDummy := False;
    TryStrToDateTime(Crd.FName, Crd.FRealDate, Fmt);
    try
     Crd.FValue := StrToFloat(Values.Values[Crd.FName], Fmt);
    except
      Ser.Free;
      Crd.Free;
      Clear;
      raise;
    end;
    Crd.FRealVal := Crd.FValue;

    if Crd.FValue > Ser.FMax then
      Ser.FMax := Crd.FValue;
    if Crd.FValue < Ser.FMin then
      Ser.FMin := Crd.FValue;
    Ser.FSum := Ser.FSum + Crd.FValue;
    if Crd.FValue < 0 then
      Ser.FNegVals := True;
    Ser.FSeriesItems.Add(Crd);
  end;
  Ser.FLinkTitle := ATitle;
  FSeriesData.Add(Ser);
  Chart.FNameType := DetectNameType(FSeriesData.Count - 1);
  try
    if (Chart.NameType = ntNumberSpan) and (Ser.Count > 1) then
    begin
      CheckNumberEquality(Ser, Self);
    end
    else if (Chart.NameType = ntDateSpan) and (Ser.Count > 1) then
    begin
      CheckDateEquality(Ser, Self);
    end
    else if IsTimeSpan(Chart.NameType) and (Ser.Count > 1) then
    begin
      CheckTimeEquality(Ser, Self);
    end
    else if (Chart.NameType = ntGeneral) then
    begin
      CheckGeneralEquality(Ser, Self);
    end;

  except
    Chart.FNameType := ntGeneral;
  end;
  SetState(stUpdating);
end;

procedure TChartWriter.AddSeries(ASeries: TSeries; ATitle : string; AGraph : TCWGraph;
  AColor : TColor; AValueAxis : TValueAxisNumber);
begin
    Chart.AddSeriesDef(ATitle, AGraph, AColor, AValueAxis);
    AddSeries(ASeries, False);
end;

procedure TChartWriter.AddSeries(Values: TStringList; ATitle : string; AGraph : TCWGraph;
  AColor : TColor; AValueAxis : TValueAxisNumber);
begin
   Chart.AddSeriesDef(ATitle, AGraph, AColor, AValueAxis);
   AddSeries(Values);
end;

procedure TChartWriter.AddSeries(ADataset: TDataSet; ANameField, ValueFields : string;
  ATitle : string = '');
var
  NF, VF : TField;
  i: integer;
  Itm: TSeriesItem;
  Ser: TSeries;
  sl : TStringList;
  SerList : TList<Tseries>;
  SerCount : integer;
begin
  if (ADataset = nil) or (Trim(ANameField) = '') or (Trim(ValueFields) = '') then
    ShowGWError(msg_MissingParams);
  NF := ADataset.FieldByName(ANameField);
  sl := TStringList.Create;
  CreateListFromDelimiter(',', ValueFields, sl);
  SerList := TList<TSeries>.Create;
  SerCount := 0;
  try
    for I := 0 to sl.Count-1 do
    { Check data type and if exists}
    begin
      VF := ADataset.FieldByName(Trim(sl[i]));
      if not (VF.DataType in [ftFloat, ftSingle, ftExtended, ftInteger]) then
        ShowGWError(msg_ValueDataType);
      inc(SerCount);
    end;
    if not (NF.DataType in[ftString, ftDate, ftDateTime, ftInteger]) then
      ShowGWError(msg_NameDataType);

    if ADataset.RecordCount = 0 then
      ShowGWError(msg_DataEmpty);
    if not InState(stUpdating) and (FSeriesData.Count > 0) then
      ShowGWError(msg_ActiveSeries);
    if (Chart = nil) or (ActiveGraph = nil) then
      ShowGWError(msg_NoActiveGraph);

    FNumberInterval := 0;
    for I := 0 to SerCount-1 do
    begin
      Ser := TSeries.Create;
      Ser.FWriter := Self;
      Ser.FMax := -MaxInt;
      Ser.FMin := MaxInt;
      SerList.Add(Ser);
    end;

    ADataset.First;
    while not ADataset.Eof do
    begin
      for I := 0 to SerCount-1 do
      begin
        Itm := TSeriesItem.Create;
        if NF.DataType = ftInteger then
          Itm.FName := NF.AsString
        else if NF.DataType in [ftDate, ftDateTime] then
          Itm.FName := DateToStr(NF.AsDateTime, Fmt)
        else if NF.DataType = ftInteger then
          Itm.FName := NF.AsString;
        VF := ADataset.FieldByName(Trim(sl[i]));
        Itm.FValue := VF.AsSingle;
        Itm.FOrigName := Itm.FName;
        Itm.FOwner := SerList[i];
        Itm.FItems := SerList[i].FSeriesItems;
        Itm.FWriter := Self;
        Itm.FLeapDummy := False;
        TryStrToDateTime(Itm.FName, Itm.FRealDate, Fmt);
        Itm.FRealVal := Itm.FValue;

        if Itm.FValue > SerList[i].FMax then
          SerList[i].FMax := Itm.FValue;
        if Itm.FValue < SerList[i].FMin then
          SerList[i].FMin := Itm.FValue;
        SerList[i].FSum := SerList[i].FSum + Itm.FValue;
        if Itm.FValue < 0 then
          SerList[i].FNegVals := True;
        SerList[i].FSeriesItems.Add(Itm);
        SerList[i].FLinkTitle := ATitle;
      end;
      ADataSet.Next;
    end;

    for I := 0 to SerCount-1 do
    begin
       FSeriesData.Add(SerList[i]);
       Chart.FNameType := DetectNameType(FSeriesData.Count - 1);
       try
        if (Chart.NameType = ntNumberSpan) and (SerList[i].Count > 1) then
        begin
          CheckNumberEquality(SerList[i], Self);
        end
        else if (Chart.NameType = ntDateSpan) and (SerList[i].Count > 1) then
        begin
          CheckDateEquality(SerList[i], Self);
        end
        else if IsTimeSpan(Chart.NameType) and (SerList[i].Count > 1) then
        begin
          CheckTimeEquality(SerList[i], Self);
        end
        else if (Chart.NameType = ntGeneral) then
        begin
          CheckGeneralEquality(SerList[i], Self);
        end;
       except
        Chart.FNameType := ntGeneral;
     end;
     SetState(stUpdating);
    end;
  finally
    sl.Free;
    SerList.Free;
  end;

end;

procedure TChartWriter.MakeTimeSpan(ANameType: TNameType);
  procedure MakeEqual;
  var
    Lowest: TDateTime;
    Highest: TDateTime;
    i, j: integer;
    SerItm: TSeriesItem;
    CompRate: integer;
    S: string;
    H: TDateTime;
    H2: integer;

    function GetTime(AnIndex: integer): string;
    var
      i: integer;
    begin
      Result := '';
      for i := 0 to FSeriesData.Count - 1 do
      begin
        if AnIndex >= FSeriesData[i].FSeriesItems.Count then
        begin
          Exit;
        end;

        if FSeriesData[i].FSeriesItems[AnIndex].FVisible then
        begin
          Result := FSeriesData[i].FSeriesItems[AnIndex].FName;
          Break;
        end;
      end;
    end;

  begin
    { Make all series equal length }

    { First, get the Contraction rate }
    CompRate := GetContraction;

    { Find the "leftmost" Series }

    Lowest := EncodeDate(2050, 1, 1);
    for i := 0 to FSeriesData.Count - 1 do
    begin
      H := StrToDateTime(FSeriesData[i].FSeriesItems[0].FName, Fmt);
      if H < Lowest then
      begin
        Lowest := H;
      end;
    end;

    { Set indentation for Series that start later }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      H := StrToDateTime(FSeriesData[i].FSeriesItems[0].FName, Fmt);
      if H > Lowest then
      begin
        if ANameType = ntHourSpan then
          H2 := HoursBetween(Lowest, H)
        else if ANameType = ntMinuteSpan then
          H2 := MinutesBetween(Lowest, H)
        else
          H2 := SecondsBetween(Lowest, H);
        FSeriesData[i].FIndent := H2 div CompRate;
      end;
    end;

    { Find the "rightmost" Series }
    Highest := 0;
    for i := 0 to FSeriesData.Count - 1 do
    begin
      H := StrToDateTime(FSeriesData[i].FSeriesItems[FSeriesData[i].Count - 1].FName, Fmt);
      if H > Highest then
      begin
        Highest := H;
      end;
    end;

    { Set "exdentation" for Series that ends earlier }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      H := StrToDateTime(FSeriesData[i].FSeriesItems[FSeriesData[i].Count - 1].FName, Fmt);
      if H < Highest then
      begin
        if ANameType = ntHourSpan then
          H2 := HoursBetween(Highest, H)
        else if ANameType = ntMinuteSpan then
          H2 := MinutesBetween(Highest, H)
        else
          H2 := SecondsBetween(Highest, H);
        FSeriesData[i].FExdent := H2 div CompRate;
      end;
    end;

    for i := 0 to FSeriesData.Count - 1 do
    begin
      for j := 1 to FSeriesData[i].FIndent do
      begin
        SerItm := TSeriesItem.Create;
        SerItm.FVisible := False;
        FSeriesData[i].FSeriesItems.Insert(0, SerItm);
      end;
      for j := 1 to FSeriesData[i].FExdent do
      begin
        SerItm := FSeriesData[i].AddItem;
        SerItm.FVisible := False;
      end;
    end;

    { Sync indented hours }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if FSeriesData[i].FIndent = 0 then
        Continue;
      for j := 0 to FSeriesData[i].Count - 1 do
      begin
        if not FSeriesData[i].FSeriesItems[j].FVisible then
        begin
          try
            SerItm := FSeriesData[i].FSeriesItems[j];
            S := GetTime(j);
            if S <> '' then
              SerItm.FName := S;
          except
            raise;
          end;
        end
        else
          Break;

      end;
    end;

    { Sync Exdented hours }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if FSeriesData[i].FExdent = 0 then
        Continue;
      for j := FSeriesData[i].Count - 1 downto 0 do
      begin
        if not FSeriesData[i].FSeriesItems[j].FVisible then
        begin
          SerItm := FSeriesData[i].FSeriesItems[j];
          S := GetTime(j);
          if S <> '' then
            SerItm.FName := S;
        end
        else
          Break;
      end;
    end;
  end;

begin
  MakeEqual;
end;

procedure TChartWriter.MakeDateSpan;
var
  Y, M, D: word;
  procedure MakeEqual;
  var
    DP: string;
    LowestDP: string;
    Lowest, Highest: TDateTime;
    LowestSer: integer;
    i, j: integer;
    Dt: TDateTime;
    SerItm: TSeriesItem;
    LeadingYear: word;
    FirstYear, LogYear: word;
    n: integer;
    LeapY: word;
    LeapSer: integer;
    CompRate: integer;
    YMap: TYearMap;

    function YMDToStr(Y, M, D: word): string;
    var
      Dt: TDate;
    begin
      Dt := EncodeDate(Y, M, D);
      Result := DateToStr(Dt, Fmt);
    end;

  begin
    { Find the Series with the lowest date part. Use the year of this as the logical year.
      If the span contains leapyears, the leapyear must be the leading year, else ComputePoints
      will not work correctly }

    Finalize(FLogYearMap);
    { Used to dedtect correct year in time spanned sections }
    setlength(FLogYearMap, 1);
    LowestDP := '1231';
    LowestSer := 0;
    LeapSer := -1;
    LeapY := 0;
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := StrToDate(FSeriesData[i].FSeriesItems[0].FName, Fmt);
      DecodeDate(Dt, Y, M, D);
      if IsLeapYear(Y) then
      begin
        LeapY := Y;
        LeapSer := i;
      end;
      DP := ExpandDM(M) + ExpandDM(D);
      if DP < LowestDP then
      begin
        LowestSer := i;
        LowestDP := DP;
      end;
    end;
    Dt := FSeriesData[LowestSer].ToDate(0);
    DecodeDate(Dt, Y, M, D);
    if LeapSer <> -1 then
    begin
      LeadingYear := LeapY;
      LowestSer := LeapSer;
    end
    else
      LeadingYear := Y;

    YMap.RealYear := Y;
    YMap.LogYear := LeadingYear;
    FLogYearMap[0] := YMap;

    { Map the other series acording to leading year }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := FSeriesData[i].ToDate(0);
      DecodeDate(Dt, Y, M, D);
      FirstYear := Y;
      LogYear := LeadingYear;
      for j := 0 to FSeriesData[i].Count - 1 do
      begin
        Dt := StrToDate(FSeriesData[i].FSeriesItems[j].FName, Fmt);
        if i = LowestSer then
        begin
          Continue;
        end;

        DecodeDate(Dt, Y, M, D);
        SerItm := FSeriesData[i].FSeriesItems[j];
        if Y > FirstYear then
        begin
          FirstYear := Y;
          inc(LogYear);
          setlength(FLogYearMap, Length(FLogYearMap) + 1);
          YMap.RealYear := Y;
          YMap.LogYear := LogYear;
          FLogYearMap[High(FLogYearMap)] := YMap;
        end;
        SerItm.FName := YMDToStr(LogYear, M, D);
      end;
    end;

    { Make all series equal length }

    { First, get the Contraction rate }
    CompRate := GetContraction;

    { Find the "leftmost" Series }

    Lowest := EncodeDate(2050, 1, 1);
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := StrToDate(FSeriesData[i].FSeriesItems[0].FName, Fmt);
      if Dt < Lowest then
      begin
        Lowest := Dt;
      end;
    end;

    { Set indentation for Series that start later }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := StrToDate(FSeriesData[i].FSeriesItems[0].FName, Fmt);
      if Dt > Lowest then
      begin
        n := DaysBetween(Dt, Lowest);
        FSeriesData[i].FIndent := n div CompRate;
      end;
    end;

    { Find the "rightmost" Series }
    Highest := 0;
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := StrToDate(FSeriesData[i].FSeriesItems[FSeriesData[i].Count - 1].FName, Fmt);
      if Dt > Highest then
      begin
        Highest := Dt;
      end;
    end;

    { Set "exdentation" for Series that ends earlier }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := StrToDate(FSeriesData[i].FSeriesItems[FSeriesData[i].Count - 1].FName, Fmt);
      if Dt < Highest then
      begin
        n := DaysBetween(Highest, Dt);
        FSeriesData[i].FExdent := n div CompRate;
        if n mod CompRate <> 0 then
          inc(FSeriesData[i].FExdent);
      end;
    end;

    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := StrToDate(FSeriesData[i].FSeriesItems[0].FName, Fmt);
      for j := 1 to FSeriesData[i].FIndent do
      begin
        SerItm := TSeriesItem.Create;
        SerItm.FVisible := False;
        SerItm.FOwner := FSeriesData[i];
        Dt := Dt - CompRate;
        SerItm.FName := DateToStr(Dt, Fmt);
        FSeriesData[i].FSeriesItems.Insert(0, SerItm);
      end;
      Dt := StrToDate(FSeriesData[i].FSeriesItems[FSeriesData[i].FSeriesItems.Count - 1]
        .FName, Fmt);
      for j := 1 to FSeriesData[i].FExdent do
      begin
        SerItm := FSeriesData[i].AddItem;
        Dt := Dt + CompRate;
        SerItm.FName := DateToStr(Dt, Fmt);
        SerItm.FVisible := False;
      end;
    end;
  end;

begin
  MakeEqual;
end;

procedure TChartWriter.MakeNumberSpan;

  procedure MakeEqual;
  var
    Lowest, Highest: integer;
    i, j: integer;
    SerItm: TSeriesItem;
    n, N2: integer;
    CompRate: integer;
    S: string;

    function GetNumber(AnIndex: integer): string;
    var
      i: integer;
    begin
      Result := '';
      for i := 0 to FSeriesData.Count - 1 do
      begin
        if AnIndex >= FSeriesData[i].FSeriesItems.Count then
        begin
          Exit;
        end;

        if FSeriesData[i].FSeriesItems[AnIndex].FVisible then
        begin
          Result := FSeriesData[i].FSeriesItems[AnIndex].FName;
          Break;
        end;
      end;
    end;

  begin
    { Make all series equal length }

    { First, get the Contraction rate }
    CompRate := GetContraction;

    { Find the "leftmost" Series }

    Lowest := MaxInt;
    for i := 0 to FSeriesData.Count - 1 do
    begin
      n := StrToInt(FSeriesData[i].FSeriesItems[0].FName);
      if n < Lowest then
      begin
        Lowest := n;
      end;
    end;

    { Set indentation for Series that start later }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      n := StrToInt(FSeriesData[i].FSeriesItems[0].FName);
      if n > Lowest then
      begin
        N2 := (n - Lowest);
        FSeriesData[i].FIndent := N2 div CompRate;
      end;
    end;

    { Find the "rightmost" Series }
    Highest := 0;
    for i := 0 to FSeriesData.Count - 1 do
    begin
      n := StrToInt(FSeriesData[i].FSeriesItems[FSeriesData[i].Count - 1].FName);
      if n > Highest then
      begin
        Highest := n;
      end;
    end;

    { Set "exdentation" for Series that ends earlier }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      n := StrToInt(FSeriesData[i].FSeriesItems[FSeriesData[i].Count - 1].FName);
      if n < Highest then
      begin
        N2 := (Highest - n);
        FSeriesData[i].FExdent := N2 div CompRate;
      end;
    end;

    for i := 0 to FSeriesData.Count - 1 do
    begin
      for j := 1 to FSeriesData[i].FIndent do
      begin
        SerItm := TSeriesItem.Create;
        SerItm.FVisible := False;
        FSeriesData[i].FSeriesItems.Insert(0, SerItm);
      end;
      for j := 1 to FSeriesData[i].FExdent do
      begin
        SerItm := FSeriesData[i].AddItem;
        SerItm.FVisible := False;
      end;
    end;

    { Sync indented dates }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if FSeriesData[i].FIndent = 0 then
        Continue;
      for j := 0 to FSeriesData[i].Count - 1 do
      begin
        if not FSeriesData[i].FSeriesItems[j].FVisible then
        begin
          try
            SerItm := FSeriesData[i].FSeriesItems[j];
            S := GetNumber(j);
            if S <> '' then
              SerItm.FName := S;
          except
            raise;
          end;
        end
        else
          Break;

      end;
    end;

    { Sync Exdented dates }
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if FSeriesData[i].FExdent = 0 then
        Continue;
      for j := FSeriesData[i].Count - 1 downto 0 do
      begin
        if not FSeriesData[i].FSeriesItems[j].FVisible then
        begin
          SerItm := FSeriesData[i].FSeriesItems[j];
          S := GetNumber(j);
          if S <> '' then
            SerItm.FName := S;
        end
        else
          Break;

      end;
    end;
  end;

begin
  MakeEqual;
end;

procedure TChartWriter.CreateXValues;
var
  LSer: TSeries;
  Ser, OrigSer: TSeries;
  i, j: integer;
  SerItm: TSeriesItem;

  procedure Check(ASeries: TSeries);
  var
    i, j: integer;
  begin
    if IsTimeSpan(Chart.NameType) or (ActiveGraph is TCWPie) then
      Exit;

    for i := ASeries.FirstItem to ASeries.LastItem do
    begin
      for j := 0 to FSeriesData.Count - 1 do
      begin
        if FSeriesData[j] = ASeries then
          Continue;
        if FSeriesData[j].LastItem >= i then
        begin
          if not SameText(ASeries.FSeriesItems[i].FName,
            FSeriesData[j].FSeriesItems[i].FName) then
          begin
            ShowGWError(msg_NameMismatch, '(' + ASeries.FSeriesItems[i].FName +
              ', ' + FSeriesData[j].FSeriesItems[i].FName + ')');
          end;
        end;
      end;
    end;
  end;

  procedure CheckDups(ASeries: TSeries; AnItem: TSeriesItem; Index: integer);
  var
    i: integer;
  begin
    if InState(stInternalAction) then
      Exit;
    for i := ASeries.FirstItem to ASeries.LastItem do
    begin
      if i = index then
        Continue;
      if SameText(ASeries.SeriesItems[i].FName, AnItem.FName) and
        not(AnItem.FLeapDummy or ASeries.SeriesItems[i].FLeapDummy) then
        ShowGWError(msg_DupName, '(' + AnItem.FName + ')');
    end;
  end;

begin
  if (Chart.NameType = ntDateSpan) then
    MakeDateSpan
  else if IsTimeSpan(Chart.NameType) then
    MakeTimeSpan(Chart.NameType)
  else if (Chart.NameType = ntNumberSpan) then
    MakeNumberSpan
  else
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Ser := FSeriesData[i];
      for j := Ser.FirstItem to Ser.LastItem do
      begin
        SerItm := Ser.FSeriesItems[j];
        SerItm.FLeapDummy := False;
      end;
    end;

  { Check consistency }
  for i := 0 to FSeriesData.Count - 1 do
  begin
    Ser := FSeriesData[i];
    Check(FSeriesData[i]);
    for j := Ser.FirstItem to Ser.LastItem do
    begin
      if Trim(Ser.SeriesItems[j].FName) = '' then
        ShowGWError(msg_NameEmpty);
      CheckDups(Ser, Ser.SeriesItems[j], j);
    end;
  end;

  { Add the names }
  FNameList.Clear;
  LSer := FLongestSeries;
  for i := 0 to LSer.FSeriesItems.Count - 1 do
  begin
     FNameList.Add(LSer.FSeriesItems[i].FName);
  end;

  if FOrigData.Count = 0 then
    for i := 0 to FSeriesData.Count - 1 do
    begin
      OrigSer := TSeries.Create;
      OrigSer.Assign(FSeriesData[i], -1, -1);
      FOrigData.Add(OrigSer);
    end;
end;

procedure TChartWriter.MoveSeries(FromIndex, ToIndex: integer);
begin
  if (ActiveGraph is TCWPie) then
    Exit;
  if Chart = nil then
    Exit;
  FSeriesData.Move(FromIndex, ToIndex);
  Chart.SeriesDefs.Items[FromIndex].Index := ToIndex;
  DoRepaint;
end;

function TChartWriter.LongestSeries: TSeries;
var
  i: integer;
  Y, M, D: word;
  Dt: TDateTime;
  Longest: integer;
  n: integer;
  Itm: TSeriesItem;

begin
  Result := nil;
  if FSeriesData.Count = 0 then
    Exit;
  if (Chart.NameType in [ntGeneral]) then
  begin
    Result := FSeriesData[0];
    Exit;
  end;
  Longest := 0;

  if IsTimeSpan(Chart.NameType) then
  begin
    Dt := FSeriesData[0].ToDate(FSeriesData[0].LastItem);
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if i > 0 then
      begin
        if FSeriesData[i].ToDate(FSeriesData[i].LastItem) > Dt then
        begin
          Longest := i;
          Dt := FSeriesData[i].ToDate(FSeriesData[i].LastItem);
        end;
      end;
      DecodeDate(FSeriesData[i].ToDate(0), Y, M, D);
      if IsLeapYear(Y) then { Leap years must come first }
      begin
        Result := FSeriesData[i];
        Break;
      end;
    end;
  end
  else
  begin
    { Number span }
    Itm := FSeriesData[0].FSeriesItems[FSeriesData[0].LastItem];
    n := StrToInt(Itm.FName);
    Result := FSeriesData[0];
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if i > 0 then
      begin
        Itm := FSeriesData[i].FSeriesItems[FSeriesData[i].LastItem];
        if StrToInt(Itm.FName) > n then
        begin
          Result := FSeriesData[i];
          n := StrToInt(Itm.FName);
        end;
      end;
    end;
  end;
  if Result = nil then
  begin
    Result := FSeriesData[Longest];
  end;
end;

procedure TChartWriter.DeleteSection(OnAxis: TAxisType; Index: integer);
begin

  if OnAxis = atNameAxis then
  begin
    if FNameSections[Index].FIsAuto then
      ShowGWError(msg_DelAutoSect);
    FNameSections.Delete(Index);
  end
  else
    FValueSections.Delete(Index);
  Invalidate;
end;

procedure TChartWriter.CreateSections;
begin
  if (NameSectionDefs <> nil) and NameSectionDefs.Visible and
    ((NameSectionDefs.Sections.Count > 0) or
    (NameSectionDefs.SectionType = stAutoSections)) then
    CreateNameSections;
  if (ValueSectionDefs <> nil) and ValueSectionDefs.Visible and
    (ValueSectionDefs.Sections.Count > 0) and (Chart.AxisCount = 1) then
    CreateValueSections;
end;

procedure TChartWriter.CreateValueSections;
var
  i: integer;
  Itms: TCWSectionItems;
  StartVal, EndVal: string;
  SectType: TSectionType;
begin
  if Chart = nil then
    Exit;
  if not ValueSectionDefs.Visible or (Chart.AxisCount > 1)  then
    Exit;
  try
    ValueSectionDefs.DoCheck;
  except
    raise;
  end;
  ClearSections(atValueAxis, False);
  Itms := ValueSectionDefs.Sections;
  for i := 0 to Itms.Count - 1 do
  begin
    StartVal := Itms.Items[i].FStartValue;
    EndVal := Itms.Items[i].FEndValue;
    if StartVal = EndVal then
      SectType := stLine
    else
      SectType := stSection;
    AddSection(atValueAxis, StartVal, EndVal, Itms.Items[i].FLongCaption,
      Itms.Items[i].FShortCaption, SectType);
  end;
end;

procedure TChartWriter.CreateNameSections;
var
  N1, N2: integer;
  LastEnd: TDateTime;
  Indx2: integer;
  Templ: TDateTimeTemplate;
  Dt1, Dt2: TDateTime;
  i: integer;
  StartWith: integer;
  AStart, AEnd: string;
  Found: Boolean;
  Interval, Space: integer;
  Year1, LastYear, LastMonth, LastWeek, LastDay: word;
  LastHour, LastMinute, LastSecond, LastMS: word;
  First: Boolean;
  Decoder: TDateTime;
  Itms: TCWSectionItems;
  LCap, SCap: string;
  FirstDt, LastDt: TDateTime;

  procedure DoError(Code: integer; Text: string = '');
  begin
    ClearSections(atNameAxis, True);
    ShowGWError(Code, Text);
  end;

  procedure DoDecode(ADt: TDateTime);
  begin
    DecodeDateTime(ADt, LastYear, LastMonth, LastDay, LastHour, LastMinute,
      LastSecond, LastMS);
  end;

  function InSpan(ADt: TDateTime): Boolean;
  begin
    Result := (ADt >= FirstDt) and (ADt <= LastDt);
  end;

  function InSections(AStart, AEnd: TDateTime): Boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to NameSectionCount - 1 do
    begin
      if AEnd > LastDt then
        Result := (AStart = StrToDateTime(NameSections[i].StartVal, Fmt))
      else if AStart < FirstDt then
        Result := (AEnd = StrToDateTime(NameSections[i].EndVal, Fmt))
      else
        Result := (AStart = StrToDateTime(NameSections[i].StartVal, Fmt)) and
          (AEnd = StrToDateTime(NameSections[i].EndVal, Fmt));
      if Result then
        Break;
    end;
  end;

  procedure AddToDateTime(var Dt1, Dt2: TDateTime);
  begin
    case Templ of
      ttMonthTemplate, ttWeekTemplate, ttDateTemplate:
        begin
          Dt1 := IncYear(Dt1);
          Dt2 := IncYear(Dt2);
        end;
      ttHourTemplate:
        begin
          Dt1 := Dt1 + 1;
          Dt2 := Dt2 + 1;
        end;
      ttMinuteTemplate:
        begin
          Dt1 := IncHour(Dt1);
          Dt2 := IncHour(Dt2);
        end;
      ttSecondTemplate:
        begin
          Dt1 := IncMinute(Dt1);
          Dt2 := IncMinute(Dt2);
        end;
    end;
  end;

  function SetTimes(Dt1, Dt2: TDateTime): integer;
  var
    SectType: TSectionType;
  begin
    begin
      Result := 0;
      if InSections(Dt1, Dt2) or (not InSpan(Dt1) and not InSpan(Dt2)) then
        while (Dt1 < LastDt) do
        begin
          AddToDateTime(Dt1, Dt2);
          if (InSpan(Dt1) or InSpan(Dt2)) and not InSections(Dt1, Dt2) then
          begin
            Break;
          end;
        end;
      if (not InSpan(Dt1) and not InSpan(Dt2)) then
      begin
        Exit;
      end;
    end;

    if not InSpan(Dt1) then
    begin
      AStart := FNameList[0];
      AEnd := DateTimeToStr(Dt2, Fmt);
      Result := Indx2 + 1;
      Found := True;
    end
    else if not InSpan(Dt2) then
    begin
      AStart := DateTimeToStr(Dt1, Fmt);
      AEnd := FNameList[FNameList.Count - 1];
      Result := FNameList.Count - 1;
      Found := True;
    end
    else
    begin
      AStart := DateTimeToStr(Dt1, Fmt);
      AEnd := DateTimeToStr(Dt2, Fmt);
      if Dt1 > Dt2 then
      begin
        AStart := FNameList[0];
      end;
      Result := Indx2 + 1;
      Found := True;
    end;
    SectType := stSection;
    if Interval = 0 then
      SectType := stLine;
    AddSection(atNameAxis, AStart, AEnd, LCap, SCap, SectType);
  end;

  procedure SetSpace(Last, Start, Max: integer);
  begin
    if not First then
    begin
      if Last > Start then
        Space := Max - (Last - Start)
      else
        Space := Start - Last;
    end;
  end;

  procedure CreateLiteralSections;
  var
    i: integer;
    Itms: TCWSectionItems;
    Indx1, Indx2: integer;
    StartVal, EndVal: string;
    SectType: TSectionType;
  begin
    Itms := NameSectionDefs.Sections;
    for i := 0 to Itms.Count - 1 do
    begin
      StartVal := Itms.Items[i].FStartValue;
      EndVal := Itms.Items[i].FEndValue;
      if Chart.NameType <> ntGeneral then
      begin
        Indx1 := IndexOfNearestName(StartVal, StartWith);
        Indx2 := IndexOfNearestName(EndVal, StartWith);
        if (Indx1 = -1) and (Indx2 = -1) then
          Continue;
        if Indx1 = -1 then
          StartVal := FNameList[0]
        else if Indx2 = -1 then
          EndVal := FNameList[FNameList.Count - 1]
        else
        begin
          StartVal := FNameList[Indx1];
          EndVal := FNameList[Indx2];
        end;
        inc(StartWith);
      end
      else
      begin
        if (FNameList.IndexOf(StartVal) = -1) or (FNameList.IndexOf(EndVal) = -1)
        then
          Continue;
      end;
      if StartVal = EndVal then
        SectType := stLine
      else
        SectType := stSection;

      AddSection(atNameAxis, StartVal, EndVal, Itms.Items[i].FLongCaption,
        Itms.Items[i].FShortCaption, SectType);
    end;
  end;

{
  Not used so far, dont realy see the usage of this
  procedure RepeatIntervals(Templ : TDateTimeTemplate);
  var
  i : integer;
  SVal, EVal : string;
  SDt, Edt, LDt : TDateTime;
  Num : integer;
  Itm : TCWSectionItem;
  begin
  Itm := NameSectionDefs.Sections.Items[0];
  SDt := StrToDateTime(FNameList[0], Fmt);
  EDt := Sdt;
  Ldt := StrToDateTime(FNameList[FNameList.Count-1]);
  Num := 0;
  while Edt < Ldt do
  begin
  case Templ of
  ttMonthTemplate: Edt := IncMonth(Edt,N1);
  ttWeekTemplate: Edt := IncWeek(Edt,N1);
  ttDateTemplate: Edt := Edt + N1;
  ttHourTemplate: Edt := IncHour(Edt,N1);
  ttMinuteTemplate: Edt := IncMinute(Edt,N1);
  ttSecondTemplate: Edt := IncSecond(Edt,N1);
  end;
  SVal := DateTimeToStr(SDt, Fmt);
  EVal := DateTimeToStr(EDt, Fmt);
  Inc(Num);
  AddSection(atNameAxis, SVal, EVal, Itm.LongCaption + ' ' + IntToStr(Num),
  Itm.ShortCaption + ' ' + IntToStr(Num), stSection);
  Sdt := EDt;
  end;
  end;
}

begin
  if not NameSectionDefs.Visible or (Chart = nil) then
    Exit;
  ClearSections(atNameAxis, False);
  try
    NameSectionDefs.DoCheck;
    NameSectionDefs.CheckConfig;
    StartWith := 0;
    if NameSectionDefs.SectionType = stLiterals then
    begin
     CreateLiteralSections;
     Exit;
    end
    else if NameSectionDefs.SectionType = stAutoSections then
    begin
      CreateAutoSections;
      Exit;
    end;
    Templ := NameSectionDefs.DateTimeTemplate;
    if Templ = ttNotUsed then
      Exit;
    LastEnd := StrToDateTime(FNameList[0], Fmt);
    FirstDt := LastEnd;
    LastDt := StrToDateTime(FNameList[FNameList.Count - 1], Fmt);
    DecodeDateTime(LastEnd, LastYear, LastMonth, LastDay, LastHour, LastMinute,
      LastSecond, LastMS);
    Year1 := LastYear;
    First := True;
    Space := 0;
    Itms := NameSectionDefs.Sections;
    repeat
      Found := False;
      for i := 0 to Itms.Count - 1 do
      begin
        AStart := Itms.Items[i].StartValue;
        AEnd := Itms.Items[i].EndValue;
        LCap := Itms.Items[i].LongCaption;
        SCap := Itms.Items[i].ShortCaption;

        if not TryStrToInt(AStart, N1) or not TryStrToInt(AEnd, N2) then
        begin
          DoError(-1, 'Invalid section value');
        end;

        { if (N2 = -1) and (I = 0) then
          If impelented, this should have a property in the sectiondefs
          like PeriodSpan
          begin
          RepeatIntervals(Templ);
          Break;
          end; }

        Dt1 := ToDay;
        Dt2 := ToDay;

        case Templ of
          ttMonthTemplate:
            begin
              if N1 > N2 then
                Interval := 12 - (N1 - N2)
              else
                Interval := N2 - N1;
              SetSpace(LastMonth, N1, 12);
              if LastMonth + Interval + Space > 12 then
                inc(Year1);

              Dt2 := EncodeDate(Year1, N2, 1);
              if N2 - Interval < 0 then
                Interval := 12 + (N2 - N1);
              Dt1 := IncMonth(Dt2, -Interval);
            end;
          ttWeekTemplate:
            begin
              LastWeek := WeekOf(LastEnd);
              if N1 > N2 then
                Interval := WeeksInAyear(Year1) - (N1 - N2)
              else
                Interval := N2 - N1;
              SetSpace(LastWeek, N1, WeeksInAyear(Year1));
              if LastWeek + Interval + Space > WeeksInAyear(Year1) then
                inc(Year1);

              Dt2 := EncodeDateWeek(Year1, N2, 1);
              Dt1 := IncWeek(Dt2, -Interval);
            end;
          ttDateTemplate:
            begin
              if N1 > N2 then
                Interval := DaysInAYear(Year1) - (N1 - N2)
              else
                Interval := N2 - N1;
              SetSpace(LastDay, N1, DaysInAYear(Year1));
              if LastDay + Interval + Space > DaysInAYear(Year1) then
                inc(Year1);

              Dt2 := EncodeDateDay(Year1, N2);
              Dt1 := IncDay(Dt2, -Interval);
            end;
          ttHourTemplate:
            begin
              if N1 > N2 then
                Interval := 24 - (N1 - N2)
              else
                Interval := N2 - N1;
              SetSpace(LastHour, N1, 24);
              if LastHour + Interval + Space >= 24 then
              begin
                Decoder := IncDay(LastEnd);
                DoDecode(Decoder);
              end;

              Dt2 := EncodeDateTime(LastYear, LastMonth, LastDay, N2, 0, 0, 0);
              Dt1 := IncHour(Dt2, -Interval);
            end;
          ttMinuteTemplate:
            begin
              if N1 > N2 then
                Interval := 60 - (N1 - N2)
              else
                Interval := N2 - N1;
              SetSpace(LastMinute, N1, 60);
              if LastMinute + Interval + Space >= 60 then
              begin
                Decoder := IncHour(LastEnd);
                DoDecode(Decoder);
              end;
              Dt2 := EncodeDateTime(LastYear, LastMonth, LastDay, LastHour,
                N2, 0, 0);
              Dt1 := IncMinute(Dt2, -Interval);
            end;
          ttSecondTemplate:
            begin
              if N1 > N2 then
                Interval := 60 - (N1 - N2)
              else
                Interval := N2 - N1;
              SetSpace(LastSecond, N1, 60);
              if LastSecond + Interval + Space >= 60 then
              begin
                Decoder := IncMinute(LastEnd);
                DoDecode(Decoder);
              end;
              Dt2 := EncodeDateTime(LastYear, LastMonth, LastDay, LastHour,
                LastMinute, N2, 0);
              Dt1 := IncSecond(Dt2, -Interval);
            end;
        end;
        SetTimes(Dt1, Dt2);
        First := False;
      end; { Section loop }
    until not Found;
  except
    if Chart.NameSectionDefs.DateTimeTemplate <> ttNotUsed then
      Chart.NameSectionDefs.FDateTimeTemplate := ttNotUsed;
    if Chart.NameSectionDefs.AutoSections <> autNotUsed then
      Chart.NameSectionDefs.FAutoSections := autNotUsed;
    raise;
  end;
end;

function TChartWriter.AddSection(OnAxis: TAxisType;
AStart, AEnd, LongCaption, ShortCaption: string; SectionType: TSectionType)
  : TSection;
var
  Sec: TSection;
  E1, E2: single;
  Dt1, Dt2: TDateTime;
  N1, N2: single;
  y1, y2, YY1, YY2: word;
  Y, M, D: word;
  NewDt: TDate;

  procedure DoError(Code: integer; Text: string = '');
  begin
    ClearSections(atNameAxis, True);
    GoBackError;
    ShowGWError(Code, Text);
  end;

  function DoAdd(Reduced: Boolean): TSection;
  begin
    Sec := TSection.Create;
    Sec.FIsReduced := Reduced;
    Sec.FWriter := Self;
    if OnAxis = atNameAxis then
    begin
      Sec.FOwner := FNamAx;
      Sec.FIndex := NameSectionCount
    end
    else
    begin
      Sec.FOwner := FValAx;
      Sec.FIndex := ValueSectionCount
    end;
    Sec.FStartVal := AStart;
    Sec.FEndVal := AEnd;
    Sec.FLongCaption := LongCaption;
    Sec.FShortCaption := ShortCaption;
    Sec.FSectionType := SectionType;
    if OnAxis = atNameAxis then
    begin
      Sec.FIndex := FNameSections.Count;
      FNameSections.Add(Sec);
    end
    else
    begin
      Sec.FIndex := FValueSections.Count;
      FValueSections.Add(Sec);
    end;
    Result := Sec;
  end;

begin
  if OnAxis = atValueAxis then
  begin
    E1 := StrToFloat(AStart, Fmt);
    E2 := StrToFloat(AEnd, Fmt);
    if E1 > E2 then
      ShowGWError(msg_StartEndValue);
  end
  else
  begin
    if IsTimeSpan(Chart.NameType) then
    begin
      if not TryStrToDateTime(AStart, Dt1, Fmt) then
      begin
        If not TryStrToDateTime(AEnd, Dt2, Fmt) then
        begin
          Result := DoAdd(True);
          Exit;
        end;
      end
      else
        try
          Dt2 := StrToDateTime(AEnd, Fmt);
        except
          ClearSections(atNameAxis, True);
          GoBackError;
          raise;
        end;
      if (Dt1 > Dt2) then
      begin
        DoError(msg_StartDateEndDate);
      end;
      { Convert from real year to log year }
      if Length(FLogYearMap) > 1 then
      begin
        y1 := YearOf(Dt1);
        y2 := YearOf(Dt2);
        YY1 := RealYearToLogYear(y1);
        YY2 := RealYearToLogYear(y2);
        if YY1 <> y1 then
        begin
          DecodeDate(Dt1, Y, M, D);
          NewDt := EncodeDate(YY1, M, D);
          ReplaceDate(Dt1, NewDt);
          AStart := DateTimeToStr(Dt1, Fmt);
        end;
        if YY2 <> y2 then
        begin
          DecodeDate(Dt2, Y, M, D);
          NewDt := EncodeDate(YY2, M, D);
          ReplaceDate(Dt2, NewDt);
          AEnd := DateTimeToStr(Dt2, Fmt);
        end;
      end;
    end
    else if Chart.NameType = ntNumberSpan then
    begin
      N1 := StrToFloat(AStart, Fmt);
      N2 := StrToFloat(AEnd, Fmt);
      if N1 > N2 then
        ShowGWError(msg_StartNumEndNum);
    end;

  end;
  Result := DoAdd(False);
  Invalidate;
end;

procedure TChartWriter.ClearSections(OnAxis: TAxisType; Both: Boolean);
begin
  if (OnAxis = atNameAxis) or Both then
    FNameSections.Clear;
  if (OnAxis = atValueAxis) or Both then
    FValueSections.Clear;
end;

function TChartWriter.SpaceOf(Position: TAxisPosition): integer;
var
  Ax: TAxisObject;
begin
  Result := 0;
  Ax := AxisOf(Position);
  if Ax <> nil then
  begin
    if Ax.OpposedSections then
      Result := Ax.LabelSpace
    else
      Result := Ax.SectionSpace + Ax.LabelSpace;
    if Position in [apLeft, apBottom] then
      Result := Result + Ax.GetWallWidth
    else if Position = apTop then
      Result := Result + GetTitleSpace;

  end
  else
  begin
    if Position = apBottom then
      Ax := AxisOf(apTop)
    else if Position = apRight then
      Ax := AxisOf(apLeft)
    else if Position = apLeft then
      Ax := AxisOf(apRight)
    else if Position = apTop then
      Ax := AxisOf(apBottom);
    if Ax <> nil then
    begin
      if Ax.OpposedSections then
      begin
        Result := Ax.SectionSpace;
        if Position in [apLeft, apBottom] then
          Result := Result + Ax.GetWallWidth
      end;
    end;
    if Position = apTop then
      Result := Result + GetTitleSpace;
  end;

  if Chart <> nil then
  begin
     Result := Result + Chart.Legends.WidestLegend(Position);
  end;
end;

function TChartWriter.GetWorkRect: TRect;
var
  D, H, W : integer;
begin
  Result := ClientRect;
  Result.Left := InnerMargins.Left;
  Result.Top := InnerMargins.Top;
  Result.Right := Result.Right - FRightSpace;
  Result.Bottom := Result.Bottom - FBottomSpace;
  if Centered then
  begin
    H := Result.Height;
    W := Result.Width;
    D := FRightSpace div 2;
    Result.Left := Result.Left+ D;
    Result.Width := W;
    D := FBottomSpace div 2;
    Result.Top := Result.Top+ D;
    Result.Height := H;
  end;
end;

function TChartWriter.GetNameCount: integer;
begin
  Result := FNameList.Count;
end;

function TChartWriter.GetNames(Index: integer): string;
begin
  Result := FNameList[Index];
end;

function TChartWriter.GetViewMode: TViewMode;
begin
  Result := FViewMode;
end;

function TChartWriter.GetNameFloatUnit: single;
begin
  if FNamAx.IsXAxis then
  begin
    Result := GetPosRect.Width / (NameCount - 1)
  end
  else
  begin
    Result := GetPosRect.Height / (NameCount - 1);
  end;
end;

function TChartWriter.GetNameLabelSpace: integer;
var
  W, H: integer;
begin
  if not(veNameLabels in AxisElements) then
  begin
    Result := Canvas.TextHeight('X') div 2 + 1;
    Exit;
  end;
  Result := FNameLabelSpace;
  if Count = 0 then
    Exit;
  if FNamAx.IsXAxis then
  begin
    H := GetTextHeight(lkName);
    if H <> 0 then
    begin
      Result := GetTextHeight(lkName) + c_HookSize + FInternalLeading;
    end;
  end
  else
  begin
    W := GetTextWidth(lkName);
    if W <> 0 then
    begin
      Result := W + c_LabelXMarg + c_HookSize;
    end;
  end;
end;

function TChartWriter.GetNameSectionSpace: integer;
var
  W: integer;
  i, j: integer;
  sl: TStringList;
begin
  Result := 0;
  if FNameSections.Count = 0 then
    Exit;
  W := 0;
  if FNamAx.IsXAxis then
  begin
    Result := GetTextHeight(lkNameSection) + GetSectionVertMargin(atNameAxis);
    Exit;
  end;
  sl := TStringList.Create;
  try
    for i := 0 to FNameSections.Count - 1 do
    begin
      CreateListFromDelimiter('|', FNameSections[i].FLongCaption, sl);
      for j := 0 to sl.Count - 1 do
      begin
        if GetTextWidth(lkNameSection, sl[j]) + c_LabelXMarg > W then
          W := GetTextWidth(lkNameSection, sl[j]) + c_LabelXMarg;
      end;
    end;
    Result := W + GetSectionHorzMargin(atNameAxis);
  finally
    sl.Free;
  end;
end;

function TChartWriter.GetTextWidth(LabelKind: TLabelKind; AText: string;
Angle: integer = 0): integer;
var
  S: string;
  sl: TStringList;
  HV, HN, HVS, HNS: string;
  WV, WN, WVS, WNS: string;
begin
  WN := FWidestName;
  if (Chart.NameType = ntNumberSpan) and (NumSpanPrecision > 0) then
    WN := WN + ',';
  if LabelKind = lkValue2 then
    WV := FWidestValue2
  else
    WV := FWidestValue;
  if FUseValShortNames and (FWidestValueShortSection <> '') then
    WVS := FWidestValueShortSection
  else
    WVS := FWidestValueSection;
  if FUseNamShortNames and (FWidestNameShortSection <> '') then
    WNS := FWidestNameShortSection
  else
    WNS := FWidestNameSection;
  HN := FTallestName;
  HV := FTallestValue;
  HVS := FTallestValueSection;
  HNS := FTallestNameSection;

  sl := TStringList.Create;
  try
    if AText <> '' then
    begin
      if LabelKind = lkName then
        WN := AText
      else if (LabelKind in [lkValue, lkValue2]) then
        WV := AText
      else if LabelKind = lkValueSection then
        WVS := AText
      else if LabelKind = lkNameSection then
        WNS := AText
      else
        WVS := AText;
    end;

    if Angle = 0 then
      Angle := DetectAngle(LabelKind, Self);
    if LabelKind = lkName then
    begin
      if not(toName in TextTilting) then
        S := WN
      else
      begin
        if (Angle < 900) then
          S := WN
        else
          S := HN;
      end;
    end
    else if (LabelKind in [lkValue, lkValue2]) then
    begin
      if not(toValue in TextTilting) then
        S := WV
      else
      begin
        if (Angle < 900) then
          S := WV
        else
          S := HV
      end;
    end
    else if LabelKind = lkNameSection then
    begin
      if not(toSection in TextTilting) then
        S := WNS
      else
      begin
        if (Angle < 900) then
          S := WNS
        else
          S := HNS
      end;
    end
    else if LabelKind = lkValueSection then
    begin
      if not(toSection in TextTilting) then
        S := WVS
      else
      begin
        if (Angle < 900) then
          S := WVS
        else
          S := HVS;
      end;
    end
    else
      S := AText;

    FHWBM.Canvas.Font.Assign(Font);
    if (LabelKind = lkName) then
      FHWBM.Canvas.Font.Assign(FNameFont)
    else if (LabelKind in [lkValue, lkValue2]) then
      FHWBM.Canvas.Font.Assign(FValueFont)
    else if (LabelKind = lkNameSection) and (NameSectionDefs <> nil) then
      FHWBM.Canvas.Font.Assign(NameSectionDefs.Font)
    else if (LabelKind = lkValueSection) and (ValueSectionDefs <> nil) then
      FHWBM.Canvas.Font.Assign(ValueSectionDefs.Font);
    sl.Text := S;
    if Angle = 450 then
    begin
      Result := GetTextDiag(FHWBM.Canvas, S);
      Result := round(sqrt(Result * Result / 2));
    end
    else if Angle = 900 then
    begin
      Result := FHWBM.Canvas.TextHeight(S)
    end
    else
    begin
      Result := GetLabelW(S, True, FHWBM.Canvas);
    end;
  finally
    sl.Free;
  end;
end;

function TChartWriter.GetTextWidth(LabelKind: TLabelKind;
Angle: integer = 0): integer;
begin
  Result := GetTextWidth(LabelKind, '', Angle);
end;

function TChartWriter.GetTextHeight(LabelKind: TLabelKind;
Angle: integer = 0): integer;
var
  S: string;
  sl: TStringList;
  H, n: integer;
  TM: TTextMetric;

begin
  Result := 0;
  if Angle = 0 then
    Angle := DetectAngle(LabelKind, Self);
  sl := TStringList.Create;
  try
    if LabelKind = lkName then
    begin
      if not(toName in TextTilting) then
        S := FTallestName
      else if (Angle <> 0) then
        S := FWidestName
      else
        S := FTallestName;
    end
    else if (LabelKind = lkValue) then
    begin
      if not(toValue in TextTilting) then
        S := FTallestValue
      else if (Angle <> 0) then
        S := FWidestValue
      else
        S := FTallestValue;
    end
    else if (LabelKind = lkValue2) then
    begin
      if not(toValue in TextTilting) then
        S := FTallestValue2
      else if (Angle <> 0) then
        S := FWidestValue2
      else
        S := FTallestValue2;
    end
    else if LabelKind = lkNameSection then
    begin
      if not(toSection in TextTilting) then
        S := FTallestNameSection
      else if (Angle <> 0) then
        S := FWidestNameSection
      else
        S := FTallestNameSection;
    end
    else
    begin
      if not(toSection in TextTilting) then
        S := FTallestValueSection
      else if (Angle <> 0) then
        S := FWidestValueSection
      else
        S := FTallestValueSection;
    end;

    if LabelKind = lkInfo then
      FHWBM.Canvas.Font.Assign(Font)
    else if (LabelKind = lkName) then
      FHWBM.Canvas.Font.Assign(FNameFont)
    else if (LabelKind in [lkValue, lkValue2]) then
      FHWBM.Canvas.Font.Assign(FValueFont)
    else if (LabelKind = lkNameSection) and (NameSectionDefs <> nil) then
      FHWBM.Canvas.Font.Assign(NameSectionDefs.Font)
    else if (LabelKind = lkValueSection) and (ValueSectionDefs <> nil) then
      FHWBM.Canvas.Font.Assign(ValueSectionDefs.Font);
    GetTextMetrics(FHWBM.Canvas.Handle, TM);
    FInternalLeading := TM.tmInternalLeading;
    sl.Text := S;
    H := 0;
    n := 0;
    if Angle = 0 then
      n := GetLabelW(S, False, FHWBM.Canvas) + FInternalLeading
    else
    begin
      if Angle = 900 then
        n := FHWBM.Canvas.TextWidth(S)
      else if Angle = 450 then
      begin
        n := GetTextDiag(FHWBM.Canvas, S);
        n := round(sqrt(n * n / 2));
      end;
    end;
    if n > H then
    begin
      Result := n;
    end;
  finally
    sl.Free;
  end;
end;

procedure TChartWriter.SetLabelFont(LabelKind: TLabelKind);
begin
  if LabelKind = lkInfo then
    Canvas.Font.Assign(Font)
  else if (LabelKind = lkName) then
    Canvas.Font.Assign(FNameFont)
  else if (LabelKind = lkValue) then
    Canvas.Font.Assign(FValueFont)
  else if (LabelKind = lkNameSection) and (NameSectionDefs <> nil) then
    Canvas.Font.Assign(NameSectionDefs.Font)
  else if (LabelKind = lkValueSection) and (ValueSectionDefs <> nil) then
    Canvas.Font.Assign(ValueSectionDefs.Font);
end;

procedure TChartWriter.SetLanguage(Value: string);
begin
  if Value = FLanguage then
    Exit;
  FLanguage := Value;
  if Value <> '' then
    Fmt := TFormatSettings.Create(Value)
  else
    Fmt := TFormatSettings.Create;
end;

function TChartWriter.GetValueSectionSpace: integer;
var
  W: integer;
  i: integer;
  sl: TStringList;
  j: integer;
begin
  Result := 0;
  if FValueSections.Count = 0 then
    Exit;
  if FValAx.IsXAxis then
  begin
    Result := GetTextHeight(lkValueSection) + GetSectionVertMargin(atValueAxis);
    Exit;
  end;
  W := GetTextWidth(lkValueSection) + c_LabelXMarg;
  sl := TStringList.Create;
  try
    for i := 0 to FValueSections.Count - 1 do
    begin
      CreateListFromDelimiter('|', FValueSections[i].FLongCaption, sl);
      for j := 0 to sl.Count - 1 do
      begin
        if GetTextWidth(lkValueSection, sl[j]) + c_LabelXMarg > W then
          W := GetTextWidth(lkValueSection, sl[j]) + c_LabelXMarg;
      end;
    end;
    Result := W + GetSectionHorzMargin(atValueAxis);
  finally
    sl.Free;
  end;
end;

function TChartWriter.GetNameLabelRect: TRect;
begin
  Result := FNamAx.LabelRect;
end;

function TChartWriter.GetSectionHorzMargin(Axis: TAxisType): integer;
begin
  Result := 0;
  if Axis = atNameAxis then
  begin
    if NameSectionDefs <> nil then
      Result := NameSectionDefs.FCaptionHorizMargin;
  end
  else
  begin
    if ValueSectionDefs <> nil then
      Result := ValueSectionDefs.FCaptionHorizMargin;
  end

end;

function TChartWriter.GetSectionVertMargin(Axis: TAxisType): integer;
begin
  Result := 0;
  if Axis = atNameAxis then
  begin
    if NameSectionDefs <> nil then
      Result := NameSectionDefs.FCaptionVertMargin;
  end
  else
  begin
    if ValueSectionDefs <> nil then
      Result := ValueSectionDefs.FCaptionVertMargin;
  end

end;

function TChartWriter.GetValueLabelRect: TRect;
begin
  Result := FValAx.LabelRect;
end;

function TChartWriter.GetNameSections(Index: integer): TSection;
begin
  Result := FNamAx.Sections[Index];
end;

function TChartWriter.GetNameSectionCount: integer;
begin
  Result := FNamAx.SectionCount;
end;

function TChartWriter.GetValueSections(Index: integer): TSection;
begin
  Result := FValAx.Sections[Index];
end;

function TChartWriter.GetValueSectionCount: integer;
begin
  Result := FValAx.SectionCount;
end;

procedure TChartWriter.ResetCanvas;
begin
  Canvas.Font.Assign(Font);
  Canvas.Brush.Assign(Brush);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
end;

procedure TChartWriter.SaveSelBM;
begin
  if FRulerVisible and not(ViewMode = vmSelecting) and (InView(TCWCurve) = nil)
  then
    Exit;
  FSelBM.SetSize(ClientRect.Width, ClientRect.Height);
  FSelBM.Canvas.CopyRect(Rect(0, 0, FSelBM.Width, FSelBM.Height), Canvas,
    ClientRect);
end;

procedure TChartWriter.SetInfoControl(Value: Boolean);
begin
  if Value <> FInfoControl then
  begin
    FInfoControl := Value;
    DoRepaint;
  end;
end;

function TChartWriter.GetValueCount: integer;
{ Counts the number og whole YUnits }
var
  YH, YL: single;
begin
  YL := ValueLow;
  YH := ValueHigh;
  Result := 0;
  while True do
  begin
    if YL > YH then
    begin
      Break;
    end
    else
    begin
      inc(Result);
      YL := YL + ValueIntervals;
    end;
  end;
end;

function TChartWriter.GetInfoControl: Boolean;
begin
  Result := FInfoControl;
end;

function TChartWriter.GetIsUpdating: Boolean;
begin
  Result := InState(stUpdating);
end;

function TChartWriter.IsTimeSpan(ANameType: TNameType): Boolean;
begin
  Result := ANameType in [ntDateSpan, ntHourSpan, ntMinuteSpan, ntSecondSpan];
end;

function TChartWriter.HasWall: Boolean;
begin
  Result := false;
  if Chart = nil then
    Exit;
  Result := (Chart.FWallWidth > 0) and
    (AxisOrientation = alBottomLeft) and (GraphBorders <> gbNone) and
     not(ActiveGraph is TCWPie);
end;

function TChartWriter.GetGraphRect: TRect;
begin
   Result := GraphSpaceRect;
end;

function TChartWriter.GetSeries(Index: integer): TSeries;
begin
  Result := FSeriesData[Index];
end;

function TChartWriter.GetSeriesItems(SeriesIndex, ItemIndex: integer)
  : TSeriesItem;
begin
  Result := FSeriesData[SeriesIndex].FSeriesItems[FSeriesData[SeriesIndex].FirstItem +
    ItemIndex];
end;

function TChartWriter.GetGraphSpaceRect: TRect;
{ This is actually the same as GraphRect }
var
  W, H, D : integer;
begin
  Result.Left := GraphLeft;
  Result.Top := GraphTop;
  Result.Right := Result.Left + GraphWidth;
  Result.Bottom := Result.Top + GraphHeight;
  if FCentered then
  begin
    D := FRightSpace div 2;
    W := GraphWidth;
    H := GraphHeight;
    Result.Left := Result.Left + D;
    Result.Right := Result.Left + W;
    D := FBottomSpace div 2;
    Result.Top := Result.Top + D;
    Result.Bottom := Result.Top + H;
  end;
end;

function TChartWriter.GetWallColor : TColor;
begin
   Result := clWindow;
   if (Chart <> nil) and (Chart is TCWAxisChart) then
     Result := Chart.WallColor;
end;

function TChartWriter.GetAxisColor : TColor;
begin
   Result := clBlack;
   if (Chart <> nil) and (Chart is TCWAxisChart) then
     Result := Chart.AxisColor;
end;

function TChartWriter.GetGradientWall : Boolean;
begin
   Result := false;
   if (Chart <> nil) and (Chart is TCWAxisChart) then
     Result := Chart.GradientWall;
end;

function TChartWriter.GetTextTilting : TTextOrientations;
begin
   Result := [];
   if Chart <> nil then
     Result := Chart.TextTilting;
end;

function TChartWriter.GetTextTiltThreshold : Integer;
begin
   Result := 1;
   if Chart <> nil then
     Result := Chart.TextTiltThreshold;
end;

function TChartWriter.GetSelRect: TRect;
var
  pStart, PEnd: integer;

begin
  try
    Result := Rect(0, 0, 0, 0);
    if not(ViewMode in [vmSelected]) then
      Exit;
    if FNamAx.IsXAxis then
    begin
      { Add unitsize to fill the rect completely }
      pStart := XFromName(Names[FDynaSectStart]);
      PEnd := XFromName(Names[FDynaSectEnd]) + FNamAx.UnitSize;
      Result := Rect(pStart, GraphPrintRect.Top, PEnd, GraphPrintRect.Bottom);
    end
    else
    begin
      pStart := YFromName(Names[FDynaSectStart]);
      PEnd := YFromName(Names[FDynaSectEnd]) + FNamAx.UnitSize;
      Result := Rect(GraphPrintRect.Left, pStart, GraphPrintRect.Right, PEnd);
    end;
  except
    raise
  end;
end;

function TChartWriter.GetTrackBar(ASeriesIndex, AItemIndex: integer;
var ATrackBar: TTrackBar): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FTrackBars.Count - 1 do
  begin
    if (FTrackBars[i].SeriesIndex = ASeriesIndex) and
      (FTrackBars[i].SeriesItmIndex = AItemIndex) then
    begin
      ATrackBar := FTrackBars[i];
      Result := True;
      Break;
    end;
  end;
end;

function TChartWriter.GetRendered: Boolean;
begin
  Result := (ActiveGraph <> nil) and (Count > 0) and not InState(stUpdating);
end;

function TChartWriter.CanRender: integer;
begin
  Result := 0;
  if Chart = nil then
    Result := R_NoChart
  else if ActiveGraph = nil then
    Result := R_NoActiveGraph
  else if (Count = 0) then
    Result := R_NoData
  else if InState(stUpdating) then
    Result := R_Updating;
end;

procedure TChartWriter.RepaintSelRect;
var
  R: TRect;
begin
  R := GraphPrintRect;
  InvalidateRect(Handle, @R, False);
  Update;
end;

procedure TChartWriter.SetGraphBorders(Value: TGraphBorders);
begin
  if Value = FGraphBorders then
    Exit;
  FGraphBorders := Value;
  if csDesigning in componentState then
    Invalidate
  else
    DoRepaint;
end;

function TChartWriter.GetGraphWidth: integer;
begin
  Result := ClientRect.Width - InnerMargins.Right - InnerMargins.Left - FRightSpace -
    SpaceOf(apRight) - SpaceOf(apLeft);
end;

function TChartWriter.GetGraphHeight: integer;
begin
  Result := ClientRect.Height - InnerMargins.Bottom - InnerMargins.Top - FBottomSpace -
    SpaceOf(apBottom) - SpaceOf(apTop);
end;

function TChartWriter.GetGraphLeft: integer;
begin
  Result := InnerMargins.Left + SpaceOf(apLeft);
end;

function TChartWriter.GetGraphTop: integer;
begin
  Result := InnerMargins.Top + SpaceOf(apTop);
end;


function TChartWriter.GetMouseTimeFormat: string;
begin
  if FMouseTimeFormat = '' then
    Result := TimeFormat
  else
    Result := FMouseTimeFormat;
end;

function TChartWriter.GetMousePrecision: TMousePrecision;
begin
  if (csDesigning in componentState) or (Rulers = ruNone) or (Rulers = ruValues)
  then
    Result := FMousePrecision
  else if (Rulers = ruNames) or (Rulers = ruBoth) then
    Result := mpMedium
  else
    Result := FMousePrecision;
end;

procedure TChartWriter.SetTimeFormat(Value: string);
begin
  if Trim(Value) = FTimeFormat then
    Exit;
  FTimeFormat := Trim(Value);
  if not(csLoading in componentState)
  then
  begin
    RestrictToClient(False);
    CheckLabelFreqs(False);
  end;
  DoRepaint;
end;

procedure TChartWriter.SetNameLabelFreq(Value: integer);
begin
  if (Value < 1) or (Value = FNameLabelFreq) then
    Exit;
  FNameLabelFreq := Value;
  Invalidate;
end;

procedure TChartWriter.SetValueLabelFreq(Value: integer);
begin
  if (Value < 1) or (Value = FValueLabelFreq) then
    Exit;
  FValueLabelFreq := Value;
  Invalidate;
end;

procedure TChartWriter.SetNameUnit(Value: integer);
begin
  if (Value < 1) or (Value = FNameUnit) then
    Exit;
  FNameUnit := Value;
  if not(csLoading in componentState) then
  begin
    RestrictToClient(False);
    CheckLabelFreqs;
  end;
  Invalidate;
end;

procedure TChartWriter.SetValueSpanFromData(Value: Boolean);
begin
 if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Chart.ValueAxis1.FValueSpanFromData := Value
    else
      Chart.ValueAxis2.FValueSpanFromData := Value;
  end;
end;

procedure TChartWriter.SetValueSpan(LowValue, HighValue: single);
begin
   if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Chart.ValueAxis1.SetValueSpan(LowValue, HighValue)
    else
      Chart.ValueAxis2.SetValueSpan(LowValue, HighValue)
  end;
end;

procedure TChartWriter.SetValueIntervals(Value: single);
begin
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Chart.ValueAxis1.FValueIntervals := Value
    else
      Chart.ValueAxis2.FValueIntervals := Value;
  end;
end;

procedure TChartWriter.SetValueLow(Value: single);
begin
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Chart.ValueAxis1.FValueLow := Value
    else
      Chart.ValueAxis2.FValueLow := Value;
  end;
end;

procedure TChartWriter.SetValueHigh(Value: single);
begin
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Chart.ValueAxis1.FValueIntervals := Value
    else
      Chart.ValueAxis2.FValueIntervals := Value;
  end;
end;

procedure TChartWriter.SetHighLow;
{ Detects high and low values when ValueSpanFromData is set }
begin
  if Count = 0 then
    Exit;
  if (Chart is TCWAxisChart) then
  begin
    if FActiveValAx = FValAx then
      GetValueSpan(Chart.ValueAxis1.FValueHigh, Chart.ValueAxis1.FValueLow)
    else
      GetValueSpan(Chart.ValueAxis2.FValueHigh, Chart.ValueAxis2.FValueLow);
  end;
end;

procedure TChartWriter.SetNumSpanPrecision(Value: integer);
begin
    if (Chart <> nil) then
  begin
      Chart.FNumSpanPrecision := Value
  end;
end;

procedure TChartWriter.SetValuePrecision(Value: integer);
begin
    if (Chart <> nil) then
  begin
    if ActiveGraph is TCWPie then
      Chart.ValuePrecision := Value
    else
      if FActiveValAx = FValAx then
      Chart.ValueAxis1.FValuePrecision := Value
    else
      Chart.ValueAxis2.FValuePrecision := Value;
  end;
end;

function TChartWriter.GetValuePrecision: integer;
begin
  Result := 0;
  if (Chart <> nil) then
  begin
    if ActiveGraph is TCWPie then
      Result := Chart.ValuePrecision
    else if FActiveValAx = FValAx then
      Result := Chart.ValueAxis1.ValuePrecision
    else
      Result := Chart.ValueAxis2.ValuePrecision;
  end;
end;

function TChartWriter.GetValueIntervals: single;
begin
  Result := 1;
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Result := Chart.ValueAxis1.ValueIntervals
    else
      Result := Chart.ValueAxis2.ValueIntervals;
  end;
end;

function TChartWriter.GetValueHigh: single;
begin
  Result := 1;
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Result := Chart.ValueAxis1.ValueHigh
    else
      Result := Chart.ValueAxis2.ValueHigh;
  end;
end;

function TChartWriter.GetValueLow: single;
begin
  Result := 1;
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Result := Chart.ValueAxis1.ValueLow
    else
      Result := Chart.ValueAxis2.ValueLow;
  end;
end;

function TChartWriter.GetValueSpanFromData: Boolean;
begin
  Result := False;
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Result := Chart.ValueAxis1.ValueSpanFromData
    else
      Result := Chart.ValueAxis2.ValueSpanFromData;
  end;
end;

function TChartWriter.GetValueFloatUnit : single;
begin
  Result := 0;
  if (Chart <> nil) then
  begin
    if FActiveValAx = FValAx then
      Result := Chart.ValueAxis1.ValueFloatUnit
    else
      Result := Chart.ValueAxis2.ValueFloatUnit;
  end;
end;

function TChartWriter.DetectNameType(SeriesIndex: integer): TNameType;
var
  S1, S2: string;
  Dt1, Dt2: TDateTime;
  N1, N2: integer;
  Y, M, D, H, MM, Sec, Ms: word;
  y2, M2, D2, H2, Mm2, Sec2, Ms2: word;
begin
  Result := ntGeneral;
  if ActiveGraph is TCWPie then
  begin
    Exit;
  end;
  if FSeriesData[0].Count < 2 then
    Exit;

  if Count = 0 then
    Exit;
  { Get the firts two items }
  S1 := FSeriesData[SeriesIndex].FSeriesItems[0].Name;
  S2 := FSeriesData[SeriesIndex].FSeriesItems[1].Name;
  if TryStrToInt(S1, N1) then
  begin
    if TryStrToInt(S1, N2) then
      Result := ntNumberSpan;
  end
  else if TryStrToDateTime(S1, Dt1, Fmt) then
  begin
    if TryStrToDateTime(S2, Dt2, Fmt) then
    begin
      DecodeDateTime(Dt1, Y, M, D, H, MM, Sec, Ms);
      DecodeDateTime(Dt2, y2, M2, D2, H2, Mm2, Sec2, Ms2);
      if (H = 0) and (MM = 0) and (Sec = 0) and (H2 = 0) and (Mm2 = 0) and
        (Sec2 = 0) then
        { Looks like an ntDateSpan }
        begin
         Result := ntDateSpan;
        end
      else if ((H <> 0) or (H2 <> 0)) and (MM = 0) and (Sec = 0) and (Mm2 = 0)
        and (Sec2 = 0) then
        Result := ntHourSpan
      else if ((MM <> 0) or (Mm2 <> 0)) and (Sec = 0) and (Sec2 = 0) then
        Result := ntMinuteSpan
      else if (Sec <> 0) or (Sec2 <> 0) then
        Result := ntSecondSpan
    end;
  end
end;

function TChartWriter.GetNameSectionDefs: TCWNameSectionDefs;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  Result := Chart.NameSectionDefs;

end;

function TChartWriter.GetValueSectionDefs: TCWValueSectionDefs;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  Result := Chart.ValueSectionDefs;
end;

function TChartWriter.GetNumSpanPrecision: integer;
begin
   Result := 0;
   if (Chart <> nil) then
  begin
      Result := Chart.NumSpanPrecision
  end;
end;

procedure TChartWriter.SetGraphBGColor(Value: TColor);
begin
  if Value = FGraphBGColor then
    Exit;
  FGraphBGColor := Value;
  DoRepaint;
end;

procedure TChartWriter.SetCentered(Value: Boolean);
begin
  if Value = FCentered then
    Exit;
  FCentered := Value;
  if not (csLoading in ComponentState) then
   RefreshGraph;
end;

procedure TChartWriter.SetChartAs(AChart : TCWChart; AsGraph : TCWGraph);
begin
   if AChart = nil then
   begin
     Chart := nil;
     Exit;
   end;

   if (AChart = Chart) and (Chart <> nil) and (Chart.AllEqual = AsGraph) then
     Exit;

   AChart.ReplaceGraphs(AsGraph);
   if AChart = Chart then
   begin
    if Count = 0 then
      Exit;
     if (Contraction <> 1) or (AChart.OverflowAction = ovScrolling) then
       Reload
     else
       RefreshGraph;
     Exit;
   end;
   Chart := AChart;
end;

procedure TChartWriter.SetChart(Value : TCWChart);
var
  NT : TNameType;
  Old : TCWChart;
  Msg : TMessage;
begin
  if Value = FChart then
    Exit;
  Old := FChart;
  if csLoading in ComponentState then
  begin
    FChart := Value;
    if FChart <> nil then
    begin
      SetPosition(Value.AxisOrientation);
      FChart.FreeNotification(Self);
      if not (csDesigning in ComponentState) then
       PostMessage(Handle, WM_LOADFILE, 0, 0);
      FNeedsIDs := True;
      //CreateIDS;
      FChart.FWID := AddToWList(Self,  Value);
    end;
    Exit;
  end;

  //if (Value <> nil) and (Value.Writer = nil) then
  //begin
  //   ShowGWError(msg_NoWriter, 'Chart');
  //end;
  FChart := Value;
  if FChart <> nil then
  begin
   try
    CreateIDS;
   except
    FChart := nil;
    GoBackError;
    raise;
   end;
   FChart.FWID := AddToWList(Self,  Value);
   SetPosition(Value.AxisOrientation);
   FChart.FreeNotification(Self);
   if not (csDesigning in ComponentState) then
   begin
     if (FChart.Dataset <> nil) or (FChart.FileName <> '') then
     begin
      WMLOADFILE(Msg);
     end
     else if Assigned(FChart.FOnGetData) then
     begin
       Clear;
       FChart.FOnGetData(Chart)
     end
     else
       Clear;
   end;
  end
  else
  begin
    Clear;
    Exit;
  end;

  if csDesigning in ComponentState then
  begin
    if (Value is TCWPieChart) and (Chart.NameType <> ntGeneral) then
      Chart.NameType := ntGeneral
    else
    begin
      RenderDesigner;
    end;
  end;
  Invalidate;
end;

procedure TChartWriter.SetPosition(Orientation: TAxisOrientation);
begin
  if Orientation = alBottomLeft then
  begin
    FNamAx.Position := apBottom;
    FValAx.Position := apLeft;
    FValAx2.Position := apRight;
  end
  else if Orientation = alBottomRight then
  begin
    FNamAx.Position := apBottom;
    FValAx.Position := apRight;
    FValAx2.Position := apLeft;
  end
  else if Orientation = alLeftTop then
  begin
    FNamAx.Position := apLeft;
    FValAx.Position := apTop;
    FValAx2.Position := apBottom;
  end
  else if Orientation = alTopLeft then
  begin
    FNamAx.Position := apTop;
    FValAx.Position := apLeft;
    FValAx2.Position := apRight;
  end
  else if Orientation = alTopRight then
  begin
    FNamAx.Position := apTop;
    FValAx.Position := apRight;
    FValAx2.Position := apLeft;
  end
  else if Orientation = alRightTop then
  begin
    FNamAx.Position := apRight;
    FValAx.Position := apTop;
    FValAx2.Position := apBottom;
  end
  else if Orientation = alLeftBottom then
  begin
    FNamAx.Position := apLeft;
    FValAx.Position := apBottom;
    FValAx2.Position := apTop;
  end
  else if Orientation = alRightBottom then
  begin
    FNamAx.Position := apRight;
    FValAx.Position := apBottom;
    FValAx2.Position := apTop;
  end;
end;

procedure TChartWriter.SetRulers(Value: TRulers);
begin
  if Value = FRulers then
    Exit;
  FRulers := Value;
  FRulerVisible := False;
  Invalidate;
end;

function TChartWriter.GetWallWidth: integer;
begin
  if HasWall and (Chart <> nil) and (Chart is TCWAxisChart) then
    Result := Chart.WallWidth
  else
    Result := 0;
end;

function TChartWriter.GetAxisOrientation : TAxisOrientation;
begin
  Result := alBottomLeft;
  if (Chart = nil) or (Chart is TCWPieChart) then
    Exit
  else
    Result := Chart.AxisOrientation;
end;

function TChartWriter.RulerPoints: TRulerPoints;
var
  Vert: Boolean;
  Defs: TSeriesDefs;
  P: TPoint;
  i: integer;
begin
  Finalize(Result);
  if (Rulers = ruNone) or (Rulers = ruValues) then
    Exit;
  Vert := FNamAx.IsXAxis;
  if PointsFromRuler(Vert, FRulerX, FRulerY, P, Defs) then
  begin
    setlength(Result, Length(Defs));
    for i := 0 to High(Defs) do
      Result[i] := Defs[i].SeriesItemIndex - FSeriesData[Defs[i].SeriesIndex].FIndent;
  end;
end;

procedure TChartWriter.HideRuler;
begin
  FRulerVisible := False;
  FRulerX := -1;
  FRulerY := -1;
  Invalidate;
end;

procedure TChartWriter.SetTitleAlignment(Value : TAlignment);
begin
   if Value = FTitleAlignment then
     Exit;
   FTitleAlignment := Value;
   Invalidate;
end;

function TChartWriter.SeriesOfTitle(ATitle: string): TSeries;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SameText(FSeriesData[i].Title, ATitle) then
    begin
      Result := FSeriesData[i];
      Break;
    end;
end;

function TChartWriter.GetAxisElements : TAxisElements;
begin
  Result := [];
  if Chart = nil then
    Exit;
  Result := Chart.AxisElements;
end;

function TChartWriter.GraphSectionFromPoint(X, Y: integer): integer;
var
  i: integer;
  R: TRect;
begin
  Result := -1;
  for i := 0 to FNameSections.Count - 1 do
  begin
    R := FNameSections[i].SectionGraphRect;
    if Windows.PtInRect(R, System.Classes.Point(X, Y)) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TChartWriter.IndexOfName(AName: string): integer;
begin
  Result := FNameList.IndexOf(AName);
end;

function TChartWriter.IndexOfNameEx(AName: string): integer;
var
  i: integer;
  S: string;
begin
  Result := -1;
  for i := 0 to FOrigData[0].Count - 1 do
  begin
    S := FOrigData[0].FSeriesItems[i].FName;
    if SameText(AName, S) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TChartWriter.IndexOfDateEx(Y, M, D: word): integer;
var
  i: integer;
  Dt: TDate;
begin
  Result := -1;
  if not IsTimeSpan(Chart.NameType) then
    Exit;
  for i := 0 to FOrigData[0].Count - 1 do
  begin
    Dt := FOrigData[0].FSeriesItems[i].FRealDate;
    if CheckDateHit(Dt, Y, M, D) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TChartWriter.IndexOfTimeEx(H, M, S: integer): integer;
var
  i: integer;
  Dt: TDateTime;
begin
  Result := -1;
  if not IsTimeSpan(Chart.NameType) then
    Exit;
  for i := 0 to FOrigData[0].Count - 1 do
  begin
    Dt := FOrigData[0].FSeriesItems[i].FRealDate;
    if CheckTimeHit(Dt, H, M, S) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TChartWriter.SpanToSeriesIndex(ASeriesIndex,
  ASpanIndex: integer): integer;
begin
  Result := ASpanIndex - FSeriesData[ASeriesIndex].FIndent;
end;

function TChartWriter.SeriesIndexOfX(APos: TPoint;
var SeriesIndex: integer): integer;
{ Obtains the index in SeriesIems that matches the x-part of Apos. }
var
  FoundPos: TPoint;
  SeriesDefs: TSeriesDefs;
  Itm: TSeriesItem;
  Series: TSeries;
begin
  Result := -1;
  if FSeriesData.Count = 0 then
    Exit;
  if not PointsFromRuler(True, APos.X, APos.Y, FoundPos, SeriesDefs) then
    Exit;

  Series := FSeriesData[SeriesDefs[0].SeriesIndex];
  Itm := Series.FSeriesItems[SeriesDefs[0].SeriesItemIndex];
  Result := IndexOfName(Itm.FName);
end;

function TChartWriter.SeriesIndexOfY(APos: TPoint;
var SeriesIndex: integer): integer;
{ Obtains the index in Series items that matches the y-part of Apos }
var
  FoundPos: TPoint;
  SeriesDefs: TSeriesDefs;
  Series: TSeries;
  Itm: TSeriesItem;
begin
  Result := -1;
  if FSeriesData.Count = 0 then
    Exit;
  if not PointsFromRuler(False, APos.X, APos.Y, FoundPos, SeriesDefs) then
    Exit;

  Series := FSeriesData[SeriesDefs[0].SeriesIndex];
  Itm := Series.FSeriesItems[SeriesDefs[0].SeriesItemIndex];
  Result := IndexOfName(Itm.FName);
end;

procedure TChartWriter.SetSelection(StartIndex, EndIndex: integer;
Add: Boolean = False);
begin
  if (ActiveGraph is TCWPie) or (Count = 0) or InState(stUpdating) then
    Exit;
  if (StartIndex < 0) or (StartIndex > FNameList.Count - 1) then
    ShowGWError(msg_IndexBounds, 'Start ');
  if (EndIndex < 0) or (EndIndex > FNameList.Count - 1) then
    ShowGWError(msg_IndexBounds, 'End ');
  if abs(StartIndex - EndIndex) = 0 then
    Exit;
  if StartIndex > EndIndex then
    ShowGWError(msg_IndexBounds, 'Start ');
  FKeepSelection := Add;
  if not Add then
    ClearSelection;
  if Add and (FDynaSectStart <> -1) then
  begin
    if StartIndex < FDynaSectStart then
      FDynaSectStart := StartIndex;
    if EndIndex > FDynaSectEnd then
      FDynaSectEnd := EndIndex;
  end
  else
  begin
    FDynaSectStart := StartIndex;
    FDynaSectEnd := EndIndex;
  end;
  FViewMode := vmNormal;
  Repaint;
  FViewMode := vmSelected;
  Invalidate;
end;

function TChartWriter.GetSelection(var StartIndex, EndIndex: integer): Boolean;
begin
  Result := (FDynaSectStart > -1) and (FDynaSectEnd > -1);
  StartIndex := FDynaSectStart;
  EndIndex := FDynaSectEnd;
end;

function TChartWriter.NamValFromPos(const X, Y: integer; var Nam: string;
var Val: single): Boolean;
{ Finds the values belonging to the point that is within the space (4 pixels) of XY }
var
  R: TRect;
  i, j: integer;
  Ser: TSeries;
  XP, YP: integer;
  G: TCWGraph;
begin
  Nam := '';
  Val := MaxInt;
  Result := False;
  R := GraphPrintRect;
  for i := 0 to FSeriesData.Count - 1 do
  begin
    if not FSeriesData[i].Visible then
      Continue;
    Ser := FSeriesData[i];
    G := ActiveGraph;
    if not(G is TCWCurve) then  {ToDo: Does not work with mixed graphs}
      raise TCWException.Create('Graph is not a curve');
    for j := 0 to Ser.ItemPoints.Count - 1 do
    begin
      XP := Ser.Points[j].X;
      YP := Ser.Points[j].Y;
      R.Left := XP - TCWCurve(G).PointWidth;
      R.Right := XP + TCWCurve(G).PointWidth;
      R.Top := YP - TCWCurve(G).PointWidth;
      R.Bottom := YP + TCWCurve(G).PointWidth;
      if Windows.PtInRect(R, System.Classes.Point(X, Y)) then
      begin
        Nam := Ser.FSeriesItems[Ser.FirstItem + j].Name;
        Val := Ser.FSeriesItems[Ser.FirstItem + j].Value;
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TChartWriter.SeriesFromMouse(var ItemIndex: integer): TSeries;
var
  MP: TPoint;
  SerInd: integer;
begin
  MP := ScreenToClient(MOuse.CursorPos);
  Result := nil;
  if DoPosInPoint(MP.X, MP.Y, MousePrecision, SerInd, ItemIndex) then
  begin
    Result := FSeriesData[SerInd];
    ItemIndex := ItemIndex - Result.FIndent;
  end;
end;

function TChartWriter.DoPosInPoint(X, Y: integer; Precision: TMousePrecision;
var SeriesIndex, SeriesItemIndex: integer): Boolean;
{ Returns true if XY is within the space of a point }
var
  Pt: TPoint;
  APos: TPoint;
  i, j: integer;
  R: TRect;
  FoundItm, FoundSer: integer;
  XDist, YDist: single;
  Dist: single;
  Defs: TSeriesDefs;
  P: TPoint;
  W: integer;
  c: TCWGraph;

begin
  Result := False;
  if InState(stUpdating) or InState(stInternalAction) then
    Exit;
  c := InView(TCWCurve);
  if c = nil then
    Exit;
  APos := Point(X, Y);
  FoundSer := -1;
  FoundItm := -1;
  Dist := MaxInt;

  if not PtInRect(GraphPrintRect, APos) then
    Exit;
  R := GraphPrintRect;
  W := TCWCurve(c).PointWidth;
  if Precision = mpHigh then
  begin
    R := Rect(APos.X - W, APos.Y - W, APos.X + W, APos.Y + W)
  end
  else if Precision = mpMedium then
  begin
    if not PointsFromRuler(FNamAx.IsXAxis, APos.X, APos.Y, P, Defs) then
      Exit;
  end;
  { Else R = graphrect }

  if Precision = mpMedium then
    for i := 0 to High(Defs) do
    begin
      P := FSeriesData[Defs[i].SeriesIndex].FSeriesItems
        [Defs[i].SeriesItemIndex].PointPos;
      XDist := Power(abs(APos.X - P.X), 2);
      YDist := Power(abs(APos.Y - P.Y), 2);
      if XDist + YDist < Dist then
      begin
        Dist := XDist + YDist;
        FoundItm := Defs[i].SeriesItemIndex;
        FoundSer := Defs[i].SeriesIndex;
      end;
    end
  else
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if not FSeriesData[i].Visible then
        Continue;
      for j := FSeriesData[i].FirstItem to FSeriesData[i].LastItem do
      begin
        Pt := FSeriesData[i].ItemPoints[j - FSeriesData[i].FirstItem];
        if not PtInRect(R, Pt) then
          Continue;
        XDist := Power(abs(APos.X - Pt.X), 2);
        YDist := Power(abs(APos.Y - Pt.Y), 2);
        if XDist + YDist < Dist then
        begin
          Dist := XDist + YDist;
          FoundItm := j;
          FoundSer := i;
        end;
      end;
    end;

  if FoundSer <> -1 then
  begin
    Result := True;
    SeriesIndex := FoundSer;
    SeriesItemIndex := FoundItm;
  end;
end;

function TChartWriter.DoPosInBar(X, Y: integer;
var SeriesIndex, SeriesItemIndex: integer): Boolean;
var
  j: integer;
begin
  Result := False;
  for j := 0 to FTrackBars.Count - 1 do
  begin
    if PtInRect(FTrackBars[j].VisibleRect, Point(X, Y)) then
    begin
      Result := True;
      SeriesIndex := FTrackBars[j].SeriesIndex;
      SeriesItemIndex := FTrackBars[j].SeriesItmIndex;
      Break;
    end;
  end;
end;

function TChartWriter.PosInBar(X, Y: integer;
var SeriesIndex, SeriesItemIndex: integer): Boolean;
begin
  Result := False;
  if InView(TCWBar) = nil then
    Exit;
  Result := DoPosInBar(X, Y, SeriesIndex, SeriesItemIndex);
  if Result then
    SeriesItemIndex := SeriesItemIndex - FSeriesData[SeriesIndex].FIndent;
end;

function TChartWriter.PosInPoint(X, Y: integer;
var SeriesIndex, SeriesItemIndex: integer): Boolean;
begin
  Result := False;
  if InView(TCWCurve) = nil then
    Exit;
  Result := DoPosInPoint(X, Y, mpHigh, SeriesIndex, SeriesItemIndex);
  if Result then
    SeriesItemIndex := SeriesItemIndex - FSeriesData[SeriesIndex].FIndent;
end;

function TChartWriter.MouseInPoint(var SeriesIndex, SeriesItemIndex
  : integer): Boolean;
var
  Pt: TPoint;
begin
  Result := False;
  if InView(TCWCurve) = nil then
    Exit;

  Pt := ScreenToClient(MOuse.CursorPos);
  Result := DoPosInPoint(Pt.X, Pt.Y, MousePrecision, SeriesIndex,
    SeriesItemIndex);
  if Result then
    SeriesItemIndex := SeriesItemIndex - FSeriesData[SeriesIndex].FIndent;
end;

function TChartWriter.MouseInBar(var SeriesIndex, SeriesItemIndex
  : integer): Boolean;
var
  Pt: TPoint;
begin
  Result := False;
  if InView(TCWBar) = nil then
    Exit;

  Pt := ScreenToClient(MOuse.CursorPos);
  Result := DoPosInBar(Pt.X, Pt.Y, SeriesIndex, SeriesItemIndex);
  if Result then
    SeriesItemIndex := SeriesItemIndex - FSeriesData[SeriesIndex].FIndent;
end;

function TChartWriter.MouseInPieSlice(var SeriesIndex, SeriesItemIndex
  : integer): Boolean;
var
  Pt: TPoint;
begin
  Result := False;
  if not(ActiveGraph is TCWPie) then
    Exit;
  Pt := ScreenToClient(MOuse.CursorPos);
  Result := DoPosInPieSlice(Pt.X, Pt.Y, SeriesIndex, SeriesItemIndex);
end;

function TChartWriter.PosInPieSlice(X, Y: integer;
var SeriesIndex, SeriesItemIndex: integer): Boolean;
begin
  Result := False;
  if not(ActiveGraph is TCWPie) then
    Exit;
  Result := DoPosInPieSlice(X, Y, SeriesIndex, SeriesItemIndex);
  if Result then
    SeriesItemIndex := SeriesItemIndex - FSeriesData[SeriesIndex].FIndent;
end;

function TChartWriter.PosInNameSection(X, Y: integer;
var SectionIndex: integer): Boolean;
var
  i: integer;
  R: TRect;
begin
  Result := False;
  if ActiveGraph is TCWPie then
    Exit;
  for i := 0 to NameSectionCount - 1 do
  begin
    R := NameSections[i].SectionGraphRect;
    if PtInRect(R, Point(X, Y)) then
    begin
      Result := True;
      SectionIndex := i;
      Break;
    end;
  end;
end;

function TChartWriter.PosInValueSection(X, Y: integer;
var SectionIndex: integer): Boolean;
var
  i: integer;
  R: TRect;
begin
  Result := False;
  if ActiveGraph is TCWPie then
    Exit;
  for i := 0 to ValueSectionCount - 1 do
  begin
    R := ValueSections[i].SectionGraphRect;
    if PtInRect(R, Point(X, Y)) then
    begin
      Result := True;
      SectionIndex := i;
      Break;
    end;
  end;
end;

function TChartWriter.DoPosInPieSlice(X, Y: integer;
var SeriesIndex, SeriesItemIndex: integer): Boolean;
var
  i, j: integer;
  Rd: single;
  Ang, LastAng: single;
  AA: single;
  c: TPoint;
  H: HRGN;
begin
  Result := False;
  for i := 0 to FSeriesData.Count - 1 do
  begin
    if not FSeriesData[i].Visible then
      Continue;
    c := FSeriesData[i].FPieRect.CenterPoint;
    H := CreateEllipticRgn(FSeriesData[i].FPieRect.Left, FSeriesData[i].FPieRect.Top,
      FSeriesData[i].FPieRect.Right, FSeriesData[i].FPieRect.Bottom);
    if not PtInRegion(H, X, Y) then
    begin
      DeleteObject(H);
      Continue;
    end;
    DeleteObject(H);

    for j := 0 to FSeriesData[i].FTrackPies.Count - 1 do
    begin
      Ang := DegToRad(FSeriesData[i].FTrackPies[j].EndAngel);
      LastAng := DegToRad(FSeriesData[i].FTrackPies[j].StartAngel);
      Rd := arcTan2(Y - c.Y, X - c.X);
      if Rd < 0 then
      begin
        AA := RadToDeg(Rd);
        AA := 360 + AA;
        Rd := DegToRad(AA);
      end;
      if (Rd > LastAng) and (Rd < Ang) then
      begin
        Result := True;
        SeriesIndex := FSeriesData[i].FTrackPies[j].FSeriesIndex;
        SeriesItemIndex := FSeriesData[i].FTrackPies[j].FSeriesItmIndex;
        Break;
      end;
    end;
    if Result then
      Break;
  end;
end;

procedure TChartWriter.ContractValues(Rate: integer;
ContractType: TContractionType);
begin
  FViewMode := vmNormal;
  FDynaSectStart := -1;
  FDynaSectEnd := -1;
  SetState(stUserContraction);
  try
    DoContractValues(Rate, ContractType);
  finally
    ClearState(stUserContraction);
  end;
  PostHist;
end;

procedure TChartWriter.DoRepaint;
var
  vm: TViewMode;
begin

  if csDestroying in componentState then
    Exit;
  if (InState(stAnimating)) and not (csDesigning in ComponentState) then
  begin
    DoAnimation;
    Exit;
  end;

  if InState(stPainting) then
    Exit;
  if (csDesigning in componentState) then
  begin
    Invalidate;
    Exit;
  end;
  if (ActiveGraph = nil) then
    Exit;
  if InState(stUpdating) or (FSeriesData.Count = 0) then
    Exit;
  if ViewMode <> vmHinting then
  begin
    Invalidate;
    Exit;
  end;
  vm := ViewMode;
  FViewMode := vmNormal;
  try
    Repaint;
  finally
    FViewMode := vm;
  end;
end;

procedure TChartWriter.DoContractValues(Rate: integer;
ContractType: TContractionType);
var
  i, j: integer;
  Ser, NewSer: TSeries;
  E, V: single;
  Cnt: integer;
  TheSeries: TList<TSeries>;
  SerItm: TSeriesItem;
  PIndex: integer;
  OrigSer: TSeries;
  Contracted: Boolean;
  G: TCWGraph;
  LastE: single;
  LastCnt: integer;

  procedure Expand(SeriesIndex: integer);
  var
    Y, M, D, H, MN, S, Ms: word;
    y1, M1, D1, h1, mn1, S1, ms1: word;
    Dt: TDateTime;
  begin

    SerItm.FReminderName := FSeriesData[SeriesIndex].FSeriesItems
      [FSeriesData[SeriesIndex].Count - 1].Name;
    if IsTimeSpan(Chart.NameType) then
    begin
      DecodeDateTime(SerItm.FRealDate, Y, M, D, H, MN, S, Ms);
      Dt := StrToDateTime(FSeriesData[i].FSeriesItems[FSeriesData[i].Count - 1].Name, Fmt);
      DecodeDateTime(Dt, y1, M1, D1, h1, mn1, S1, ms1);
      SerItm.FRealDate := EncodeDateTime(Y, M1, D1, h1, mn1, S, ms1);
    end;

  end;

  procedure AssignBase;
  var
    i: integer;
  begin
    FSeriesData.Clear;
    for i := 0 to FContractionBase.Count - 1 do
    begin
      OrigSer := TSeries.Create;
      OrigSer.Assign(FContractionBase[i], -1, -1);
      FSeriesData.Add(OrigSer);
    end;
  end;

begin
  if Chart.NameType = ntGeneral then
    ShowGWError(msg_ContractGeneral);
  if (Rate = 1) and not(ContractType = ctIncremental) then
  begin
    ClearState(stInternalContraction);
    Exit;
  end;

  FContraction := Contraction;
  if ContractType = ctExponential then
  begin
    FContraction := Rate;
  end
  else
  begin
    begin
      if (Rate < FContraction) and (ContractType = ctExplicit) then
      begin
        ShowGWError(msg_DecreaseContraction);
      end;

      if ContractType = ctIncremental then
      begin
        FContraction := FContraction + Rate;
        Rate := FContraction;
      end
      else { Explisit }
      begin
        if Rate = FContraction then
          Exit;
        FContraction := Rate;
      end;
      AssignBase;
    end;
  end;

  TheSeries := TList<TSeries>.Create;
  try
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Ser := FSeriesData[i];
      NewSer := TSeries.Create;
      NewSer.FLeapdateCount := Ser.FLeapdateCount;
      NewSer.FWriter := Self;
      NewSer.FIndent := 0;
      NewSer.FExdent := 0;
      LastE := 0;
      LastCnt := 0;
      E := 0;
      Cnt := 0;
      Contracted := False;
      for j := Ser.FirstItem to Ser.LastItem do
      begin
        Contracted := False;
        inc(Cnt);
        V := Ser.FSeriesItems[j].FValue;
        E := E + V;
        if (j mod Rate = 0) then
        begin
          Contracted := True;
          LastE := E;
          LastCnt := Cnt;
          E := E / Cnt;
          SerItm := NewSer.AddItem;;
          SerItm.Assign(Ser.FSeriesItems[j]);
          SerItm.FOwner := NewSer;
          SerItm.FItems := NewSer.FSeriesItems;
          SerItm.FValue := E;
          E := 0;
          Cnt := 0;
        end;
      end;
      { OBS }
      G := ActiveGraph;
      if ((G is TCWBar) and not(boTruncReminder in TCWBar(G).Options)) or
        (G is TCWCurve) then
        if not Contracted and (NewSer.Count >= 2) then
        begin
          Expand(i);
          E := E + LastE;
          E := E / (Cnt + LastCnt);
          SerItm.FValue := E;
        end;
      if NewSer.Count < 2 then
      begin
        NewSer.Free;
        GoBackError;
        ShowGWError(msg_ContractInsufficient);
      end;
      TheSeries.Add(NewSer);
    end;
    PIndex := FScrollIndex;
    InternalClear;
    SetState(stInternalAction);
    try
      for i := 0 to TheSeries.Count - 1 do
        AddSeries(TheSeries[i], True);
      if ContractType = ctExponential then
        FContraction := Contraction;
      Execute;
    finally
      ClearState(stInternalAction);
      FScrollIndex := PIndex;
    end;
  finally
    TheSeries.Free;
  end;
end;

procedure TChartWriter.SetContraction(Value: integer);
begin
  if Chart.NameType = ntGeneral then
    ShowGWError(msg_ContractGeneral);
  if (Value < 0) or (FSeriesData.Count = 0) then
    Exit;
  FViewMode := vmNormal;
  FDynaSectStart := -1;
  FDynaSectEnd := -1;
  SetState(stUserContraction);
  try
    DoContractValues(Value, FContractionType);
  finally
    ClearState(stUserContraction);
  end;
  PostHist;
  DoRepaint;
end;

function TChartWriter.XFromName(AValue: string): integer;
{ Returns the X-coordinate of AValue }
var
  P: TPoint;
begin
  P := FLongestSeries.PosFromNamVal(AValue, ValueLow);
  Result := P.X;
end;

function TChartWriter.YFromName(AValue: string): integer;
{ Returns the Y-coordinate of AValue }
var
  P: TPoint;
begin
  P := FLongestSeries.PosFromNamVal(AValue, ValueHigh);
  Result := P.Y;
end;

function TChartWriter.GetGraphPrintRect: TRect;
{ GraphRect - GraphMargs }
begin
  Result := GraphRect;
  Result.Left := Result.Left + GraphMargins.Left;
  Result.Top := Result.Top + GraphMargins.Top;
  Result.Right := Result.Right - GraphMargins.Right;
  Result.Bottom := Result.Bottom - GraphMargins.Bottom;
end;

function TChartWriter.GetPosRect(SeriesCount: integer = -1): TRect;
var
  W: integer;
  G: TCWGraph;
  Y: integer;
  R: TRect;
  wd: integer;
  Cpr: Boolean;
  B : integer;
begin
  Result := GetGraphPrintRect;
  G := ActiveGraph;
  if not(G is TCWBar) then
    Exit;

  TCWBar(G).GetBarSpace(W, Cpr, B);
  W := W div 2;
  if FNamAx.IsXAxis then
  begin
    Result.Left := Result.Left + W;
    Result.Right := Result.Right - W - 1;
    if TCWBar(G).Compressing then
    begin
      { Do nothing }
    end
    else if TCWBar(G).BarStyle = bsCylinder then
    begin
      Result.Top := Result.Top + TCWBar(G).GetCylinderHatHeight;
    end
    else if TCWBar(G).BarStyle = bsCube then
      with G as TCWBar do
      begin
        R := Rect(0, 0, BarWidth, 0);
        Get3DWH(R, wd, Y);
        Result.Top := Result.Top + Y + 1;
      end;
  end
  else
  begin
    Result.Top := Result.Top + W;
    Result.Bottom := Result.Bottom - W - 1;
  end
end;

function TChartWriter.PosFromValue(AValue: single; AAxis: TValueAxis): integer;
{ Returns the X or Y coordinate of the Value axis computetd from AValue }
begin
  Result := AAxis.GetExactValuePos(AValue);
end;

function TChartWriter.PosFromDate(ADate: TDateTime): integer;
begin
  Result := FNamAx.PosFromDate(ADate);
end;

function TChartWriter.PosFromHour(ADate: TDateTime): integer;
begin
  Result := FNamAx.PosFromHour(ADate);
end;

function TChartWriter.PosFromMinute(ADate: TDateTime): integer;
begin
  Result := FNamAx.PosFromMinute(ADate);
end;

function TChartWriter.PosFromSecond(ADate: TDateTime): integer;
begin
  Result := FNamAx.PosFromSecond(ADate);
end;

function TChartWriter.PosFromNumber(ANumber: single): integer;
begin
  Result := FNamAx.PosFromNumber(ANumber);
end;

procedure TChartWriter.FontChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) or (FStates <> []) then
    Exit;
  with Sender as TFont do
   OnChange := nil;
  try
    RefreshGraph;
  finally
    with Sender as TFont do
     OnChange := FontChanged;
  end;
end;

procedure TChartWriter.CreateIDS;
var
  C : TCWChart;
  i : integer;
begin
   DeleteFromWList(Self);
   C := Chart;
   if C = nil then
     Exit;
   for I := 0 to C.Legends.Count-1 do
     begin
       if C.Legends.Items[i].Legend <> nil then
        C.Legends.Items[i].Legend.FWID := AddToWList(Self, C.Legends.Items[i].Legend);
     end;
   for I := 0 to C.SeriesDefs.Count-1 do
    if C.SeriesDefs[i].Graph <> nil then
     C.SeriesDefs[i].Graph.FWID := AddToWList(Self, C.SeriesDefs[i].Graph);
   if C.NameSectionDefs <> nil then
     C.NameSectionDefs.FWID := AddToWList(Self, C.NameSectionDefs);
   if C.ValueSectionDefs <> nil then
     C.ValueSectionDefs.FWID := AddToWList(Self, C.ValueSectionDefs);
end;

procedure TChartWriter.AddDsgnSeries(ANameType : TNameType; indx : integer);
var
 sl : TStringList;

 procedure AddGeneral;
 var
   i : integer;
   Cnt : integer;
   V : single;
   S : string;
 begin
   if Chart.ItemColors.Count > 1 then
     Cnt := Chart.ItemColors.Count
   else
     Cnt := 10;

   for I := 1 to Cnt do
     begin
        V := Indx*5 + Random(10);
        if V < 2 then
        V := 2;
        S := FLoatToStr(V,Fmt);
        if (Chart.ItemColors.Count > 1) and (Chart.ItemColors.Items[i-1].FItemName <> '') then
          sl.Values[Chart.ItemColors.Items[i-1].FItemName] := S
        else
          sl.Values[Chr(i+64)] := S;
     end;
 end;

 procedure AddDateSpan;
 var
   i : integer;
   Dt : TDateTime;
   X : integer;
 begin
    X := Random(1);
    if X = 0 then
     Dt := ToDay-182
    else
     Dt := ToDay-365;
    for I := 1 to 182 do
     begin
        sl.Values[DateToStr(Dt, Fmt)] := FloatToStr(Indx*5 + Random(10), Fmt);
        Dt := Dt + 1;
     end;
 end;

 procedure AddHourSpan;
 var
   i : integer;
   Dt : TDateTime;
   Y,M,D,H,MM,S,MS : word;
 begin
    Dt := Now;
    DecodeDateTime(Dt, Y, M, D, H, MM, S, MS);
    Dt := EncodeDateTime(Y, M, D, H, 0, 0, 0);
    IncHour(Dt, -24);
    for I := 1 to 24 do
     begin
        sl.Values[DateTimeToStr(Dt, Fmt)] := FloatToStr(Indx*5 + Random(10), Fmt);
        Dt := IncHour(Dt);
     end;
 end;

procedure AddMinuteSpan;
 var
   i : integer;
   Dt : TDateTime;
   Y,M,D,H,MM,S,MS : word;
 begin
    Dt := Now;
    DecodeDateTime(Dt, Y, M, D, H, MM, S, MS);
    Dt := EncodeDateTime(Y, M, D, H, MM, 0, 0);
    IncMinute(Dt, -60);
    for I := 1 to 60 do
     begin
        sl.Values[DateTimeToStr(Dt, Fmt)] := FloatToStr(Indx*5 + Random(10), Fmt);
        Dt := IncMinute(Dt);
     end;
 end;

 procedure AddSecondSpan;
 var
   i : integer;
   Dt : TDateTime;
   Y,M,D,H,MM,S,MS : word;
 begin
    Dt := Now;
    DecodeDateTime(Dt, Y, M, D, H, MM, S, MS);
    Dt := EncodeDateTime(Y, M, D, H, MM, S, 0);
    IncSecond(Dt, -60);
    for I := 1 to 60 do
     begin
        sl.Values[DateTimeToStr(Dt, Fmt)] := FloatToStr(Indx * 5 + Random(10), Fmt);
        Dt := IncSecond(Dt);
     end;
 end;

 procedure AddNumberSpan;
var
   i : integer;
 begin
    for I := 1 to 50 do
     begin
        sl.Values[IntToStr(i)] := FloatToStr(Indx*5 + Random(10), Fmt);
      end;
 end;

begin
  sl := TStringList.Create;
  sl.DefaultEncoding := TEncoding.Utf8;
  case ANameType of
    ntDateSpan: AddDateSpan;
    ntHourSpan: AddHourSpan;
    ntMinuteSpan: AddMinuteSpan;
    ntSecondSpan: AddSecondSpan;
    ntNumberSpan: AddNumberSpan;
    ntGeneral: AddGeneral;
  end;
  FDsgnData.Add(sl);
end;

procedure TChartWriter.RenderDesigner(Deleted : integer = 0);
var
  i : integer;
  Err : Boolean;

begin
   if not LiveGraphs or DsgnRealData then
     Exit;
   if Chart = nil then
     Exit;
   Clear;
   FDsgnData.Clear;
   Invalidate;
   Err := false;
   for I := 0 to Chart.SeriesDefs.Count-1 do
   begin
     if Chart.SeriesDefs[i].Graph = nil then
     begin
       Err := True;
       Break;
     end;
   end;
   if Err then
    Exit;
   for I := 0 to Chart.SeriesDefs.Count-1 do
   begin
       AddDsgnSeries(Chart.NameType, I);
   end;
   for I := 0 to FDsgnData.Count-1 do
   begin
     AddSeries(FDsgnData[i]);
   end;
   Execute;
end;


initialization
Fmt := TFormatSettings.Create;
WriterList := TWriterList.Create;
System.ReportMemoryLeaksOnShutdown := True;

finalization
WriterList.Free;

end.

