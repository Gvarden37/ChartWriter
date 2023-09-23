{**************************************************************************** }
{ Version 1.0.0, October 2023 }
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


{An overview of the ChartWriter code can be found in topic "Technical overview"
in the help file}


Unit ChartWriter;

interface

{$R *.res}

uses
  System.SysUtils, System.Generics.Collections, System.Classes, Windows,
  Vcl.Controls,
  System.Types, Vcl.Graphics, Math, Vcl.StdCtrls, System.Messaging,
  Vcl.AppEvnts, GraphUtil,
  Messages, System.UITypes, System.Rtti, GDIPUtil, GDIPapi, GDIPObj,
  Forms, Vcl.ExtCtrls, Data.DB;

const
  {Save image options}
  Img_GraphOnly = 1;
  Img_GraphAndLabels = 2;
  Img_All = 3;

  {Message constants}
  WM_SAVESELBM = wm_user + 200;
  WM_ENDEXEC = wm_user + 201;
  WM_REFRESHCHART = wm_user + 202;
  WM_ERROR = wm_user + 203;
  WM_LOADFILE = wm_user + 204;
  WM_AFTERBUILD = wm_user + 205;

  {Scale constants}
  c_HookSize = 5;
  c_HookMargin = 3;
  c_HookSpace = c_HookSize + c_HookMargin;
  c_LabelXMarg = 6; { Spaces between labels }
  c_QualifierMargin = 8;

  {Animation validation results}
  AN_OK = 0;
  AN_NoActiveGraph = 1;
  AN_DifferentGraphs = 2;
  AN_AnimationNotDefined = 3;
  AN_NoSpace = 4;

  {Minnimum text space needed for bar texts}
  MinBarTextSpaceVert = 30;
  MinBarTextSpaceHorz = 15;


  { TCWFileReader constants }
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

  {Legends}
  leg_BorderMarg = 4;
  leg_PointRad = 6;

  {Contraction error constants,delivered by CanContract}
  con_NoChart = 1;
  con_SpanChartOnly = 2;
  con_Paged = 3;

  { Property name constants, used in SaveTo- LoadFromFile }
  C_Chart = 'Chart';
  C_InnerLeftMargin = 'InnerLeftMargin';
  C_InnerTopMargin = 'InnerTopMargin';
  C_InnerRightMargin = 'InnerRightMargin';
  C_InnerBottomMargin = 'InnerBottomMargin';
  C_GraphLeftMargin = 'GraphLeftMargin';
  C_GraphTopMargin = 'GraphTopMargin';
  C_GraphRightMargin = 'GraphRightMargin';
  C_GraphBottomMargin = 'GraphBottomMargin';
  C_LiveResize = 'LiveResize';
  C_Graph = 'Graph';
  C_Title = 'Title';
  C_TitleAlignment = 'TitleAlignment';
  C_TitleFont = 'TitleFont';
  C_Name = 'Name';
  C_Ident = 'Ident';
  C_ImageFileName = 'ImageFileName';
  C_Color = 'Color';
  C_ValueAxis = 'ValueAxis';
  C_Visible = 'Visible';
  C_Font = 'Font';
  C_KeepFontColor = 'KeepFontColor';
  C_Pen = 'Pen';
  C_Brush = 'Brush';
  C_GraphBGColor = 'GraphBGColor';
  C_GraphBorders = 'GraphBorders';
  C_Animation = 'Animation'; {Pie and Curve}
  C_Animations = 'Animations'; {Bar only}
  C_AnimationSpeed = 'AnimationSpeed';
  C_AnimationPause = 'AnimationPause';
  C_AnGrow = 'anGrow';
  C_AnFlow = 'anFlow';
  C_ValueSpanFromData = 'ValueSpanFromData';
  C_ValueIntervals = 'ValueIntervals';
  C_ValueLow = 'ValueLow';
  C_ValueHigh = 'ValueHigh';
  C_SpanType = 'SpanType';
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
  C_NameQualifier = 'NameQualifier';
  C_ValueScale1 = 'ValueScale1';
  C_ValueScale2 = 'ValueScale2';
  C_NameScale = 'NameScale';
  C_ValueFloatUnit = 'ValueFloatUnit';
  C_CurveStyle = 'Style';
  C_CurveLineStyle = 'LineStyle';
  C_CurveLineWidth = 'LineWidth';
  C_CurveBaseLineValue = 'BaseLineValue';
  C_LineShape = 'LineShape';
  C_SmoothLines = 'SmoothLines';
  C_AreaOutline = 'AreaOutline';
  C_AreaOutlineColor = 'AreaOutlineColor';
  C_UseSeriesStyles = 'UseSeriesStyles';
  C_MinPointSpacing = 'MinPointSpacing';
  C_MaxPointSpacing = 'MaxPointSpacing';
  C_AlternativeGraph = 'AlternativeGraph';
  C_Categories = 'Categories';
  C_CurveBrush = 'AreaBrush';
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
  C_tcName = 'ctName';
  C_tcValue = 'ctValue';
  C_tcTitle = 'ctTitle';
  C_tcPercentage = 'ctPercentage';
  C_ShowQualifier = 'ShowQualifier';
  C_AutoSize = 'AutoSize';
  C_PieSize = 'PieSize';
  C_Slope = 'Slope';
  C_DoughnutSize = 'DoughnutSize';
  C_DiscDepth = 'DiscDepth';
  C_Style = 'Style';
  C_StartAngle = 'StartAngle';
  C_PieSliceSpacing = 'PieSliceSpacing';
  C_poPrintPercentages = 'poPrintPercentages';
  C_poPrintValues = 'poPrintValues';
  C_poPrintNames = 'poPrintNames';
  C_poPrintSeriesTitles = 'poPrintSeriesTitles';
  C_poClientBorder = 'poClientBorder';
  C_poTextBackground = 'poTextBackground';
  C_poPrintTitleInDoughnut = 'poPrintTitleInDoughnut';
  C_poPinText = 'poPinText';
  C_poAllowImages = 'poAllowImages';
  C_SeriesTitleFont = 'SeriesTitleFont';
  C_Scrollable = 'Scrollable';
  C_MouseTimeFormat = 'MouseTimeFormat';
  C_NumSpanPrecision = 'NumSpanPrecision';
  C_AllowImages = 'AllowImages';
  C_PointWidth = 'PointWidth';
  C_ScrollingBarWidth = 'ScrollingBarWidth';
  C_toName = 'toName'; { Text orientation }
  C_toValue = 'toValue';
  C_TextTiltThreshold = 'TextTiltThreshold';
  C_TextTilting = 'TextTilting';
  C_veNameLabels = 'veNameLables';
  C_veValueLabels = 'veValueLables';
  C_veNameDividerLines = 'veNameDividerLinesLines';
  C_veValueDividerLines = 'veValueDividerLines';
  C_DrawBaseLine = 'DrawBaseLine';
  C_ValuePrecision = 'ValuePrecision';
  C_ValuePrecision1 = 'ValuePrecision1';
  C_ValuePrecision2 = 'ValuePrecision2';
  C_Percentages = 'Percentages';
  C_slMean = 'slMean';
  C_slMedian = 'slMedian';
  C_slRegression = 'slRegression';
  C_slMode = 'slMode';
  C_DividerLinePen = 'DividerLinePen';
  C_OverflowAction = 'OverflowAction';
  C_WallWidth = 'WallWidth';
  C_GradientWall = 'GradientWall';
  C_WallColor = 'WallColor';
  C_WallBorderColor = 'WallBorderColor';
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
  C_PointName = 'PointName';
  C_Transparency = 'Transparency';
  C_VertMargins = 'VertMargins';
  C_Text = 'Text';
  C_Qualifier = 'Qualifier';
  C_ShowLabels = 'ShowLabels';
  C_MinLabelSpacing = 'MinLabelSpacing';
  C_ShowDividerLines = 'ShowDividerLines';
  C_StatLine = 'StatLine';
  C_SMAPeriods ='SMAPeriods';
  C_StatLineWidth = 'StatLineWidth';
  C_StatBackgroundBlending = 'StatBackgroundBlending';

  { Render error codes }
  R_NoActiveGraph = 1;
  R_NoData = 2;
  R_Updating = 3;
  R_NoChart = 4;
  R_Unknown = 0;

  {Style conflicting error}
  S_NeighborWithOthers = 1;
  S_PointsWithOthers = 2;
  S_OthersWithNeighbor = 3;
  S_OthersWithPoints = 4;

type
  TChartWriter = class;

  TWriterListElement = class
    Writer : TChartWriter;
    Obj : TObject;
  end;
  TWriterList = TObjectList<TWriterListElement>;
  {The Writer list is used to deliver Writer reference
  to the chart objects. This happens when a chart gets the focus
  It is also used to check if objects are used by more than one chart writer.
  Relevent methods: AddToVList and CreateIDs}

  TCWException = class(Exception)
    Errorcode: integer;
  end;

  { Point coordinates }
  TPointArray = TList<TPoint>;

  {Stores bar item info}
  TTrackBar = record
    BarRect: TRect;
    NormalRect: TRect;
    VisibleRect: TRect;
    SeriesIndex, SeriesItmIndex: integer;
  end;

  TTrackBars = TList<TTrackBar>;

  { Stotes pie slice info}
  TPieSlice = record
  private
    FSeriesIndex, FSeriesItmIndex: integer;
  public
    Center: TPoint;
    TextAngle : single;
    TextBounds : TRect;
    StartAngle, EndAngle: single;
    Color: TColor;
    Percentage: single;
  end;

  TTrackPies = TList<TPieSlice>;

  TSeriesInfo = record
    { Collects info in PointsFromRuler/ProcessRulers and other mouse oriented operations }
    SeriesIndex, SeriesItemIndex: integer;
  end;

  TSeriesInfoItems = array of TSeriesInfo;

  { Mode function return values. Statistics }
  TModeNumber = record
    Number: string;
    Cnt: integer;
  end;

  TModeNumbers = array of TModeNumber;

  TSeries = class;
  TCWChart = class;

  TGraphObject = class
    { Base class for graph objects. Internal use }
  private
    FWriter: TChartWriter;
    function GetChart : TCWChart;
    property Writer: TChartWriter read FWriter;
    property Chart : TCWChart read GetChart;
  end;

  { Datasets }
  TData = TObjectList<TSeries>;
  TSeriesItem = class;

  TSeriesItems = TObjectList<TSeriesItem>;

  TSeriesItem = class(TGraphObject)
    { Series items }
  private
    FItems: TSeriesItems;
    FLeapDummy: Boolean;
    FName: string;
    FOrigName: string; { Used by save to file }
    FOwner: TSeries;
    FRealDate: TDateTime;
    FRealVal: single;
    FReminderName: string;
    FSpace: Boolean;
    FValue: single;
    FVisible : Boolean; {Internal use by CreateSpan}

    function GetBarRect: TRect;
    function GetIndex: integer;
    function GetItemIndex: integer;
    function GetName: string;
    function GetPieSlice: TPieSlice;
    function GetPointPos: TPoint;
    function GetPst : single;
    function GetSeriesIndex: integer;
    function GetVal : single;
    property Index: integer read GetIndex; { In span }

  public
    procedure Assign(Source: TSeriesItem);
    property BarRect: TRect read GetBarRect;
    property ItemIndex: integer read GetItemIndex; { 0-based }
    property Name: string read GetName write FName; {Name of point}
    property PieSlice: TPieSlice read GetPieSlice;
    property PointPos: TPoint read GetPointPos; {Point coordinate}
    property Pst : single read GetPst; {Calc pst}
    property RealDate: TDateTime read FRealDate;
    {Stores the actual date in time spanned series}
    property SeriesIndex: integer read GetSeriesIndex;
    property Value: single read GetVal write FValue; {Point value}
  End;

  {Enumerrated types}
  TAnimation = (anFlow, anGrow);
  TAnimations = set of TAnimation;
  TAnimationSpeed = (asFast, asMediumFast, asMediumSlow, asSlow);
  TAutoSections = (autNotUsed, autDays, autDates, autWeeks, autMonths, autYears);
  TAxisOrientation = (alBottomLeft, alBottomRight, alLeftTop, alTopLeft, alTopRight,
    alRightTop, alLeftBottom, alRightBottom);
  TAxisPosition = (apLeft, apTop, apRight, apBottom);
  TBarLayout = (blStacked, blSideBySide);
  TBarOption = (boBaseLine, boOutLines, boText, boBarImages, boTruncReminder);
  TBarOptions = set of TBarOption;
  TBarStyle = (bsFlat, bsCube, bsCylinder, bsGradientWidth, bsGradientLength);
  TCaptionLayout = (clSameSideAsLabels, clOppositeSideOfLabels);
  TCaptionType = (ctDefined, ctName, ctValue, ctNameAndValue);
  TColorUsage = (cuOnSeries, cuOnItems);
  TContentFlow = (cfTopBottom, cfLeftRight);
  TContractionType = (ctExponential, ctIncremental, ctExplicit);
  TCurvelineStyle = (lsSolid, lsDot, lsDash);
  TCurveStyle = (csLine, csClientArea, csBaseLineArea, csNeighborArea, csPoints);
  TDateTimeTemplate = (ttNotUsed, ttMonthTemplate, ttWeekTemplate, ttDateTemplate,
    ttHourTemplate, ttMinuteTemplate, ttSecondTemplate);
  TGraphBorders = (gbAxis, gbAllSides, gbNone);
  TLabelKind = (lkName, lkValue, lkValue2, lkNameSection, lkValueSection, lkQualifier, lkInfo);
  TLegendAlignment = (laLeftOrTop, laCenter, laRightOrBottom);
  TLegendAnchoring = (anLeftOutside, anLeftInside, anTopOutside, anTopInside,
    anRightOutside, anRightInside, anBottomOutside, anBottomInside, anSeriesOutside, anPointInside);
  TLegendBullets = (lbNone, lbSquare, lbCircle, lbLine);
  TLegendContent = (coSeriesTitle, coValue, coName, coNameSpan);
  TLegendContents = set of TLegendContent;
  TLineShape = (lsStraight, lsBezier, lsStep);
  TMouseInfo = (miName, miValue, miBoth, miNone);
  TMousePrecision = (mpHigh, mpMedium, mpLow);
  TNameSectionType = (stUndefined, stLiterals, stAutoSections, stDateTimeTemplate);
  TNameType = (ntMonthSpan, ntDateSpan, ntHourSpan, ntMinuteSpan, ntSecondSpan, ntNumberSpan,
    ntGeneral, ntCategory);  TSpanType = ntMonthSpan..ntNumberSpan;
  TOverflowAction = (ovContraction, ovCompression, ovScrolling, ovNone);
  TPieOption = (poPrintPercentages, poPrintNames, poPrintValues, poPrintSeriesTitles,
    poPrintTitlesInDoughnut, poTextBackground, poPinText, poClientBorder, poAllowImages);
  TPieOptions = set of TPieOption;
  TPieStyle = (psFlat, psDisc);
  TPointLocating = (plNameValue, plIndexes, plMaxValue, plMinValue, plEvent);
  TPointMarkers = (pmNone,pmSmallBall, pmBigBall, pmSmallConcentric, pmBigConcentric, pmDot, pmText, pmOwnerDraw);
  TPointOption = (poShowConnectionLine, poThinConnectionLine, poEnlargePoint);
  TPointOptions = set of TPointOption;
  TRulerPoints = array of integer;
  TRulers = (ruNames, ruValues, ruBoth, ruNone);
  TSaveFormat = (sfDataOnly, sfDataExtended, sfRich);
  TSaveTimeType = (ttLocal, ttUnix, ttISO8601);
  TScaleType = (stNameScale, stValueScale1, stValueScale2);
  TScrollType = (stNext, stPrev, stNextPage, stPrevPage, stFirst, stLast);
  TSectionElement = (seText, seLine);
  TSectionType = (stSection, stLine);
  TSelectionExec = (seZoom, seEvent, seOther);
  TSpaceResponse =(srAccept, srRefuseAbort, srRefuseExcept);
  TState = (stUpdating, stZoomed, stInternalContraction, stUserContraction,
    stInternalAction, stExecuting, stLabelFreqs, stPainting, stActivating,
    stAnimating, stInitAnimation, stAnimationPause, stResumeAnimation, stLimbo, stOverflow);
  TStates = set of TState;
  TStatLine = (slNone, slMean, slMedian, slRegression, slMode, slSMA);
  TTextContent = (tcValue, tcName, tcTitle, tcPercentage);
  TTextContents = set of TTextContent;
  TTextOrientation = (toName, toValue, toSection);
  TTextOrientations = set of TTextOrientation;
  TUpdateKind = (ukLabelFreq, ukRestrict, ukScroll, ukPaint);
  TUpdateKinds = Set of TUpdateKind;
  TValueFormat = (vtNone, vtSeparated, vtAbbreviated);
  TValueScaleNumber = (vsValueScale1, vsValueScale2, vsNone);
  TViewMode = (vmNormal, vmSelecting, vmSelected, vmHinting);

  TAxisObject = class;
  TCWCategory = class;
  TCWCategories = class;

  TIntPointArray = array of TPoint;

  TInfo = record
  {Stores info used to display ruler and mouse hints}
    Text: TStringList;
    Items: TList<TSeriesItem>;
    Colors: TList<TColor>;
  end;

  TAnimInfo = record
  { Used transfer animation states between graphs and the DoAnimation procedure }
    AnimInit : Boolean; {Used by pie to draw an outer ellipse}
    LastAngle : single;
    LastColor : TColor;
    LastPt: TPoint;
    LastXY: integer;
    NextAngle : single;
    NextHorz: integer;
    NextItem : integer;
    NextSeries: integer;
    NextVert: integer;
    Paused: Boolean;
    StartPause: Boolean;
    Stopped: Boolean;
  end;

  TAnimationTuner = record
  {Base values used by the DoAnimation procedure}
    Delay: integer;
    { Delay frequences }
    FastFreq: integer;
    MediumFastFreq: integer;
    MediumSlowFreq: integer;
    SlowFreq: integer;
    AngleIncrement : single;
  end;

  {Class used to handle overlapping rectangles. Used by Pies and Bars when printing text}
  TMoveDir = (mdUp, mdLeft, mdRight, mdDown);
  {The directikon to move a rectangle when an overlap occurs}
  TOverlaps = class
  private
     FRects : TList<TRect>;
     FOuter : TRect; {The outer bounds that cannot be crossed}
     FMoveDir : TMoveDir;
  public
     procedure Add(ARect : TRect);
    constructor Create(OutBounds : TRect; MoveDir : TMoveDir);
    destructor Destroy; override;
    function GetRect(ARect : TRect) : TRect;
    function MoveIt(ARect : TRect; var MovedRect : TRect) : Boolean;
    function Overlapped(ARect : TRect; var OverlappedRect : TRect) : Boolean;
    property MoveDir : TMoveDir read FMoveDir write FMoveDir;
  end;


  { TSection -----------------------------------------------------------------
  Section item. Stores the start and end valeus and captions}

  TSection = class(TGraphObject)
  private
    FEndVal: string;
    FIndex: integer;
    FIsAuto: Boolean;
    FIsReduced: Boolean;
    FLongCaption: string;
    FOwner: TAxisObject;
    FSectionType: TSectionType;
    FShortCaption: string;
    FStartVal: string;

    function GetSectionGraphRect: TRect; { Rect within graph }
    function GetSectionLabelRect: TRect; {Caption rect}

  public
    constructor Create;
    property EndVal: string read FEndVal;
    property LongCaption: string read FLongCaption write FLongCaption;
    property SectionGraphRect: TRect read GetSectionGraphRect;
    property SectionLabelRect: TRect read GetSectionLabelRect;
    property SectionType: TSectionType read FSectionType;
    property ShortCaption: string read FShortCaption write FShortCaption;
    property StartVal: string read FStartVal;
  end;

  TSections = TObjectList<TSection>; {Section items}

  {Events}
  TContractionEvent = procedure(Sender: TObject; MaxPoints: integer) of object;
  TDrawBarEvent = procedure(Sender: TObject; SeriesIndex, ItemIndex: integer;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawCurveEvent = procedure(Sender: TObject; SeriesIndex: integer; ACanvas:
    TCanvas; var Handled: Boolean) of object;
  TDrawCurveLineEvent = procedure(Sender: TObject;
    FromPoint, ToPoint: TPoint; SeriesIndex, ItemIndex: integer;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawGraphEvent = procedure(Sender: TObject; Canvas: TCanvas) of object;
  TDrawLabelEvent = procedure(Sender: TObject; Scale: TScaleType; ALabel: string;
    ATime: TDateTime; APosition: TPoint; ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawLabelsEvent = procedure(Sender: TObject; LabelKind: TLabelKind;
    ACanvas: TCanvas) of object;
  TDrawPieSliceEvent = procedure(Sender: TObject; SeriesIndex, ItemIndex: integer;
    AText : string; ATextRect : TRect; ACanvas: TCanvas; var Handled: Boolean) of object;
  TDrawPointEvent = procedure(Sender: TObject; SeriesIndex, ItemIndex: integer;
    APosition: TPoint; ACanvas: TCanvas) of object;
  TDrawSectionEvent = procedure(Sender: TObject; Scale: TScaleType;
    APosition: TPoint; ASection: TSection; ASectionElement: TSectionElement;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TLegendContentEvent= procedure(Sender: TObject; const ASeriesIndex: integer;
    var Content : string) of object;
  TLoadFileEvent = procedure(Sender: TObject; var AFileName : string) of object;
  TLocatePointEvent = procedure(Sender: TObject; var SeriesIndex: integer;
    var ItemIndex: integer) of object;
  TMeasureLabelEvent = procedure(Sender: TObject; ALabelKind: TLabelKind;
    var Widest, Tallest: string) of object;
  TMouseInfoEvent = procedure(Sender: TObject; Info: TInfo; Position: TPoint;
    ACanvas: TCanvas; var Handled: Boolean) of object;
  TMouseItemEvent = procedure(Sender: TObject; SeriesIndex, ItemIndex: integer) of object;
  TQuerySpaceEvent= procedure(Sender: TObject; const QueryResult: integer;
    var Response : TSpaceResponse) of object;
  TTitleEvent = procedure(Sender: TObject; const ASeriesIndex: integer;
    var Title : string) of object;

  TCWLegend = class;
  TCWLegends = class;

  TRectInfo = record
  { Used by Legends}
    R: TRect;
    P: TPoint;
    Index: integer;
    Leg: TCWLegend;
  end;

  TDataAttributes = record
  {Used by GetDataAttributes}
     Graph1 : string;
     Graph2 : string;
     HighValue : single;
     ItemAttributes : Boolean;
     ItemCount : integer;
     ItemImages : Boolean;
     LowValue : single;
     MainTitle : Boolean;
     NameType : TNameType;
     Percentages : Boolean;
     SeriesColors : Boolean;
     SeriesCount : integer;
     SeriesTitles : Boolean;
     TimeType : TSaveTimeType;
  end;
  TLegendRects = TList<TRectInfo>;

  TLegContentList = TObjectList<TStringList>;
  {Stores the texts collected by CreateLegendContent}

  TCWGraph = class(TComponent)
  {Base class graphs}
  private
    FAnimation : Boolean;
    FAnimationPause : Integer;
    FAnimationSpeed: TAnimationSpeed;
    FFont : TFont;
    FGDIP: TGPGraphics;
    FKeepFontColor : Boolean;
    FPaintItemIndex : integer;
    FPaintSeriesIndex : integer;
    FWID : TWriterListElement;
    FWriter: TChartWriter; {Not used? FWID instead}
    function GetActiveColor : TColor;
    function GetCanvas: TCanvas;
    function GetChart : TCWChart;
    function GetInternalActiveColor(SeriesIndex, ItemIndex : integer): TColor; virtual;
    function GetSeries(Value : integer) : TSeries;
    function GetWriter : TChartWriter;
    procedure FontChange(Sender : TObject);
    procedure SetActiveColor(const Value : TColor);
    procedure SetAnimation(const Value : Boolean);
    procedure SetInternalActiveColor(SeriesIndex, ItemIndex : integer; Value : TColor); virtual;
    procedure SetKeepFontColor(const Value : Boolean);
    property InternalActiveColor[SeriesIndex, ItemIndex : integer] : TColor
     read GetInternalActiveColor write SetInternalActiveColor;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Animation: Boolean read FAnimation write SetAnimation default False;
    property Canvas: TCanvas read GetCanvas;
    property GDIP: TGPGraphics read FGDIP;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InChart(SeriesIndex : integer = -1) : Boolean;
    procedure Draw; virtual;
    property ActiveColor : TColor read GetActiveColor write SetActiveColor;
    property Chart : TCWChart read GetChart;
    property Series[Index : integer] : TSeries read GetSeries;
    property Writer: TChartWriter read GetWriter;

  published
    property AnimationPause : integer read FAnimationPause write FAnimationPause default 0;
    property AnimationSpeed: TAnimationSpeed read FAnimationSpeed
      write FAnimationSpeed default asMediumFast;
    property Font : TFont read FFont write FFont;
    property KeepFontColor : Boolean read FKeepFontColor write SetKeepFontColor;
  end;

  TChartListItem = class
  {Used with TChartList help object}
  private
    FChart : TCWChart;
    FGraph : TCWGraph;
    FTitle : string;
  public
    property Chart : TCWChart read FChart;
    property Graph : TCWGraph read FGraph;
    property Title : string read FTitle;
  end;
  TChartListItems = TObjectList<TChartListItem>;

  TChartList = class
  {Helper object, helping to organize and prsent multiple charts}
   private
    FItemIndex : integer;
    FItems : TChartListItems;
    FPrevChart : TChartListItem;
    FWriter : TChartWriter;
    FOnChange : TNotifyEvent;
    function GetCount : integer;
    function GetItems(Index : integer) : TChartListItem;
    procedure Organize;
    procedure SetItemIndex(const Value : integer);
   public
    constructor Create;
    destructor Destroy; override;
    function CanMoveNext : Boolean;
    function CanMovePrev : Boolean;
    function IndexOf(AChart : TCWChart; AGraph :TCWGraph) : integer;
    procedure Add(AChart : TCWChart);
    procedure Next;
    procedure Prev;
    property Count : integer read GetCount;
    property ItemIndex : integer read FItemIndex write SetItemIndex;
    property Items[Index:Integer] : TChartListItem read GetItems;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  TInternalChartList = TList<TCWChart>;
  {Intermediate storage for TChartList data}
  TCWNameSectionDefs = class;
  TCWValueSectionDefs = class;
  TCWValueScale = class;

  TCWAxisGraph = class(TCWGraph)
  {Base for graphs that are rendered wihin an axis system: Curves and bars}
  private
    FAnimationBooster: integer;
    FAnimations: TAnimations;
    FDrawBaseline : Boolean;
    FHorzCounter: integer;
    FMaxPointSpacing: integer;
    FSMAPeriods : integer;
    FStatLine: TStatLine;
    FStatLineWidth : integer;
    FStatBackgroundBlending : integer;
    FVertCounter: integer;

    function CalculateSMA(SeriesIndex : integer; Period : integer) : TPointArray;
    function GetMaxPointSpacing: integer; virtual; abstract;
    function GetMinPointSpacing: integer; virtual; abstract;
    function GetValueScale : TCWValueScale;
    procedure DrawStatLine(GDIP : TGPGraphics; ASource : TSeries);
    procedure SetAnimationBooster(const Value: integer);
    procedure SetAnimations(const Value: TAnimations);
    procedure SetMaxPointSpacing(const Value: integer);
    procedure SetMinPointSpacing(const Value: integer); virtual; abstract;
    procedure SetSMAPeriods(const Value : integer);
    procedure SetStatLine(const Value: TStatLine);
    procedure SetStatLineWidth(const Value : integer);
    procedure SetStatBackgroundBlending(const Value : integer);

  protected
    property AnimationBooster: integer read FAnimationBooster
      write SetAnimationBooster default 0; {published for bars}
    property Animations: TAnimations read FAnimations write SetAnimations
      default []; {published for barts}
    property MaxPointSpacing: integer read GetMaxPointSpacing
      write SetMaxPointSpacing default 0;
    property MinPointSpacing: integer read GetMinPointSpacing
      write SetMinPointSpacing;
  public
    constructor Create(AOwner: TComponent); override;
    function QuerySpace(ItemCount : integer=-1): integer; virtual;
    function SeriesCount : integer;
    property ValueScale : TCWValueScale read GetValueScale;
  published
    property DrawBaseLine : Boolean read FDrawBaseline write FDrawBaseline default false;
    property SMAPeriods : integer read FSMAPeriods write SetSmaPeriods default 0;
    property StatBackgroundBlending : integer read FStatBackgroundBlending
      write SetStatbackgroundBlending default 0;
    property StatLine: TStatLine read FStatLine write SetStatLine
      default slNone;
    property StatLineWidth: integer read FStatLineWidth write SetStatLineWidth default 1;
  end;

  {Curve styles}
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
    function IndexOfStyle(const AStyle : TCurveStyle) : integer;
    procedure ApplyOnAll(AStyle : TCurveStyle; ALineStyle : TCurveLineStyle; ALineWidth : integer);
    procedure Assign(Source: TPersistent); override;
    property Items[AIndex: integer]: TCWSeriesStyle read GetItem write SetItem;
  end;

  TCWSeriesStyle = class(TCollectionItem)
  private
    FLineStyle: TCurvelineStyle;
    FLineWidth: integer;
    FStyle: TCurveStyle;
    FSeriesTitle: string;
    procedure SetLineStyle(const Value: TCurvelineStyle);
    procedure SetLineWidth(const Value: integer);
    procedure SetStyle(const Value: TCurveStyle);
    procedure SetSeriesTitle(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property LineStyle: TCurvelineStyle read FLineStyle write SetLineStyle
      default lsSolid;
    property LineWidth: integer read FLineWidth write SetLineWidth default 1;
    property SeriesTitle: string read FSeriesTitle write SetSeriesTitle;
    property Style: TCurveStyle read FStyle write SetStyle default csLine;
  end;

  TCWCategories = class(TOwnedCollection)
  private
    function GetItem(AIndex: integer): TCWCategory;
    procedure SetItem(AIndex: integer; const Value: TCWCategory);
  protected
    procedure Update(Item : TCollectionItem); override;
  public
    function Add: TCWCategory;
    function IndexOf(const CategoryName: string): integer;
    function GetColor(const CategoryName: string): TColor;
    procedure Assign(Source: TPersistent); override;
    procedure SetColor(const ItemName: string; AColor: TColor);
    property Items[AIndex: integer]: TCWCategory read GetItem write SetItem; default;
  end;

  TCWCategory = class(TCollectionItem)
  private
    FCategoryName: string;
    FColor: TColor;
    FImage : TPicture;
    FImageFileName : string;
    function GetColor: TColor;
    procedure SetCategoryName(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetImage(const Value : TPicture);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CategoryName: string read FCategoryName write SetCategoryName;
    property Color: TColor read GetColor write SetColor default clBlack;
    property Image : TPicture read FImage write SetImage;
  end;

  TCWCurve = class(TCWAxisGraph)
  private
    FAreaBrush: TBrush; {Neighbor polygon only}
    FAreaOutline : Boolean;
    FAreaOutlineColor : TColor;
    FBaseLineValue: single;
    FBeaconIndex: integer;
    FBeaconPoints: Boolean; {"Rolling" points, following the mouse}
    FFirstBaseLine : Boolean;
    FGPen : TGPPen;
    FLineShape : TLineShape;
    FLineStyle: TCurvelineStyle;
    FLineWidth: integer;
    FMinPointSpacing: integer; { Minimun unit space before Contraction takes place }
    FPointMarkers: TPointMarkers;
    FPointWidth: integer;
    FSeriesStyles: TCWSeriesStyles;
    FSmoothLines : Boolean;
    FStyle: TCurveStyle;
    FOnDrawCurve: TDrawCurveEvent;
    FOnDrawCurveLine: TDrawCurveLineEvent;
    FOnDrawPoint: TDrawPointEvent;
    FOnMouseEnterPoint: TMouseItemEvent;
    FOnMouseLeavePoint: TMouseItemEvent;
    function BeaconsActive: Boolean;
    function GetActiveLineStyle(ASeries : TSeries): TCurvelineStyle;
    function GetActiveLineWidth(ASeries: TSeries): integer;
    function GetActiveStyle(ASeries: TSeries): TCurveStyle;
    function GetInternalActiveColor(SeriesIndex, ItemIndex : integer): TColor; override;
    function GetLineStyle: TCurvelineStyle;
    function GetLineWidth: integer;
    function GetMaxPointSpacing: integer; override;
    function GetMinPointSpacing: integer; override;
    function GetStyle: TCurveStyle;
    function GetStyleIndex(ASeries: TSeries): integer;
    function GetUseSeriesStyles: Boolean;
    procedure BrushChanged(Sender: TObject);
    procedure DoDrawNeighbors;
    procedure DrawNeighbors(Serie1, Serie2, NeighborIndex: integer);
    procedure DrawTheLine(ASource : TSeries; Indx: integer;
      x1, y1, x2, y2: integer; StatLine : Boolean = false);
    procedure DrawBezierCurve(ASource : TSeries);
    procedure DrawBezierBaselineArea(ASource : TSeries; Pts : TPointDynArray);
    procedure SetActiveLineStyle(ASeries : TSeries; Value: TCurvelineStyle);
    procedure SetActiveLineWidth(ASeries: TSeries; Value: integer);
    procedure SetActiveStyle(ASeries: TSeries; Value: TCurveStyle);
    procedure SetAreaOutline(const Value : Boolean);
    procedure SetAreaOutlineColor(const Value : TColor);
    procedure SetBaseLineValue(const Value: single);
    procedure SetBeaconPoints(const Value: Boolean);
    procedure SetInternalActiveColor(SeriesIndex, ItemIndex : integer;
      Value : TColor); override;
    procedure SetLineShape(const Value : TLineShape);
    procedure SetLineStyle(const Value: TCurvelineStyle);
    procedure SetLineWidth(const Value: integer);
    procedure SetMinPointSpacing(const Value: integer); override;
    procedure SetPointMarkers(const Value: TPointMarkers);
    procedure SetPointWidth(const Value: integer);
    procedure SetSmoothLines(const Value : Boolean);
    procedure SetStyle(const Value: TCurveStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanApplyStyle(AStyle : TCurveStyle; InSeriesStyles : Boolean) : integer;
    function QuerySpace(ItemCount : integer = -1) : integer; override;
    procedure Draw; override;
    property ActiveLineStyle[ASeries: TSeries]: TCurvelineStyle
      read GetActiveLineStyle write SetActiveLineStyle;
    property ActiveLineWidth[ASeries: TSeries]: integer
      read GetActiveLineWidth write SetActiveLineWidth;
    property ActiveStyle[ASeries: TSeries]: TCurveStyle
      read GetActiveStyle write SetActiveStyle;
    property UseSeriesStyles: Boolean read GetUseSeriesStyles;
  published
    property Animation;
    property AreaBrush: TBrush read FAreaBrush write FAreaBrush;
    property AreaOutline : Boolean read FAreaOutline write SetAreaOutline default false;
    property AreaOutlineColor : TColor read FAreaOutlineColor write SetAreaOutlineColor default clBlack;
    property BaseLineValue: single read FBaseLineValue write SetBaseLineValue;
    property BeaconPoints: Boolean read FBeaconPoints write SetBeaconPoints default False;
    property LineShape : TLineShape read FLineShape write SetLineShape default lsStraight;
    property LineStyle: TCurvelineStyle read GetLineStyle write SetLineStyle default lsSolid;
    property LineWidth: integer read GetLineWidth write SetLineWidth default 1;
    property MaxPointSpacing;
    property MinPointSpacing default 2;
    property OnDrawCurve: TDrawCurveEvent read FOnDrawCurve write FOnDrawCurve;
    property OnDrawCurveLine: TDrawCurveLineEvent read FOnDrawCurveLine write FOnDrawCurveLine;
    property OnDrawPoint: TDrawPointEvent read FOnDrawPoint write FOnDrawPoint;
    property OnMouseEnterPoint: TMouseItemEvent read FOnMouseEnterPoint write FOnMouseEnterPoint;
    property OnMouseLeavePoint: TMouseItemEvent read FOnMouseLeavePoint write FOnMouseLeavePoint;
    property PointMarkers: TPointMarkers read FPointMarkers write SetPointMarkers default pmNone;
    property PointWidth: integer read FPointWidth write SetPointWidth default 5;
    property SeriesStyles: TCWSeriesStyles read FSeriesStyles write FSeriesStyles;
    property SmoothLines : Boolean read FSmoothLines write SetSmoothLines default true;
    property Style: TCurveStyle read GetStyle write SetStyle default csLine;
  end;

  TCWBar = class(TCWAxisGraph)
  private
    FAutoSize : Boolean;
    FBarStyle: TBarStyle;
    FBarWidth: integer;
    FBaseLineValue: single;
    FCubeAngle: integer;
    FCubeDepth: integer;
    FItemSpacing: integer;
    FLayout: TBarLayout;
    FOptions: TBarOptions;
    FOrigBarWidth : integer;
    FOverlaps : TOverlaps;
    FScrollingBarWidth: integer;
    FSeriesSpacing: integer;
    FShowQualifier : Boolean;
    FTextContents : TTextContents;
    FOnDrawBar: TDrawBarEvent;
    FOnMouseEnterBar: TMouseItemEvent;
    FOnMouseLeaveBar: TMouseItemEvent;
    function Compressing: Boolean;
    function Get3DBarWidth: integer;
    function GetBarStyle: TBarStyle;
    function GetBarWidth: integer;
    function GetCylinderHatHeight : integer;
    function GetInternalActiveColor(SeriesIndex, ItemIndex :Integer): TColor; override;
    function GetMaxPointSpacing: integer; override;
    function GetMinPointSpacing: integer; override;
    function GetScrollingBarWidth : integer;
    function GetSeriesSpacing: integer;
    function GetTextQualifier : string;
    procedure DoDraw;
    procedure Get3DDims(ARect: TRect; var TopLeftPt, TopRightPt,
      BottomLeftPt, BottomRightPt: TPoint);
    procedure Get3DWH(ARect: TRect; var AWidth, AHeight: integer);
    procedure GetCathesus(ARect : TRect; var HorzCath, VertCath: integer);
    procedure SetAutoSize(const Value : Boolean);
    procedure SetBarStyle(const Value: TBarStyle);
    procedure SetBarWidth(const Value: integer);
    procedure SetBaseLineValue(const Value: single);
    procedure SetCubeAngle(const Value: integer);
    procedure SetCubeDepth(const Value: integer);
    procedure SetInternalActiveColor(SeriesIndex, ItemIndex : integer;
      Value : TColor); override;
    procedure SetItemSpacing(const Value: integer);
    procedure SetLayout(const Value: TBarLayout);
    procedure SetMinPointSpacing(const Value: integer); override;
    procedure SetOptions(const Value: TBarOptions);
    procedure SetScrollingBarWidth(const Value: integer);
    procedure SetSeriesSpacing(const Value: integer);
    procedure SetShowQualifier(const Value : Boolean);
    procedure SetTextContents(const Value : TTextContents);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetBarSpace(var BlockWidth: integer; var Compressed: Boolean;
      var BarWidth : integer; ItemCount : integer = -1): Boolean;
    function Is3D: Boolean;
    function QuerySpace(ItemCount : integer = -1) : integer; override;
    procedure Draw; override;
    property MaxPointSpacing;
    property MinPointSpacing;
    property TextQualifier: string read GetTextQualifier;

  published
    property AnimationBooster;
    property Animations;
    property AutoSize : Boolean read FAutoSize write SetAutoSize;
    property BarStyle: TBarStyle read GetBarStyle write SetBarStyle default bsFlat;
    property BarWidth: integer read GetBarWidth write SetBarWidth default 20;
    property BaseLineValue: single read FBaseLineValue write SetBaseLineValue;
    property CubeAngle: integer read FCubeAngle write SetCubeAngle default 45;
    property CubeDepth: integer read FCubeDepth write SetCubeDepth default 0;
    property ItemSpacing: integer read FItemSpacing write SetItemSpacing default 3;
    property Layout: TBarLayout read FLayout write SetLayout default blSideBySide;
    property Options: TBarOptions read FOptions write SetOptions default [];
    property ScrollingBarWidth: integer read GetScrollingBarWidth
      write SetScrollingBarWidth default 20;
    property SeriesSpacing: integer read GetSeriesSpacing write SetSeriesSpacing default 0;
    property ShowQualifier : Boolean read FShowQualifier write SetShowQualifier;
    property TextContents : TTextContents read FTextContents
      write SetTextContents default [tcValue];

    property OnDrawBar: TDrawBarEvent read FOnDrawBar write FOnDrawBar;
    property OnMouseEnterBar: TMouseItemEvent read FOnMouseEnterBar write FOnMouseEnterBar;
    property OnMouseLeaveBar: TMouseItemEvent read FOnMouseLeaveBar write FOnMouseLeaveBar;
  end;

  TCWPie = class(TCWGraph)
  private
    FCanDrawText : Boolean;
    FDiscDepth : integer;
    FDoughnutSize: integer;
    FNumPaint: integer;
    FOnDrawPieSlice: TDrawPieSliceEvent;
    FOnMouseEnterPieSlice: TMouseItemEvent;
    FOnMouseLeavePieSlice: TMouseItemEvent;
    FOptions: TPieOptions;
    FPieSize : integer;
    FSeriesTitleFont : TFont;
    FSliceSpacing: single;
    FSlope : integer;
    FStartAngle : integer;
    FStyle : TPieStyle;
    FTitleSpace: integer;
    FValuePrecision : integer;
    function GetActualSize: integer;
    function GetHeight : integer;
    function GetInternalActiveColor(SeriesIndex, ItemIndex : integer): TColor; override;
    function GetTitleSpace : integer;
    function GetWidth : integer;
    procedure SetDiscDepth(const Value: integer);
    procedure SetDoughnutSize(const Value: integer);
    procedure SetInternalActiveColor(SeriesIndex, ItemIndex : integer; Value : TColor); override;
    procedure SetOptions(const Value: TPieOptions);
    procedure SetPieSize(const Value : integer);
    procedure SetSliceSpacing(const Value: single);
    procedure SetSlope(const Value : integer);
    procedure SetStartAngle(const Value : integer);
    procedure SetStyle(const Value : TPieStyle);
    procedure SetValuePrecision(const Value : integer);
    procedure TitleFontChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw; override;
    property ActualSize: integer read GetActualSize;
    property Height : integer read GetHeight;
    property Width : integer read GetWidth;
  published
    property Animation;
    property DiscDepth: integer read FDiscDepth write SetDiscDepth default -5;
    property DoughnutSize: integer read FDoughnutSize write SetDoughnutSize default 0;
    property OnDrawPieSlice: TDrawPieSliceEvent read FOnDrawPieSlice  write FOnDrawPieSlice;
    property OnMouseEnterPieSlice: TMouseItemEvent read FOnMouseEnterPieSlice write FOnMouseEnterPieSlice;
    property OnMouseLeavePieSlice: TMouseItemEvent read FOnMouseLeavePieSlice  write FOnMouseLeavePieSlice;
    property Options: TPieOptions read FOptions write SetOptions default [poAllowImages];
    property PieSize : integer read FPieSize write SetPieSize default 500;
    property SeriesTitleFont : TFont read FSeriesTitleFont write FSeriesTitleFont;
    property SliceSpacing: single read FSliceSpacing write SetSliceSpacing;
    property Slope : integer read FSlope write SetSlope default 0;
    property StartAngle : integer read FStartAngle write SetStartAngle default 0;
    property Style : TPieStyle read FStyle write SetStyle default psflat;
    property ValuePrecision : integer read FValuePrecision write SetValuePrecision default 0;
  end;

  { TCWSectionItem ------------------------------------------------------------ }

  TCWSectionDefs = class;

  TCWSectionItem = class(TCollectionItem)
  private
    FEndValue: string;
    FLongCaption: string;
    FShortCaption: string;
    FStartValue: string;
    function GetSection: TCWSectionDefs;
    function GetWriter: TChartWriter;
    procedure CheckDataType(const Value: string);
    procedure SetEndValue(const Value: string);
    procedure SetLongCaption(const Value : string);
    procedure SetShortCaption(const Value : string);
    procedure SetStartValue(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    property Writer: TChartWriter read GetWriter;
    property Section: TCWSectionDefs read GetSection;
  published
    property EndValue: string read FEndValue write SetEndValue;
    property LongCaption: string read FLongCaption write SetLongCaption;
    property ShortCaption: string read FShortCaption write SetShortCaption;
    property StartValue: string read FStartValue write SetStartValue;
  end;

  { TCWSectionItems ----------------------------------------------------------- }

  TCWSectionItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: integer): TCWSectionItem;
    procedure CheckEmpties;
    procedure SetItem(AIndex: integer; const Value: TCWSectionItem);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item : TCollectionItem); override;
  public
    function Add: TCWSectionItem;
    procedure Assign(Source: TPersistent); override;
    property Items[AIndex: integer]: TCWSectionItem read GetItem write SetItem;
  end;

  { TCWSectionDefs -------------------------------------------------------------- }

  TCWSectionDefs = class(TComponent)
  private
    FCaptionHorizMargin: integer;
    FCaptionLayout: TCaptionLayout;
    FCaptionVertMargin: integer;
    FFont: TFont;
    FPen: TPen;
    FProlongedLines: Boolean;
    FSections: TCWSectionItems;
    FShowLines: Boolean;
    FVisible: Boolean;
    FWID : TWriterListElement;
    FWriter : TChartWriter;
    function GetOwnerChart : TCWChart;
    function GetSectionType: TNameSectionType;
    function GetWriter : TChartWriter;
    procedure DoCheck;
    procedure GraphChanged(Sender : TObject);
    procedure SetCaptionHorzMargin(const Value: integer);
    procedure SetCaptionLayout(const Value : TCaptionLayout);
    procedure SetCaptionVertMargin(const Value: integer);
    procedure SetProlongedLines(const Value : Boolean);
    procedure SetShowLines(const Value : Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property SectionType: TNameSectionType read GetSectionType;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddSection(const StartValue, EndValue, LongCaption, ShortCaption : string);
    property Chart : TCWChart read GetOwnerChart;
    property Writer: TChartWriter read GetWriter;
  published
    property CaptionHorizMargin: integer read FCaptionHorizMargin
      write SetCaptionHorzMargin default 0;
    property CaptionLayout: TCaptionLayout read FCaptionLayout
      write SetCaptionLayout default clOppositeSideOfLabels;
    property CaptionVertMargin: integer read FCaptionVertMargin
       write SetCaptionVertMargin default 0;
    property Font: TFont read FFont write FFont;
    property Pen: TPen read FPen write FPen;
    property ProlongedLines: Boolean read FProlongedLines
       write SetProlongedLines default False;
    property Sections: TCWSectionItems read FSections write FSections;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TCWNameSectionDefs = class(TCWSectionDefs)
  private
    FAutoSections: TAutoSections;
    FDateTimeTemplate: TDateTimeTemplate;
    procedure CheckConfig;
    procedure SetAutoSections(const Value: TAutoSections);
    procedure SetDateTimeTemplate(const Value: TDateTimeTemplate);
  public
    property SectionType;
  published
    property AutoSections: TAutoSections read FAutoSections
      write SetAutoSections default autNotUsed;
    property DateTimeTemplate: TDateTimeTemplate read FDateTimeTemplate
      write SetDateTimeTemplate default ttNotUsed;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCWValueSectionDefs = class(TCWSectionDefs)
  published
  end;

  { TCWSeriesDefs------------------------------------------------------------- }

  TCWSeriesDef = class;

  TCWSeriesDefs = class(TOwnedCollection)
  private
    function GetChart : TCWChart;
    function GetItem(AIndex: integer): TCWSeriesDef;
    procedure SetItem(AIndex: integer; const Value: TCWSeriesDef);
  protected
      procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
      procedure Update(Item : TCollectionItem); override;
  public
    function Add: TCWSeriesDef;
    procedure Assign(Source: TPersistent); override;
    function IndexOf(const AGraph: TCWGraph): integer;
    function IndexOfGraphType(AGraphType : TClass) : integer;
    function IndexOfTitle(ATitle : string) : integer;
    function Insert(Index: integer): TCWSeriesDef;
    property Chart : TCWChart read GetChart;
    property Items[AIndex: integer]: TCWSeriesDef read GetItem write SetItem; default;
  end;

  TValueAxis = class;

  TCWSeriesDef = class(TCollectionItem)
  private
    FColor : TColor;
    FDataCache : TStringList;
    FGraph: TCWGraph;
    FTitle : string;
    FValueScale: TValueScaleNumber;
    FVisible : Boolean;
    function ActualAxis : TValueAxis; {References the TValueAxis object}
    function GetChart: TCWChart;
    function GetData : TstringList;
    function GetTitle: string;
    function GetWriter: TChartWriter;
    procedure SetColor(const Value : TColor);
    procedure SetGraph(const Value: TCWGraph);
    procedure SetTitle(const Value : string);
    procedure SetValueScale(const Value: TValueScaleNumber);
    procedure SetVisible(const Value : Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteCache;
    property Chart : TCWChart read GetChart;
    property DataCache : TStringList read GetData;
    property Writer: TChartWriter read GetWriter;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Graph: TCWGraph read FGraph write SetGraph;
    property Title: string read GetTitle write SetTitle;
    property ValueScale : TValueScaleNumber read FValueScale write SetValueScale default vsValueScale1;
    property Visible : Boolean read FVisible write SetVisible default true;
  end;

  TCWLegendItem = class(TCollectionItem)
  private
    FLegend: TCWLegend;
    procedure SetLegend(const Value: TCWLegend);
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
    function WidestLegend(APosition: TAxisPosition): integer;
    procedure SetItem(AIndex: integer; const Value: TCWLegendItem);
  protected
    procedure Update(Item : TCollectionItem); override;
  public
    function Add: TCWLegendItem;
    function IndexOf(ALegend: TCWLegend): integer;
    function Insert(Index: integer): TCWLegendItem;
    // procedure Assign(Source: TPersistent); override;
    property Items[AIndex: integer]: TCWLegendItem read GetItem write SetItem;
  end;

  { TCWLegend ------------------------------------------------------------------ }

  TCWLegend = class(TComponent)
  private
    FAlignment: TLegendAlignment;
    FAnchoring: TLegendAnchoring;
    FBorder: Boolean;
    FBrush: TBrush;
    FBullets: TLegendBullets;
    FColoredText: Boolean;
    FContentFlow: TContentFlow;
    FContentList: TLegContentList;
    FContents: TLegendContents;
    FFont: TFont;
    FHeight: integer;
    FHorizMargins: integer;
    FLineHeight: integer;
    FOnContent : TLegendContentEvent;
    FOnLocatePoint: TLocatePointEvent;
    FOnOverflow : TNotifyEvent;
    FPointItemIndex: integer;
    FPointLocating: TPointLocating;
    FPointName: string;
    FPointOptions: TPointOptions;
    FPointSeriesIndex: integer;
    FPointValue: string;
    FText: TStrings;
    FTextHeight: integer;
    FTextWidth: integer;
    FTransparency: integer;
    FVertMargins: integer;
    FVisible: Boolean;
    FWID : TWriterListElement;
    FWriter : TChartWriter;
    FWidth: integer;
    function AxisPosition: TAxisPosition;
    function GetCanvas: TCanvas;
    function GetHeight: integer;
    function GetInsideGraph: Boolean;
    function GetLeft: integer;
    function GetOwnerChart : TCWChart;
    function GetTop: integer;
    function GetWidth: integer;
    function GetWriter : TChartWriter;
    function Summary: Boolean;
    procedure CreateContent;
    procedure SetAlignment(const Value : TLegendAlignment);
    procedure SetAnchoring(const Value : TLegendAnchoring);
    procedure SetBorder(const Value : Boolean);
    procedure SetBullets(const Value : TLegendBullets);
    procedure SetContentFlow(const Value : TContentFlow);
    procedure SetContents(const Value : TLegendContents);
    procedure SetHorizMargins(const Value : integer);
    procedure SetPointItemIndex(const Value: integer);
    procedure SetPointName(const Value: string);
    procedure SetPointSeriesIndex(const Value: integer);
    procedure SetPointValue(const Value: string);
    procedure SetText(const Value: TStrings);
    procedure SetTransparency(const Value: integer);
    procedure SetVertMargins(const Value : integer);
    procedure SetVisible(const Value: Boolean);
    property Canvas: TCanvas read GetCanvas;
    property InsideGraph: Boolean read GetInsideGraph;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw;
    property Chart: TCWChart read GetOwnerChart;
    property Height: integer read GetHeight;
    property Left: integer read GetLeft;
    property Top: integer read GetTop;
    property Width: integer read GetWidth;
    property Writer : TChartWriter read GetWriter;
  published
    property Alignment: TLegendAlignment read FAlignment write SetAlignment
      default laLeftOrTop;
    property Anchoring: TLegendAnchoring read FAnchoring write SetAnchoring
      default anRightOutside;
    property Border: Boolean read FBorder write SetBorder default False;
    property Brush: TBrush read FBrush write FBrush;
    property Bullets: TLegendBullets read FBullets write SetBullets  default lbSquare;
    property ColoredText: Boolean read FColoredText write FColoredText  default True;
    property ContentFlow: TContentFlow read FContentFlow write SetContentFlow
      default cfTopBottom;
    property Contents: TLegendContents read FContents write SetContents
      default [coSeriesTitle];
    property Font: TFont read FFont write FFont;
    property HorizMargins: integer read FHorizMargins write SetHorizMargins default 5;
    property OnContent : TLegendContentEvent read FOnContent write FOnContent;
    property OnLocatePoint: TLocatePointEvent read FOnLocatePoint
      write FOnLocatePoint;
    property OnOverflow : TNotifyEvent read FOnOverflow  write FOnOverflow;
    property PointItemIndex: integer read FPointItemIndex write SetPointItemIndex
      default 0;
    property PointLocating: TPointLocating read FPointLocating write FPointLocating
      default plNameValue;
    property PointName: string read FPointName write SetPointName;
    property PointOptions: TPointOptions read FPointOptions write FPointOptions
      default [poShowConnectionLine, poEnlargePoint];
    property PointSeriesIndex: integer read FPointSeriesIndex
      write SetPointSeriesIndex default 0;
    property PointValue: string read FPointValue write SetPointValue;
    property Text: TStrings read FText write SetText;
    property Transparency: integer read FTransparency write SetTransparency default 255;
    property VertMargins: integer read FVertMargins write SetVertMargins  default 5;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TSeries = class(TGraphObject)
  private
    FBarPoints: TPointArray;
    FDoughnutRect: TRect;
    FIndent, FExdent: integer;
    FLeapdateCount: integer;
    FLinkTitle : string;
    FMin, FMax, FSum : single;
    FNegVals : Boolean;
    FPieRect: TRect;
    FPieState: integer;
    FPoints: TPointArray;
    FSeriesItems: TSeriesItems;
    FStartDate, FEndDate: TDateTime;
    FTrackPies: TTrackPies; { Logs the coords of the pislices }
    function GetCount: integer;
    function GetEndDate: TDateTime;
    function GetFirstItem: integer;
    function GetGraph: TCWGraph;
    function GetIndex: integer;
    function GetItemCount: integer;
    function GetItemPoints: TPointArray;
    function GetItems(Index: integer): TSeriesItem;
    function GetLastItem: integer;
    function GetPieTitleRect: TRect;
    function GetStartDate : TDatetime;
    function GetTitle : string;
    function GetUseCategoryColors: Boolean;
    function GetVisible: Boolean;
    function PosFromNamVal(AName: string; AVal: single): TPoint;
    function ToDate(Index: integer): TDateTime; {In time spans, convert Name to date/time}
    property Count: integer read GetCount; { Full span }
    property FirstItem: integer read GetFirstItem; {First item index in span}
    property LastItem: integer read GetLastItem; {Last item index in span}
    property Points: TPointArray read FPoints write FPoints;
    property SeriesItems: TSeriesItems read FSeriesItems; { Full span }
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem: TSeriesItem;
    function AsStrings(TimeType : TSaveTimeType; IncludeItemProps: Boolean = False): TStringList;
    function Avg: single; {Mean}
    function IndexOfDate(Y, M, D: word): integer; { 0-based }
    function IndexOfName(AName: string): integer; { 0-based }
    function IndexOfTime(H, M, S: integer): integer; { 0-based }
    function IndexOfValue(AValue: single): integer; { 0-based }
    function MaxVal: single;
    function Median: single;
    function MinVal: single;
    function Mode: TModeNumbers;
    function Sum: single;
    procedure Assign(ASource: TSeries; FromIndex, ToIndex: integer);
    procedure ToPercentages;
    procedure ToRealValues;
    property DoughnutRect: TRect read FDoughnutRect;
    property EndDate: TDateTime read GetEndDate write FEndDate;
    { Used with name types that is not time lines,
    but when data is extracted from a date span. Can be used to create a legend. }
    property Graph: TCWGraph read GetGraph;
    property Index: integer read GetIndex; { In the Data list }
    property ItemCount: integer read GetItemCount; { 0 based }
    property ItemPoints: TPointArray read GetItemPoints;
    property Items[Index: integer]: TSeriesItem read GetItems; { 0 based }
    property PieRect: TRect read FPieRect;
    property PieTitleRect: TRect read GetPieTitleRect;
    property StartDate: TDateTime read GetStartDate write FStartDate;
    {See EndDate}
    property Title: string read GetTitle;
    property UseCategoryColors: Boolean read GetUseCategoryColors;
    property Visible: Boolean read GetVisible;
  end;

  TRulerHintInfo = record
  {Stores info used to display ruler hints. See PointsFromRuler method}
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
  {Stores actual years (opposed to "logical" years) in time spanned series}
    RealYear, LogYear: word;
  end;

  TYears = TList<TYearInfo>;
  TFiles = TObjectList<TStringList>;

  {Reads in CWR-files}
  TCWFileReader = class(TGraphObject)
  private
    FDataContent: TFiles;
    FGraphs: TFiles;
    FProps: TStringList;
    FTimeType : TSaveTimeType;
    function GetID(SeriesIndex: integer): string;
    function GetItemColor(SeriesIndex, ItemIndex: integer): TColor;
    function GetItemCount(SeriesIndex: integer): integer;
    function GetItemName(SeriesIndex, ItemIndex: integer): string;
    function GetItemValue(SeriesIndex, ItemIndex: integer): single;
    function GetPropValue(APropName: string): string;
    function GetRealDate(SeriesIndex, ItemIndex: integer): TDateTime;
    function GetRealValue(SeriesIndex, ItemIndex: integer): single;
    function GetSeriesCount: integer;
    function GetSeriesTitle(SeriesIndex: integer): string;
  public
    constructor Create(AFileName: TFileName; AChartWriter: TChartWriter);
    destructor Destroy; override;
    function GetSeriesStrings(SeriesIndex: integer): TStringList;
    property ID[SeriesIndex: integer]: string read GetID;
    property ItemColor[SeriesIndex, ItemIndex: integer]: TColor read GetItemColor;
    property ItemCount[SeriesIndex: integer]: integer read GetItemCount;
    property ItemName[SeriesIndex, ItemIndex: integer]: string read GetItemName;
    property ItemRealDate[SeriesIndex, ItemIndex: integer]: TDateTime read GetRealDate;
    property ItemRealValue[SeriesIndex, ItemIndex: integer]: single read GetRealValue;
    property ItemValue[SeriesIndex, ItemIndex: integer]: single read GetItemValue;
    property PropValue[PropName: string]: string read GetPropValue;
    property SeriesCount: integer read GetSeriesCount;
    property SeriesTitle[SeriesIndex: integer]: string read GetSeriesTitle;
  end;

  TAxisObject = class(TGraphObject)
  {Base class for objects that draw sections and labels. Their definition
  counterparts are TCWChart.NameScale, TCWChart.ValueScale1 and TCWChart.ValueScale2

  Instead of TNameScale, TValueScale, etc, the term Axis is chosen, to avoid
  confusions

  The axis objects should be revised. There are some overlapping with the
  the chartwriter class.}
  private
    FGDIP: TGPGraphics;
    FLabelHookRect: TRect;
    FLabelTextRect: TRect;
    FPosition: TAxisPosition;
    function Get3DPoly: TIntPointArray; {Wall}
    function GetCanvas: TCanvas;
    function GetCount: integer; virtual; abstract;
    function GetFloorPoly: TIntPointArray; {Wall}
    function GetFloorRect: TRect; {Wall}
    function GetGrRect: TRect;
    function GetIsXAxis: Boolean;
    function GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint; virtual;
    function GetLabelFreq: integer; virtual;
    function GetLabelRect: TRect;
    function GetLabelSpace: integer; virtual;
    function GetOpposedSections: Boolean;
    function GetQualifier : string; virtual;
    function GetQualifierRect : TRect;
    function GetQualifierSpace : integer; virtual; abstract;
    function GetSectionCount: integer; virtual;
    function GetSectionRect(Index: integer): TRect; virtual; { Label text rect }
    function GetSections: TSections; virtual;
    function GetSectionSpace: integer; virtual;
    function GetSectionSpaceRect: TRect; { Rect of all section labels }
    function GetUnitCount: integer; virtual;
    function GetUnitSize: integer; virtual;
    function GetWallOffset: integer; {Wall}
    function GetWallPoly: TIntPointArray; {Wall}
    function GetWallRect: TRect; {Wall}
    function GetWallWidth: integer; {Wall}
    procedure Draw;
    procedure Draw3DAxis; {Wall}
    procedure DrawAxis;
    procedure DrawLabels; virtual; abstract;
    procedure DrawSections;
    procedure GetAxisHookPos(X, Y: integer; var StartPt, EndPt: TPoint);
    procedure GetGDI;
    property Canvas: TCanvas read GetCanvas;
    property GrRect: TRect read GetGrRect;
    property LabelFreq: integer read GetLabelFreq;
    property LabelRect: TRect read GetLabelRect;
    property LabelSpace: integer read GetLabelSpace;
    property OpposedSections: Boolean read GetOpposedSections;
    property SectionCount: integer read GetSectionCount;
    property Sections: TSections read GetSections;
    property SectionSpace: integer read GetSectionSpace;
    property SectionSpaceRect: TRect read GetSectionSpaceRect;
    property UnitCount: integer read GetUnitCount;
    property UnitSize: integer read GetUnitSize;
  public
    constructor Create;
    destructor Destroy; override;
    property IsXAxis: Boolean read GetIsXAxis;
    property Position: TAxisPosition read FPosition write FPosition;
  end;

  TValueAxis = class(TAxisObject)
  {Handles labels of ValueScale1}
  private
    function GetCount: integer; override;
    function GetExactValuePos( AValue: single) : integer;
    function GetHigh: single; virtual;
    function GetInterval: single;
    function GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint; override;
    function GetLabelFreq: integer; override;
    function GetLabelSpace: integer; override;
    function GetLow: single; virtual;
    function GetQualifier: string; override;
    function GetQualifierSpace : integer; override;
    function GetSectionCount: integer; override;
    function GetSectionRect(Index: integer): TRect; override;
    function GetSections: TSections; override;
    function GetSectionSpace: integer; override;
    function GetUnitCount: integer; override;
    function GetUnitSize: integer; override;
    procedure DrawLabels; override;
    property Count: integer read GetCount;
    property HighVal: single read GetHigh;
    property Interval: single read GetInterval;
    property LowVal: single read GetLow;
  end;

  TValueAxis2 = class(TValueAxis)
  {Handles labels of ValueScale2}
  private
    function GetCount: integer; override;
    function GetHigh: single; override;
    function GetLabelFreq: integer; override;
    function GetLow: single; override;
  end;

  TNameAxis = class(TAxisObject)
  {Handles labels of name scale}
  private
    function GetCount: integer; override;
    function GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint; override;
    function GetLabelFreq: integer; override;
    function GetLabelSpace: integer; override;
    function GetQualifier : string; override;
    function GetQualifierSpace : integer; override;
    function GetSectionCount: integer; override;
    function GetSectionRect(Index: integer): TRect; override;
    function GetSections: TSections; override;
    function GetSectionSpace: integer; override;
    function GetUnitCount: integer; override;
    function GetUnitSize: integer; override;
    function PosFromDate(ADate: TDateTime): integer;
    function PosFromHour(ADate: TDateTime): integer;
    function PosFromMinute(ADate: TDateTime): integer;
    function PosFromNumber(ANumber: single): integer;
    function PosFromSecond(ADate: TDateTime): integer;
    procedure DrawLabels; override;
  end;

  TCWMargins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: integer;
    FWriter: TChartWriter;
    function CheckMargin(const OldValue, NewValue: integer): Boolean;
    procedure SetBottom(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure UpdateValue;
  public
    property Writer: TChartWriter read FWriter;
  published
    property Bottom: integer read FBottom write SetBottom default 0;
    property Left: integer read FLeft write SetLeft default 0;
    property Right: integer read FRight write SetRight default 0;
    property Top: integer read FTop write SetTop default 0;
  end;

  TCWScale = class(TPersistent)
  {Base class for TCWNameScale, TCWValueScale1 and 2}
  private
    FFont : TFont;
    FOwner : TCWChart;
    FPen : TPen;
    FQualifier : string;
    FShowDividerLines : Boolean;
    FShowLabels : Boolean;
    FMinLabelSpacing : integer;
    function GetLabelRect : TRect; virtual; abstract;
    function GetWriter : TChartWriter;
    procedure FontChange(Sender : TObject);
    procedure SetMinLabelSpacing(const Value : integer);
    procedure SetQualifier(const Value : string);
    procedure SetShowDividerLines(const Value : Boolean);
    procedure SetShowLabels(const Value : Boolean);
  public
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  public
    constructor Create;
    property LabelRect : TRect read GetLabelRect;
    property Writer : TChartWriter read GetWriter;
  published
    property Font : TFont read FFont write FFont;
    property MinLabelSpacing : integer read FMinLabelSpacing write SetMinLabelSpacing default 5;
    property Pen : TPen read FPen write FPen;
    property Qualifier : string read FQualifier write SetQualifier;
    property ShowDividerLines : Boolean read FShowDividerLines write SetShowDividerLines default false;
    property ShowLabels : Boolean read FShowLabels write SetShowLabels default true;
  end;


  TCWNameScale = class(TCWScale)
  private
    FAllowImages : Boolean;
    FClipLabels : Boolean;
    FNumSpanPrecision: integer;
    FOverflowAction : TOverFlowAction;
    function GetLabelRect : TRect; override;
    procedure SetAllowImages(const Value : Boolean);
    procedure SetNumSpanPrecision(const Value : integer);
    procedure SetOverflowAction(const Value : TOverflowAction);
  public
    constructor Create;
    procedure Assign(Source : TPersistent); override;
  published
    property AllowImages : Boolean read FAllowImages write SetAllowImages default True;
    property ClipLabels : Boolean read FClipLabels write FClipLabels default false  ;
    property NumSpanPrecision : integer read FNumSpanPrecision write SetNumSpanPrecision default 0;
    property OverflowAction : TOverflowAction read FOverFlowAction write SetOverFlowAction default ovNone;
  end;

  TCWValueScale = class(TCWScale)
  private
    FScaleRounding : Boolean;
    FUserIntervals : single;
    FValueFormat : TValueFormat;
    FValueHigh: single;
    FValueIntervals: single;
    FValueLow: single;
    FValuePrecision: integer;
    FValueSpanFromData: Boolean;
    FValueUnit : integer;
  private
    function GetCorrelationSeries : TSeries;
    function GetLabelRect : TRect; override;
    function GetValueCount: integer;
    function GetValueFloatUnit: single;
    procedure SetHighLow;
    procedure SetScaleRounding(const Value : Boolean);
    procedure SetValueHigh(const Value: single);
    procedure SetValueIntervals(const Value: single);
    procedure SetValueLow(const Value: single);
    procedure SetValuePrecision(const Value: integer);
    procedure SetValueSpanFromData(const Value: Boolean);
  protected
    property ValueFormat : TValueFormat read FValueFormat write FValueFormat default vtNone;
    { Not implemented so far. Thousand separators are standard}
  public
    procedure Assign(Source : TPersistent); override;
    procedure SetValueSpan(LowValue, HighValue: single);
    property ValueCount: integer read GetValueCount;
    property ValueFloatUnit: single read GetValueFloatUnit;
  published
    property ScaleRounding : Boolean read FScaleRounding write SetScaleRounding;
    property ValueHigh: single read FValueHigh write SetValueHigh;
    property ValueIntervals: single read FValueIntervals write SetValueIntervals;
    property ValueLow: single read FValueLow write SetValueLow;
    property ValuePrecision: integer read FValuePrecision
      write SetValuePrecision default 0;
    property ValueSpanFromData: Boolean read FValueSpanFromData
      write SetValueSpanFromData default False;
  end;

  TCWPieChart = class;
  TCWCategoryBarChart = class;

  TZoomLogItem = record
  {Zoom history}
    FStart, FEnd : integer;
  end;

  TZoomLog = TList<TZoomLogItem>;

  TCWChart = class(TComponent)
  {Chart base class.
  Contains the properties for all the chart types. The protected properties
  are published for the types that actually needs them.
  At run time protected properties are all available as read write through the
  central chartwriter component. The global variable StrictProtection ensures
  that reading/writing properties not relevant for the type will cause an exception.
  THis behaviou can be changed by setting StrictProtection to false.}
  private
    FAlternativeGraph : TCWGraph;
    FAlternativeOrient : TAxisOrientation;
    FAnimationEnabled : Boolean;
    FAxisOrientation : TAxisOrientation;
    FCategories: TCWCategories;
    FCWRFileName : string;
    FDataset : TDataset;
    FFileName : TFileName;
    FGradientWall: Boolean;
    FGraphBGColor : TColor;
    FGraphBorders : TGraphBorders;
    FGraphMargins: TCWMargins;
    FHasAnimated : Boolean;
    FInnerMargins: TCWMargins;
    FLegends : TCWLegends;
    FMouseTimeFormat : string;
    FNameDBField, FValueDBFields : string;
    FNameScale : TCWNameScale;
    FNameSectionDefs : TCWNameSectionDefs;
    FOnGetData : TNotifyEvent;
    FOnTitle : TTitleEvent;
    FOrigData : TData;
    FPercentages : Boolean;
    FPointRects: TLegendRects;
    FSeriesDefs : TCWSeriesDefs;
    FSeriesRects: TLegendRects;
    FSpanType: TSpanType;
    FSummaryRects: TLegendRects;
    FTextTilting : TTextOrientations;
    FTextTiltThreshold : integer;
    FTimeFormat : string;
    FTitle : string;
    FTitleAlignment : Talignment;
    FTitleFont : TFont;
    FValueFormat : TValueFormat;
    FValueScale1 : TCWValueScale;
    FValueScale2 : TCWValueScale;
    FValueSectionDefs : TCWValueSectionDefs;
    FWallBordercolor: TColor;
    FWallColor: TColor;
    FWallWidth : integer;
    FWID : TWriterListElement;
    FWriter : TChartWriter;
    FZoomLog : TZoomLog;
    FZoomStart, FZoomEnd: integer;
    function ColorUsage : TColorUsage;
    function GetAxisCount : integer;
    function GetDataset : TDataset;
    function GetFileName : TFileName;
    function GetMouseTimeFormat : string;
    function GetNameDBField : string;
    function GetNameType : TNameType;
    function GetPercentages : Boolean;
    function GetTitle : string;
    function GetValueDBFields : String;
    function GetVisibleCount : integer;
    function GetWallWidth : integer;
    function GetWriter : TChartWriter;
    function GetZoomed : Boolean;
    function ValAx2Graph : TCWGraph; {Retuns the graph connected to ValueAxis2}
    function VisibleGraphCount(GraphType : TClass) : integer;
    procedure CreateLegendContent;
    procedure DrawLegends;
    procedure DrawQualifiers;
    procedure DrawTitle;
    procedure FontChange(Sender : TObject);
    procedure SetAlternativeGraph(const Value : TCWGraph);
    procedure SetAxisOrientation(const Value: TAxisOrientation);
    procedure SetDataset(const Value : TDataset);
    procedure SetFileName(const Value : TFileName);
    procedure SetGradientWall(const Value: Boolean);
    procedure SetGraphBGColor(const Value : TColor);
    procedure SetGraphBorders(const Value : TGraphBorders);
    procedure SetNameDBField(const Value : string);
    procedure SetNameSectionDefs(const Value : TCWNameSectionDefs);
    procedure SetSpanType(const Value: TSpanType);
    procedure SetTextTilting(const Value : TTextOrientations);
    procedure SetTextTiltThreshold(const Value : integer);
    procedure SetTimeFormat(const Value : string);
    procedure SetTitle(const Value : string);
    procedure SetTitleAlignment(const Value : TAlignment);
    procedure SetValueDBFields(const Value : string);
    procedure SetValueSectionDefs(const Value : TCWValueSectionDefs);
    procedure SetWallBorderColor(const Value: TColor);
    procedure SetWallColor(const Value: TColor);
    procedure SetWallWidth(const Value : integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property AlternativeGraph : TCWGraph read FAlternativeGraph write SetAlternativeGraph;
    property AxisOrientation : TAxisOrientation read FAxisOrienTation write SetAxisOrientation default alBottomLeft;
    property Categories: TCWCategories read FCategories write FCategories;
    property GradientWall: Boolean read FGradientWall write SetGradientWall default False;
    property GraphBorders : TGraphBorders read FGraphBorders write SetGraphBorders;
    property MouseTimeFormat : string read GetMouseTimeFormat write FMouseTimeFormat;
    property NameScale : TCWNameScale read FNameScale write FNameScale;
    property NameSectionDefs : TCWNameSectionDefs read FNameSectionDefs write SetNameSectionDefs;
    property Percentages : Boolean read GetPercentages write FPercentages default false;
    property SpanType: TSpanType read FSpanType write SetSpanType default ntDateSpan;
    property TextTilting : TTextOrientations read FTextTilting write SetTextTilting default [];
    property TextTiltThreshold : integer read FTextTiltThreshold write SetTextTiltThreshold default 1;
    property TimeFormat : string read FTimeFormat write SetTimeFormat;
    property ValueFormat : TValueFormat read FValueFormat write FValueFormat default vtNone;
    { Not implemented so far. Thousand separators are standard}
    property ValueScale1 : TCWValueScale read FValueScale1 write FValueScale1;
    property ValueScale2 : TCWValueScale read FValueScale2 write FValueScale1;
    property ValueSectionDefs : TCWValueSectionDefs read FValueSectionDefs write SetValueSectionDefs;
    property WallBorderColor: TColor read FWallBorderColor write SetWallBorderColor default clBlack;
    property WallColor: TColor read FWallColor write SetWallColor default clWindow;
    property WallWidth : integer read GetWallWidth write SetWallWidth default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddLegend : TCWLegend;
    function AllEqual : TCWGraph;
    function CanAnimate : integer;
    function GraphCount(AGraph : TCWGraph) : integer;
    function IsActive : Boolean;
    function IsCached : Boolean;
    function PerformAnimation : integer;
    procedure AddSeriesDef(AGraph : TCWGraph);overload;
    procedure AddSeriesDef(ATitle : string; AGraph : TCWGraph; AColor : TColor;
      AValueAxis : TValueScaleNumber);overload;
    procedure ClearCache;
    procedure ClearSeriesDefs;
    procedure DeleteLegend(Index : integer);
    procedure GetUniqueGraphs(var Graph1, Graph2 : TCWGraph);
    procedure Reload;
    procedure ReplaceGraphs(NewGraph : TCWGraph);
    procedure SaveCache;
    procedure SetSeriesDef(Index : integer; ATitle : string; AGraph : TCWGraph;
      AColor : TColor; AValueAxis : TValueScaleNumber);
    property AnimationEnabled : Boolean read FAnimationEnabled write FAnimationEnabled;
    property AxisCount : integer read GetAxisCount;
    property HasAnimated : Boolean read FHasAnimated write FHasAnimated;
    property VisibleCount : integer read GetVisibleCount;
    property Writer: TChartWriter read GetWriter;
    property Zoomed : Boolean read GetZoomed;
    property ZoomEnd : integer read FZoomEnd;
    property ZoomStart : integer read FZoomStart;
  published
    property Dataset : TDataset read GetDataset write SetDataset;
    property FileName : TFileName read GetFileName write SetFileName;
    property GraphBGColor : TColor read FGraphBGColor write SetGraphBGColor default clWindow;
    property GraphMargins: TCWMargins read FGraphMargins write FGraphMargins;
    property InnerMargins: TCWMargins read FInnerMargins write FInnerMargins;
    property Legends : TCWLegends read FLegends write FLegends;
    property NameDBField : string read GetNameDBField write SetNameDBField;
    property OnGetData : TNotifyEvent read FOnGetData write FOnGetData;
    property OnTitle : TTitleEvent read FOnTitle write FOnTitle;
    property SeriesDefs : TCWSeriesDefs read FSeriesDefs write FSeriesDefs;
    property Title : string read GetTitle write SetTitle;
    property TitleAlignment : TAlignment read FTitleAlignment write SetTitleAlignment default taCenter;
    property TitleFont : TFont read FTitleFont write FTitleFont;
    property ValueDBFields : string read GetValueDBFields write SetValueDBFields;
  end;

  TCWSpanChart = class(TCWChart)
  published
    property AlternativeGraph;
    property AxisOrientation;
    property GradientWall;
    property GraphBorders;
    property MouseTimeFormat;
    property NameScale;
    property NameSectionDefs;
    property SpanType;
    property TextTilting;
    property TextTiltThreshold;
    property TimeFormat;
    property ValueScale1;
    property ValueScale2;
    property ValueSectionDefs;
    property WallBorderColor;
    property WallColor;
    property WallWidth;
  end;

  TCWGeneralChart = class(TCWChart)
  published
    property AlternativeGraph;
    property AxisOrientation;
    property GradientWall;
    property GraphBorders;
    property NameScale;
    property NameSectionDefs;
    property TextTilting;
    property TextTiltThreshold;
    property ValueScale1;
    property ValueScale2;
    property ValueSectionDefs;
    property WallBorderColor;
    property WallColor;
    property WallWidth;
  end;

  TCWCategoryChart = class(TCWChart)
    published
     property Categories;
     property Percentages;
  end;

  TCWCategoryBarChart = class(TCWCategoryChart)
  published
   property AlternativeGraph;
   property AxisOrientation;
   property GradientWall;
   property GraphBorders;
   property NameScale;
   property NameSectionDefs;
   property TextTilting;
   property TextTiltThreshold;
   property ValueScale1;
   property ValueSectionDefs;
   property WallBorderColor;
   property WallColor;
   property WallWidth;
  end;

  TCWPieChart = class(TCWCategoryChart)
  private
  published
  end;

  TChartWriter = class(TCustomControl)
  private
    { Private declarations }
    {Internal use}
    FActiveValAx : TAxisObject;
    FAlfaBM: Vcl.Graphics.TBitmap; {Transparent layer used with mouse sselection}
    FAnimInfo: TAnimInfo; {Used by the animation engine: DoAnimation}
    FAppEvent: TApplicationEvents;
    FBall: Vcl.Graphics.TBitmap;
    FBallSmall: Vcl.Graphics.TBitmap;
    FBottomSpace : integer; {+ FRightspace, used to center the chart}
    FCallCounter : integer; {SetLabelfreqs}
    FChartList : TChartList;
    FCompressed : Boolean;
    FContractionBase: TData; { The series that are used as a base for computing Contraction }
    FContractionCount: integer; { Debug, picking up endless Contract loop, see RestrictToClient }
    FDsgnData : TObjectList<TStringList>;
    FDynaSectStart, FDynaSectEnd: integer; {Tracking mouse zoom}
    FFormHandle: Hwnd;
    FHintItem: integer; {The item that displays the mouse move hint }
    FHintSeries: integer; {The Series that displays the mouse move hint }
    FHintText: string;
    FHWBM: TBitmap; { Helper to compute text dimensions. See GetTextWidth og GetTextHeight }
    FImageSize : TSize;
    FInItemInd: integer;
    FInSerInd: integer; { <> -1 if mouse is in this point }
    FInternalChartList : TInternalChartList;
    FInternalLeading: integer;
    FLastMousePos: TPoint; { Tracking ruler movements }
    FLastMouseX: integer; {Tracking mouse travel distance horz}
    FLastMouseY: integer; {Tracking mouse travel distance vert}
    FLeapdateCount: integer;
    FLogYearMap: array of TYearMap;
    FMouseDistanceX: integer;
    FMouseDistanceY: integer;
    FNameAxis: TNameAxis;
    FNeedsIds : Boolean;
    FNumberInterval: integer; { Helps to control equality of series }
    FOldActive: TCWGraph;
    FOldWndHandler: Pointer;
    FOrigGraphSize: TSize;
    FPosRect : TRect;  {Static for GetPosrect, used with point computing}
    FRightSpace : integer;
    FRulerPosition: TPoint;
    FRulerX, FRulerY: integer; { Position of ruler }
    FScrollIndex: integer; { Pointer to data start, when scrollable }
    FSelBM: Vcl.Graphics.TBitmap;
    FSelectingRect: TRect;  { The following are used with selecting }
    FSelStartX: integer;
    FSelstartY: integer;
    FSeriesData: TData;
    FStates: TStates;
    FTallestName: string;
    FTallestNameSection: string;
    FTallestNameShortSection: string;
    FTallestValue: string;
    FTallestValue2 : string; { Widest/highest text of the Series }
    FTallestValueSection: string;
    FTallestValueShortSection: string;
    FTextComputed : Boolean; {Flag to prevent ComputeTextExtent to run twice}
    FUpdateKinds: TUpdateKinds; { Used with Begin/EndUpdate. }
    FUseNamShortNames: Boolean;
    FUseValShortNames: Boolean;
    FValueAxis: TValueAxis;
    FValueAxis2: TValueAxis2;
    FViewMode: TViewMode; {Hinting, selecting, etc}
    FYears: TYears; {Keeping the real yeras of a time span}
    FWidestName: String;
    FWidestNameSection: string;
    FWidestNameShortSection: string;
    FWidestValue: string;
    FWidestValue2: string;
    FWidestValueSection: string;
    FWidestValueShortSection: string;
    FWndHandlerPtr: Pointer;


    {Readers / Writers}
    FBezierMargin : integer;
    FBorder: Boolean;
    FCentered : Boolean;
    FChart : TCWChart;
    FContraction: integer;
    FContractionType: TContractionType;
    FFixedGraphHeight : integer;
    FFixedGraphWidth : integer;
    FHintColor: TColor;
    FInfoControl: Boolean;
    FKeepSelection: Boolean;
    FLanguage: String;
    FLiveResize : Boolean;
    FLongestSeries : TSeries;
    FMinContraction : integer; {Minimum contraction allowed. Set by RestrictToClient}
    FMouseInfo: TMouseInfo;
    FMousePrecision: TMousePrecision;
    FMouseSelect: Boolean;
    FMouseSizing : Boolean;
    FNameLabelFreq: integer; { Frequency of name labels }
    FNameLabelSpace: integer;
    { Name label space at x and y, default value 25, used design time when no data
    Actual space is computed by TAxisObject}
    FNameList: TStringList; { Name values }
    FNameSectionFreq: integer;
    FNameSections: TSections;
    FNameUnit: integer;
    FRulerGuideLines: Boolean;
    FRulers: TRulers;
    FRulerVisible: Boolean;
    FScrolling: Boolean;
    FSelectionExec: TSelectionExec; { How to transform a selected area into a dynamic section }
    FTrackBars: TTrackBars; { Logs the bar rectangles to make it easy to track them by the mouse }
    FUserContraction : integer;
    FValueLabelFreq: integer; { Frequency of name value labels, scale1 }
    FValueLabelFreq2: integer; { Frequency of name value labels, scale2 }
    FValueLabelSpace: integer;
    { Value label space at x and y, default value 40, used design time when no data
    Actual space is computed by TAxisObject}
    FValueSectionFreq: integer;
    FValueSections: TSections;
    FWallAngle: single; {Fixed, not published}

    {Events}
    FAfterBuildChart : TNotifyEvent;
    FAfterDrawGraph: TDrawGraphEvent;
    FOnDataChange: TNotifyEvent;
    FOnDrawSection: TDrawSectionEvent;
    FOnDrawLabel: TDrawLabelEvent;
    FOnLoadFile : TLoadFileEvent;
    FOnMeasureLabel: TMeasureLabelEvent;
    FOnMouseInfo: TMouseInfoEvent;
    FOnQuerySpace : TQuerySpaceEvent;
    FOnRuler: TNotifyEvent;
    FOnSelected: TNotifyEvent;
    FOnZoom: TNotifyEvent;

    function AxisOf(Posit: TAxisPosition): TAxisObject;
    function CanScrollTo(AScrollIndex: integer): Boolean;
    function CheckPieValues(Values: TSeriesItems; APie: TCWPie): integer;
    function ComputeTextPos(X, Y: integer; ALabel: string; AnAxis: TAxisObject): TPoint;
    function DetectSpanType(SeriesIndex: integer): TSpanType;
    function DoPosInBar(X, Y: integer;  var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function DoPosInPieSlice(X, Y: integer; var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function DoPosInPoint(X, Y: integer; Precision: TMousePrecision;
     var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function GetAbsoluteItemCount : integer;
    function GetActiveGraph: TCWGraph;
    function GetActiveValAx : TCWValueScale;
    function GetAxisOrientation : TAxisOrientation;
    function GetCategories : TCWCategories;
    function GetContraction: integer;
    function GetCount: integer;
    function GetCurrentPage : integer;
    function GetCWBoundsRect : TRect;
    function GetGradientWall : Boolean;
    function GetGraphBGColor : TColor;
    function GetGraphBorders : TGraphBorders;
    function GetGraphHeight: integer;
    function GetGraphLeft: integer;
    function GetGraphPrintRect: TRect;
    function GetGraphRect: TRect;
    function GetGraphSpaceRect: TRect;
    function GetGraphTop: integer;
    function GetGraphWidth: integer;
    function GetImageFilename(AFilename : string; Categoryindex : integer) : string;
    function GetInfoControl: Boolean;
    function GetIsUpdating: Boolean;
    function GetMousePrecision: TMousePrecision;
    function GetMouseTimeFormat: string;
    function GetNameCount: integer;
    function GetNameFloatUnit: single;
    function GetNames(Index: integer): string;
    function GetNameScale : TCWNameScale;
    function GetNameSectionCount: integer;
    function GetNameSectionDefs: TCWNameSectionDefs;
    function GetNameSections(Index: integer): TSection;
    function GetNumPrecisionString(Num : string; NumSpanPrec : integer): string;
    function GetOverflowError : Boolean;
    function GetPageEnd : integer;
    function GetPageCount : integer;
    function GetPageStart : integer;
    function GetPercentages : Boolean;
    function GetPosRect(SeriesCount: integer = -1): TRect;
    function GetRendered: Boolean;
    function GetScaleExtension : integer;
    function GetScrollPointSpacing: integer;
    function GetSelRect: TRect;
    function GetSeries(Index: integer): TSeries;
    function GetSeriesAttributes(ASeries : TStringList; TimeType : TSaveTimetype;
     var Intervals : integer): TDataAttributes;
    function GetSeriesItems(SeriesIndex, ItemIndex: integer): TSeriesItem;
    function GetSpanType : TSpanType;
    function GetTextHeight(LabelKind: TLabelKind; Angle: integer = 0): integer;
    function GetTextTilting : TTextOrientations;
    function GetTextTiltThreshold : integer;
    function GetTextWidth(LabelKind: TLabelKind; Angle: integer = 0) : integer; overload;
    function GetTextWidth(LabelKind: TLabelKind; AText: string; Angle: integer = 0): integer; overload;
    function GetTimeFormat : string;
    function GetTitleAlignment : TAlignment;
    function GetTitleFont : TFont;
    function GetTitleSpace : integer;
    function GetTrackBar(ASeriesIndex, AItemIndex: integer; var ATrackBar: TTrackBar): Boolean;
    function GetValPrecision: integer;
    function GetValueCount: integer;
    function GetValueFloatUnit: single;
    function GetValuePrecision: integer;
    function GetValueScale1 : TCWValueScale;
    function GetValueScale2 : TCWValueScale;
    function GetValueSectionCount: integer;
    function GetValueSectionDefs: TCWValueSectionDefs;
    function GetValueSections(Index: integer): TSection;
    function GetViewMode: TViewMode;
    function GetVisibleCount: integer;
    function GetWallBorderColor : TColor;
    function GetWallColor : TColor;
    function GetWallWidth: integer;
    function GetWorkRect: TRect;
    function GraphSectionFromPoint(X, Y: integer): integer;
    function GraphTypeInChart(AGraphType : TClass) : Boolean;
    function IndexOfNearestName(AName: string; StartWith: integer = 0): integer;
    function InSpan(ASeries: TSeries; LowVal, HighVal: single;
     var ErrorNumber: single): Boolean; overload;
    function InSpan(LowVal, HighVal: single; var ErrorNumber: single) : Boolean; overload;
    function InState(AState: TState): Boolean;
    function InView(AGraphType: TClass): TCWGraph; overload;
    function InView(AGraphType: TClass; var Index: integer): TCWGraph; overload;
    function IsLeapDate(ASeries: TSeries; ItmIndex: integer): Boolean;
    function IsPointVisible(AScrollIndex: integer): Boolean;
    function LongestSeries: TSeries;
    function PointsFromRuler(Vertical: Boolean; X, Y: integer; var APos: TPoint;
     var SeriesDefs: TSeriesInfoItems): Boolean;
    function RealYearToLogYear(ARealYear: word): word;
    procedure RecomputeHighLowValues;
    function SeriesIndexOfX(APos: TPoint; var SeriesIndex: integer): integer;
    function SeriesIndexOfY(APos: TPoint; var SeriesIndex: integer): integer;
    function SetHintText(X, Y: integer; AGraph: TCWGraph): Boolean; overload;
    function SetHintText(X, Y: integer; SerInd, ItmInd: integer;
     AGraph: TCWGraph): Boolean; overload;
    function SpaceOf(Position: TAxisPosition): integer;
    function XFromName(AValue: string): integer;
    function YFromName(AValue: string): integer;
    procedure AddDsgnSeries(ASpanType : TSpanType; indx : integer);
    procedure AddSeries(ASeries: TSeries; Internal: Boolean = False); overload;
    procedure AppMsg(var Msg: tagMsg; var Handled: Boolean);
    procedure AssignOrigData;
    procedure CancelBeacons;
    procedure CheckImage;
    procedure ClearState(AState: TState);
    procedure ComputePoints(ASeries: TSeries = nil; Recalc: Boolean = False);
    procedure ComputeTextExtent;
    procedure ConcludeSelecting(XPos, YPos: integer);
    procedure CorrectPie;
    procedure CreateAutoSections;
    procedure CreateIDS;
    procedure CreateNameSections;
    procedure CreateValueSections;
    procedure CreateSpan;
    procedure DoAnimation;
    procedure DoContraction(Rate: integer; ContractType: TContractionType);
    procedure DoLoadFiles(AFileName: string; Files: TFiles);
    procedure DoRepaint;
    procedure DoScrollTo(AScrollIndex: integer);
    procedure DoZoom(StartIndex, EndIndex: integer);
    procedure DrawBorders;
    procedure GetProps(PropList: TStringList);
    procedure GoBackError(DoReload : Boolean = false);
    procedure InitAnimation;
    procedure InternalClear;
    procedure KeyD(Key: word; Shift: TShiftState);
    procedure KeyU(Key: word; Shift: TShiftState);
    procedure LoadFromPropFile(AFileName: TFileName);
    procedure MakeDateSpan;
    procedure MakeMonthSpan;
    procedure MakeNumberSpan;
    procedure MakeTimeSpan(ASpanType: TSpanType);
    procedure NormaliseDates(ASeries: TSeries; SerIndx: integer);
    procedure NWndProc(var Messg: TMessage);
    procedure Reload;
    procedure RepaintSelRect;
    procedure ResetCanvas(AGraph : TCWGraph);
    procedure ResetIDS;
    procedure RestrictToClient(Restore: Boolean);
    procedure SaveSelBM;
    procedure SetAxisOrientation(const Value: TAxisOrientation);
    procedure SetBezierMargin(const Value: integer);
    procedure SetBorder(const Value: Boolean);
    procedure SetCentered(const Value: Boolean);
    procedure SetChart(const Value: TCWChart);
    procedure SetContraction(const Value: integer);
    procedure SetFixedGraphHeight(const Value: integer);
    procedure SetFixedGraphWidth(const Value: integer);
    procedure SetGradientWall(const Value: Boolean);
    procedure SetGraphBGColor(const Value: TColor);
    procedure SetGraphBorders(const Value: TGraphBorders);
    procedure SetHighLow;
    procedure SetInfoControl(const Value: Boolean);
    procedure SetLabelFont(const LabelKind: TLabelKind);
    procedure SetLabelFreqs(ComputePts: Boolean = True);
    procedure SetLanguage(Value: string);
    procedure SetMouseTimeFormat(const Value: string);
    procedure SetNameLabelFreq(const Value: integer);
    procedure SetNameSectionDefs(const Value: TCWNameSectionDefs);
    procedure SetNameUnit(const Value: integer);
    procedure SetPercentages(const Value: Boolean);
    procedure SetPosition(const Orientation: TAxisOrientation);
    procedure SetProps(const PropList: TStringList);
    procedure SetRulers(const Value: TRulers);
    procedure SetScrollIndex(const Value: integer);
    procedure SetSpanType(const Value: TSpanType);
    procedure SetState(const AState: TState);
    procedure SetTextTilting(const Value: TTextOrientations);
    procedure SetTextTiltThreshold(const Value: integer);
    procedure SetTimeFormat(const Value: string);
    procedure SetTitleAlignment(const Value: TAlignment);
    procedure SetTitleFont(const Value: TFont);
    procedure SetValueLabelFreq(const Value: integer);
    procedure SetValuePrecision(const Value: integer);
    procedure SetValueSectionDefs(const Value: TCWValueSectionDefs);
    procedure SetWallBorderColor(const Value: TColor);
    procedure SetWallColor(const Value: TColor);
    procedure SetWallWidth(const Value: integer);
    procedure WMAFTERBUILD(var Msg : TMessage); message WM_AFTERBUILD;
    procedure WMENDEXEC(var Msg: Tmessage); message WM_ENDEXEC;
    procedure WMERASEBKGND(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMERROR(var Msg: Tmessage); message WM_ERROR;
    procedure WMLOADFILE(var Msg : TMessage); message WM_LOADFILE;
    procedure WMMouseLeave(var Message: TWMMouse); message WM_MOUSELEAVE;
    procedure WMREFRESHCHART(var Msg : TMessage); message WM_REFRESHCHART;
    procedure WMSAVESELBM(var Msg: Tmessage); message WM_SAVESELBM;
    property ActiveValueScale : TCWValueScale read GetActiveValAx;
    property InternalChartList: TInternalChartList read FInternalChartList;
    property NameFloatUnit: single read GetNameFloatUnit;
    property NameUnit: integer read FNameUnit write SetNameUnit;
    property SelRect: TRect read GetSelRect;
    property ViewMode: TViewMode read GetViewMode;
    property WallAngle: single read FWallAngle; {Fixed 45}
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Resize; override;
    property MouseInfoControl: Boolean read GetInfoControl write SetInfoControl;
    property ValuePrecision: integer read GetValuePrecision write SetValuePrecision;
  public
    { Public declarations }
    AnimationTuner: TAnimationTuner;
    LiveGraphs : Boolean;
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    function AddSection(OnScale: TScaleType; AStart, AEnd, LongCaption,
      ShortCaption: string; SectionType: TSectionType): TSection;
    function AllVisible: Boolean;
    function AsBitmap(GraphElement: integer): TBitmap;
    function BaseNameInterval: integer; { The original intervals between the names in
    spanned series, as set by the user.
    When computing Contraction this value is subtracted from the result. }
    function CanContract : integer;
    function CanRender: integer;
    function CanScroll(ScrollType: TScrollType): Boolean;
    function CanScrollBy(Delta: integer): Boolean;
    function GetAlternativeGraph : TCWGraph;
    function GetDataAttributes(AFileName : string) : TDataAttributes;
    function GetFileType(AFileName : string) : integer; {-1 invalid, 0 rich, 1 data}
    function GetScrollable: Boolean;
    function GetSelection(var StartIndex, EndIndex: integer): Boolean;
    function HasWall: Boolean;
    function IndexOfDateEx(Y, M, D: word): integer;
    function IndexOfName(AName: string): integer; { In the names list }
    function IndexOfNameEx(AName: string): integer; { In the orig list }
    function IndexOfTimeEx(H, M, S: integer): integer;
    function IsAnimating : Boolean;
    function IsAxisChart(AChart : TCWChart = nil) : Boolean;
    function IsCompressed : Boolean;
    function IsContractionIdle : Boolean;
    function IsTimeSpan: Boolean;
    function MouseInBar(var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function MouseInPieSlice(var SeriesIndex, SeriesItemIndex: integer) : Boolean;
    function MouseInPoint(var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function NamValFromPos(const X, Y: integer; var Nam: string; var Val: single): Boolean;
    function PosFromDate(ADate: TDateTime): integer;
    function PosFromHour(ADate: TDateTime): integer;
    function PosFromMinute(ADate: TDateTime): integer;
    function PosFromNumber(ANumber: single): integer;
    function PosFromSecond(ADate: TDateTime): integer;
    function PosFromValue(AValue: single; AAxis: TValueAxis): integer;
    function PosInBar(X, Y: integer; var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function PosInNameSection(X, Y: integer; var SectionIndex: integer) : Boolean;
    function PosInPieSlice(X, Y: integer; var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function PosInPoint(X, Y: integer; var SeriesIndex, SeriesItemIndex: integer): Boolean;
    function PosInValueSection(X, Y: integer; var SectionIndex: integer): Boolean;
    function RulerPoints: TRulerPoints;
    function SeriesFromMouse(var ItemIndex: integer): TSeries;
    function SeriesOfTitle(ATitle: string): TSeries;
    function SpanToSeriesIndex(ASeriesIndex, ASpanIndex: integer): integer;
    procedure AddSeries(ADataset: TDataSet; ANameField, ValueFields : string;
      ATitle : string = ''); overload;
    procedure AddSeries(ASeries: TSeries; ATitle : string = ''); overload;
    procedure AddSeries(ASeries: TSeries; ATitle : string; AGraph : TCWGraph;
      AColor : TColor; AValueAxis : TValueScaleNumber); overload;
    procedure AddSeries(Values: TStringList; ATitle : string = ''); overload;
    procedure AddSeries(Values: TStringList; ATitle : string; AGraph : TCWGraph;
      AColor : TColor; AValueAxis : TValueScaleNumber); overload;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure CheckSeriesDefs;
    procedure CheckValspans;
    procedure Clear;
    procedure ClearObjects;
    procedure ClearSections(OnAxis: TScaleType; Both: Boolean);
    procedure ClearSelection;
    procedure ContractValues(Rate: integer; ContractType: TContractionType);
    procedure CreateChartFromDataset(ADataset : TDataset);
    procedure CreateSections;
    procedure DeleteSection(OnAxis: TScaleType; Index: integer);
    procedure EndUpdate;
    procedure Execute;
    procedure First;
    procedure GetValueSpan(ASeries: TSeries; var HighVal, LowVal: single); overload;
    procedure GetValueSpan(var HighVal, LowVal: single); overload;
    procedure HideRuler;
    procedure Last;
    procedure LoadDataFromCWRFile(AFileName: TFileName; DoExecute : Boolean = True);
    procedure LoadFromCache;
    procedure LoadFromDatabase(ADataset : TDataset; NameField, ValueFields : string;
      ATitle : string = ''; DoExecute : Boolean = True);
    procedure LoadFromFile(AFileName: TFileName; DoExecute : Boolean = True);
    procedure MoveSeries(FromIndex, ToIndex: integer);
    procedure NextPage;
    procedure NextPoint;
    procedure PrevPage;
    procedure PrevPoint;
    procedure RefreshChart;
    procedure RenderDesigner(Deleted : integer = 0);
    procedure RepaintGraph;
    procedure ResetContraction;
    procedure SaveAsImage(GraphElement: integer; AFileName: string);
    procedure SaveToFile(AFileName: TFileName; FileFormat : TSaveFormat =sfDataOnly;
      TimeType : TSaveTimeType =ttLocal);
    procedure ScrollBy(Delta: integer);
    procedure ScrollTo(AScrollIndex: integer);
    procedure SetAlternativeGraph(const Value: TCWGraph);
    procedure SetChartAs(AChart : TCWChart; AsGraph : TCWGraph);
    procedure SetSelection(StartIndex, EndIndex: integer; Add: Boolean = False); overload;
    procedure SetValueSpan(LowValue, HighValue: single);
    procedure Unzoom;
    procedure Zoom(NameSectionIndex: integer); overload;
    procedure Zoom(StartIndex, EndIndex: integer); overload;
    procedure Zoom; overload;
    property AbsoluteItemCount : integer read GetAbsoluteItemCount;
    property ActiveGraph: TCWGraph read GetActiveGraph;
    {Represents the master graph of a possibly mixed chart. If mixed the bar will
    always be the master, while the curve must adapt it's point postions to the bar center points}
    property AlternativeGraph : TCWGraph read GetAlternativeGraph write SetAlternativeGraph;
    property AxisOrientation: TAxisOrientation read GetAxisOrientation write SetAxisOrientation;
    property Categories : TCWCategories read GetCategories;
    property ChartList : TChartList read FChartList;
    property Contraction: integer read GetContraction write SetContraction;
    property Count: integer read GetCount;
    property CurrentPage : integer read GetCurrentPage;
    property CWBoundsRect : TRect read GetCWBoundsRect;
    property GradientWall : Boolean read GetGradientWall write SetGradientWall;
    property GraphBGColor: TColor read GetGraphBGColor write SetGraphBGColor;
    property GraphHeight: integer read GetGraphHeight;
    property GraphLeft: integer read GetGraphLeft;
    property GraphPrintRect: TRect read GetGraphPrintRect;
    property GraphRect: TRect read GetGraphRect;
    property GraphSpaceRect: TRect read GetGraphSpaceRect;
    property GraphTop: integer read GetGraphTop;
    property GraphWidth: integer read GetGraphWidth;
    property IsUpdating: Boolean read GetIsUpdating;
    property NameCount: integer read GetNameCount;
    property NameLabelFreq: integer read FNameLabelFreq write SetNameLabelFreq;
    property Names[Index: integer]: string read GetNames;
    property NameScale : TCWNameScale read GetNameScale;
    property NameSectionCount: integer read GetNameSectionCount;
    property NameSectionDefs: TCWNameSectionDefs read GetNameSectionDefs
      write SetNameSectionDefs;
    property NameSections[Index: integer]: TSection read GetNameSections;
    property OldActive: TCWGraph read FOldActive;
    property OverflowError : Boolean read GetOverflowError;
    property PageCount : integer read GetPageCount;
    property PageEnd : integer read GetPageEnd;
    property PageStart : integer read GetPageStart;
    property Percentages : Boolean read GetPercentages write SetPercentages;
    property Rendered: Boolean read GetRendered;
    property Scrollable: Boolean read GetScrollable;
    property ScrollIndex: integer read FScrollIndex write SetScrollIndex;
    property Series[Index: integer]: TSeries read GetSeries;
    property SeriesItems[SeriesIndex, ItemIndex: integer]: TSeriesItem read GetSeriesItems;
    property SpanType : TSpanType read GetSpanType write SetSpanType;
    property TextTilting: TTextOrientations read GetTextTilting write SetTextTilting;
    property TextTiltThreshold: integer read GetTextTiltThreshold write SetTextTiltThreshold;
    property TimeFormat : string read GetTimeFormat write SetTimeFormat;
    property TitleAlignment : TAlignment read GetTitleAlignment write SetTitleAlignment;
    property TitleFont : TFont read GetTitleFont write SetTitleFont;
    property ValueCount: integer read GetValueCount;
    property ValueFloatUnit : single read GetValueFloatUnit;
    property ValueLabelFreq: integer read FValueLabelFreq write SetValueLabelFreq;
    property ValueLabelFreq2: integer read FValueLabelFreq2 write FValueLabelFreq2;
    property ValueScale1 : TCWValueScale read GetValueScale1;
    property ValueScale2 : TCWValueScale read GetValueScale2;
    property ValueSectionCount: integer read GetValueSectionCount;
    property ValueSectionDefs: TCWValueSectionDefs read GetValueSectionDefs
      write SetValueSectionDefs;
    property ValueSections[Index: integer]: TSection read GetValueSections;
    property VisibleCount: integer read GetVisibleCount;
    property WallBorderColor: TColor read GetWallBorderColor write SetWallBorderColor;
    property WallColor : TColor read GetWallColor write SetWallColor;
    property WallWidth: integer read GetWallWidth write SetWallWidth;
    property WorkRect: TRect read GetWorkRect;
  protected
    {Not published. Doubt about benefits. Setting these properties > 0 freezes the graph dimesions
    in the desired direction. }
    property FixedGraphHeight : integer read FFixedGraphHeight write SetFixedGraphHeight default 0;
    property FixedGraphWidth : integer read FFixedGraphWidth write SetFixedGraphWidth default 0;

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

    property AfterBuildChart : TNotifyEvent read FAfterBuildChart write FAfterBuildChart;
    property AfterDrawGraph: TDrawGraphEvent read FAfterDrawGraph write FAfterDrawGraph;
    property BezierMargin : integer read FBezierMargin write SetBezierMargin default 0;
    property Border: Boolean read FBorder write SetBorder default False;
    property Centered : Boolean read FCentered write SetCentered default false;
    property Chart : TCWChart read FChart write SetChart;
    property ContractionType: TContractionType read FContractionType write FContractionType default ctExplicit;
    property GraphBorders: TGraphBorders read GetGraphBorders  write SetGraphBorders default gbAxis;
    property Language: string read FLanguage write SetLanguage;
    property LiveResize : Boolean read FLIveResize write FLiveResize default true;
    property MouseInfo: TMouseInfo read FMouseInfo write FMouseInfo default miBoth;
    property MousePrecision: TMousePrecision read GetMousePrecision write FMousePrecision default mpHigh;
    property MouseSelect: Boolean read FMouseSelect write FMouseSelect default True;
    property MouseTimeFormat: string read GetMouseTimeFormat write SetMouseTimeFormat;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnDrawLabel: TDrawLabelEvent read FOnDrawLabel write FOnDrawLabel;
    property OnDrawSection: TDrawSectionEvent read FOnDrawSection  write FOnDrawSection;
    property OnLoadFile : TLoadFileEvent read FOnLoadFile write FOnLoadFile;
    property OnMeasureLabel: TMeasureLabelEvent read FOnMeasureLabel write FOnMeasureLabel;
    property OnMouseInfo: TMouseInfoEvent read FOnMouseInfo write FOnMouseInfo;
    property OnQuerySpace : TQuerySpaceEvent read FOnQuerySpace write FOnQuerySpace;
    property OnRuler: TNotifyEvent read FOnRuler write FOnRuler;
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
    property RulerGuideLines: Boolean read FRulerGuideLines write FRulerGuideLines default false;
    property Rulers: TRulers read FRulers write SetRulers default ruNone;
    property SelectionExec: TSelectionExec read FSelectionExec write FSelectionExec default seZoom;
  end;


function qt(AStr: string): string;
function FormatNum(ANumber: single; Precision: integer; ThousandSep : Boolean = false): string;
function InvertColor(Color : TColor) : TColor;

var
  StrictProtection : Boolean;


implementation

uses DateUtils, Dialogs, System.Generics.Defaults, JPEG, Vcl.Imaging.pngImage;

var
  Fmt: TFormatSettings;
  WriterList : TWriterList;
  {WriterList keeps track of all graphs objects that interacts with a writer.
  Its is used to check if an object tries to link to more than one writer.
  This is not allowed and causes an exception. AddToWlist updates the list}
const
{Error message board. Many messages are depricated. Should be cleaned up.}
  MsgCnt = 111;
  ErrorMsg: array [0 .. MsgCnt - 1] of string = (
    { 0 } 'Value is tied to Series bounds',
    { 1 } 'Not enough space to render this diagram',
    { 2 } 'Cannot use pie as an alternative graph in a span chart',
    { 3 } 'Cannot set alternative graph on a double value axis chart',
    { 4 } 'Low value must be lower than High value',
    { 5 } 'Number intervals must be equal',
    { 6 } 'Date and time intervals must be equal',
    { 7 } 'Series are active. You must clear the graph before you can add new series',
    { 8 } 'Cannot set alternative graph on a chart with different graphs',
    { 9 } 'No data loaded',
    { 10 } 'Value not in span %s',
    { 11 } 'Length of Series must be equal for general purpose data types',
    { 12 } '%s is not present in Series',
    { 13 } '%s must be greater than its predecessor',
    { 14 } 'No series defined in chart %s',
    { 15 } 'A series must at least contain two values',
    { 16 } 'Minimum 2 series when curve style is neighbor areas',
    { 17 } 'Max 3 series when curve style is NeigborArea',
    { 18 } 'Cannot mix the Points style with other styles',
    { 19 } 'Series must be neighbors.',
    { 20 } '%s index out of bounds',
    { 21 } 'Name cannot be empty',
    { 22 } 'Cannot delete an auto section',
    { 23 } 'Start value greater than end value',
    { 24 } 'Cannot mix the NeighborArea style with other styles',
    { 25 } 'Cannot mix this style with the NeighborArea style',
    { 26 } 'Start date greater than end date',
    { 27 } 'Start number greater than end number',
    { 28 } 'Cannot mix this style with the Points style',
    { 29 } 'DiscDepth percentage must be in the range of -1 to -10',
    { 30 } 'Slope must be in the range of 0 - 70 degrees',
    { 31 } 'StartAngle must be in the range of 0 - 360 degrees',
    { 32 } 'Max 3 decimals',
    { 33 } 'Minimum pie size is 50',
    { 34 } 'Graph not assigned',
    { 35 } 'Item spacing must be positive',
    { 36 } 'Contraction is only allowed with spanned charts',
    { 37 } 'Contraction failed. Insufficient result.',
    { 38 } 'Only bars allowed for this chart type',
    { 39 } 'Must be 0 or >= 100',
    { 40 } 'Contraction only allowed on spanned series',
    { 41 } 'No active graph',
    { 42 } '%s is not a number',
    { 43 } 'Cannot decrease Contraction',
    { 44 } '%s is not a valid CW file',
    { 45 } 'Duplicate series name %s',
    { 46 } 'Note! Only two graphs allowed in a correlation view',
    { 47 } 'No data found',
    { 48 } 'Missing data terminator',
    { 49 } 'Could not resolve graph type',
    { 50 } 'Cannot access protected property %s',
    { 51 } 'Alternative must be different from the first priority graph',
    { 52 } 'Cannot contract a paged diagram',
    { 53 } 'Horizontal curves are not supported',
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
    { 72 } 'SpanType and DateTimeTemplate are not compliant.',
    { 73 } 'SpanType and AutoSection are not compliant.',
    { 74 } '%s is not a valid date / time',
    { 75 } 'Invalid number (%s). Template values must be integers',
    { 76 } 'Section value (%s) out of scope',
    { 77 } 'Insufficient data',
    { 78 } 'Name type and date / time format mismatch',
    { 79 } 'Note that walls can only be shown when name / value axis Orientation is bottom / left',
    { 80 } 'Note that walls cannot be shown when there are no graph borders defined',
    { 81 } 'Pies can only ber hosted by TCWPieChart type',
    { 82 } 'Name type is not compatible with the loaded data',
    { 83 } 'Cannot use compression when layout is side by side',
    { 84 } '%s is not supported by this chart type',
    { 85 } 'Cannot insert pies in an axis chart',
    { 86 } 'Cannot insert axis graphs in a pie chart',
    { 87 } 'Writer not assigned',
    { 88 } 'No active chart',
    { 89 } 'Graph not in chart',
    { 90 } 'Cannot use side by side layout when overflow action is compression',
    { 91 } 'Duplicate Legend',
    { 92 } 'Undefined Graph in Chart',
    { 93 } 'Series titles incomplete in data',
    { 94 } 'Title not found (%s)',
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
   { 108 } 'Cannot share %s among different writers',
   { 109 } 'Compression cannot be applied on category items.',
   { 110 } 'Cannot combine scrolling and animation'
    );

  msg_TiedToSeries = 0;
  msg_NoSpace = 1;
  msg_PieSpanAlternative = 2;
  msg_DoubleAxisAlternative = 3;
  msg_LowHigh = 4;
  msg_NumIntervals = 5;
  msg_DateIntervals = 6;
  msg_ActiveSeries = 7;
  msg_DifferentGrapsAlternative = 8;
  msg_DataEmpty = 9;
  msg_NotInSpan = 10;
  msg_EqualLength = 11;
  msg_NotPresent = 12;
  msg_GreaterPred = 13;
  msg_NoSeries = 14;
  msg_TwoValues = 15;
  msg_NeighborTwoSeries = 16;
  msg_NeighborThreeSeries = 17;
  msg_MixPointsOthers = 18;
  msg_NotNeighborSeries = 19;
  msg_IndexBounds = 20;
  msg_NameEmpty = 21;
  msg_DelAutoSect = 22;
  msg_StartEndValue = 23;
  msg_MixNeighborOthers = 24;
  msg_MixThisNeighbor = 25;
  msg_StartDateEndDate = 26;
  msg_StartNumEndNum = 27;
  msg_MixThisPoints = 28;
  msg_DiscDepth = 29;
  msg_PieSlope = 30;
  msg_StartAngle = 31;
  msg_Max3Decimals = 32;
  msg_MinPieSize = 33;
  msg_GraphNotAssigned = 34;
  msg_ItemSpacing = 35;
  msg_ContractCategory = 36;
  msg_ContractInsufficient = 37;
  msg_BarsOnly = 38;
  msg_FixedGraphWidth = 39;
  msg_ContractionDenied = 40;
  msg_NoActiveGraph = 41;
  msg_InvalidNumber = 42;
  msg_DecreaseContraction = 43;
  msg_FileFormat = 44;
  msg_DupSerName = 45;
  msg_DupItemName = 46;
  msg_NoDataFound = 47;
  msg_MissingTerminator = 48;
  msg_ResolveGraphType = 49;
  msg_PrivateProp = 50;
  msg_AlternativeSelf = 51;
  msg_ContractPaged = 52;
  msg_HorizontalCurves = 53;
  msg_UndefViewMember = 54; {NotUsed}
  msg_NothingToSave = 55;
  msg_NameMismatch = 56;
  msg_DupName = 57;
  msg_SectIncompatible = 58; {NotUsed}
  msg_DupGraph = 59; {NotUsed}
  msg_GraphOneView = 60; {NotUsed}
  msg_NoWriter = 61; {NotUsed}
  msg_NoMember = 62; {NotUsed}
  msg_SeriesSpacesTooSmall = 63;
  msg_TimeSpanZero = 64;
  msg_FlowSideBySide = 65; {NotUsed}
  msg_StackedFlow = 66; {NotUsed}
  msg_Angle1070 = 67;
  msg_RelationMin8 = 68; {NotUsed}
  msg_DoughnutRange = 69;
  msg_SectStartValue = 70;
  msg_SectEndValue = 71;
  msg_SpanTypeTemplate = 72;
  msg_SpanTypeAutoSect = 73;
  msg_InvalidDateTime = 74;
  msg_TemplateIntegers = 75;
  msg_SectValueScope = 76;
  msg_InSufficientData = 77; {NotUsed}
  msg_SpanTypeDtFormatMismatch = 78;
  msg_WallOrientation = 79;
  msg_WallBorder = 80;
  msg_PieGeneral = 81;
  msg_SpanTypeData = 82;
  msg_CompressionSideBySide = 83;
  msg_OverflowAction = 84;
  msg_PieAxisChart = 85; {NotUsed}
  msg_AxisPieChart = 86; {NotUsed}
  msg_WriterNotAssigned = 87;
  msg_NoActiveChart = 88;
  msg_GraphNotInChart = 89;
  msg_SideBySideCompression = 90;
  msg_DupLegend = 91;
  msg_UndefGraph = 92;
  msg_TitlesIncomplete = 93;
  msg_TitleNotFound = 94;
  msg_SpanTypeConflict = 95;
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
  msg_GeneralCompression = 109;
  msg_ScrollAnimation = 110; {Not relevant}

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

  TMovingAverage = record
  private
    buffer: TArray<Double>;
    head: Integer;
    Capacity: Integer;
    Count: Integer;
    sum, fValue: Double;
  public
    constructor Create(aCapacity: Integer);
    function Add(Value: Double): Double;
    procedure Reset;
    property Value: Double read fValue;
  end;

function TMovingAverage.Add(Value: Double): Double;
begin
  head := (head + 1) mod Capacity;
  sum := sum + Value - buffer[head];
  buffer[head] := Value;

  if count < capacity then
  begin
    inc(Count);
    fValue := sum / count;
    exit(fValue);
  end;
  fValue := sum / Capacity;
  Result := fValue;
end;

constructor TMovingAverage.Create(aCapacity: Integer);
begin
  Capacity := aCapacity;
  SetLength(buffer, aCapacity);
  Reset;
end;

procedure TMovingAverage.Reset;
var
  i: integer;
begin
  head := -1;
  Count := 0;
  sum := 0;
  fValue := 0;
  for i := 0 to High(buffer) do
    buffer[i] := 0;
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
  if not StrictProtection and (ErrorCode = msg_PrivateProp) then
    Exit;

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
Var
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

(*procedure DeleteFromWList(AWriter : TChartWriter);
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
end;*)

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

procedure ColorBlend(const ACanvas: HDC; const ARect: TRect;
  const ABlendColor: TColor; const ABlendValue: Integer);
var
  DC: HDC;
  Brush: HBRUSH;
  Bitmap: HBITMAP;
  BlendFunction: TBlendFunction;
begin
  DC := CreateCompatibleDC(ACanvas);
  Bitmap := CreateCompatibleBitmap(ACanvas, ARect.Right - ARect.Left,
    ARect.Bottom - ARect.Top);
  Brush := CreateSolidBrush(ColorToRGB(ABlendColor));
  try
    SelectObject(DC, Bitmap);
    Windows.FillRect(DC, Rect(0, 0, ARect.Right - ARect.Left,
      ARect.Bottom - ARect.Top), Brush);
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.AlphaFormat := 0;
    BlendFunction.SourceConstantAlpha := ABlendValue;
    Windows.AlphaBlend(ACanvas, ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, DC, 0, 0,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, BlendFunction);
  finally
    DeleteObject(Brush);
    DeleteObject(Bitmap);
    DeleteDC(DC);
  end;
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


function InvertColor(Color: TColor): TColor;
const
   WhiteRGB = clWhite;
   BlackRGB = clBlack;
  var
    BackgroundRGB: Longint;
    BackgroundR, BackgroundG, BackgroundB: Byte;
    BackgroundLuminance: Double;

  begin
    BackgroundRGB := ColorToRGB(Color);
    BackgroundR := GetRValue(BackgroundRGB);
    BackgroundG := GetGValue(BackgroundRGB);
    BackgroundB := GetBValue(BackgroundRGB);

  { Calculate the relative luminance of the background color, OpenAI}
  BackgroundLuminance := (0.2126 * BackgroundR + 0.7152 * BackgroundG + 0.0722 * BackgroundB) / 255;

  { Decide whether to use white or black foreground based on the background luminance}
    if BackgroundLuminance > 0.5 then
     Result := BlackRGB
    else
     Result := WhiteRGB;
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

function MakeGPBrush(ASource: TBrush; BGColor : TColor; FGColor : TColor = clNone) : TGPBrush;
var
  R, G, B: byte;
  Clr : integer;
  GPClr, GPBGClr : TGPColor;
  HStyle : THatchStyle;
begin
  if FGColor = clNone then
    Clr := ColorToRgb(ASource.Color)
  else
    Clr := FGColor;
  R := GetRValue(Clr);
  G := GetGValue(Clr);
  B := GetBValue(Clr);
  GPClr := MakeColor(R, G, B);

  R := GetRValue(BGColor);
  G := GetGValue(BGColor);
  B := GetBValue(BGColor);
  GPBGClr := MakeColor(R, G, B);


  if ASource.Style in [bsHorizontal..bsDiagCross] then
  begin
    HStyle := HatchStyleHorizontal;
    case ASource.Style of
      bsHorizontal:HStyle := HatchStyleHorizontal;
      bsVertical:HStyle := HatchStyleVertical;
      bsFDiagonal:HStyle := HatchStyleForwardDiagonal;
      bsBDiagonal:HStyle := HatchStyleBackwardDiagonal;
      bsCross:HStyle := HatchStyleCross;
      bsDiagCross:HStyle := HatchStyleDiagonalCross;
    end;
    Result := TGPHatchBrush.Create(HStyle, GPClr, GPBGClr);
  end
  else
    Result := TGPSolidBrush.Create(GPClr);
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

procedure TranslatePenStyle(APen : TPen; AGPPen : TGPPen);
begin
  AGPPen.SetDashStyle(dashStyleSolid);
  case Apen.Style of
    psDash: AGPPen.SetDashStyle(dashStyleDash);
    psDot: AGPPen.SetDashStyle(dashStyleDot);
    psDashDot: AGPPen.SetDashStyle(dashStyleDashDot);
    psDashDotDot: AGPPen.SetDashStyle(dashStyleDashDotDot);
    psClear: ;
    psInsideFrame: ;
    psUserStyle: ;
    psAlternate:;
  end;
  AGPPen.SetWidth(APen.Width);
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

type
  TPointD = record
    X, Y: Double;
  end;

function CalculateTangentPoint(const Center: TPointD; SemiMajorAxis, SemiMinorAxis, AngleDegrees: Double): TPointD;
var
  AngleRadians, CosTheta, SinTheta: Double;
  TangentX, TangentY: Double;
begin
  // Convert the angle from degrees to radians
  AngleRadians := DegToRad(AngleDegrees);

  // Calculate the cosine and sine of the angle
  CosTheta := Cos(AngleRadians);
  SinTheta := Sin(AngleRadians);

  // Calculate the x and y coordinates of the tangent point
  TangentX := Center.X + SemiMajorAxis * CosTheta;
  TangentY := Center.Y - SemiMinorAxis * SinTheta; // Note the negative sign due to the y-axis orientation

  Result.X := TangentX;
  Result.Y := TangentY;
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
    case CW.Chart.SpanType of
      ntMonthSpan: AFormat := 'mm';
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
      if Writer.FNameAxis.IsXAxis then
        Result := Writer.Chart.NameScale.Font.Orientation
    end
    else if LabelKind = lkValue then
    begin
      if Writer.FValueAxis.IsXAxis then
        Result := Writer.Chart.ValueScale1.Font.Orientation
    end
    else if LabelKind = lkValue2 then
    begin
      if Writer.FValueAxis2.IsXAxis then
        Result := Writer.Chart.ValueScale2.Font.Orientation
    end
    else if (LabelKind = lkValueSection) then
    begin
      if Writer.FValueAxis.IsXAxis then
      begin
        if Writer.ValueSectionDefs <> nil then
          Result := Writer.ValueSectionDefs.Font.Orientation;
      end;
    end
    else if (LabelKind = lkNameSection) then
    begin
      if Writer.FNameAxis.IsXAxis then
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
  Result := StrToFloat(S, Fmt);
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
  S : string;
  TT : string;
  P : integer;
  HasData : Boolean;
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
  S := WorkSL[0];
  if S.StartsWith('[TIME TYPE') then
  begin
    S := Trim(S);
    P := Pos('=', S);
    TT := Copy(S, P+1, Length(S)-P-1);
    if SameText(TT, 'UNIX') then
      FTimeType := ttUnix
    else if SameText(TT, 'ISO8601') then
      FTimeType := ttISO8601
    else
      FTimeType := ttLocal;
    WorkSl.Delete(0);
  end
  else
    FTimeType := ttLocal;


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
      HasData := false;
      sl := TStringList.Create;
      sl.DefaultEncoding := TEncoding.Utf8;
      repeat
        for i := Indx to WorkSl.Count - 1 do
        begin
          if WorkSl[i] = EOF then
          begin
            if sl.Count > 0 then
            begin
              FDataContent.Add(sl);
              HasData := True;
            end;
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

      if not HasData then
       sl.Free;
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

{ TChartList ----------------------------------------------------}

procedure TChartList.SetItemIndex(const Value : integer);
var
  CL : TChartListItem;
begin
  if csDesigning in FWriter.ComponentState then
    Exit;
  if Value = FItemIndex then
    Exit;
  if (Value > Count-1) or (Value <-1) then
    ShowGWError(msg_IndexBounds);
  FItemIndex := Value;
  if Value = -1 then
  begin
    FWriter.Chart := nil;
    Exit;
  end;
  Cl := FItems[Value];
  if CL.Graph <> nil then
     FWriter.SetChartAs(Cl.Chart, CL.Graph)
   else
   begin
     FWriter.Chart := Cl.Chart;
   end;
end;

function TChartList.GetItems(Index: Integer) : TChartListItem;
begin
     Result := FItems[Index];
end;

constructor TChartList.Create;
begin
   FItems := TChartListItems.Create;
   FPrevChart := TChartListItem.Create;
   FItemIndex := -1;
end;

destructor TChartList.Destroy;
begin
   FItems.Free;
   FPrevChart.Free;
   inherited;
end;

function TChartList.GetCount : integer;
begin
  Result := FItems.Count;
end;

procedure TChartList.Next;
begin
  if CanMoveNext then
  begin
    ItemIndex := ItemIndex + 1;
  end;
end;

procedure TChartList.Prev;
begin
  if CanMovePrev then
  begin
    ItemIndex := ItemIndex - 1;
  end;
end;

function TChartList.CanMoveNext : Boolean;
begin
   Result := ItemIndex < Count-1;
end;

function TChartList.CanMovePrev : Boolean;
begin
   Result := ItemIndex > 0;
end;

procedure TChartList.Organize;
var
  Indx : integer;
  I : integer;
  C : TCWChart;
  Itm : TChartListItem;
  S : string;
  function GetGraphType(AGraph : TCWGraph) : string;
  begin
    if AGraph is TCWCurve then
      Result := 'Curve'
    else if AGraph is TCWBar then
      Result := 'Bar'
    else
      Result := 'Pie';
  end;

begin
   FItems.Clear;
   C := nil;
    for I := 0 to FWriter.InternalChartList.Count-1 do
    begin
      C := FWriter.InternalChartList[i];
      if C.SeriesDefs.Count = 0 then
      begin
         FWriter.GoBackError;
         if C.Title <> '' then
           ShowGWError(msg_NoSeries, C.Title)
         else
           ShowGWError(msg_NoSeries, 'Untitled')
      end;
      if C.SeriesDefs[0].FGraph = nil then
      begin
         FWriter.GoBackError;
         ShowGWError(msg_GraphNotAssigned);
      end;
      if C.FAlternativeGraph <> nil then
      begin
        Itm := TChartListItem.Create;
        Itm.FChart := C;
        Itm.FGraph := C.SeriesDefs[0].Graph;
        Itm.FTitle := C.FTitle + ' (' + GetGraphType(C.SeriesDefs[0].Graph) + ')';
        FItems.Add(Itm);

        Itm := TChartListItem.Create;
        Itm.FChart := C;
        Itm.FGraph := C.FAlternativeGraph;
        Itm.FTitle := C.FTitle + ' (' + GetGraphType(Itm.Graph) + ')';
        FItems.Add(Itm);
      end
      else
      begin
        S := C.Title;
        Itm := TChartListItem.Create;
        Itm.FChart := C;
        if C.AxisCount > 1 then
         Itm.FGraph := nil
        else
         Itm.FGraph := C.SeriesDefs[0].Graph;
        Itm.FTitle := C.FTitle;
        FItems.Add(Itm);
      end;
    end;
    if C = nil then
      Exit;

    if C.AxisCount = 1 then
     Indx := IndexOf(FWriter.Chart, FWriter.ActiveGraph)
    else
     Indx := IndexOf(FWriter.Chart, nil);
    if Indx <> FItemIndex then
    begin
     FItemIndex := Indx;
     if Assigned(FOnChange) then
      FOnChange(Self);
    end;
 end;

procedure TChartList.Add(AChart : TCWChart);
var
  Indx : integer;
  function GetGraphType(AGraph : TCWGraph) : string;
  begin
    if AGraph is TCWCurve then
      Result := 'Curve'
    else if AGraph is TCWBar then
      Result := 'Bar'
    else
      Result := 'Pie';
  end;

begin
   if csDesigning in FWriter.ComponentState then
     Exit;
   Indx := FWriter.InternalChartList.IndexOf(AChart);
   if Indx <> -1 then
     Exit;
   FWriter.InternalChartList.Add(AChart);
   Organize;
end;

function TChartList.IndexOf(AChart : TCWChart; AGraph :TCWGraph) : integer;
var
  i : integer;
  Hit : Boolean;
begin
  Result := -1;
  for I := 0 to Count-1 do
  begin
    if AGraph = nil then
     Hit := (FItems[i].Chart = AChart)
    else
     Hit := (FItems[i].Chart = AChart) and (FItems[i].Graph = AGraph);
    if Hit then
    begin
      Result := i;
      Break;
    end;
  end;
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
  Result := FValue / FOwner.Sum * 100;
end;

function TSeriesItem.GetVal : single;
begin
  Result := FValue;
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
end;

{TGraphObject-----------------------------------------------------}

function TGraphObject.GetChart : TCWChart;
  begin
    Result := nil;
    if FWriter <> nil then
      Result := FWriter.Chart;
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

function TAxisObject.GetQualifierRect : TRect;
var
  L, T, R, B: integer;
  SectSpace : integer;
begin
  T := 0;
  L := 0;
  R := 0;
  B := 0;
  if Position in [apTop, apBottom] then
  begin
    L := GrRect.Left;
    R := GrRect.Right;
  end
  else if Position in [apLeft, apRight] then
  begin
    B := GrRect.Bottom + GetWallOffset;
    T := GrRect.Top + GetWallOffset;
  end;

  if OpposedSections then
    SectSpace := 0
  else
    SectSpace := SectionSpace;
  if Position = apTop then
  begin
    T := GrRect.Top - LabelSpace - GetQualifierSpace - SectSpace;
    B := T + GetQualifierSpace;
  end
  else if Position = apBottom then
  begin
    B := GrRect.Bottom + LabelSpace + GetFloorRect.Height + GetQualifierSpace + SectSpace;
    T := B - GetQualifierSpace;
  end
  else if Position = apLeft then
  begin
    L := GrRect.Left - LabelSpace - GetWallWidth - SectSpace-GetQualifierSpace;
    R := L + GetQualifierSpace;
  end
  else if Position = apRight then
  begin
    L := GrRect.Right + LabelSpace + SectSpace;
    R := L + GetQualifierSpace;
  end;
  Result := Rect(L, T, R, B);
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
    L := GrRect.Left;
    R := GrRect.Right;
    HookL := L;
    TxtL := L;
    HookR := R;
    TxtR := R;
  end
  else if Position in [apLeft, apRight] then
  begin
    B := GrRect.Bottom + GetWallOffset;
    T := GrRect.Top + GetWallOffset;
    HookB := B;
    TxtB := B;
    HookT := T;
    TxtT := T;
  end;

  if Position = apTop then
  begin
    T := GrRect.Top - LabelSpace;
    B := T + LabelSpace;
    HookB := B;
    HookT := B - c_HookSpace;
    TxtB := HookT;
    TxtT := T;
  end
  else if Position = apBottom then
  begin
    B := GrRect.Bottom + LabelSpace + GetFloorRect.Height;
    T := B - LabelSpace;
    HookB := T + c_HookSpace;
    HookT := T;
    TxtT := HookB;
    TxtB := B;

  end
  else if Position = apLeft then
  begin
    L := GrRect.Left - LabelSpace - GetWallWidth;
    R := L + LabelSpace;
    HookR := R;
    HookL := R - c_HookSpace;
    TxtR := HookL;
    TxtL := L;
  end
  else if Position = apRight then
  begin
    L := GrRect.Right;
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
  Y := GrRect.Bottom + Writer.Chart.WallWidth * Sin(a);
  Result.Top := GrRect.Top;
  Result.Left := GrRect.Left - Writer.Chart.WallWidth;
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
  Y := GrRect.Top + Writer.Chart.WallWidth * Sin(a);
  R := GetWallRect;
  P1 := Point(R.Left, round(Y));
  P2 := GrRect.TopLeft;
  P3 := Point(GrRect.Left, GrRect.Bottom);
  P4 := Point(GrRect.Left - Writer.Chart.WallWidth, R.Bottom);
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
    Result := Writer.Chart.WallWidth;
end;

function TAxisObject.GetFloorPoly: TIntPointArray;
var
  P1, P2, P3, P4: TPoint;
  X: single;
  R: TRect;
begin
  R := GetFloorRect;
  P1 := Point(R.Left, R.Bottom);
  X := R.Right - Writer.Chart.WallWidth;
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
  if not Writer.IsAxisChart then
    Exit;
  if Classtype = TNameAxis then
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
    L := GrRect.Left;
    R := GrRect.Right;
  end
  else if Position in [apLeft, apRight] then
  begin
    T := GrRect.Top;
    B := GrRect.Bottom;
  end;

  if Position = apTop then
  begin
    if OpposedSections then
    begin
      T := GrRect.Bottom;
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
      B := GrRect.Top;
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
      L := GrRect.Right;
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
      R := GrRect.Left;
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

function TAxisObject.GetQualifier;
begin
  Result := '';
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
  Prolong: Boolean;
  TextRects: array of TRect;

  function DoEvent(P: TPoint; AElement: TSectionElement): Boolean;
  var
    Ax: TScaleType;
  begin
    Result := False;
    if ClassType = TNameAxis then
      Ax := stNameScale
    else if ClassType = TValueAxis2 then
      Ax := stValueScale2
    else
      Ax := stValueScale1;
    if Assigned(Writer.FOnDrawSection) then
    begin
      Writer.FOnDrawSection(Writer, Ax, P, Sections[i], AElement,
        Canvas, Result);
    end;
  end;

  function GetTextSize(AText: string): TSize;
  begin
    if ClassType = TNameAxis then
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
    if ClassType = TValueAxis then
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
    if (ClassType = TValueAxis) and (Writer.Chart.ValueSectionDefs <> nil) then
    begin
      Canvas.Pen.Assign(Writer.Chart.ValueSectionDefs.Pen);
      Writer.SetLabelFont(lkValueSection);
    end
    else if (Writer.Chart.NameSectionDefs <> nil) then
    begin
      Canvas.Pen.Assign(Writer.Chart.NameSectionDefs.Pen);
      Writer.SetLabelFont(lkNameSection);
    end;

  end;

  function GetRect(X, Y : integer; Txt : string) : TRect;
  var
    W : integer;
  begin
    Result.Left := X;
    Result.Top := Y;
    Result.Bottom := Writer.GetTextHeight(lkValueSection);
    if ClassType = TValueAxis then
    begin
      W := Writer.GetTextWidth(lkValueSection, Txt);
      Result.Right := Result.Left + W;
    end
    else
    begin
      W := Writer.GetTextWidth(lkNameSection, Txt);
      Result.Right := Result.Left + W;
    end;
  end;

  procedure LogTextRect(X, Y : integer; Txt: string);
  var
    R : TRect;
  begin
    R := GetRect(X, Y, Txt);
    setlength(TextRects, Length(TextRects) + 1);
    TextRects[High(TextRects)] := R;
  end;

  function CheckEdges(TxtRect : TRect) : Boolean;
  begin
    Result := True;
    if IsXAxis and (TxtRect.Right > Writer.ClientRect.Right) then
    begin
      Result := false;
      Exit;
    end
    else if IsXAxis and (TxtRect.Left < Writer.ClientRect.Left) then
    begin
      Result := false;
      Exit;
    end
    else if not IsXAxis and (TxtRect.Bottom > Writer.ClientRect.Bottom) then
    begin
      Result := false;
      Exit;
    end
    else if not IsXAxis and (TxtRect.Top < Writer.ClientRect.Top) then
    begin
      Result := false;
      Exit;
    end;
  end;

  function TextFits(X, Y : integer; Short : string; var Txt: string): Boolean;
  var
    i: integer;
    R: TRect;
  begin
    R := GetRect(X, Y, Txt);
    Result := CheckEdges(R);
    if not Result and (Short <> '') then
    begin
      R := GetRect(X, Y, Txt);
      Result := CheckEdges(R);
    end;
    if not Result then
      Exit;

    for i := 0 to High(TextRects) do
    begin
      if TextRects[i].IntersectsWith(R) then
      begin
        Result := False;
        Break;
      end;
    end;
    if not Result and (Short <> '') then
    begin
      Txt := Short;
      Result := True;
    end
    else
      Exit;
    R := GetRect(X, Y, Txt);
    for i := 0 to High(TextRects) do
    begin
      if TextRects[i].IntersectsWith(R) then
      begin
        Result := False;
        Break;
      end;
    end;

  end;

  function FirstSection : integer;
  var
    i : integer;
  begin
    Result := -1;
    for I := 0 to SectionCount-1 do
    begin
      if Sections[i].SectionType = stSection then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

  function LastSection : integer;
  var
    i : integer;
  begin
    Result := -1;
    for I := 0 to SectionCount-1 do
    begin
      if Sections[i].SectionType = stSection then
      begin
        Result := i;
      end;
    end;
  end;

begin
  Gr := GrRect;
  if (ClassType = TValueAxis2) then
    Exit;

  if ((ClassType = TValueAxis) and (Writer.Chart.ValueSectionDefs = nil)) or
    ((ClassType = TNameAxis) and (Writer.Chart.NameSectionDefs = nil)) then
    Exit;

  if ((ClassType = TValueAxis) and not Writer.Chart.ValueSectionDefs.Visible) or
    ((ClassType = TNameAxis) and not Writer.Chart.NameSectionDefs.Visible) then
    Exit;

  if ClassType = TValueAxis then
  begin
    SetCanvas;
    Prolong := Writer.Chart.ValueSectionDefs.ProlongedLines;
  end
  else
  begin
    SetCanvas;
    Prolong := Writer.Chart.NameSectionDefs.ProlongedLines;
  end;
  try
    Finalize(TextRects);
    for i := 0 to SectionCount - 1 do
    begin
      SR := GetSectionRect(i);
      if (SR.Width = 0) and (SR.Height = 0) then
        Continue;
      Canvas.Brush.Style := bsClear;
      if (Sections[i].SectionType = stLine)
      or (LinesVisible and not (i = FirstSection) and not (i = LastSection)) then
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

      if ClassType = TNameAxis then
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

      if X + Sz.cx > Writer.ClientRect.Right then
       Continue
      else if Y + Sz.cy > Writer.ClientRect.Bottom then
       Continue;

      if DoEvent(Point(X, Y), seText) then
      begin
        SetCanvas;
        Continue;
      end;

      if (ClassType = TNameAxis) and (Writer.FNameSectionFreq > 0) then
      begin
        if (i Mod Writer.FNameSectionFreq = 0) then
        begin
          if UseShorts then
          begin
            if not TextFits(X, Y, '', Nm) then
              Continue
          end
          else
          begin
            if not TextFits(X, Y, Sections[i].ShortCaption, Nm) then
              Continue
          end;
          Canvas.TextOut(X, Y, Nm);
          LogTextRect(X, Y, Nm);
        end;
      end
      else
      begin
        if (Writer.FNameSectionFreq > 0) and (i Mod Writer.FValueSectionFreq = 0)  then
        begin
          if UseShorts then
          begin
            if not TextFits(X, Y, '', Nm) then
              Continue
          end
          else
          begin
            if not TextFits(X, Y, Sections[i].ShortCaption, Nm) then
              Continue
          end;
          Canvas.TextOut(X, Y, Nm);
          LogTextRect(X, Y, Nm);
        end;
      end;
      if Assigned(Writer.FOnDrawSection) then
        SetCanvas; { Might have been changed in event }
    end;
  finally
  end;
  Writer.ResetCanvas(nil);
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
  GPen := TGPPen.Create(MakeGPClr(Writer.Chart.WallBorderColor));
  if not Writer.Chart.GradientWall then
    GBrush := TGPSolidBrush.Create(MakeGPClr(Writer.Chart.WallColor))
  else
  begin
    Clr1 := MakeGPClr(Writer.Chart.WallColor);
    Clr2 := MakeGPClr(Writer.Chart.GraphBGColor);

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
    if Writer.Chart.GradientWall then
      FGDIP.FillPolygon(GradBrush, PGPPoint(@GPts[0]), 4)
    else
      FGDIP.FillPolygon(GBrush, PGPPoint(@GPts[0]), 4);
    FGDIP.DrawPolygon(GPen, PGPPoint(@GPts[0]), 4);

  finally
    GPen.Free;
    if Writer.Chart.GradientWall then
      GradBrush.Free
    else
      GBrush.Free;
    FGDIP.Free;
  end;
end;

procedure TAxisObject.DrawAxis;
var
  R: TRect;
  V : TCWScale;
begin
  if Chart is TCWPieChart then
    Exit;
  if Chart.GraphBorders = gbNone then
    Exit;
  if Writer.HasWall then
  begin
    Draw3DAxis;
  end;
  if ClassType = TValueAxis2 then
    V := Writer.Chart.ValueScale2
  else if ClassType = TValueAxis then
    V := Writer.Chart.ValueScale1
  else
    V := Writer.Chart.NameScale;


  R := GrRect;
  with Canvas do
  begin
    Pen.Color := V.Pen.Color;
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
  Writer.ResetCanvas(nil);
end;

procedure TAxisObject.Draw;
begin
  if (Writer.ActiveGraph is TCWAxisGraph) then
  begin
    if ClassType = TValueAxis then
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
  ValSpan := HighVal - LowVal;
  P := AValue - LowVal; { Get the "index" in the span }
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
  V : TCWValueScale;
begin
  if ClassType = TValueAxis2 then
    V := Chart.ValueScale2
  else
    V := Chart.ValueScale1;
  if not V.ShowLabels then
  begin
    Result := 0;
    Exit;
  end;
  Result := Writer.FValueLabelSpace;
  if ClassType = TValueAxis2 then
  begin
    lk := lkValue2;
  end
  else
  begin
    lk := lkValue;
  end;
  if not IsXAxis then
  begin
    W := Writer.GetTextWidth(lk);
    if W <> 0 then
    begin
      Result := W + c_LabelXMarg + c_HookSize;
    end;
  end
  else
  begin
    H := Writer.GetTextHeight(lk);
    if H <> 0 then
    begin
      Result := H + c_HookSize;
    end;
  end;
end;

function TValueAxis.GetSectionSpace: integer;
var
  W: integer;
  i: integer;
  sl: TStringList;
  j: integer;
  Marg : integer;
begin
  Result := 0;
  Marg := 0;
  if Chart.ValueSectionDefs = nil then
    Result := 0
  else if not Chart.ValueSectionDefs.Visible then
    Result := 0
  else
  begin
    if Writer.FValueSections.Count = 0 then
      Exit;
    if IsXAxis then
    begin
      if Chart.ValueSectionDefs <> nil then
        Marg := Chart.ValueSectionDefs.FCaptionVertMargin;
      Result := Writer.GetTextHeight(lkValueSection) + Marg;
      Exit;
    end;
    W := Writer.GetTextWidth(lkValueSection) + c_LabelXMarg;
    sl := TStringList.Create;
    try
      for i := 0 to Writer.FValueSections.Count - 1 do
      begin
        CreateListFromDelimiter('|', Writer.FValueSections[i].FLongCaption, sl);
        for j := 0 to sl.Count - 1 do
        begin
          if Writer.GetTextWidth(lkValueSection, sl[j]) + c_LabelXMarg> W then
            W := Writer.GetTextWidth(lkValueSection, sl[j]) + c_LabelXMarg;
        end;
      end;
      if Chart.ValueSectionDefs <> nil then
        Marg := Chart.ValueSectionDefs.FCaptionHorizMargin;
      Result := W + Marg;
    finally
      sl.Free;
    end;
  end;
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
  if not Chart.ValueSectionDefs.Visible then
    Exit;
  Sect := Sections[Index];
  SVal := StrToFloat(Sect.FStartVal);
  EVal := StrToFloat(Sect.FEndVal);

  if not ((SVal >= LowVal) and (SVal <= HighVal))
  and not ((EVal >= LowVal) and (EVal <= HighVal)) then
   Exit;

  if (SVal < LowVal) and (EVal > HighVal) then
    Exit;

  if SVal < LowVal then
    SVal := LowVal;

  if EVal > HighVal then
    EVal := HighVal;

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
  if ClassType = TValueAxis2 then
   Result := Chart.FValueScale2.FValueUnit
  else
   Result := Chart.FValueScale1.FValueUnit
end;

function TValueAxis.GetUnitCount: integer;
begin
  Result := 0;
  begin
    if Writer.Chart <> nil then
    begin
      if ClassType = TValueAxis2 then
        Result := Chart.ValueScale2.ValueCount
      else
        Result := Chart.ValueScale1.ValueCount
    end;
  end;
end;

function TValueAxis.GetQualifier : string;
begin
    if ClassType = TValueAxis2 then
      Result := Chart.ValueScale2.Qualifier
    else
      Result := Chart.ValueScale1.Qualifier
end;

function TValueAxis.GetQualifierSpace : integer;
var
  lk: TLabelKind;
  QW : integer;
  VS : string;
begin
  Result := 0;
  if ClassType = TValueAxis2 then
  begin
    VS := Chart.ValueScale2.Qualifier;
    lk := lkValue2;
  end
  else
  begin
    VS := Chart.ValueScale1.Qualifier;
    lk := lkValue;
  end;
  if Trim(VS) = '' then
    Exit;
  if not IsXAxis then {Draw vert}
  begin
    QW := Writer.GetTextHeight(lk);
    if QW > 0 then
      Result := QW + c_QualifierMargin*2;
  end
  else
  begin
    QW := Writer.GetTextHeight(lk);
    if QW > 0 then
      Result := QW+ c_QualifierMargin*2;
  end;
end;

function TValueAxis.GetInterval: single;
begin
    if ClassType = TValueAxis2 then
      Result := Chart.ValueScale2.ValueIntervals
    else
      Result := Chart.ValueScale1.ValueIntervals
end;

function TValueAxis.GetLow: single;
begin
   if ClassType = TValueAxis2 then
      Result := Chart.ValueScale2.ValueLow
    else
      Result := Chart.ValueScale1.ValueLow
end;

function TValueAxis.GetHigh: single;
begin
   if ClassType = TValueAxis2 then
      Result := Chart.ValueScale2.ValueHigh
    else
      Result := Chart.ValueScale1.ValueHigh
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
  LastVal : string;
  Prec: integer;
  Line3DStart, Line3DEnd: TPoint;
  SPL, SPT, SPR, SPB : integer;
  Q : string;
  V : TCWValueScale;
  lk : TLabelKind;
  Freq : integer;
  Spacing : integer;

  procedure DoWrite;
  var
    Dt: TDateTime;
    YOk, XOk: Boolean;
    Ax : TScaleType;
  begin
    if LastVal = S then
      Exit;
    if Assigned(Writer.FOnDrawLabel) then
    begin
      Dt := 0;
      Handled := False;
      if ClassType = TValueAxis2 then
        Ax := stValueScale2
      else
        Ax := stValueScale1;
      Writer.FOnDrawLabel(Writer, AX, S, Dt,
        LabelPos, Canvas, Handled);
      if not Handled then
      begin
        Canvas.TextOut(LabelPos.X, LabelPos.Y, S);
      end;
      { Handled should only be true when text is drawn by user. }
      Writer.ResetCanvas(nil);
      Writer.SetLabelFont(lkValue);
    end
    else
    begin
      YOk := (LabelPos.Y >= GrRect.Top) or (SPT > 0);
      if YOk then
        YOk := (LabelPos.Y <= GrRect.Bottom) or (SPB > 0);
      XOk := (LabelPos.X >= GrRect.Left) or (SPL > 0);
      if XOk then
        XOk := (LabelPos.X + Writer.GetTextWidth(lkValue, S) <= GrRect.Right) or (SPR > 0);
      if YOk and XOk then
      begin
        Canvas.TextOut(LabelPos.X, LabelPos.Y, S);
        LastVal := S;
      end;
    end;
  end;

  procedure DrawLine;
  var
    GPen: TGPPen;
  begin
    if (Chart.GraphBorders = gbAxis) or (Chart.GraphBorders = gbAllSides) then
    begin
      {Draws the hooks}
      if not Writer.HasWall then
      begin
        Canvas.MoveTo(PointLnStart.X, PointLnStart.Y);
        Canvas.LineTo(PointLnEnd.X, PointLnEnd.Y);
      end
      else
      begin
        GPen := TGPPen.Create(MakeGPClr(V.Pen.Color));
        FGDIP.DrawLine(GPen, Line3DStart.X, Line3DStart.Y, Line3DEnd.X,
          Line3DEnd.Y);
        GPen.Free;
      end;
      if V.ShowDividerLines then
      begin
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

  function MeasureSpacing : integer;
  var
    E: single;
    FirstX, FirstY : integer;
    i : integer;
    Cnt : integer;
  begin
    i := 0;
    Cnt := 0;
    E := LowVal;
    FirstX := 0;
    FirstY := 0;
    while Cnt = 0 do
    begin
      if (i mod Freq <> 0)then
      begin
        inc(i);
        E := E + Interval;
        Continue;
      end;
      if i = 0 then
      begin
        Q:= GetQualifier;
        if Q <> '' then
        begin
          if Writer.GetTextWidth(lk, Q) <=  Writer.GetTextWidth(lk) then
          begin
           E := E + Interval;
           inc(i);
           Continue;
          end;
        end;
      end
      else
       Inc(Cnt);
      S := FormatNum(E, Prec, True);
      if IsXAxis then
      begin
        X := GetExactValuePos(E);
        GetAxisHookPos(X, Y, PointLnStart, PointLnEnd);
        if Writer.HasWall then
          X := X - Chart.WallWidth;
      end
      else
      begin
        Y := GetExactValuePos(E);
        GetAxisHookPos(X, Y, PointLnStart, PointLnEnd);
        if Writer.HasWall then
        begin
          Y := Y + GetWallOffset;
        end;
      end;
      LabelPos := GetLabelCenterPos(S, X, Y);
      if i = 0 then
      begin
        FirstX := LabelPos.X;
        FirstY := LabelPos.Y;
      end;
      inc(i);
    end;
    if IsXAxis then
    begin
      Result := LabelPos.X-FirstX;
    end
    else
    begin
      Result := FirstY-LabelPos.Y;
    end;
  end;

begin
  if ClassType = TValueAxis2 then
  begin
    V := Chart.ValueScale2;
    Writer.FActiveValAx := Writer.FValueAxis2;
    lk := LkValue2;
  end
  else
  begin
    V := Chart.ValueScale1;
    Writer.FActiveValAx := Writer.FValueAxis;
    lk := lkValue;
  end;
  if not V.ShowLabels then
    Exit;
  if Writer.VisibleCount = 0 then
    Exit;
  Freq := LabelFreq;
  Canvas.Pen.Assign(V.Pen);
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
  Writer.SetLabelFont(lk);
  SPT := Writer.SpaceOf(apTop);
  SPL := Writer.SpaceOf(apLeft);
  SPR := Writer.SpaceOf(apRight);
  SPB := Writer.SpaceOf(apBottom);
  LastVal := '';
  try
    Prec := V.ValuePrecision;
    if V.FMinLabelSpacing> 5 then
    {Measure distance between labels. Increase if ditance < if MinLabelspacing}
    begin
      Spacing := MeasureSpacing;
      while Spacing < V.FMinLabelSpacing do
      begin
         inc(Freq);
         Spacing := MeasureSpacing;
      end;
    end;
    while (E <= HighVal) do
    begin
      if i = 0 then
      begin
        Q:= GetQualifier;
        if Q <> '' then
        begin
          if Writer.GetTextWidth(lk, Q) <=  Writer.GetTextWidth(lk) then
          begin
           E := E + Interval;
           inc(i);
           Continue;
          end;
        end;
      end;

      if i mod Freq <> 0 then
      begin
        E := E + Interval;
        inc(i);
        Continue;
      end;

      S := FormatNum(E, Prec, True);
      if IsXAxis then
      begin
        X := GetExactValuePos(E);
        GetAxisHookPos(X, Y, PointLnStart, PointLnEnd);
        if Writer.HasWall then
          X := X - Writer.Chart.WallWidth;
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
          Line3DEnd.X := PointLnStart.X + Writer.Chart.WallWidth + c_HookSize;
          Line3DEnd.Y := Line3DStart.Y - GetWallOffset;
        end;
      end;
      try
 //       if i mod LabelFreq = 0 then
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
    Writer.ResetCanvas(nil);
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
  Result := Chart.ValueScale2.ValueCount;
end;

function TValueAxis2.GetHigh: single;
begin
  Result := Chart.ValueScale2.ValueHigh;
end;

function TValueAxis2.GetLow: single;
begin
  Result := Chart.ValueScale2.ValueLow;
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
var
  W, H: integer;
begin
  if not Chart.NameScale.ShowLabels then
  begin
    Result := 0;
    Exit;
  end;
  Result := Writer.FNameLabelSpace; ///
  if Writer.Count = 0 then
    Exit;
   if IsXAxis then
  begin
    H := Writer.GetTextHeight(lkName);
    if H <> 0 then
    begin
      Result := Writer.GetTextHeight(lkName) + c_HookSpace;
    end;
  end
  else
  begin
    W := Writer.GetTextWidth(lkName);
    if W <> 0 then
    begin
      Result := W + c_HookSize  + c_HookMargin;
    end;
  end;
end;

function TNameAxis.GetSectionSpace: integer;
var
  Marg : integer;
begin
  Result := 0;
  Marg := 0;
  if Chart.NameSectionDefs = nil then
    Result := 0
  else if not Chart.NameSectionDefs.Visible then
    Result := 0
  else
  begin
    if Writer.FNameSections.Count = 0 then
      Exit;
    if IsXAxis then
    begin
      if Chart.NameSectionDefs <> nil then
        Marg := Chart.NameSectionDefs.FCaptionVertMargin;
      Result := Writer.GetTextHeight(lkNameSection) + Marg;
    end
    else
    begin
      if Chart.NameSectionDefs <> nil then
        Marg := Chart.NameSectionDefs.FCaptionVertMargin;
      Result := Writer.GetTextWidth(lkNameSection) + Marg + c_LabelXMarg;
    end;
  end;
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

function TNameAxis.GetQualifier : string;
begin
    Result := Chart.NameScale.Qualifier;
end;

function TNameAxis.GetQualifierSpace : integer;
var
  QW : integer;
  VS : string;
begin
  Result := 0;
  VS := Chart.NameScale.Qualifier;
  if Trim(VS) = '' then
    Exit;
  if not IsXAxis then {Draw vert}
  begin
    QW := Writer.GetTextHeight(lkName);
    if QW > 0 then
      Result := QW + c_QualifierMargin;
  end
  else
  begin
    QW := Writer.GetTextHeight(lkName);
    if QW > 0 then
      Result := QW+ c_QualifierMargin;
  end;
end;

function TNameAxis.GetLabelCenterPos(ALabel: string; X, Y: integer): TPoint;
begin
  Result := Writer.ComputeTextPos(X, Y, ALabel, Self);///
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
    Result := GrRect.Top +
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
  sl: TStringList;
  LblY: integer;
  Handled: Boolean;
  Messy: Boolean;
  LastPenPos: integer;
  IsReminder: Boolean;
  Line3DStart, Line3DEnd: TPoint;
  GPen: TGPPen;
  HasImages : Boolean;
  Freq : integer;
  Spacing : integer;
  Ch : TCWChart;

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

  function MeasureSpacing : integer;
  var
    FirstX, FirstY : integer;
    i : integer;
    Cnt : integer;
    Sz : integer;
  begin
    i := 0;
    Cnt := 0;
    FirstX := 0;
    FirstY := 0;
    while Cnt = 0 do
    begin
      if (i mod Freq <> 0)then
      begin
        inc(i);
        Continue;
      end;
      if i > 0 then
        inc(Cnt);

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
          X := X - Writer.Chart.WallWidth;
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
      if i = 0 then
      begin
        FirstX := X;
        FirstY := Y;
      end;
      inc(i);
    end;
    if IsXAxis then
    begin
      Sz := Writer.GetTextWidth(lkName) div 2;
      Result := (X-Sz)-(FirstX+Sz);
    end
    else
    begin
      Sz := Writer.GetTextHeight(lkName) div 2;
      Result := (Y-Sz)-(FirstY+Sz);
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
  if not Writer.Chart.NameScale.ShowLabels then
    Exit;
   if Writer.VisibleCount = 0 then
     Exit;
  R := GrRect;
  X := LabelRect.Left;
  Y := LabelRect.Top;
  sl := TStringList.Create;
  HasImages := (Writer.FImageSize.cx > 0) and Writer.Chart.NameScale.AllowImages;
  Writer.SetLabelFont(lkName);
  GetGDI;
  try
    LastPenPos := -1;
    Freq := Writer.NameLabelFreq;
    Ch := Chart;
    if not (Ch is TCWCategoryChart)
    and (Ch.NameScale.FMinLabelSpacing > 5) then
    begin
      {Measure distance between labels. Increase if ditance < if MinLabelspacing}
      Spacing := MeasureSpacing;
      while Spacing < Ch.NameScale.FMinLabelSpacing do
      begin
        inc(Freq);
        Spacing := MeasureSpacing;
      end;
    end;
    for i := 0 to UnitCount - 1 do
    begin
      if (i mod Freq <> 0)then
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
          X := X - Writer.Chart.WallWidth;
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
          if Writer.IsTimeSpan then
            S := GetTimeStr(Writer, Chart.TimeFormat, StrToDateTime(S, Fmt));
          LabelPos := GetLabelCenterPos(S, X, Y);
          if IsXAxis and ((Canvas.Font.Orientation = 0) or HasImages) then
            LabelPos.X := LabelPos.X - Writer.GetTextWidth(lkName, S) div 2;

          if (i > 0) and (LastPenPos <> -1) and (Chart.NameScale.Font.Orientation = 0)
          then
          begin
            if Writer.FNameAxis.IsXAxis then
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
            if Writer.IsTimeSpan then
              S := GetTimeStr(Writer, Chart.TimeFormat, StrToDateTime(S, Fmt))
            else if (Chart.GetNameType = ntNumberSpan) and
              (Chart.NameScale.NumSpanPrecision > 0) then
            begin
              S := Writer.GetNumPrecisionString(S, Writer.Chart.NameScale.NumSpanPrecision);
            end;
            LabelPos := GetLabelCenterPos(S, X, Y);
            if IsXAxis then
            begin
              if not Chart.NameScale.ClipLabels then
              begin
               if LabelPos.X < Writer.ClientRect.Left then
                Continue;
               if LabelPos.X + Writer.GetTextWidth(lkName, S) > Writer.ClientRect.Right then
                Continue;
              end;
            end
            else
            begin
             if LBlY < Writer.ClientRect.Top then
               Continue;
            end;
            if Assigned(Writer.FOnDrawLabel) then
            begin
              Handled := False;
              Writer.FOnDrawLabel(Writer, stNameScale, S,
                GetItem(i).RealDate, LabelPos, Canvas, Handled);
              if not Handled then
              begin
                Canvas.TextOut(LabelPos.X, LblY, S);
                LastPenPos := Canvas.PenPos.X;
              end
              else
                LastPenPos := -1;
              { Handled should only be true when text is drawn by user. }
              Writer.ResetCanvas(nil);
              Writer.SetLabelFont(lkName);
            end
            else
            begin
              if HasImages then
              begin
                 Chart.Categories.Items[i].Image.Bitmap.Transparent := True;
                 Chart.Categories.Items[i].Image.Bitmap.TransparentMode := tmAuto;
                  Canvas.Draw(LabelPos.X, LblY, Writer.Chart.Categories.Items[i].Image.Bitmap);
                 LastPenPos := -1;
              end
              else
              begin
               Canvas.TextOut(LabelPos.X, LblY, S);
               if IsXAxis then
                LastPenPos := Canvas.PenPos.X
               else
                LastPenPos := Canvas.PenPos.Y;
               end;
            end;
            inc(LblY, Writer.GetTextHeight(lkName));
          end;
          if ((Writer.Chart.GraphBorders = gbAxis) or
            (Writer.GraphBorders = gbAllSides)) then
          { Hooks }
          begin
            if not Writer.HasWall then
            begin
              GPen := TGPPen.Create(MakeGPClr(Chart.Namescale.Pen.Color));
              TranslatePenStyle(Chart.Namescale.Pen, GPen);
              FGDIP.DrawLine(GPen, PointLnStart.X, PointLnStart.Y,
               PointLnEnd.X, PointLnEnd.Y);
              GPen.Free;
            end
            else
            begin
              GPen := TGPPen.Create(MakeGPClr(Chart.WallBorderColor));
              try
                FGDIP.DrawLine(GPen, Line3DStart.X, Line3DStart.Y, Line3DEnd.X,
                  Line3DEnd.Y);
              finally
                GPen.Free;
              end;

            end;
          end;
          if Chart.NameScale.ShowDividerLines and (i > 0) then
          begin
            GPen := TGPPen.Create(MakeGPClr(Chart.Namescale.Pen.Color));
            TranslatePenStyle(Chart.Namescale.Pen, GPen);
            if IsXAxis then
            begin
              if Writer.HasWall then
                FGDIP.DrawLine(GPen, PointLnStart.X, GrRect.Bottom,
                  PointLnStart.X, GrRect.Top)
              else
                FGDIP.DrawLine(GPen,PointLnStart.X, PointLnStart.Y + c_HookSize,
                  PointLnStart.X, GrRect.Top);
            end
            else
            begin
              FGDIP.DrawLine(GPen,PointLnStart.X + c_HookSize, PointLnStart.Y,
                  GrRect.Right, PointLnStart.Y);
            end;
            GPen.Free;
          end;
      finally
      end;
    end;
  finally
    FGDIP.Free;
    sl.Free;
  end;
  Writer.ResetCanvas(nil);
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

{TCWScale ----------------------------------------------------------}

constructor TCWScale.Create;
begin
  FFont := TFont.Create;
  FPen := TPen.Create;
  FShowLabels := True;
  FFont.OnChange := FontChange;
  FPen.OnChange := FontChange;
  FMinLabelSpacing := 5;
end;

destructor TCWScale.Destroy;
begin
  FFont.Free;
  FPen.Free;
  inherited;
end;

procedure TCWScale.Assign(Source : TPersistent);
begin
  if Source is TCWScale then
  begin
    FFont := TFont.Create;
    FFont.Assign(TCWScale(Source).Font);
    FPen := TPen.Create;
    FPen.Assign(TCWScale(Source).Pen);
    FQualifier := TCWScale(Source).Qualifier;
    FShowLabels := TCWNameScale(Source).ShowLabels;
    FShowDividerLines := TCWNameScale(Source).ShowDividerLines;
  end
  else
    inherited Assign(Source)
end;

function TCWScale.GetWriter : TChartWriter;
begin
  Result := FOwner.Writer;
end;

procedure TCWScale.FontChange(Sender : TObject);
var
  E : TNotifyEvent;
begin
 E := nil;
 if Fowner.IsActive and not(csLoading in FOwner.componentState) then
 begin
    if Sender is TPen then
    with Sender as TPen do
    begin
      E := OnChange;
      OnChange := nil;
    end
    else if Sender is TFont then
    with Sender as TFont do
    begin
      E := OnChange;
      OnChange := nil;
    end;
    Writer.RefreshChart;
    if Sender is TPen then
    with Sender as TPen do
    begin
      OnChange := E;
    end
    else if Sender is TFont then
    with Sender as TFont do
    begin
      OnChange := E;
    end;
 end;
end;

procedure TCWScale.SetQualifier(const Value : string);
var
  V : string;
begin
  V := Trim(Value);
  if V = FQualifier then
    Exit;
  FQualifier := V;
  if Fowner.IsActive and not(csLoading in FOwner.componentState) then
    Writer.RefreshChart;
end;

procedure TCWScale.SetMinLabelSpacing(const Value : integer);
begin
  if FMinLabelSpacing = Value then
    Exit;
  if Value < c_LabelXMarg then
    Exit;
  if Value > 50 then
    Exit;
  FMinLabelSpacing := Value;
  if csLoading in FOwner.ComponentState then
    Exit;
  if FOwner.IsActive then
    Writer.RefreshChart;
end;


procedure TCWScale.SetShowLabels(const Value: Boolean);
begin
  if Value = FShowLabels then
    Exit;
  FShowLabels := Value;
  if Fowner.IsActive and not (csLoading in FOwner.ComponentState) then
  begin
   Writer.RestrictToClient(False);
   Writer.SetLabelFreqs;
   Writer.DoRepaint;
  end;
end;

procedure TCWScale.SetShowDividerLines(const Value: Boolean);
begin
  if Value = FShowDividerLines then
    Exit;
  FShowDividerLines := Value;
  if Fowner.IsActive and not (csLoading in FOwner.ComponentState) then
  begin
   Writer.RestrictToClient(False);
   Writer.SetLabelFreqs;
   Writer.DoRepaint;
  end;
end;

{ TCWNameScale -----------------------------------------------------------}

constructor TCWNameScale.Create;
begin
  FOverflowAction := ovNone;
  FAllowImages := True;
  inherited;
end;

procedure TCWNameScale.Assign(Source : TPersistent);
begin
  if Source is TCWNameScale then
  begin
    FNumSpanPrecision := TCWNameScale(Source).NumSpanPrecision;
    FOverflowAction := TCWNameScale(Source).OverflowAction;
    inherited Assign(Source);
  end
  else
    inherited Assign(Source)
end;

procedure TCWNameScale.SetNumSpanPrecision(const Value: integer);
begin
  if Value = FNumSpanPrecision then
    Exit;
  FNumSpanPrecision := Value;
  if Writer <> nil then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWNameScale.SetOverflowAction(const Value : TOverflowAction);
begin
  if Value = FOverflowAction then
    Exit;
  if Writer <> nil then
  begin
    if (Writer.ActiveGraph is TCWBar) and not (csLoading in FOwner.ComponentState) then
    begin
      if Writer.Count > 0 then
       if (TCWBar(Writer.ActiveGraph).Layout = blSideBySide) and (Value = ovCompression) then
        ShowGWError(msg_CompressionSideBySide);
    end;
  end;
  if not (csLoading in FOwner.ComponentState) then
  begin
    if (FOwner.GetNametype in [ntCategory, ntGeneral]) then
     if Value = ovContraction then
      ShowGWError(msg_OverflowAction, 'Contraction');
    if FOwner.GetNametype = ntCategory then
     if Value = ovCompression then
      ShowGWError(msg_OverflowAction, 'Compression');
  end;

  FOverflowAction := Value;
  if (Value = ovScrolling) and (Writer <> nil) then
    Writer.FScrollIndex := 0;
  if FOwner.IsActive then
  begin
      Writer.Reload;
  end;
end;

procedure TCWNameScale.SetAllowImages(const Value : Boolean);
begin
  if Value = FAllowImages then
    Exit;
  FAllowImages := Value;
  if csLoading in FOwner.ComponentState then
  begin
    Exit;
  end;
  if Writer <> nil then
    Writer.RefreshChart;
end;

function TCWNameScale.GetLabelRect : TRect;
begin
  Result := FOwner.Writer.FNameAxis.LabelRect;
end;

{ TCWValueScale-----------------------------------------------------------}

procedure TCWValueScale.Assign(Source : TPersistent);
begin
  if Source is TCWValueScale then
  begin
    FValueIntervals := TCWValueScale(Source).ValueIntervals;
    FValueHigh := TCWValueScale(Source).ValueHigh;
    FValueLow := TCWValueScale(Source).ValueLow;
    FValueSpanFromData := TCWValueScale(Source).ValueSpanFromData;
    FValuePrecision := TCWValueScale(Source).ValuePrecision;
    inherited Assign(Source);
  end
  else
    inherited Assign(Source)
end;

function TCWValueScale.GetValueFloatUnit: single;
begin
  if ValueCount = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if Writer.FValueAxis.IsXAxis then
  begin
    Result := Writer.GraphSpaceRect.Width / ValueCount;
    { Only called by RestrictToClinet}
  end
  else
  begin
    Result := Writer.GraphSpaceRect.Height / ValueCount;
  end;
end;

function TCWValueScale.GetValueCount: integer;
{ Counts the number og whole YUnits }
begin
  Result := floor((ValueHigh - ValueLow + 1) / FValueIntervals);
end;

procedure TCWValueScale.SetValueSpanFromData(const Value: Boolean);
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
        SetLabelFreqs;
        DoRepaint;
      end;
    end;
end;

procedure TCWValueScale.SetValueHigh(const Value: single);
var
  ErrNumber: single;
  Insp: Boolean;
  Ser : TSeries;
begin
  if (Value = FValueHigh) then
    Exit;
  if Value <= FValueLow then
    ShowGWError(msg_LowHigh);
  if not(csLoading in FOwner.componentState) and FOwner.IsActive then
    begin
      if ValueSpanFromData then
        Exit;
      Ser := GetCorrelationSeries;
      if Ser = nil then
        Insp := Writer.InSpan(ValueLow, Value, ErrNumber)
      else
        Insp := Writer.InSpan(Ser,ValueLow, Value, ErrNumber);

      if not Insp then
      begin
        ShowGWError(msg_NotInSpan, '(' + FormatNum(Value,
          ValuePrecision) + ')');
      end;
    end;
  FValueHigh := Value;
  if FOwner.IsActive then
    Writer.RefreshChart;
end;

procedure TCWValueScale.SetValueLow(const Value: single);
var
  ErrNumber: single;
  Insp: Boolean;
  Ser : TSeries;
begin
  if (Value = FValueLow) then
    Exit;
  if Value >= FValueHigh then
    ShowGWError(msg_LowHigh);
  if not(csLoading in FOwner.componentState) and FOwner.IsActive then
    begin
      if ValueSpanFromData then
        Exit;
      Ser := GetCorrelationSeries;
      if Ser = nil then
        Insp := Writer.InSpan(Value, ValueHigh, ErrNumber)
      else
        Insp := Writer.InSpan(Ser,Value,ValueHigh, ErrNumber);

      if not Insp then
      begin
        ShowGWError(msg_NotInSpan, '(' + FormatNum(Value, ValuePrecision) + ')');
      end;
    end;

  FValueLow := Value;
  if FOwner.IsActive then
    Writer.RefreshChart;
end;

procedure TCWValueScale.SetValueIntervals(const Value: single);
begin
  if Value = FValueIntervals then
    Exit;
  if (Value < 0.1) or (Value = FValueIntervals) then
    Exit;
  FValueIntervals := Value;
  FUserIntervals := Value;
  if FOwner.IsActive and not(csLoading in FOwner.componentState) and not Writer.InState(stExecuting) then
  begin
    Writer.ComputeTextExtent;
    Writer.RefreshChart;
  end;
end;

procedure TCWValueScale.SetValuePrecision(const Value: integer);
begin
  if Value = FValuePrecision then
    Exit;
  if (Value < 0) or (Value > 3) then
    Exit;
  FValuePrecision := Value;
  if FOwner.IsActive and not(csLoading in FOwner.componentState) then
  begin
    Writer.ComputeTextExtent;
    Writer.RefreshChart;
  end;
end;

procedure TCWValueScale.SetScaleRounding(const Value : Boolean);
begin
  if Value = FScaleRounding then
    Exit;
  FScaleRounding := Value;
  if FOwner.IsActive and not(csLoading in FOwner.componentState) then
  begin
    Writer.ComputeTextExtent;
    Writer.RefreshChart;
  end;
end;

function TCWValueScale.GetCorrelationSeries : TSeries;
var
  i : integer;
begin
  Result := nil;
  if FOwner.AxisCount = 2 then
  begin
    for I := 0 to FOwner.SeriesDefs.Count-1 do
      begin
        if Self = Fowner.ValueScale2 then
        begin
          if FOwner.SeriesDefs[i].FValueScale = vsValueScale2 then
          begin
            Result := FOwner.Writer.Series[i];
            Break;
          end
        end
        else
        begin
          if FOwner.SeriesDefs[i].FValueScale = vsValueScale1 then
          begin
            Result := FOwner.Writer.Series[i];
            Break;
          end
        end
      end;
  end;
end;

procedure TCWValueScale.SetHighLow;
var
  Ser : TSeries;
begin
  if not FOwner.IsActive then
    Exit;
  if Fowner.Writer.Count = 0 then
    Exit;
  Ser := GetCorrelationSeries;
  if Ser <> nil then
    Writer.GetValueSpan(Ser, FValueHigh, FValueLow)
  else
    Writer.GetValueSpan(FValueHigh, FValueLow);
  if ScaleRounding and ValueSpanFromData then
  begin
    FValueHigh := Ceil(FValueHigh);
    FValueLow := Floor(FValueLow);
  end;
end;

procedure TCWValueScale.SetValueSpan(LowValue, HighValue: single);
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
      RefreshChart;
    end;
end;

function TCWValueScale.GetLabelRect : TRect;
begin
  if Self = FOwner.ValueScale2 then
    Result := FOwner.Writer.FValueAxis2.LabelRect
  else
   Result := FOwner.Writer.FValueAxis.LabelRect;
end;

{ TCWChart ---------------------------------------------------------------}

constructor TCWChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCategories := TCWCategories.Create(Self, TCWCategory);
  FSeriesDefs := TCWSeriesDefs.Create(Self, TCWSeriesDef);
  FLegends := TCWLegends.Create(Self, TCWLegendItem);
  FValueScale1 := TCWValueScale.Create;
  FSpanType := ntDateSpan;
  FAnimationEnabled := True;
  with FValueScale1 do
  begin
    FValueHigh := 1;
    FValueLow := 0;
    FValueIntervals := 1;
    FUserIntervals := 1;
    FValuePrecision := 0;
    FOwner := Self;
  end;
  FValueScale2 := TCWValueScale.Create;
  with FValueScale2 do
  begin
    FValueHigh := 1;
    FValueLow := 0;
    FValueIntervals := 1;
    FUserIntervals := 1;
    FValuePrecision := 0;
    FOwner := Self;
  end;
  FInnerMargins := TCWMargins.Create;
  FGraphMargins := TCWMargins.Create;
  FOrigData := TData.Create;
  FNameScale := TCWNameScale.Create;
  FNameScale.FOwner := Self;
  FTitleFont := TFont.Create;
  FTitleFont.OnChange := FontChange;
  FTitleFont.Size := 12;
  FTitleFont.Color := clBlue;
  FTitleAlignment := taCenter;
  FSeriesRects := TLegendRects.Create;
  FPointRects := TLegendRects.Create;
  FSummaryRects := TLegendRects.Create;
  FWallColor := clWindow;
  FWallBorderColor := clBlack;
  FTextTilting := [];
  FTextTiltThreshold := 1;
  FGraphBGColor := clWindow;
  FTitleAlignment := taCenter;
  FZoomStart := -1;
  FZoomEnd := -1;
  FZoomLog := TZoomLog.Create;
end;

destructor TCWChart.Destroy;
begin
  FCategories.Free;
  FSeriesDefs.Free;
  FValueScale1.Free;
  FValueScale2.Free;
  FNameScale.Free;
  FOrigData.Free;
  FTitleFont.Free;
  FLegends.Free;
  FSeriesRects.Free;
  FPointRects.Free;
  FSummaryRects.Free;
  FZoomLog.Free;
  FInnerMargins.Free;
  FGraphMargins.Free;
  inherited;
end;

procedure TCWChart.Notification(AComponent: TComponent; Operation: TOperation);
var
  i : integer;
begin
  if (csDestroying in componentState) then
  begin
    inherited;
    Exit;
  end;

  if (Operation = opRemove) then
  begin
     if AComponent is TChartWriter then
     with AComponent as TChartWriter do
       ResetIDS;
     if (AComponent is TCWNameSectionDefs) then
      FNameSectionDefs := nil
     else if (AComponent is TCWValueSectionDefs) then
      FValueSectionDefs := nil
     else if AComponent is TCWGraph then
     begin
        for I := 0 to SeriesDefs.Count-1 do
          if SeriesDefs[i].FGraph = AComponent then
            SeriesDefs[i].FGraph := nil;
     end;
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
      if (FSeriesDefs.Items[i].ValueScale = vsValueScale1) and
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

procedure TCWChart.SetWallWidth(const Value: integer);
begin
  if ClassType = TCWPieChart then
    Exit;
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
      else if Writer.Chart.GraphBorders = gbNone then
        ShowGWMessage(msg_WallBorder);
    end;
  end;
  if IsActive then
    Writer.RefreshChart;
end;

procedure TCWChart.SetSpanType(const Value: TSpanType);
var
  NT : TSpanType;
  G :TCWGraph;
begin
  if csLoading in ComponentState then
  begin
    FSpanType := Value;
    Exit;
  end;
  if (ClassType = TCWPieChart) or (Self is TCWCategoryBarChart) or (ClassType = TCWGeneralChart) then
    Exit;
  if not (csDesigning in ComponentState) then
  begin
      if (Writer <> nil) and (Writer.Count > 0) then
        Exit;
  end;

  if Value = FSpanType then
    Exit;
  if IsActive and (Writer.Count > 0) then
  begin
     NT := Writer.DetectSpanType(0);
     if (NT <> Value) and not (Writer.ActiveGraph is TCWCurve) then
       ShowGWError(msg_SpanTypeData);
  end;

  if (Writer <> nil) and (SeriesDefs.Count > 0) then
  begin
   G := AllEqual;
   if (G <> nil) and not (G is TCWCurve) then
     ShowGWMessage(msg_ChangeSpanned);
  end;
  FSpanType := Value;
end;

procedure TCWChart.SetTitle(const Value: string);
var
 V: string;
begin
  if Value = FTitle then
    Exit;
  V := Trim(Value);
  if (csLoading in componentState) then
  begin
    FTitle := Value;
    Exit;
  end;
  FTitle := V;
  if IsActive then
  begin
    Writer.RestrictToClient(False);
    Writer.SetLabelFreqs;
    Writer.DoRepaint;
  end;
end;

function TCWChart.GetMouseTimeFormat : string;
begin
  if FMouseTimeFormat = '' then
  begin
     Result := TimeFormat
  end
  else
    Result := FMouseTimeFormat;
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
  if FWriter <> nil then
    Result := FWriter
  else
    Result := GetWriterFromWList(FWID);
end;

function TCWChart.GetNameType : TNameType;
begin
  if ClassType = TCWSpanChart then
    Result := FSpanType
  else if ClassType = TCWCategoryChart then
    Result := ntCategory
  else
    Result := ntGeneral;
end;

function TCWChart.GetFileName : TFileName;
begin
  Result := FFileName;
end;

function TCWChart.GetDataset : TDataset;
begin
  Result := FDataset;
end;

function TCWChart.GetNameDBField : string;
begin
  Result := FNameDBField;
end;

function TCWChart.GetValueDBFields : String;
begin
  Result := FValueDBFields;
end;

function TCWChart.GetPercentages : Boolean;
begin
  Result := FPercentages;
end;

function TCWChart.GetZoomed : Boolean;
begin
   Result := false;
   if IsActive then
     Result := Writer.InState(stZoomed);
end;

procedure TCWChart.FontChange(Sender : TObject);
begin
  if not IsActive then
    Exit;
  if (csLoading in ComponentState) or (Writer.FStates <> []) then
    Exit;
  if Sender is TFont then
  with Sender as TFont do
   OnChange := nil
  else with Sender as TPen do
   OnChange := nil;
  try
    Writer.RefreshChart;
  finally
    if Sender is TFont then
    with Sender as TFont do
      OnChange := FontChange
    else
    with Sender as TPen do
     OnChange := FontChange;
  end;

end;

procedure TCWChart.SetNameSectionDefs(const Value: TCWNameSectionDefs);
var
  FOld: TCWNameSectionDefs;
begin
  if Value = NameSectionDefs then
    Exit;
  if csLoading in ComponentState then
  begin
    FNameSectionDefs := Value;
    Value.FWID := AddToWList(Writer, Value);
    Value.FWriter := Writer;
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
    Value.FWriter := Writer;
  end;
  if IsActive and (Value <> nil)
  and not(csLoading in componentState) then
  begin
    Writer.CreateNameSections;
    Writer.RefreshChart;
  end
  else if IsActive and (Value = nil)
  and not(csLoading in componentState) then
     Writer.RefreshChart;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCWChart.SetValueSectionDefs(const Value: TCWValueSectionDefs);
begin
  if Value = ValueSectionDefs then
    Exit;

  if csLoading in ComponentState then
  begin
    FValueSectionDefs := Value;
    Value.FWID := AddToWList(Writer, Value);
    Value.FWriter := Writer;
    Exit;
  end;
  FValueSectionDefs := Value;
  if Value <> nil then
  begin
    Value.FWID := AddToWList(Writer,  Value);
    Value.FWriter := Writer;
  end;

  if IsActive and (Value <> nil)
  and not(csLoading in componentState)
  and (AxisCount = 1) then
  begin
    Writer.CreateValueSections;
    Writer.RefreshChart;
  end
  else if IsActive and (Value = nil)
  and not(csLoading in componentState) then
    Writer.RefreshChart;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCWChart.SetWallBorderColor(const Value: TColor);
begin
  if Value = FWallBorderColor then
    Exit;
  FWallBorderColor := Value;
  if IsActive then
  begin
      Writer.DoRepaint;
  end;
end;

procedure TCWChart.SetGradientWall(const Value: Boolean);
begin
  if Value = FGradientWall then
    Exit;
  FGradientWall := Value;
  if IsActive then
   Writer.DoRepaint;
end;

procedure TCWChart.SetWallColor(const Value: TColor);
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
   Writer.Canvas.Font.Assign(TitleFont);
   Y := Writer.WorkRect.Top + 5;
   X := 0;
   case TitleAlignment of
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

 function GetRect(Ax : TCWValueScale) : TRect;
 var
   AxObj : TAxisobject;
 begin
     if Ax= nil then
       AxObj := Writer.FNameAxis
     else if Ax = ValueScale1 then
      AxObj := Writer.FValueAxis
     else
      AxObj := Writer.FValueAxis2;

     Result := AxObj.GetQualifierRect;
 end;

 procedure DrawValueQualifiers;
 begin
    S1 := ValueScale1.FQualifier;
    S2 := ValueScale2.FQualifier;

    if (S1 = '') and (S2 = '') then
     Exit;
    Orient := Writer.Canvas.Font.Orientation;
    if S1 <> '' then
    begin
      Writer.Canvas.Font.Assign(ValueScale1.Font);
      H := Writer.GetTextHeight(lkValue);
      W := Writer.GetTextWidth(lkValue, S1);
      R := GetRect(ValueScale1);
      begin
         if not Writer.FValueAxis.IsXAxis then
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
      Writer.Canvas.Font.Assign(ValueScale2.Font);
      H := Writer.GetTextHeight(lkValue2);
      W := Writer.GetTextWidth(lkValue2, S2);
      R := GetRect(ValueScale2);
      begin
         if not Writer.FValueAxis2.IsXAxis then
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

 procedure DrawNameQualifier;
 begin
    if NameScale.Qualifier = '' then
     Exit;
    Writer.Canvas.Font.Assign(NameScale.Font);
    S1 := NameScale.Qualifier;
    Orient := NameScale.Font.Orientation;
    NameScale.Font.Orientation :=0;
    Writer.Canvas.Font.Orientation := 0;
    H := Writer.GetTextHeight(lkName);
    W := Writer.GetTextWidth(lkName, S1);
    R := GetRect(nil);
    if Writer.FValueAxis.IsXAxis then
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
     NameScale.Font.Orientation := 0;
     Writer.Canvas.Font.Orientation := Orient;
  end;

begin
   Writer.Canvas.Brush.Style := bsClear;
   DrawValueQualifiers;
   DrawNameQualifier;
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

function TCWChart.ValAx2Graph : TCWGraph;
var
  i : integer;
begin
  Result := nil;
  for I := 0 to FseriesDefs.Count-1 do
    if FseriesDefs.Items[i].ValueScale = vsValueScale2 then
    begin
      Result := FseriesDefs.Items[i].Graph;
    end;
end;

function TCWChart.ColorUsage : TColorUsage;
begin
   Result := cuOnSeries;
   if (ClassType = TCWCategoryBarChart) or (ClassType = TCWPieChart) then
   begin
     if Categories.Count > 0 then
       Result := cuOnItems;
   end;
end;

procedure TCWChart.SetAxisOrientation(const Value: TAxisOrientation);
begin
  if Value = FAxisOrientation then
    Exit;
  if csLoading in ComponentState then
  begin
     FAxisOrientation := Value;
     Exit;
  end;
  if Writer <> nil then
  begin
    if Writer.InView(TCWCurve) <> nil then
    begin
      case Value of
        alLeftTop, alRightTop, alLeftBottom, alRightBottom:
        begin
          ShowGWMessage(msg_HorizontalCurves);
          Exit;
        end;
      end;
    end;
  end;
  FAxisOrientation := Value;
  if Writer <> nil then
    Writer.SetPosition(Value);
  if not IsActive then
    Exit;
  Writer.FDynaSectStart := -1;
  Writer.FDynaSectEnd := -1;
  Writer.FViewMode := vmNormal;
  Writer.RefreshChart;
end;

procedure TCWChart.ClearSeriesDefs;
begin
    FSeriesDefs.Clear;
end;

procedure TCWChart.SetSeriesDef(Index : integer; ATitle : string; AGraph : TCWGraph; AColor : TColor; AValueAxis : TValueScaleNumber);
var
  SD: TCWSeriesDef;
begin
   SD := FSeriesDefs.Items[Index];
   SD.FGraph := AGraph;
   SD.FColor := AColor;
   SD.FValueScale := AValueAxis;
   SD.FTitle := ATitle;
end;

procedure TCWChart.AddSeriesDef(ATitle : string; AGraph : TCWGraph; AColor : TColor; AValueAxis : TValueScaleNumber);
var
  SD : TCWSeriesDef;
begin
   if AGraph = nil then
     Exit;
   if (ClassType = TCWSpanChart) and (AGraph is TCWPie) then
     ShowGWError(msg_PieGeneral);
   SD := FSeriesDefs.Add;
   SD.FGraph := AGraph;
   SD.FColor := AColor;
   SD.FValueScale := AValueAxis;
   SD.FTitle := ATitle;
end;

procedure TCWChart.AddSeriesDef(AGraph : TCWGraph);
begin
  AddSeriesDef('', AGraph, clBlack, vsValueScale1);
end;

function TCWChart.AddLegend : TCWLegend;
var
  L : TCWLegendItem;
begin
     Result := TCWLegend.Create(Owner);
     L := Legends.Add;
     L.Legend := Result;
end;

procedure TCWChart.DeleteLegend(Index : integer);
begin
  Legends.Delete(Index);
end;

procedure TCWChart.ReplaceGraphs(NewGraph : TCWGraph);
var
  i : integer;
  OldAlt : TCWGraph;
  OldOrient : TAxisOrientation;
begin
  if NewGraph = FSeriesDefs[0].FGraph then
    Exit;
  OldAlt := FSeriesDefs[0].FGraph;
  OldOrient := AxisOrientation;
  if NewGraph is TCWCurve then
    Writer.SetPosition(alBottomLeft);
    {To prevent SetGraph from protesting, in case vertical view}
  try
    for I := 0 to FSeriesDefs.Count-1 do
    begin
      FSeriesDefs.Items[i].Graph := NewGraph;
      if ClassType = TCWCategoryBarChart then
      begin
          FSeriesDefs.Items[i].FValueScale := vsValueScale1
      end
      else if ClassType = TCWPieChart then
      begin
          FSeriesDefs.Items[i].FValueScale := vsNone;
      end;
    end;
  finally
     Writer.SetPosition(OldOrient);
  end;

  if (FAlternativeGraph <> nil) and not (csDesigning in ComponentState) then
  begin
  {The alternative cannot be reached in design mode}
    FAlternativeGraph := OldAlt;
    if NewGraph is TCWAxisGraph then
    begin
      FAxisOrientation := FAlternativeOrient;
      Writer.SetPosition(FAlternativeOrient);
      FAlternativeOrient := OldOrient;
    end;
  end;

end;

function TCWChart.AllEqual : TCWGraph;
var
  i : integer;
  VT : TValueScaleNumber;
begin
  Result := nil;
  if FSeriesDefs.Count = 0 then
    Exit;
  Result := FSeriesDefs.Items[0].Graph;
  if Result = nil then
    Exit;
  VT := FSeriesDefs.Items[0].ValueScale;
  Result := FSeriesDefs.Items[0].Graph;
  for I := 1 to SeriesDefs.Count-1 do
  begin
    if (VT <> FSeriesDefs.Items[i].ValueScale)
    or (Result <> FSeriesDefs.Items[i].Graph) then
    begin
      Result := nil;
      Break;
    end;
  end;
end;

procedure TCWChart.GetUniqueGraphs(var Graph1, Graph2 : TCWGraph);
var
  i : integer;
begin
  Graph1 := nil;
  Graph2 := nil;
  for I := 0 to SeriesDefs.Count-1 do
    begin
       if Graph1 = nil then
         Graph1 := SeriesDefs[i].Graph
       else
       if (Graph1 <> nil) and (SeriesDefs[i].Graph <> Graph1) then
       begin
         Graph2 := SeriesDefs[i].Graph;
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

function TCWChart.VisibleGraphCount(GraphType : TClass) : integer;
{Counts number of visible graphs of GraphType}
var
  i : integer;
begin
  Result := 0;
  for i := 0 to SeriesDefs.Count - 1 do
    if (SeriesDefs[i].Visible) and (SeriesDefs[i].Graph.ClassType = GraphType) then
      inc(Result);
end;


procedure TCWChart.SetTextTilting(const Value: TTextOrientations);
begin
  if Value = FTextTilting then
    Exit;
  FTextTilting := Value;
  if not(csLoading in componentState) and IsActive then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWChart.SetTextTiltThreshold(const Value: integer);
begin
  if Value = FTextTiltThreshold then
    Exit;
  if (Value < 1) or (Value > 100) then
    Exit;
  FTextTiltThreshold := Value;
  if not(csLoading in componentState) and IsActive then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWChart.SetFileName(const Value: TFileName);
var
  V : TFileName;
begin
  V := Trim(Value);
  if SameText(V, FFileName) then
    Exit;
  FFileName := V;
  if FFileName <> '' then
    FDataset := nil;
  if (csDesigning in ComponentState) and IsActive and Writer.LiveGraphs
  and not (csLoading in ComponentState) then
  begin
    if (FFileName = '') then
    begin
      Writer.RenderDesigner;
      ClearCache;
      SaveCache;
      Exit;
    end;
    if not FileExists(FileName) then
      ShowGWError(-1,'File ' + FileName + ' not found');
    if Writer.GetFileType(FileName) <> 1 then
      ShowGWError(msg_RichDesign);
    Writer.LoadFromFile(FileName);
  end;
end;

procedure TCWChart.SetDataset(const Value: TDataset);
begin
  if Value = FDataset then
    Exit;
  FDataset := Value;
  if Value <> nil then
    FFilename := '';
  if (csDesigning in ComponentState) and IsActive and Writer.LiveGraphs then
  begin
    if (FNameDBField <> '') and (FValueDBFields <> '') and (FDataSet <> nil) then
     Writer.LoadFromDataBase(FDataSet, FNameDBField, FValueDBFields)
    else
    begin
      Writer.RenderDesigner;
      ClearCache;
      SaveCache;
    end;
  end;
end;

procedure TCWChart.SetValueDBFields(const Value: string);
begin
   if Value = FValueDBFields then
     Exit;
   FValueDBFields := Value;
   if (csDesigning in ComponentState) and IsActive and Writer.LiveGraphs then
   begin
    if (FNameDBField <> '') and (FValueDBFields <> '') and (FDataSet <> nil) then
     Writer.LoadFromDataBase(FDataSet, FNameDBField, FValueDBFields)
    else
      Writer.RenderDesigner;
  end;
end;

procedure TCWChart.SetNameDBField(const Value: string);
begin
   if Value = FNameDBField then
     Exit;
   FNameDBField := Value;
   if (csDesigning in ComponentState) and IsActive and Writer.LiveGraphs then
   begin
    if (FNameDBField <> '') and (FValueDBFields <> '') and (FDataSet <> nil) then
     Writer.LoadFromDataBase(FDataSet, FNameDBField, FValueDBFields)
    else
      Writer.RenderDesigner;
  end;
end;

procedure TCWChart.SetTitleAlignment(const Value: TAlignment);
begin
   if Value = FTitleAlignment then
     Exit;
   FTitleAlignment := Value;
   if IsActive then
     Writer.DoRepaint;
end;

procedure TCWChart.SetTimeFormat(const Value: string);
begin
if Trim(Value) = FTimeFormat then
    Exit;
  FTimeFormat := Trim(Value);
  if not(csLoading in componentState)
  then if IsActive then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWChart.SetGraphBGColor(const Value: TColor);
begin
  if Value = FGraphBGColor then
    Exit;
  FGraphBGColor := Value;
  if IsActive then
    Writer.DoRepaint;
end;

procedure TCWChart.SetGraphBorders(const Value: TGraphBorders);
begin
  if Value = FGraphBorders then
    Exit;
  FGraphBorders := Value;
  if IsActive then
    Writer.DoRepaint;
end;

procedure TCWChart.SetAlternativeGraph(const Value: TCWGraph);
var
 G1, G2 : TCWGraph;
begin
  if Value = FAlternativeGraph then
    Exit;
   if (csLoading in ComponentState) then
   begin
     FAlternativeGraph := Value;
     Exit;
   end;
  if (ClassType = TCWSpanChart) and (Value is TCWPie) then
    ShowGWError(msg_PieSpanAlternative);
  if AxisCount > 1 then
    ShowGWError(msg_DoubleAxisAlternative);
  GetuniqueGraphs(G1, G2);
  if (G1 <> nil) and (G2 <> nil) then
    ShowGWError(msg_DifferentGrapsAlternative);
  if Value = G1 then
    ShowGWError(msg_AlternativeSelf);
  FAlternativeGraph := Value;
end;

procedure TCWChart.SaveCache;
var
  i, j : integer;
  S : string;
  Prec : integer;
begin
   if IsCached then
     Exit;
   for I := 0 to Writer.Count-1 do
   begin
     for j := Writer.FSeriesData[i].FirstItem to Writer.FSeriesData[i].LastItem do
     begin
       Prec := Writer.ValuePrecision;
       S := Writer.FSeriesData[i].FSeriesItems[j].Name + '=' +
       FormatNum(Writer.FSeriesData[i].FSeriesItems[j].Value, Prec);
       SeriesDefs[i].FDataCache.Add(S);
     end;

   end;
end;

procedure TCWChart.ClearCache;
var
  i : integer;
begin
   if not IsCached then
     Exit;
   for I := 0 to Writer.Count-1 do
   begin
       SeriesDefs[i].FDataCache.Clear;
   end;
end;

function TCWChart.IsCached : Boolean;
begin
   Result := (SeriesDefs.Count > 0) and (SeriesDefs[0].FDataCache.Count > 0);
end;

function TCWChart.CanAnimate : integer;
begin
  Result := AN_OK;
  if csDesigning in ComponentState then
    Result := -1
  else if not IsActive then
    Result := AN_NoActiveGraph
  else if AllEqual = nil then
    Result := AN_DifferentGraphs
  else if Writer.InState(stLimbo) then
    Result := AN_NoSpace
  else if VisibleCount = 0 then
    Result := AN_NoActiveGraph
  else if
   ((Writer.ActiveGraph is TCWBar) and (TCWBar(Writer.ActiveGraph).Animations = []))
   or ((Writer.ActiveGraph is TCWPie) and not TCWPie(Writer.ActiveGraph).Animation)
   or ((Writer.ActiveGraph is TCWCurve) and not TCWCurve(Writer.ActiveGraph).Animation) then
     Result := AN_AnimationNotDefined;
end;

function TCWChart.PerformAnimation : integer;
begin
  Result := CanAnimate;
  if Result <> AN_OK then
    Exit;
  FAnimationEnabled := True;
  FHasAnimated := false;
  Writer.RefreshChart;
end;

procedure TCWChart.Reload;
var
  MSG : TMessage;
begin
  if not IsActive then
    Exit;
  ClearCache;
  Writer.WMLoadFile(Msg);
end;

{ TCWGraph ------------------------------------------------------------- }

constructor TCWGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FAnimationSpeed := asMediumFast;
  FreeNotification(AOwner);
end;

destructor TCWGraph.Destroy;
begin
  inherited;
  FFont.Free;
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
  if not(ClassType = TCWCurve) then
  begin
    FGDIP := TGPGraphics.Create(Canvas.Handle);
    FGDIP.SetSmoothingMode(SmoothingModeAntiAlias);
    FGDIP.SetInterpolationMode(InterpolationMode.InterpolationModeHighQuality);
    FGDIP.SetCompositingQuality
      (CompositingQuality.CompositingQualityHighQuality);
  end;
  if Writer.Chart.ValAx2Graph = self then
      Writer.FActiveValAx := Writer.FValueAxis2
    else
      Writer.FActiveValAx := Writer.FValueAxis;
  Canvas.Font.Assign(Font);
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
  if (Operation = opRemove)
  and ((AComponent is TChartWriter) or (AComponent is TCWChart)) then
  begin
    FWriter := nil;
  end;
  inherited;
end;

function TCWGraph.GetSeries(Value: integer) : TSeries;
begin
  Result := nil;
  if (Writer <> nil) and (Writer.Chart <> nil) then
    Result := Writer.Series[Value];
end;

function TCWGraph.GetInternalActiveColor(SeriesIndex, ItemIndex : integer): TColor;
begin
  Result := clBlack;
end;

procedure TCWGraph.SetInternalActiveColor(SeriesIndex, ItemIndex : integer; Value : TColor);
begin
  //
end;

function TCWGraph.GetActiveColor : TColor;
begin
   Result := clBlack;
   if InChart and Writer.InState(stPainting) then
     Result := InternalActiveColor[FPaintSeriesIndex, FPaintItemIndex];
end;

procedure TCWGraph.SetActiveColor(const Value : TColor);
begin
   if InChart and Writer.InState(stPainting) then
    InternalActiveColor[FPaintSeriesIndex, FPaintItemIndex] := Value;
end;

procedure TCWGraph.SetAnimation(const Value : Boolean);
begin
  FAnimation := Value;
end;

function TCWGraph.GetWriter : TChartWriter;
begin
    if FWriter <> nil then
     Result := GetWriterFromWList(FWID)
    else
      Result := FWriter;
end;

function TCWGraph.GetChart : TCWChart;
begin
  Result := nil;
  if Writer <> nil then
    Result := Writer.Chart;
end;

procedure TCWGraph.FontChange(Sender : TObject);
begin
  if InChart then
    Writer.DoRepaint;
end;

procedure TCWGraph.SetKeepFontColor(const Value : Boolean);
begin
  if Value = FKeepFontColor then
    Exit;
  FKeepFontColor := Value;
  if csLoading in ComponentState then
    Exit;
  if InChart then
    Writer.DoRepaint;
end;


{ TCWAxisGraph ----------------------------------------------------- }

constructor TCWAxisGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
     if (D2.Graph = D.Graph) and (D2.ValueScale = D.ValueScale) and D2.Visible then
       Inc(Result);
  end;

end;

function TCWAxisGraph.QuerySpace(ItemCount : integer) : integer;
begin
   Result := 0;
   if Writer.Chart = nil then
    ShowGWError(msg_NoActiveChart);
   if Writer = nil then
    ShowGWError(msg_WriterNotAssigned);
   if (Writer.Count = 0) and (ItemCount = -1) then
    ShowGWError(msg_DataEmpty);
   if not InChart then
    ShowGWError(msg_GraphNotInChart);
end;

function TCWAxisGraph.GetValueScale : TCWValueScale;
var
  Ser : integer;
begin
  Result := nil;
  if InChart then
  begin
     Ser := Writer.Chart.SeriesDefs.IndexOf(Self);
     if Writer.Chart.SeriesDefs[Ser].FValueScale = vsValueScale1 then
       Result := Writer.Chart.ValueScale1
     else
       Result := Writer.Chart.ValueScale2;
  end;
end;

procedure TCWAxisGraph.SetAnimations(const Value: TAnimations);
begin
  FAnimations := Value;
end;

procedure TCWAxisGraph.SetAnimationBooster(const Value: integer);
begin
  if (Value < -5) or (Value > 5) then
    Exit;
  FAnimationBooster := Value;
end;

procedure TCWAxisGraph.SetMaxPointSpacing(const Value: integer);
begin
  if (Value = FMaxPointSpacing) or (Value < 0) or (Value > 100) then
    Exit;
  if ClassType = TCWBar then
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
        RefreshChart;
      end;
    end;
end;

procedure TCWAxisGraph.SetStatLine(const Value: TStatLine);
begin
  if Value = FStatLine then
    Exit;
  FStatLine := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWAxisGraph.SetSMAPeriods(const Value : integer);
begin
  if Value = FSMAPeriods then
    Exit;
  if Value < 0 then
    Exit;
  FSMAPeriods := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWAxisGraph.SetStatlineWidth(const Value : integer);
begin
  if Value = FStatLineWidth then
    Exit;
  if (Value < 0) or (Value > 3) then
    Exit;
  FStatLineWidth := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWAxisGraph.SetStatBackgroundBlending(const Value : integer);
begin
  if Value = FStatBackgroundBlending then
    Exit;
  FStatBackgroundBlending := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWAxisGraph.CalculateSMA(SeriesIndex : integer; period: Integer): TPointArray;
var
  i: Integer;
  ASeries : TSeries;
  Data : array of Single;
  Avg : TMovingAverage;

begin
  ASeries := Writer.FSeriesData[SeriesIndex];
  SetLength(Data, ASeries.ItemCount);

  Avg := TMovingAverage.Create(Period);
  for i := 0 to ASeries.ItemCount - 1 do
  begin
      Data[i] := Avg.Add(ASeries.FSeriesItems[i].Value);
  end;

  Result := TPointArray.Create;
  {Compute the point coordiantes}
  for I := 0 to ASeries.ItemCount-1 do
  begin
   // if i >= Period then
      Result.Add(ASeries.PosFromNamVal(ASeries.FSeriesItems[i].FName, Data[i]));
  end;
end;

procedure TCWAxisGraph.DrawStatLine(GDIP : TGPGraphics; ASource : TSeries);
var
  Points : TPointArray;
  GPen : TGPPen;
    procedure DrawLine(x1, y1, x2, y2: integer);
    begin
      GDIP.DrawLine(GPen, x1, y1, x2, y2);
    end;

    function GetTextRect(X, Y : integer; Txt : string) : TRect;
    begin
      Result.Left := X+1;
      Result.Top := Y+1;
      Result.Right := Result.Left + Canvas.TextWidth(Txt)+5;
      Result.Bottom := Result.Top + Canvas.TextHeight(Txt)+5;
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
      Rgn : HRGN;
      Rct : TRect;

    begin
      if not Writer.FNameAxis.IsXAxis then
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

      Canvas.Pen.Color := InternalActiveColor[ASource.Index, -1];
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
        RCT := Writer.GetGraphPrintRect;
        Rgn := CreateRectRgn(RCT.Left, RCT.Top, RCT.Right, RCT.Bottom);
        GDIP.SetClip(RGN);
        DrawLine(pStart.X, pStart.Y, PEnd.X, PEnd.Y);
        DeleteObject(Rgn);
      end;
    end;

    procedure DrawMeanLine;
    var
      MeanLine: single;
      P: integer;
      R: TRect;
      TR : TRect;
      S : string;
    begin
      R := Writer.GetGraphPrintRect;
      if not Writer.FNameAxis.IsXAxis then
        Exit;
      MeanLine := ASource.Avg;
      P := Writer.PosFromValue(MeanLine, Writer.Chart.SeriesDefs[ASource.Index].ActualAxis);
      begin
        S := FormatNum(MeanLine, 1);
        TR := GetTextRect(R.Left, P, S);
        DrawLine(R.Left, P, R.Right, P);
        Canvas.Rectangle(TR);
        Canvas.TextOut(TR.Left + 3, TR.Top + 3, S);
      end;
    end;

    procedure DrawMedianLine;
    var
      MedianLine: single;
      P: integer;
      R: TRect;
      TR : TRect;
      S : string;
    begin
      R := Writer.GetGraphPrintRect;
      if not Writer.FNameAxis.IsXAxis then
        Exit;
      MedianLine := ASource.Median;
      P := Writer.PosFromValue(MedianLine, Writer.Chart.SeriesDefs[ASource.Index].ActualAxis);
      begin
        S := FormatNum(MedianLine, 1);
        TR := GetTextRect(R.Left, P, S);
        DrawLine(R.Left, P, R.Right, P);
        Canvas.Rectangle(TR);
        Canvas.TextOut(TR.Left+3, TR.Top+3, S);
      end;
    end;

    procedure DrawModeLines;
    var
      Nums: TModeNumbers;
      i: integer;
      P: integer;
      R: TRect;
      TR : TRect;
      S : string;
    begin
      R := Writer.GetGraphPrintRect;
      Nums := ASource.Mode;
      for i := 0 to High(Nums) do
      begin
        P := Writer.PosFromValue(StrToFloat(Nums[i].Number, Fmt),
          Writer.Chart.SeriesDefs[ASource.Index].ActualAxis);
        begin
          DrawLine(R.Left, P, R.Right, P);
          S := Nums[i].Number + ' (' +  IntToStr(Nums[i].Cnt) + ')';
          TR := GetTextRect(R.Left, P, S);
          Canvas.Rectangle(TR);
          Canvas.TextOut(TR.Left + 3, TR.Top + 3, S);
        end;
      end;
    end;

    procedure DrawSMA;
    var
      Pts : TPointArray;
      i : integer;
    begin
       if SMAPeriods = 0 then
        FSMAPeriods := ASource.ItemCount
       else if SMAPeriods > ASource.ItemCount then
        FSMAPeriods := ASource.ItemCount;
       Pts := CalculateSMA(ASource.Index, SMAPeriods);
       try
         for I := 0 to Pts.Count-1 do
         begin
           if I > 0 then
             DrawLine(Pts[i-1].X, Pts[i-1].Y, Pts[i].X, Pts[i].Y);
         end;

       finally
         Pts.Free;
       end;
    end;
begin
    if Writer.IsAnimating then
      Exit;
    if not (Writer.Chart.ClassType = TCWSpanChart)
     and not (Writer.Chart.ClassType = TCWGeneralChart) then
      Exit;
    if Writer.InView(TCWBar) <> nil then
      Points := ASource.FBarPoints
    else
      Points := ASource.FPoints;
    GPen := TGPPen.Create(MakeGPClr(Writer.Chart.SeriesDefs[ASource.Index].Color));
    GPen.SetWidth(StatLineWidth);
    Canvas.Font.Assign(FFont);
    Canvas.Font.Color := clBlack;
    Canvas.Brush.Color := clCream;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    try
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
      end
      else if slSMA = StatLine then
      begin
        DrawSMA;
      end;
    finally
      GPen.Free;
      Canvas.Font.Assign(FFont);
    end;
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

procedure TCWSeriesStyle.SetStyle(const Value: TCurveStyle);
var
  Res : integer;
begin
  Res := TCWCurve(Collection.Owner).CanApplyStyle(Value, True);
  if Res = S_PointsWithOthers then
     ShowGWError(msg_MixPointsOthers)
  else if Res = S_NeighborWithOthers then
     ShowGWError(msg_MixNeighborOthers)
  else if Res = S_OthersWithNeighbor then
     ShowGWError(msg_MixThisNeighbor)
  else if Res = S_OthersWithPoints then
     ShowGWError(msg_MixThisPoints);
  FStyle := Value;
  if TCWCurve(Collection.Owner).Writer <> nil then
    TCWCurve(Collection.Owner).Writer.DoRepaint;
end;

procedure TCWSeriesStyle.SetLineStyle(const Value: TCurvelineStyle);
begin
  FLineStyle := Value;
  if TCWCurve(Collection.Owner).Writer <> nil then
    TCWCurve(Collection.Owner).Writer.DoRepaint;
end;

procedure TCWSeriesStyle.SetLineWidth(const Value: integer);
begin
  if Value < 1 then
    Exit;
  FLineWidth := Value;
  if TCWCurve(Collection.Owner).Writer <> nil then
    TCWCurve(Collection.Owner).Writer.DoRepaint;
end;

procedure TCWSeriesStyle.SetSeriesTitle(const Value: string);
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

function TCWSeriesStyles.IndexOfStyle(const AStyle: TCurveStyle): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if AStyle = Items[i].Style then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TCWSeriesStyles.ApplyOnAll(AStyle : TCurveStyle; ALineStyle : TCurveLineStyle;
 ALineWidth : integer);
begin
    Clear;
    with Owner as TCWCurve do
    begin
        FStyle := AStyle;
        FLineStyle := ALineStyle;
        FLineWidth := ALineWidth;
        if InChart then
          Writer.DoRepaint;
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

constructor TCWCategory.Create(Collection: TCollection);
begin
  inherited;
  FImage := TPicture.Create;
end;

destructor TCWCategory.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TCWCategory.SetColor(const Value: TColor);
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

function TCWCategory.GetColor: TColor;
begin
    Result := ColorToRgb(FColor);
end;

function TCWCategory.GetDisplayName: string;
begin
  if FCategoryName = '' then
    Result := 'Unnamed: ' + ColorToString(FColor)
  else
    Result := FCategoryName + ': ' + ColorToString(FColor);
end;

procedure TCWCategory.SetCategoryName(const Value: string);
var
  i, Indx: integer;
  C : TCWChart;
begin
  if Trim(Value) = '' then
  begin
    FCategoryName := '';
    Exit;
  end;
  for i := 0 to Collection.Count - 1 do
  begin
    if i <> Index then
    begin
      if SameText(Value, TCWCategories(Collection).Items[i].FCategoryName) then
        ShowGWError(msg_DupItemName, Value);
    end;
  end;
  FCategoryName := Value;
  C := TCWChart(Collection.Owner);
  if C.IsActive then
  begin
     { Sync with series items}
     for I := 0 to C.Writer.Count-1 do
     begin
        Indx := C.Writer.Series[i].IndexOfName(FCategoryName);
        if Indx <> -1 then
         C.Writer.Series[i].FSeriesItems[i].FName := FCategoryName;
     end;
     C.Writer.DoRepaint;
  end;
end;

procedure TCWCategory.SetImage(const Value : TPicture);
begin
  FImage.Assign(Value);
end;

procedure TCWCategory.Assign(Source: TPersistent);
var
  wSrc: TCWCategory;
begin
  if Source is TCWCategory then
  begin
    wSrc := TCWCategory(Source);
    FColor := wSrc.Color;
    FCategoryName := wSrc.FCategoryName;
  end
  else
    inherited;
end;

{ TCWColors------------------------------------------------------- }

function TCWCategories.GetItem(AIndex: integer): TCWCategory;
begin
  Result := TCWCategory(inherited Items[AIndex]);
end;

procedure TCWCategories.SetItem(AIndex: integer; const Value: TCWCategory);
begin
  inherited SetItem(AIndex, Value);
end;

procedure TCWCategories.Update(Item : TCollectionItem);
begin
  inherited;
end;

function TCWCategories.Add: TCWCategory;
begin
  Result := TCWCategory(inherited Add);
end;

procedure TCWCategories.Assign(Source: TPersistent);
var
  wSrc: TCWCategories;
  loop: integer;
begin
  if (Source is TCWCategories) then
  begin
    wSrc := TCWCategories(Source);
    Clear;
    for loop := 0 to wSrc.Count - 1 do
      Add.Assign(wSrc.Items[loop]);
  end
  else
    inherited;
end;

function TCWCategories.IndexOf(const CategoryName: string): integer;
var
  i: integer;
begin
  Result := -1;
  if Trim(CategoryName) = '' then
    Exit;
  for i := 0 to Count - 1 do
  begin
    if SameText(CategoryName, Items[i].CategoryName) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TCWCategories.GetColor(const CategoryName: string): TColor;
var
  Indx: integer;
begin
  Result := clBlack;
  Indx := IndexOf(CategoryName);
  if Indx <> -1 then
    Result := Items[Indx].Color;
end;

procedure TCWCategories.SetColor(const ItemName: string; AColor: TColor);
var
  Indx: integer;
  c: TCWCategory;
begin
  Indx := -1;
  if Trim(ItemName) <> '' then
    Indx := IndexOf(ItemName);
  if Indx <> -1 then
    Items[Indx].Color := AColor
  else
  begin
    c := Add;
    c.FCategoryName := ItemName;
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

function TCWCurve.QuerySpace(ItemCount : integer = -1) : integer;
var
 MinSpace : integer;
 W : integer;
 Cnt : integer;
begin
   Result := inherited;
   MinSpace := MinPointSpacing;
   if ItemCount = -1 then
   begin
   Cnt := Writer.LongestSeries.Count;
   end
   else
     Cnt := ItemCount;
   if Writer.FNameAxis.IsXAxis then
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

procedure TCWCurve.BrushChanged(Sender: TObject);
begin
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetStyle(const Value: TCurveStyle);
var
  Res : integer;
begin
  if (csLoading in componentState) then
  begin
    FStyle := Value;
    Exit;
  end;
  if Value = FStyle then
    Exit;
  Res := CanApplyStyle(Value, False);
  if Res = S_PointsWithOthers then
     ShowGWError(msg_MixPointsOthers)
  else if Res = S_NeighborWithOthers then
     ShowGWError(msg_MixNeighborOthers)
  else if Res = S_OthersWithNeighbor then
     ShowGWError(msg_MixThisNeighbor)
  else if Res = S_OthersWithPoints then
     ShowGWError(msg_MixThisPoints);

  FStyle := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetLineStyle(const Value: TCurvelineStyle);
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

procedure TCWCurve.SetLineShape(const Value: TLineShape);
begin
  if (csLoading in componentState) then
  begin
    FLineShape := Value;
    Exit;
  end;
  if FLineShape = Value then
    Exit;
  FLineShape := Value;
  if Writer <> nil then
  begin
   Writer.RefreshChart;
  end;
end;

procedure TCWCurve.SetLineWidth(const Value: integer);
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

function TCWCurve.GetStyle: TCurveStyle;
begin
    Result := FStyle
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

procedure TCWCurve.SetBaseLineValue(const Value: single);
begin
  if (FBaseLineValue = Value) then
    Exit;
  FBaseLineValue := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetAreaOutlineColor(const Value : TColor);
begin
  if (FAreaOutlineColor = Value) then
    Exit;
  FAreaOutlineColor := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetAreaOutline(const Value : Boolean);
begin
  if (FAreaOutline = Value) then
    Exit;
  FAreaOutline := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetSmoothLines(const Value : Boolean);
begin
  if (FSmoothLines = Value) then
    Exit;
  FSmoothLines := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetPointMarkers(const Value: TPointMarkers);
begin
  if Value = FPointMarkers then
    Exit;
  FPointMarkers := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWCurve.SetPointWidth(const Value: integer);
begin
  if (Value < 3) then
    Exit;
  FPointWidth := Value;
end;

procedure TCWCurve.SetMinPointSpacing(const Value: integer);
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
      Writer.RefreshChart;
    end;
  end;
end;

function TCWCurve.GetMinPointSpacing;
begin
  Result := FMinPointSpacing;
end;

procedure TCWCurve.SetBeaconPoints(const Value: Boolean);
begin
  if Value = FBeaconPoints then
    Exit;
  FBeaconPoints := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWCurve.GetInternalActiveColor(SeriesIndex, ItemIndex : integer): TColor;
begin
  Result := Writer.Chart.FSeriesDefs.Items[SeriesIndex].Color;
end;

procedure TCWCurve.SetInternalActiveColor(SeriesIndex, ItemIndex : integer; Value : TColor);
begin
  Writer.Chart.FSeriesDefs.Items[SeriesIndex].FColor := Value;
end;

function TCWCurve.CanApplyStyle(AStyle : TCurveStyle; InSeriesStyles : Boolean) : integer;
begin
  Result := 0;
  if AStyle in[csPoints, csNeighborArea] then
  begin
    if (SeriesStyles.Count > 0) or InSeriesStyles then
    begin
      if AStyle = csPoints then
       Result := S_PointsWithOthers
      else
       Result := S_NeighborWithOthers;
    end;
  end
  else
  begin
     if ((Style = csPoints) and InseriesStyles) then
       Result := S_OthersWithPoints
     else if ((Style = csNeighborArea) and InseriesStyles) then
       Result := S_OthersWithNeighbor;
  end;
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

procedure TCWCurve.DrawBezierBaselineArea(ASource : TSeries; Pts : TPointDynArray);
{Called from DrawArea. Pts is actually the baseline plygon}
var
   FX1, FX2,FY1, FY2 : single;
   CP1, CP2 : TGPPointF;
   i : integer;
   Path: TGPGraphicsPath;
   GBrush : TGPBrush;
   Clr : TColor;
   CStyl : TCurveLineStyle;
   W : integer;
   BaseLine : integer;

   procedure GetBaseLine;
   var
     BV : single;
   begin
        BV := BaseLineValue;
        begin
          if (BV < Writer.ActiveValueScale.ValueLow) or (BV > Writer.ActiveValueScale.ValueHigh) then
            BV := MaxInt;
        end;
        if BV <> MaxInt then
          BaseLine := Writer.PosFromValue(BV, Writer.Chart.SeriesDefs[ASource.Index].ActualAxis)
        else
        begin
          if Writer.FNameAxis.IsXAxis then
            BaseLine := Writer.GetGraphPrintRect.Bottom
          else
            BaseLine := Writer.GetGraphPrintRect.Left;
        end;
   end;

begin
    if Length(Pts) < 5 then
     Exit;
    CStyl := ActiveLineStyle[ASource];
    W := ActiveLineWidth[ASource];
    if FAreaOutline and (ActiveStyle[Asource] in [csClientArea, csNeighborArea]) then
      Clr := FAreaOutlineColor
    else
      Clr := InternalActiveColor[ASource.Index, -1];
    FGPen.SetColor(MakeGPClr(clr));
    FGPen.SetWidth(W);
    if CStyl = lsDot then
      FGPen.SetDashStyle(DashStyle.DashStyleDot)
    else if CStyl = lsDash then
      FGPen.SetDashStyle(DashStyle.DashStyleDash)
    else
      FGPen.SetDashStyle(DashStyle.DashStyleSolid);

    Path := TGPGraphicsPath.Create;
    GBrush := MakeGPBrush(AreaBrush, Writer.Chart.GraphBGColor, InternalActiveColor[ASource.Index, -1]);
    try
      {Get the curve part}
      for i := 0 to High(Pts) - 4 do
      begin
        CP1 := MakePoint(Pts[i].X + (Pts[i + 1].X - Pts[i].X) / 3,
                                    Pts[i].Y + (Pts[i + 1].Y - Pts[i].Y) / 3);
        CP2 := MakePoint(Pts[i + 1].X - (Pts[i + 2].X - Pts[i].X) / 3,
                                    Pts[i + 1].Y - (Pts[i + 2].Y - Pts[i].Y) / 3);
        FX1 := Pts[i].X;
        FX2 := Pts[i+1].X;
        FY1 := Pts[i].Y;
        FY2 := Pts[i+1].Y;
        Path.AddBezier(MakePoint(FX1, FY1), CP1, CP2, MakePoint(FX2, FY2));
      end;

      for I := High(Pts) - 3 to High(Pts)-1 do
      begin
        {Get the side an bottom lnes}
        FX1 := Pts[I].X;
        FX2 := Pts[I+1].X;
        FY1 := Pts[I].Y;
        FY2 := Pts[I+1].Y;
        Path.AddLine(MakePoint(FX1,FY1), MakePoint(FX2,FY2));
      end;
      GDIP.FillPath(GBrush, Path);
      GDIP.DrawPath(FGPen, Path);
      FGPen.SetColor(MakeGPClr(clBlack));
      GetBaseline;
      GDIP.DrawLine(FGPen, Writer.GetGraphPrintRect.Left, BaseLine,
       Writer.GetGraphPrintRect.Right, Baseline);
    finally
      Path.Free;
      GBrush.Free;
    end;
end;

procedure TCWCurve.DrawBezierCurve(ASource : TSeries);
var
   FX1, FX2,FY1, FY2 : single;
   CP1, CP2 : TGPPointF;
   a : TPointArray;
   i : integer;
   Path: TGPGraphicsPath;
   GBrush : TGPBrush;
   Clr : TColor;
   CStyl : TCurveLineStyle;
   W : integer;
   R : TRect;

begin
    if Writer.InView(TCWBar) <> nil then
      A := ASource.FBarPoints
    else
      A := ASource.FPoints;
    if A.Count < 3 then
     Exit;
    CStyl := ActiveLineStyle[ASource];
    W := ActiveLineWidth[ASource];
    if FAreaOutline and (ActiveStyle[Asource] in [csClientArea, csNeighborArea]) then
      Clr := FAreaOutlineColor
    else
      Clr := InternalActiveColor[ASource.Index, -1];
    FGPen.SetColor(MakeGPClr(clr));
    FGPen.SetWidth(W);
    if CStyl = lsDot then
      FGPen.SetDashStyle(DashStyle.DashStyleDot)
    else if CStyl = lsDash then
      FGPen.SetDashStyle(DashStyle.DashStyleDash)
    else
      FGPen.SetDashStyle(DashStyle.DashStyleSolid);

    Path := TGPGraphicsPath.Create;
    GBrush := MakeGPBrush(AreaBrush, Writer.Chart.GraphBGColor, InternalActiveColor[ASource.Index, -1]);
    try
      for i := 0 to A.Count - 3 do
      begin
        CP1 := MakePoint(a[i].X + (a[i + 1].X - a[i].X) / 3,
                                    a[i].Y + (a[i + 1].Y - a[i].Y) / 3);
        CP2 := MakePoint(a[i + 1].X - (a[i + 2].X - a[i].X) / 3,
                                    a[i + 1].Y - (a[i + 2].Y - a[i].Y) / 3);
        FX1 := a[i].X;
        FX2 := a[i+1].X;
        FY1 := a[i].Y;
        FY2 := a[i+1].Y;
        Path.AddBezier(MakePoint(FX1, FY1), CP1, CP2, MakePoint(FX2, FY2));

      end;
      FX1 := a[a.Count-2].X;
      FX2 := a[a.Count-1].X;
      FY1 := a[a.Count-2].Y;
      FY2 := a[a.Count-1].Y;
      Path.AddLine(MakePoint(FX1,FY1), MakePoint(FX2,FY2));
      if Style = csClientArea then
      begin
        R := Writer.GraphPrintRect;
        Path.AddLine(MakePoint(a[a.Count-1].X,a[a.Count-1].Y), MakePoint(R.Right, R.Bottom));
        Path.AddLine(MakePoint(R.Right, R.Bottom), MakePoint(R.Left, R.Bottom));
        Path.AddLine(MakePoint(R.Left, R.Bottom), MakePoint(R.Left, A[0].Y));
      end;
      if Style = csClientArea then
       GDIP.FillPath(GBrush, Path);
      GDIP.DrawPath(FGPen, Path);
    finally
      Path.Free;
      GBrush.Free;
    end;
end;

procedure TCWCurve.DrawTheLine(ASource : TSeries; Indx: integer;
 x1, y1, x2, y2: integer; StatLine : Boolean = false);
var
   Clr: TColor;
   CStyl: TCurvelineStyle;
   W: integer;
begin
    CStyl := ActiveLineStyle[ASource];
    W := ActiveLineWidth[ASource];
    if FAreaOutline and (ActiveStyle[Asource] in [csClientArea, csNeighborArea]) then
      Clr := FAreaOutlineColor
    else
      Clr := InternalActiveColor[ASource.Index, -1];
    if StatLine then
      W := StatLineWidth;
    FGPen.SetColor(MakeGPClr(clr));
    FGPen.SetWidth(W);
    if CStyl = lsDot then
      FGPen.SetDashStyle(DashStyle.DashStyleDot)
    else if CStyl = lsDash then
      FGPen.SetDashStyle(DashStyle.DashStyleDash)
    else
      FGPen.SetDashStyle(DashStyle.DashStyleSolid);

    if (LineShape = lsStep) and not StatLine and not (ActiveStyle[Asource] = csNeighborArea)then
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
  BrkLine : integer;
  SeriesFinished: Boolean;
  loop: integer;
  LoopCnt: integer;
  Stop : Boolean;

  procedure DoDraw(ASource: TSeries; StatLinesOnly : Boolean);
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
    Points: TPointArray;
    IsHandled: Boolean;
    GrRect : TRect;

    procedure SetOrigProps;
    begin
      if OrigProps.NeedsRestore then
        Exit;
      OrigProps.Clr := InternalActiveColor[ASource.Index, -1];
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
      Cnt: integer;
      BaseLine: integer;
      BV: single;

      GPts: TPointDynArray;
      GBrush: TGPBrush;
      PenClr : TGPColor;

    begin
      if Stp then
        setlength(GPts, (ASource.LastItem - ASource.FirstItem) * 2 + 3)
      else
       setlength(GPts, ASource.LastItem - ASource.FirstItem + 1 + 3);

      GPts[0] := MakePoint(P.X, P.Y);
      LastPt := P;
      if Cs = csBaseLineArea then
      begin
        BV := BaseLineValue;
        begin
          if (BV < Writer.ActiveValueScale.ValueLow) or (BV > Writer.ActiveValueScale.ValueHigh) then
            BV := MaxInt;
        end;
        if BV <> MaxInt then
          BaseLine := Writer.PosFromValue(BV, Chart.SeriesDefs[ASource.Index].ActualAxis)
        else
        begin
          if Writer.FNameAxis.IsXAxis then
            BaseLine := Writer.GetGraphPrintRect.Bottom
          else
            BaseLine := Writer.GetGraphPrintRect.Left;
        end;
      end
      else
      begin
        if Writer.FNameAxis.IsXAxis then
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
            GPts[Cnt] := MakePoint(P.X, P.Y);
            Inc(Cnt);
          end;
          P := Points[i - ASource.FirstItem];
          if FFirstBaseLine  then
          begin
             if (P.Y >= Baseline) then
               GPts[Cnt] := MakePoint(P.X, BaseLine)
             else
               GPts[Cnt] := MakePoint(P.X, P.Y);
          end
          else
          begin
            if (P.Y < Baseline) then
              GPts[Cnt] := MakePoint(P.X, BaseLine)
            else
              GPts[Cnt] := MakePoint(P.X, P.Y)
          end;
          inc(Cnt);
        end;
        if Writer.FNameAxis.IsXAxis then
        begin
          GPts[High(GPts) - 2] := MakePoint(P.X, BaseLine);
          GPts[High(GPts) - 1] := MakePoint(GPts[0].X, BaseLine);
        end
        else
        begin
          GPts[High(GPts) - 2] := MakePoint(BaseLine, P.Y);
          GPts[High(GPts) - 1] := MakePoint(BaseLine, GPts[0].Y);
        end;
        GPts[High(GPts)] := GPts[0];

        GBrush := MakeGPBrush(AreaBrush, Writer.Chart.GraphBGColor, InternalActiveColor[ASource.Index, -1]);
        try
        if LineShape = lsBezier then
        begin
          DrawBezierBaseLineArea(ASource, GPts);
          Exit;
        end
        else
         FGDIP.FillPolygon(GBrush, PGPPoint(@GPts[0]), Length(GPts));
        if AreaOutline then
        begin
          PenClr := MakeGPClr(AreaOutlineColor)
        end
        else
          PenClr := MakeGPClr(InternalActiveColor[ASource.Index, -1]);

        FGPen.SetColor(PenClr);
        if AreaOutline then
         FGDIP.DrawPolygon(FGPen, PGPPoint(@GPts[0]), Length(GPts));
        finally
         GBrush.Free;
        end;
      finally
        Writer.ResetCanvas(self);
      end;

    end;

    function DoLineEvent(ItemIndex: integer;
      Event: TDrawCurveLineEvent; StartPoint, EndPoint: TPoint): Boolean;
    begin
      if Writer.InState(stAnimating) then
        Exit;
      Result := False;
      if not Assigned(Event) or Assigned(FOnDrawCurve)
      then { Cannot do both }
        Exit;
      SetOrigProps;
      Event(ASource, Point(StartPoint.X, StartPoint.Y),
        Point(EndPoint.X, EndPoint.Y), ASource.Index, ItemIndex,
        Canvas, Result);
    end;

    procedure DrawThePoints(AIndex: integer);
    var
      PIndx: integer;
      Clr: TColor;

      procedure DrawConcentric;
      var
        R : TRect;
        GBrush : TGPBrush;
        W : integer;
      begin
         if PointMarkers = pmSmallConcentric then
           W := 4
         else
           W := 6;
         Clr := InternalActiveColor[ASource.Index, -1];
         R.Left := Points[PIndx].X - W;
         R.Right := Points[PIndx].X + W;
         R.Top := Points[PIndx].Y - W;
         R.Bottom := Points[PIndx].Y + W;
         FGPen.SetColor(MakeGPClr(Clr));
         FGDIP.DrawEllipse(FGPEN, MakeRect(R));
         if PointMarkers = pmSmallConcentric then
           W := 2
         else
           W := 3;
         R.Left := Points[PIndx].X - W;
         R.Right := Points[PIndx].X + W;
         R.Top := Points[PIndx].Y - W;
         R.Bottom := Points[PIndx].Y + W;
         GBrush := TGPSolidBrush.Create(MakeGPClr(Clr));
         FGDIP.FillEllipse(GBrush, MakeRect(R));
         GBrush.Free;
      end;

    begin
      GrRect := Writer.GraphRect;
      PIndx := AIndex - ASource.FirstItem;
      if (ActiveStyle[ASource] = csPoints) and (PointMarkers = pmNone) then
        FPointMarkers := pmDot;
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
          DrawConcentric;
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
        else if PointMarkers in [pmSmallConcentric, pmBigConcentric] then
          DrawConcentric
        else if PointMarkers = pmText then
        begin
          S := FormatNum(ASource.FSeriesItems[AIndex].Value,
            Writer.ValuePrecision);
          Sz := Canvas.TextExtent(S);
          Clr := InternalActiveColor[ASource.Index, -1];
          R.Left := Points[PIndx].X;
          R.Right := Points[PIndx].X + Sz.cx;
          R.Top := Points[PIndx].Y - (Sz.cy div 2) - 6;
          R.Bottom := Points[PIndx].Y + (Sz.cy div 2) - 6;
          if GrRect.Contains(R) then
          begin
            Canvas.Font.Color := Clr;
            Canvas.Pen.Color := clBlack;
            Canvas.Brush.Style := bsClear;
            Canvas.Brush.Color := Writer.Chart.GraphBGColor;
            Canvas.TextRect(R, R.Left, R.Top, S);
            Canvas.Pen.Color := Clr;
          end;
        end

        else
        begin
          Clr := InternalActiveColor[ASource.Index, -1];
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

  begin
    GrRect := Writer.GraphRect;
    FPaintSeriesIndex := ASource.Index;
    RestoreOrigProps;
    if Anim and not(ActiveStyle[ASource] = csLine) and
      not(ActiveStyle[ASource] = csPoints) then
      Cs := csLine
    else
      Cs := ActiveStyle[ASource];
    if ActiveStyle[ASource] = csNeighborArea then
      Stp := false
    else
      Stp := LineShape = lsStep;
    if (Cs = csBaseLineArea) and
      ((BaseLineValue < Writer.ActiveValueScale.ValueLow) or (BaseLineValue > Writer.ActiveValueScale.ValueHigh)) then
      Cs := csClientArea;

    if (cs = csNeighborArea) and ((Writer.VisibleCount < 2) or (Writer.Chart.GraphCount(Self) < 2)) then
      Cs := csLine;

    if Writer.GraphTypeInChart(TCWBar) then
      Points := ASource.FBarPoints
    else
      Points := ASource.FPoints;
    if (ASource.ItemCount = 0) or (Points.Count = 0) then
      Exit;

    if StatLinesOnly then
    begin
      DrawStatLine(GDIP, ASource);
      Exit;
    end;

    if Assigned(FOnDrawCurve) then
    begin
      IsHandled := False;
      SetOrigProps;
      if not Anim then
        FOnDrawCurve(Self, ASource.Index, Canvas, IsHandled);
      if IsHandled then
      begin
        RestoreOrigProps;
        Writer.ResetCanvas(Self);
        Exit;
      end;
    end;

    if (Cs = csNeighborArea) then
    { Drawn at the end of the paint process }
    begin
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

    if (LineShape = lsBezier) and (Cs = csLine) then
    begin
      DrawBezierCurve(ASource);
    end
    else if (LineShape = lsBezier) and (Cs = csBaseLineArea) then
      DrawArea
    else if (Cs = csClientArea) or (Cs = csBaseLineArea) then
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
          if not DoLineEvent(i - ASource.FirstItem, FOnDrawCurveLine,
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

    if DrawBaseline then
    begin
        BrkLine := Writer.PosFromValue(BaseLineValue, Chart.SeriesDefs[ASource.Index].ActualAxis);
        Canvas.Pen.Color := clBlack;
        if Writer.FNameAxis.IsXAxis then
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
    RestoreOrigProps;
    if (PointMarkers <> pmNone) and (ActiveStyle[ASource] = csLine) and
      not Writer.InState(stAnimating) then
      DrawThePoints(ASource.LastItem);
    Writer.ResetCanvas(Self);
  end;

  function GetNextVisible(ThisIndx : integer) : integer;
  var
    i : integer;
  begin
    Result := -1;
    for I := ThisIndx+1 to Chart.SeriesDefs.Count-1 do
    begin
      if Chart.SeriesDefs[i].Visible then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

  procedure PerformDraw(Index : integer; StatLinesOnly : Boolean; var Stop : Boolean);
  begin
      if not InChart(Index) then
       Exit;
      Stop := false;
      if Writer.Chart.FSeriesDefs.Items[i].Graph <> nil then
        if (Writer.Series[Index].Visible)
         and (Writer.Chart.FSeriesDefs.Items[Index].Graph = Self) then
        begin
          FPaintItemIndex := Index;
          DoDraw(Series[Index], StatLinesOnly);
        end;
      if Anim and SeriesFinished then
      begin
        Writer.FAnimInfo.NextHorz := 0;
        Writer.FAnimInfo.NextSeries := GetNextVisible(Index);
        if Writer.FAnimInfo.NextSeries = -1 then
          Writer.ClearState(stAnimating)
        else if AnimationPause <> 0 then
          Writer.FAnimInfo.Paused := True;
        Stop := True;
      end;
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
    if Writer.FAnimInfo.NextSeries = -1 then
     Writer.FAnimInfo.NextSeries := GetNextVisible(-1);
    loop := Writer.FAnimInfo.NextSeries;
    LoopCnt := loop;
  end;
  OrigProps.NeedsRestore := False;
  if Writer <> nil then
  begin
    FGDIP := TGPGraphics.Create(Canvas.Handle);
    FGDIP.SetSmoothingMode(SmoothingModeAntiAlias);
    FGDIP.SetInterpolationMode(InterpolationMode.InterpolationModeHighQuality);
    FGDIP.SetCompositingQuality(CompositingQuality.CompositingQualityHighQuality);
    FGPEN := TGPPen.Create(0);
    FFirstBaseLine := True;
    try
       for i := loop to LoopCnt do
       begin
         PerformDraw(I, false, Stop);
         if Stop then {Animation stop}
          Break;
       end;
       FFirstBaseline := false;
       for i := LoopCnt downto Loop do
       begin
        if ActiveStyle[Writer.Series[i]] = csBaselineArea then
          PerformDraw(I, False, Stop); {Mirror the baseline}
       end;
       if not Anim then
        DoDrawNeighbors;

       if StatLine <> slNone then
       begin
         if (StatBackgroundBlending > 0)  then
         begin
          writer.FAlfaBM.SetSize(writer.GraphRect.Width, writer.GraphRect.Height);
          writer.FAlfaBM.Canvas.CopyRect(Rect(0, 0, writer.FAlfaBM.Width, writer.FAlfaBM.Height),
           Canvas, writer.GraphRect);
          DrawAlphaBlend(writer.FAlfaBM.Canvas.Handle, Rect(0, 0, writer.FAlfaBM.Width,
          writer.FAlfaBM.Height), StatBackgroundBlending);
          Canvas.Draw(writer.GraphRect.Left, writer.GraphRect.Top, writer.FAlfaBM);
         end;

         for i := loop to LoopCnt do
         begin
           PerformDraw(I, True, Stop);
         end;
       end;
    finally
      FGDIP.Free;
      FGPEN.Free;
    end;
  end;

end;

procedure TCWCurve.DoDrawNeighbors;
var
  i, Indx: integer;
  function NextVisible(ThisInd : integer) : integer;
  var
    i : integer;
  begin
     Result := -1;
     for I := ThisInd + 1 to Chart.SeriesDefs.Count-1 do
       if Chart.SeriesDefs[i].Visible then
       begin
         Result := i;
         Break;
       end;
  end;

begin
  if Style <> csNeighborArea then
    Exit;
  for I := 0 to Writer.Count-1 do
  begin
    if not Chart.SeriesDefs[i].Visible then
      Continue;
    Indx := NextVisible(i);
    if Indx <> -1 then
    DrawNeighbors(i, Indx, Indx);
  end;
end;

procedure TCWCurve.DrawNeighbors(Serie1, Serie2, NeighborIndex: integer);
var
  tmp: integer;
  FirstIndx, LastIndx: integer;
  Pt: TPoint;
  i: integer;
  Cnt: integer;
  GP1, GP2 : TPointArray;

  GPts: TPointDynArray;
  GPts1: TPointDynArray;
  GPts2: TPointDynArray;
  GBrush: TGPBrush;
  PenClr : TGPColor;
  Shp : TLineShape;

begin
  if Writer = nil then
    Exit;
  if Writer.VisibleCount < 2 then
    Exit;
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

  setlength(GPts1, (LastIndx - FirstIndx + 1));
  setlength(GPts2, (LastIndx - FirstIndx + 1));

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

  {Make dyna points}
  for i := FirstIndx to LastIndx do
  begin
    Pt := GP1[i - Writer.Series[Serie1].FIndent];
    GPts1[i - Writer.Series[Serie1].FIndent] := MakePoint(Pt.X, Pt.Y);
  end;
  Cnt := 0;
  for i := LastIndx downto FirstIndx do
  begin
    Pt := GP2[i - Writer.Series[Serie2].FIndent];
    GPts2[Cnt] := MakePoint(Pt.X, Pt.Y);
    inc(Cnt);
  end;

  {Combine curves}
  setlength(GPts, Length(GPts1) + Length(GPts2));

  for i := 0 to Length(GPts1)-1 do
    GPts[i] := GPts1[i];

  for i := Length(GPts2) - 1 downto 0 do
    GPts[i + Length(GPts1)] := GPts2[i];

  Shp := LineShape;
  if Shp = lsBezier then
    FLineShape := lsStraight;
  GBrush := MakeGPBrush(AreaBrush, Writer.Chart.GraphBGColor, AreaBrush.Color);
  try
    FGDIP.FillPolygon(GBrush, PGPPoint(@GPts[0]), Length(GPts));
    if AreaOutline then
    begin
      PenClr := MakeGPClr(AreaOutlineColor)
    end
    else
      PenClr := MakeGPClr(AreaBrush.Color);

    FGPen.SetColor(PenClr);
    if AreaOutline then
     FGDIP.DrawPolygon(FGPen, PGPPoint(@GPts[0]), Length(GPts));
  finally
     GBrush.Free;
  end;

  {Draw the "actual" line}
  if (NeighborIndex = 1) and (Writer.VisibleCount = 2) then
  begin
    FStyle := csLine; {Prevent DrawTheLine from choosing the AreaOutlineColor}
    for i := 0 to High(GPts2) do
    begin
      if i > 0 then
      begin
        DrawTheLine(Writer.Series[Serie2], 0, GPts2[i - 1].X, GPts2[i - 1].Y, GPts2[i].X,
          GPts2[i].Y);
      end;
    end;
    FStyle := csNeighborArea;
  end;

  if (NeighborIndex = 2) then
  begin
    FStyle := csLine;
    for i := 0 to High(GPts1) do
    begin
      if i > 0 then
      begin
        DrawTheLine(Writer.Series[Serie1], 0, GPts1[i - 1].X, GPts1[i - 1].Y, GPts1[i].X,
          GPts1[i].Y);
      end;
    end;
    FStyle := csNeighborArea;
  end;
  FLineShape := Shp;
  Writer.ResetCanvas(Self);
end;

{ TCWBar ------------------------------------------------------------ }

constructor TCWBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarWidth := 20;
  FOrigBarWidth := FBarWidth;
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
  else if (BarWidth <= 10) and not (BarWidth = 0) then
    Result := False
  else if (FBarStyle = bsCube) and (CubeAngle = 0) then
    Result := false
  else
    Result := FBarStyle in [bsCube, bsCylinder];
  if (Writer <> nil) and Result then
  begin
    Result := Writer.FNameAxis.IsXAxis and not(boBaseLine in Options);
  end;
end;

function TCWBar.QuerySpace(ItemCount : integer = -1) : integer;
var
  BWidth, W : integer;
  Cpr : Boolean;
  Adjusted : Boolean;
  B : integer;
begin
  Result := inherited;
  BWidth := BarWidth;
  FBarWidth := 0;
  W := 0;
  try
    if not GetBarSpace(W, Cpr, Result, ItemCount) then
    begin
      Result := -1;
      FBarWidth := BWidth;
      Exit;
    end
    else
    begin
      if Result >= BWidth then
      begin
        if AutoSize then
          W := Result;
        Result := 0;
      end;
    end;

  finally
    if BWidth = 0 then
      FBarWidth := W
    else
      FBarWidth := BWidth;
    if (FBarStyle = bsCube) then
    begin
      Adjusted := false;
      if AutoSize then
      begin {Expand to maximum}
        FBarWidth := 1;
        while GetBarSpace(B, Cpr, Result, ItemCount) do
        begin
         Adjusted := True;
         Inc(FBarWidth);
        end;
        if Adjusted then
          Dec(FBarWidth);
      end
      else while not GetBarSpace(B, Cpr, Result, ItemCount) and (FBarWidth > 0) do
      {Shrink to fit, if necessary}
      begin
       Adjusted := True;
       Dec(FBarWidth);
      end;
      if AutoSize and not Adjusted then
      begin
      end
      else
      if FBarWidth = 0 then
        Result := -1
      else
        Result := FBarWidth;
    end
    else if AutoSize and (Result = 0) then
      Result := W;
    FBarWidth := BWidth;
  end;

end;

procedure TCWBar.SetOptions(const Value: TBarOptions);
begin
  if Value = FOptions then
    Exit;
  FOptions := Value;
  if Writer = nil then
    Exit;
  Writer.DoRepaint;
end;

procedure TCWBar.SetBaseLineValue(const Value: single);
begin
  if FBaseLineValue = Value then
    Exit;
  FBaseLineValue := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWBar.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize = Value then
    Exit;
  FAutoSize := Value;
  if csLoading in ComponentState then
    Exit;
  if InChart then
    Writer.RefreshChart;
end;

procedure TCWBar.SetLayout(const Value: TBarLayout);
begin
  if Value = FLayout then
    Exit;
  if (Writer <> nil) and InChart then
  if (Writer.Chart.NameScale.FOverflowAction = ovCompression) and (Value = blSideBySide)
   then
    ShowGWError(msg_SideBySideCompression);
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
      Writer.RefreshChart;
    end;
  end;
end;

procedure TCWBar.SetItemSpacing(const Value: integer);
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
      Writer.RefreshChart;
    end;
  end;
end;

procedure TCWBar.SetSeriesSpacing(const Value: integer);
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
      Writer.RefreshChart;
    end;
  end;
end;

procedure TCWBar.SetBarWidth(const Value: integer);
begin
  if Value = FBarWidth then
    Exit;
  if Value <= 0 then
    Exit;
  FBarWidth := Value;
  FOrigBarWidth := FBarWidth;
  if CubeDepth > FBarWidth * 2 then
    FCubeDepth := FBarWidth * 2;
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
      Writer.RefreshChart;
    end;
  end;
end;

function TCWBar.GetBarWidth: integer;
begin
  if (Writer <> nil) and Writer.Scrollable then
    Result := ScrollingBarWidth
  else
  begin
      Result := FBarWidth
  end;
end;

function TCWBar.GetTextQualifier : string;
begin
   Result := '';
   if ValueScale <> nil then
     Result := ValueScale.Qualifier;
end;

function TCWBar.GetScrollingBarWidth : integer;
begin
  if FBarWidth = 0 then
    Result := FScrollingBarWidth
  else
    Result := FBarWidth;
end;

procedure TCWBar.SetScrollingBarWidth(const Value: integer);
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
      Writer.RefreshChart;
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
    if (Layout = blSideBySide) and (Writer.VisibleCount > 1) then
    begin
      Result := (BWidth * Writer.VisibleCount) + ItemSpacing +
        (SeriesSpacing * (Writer.VisibleCount - 1));
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

procedure TCWBar.SetMinPointSpacing(const Value: integer);
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
    W := BarWidth;
  if BarStyle = bsCube then
  begin
    R := Rect(0, 0, BarWidth, 0);
    Get3DWH(R, Result, H);
  end
  else
    Result := W;
end;

procedure TCWBar.SetTextContents(const Value: TTextContents);
begin
  if Value = FTextContents then
    Exit;
  FTextContents := Value;
  if not (csLoading in ComponentState) and InChart then
    Writer.DoRepaint;
end;

procedure TCWBar.SetShowQualifier(const Value : Boolean);
begin
  if Value = FShowQualifier then
    Exit;
  FShowQualifier := Value;
  if not (csLoading in ComponentState) and InChart then
    Writer.DoRepaint;
end;

procedure TCWBar.SetCubeAngle(const Value: integer);
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
      Writer.RefreshChart;
    end;
  end;
end;

procedure TCWBar.SetCubeDepth(const Value: integer);
var
  V : integer;
begin
  if Value = FCubeDepth then
    Exit;
  V := Value;
  if (V < -8) then
      V := -8;
  FCubeDepth := V;
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
      Writer.RefreshChart;
    end;
  end;
end;

procedure TCWBar.SetBarStyle(const Value: TBarStyle);
begin
  if Value = FBarStyle then
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
      Writer.RefreshChart;
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

procedure TCWBar.SetInternalActiveColor(SeriesIndex, ItemIndex : integer; Value : TColor);
var
  S : string;
  Indx : integer;
begin
  if Writer.Chart.ColorUsage = cuOnItems then
  begin
      S := Writer.Series[SeriesIndex].SeriesItems[ItemIndex].FName;
      Indx := Writer.Chart.Categories.IndexOf(S);
      if Indx <> -1 then
        Writer.Chart.Categories.Items[Indx].FColor := Value
      else
        Writer.Chart.Categories.Items[ItemIndex].FColor := Value;
  end
  else
   Writer.Chart.SeriesDefs.Items[SeriesIndex].FColor := Value;
end;

function TCWBar.GetInternalActiveColor(SeriesIndex, ItemIndex : integer): TColor;
var
  R, G, B : Byte;
  S : string;
  Indx : integer;
begin
  if Writer.Chart.ColorUsage = cuOnSeries then
  begin
    Result := Writer.Chart.FSeriesDefs.Items[SeriesIndex].Color;
  end
  else
  begin
    if ItemIndex > Writer.Chart.Categories.Count-1 then
    begin
       if csDesigning in ComponentState then
      begin
        R := Random(255);
        G := Random(255);
        B := Random(255);
        Result := RGB(R,G,B);
      end
      else
       Result := clBlack;
    end
    else
    begin
      S := Writer.Series[SeriesIndex].SeriesItems[ItemIndex].FName;
      Indx := Writer.Chart.Categories.IndexOf(S);
      if Indx <> -1 then
        Result := Writer.Chart.Categories.Items[Indx].Color
      else
        Result := Writer.Chart.Categories.Items[ItemIndex].Color;
    end;
  end;
end;

function TCWBar.GetSeriesSpacing: integer;
begin
  Result := FSeriesSpacing;
end;

function TCWBar.Compressing: Boolean;
begin
  Result := InChart and (Writer.Chart.NameScale.OverflowAction = ovCompression)
  and (Writer.VisibleCount = 1);
end;

function TCWBar.GetCylinderHatHeight : integer;
begin
   Result := Ceil(BarWidth * 0.2)
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
  Wdth, WdthX: integer;
  MinTextSpace : integer;
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
  CanDrawText : Boolean;
  IsFirst : Boolean;


  function GRect: TRect;
  begin
    Result := Writer.FPosRect;
  end;

  function ItmClrsMultiSeries : Boolean;
  begin
    Result := (Writer.Count > 1) and (Layout = blStacked) and (Writer.Chart.ColorUsage = cuOnItems);
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
    if (Writer.FTrackBars.Count > 0) and (Layout = blStacked)then

    begin
      Tb := Writer.FTrackBars[Writer.FTrackBars.Count - 1];
      BothOver := False;
      BothUnder := False;
      if UsebaseLine then
      begin
        if (Writer.FNameAxis.IsXAxis) then
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
      if (Writer.FNameAxis.IsXAxis) then
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

  procedure DrawBarText(ARect: TRect; SerInd, ItmIndex: integer; FirstBar : Boolean);
  var
    S, Q: string;
    sl : TStringList;
    W, H: integer;
    X, Y: integer;
    V, P: single;
    Gr: TRect;
    TLeft, TRight, BLeft, BRight : TPoint;
    OlapRect : TRect;
    HasImages : Boolean;
  const
    Marg = 3;

  function Stacked : Boolean;
  begin
    Result := (Layout = blStacked) and (Writer.VisibleCount > 1);
  end;

  procedure SetTextWidth;
  begin
      if Canvas.TextWidth(S) > W then
       W := Canvas.TextWidth(S);
      if HasImages then
      begin
        if Writer.FImageSize.cx > W then
          W := Writer.FImageSize.cx;
      end;
  end;

  procedure DoDraw(R : TRect);
  var
    i : integer;
    BGClr : TColor;
  begin
    BGClr := Canvas.Pixels[R.CenterPoint.X, R.CenterPoint.Y];
    Canvas.Font.Color := InvertColor(BGClr);
    H := Canvas.TextHeight('X');
    Y := R.Top;
    if HasImages then
      begin
        X := R.CenterPoint.X - Writer.FImageSize.cx div 2;
        Chart.Categories.Items[ItmIndex].Image.Graphic.Transparent := True;
        Canvas.Draw(X, Y, Chart.Categories.Items[ItmIndex].Image.Graphic);
        inc(Y, Writer.FImageSize.cy);
      end;

    for I := 0 to sl.Count-1 do
    begin
     W := Canvas.TextWidth(Sl[i]);
     X := R.CenterPoint.X - W div 2;
     Canvas.TextOut(X, Y, Sl[i]);
     inc(Y, H);
    end;

  end;

  begin
    HasImages := (Writer.FImageSize.cx > 0) and (boBarImages in Options);
    if not(boText in Options) and not HasImages then
      Exit;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
    if Writer.InState(stAnimating) and not(Animations = [anFlow]) and
      not ((AnimationPause <> 0) and (Animations = [anFlow])) then
    begin
      if not Writer.InState(stAnimationPause) and not VertFinished then
      begin
        Exit;
      end;
    end;

    Canvas.Font.Assign(Font);
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
    if (tcValue in TextContents) and not Chart.Percentages then
    begin
     S := FormatNum(V, Writer.ValuePrecision, True) + Q;
     sl.Add(S);
     SetTextWidth;
    end;
    if tcPercentage in TextContents then
    begin
     if Chart.Percentages then
      P := Writer.SeriesItems[SerInd, ItmIndex].Value
     else
      P := Writer.SeriesItems[SerInd, ItmIndex].Pst;
     S := FormatNum(P, Writer.ValuePrecision, True) + '%';
     sl.Add(S);
     SetTextWidth;
    end;

    H := Writer.GetTextHeight(lkInfo) * sl.Count;
    if HasImages then
      inc(H, Writer.FImageSize.cy);
    Canvas.Brush.Style := bsClear;
    Gr := Writer.GetGraphPrintRect;

    if (BarStyle = bsCube) and (not Stacked or FirstBar) then
    begin
     Get3DDims(ARect, TLeft, TRight, BLeft, BRight);
     ARect.Top := ARect.Top + (TRight.Y-ARect.Top);
     ARect.Right := ARect.Right + (TRight.X - ARect.Right);
    end
    else if (BarStyle = bsCylinder) and (not Stacked or FirstBar) then
    begin
     ARect.Top := ARect.Top - GetCylinderHatHeight;
    end;

    if Writer.FNameAxis.IsXAxis then
    begin
      if not(boBaseLine in Options) or
        ((boBaseLine in Options) and (V >= BaseLineValue)) then
      begin
        if (ARect.Top - Marg - H < Writer.GraphRect.Top) or (Stacked and not FirstBar) then
        begin
          Y := ARect.Top + Marg;
        end
        else
        begin
          Y := ARect.Top - Marg - H;
        end;
      end
      else
      begin
        if (ARect.Bottom + H + Marg > Writer.GraphRect.Bottom) or (Stacked and not FirstBar) then
        begin
          Y := ARect.Bottom - Marg - H;
        end
        else
        begin
          Y := ARect.Bottom + Marg;
        end;
      end;
      if (Y > Writer.GraphRect.Top) and (Y < Writer.GraphRect.Bottom) then
      begin
        GR := ARect;
        GR.Left := ARect.CenterPoint.X - W div 2;
        GR.Right := Gr.Left + W;
        GR.Top := Y;
        GR.Bottom := GR.Top + H;
        if FOverlaps.Overlapped(GR, OlapRect) then
        begin
          if GR.CenterPoint.Y > FOverlaps.FOuter.CenterPoint.Y then
           FOverlaps.MoveDir := mdUp
          else
           FOverlaps.MoveDir := mdDown;
        end;
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
        if (ARect.Right + W + Marg < Writer.GraphRect.Right) and (not Stacked or (Stacked and FirstBar)) then
        begin
          X := ARect.Right + Marg;
        end
        else
        begin
          X := ARect.Right - Marg-W;
        end;
      end
      else
      begin
        if (ARect.Left - W - Marg > Writer.GraphRect.Left) and (not Stacked or (Stacked and FirstBar)) then
        begin
          X := ARect.Left - W - Marg;
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
        if FOverlaps.Overlapped(GR, OlapRect) then
        begin
          if GR.CenterPoint.X > FOverlaps.FOuter.CenterPoint.X then
           FOverlaps.MoveDir := mdLeft
          else
           FOverlaps.MoveDir := mdRight;
        end;
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
    if Writer.FNameAxis.IsXAxis then
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
      if Dir = gdVertical then
      begin
        R.Bottom := Pt.Y;
        GradientFillCanvas(Canvas, EndClr, Color, R, Dir);
        R := ARect;
        R.Top := R.CenterPoint.Y;
        GradientFillCanvas(Canvas, Color, EndClr, R, Dir);
      end
      else
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
    LastClr : TColor;

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
      D3Inc := GetCylinderHatHeight;
    end;
    inc(ARect.Bottom, D3Inc);
    R := ARect;
    R.Bottom := Writer.GetGraphPrintRect.Bottom - 1 + D3Inc;
    R.Top := R.Bottom - GetCylinderHatHeight;
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
      if Layout = blStacked  then
      begin
        if Writer.InState(stAnimating) then
          LastClr := Writer.FAnimInfo.LastColor
        else
          LastClr := LastColor;

        GPClr1 := MakeGPClr(LastClr);
        Clr := ChangeColor(LastClr, d3_GradientCylinder);
        GPClr2 := MakeGPClr(Clr);

        GradBrush := TGPLinearGradientBrush.Create(MakePoint(R.Left, 10),
          MakePoint(R.Right, 10), GPClr1, GPClr2);
        GDIP.FillPie(GradBrush, MakeRect(R), 0, 180);
        GradBrush.Free;
        if (boOutLines in Options) or ItmClrsMultiseries then
        begin
          DrawOutLine(LastColor, False);
        end;
      end ;
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
      Block := (W3D * Writer.VisibleCount) + (SeriesSpacing * (Writer.VisibleCount - 1));
    end
    else
      Block := (W * Writer.VisibleCount) + (SeriesSpacing * (Writer.Count - 1));
    if Writer.FNameAxis.IsXAxis then
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
      if SelColor <> InternalActiveColor[SerInd, ItmNumber] then
      begin
        Canvas.Brush.Color := InternalActiveColor[SerInd, ItmNumber];
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
      Block := (W3 * Writer.VisibleCount) + (SeriesSpacing * (Writer.VisibleCount - 1));
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
      if Writer.FNameAxis.IsXAxis then
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
      if Writer.FNameAxis.IsXAxis then
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
    if Writer.FNameAxis.IsXAxis then
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

{ Main proc Draw bars }
begin
  if Writer = nil then
    Exit;
  if not InChart then
    Exit;
  Writer.FTrackBars.Clear;

  UsebaseLine := (boBaseLine in Options) and (BaseLineValue > Writer.ActiveValueScale.ValueLow) and
    (BaseLineValue < Writer.ActiveValueScale.ValueHigh);
  Canvas.Brush.Style := bsSolid;
  BS := ItemSpacing;
  if (Layout = blSideBySide) and not Compressing then
    BS := BS + (SeriesSpacing * (Writer.VisibleCount - 1));

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
      WInt := round(WInt / Writer.VisibleCount);
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

  if Writer.FNameAxis.IsXAxis then
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
    if AnimationBooster >= 0 then
     Writer.FAnimInfo.NextVert := Writer.FAnimInfo.NextVert +
       AnimationBooster + 1
     else
      Writer.FAnimInfo.NextVert := Writer.FAnimInfo.NextVert + 1;
    if not HorzAnim then
    begin
      LastBar := Writer.GetGraphPrintRect;
      inc(LastBar.Left);
      Canvas.Brush.Color := Writer.Chart.GraphBGColor;
      Canvas.FillRect(LastBar);
      Writer.FNameAxis.DrawSections;
      Writer.FValueAxis.DrawSections;
      Writer.DrawBorders;
    end;
  end;
  Posits := TBarPosits.Create;
  try
    VertChanged := False;
    VertFinished := False;
    while GetNext(BarIndx, Posits, ItmNumber) do
    begin
      FPaintItemIndex := ItmNumber;
      for i := 0 to Posits.Count - 1 do
      begin
        BrkLine := Writer.PosFromValue(BaseLineValue, Chart.SeriesDefs[Posits[i].SerInd].ActualAxis);
        FPaintSeriesIndex := Posits[i].SerInd;
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

        SelColor := InternalActiveColor[Posits[i].SerInd, ItmNumber];
        Canvas.Brush.Color := SelColor;
        if (boOutLines in Options) or ItmClrsMultiseries then
        begin
          Canvas.Pen.Color := ChangeColor(SelColor, d3_Outline)
        end
        else
          Canvas.Pen.Color := Canvas.Brush.Color;

        if BarIndx > 0 then
        begin
          if Writer.FNameAxis.IsXAxis then
          begin
            Pt := Posits[i];
          end
          else
          begin
            Pt := Posits[i];
          end;
        end;
        if Writer.FNameAxis.IsXAxis then
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
        if not (Writer.FNameAxis.IsXAxis)  and (R.Width <> 0) then
          inc(R.Left);

        if (R.Height = 0)
          and (Writer.Series[Posits[i].SerInd].FSeriesItems[ItmNumber].Value > 0) then
          begin
           Dec(R.Top);
          end;
        if (R.Width = 0)
          and (Writer.Series[Posits[i].SerInd].FSeriesItems[ItmNumber].Value > 0) then
          begin
           Inc(R.Left);
           Inc(R.Right,2);
          end;

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
            if Writer.Chart.ColorUsage = cuOnItems then
            if SelColor <> InternalActiveColor[Posits[i].SerInd, ItmNumber] then
            begin
              Canvas.Brush.Color := InternalActiveColor[Posits[i].SerInd, ItmNumber];
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
        Writer.FAnimInfo.LastColor := LastColor;
        if Writer.FAnimInfo.Stopped then
          Break;
        Writer.ResetCanvas(Self);
      end; { End posit loop }

      if (Posits.Count > 0) and (Layout = blStacked) then
      begin
        if Writer.FNameAxis.IsXAxis then
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
  if ((boText in Options) and (TextContents <> [])) or (boBarImages in Options)
   then
  begin
    if Writer.FNameAxis.IsXAxis then
      MinTextSpace := MinBarTextSpaceVert
    else
      MinTextSpace := MinBarTextSpaceHorz;
    WdthX := Get3DBarWidth;
    if Layout = blSideBySide then
      CanDrawText := ((WdthX + SeriesSpacing >= MinTextSpace)
       and (WdthX + ItemSpacing >= MinTextSpace)) or (WdthX >= MinTextSpace)
    else
      CanDrawText := (WdthX + ItemSpacing >= MinTextSpace) or (WdthX >= MinTextSpace);
    if CanDrawText then
    for i := 0 to Writer.FTrackBars.Count - 1 do
    begin
      if Writer.InState(stAnimating) then
        IsFirst := (FHorzCounter mod Writer.VisibleCount = 0)
      else
        IsFirst := (i mod Writer.VisibleCount = 0);
      DrawBarText(Writer.FTrackBars[i].VisibleRect,
       Writer.FTrackBars[i].SeriesIndex, Writer.FTrackBars[i].SeriesItmIndex,
       IsFirst);

    end;
  end;

  if UseBrk and DrawBaseline then
  begin
    Canvas.Pen.Color := clBlack;
    if Writer.FNameAxis.IsXAxis then
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
  Writer.ResetCanvas(Self);
end;

procedure TCWBar.Draw;
var
  i : integer;
begin
  inherited Draw;
  try
    if Writer.FNameAxis.IsXAxis then
      FOverlaps := TOverlaps.Create(Writer.GraphRect, mdDown)
    else
     FOverlaps := TOverlaps.Create(Writer.GraphRect, mdLeft);
    DoDraw;

    if StatLine <> slNone then
    begin
       if (StatBackgroundBlending > 0)  then
         begin
          writer.FAlfaBM.SetSize(writer.GraphRect.Width, writer.GraphRect.Height);
          writer.FAlfaBM.Canvas.CopyRect(Rect(0, 0, writer.FAlfaBM.Width, writer.FAlfaBM.Height),
           Canvas, writer.GraphRect);
          DrawAlphaBlend(writer.FAlfaBM.Canvas.Handle, Rect(0, 0, writer.FAlfaBM.Width,
          writer.FAlfaBM.Height), StatBackgroundBlending);
          Canvas.Draw(writer.GraphRect.Left, writer.GraphRect.Top, writer.FAlfaBM);
         end;
       for I := 0 to Chart.SeriesDefs.Count-1 do
       begin
         if Chart.SeriesDefs[i].Visible and (Chart.SeriesDefs[i].Graph = self) then
           DrawStatLine(FGDIP, Series[i]);

       end;
    end;
  finally
    FGDIP.Free;
    FOverlaps.Free;
  end;
end;

function TCWBar.GetBarSpace(var BlockWidth: integer;
  var Compressed: Boolean; var BarWidth : integer; ItemCount : integer = -1): Boolean;
{ Checks if there is enough space to render the diagram within then current graph rectangle
  given the current bar width and spacing. The BlocWidth param returns the required block
  width (bar width + spacing). If this is too big the function returns false and returns
  the "correct" BarWidth }
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
  c_SideBySideMin = 1;
  c_RegularMin = 1;

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
  if Writer.FNameAxis.IsXAxis then
    GrWidth := Writer.GetGraphPrintRect.Width
  else
    GrWidth := Writer.GetGraphPrintRect.Height;
  if ItemCount = -1 then
  begin
   if Writer.FLongestSeries = nil then
     Writer.FLongestSeries := Writer.LongestSeries;
   BlockCount := Writer.FLongestSeries.FSeriesItems.Count;
  end
  else { When seriescount = 0. relevent whet call from GetDataAttributs}
    BlockCount := ItemCount;
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
    BlockWidth := Trunc(BlockSize); { Block size in whole units }
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
    BarWidth := Trunc(BarWidth / RealDataCount);
    { The indivdual bar widths in a side by side layout }
    if BarWidth < SideBySideMin then
    { Side by side cannot be less than 4 or fixed, else looks unfriendly }
    begin
      Result := False;
      BlockWidth := SideBySideMin * RealDataCount + Spacing;
    end;
  end;

  if (Writer.Chart.NameScale.OverflowAction = ovCompression) and not Result then
  begin
    Result := True;
    Compressed := True;
    if ItemCount = -1 then
      BlockWidth := round(Writer.GraphPrintRect.Width / Writer.NameCount)
    else
      BlockWidth := round(Writer.GraphPrintRect.Width / ItemCount);
    if BlockWidth = 0 then
      BlockWidth := 1;
  end;
end;

{ TCWPie ------------------------------------------------------------ }

constructor TCWPie.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDiscDepth := -5;
  FSlope := 0;
  FPieSize := 500;
  FSeriesTitleFont := TFont.Create;
  FSeriesTitleFont.OnChange := TitleFontChange;
  FCanDrawText := True;
  FOptions := [poAllowImages];
end;

destructor TCWPie.Destroy;
begin
  FSeriesTitleFont.Free;
  inherited;
end;

procedure TCWPie.TitleFontChange(Sender : TObject);
begin
   if InChart then
     Writer.DoRepaint;
end;

procedure TCWPie.SetOptions(const Value: TPieOptions);
begin
  if FOptions = Value then
    Exit;
  FOptions := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetSliceSpacing(const Value: single);
begin
  if (Value > 2) or (Value < 0) or (Value = FSliceSpacing) then
    Exit;
  FSliceSpacing := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetDiscDepth(const Value: integer);
begin
  if csLoading in ComponentState then
   begin
     FDiscDepth := Value;
     Exit;
   end;
   if (Value = FDiscDepth) then
    Exit;
  if (Value < -10) then
    ShowGWError(msg_DiscDepth)
  else if Value = 0 then
    Exit;
  FDiscDepth := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetSlope(const Value : integer);
begin
   if FSlope = Value then
     Exit;
   if csLoading in ComponentState then
   begin
     FSlope := Value;
     Exit;
   end;
   if (Value > 70) or (Value < 0) then
     ShowGWError(msg_PieSlope);
   FSlope := Value;
   if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetStyle(const Value : TPieStyle);
begin
   if FStyle = Value then
     Exit;
   if csLoading in ComponentState then
   begin
     FStyle := Value;
     Exit;
   end;
   FStyle := Value;
   if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetStartAngle(const Value : integer);
begin
   if FStartAngle = Value then
     Exit;
   if csLoading in ComponentState then
   begin
     FStartAngle := Value;
     Exit;
   end;
   if (Value < 0) or (Value > 360) then
     ShowGWError(msg_StartAngle);
   FStartAngle := Value;
   if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetValuePrecision(const Value : integer);
begin
  if Value = FValuePrecision then
   Exit;

  if csLoading in ComponentState then
   begin
     FValuePrecision := Value;
     Exit;
   end;
  if Value < 0 then
    Exit;
  if Value > 3 then
    ShowGWError(msg_Max3Decimals);
  FValuePrecision := Value;
  if InChart then
    Writer.RefreshChart;
end;

procedure TCWPie.SetPieSize(const Value : integer);
begin
   if FPieSize = Value then
     Exit;
   if csLoading in ComponentState then
   begin
     FPieSize := Value;
     Exit;
   end;
   if (Value < 50) then
     ShowGWError(msg_MinPieSize);
   FPieSize := Value;
   if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWPie.SetDoughnutSize(const Value: integer);
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
  StartPt: TPoint;
  ShadowPie : integer;
  ShadowRect : TRect;
  ShadowLoop : integer;
  ShadowCnt : integer;
  Wdth, Hght : integer;
  MaxText : integer;
  OuterWidth, OuterHeight : integer;
  Anim : Boolean;
  SeriesFinished : Boolean;
  SliceFinished : Boolean;
  IsDrawn : Boolean;
const
  Spacing = 10;
  PinMarg = 10;

  function GetCenter(var BoundsRect : TRect; Ang:single) : TPoint;
  var
    h, w : integer;
    T : single;
    DoRect : TRect;
    wd, hd : integer;
    Lf, Tp : integer;
    R : TRect;
  begin
    if DoughnutSize > 0 then
    begin
      wd := round(BoundsRect.Width * FDoughnutSize / 100);
      hd := round(BoundsRect.Height * FDoughnutSize / 100);
      Lf := BoundsRect.CenterPoint.X - wd div 2;
      Tp := BoundsRect.CenterPoint.Y - hd div 2;
      DoRect := Rect(Lf, Tp, Lf+wd, Tp+hd);
      W := (BoundsRect.Width - (DoRect.Left - BoundsRect.Left)) div 2;
      H := (BoundsRect.Height - (DoRect.Top - BoundsRect.Top)) div 2;
    end
    else
    begin
      H := Round(BoundsRect.Height div 2 div 2);
      W := Round(BoundsRect.Width div 2 div 2);
    end;

    R.Left := BoundsRect.CenterPoint.X - W;
    R.Right := BoundsRect.CenterPoint.X + W;
    R.Top := BoundsRect.CenterPoint.Y - H;
    R.Bottom := BoundsRect.CenterPoint.Y + H;

    T := ArcTan2(W*Sin(Ang),H*Cos(Ang));
    Result.X := Round(R.CenterPoint.X + W * Cos(t));
    Result.Y := Round(R.CenterPoint.Y + H * Sin(t));
    BoundsRect := R;
  end;

  function ShadowSize : integer;
  begin
    if Style = psDisc then
    begin
      if DiscDepth < 0 then
       Result := Round(Abs(DiscDepth) * ActualSize / 100)
      else
       Result := DiscDepth;
    end
    else
      Result := 0;
  end;

  procedure DoDraw(ASource: TSeries);
  var
    GPen: TGPPen;
    GBrush: TGPSolidBrush;
    SeritIndx: integer;
    Overlaps : TOverlaps;

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
      Pst: single; DoughNut : Boolean = false);
    var
      Ang: single;
      TP: TPieSlice;
      RealItmIndex: integer;
      N : integer;
      Clr : integer;
      ClipRgn : HRGN;
      AR  : TRect;
    begin
      AR := ShadowRect;
      ClipRgn := 0;
      try
        if ASource.FPieState <> 0 then
        begin
          SetTheColor(clGray);
        end
        else if Serit[ItmIndex].FSpace then
        begin
          SetTheColor(Writer.Chart.GraphBGColor);
        end
        else
        begin
          if SeritIndx > Chart.Categories.Count-1 then
          begin
            { Shades of gray}
            N := ASource.ItemCount-Chart.Categories.Count;
            Clr := 255 div N * SeritIndx;
            Clr := RGB(Clr, Clr, Clr);
            if ShadowPie = 1 then
             Clr := ChangeColor(Clr, 20);
            SetTheColor(Clr);
          end
          else
          begin
           Clr := InternalActiveColor[ASource.Index, SeritIndx];
           if (ShadowPie = 0) or DoughNut then
             Clr := ChangeColor(Clr, 20);
           SetTheColor(Clr);
          end;
        end;
        if (ShadowPie = 1) or Doughnut then
        begin
          if not DoughNut then
          begin
            AR := DrawRect;
            Inc(Ar.Top, ShadowSize);
            {Lower the top with the sahdow size to synchronize the center points.
            This makes the draw rect somewhat oval}
            GDIP.DrawArc(GPen, MakeRect(AR.Left, AR.Top,
              AR.Width, AR.Height), LastAngle, Angle);
          end
          else
            AR := DrawRect;
          GDIP.FillPie(GBrush, MakeRect(AR.Left, AR.Top,
           AR.Width, AR.Height), LastAngle, Angle);
          if DoughNut then
            Exit;
        end
        else
        begin
         if (ShadowPie = 0) then
         begin
           {Drawing the shadow color. Clip the top half, else the shadow
           will be visible on top }
           ClipRgn := CreateRectRgn(AR.Left, AR.Top + AR.Height div 2,
              AR.Right, AR.Bottom+1);
            GDIP.SetClip(ClipRgn,CombineModeReplace);
         end;

         GDIP.FillPie(GBrush, MakeRect(AR.Left, AR.Top,
           AR.Width, AR.Height), LastAngle, Angle);
         GDIP.DrawPie(GPen, MakeRect(AR.Left, AR.Top, AR.Width,
          AR.Height), LastAngle, Angle);
        end;
      finally
        GBrush.Free;
        GPen.Free;
        if ShadowPie = 0 then
        begin
         GDIP.ResetClip;
         DeleteObject(ClipRgn);
        end;
      end;

      if ASource.FPieState <> 0 then
        Exit;

      if SliceSpacing <> 0 then
        RealItmIndex := ASource.FTrackPies.Count
      else
        RealItmIndex := ItmIndex;

      if not Serit[ItmIndex].FSpace then
      begin
        TP.EndAngle := LastAngle + Angle;
        TP.StartAngle := LastAngle;
        TP.Color := InternalActiveColor[ASource.Index, SeritIndx];
        TP.FSeriesIndex := ASource.Index;
        TP.FSeriesItmIndex := RealItmIndex;
        TP.Percentage := Pst;
      end;

      Angle := LastAngle + (Angle / 2);
      if Angle > 360 then
        Angle := Angle - 360;
      Ang := DegToRad(Angle);
      Center := GetCenter(AR, Ang); {Center of text within the slice}
      if not Serit[ItmIndex].FSpace then
      begin
        TP.Center := Center; { Save center in track pies. Picked up by DrawText}
        TP.TextBounds := AR;
        TP.TextAngle := Angle;
        TP.Color := InternalActiveColor[ASource.Index,SeritIndx];
        ASource.FTrackPies.Add(TP);
        AR := DrawRect;
        Inc(AR.Top, ShadowSize);
        ASource.FPieRect := AR;
      end;
      Writer.Canvas.Font.Assign(Font);
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
      SourceAngle : single;
      ThisItem : integer; {Animation}
      R: TRect;
      DrawRect: TRect;
      PinRect : TRect;
      OuterRect : TRect;
      Center: TPoint;
      S: string;
      Serit: TSeriesItems;
      Handled: Boolean;
      Err: Boolean;
      AngleCnt : single;

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
        BoundsRect : TRect;
        Tangl : single;
        Dummy : TRect;
        TR : TRect;
        Pst : single;
        HasImages : Boolean;
        TxtHeight : integer;

        function HasMultiBackgoundClr(ARect : TRect; var BGClr : TColor) : Boolean;
        type
          TClrs = array[1..9] of TColor;
        var
          Clrs : TClrs;
          ClrCount : integer;
          X, Y : integer;
          i : integer;
          LastClr : TColor;
        begin
          ClrCount := 0;
          Result := false;
          for I := 1 to 3 do
          begin
            if I = 1 then
              X := ARect.Left+2
            else if i = 2 then
              X := ARect.CenterPoint.X
            else
              X := ARect.Right-2;
            Y := ARect.Top + 2;
            inc(ClrCount);
            Clrs[ClrCount] := Canvas.Pixels[X,Y];
            Y := ARect.CenterPoint.Y;
            inc(ClrCount);
            Clrs[ClrCount] := Canvas.Pixels[X,Y];
            Y := ARect.Bottom-2;
            inc(ClrCount);
            Clrs[ClrCount] := Canvas.Pixels[X,Y];
          end;
          LastClr := Clrs[1];
          for I := 2 to 9 do
            begin
               if Clrs[i] <> LastClr then
               begin
                 Result := True;
                 Break;
               end
               else
                LastClr := Clrs[i];
            end;
          if not Result then
            BGClr := Clrs[1];
        end;

        procedure DrawImagedText;
        begin
          Chart.Categories.Items[Indx].Image.Graphic.Transparent := True;
          Canvas.Draw(TR.Left, TR.Top, Chart.Categories.Items[Indx].Image.Graphic);
          if TxtHeight < Writer.FImageSize.cy then
            TR.Top := Tr.Top + (Writer.FImageSize.cy - TxtHeight) div 2
          else if TxtHeight > Writer.FImageSize.cy then
            TR.Top := Tr.Top - (TxtHeight - Writer.FImageSize.cy) div 2;
          Canvas.TextOut(TR.Left + Writer.FImageSize.cx + 3, TR.Top, S);
        end;

        procedure DrawSliceText;
        var
         Ratio : single;
         GBrush: TGPSolidBrush;
        begin
          TR.Left := Center.X - TX.cx div 2;
          TR.Right := TR.Left + TX.Width;
          TR.Top := Center.Y - TX.cy div 2;
          TR.Bottom := TR.Top + TX.Height;
          Ratio := BoundsRect.Height / BoundsRect.Width;
          while Overlaps.Overlapped(TR, Dummy) do
          begin
            if Dummy = Overlaps.FOuter then
             Break;
            if ((TAngl >= 0) and (TAngl <= 90)) then
            {Move down}
            begin
              TR.Left := Tr.Left - Round(2/Ratio);
              TR.Top := Tr.Top + 2;
              TR.Bottom := Tr.Bottom + 2;
              Tr.Right := TR.Right - Round(2/Ratio);
            end
            else if (TAngl > 90) and (TAngl <= 180) then
            {Move Up}
            begin
              TR.Left := Tr.Left + Round(2/Ratio);
              TR.Top := Tr.Top - 2;
              TR.Bottom := Tr.Bottom - 2;
              Tr.Right := TR.Right + Round(2/Ratio);
            end
            else if (TAngl > 180) and (TAngl <= 270) then
            {Move Up}
            begin
              TR.Left := Tr.Left + Round(2/Ratio);
              TR.Top := Tr.Top - 2;
              TR.Bottom := Tr.Bottom - 2;
              Tr.Right := TR.Right + Round(2/Ratio);
            end
            else
            {Move Down}
            begin
              TR.Left := Tr.Left - Round(2/Ratio);
              TR.Top := Tr.Top + 2;
              TR.Bottom := Tr.Bottom + 2;
              Tr.Right := TR.Right -Round(2/Ratio);
            end;
          end;
          Overlaps.Add(TR);
          if (poTextBackground in Options) and HasMultiBackgoundClr(TR, Clr) then
          begin
            GBrush := TGPSolidBrush.Create(MakeGPClr(RGB(200, 200, 200), 60));
            try
              FGDIP.FillRectangle(GBrush, MakeRect(TR));
              if not KeepFontColor then
               Canvas.Font.Color := clBlack;
            finally
              GBrush.Free;
            end;
          end;

          if Assigned(FOnDrawPieSlice) then
          begin
            FOnDrawPieSlice(Self, ASource.Index, Indx, S, TR, Canvas, Handled);
          end;
          if not Handled then
          begin
            if HasImages then
              DrawImagedText
            else
              Canvas.TextOut(TR.Left, TR.Top, S);
          end;
        end;

        procedure DrawPinText;
        var
          GPen: TGPPen;
          PinCenter : TPoint;
          H, W : integer;
          T : single;
          Adeg : single;
          BelowY : Boolean;
          RightOfX : Boolean;
          X, Y : integer;

        begin
          GPen := TGPPen.Create(MakeGPClr(clBlack));
          W := PinRect.Width div 2;
          H := PinRect.Height div 2;
          T := ArcTan2(W*Sin(DegToRad(TAngl)),H*Cos(DegToRad(TAngl)));
          PinCenter.X := Round(PinRect.CenterPoint.X + W * Cos(t));
          PinCenter.Y := Round(PinRect.CenterPoint.Y + H * Sin(t));
          GDIP.DrawLine(GPen, Center.X, Center.Y, PinCenter.X, PinCenter.Y);
          GPen.Free;

          ADeg := TAngl;
          if ADeg > 360 then
            ADeg := ADeg-360;

          BelowY := false;
          RightOfX := false;
          if (ADeg >= 0) and (Adeg <= 180) then {Below Y}
          begin
            BelowY := True;
            if Adeg <= 90 then {Right of X}
              RightOfX := True;
          end
          else
          begin
             if ADeg >= 270 then
               RightOfX := True;
          end;
          Overlaps.FOuter := Writer.GraphRect;
          if BelowY then
           Overlaps.FMoveDir := mdUp
          else
           Overlaps.FMoveDir := mdDown;
          if BelowY then
          begin
            if RightOfX  then
            begin
              X := PinCenter.X;
              Y := PinCenter.Y;
            end
            else
            begin
               X := PinCenter.X - Canvas.TextWidth(S) - 5;
               Y := PinCenter.Y;
            end;
          end
          else
          begin
              Y := PinCenter.Y - Canvas.TextHeight(S);
              if RightOfX then
               X := PinCenter.X
              else
              begin
                X := PinCenter.X - Canvas.TextWidth(S) - 5;
              end;
          end;
          TR.Left := X;
          TR.Top := Y;
          TR.Width := Canvas.TextWidth(S);
          TR.Height := Canvas.TextHeight(S);
          TR := Overlaps.GetRect(TR);
          if Assigned(FOnDrawPieSlice) then
          begin
            FOnDrawPieSlice(Self, ASource.Index, Indx, S, TR, Canvas, Handled);
          end;
          if not Handled then
          begin
           if HasImages then
              DrawImagedText
            else
              Canvas.TextOut(TR.Left, TR.Top, S);
          end;
        end;

      begin
        if Anim and (ShadowPie = 0) then
          Exit
        else if not Anim and (ShadowPie = 0) then
          Exit;
        ItmIndx := ASource.FTrackPies[Indx].FSeriesItmIndex;
        Center := ASource.FTrackPies[Indx].Center;
        TAngl := ASource.FTrackPies[Indx].TextAngle;
        BoundsRect := ASource.FTrackPies[Indx].TextBounds;
        if (BoundsRect.Width <= 0) or (BoundsRect.Height <= 0) then
        begin
          FCanDrawText := false;
          Exit;
        end;
        FCanDrawText := True;
        HasImages := (Writer.FImageSize.cx > 0) and (poAllowImages in Options);
        Canvas.Font.Assign(Font);
        Clr := clBlack;
        if poPinText in Options then
          Clr := Font.Color
        else if KeepFontColor then
          Clr := Font.Color
        else if not KeepFontColor then
        begin
         Clr := ASource.FTrackPies[Indx].Color;
         Clr := InvertColor(Clr);
        end;
        Canvas.Font.Color := Clr;
        Canvas.Brush.Style := bsClear;
        S := '';
        Itm := ASource.FSeriesItems[ItmIndx];
        if Itm.Value = 0 then
          Exit;
        if Chart.Percentages then
          Pst := Itm.Value
        else
          Pst := Itm.Pst;
        if (poPrintNames in Options) and not HasImages then
          S := Itm.Name;
        if (poPrintValues in Options) and not Chart.Percentages then
          S := S + ' ' + FormatNum(Itm.Value, Writer.ValuePrecision, True);

        if (poPrintPercentages in Options) then
        begin
          if (poPrintValues in Options) and not Chart.Percentages then
            S := S + ' ('
          else
            S := S + ' ';
          S := S + FormatNum(Pst, Writer.ValuePrecision) + '%';
          if (poPrintValues in Options) and not Chart.Percentages then
             S := S + ')';
        end;
        if HasImages then
        begin
          TX := Canvas.TextExtent(S);
          TxtHeight := TX.cy;
          TX.cx := TX.cx +  Writer.FImageSize.cx + 3;
          if Writer.FImageSize.cy > TX.cy then
            TX.cy := Writer.FImageSize.cy;
        end
        else
        begin
          TX := Canvas.TextExtent(S);
          TxtHeight := TX.cy;
        end;
        Handled := False;
        if not (poPinText in Options) then
           DrawSliceText
         else
           DrawPinText;
      end;

      procedure DrawDoughnut(PieRect: TRect);
      var
        wd, hd, L, T: integer;
        i : integer;
        R : TRect;

        procedure DrawTitle;
        var
          R : TRect;
          X, Y : integer;
          S : string;
        begin
          if not FCanDrawtext then
           Exit;
           if Anim then
            if not SliceFinished then
              Exit;

           Canvas.Font.Assign(FSeriesTitleFont);
           Canvas.Brush.Style := bsClear;
           R := ASource.DoughnutRect;
           S := ASource.Title;
           X := R.CenterPoint.X - Canvas.TextWidth(S) div 2;
           Y := R.CenterPoint.Y - Canvas.TextHeight(S) div 2;
           Canvas.TextOut(X, Y, S);
           Canvas.Font.Assign(Font);
        end;

      begin
        if ShadowPie = 0 then
          Exit;
        wd := round(PieRect.Width * FDoughnutSize / 100);
        hd := round(PieRect.Height * FDoughnutSize / 100);
        L := ShadowRect.CenterPoint.X - wd div 2;
        T := ShadowRect.CenterPoint.Y - hd div 2 - ShadowSize;
        ASource.FDoughnutRect := Rect(L, T, L+wd, T+hd);

        if not (Style=psDisc) then
        begin
          SetTheColor(Writer.Chart.GraphBGColor);
          GDIP.FillEllipse(GBrush, MakeRect(L, T, wd, hd));
          GDIP.DrawEllipse(GPen, MakeRect(L, T, wd, hd));
          GPen.Free;
          GBrush.Free;
          ASource.FDoughnutRect := Rect(L, T, L+wd, T+hd);
          if (poPrintSeriesTitles in Options)
          and (poPrintTitlesInDoughnut in Options) then
             DrawTitle;
          Exit;
        end;

        R := ASource.FDoughnutRect;
        Inc(R.Top, ShadowSize);
        Inc(R.Bottom, ShadowSize);
        LastAngl := StartAngle;
        SeritIndx := 0;
        for i := 0 to Serit.Count - 1 do
          begin
            Pst := Serit[i].Value;
            a := Pst * 360 / 100;
            SourceAngle := a;

            if Anim and (i = Writer.FAnimInfo.NextItem) then
            begin
              Writer.FAnimInfo.NextAngle := Writer.FAnimInfo.NextAngle +
               Writer.AnimationTuner.AngleIncrement; {Accumulate angle of slice}
              a := Writer.FAnimInfo.NextAngle;
              if Writer.FAnimInfo.NextAngle >= SourceAngle then {End of slice}
              begin
                Break;
              end;
            end;

            DrawPieSlice(Serit, i, a, LastAngl, R, Center,
              Pst + SliceSpacing, True);
            if not Serit[i].FSpace then
            begin
              inc(SeritIndx);
            end;

            if Anim and (i = Writer.FAnimInfo.NextItem) then
             Break
            else
            begin
             LastAngl := LastAngl + a;
             if LastAngl > 360 then
              LastAngl := LastAngl - 360;
            end;
          end;
        Inc(R.Top, ShadowSize);
        SetTheColor(Writer.Chart.GraphBGColor);
        GDIP.FillEllipse(GBrush, MakeRect(R));
        GPen.Free;
        GBrush.Free;
        ASource.FDoughnutRect := R;
        if (poPrintSeriesTitles in Options)
          and (poPrintTitlesInDoughnut in Options) then
            DrawTitle;
      end;

      procedure SetDrawRect;
      var
        L, T: integer;
        Ratio : single;
      begin
        {Draw rect is the rect of the pie. In 3d Draw rect is hosted by the
        ShadowRect which is DrawRect + ShadowSize}
        if R.Width >= R.Height then
        begin
          L := StartPt.X + (FNumPaint - 1) * OuterWidth + (FNumPaint - 1) * Spacing + (FNumPaint - 1);
          T := StartPt.Y;
        end
        else
        begin
          L := StartPt.X;
          T := StartPt.Y + (FNumPaint - 1) * OuterHeight + (FNumPaint - 1) * Spacing +
          FNumPaint * FTitleSpace;
        end;

        DrawRect.Left := L;
        DrawRect.Top := T;
        DrawRect.Right := DrawRect.Left + Wdth;
        DrawRect.Bottom := T + Hght;
        ASource.FPieRect := DrawRect;

        SHadowRect := DrawRect;
        if Style = psDisc then
        begin
          inc(ShadowRect.Bottom, ShadowSize);
        end;

        if ShadowRect.Width = 0 then
         Ratio := 0
        else
          Ratio := Shadowrect.Height / Shadowrect.width;

        {OuterRect is shadow rect shifted down by shadowsize. Shifting is done in
        DrawPieslice. This is necessary to synchronize the centerpoints of
        DrawRect and ShadowRect}
        OuterRect.Left := ShadowRect.Left;
        OuterRect.Top := ShadowRect.Top + ShadowSize;
        OuterRect.Right := ShadowRect.Right;
        OuterRect.Bottom := ShadowRect.Bottom;

        {PinRect is the rect that defines the end points of the pins. Relevant when
        poPinText is checked in Options}
        PinRect.Left := OuterRect.Left - PinMarg;
        PinRect.Top := OuterRect.Top - Round(PinMarg * Ratio);
        PinRect.Right := OuterRect.Right+ PinMarg;
        PinRect.Bottom := OuterRect.Bottom + Round(PinMarg * Ratio);

        if Anim and Writer.FAnimInfo.AnimInit then
        begin
           if (Writer.Chart.GraphBGColor = clSilver)
           or (Writer.Chart.GraphBGColor = clBtnFace) then
             GPen := TGPPen.Create(MakeGPClr(clBlack))
           else
             GPen := TGPPen.Create(MakeGPClr(clSilver));
           GDIP.DrawEllipse(GPen, MakeRect(OuterRect));
           GPen.Free;
           Writer.FAnimInfo.AnimInit := false;
        end;
      end;

    begin { Draw Pie }
      Anim := Writer.InState(stAnimating) and not Writer.InState(stInitAnimation);
      R := Writer.GraphRect;
      ASource.FTrackPies.Clear;
      LastAngl := StartAngle;
      AngleCnt :=  0;
      Serit := nil;

      Serit := TSeriesItems.Create;
      if not TCWPieChart(Writer.Chart).Percentages then
        CalcPst(ASource, ASource.FSeriesItems, Serit)
      else
        AssignItems(ASource, ASource.FSeriesItems, Serit);
      SetDrawRect;
      if SliceSpacing > 0 then
      begin
        CreateSpaces(Serit);
      end;
      Overlaps := TOverlaps.Create(DrawRect, mdUp);
      try
        Err := False;
        ThisItem := 0;
        if ASource.FPieState in [0, 3] then
        begin
          { Draw the slices }
          if ASource.FPieState = 3 then
            ASource.FPieState := 0;
          SeritIndx := 0;
          for i := 0 to Serit.Count - 1 do
          begin
            SliceFinished := false;
            SeriesFinished := false;
            FPaintItemIndex := i;
            Pst := Serit[i].Value;
            a := Pst * 360 / 100;
            SourceAngle := a;
            AngleCnt := AngleCnt + a;
            if Floor(AngleCnt) > 360 then
            begin
              ASource.FPieState := 3;
              Error;
              Break;
            end;

            if Anim and (i = Writer.FAnimInfo.NextItem) then
            begin
              Writer.FAnimInfo.NextAngle := Writer.FAnimInfo.NextAngle +
               Writer.AnimationTuner.AngleIncrement; {Accumulate angle of slice}
              a := Writer.FAnimInfo.NextAngle;
              DrawPieSlice(Serit, i, a, LastAngl, DrawRect, Center,
              Pst + SliceSpacing);
              if (Writer.FAnimInfo.NextAngle >= SourceAngle) and ((ShadowPie = 1) or (Style = psFlat)) then {End of slice}
              begin
                SliceFinished := True;
                ThisItem := i;
                inc(Writer.FAnimInfo.NextItem);
                Writer.FAnimInfo.NextAngle := 0;
                if (Writer.FAnimInfo.NextItem > Serit.Count-1) then
                begin
                 SeriesFinished := True;
                 Writer.FAnimInfo.NextItem := 0;
                 Writer.FAnimInfo.NextAngle := 0;
                end;
                Break;
              end;
            end
            else if not Anim then
             DrawPieSlice(Serit, i, a, LastAngl, DrawRect, Center, Pst + SliceSpacing);

            if not Serit[i].FSpace then
            begin
              inc(SeritIndx);
            end;
            if Anim and (i = Writer.FAnimInfo.NextItem) then
             Break
            else
             LastAngl := LastAngl + a;
          end;
          if (FDoughnutSize > 0) then
          begin
             DrawDoughnut(DrawRect);
          end;
          for i := 0 to ASource.FTrackPies.Count - 1 do
          begin
             if Anim then
             begin
              if not SliceFinished then
                Break;
              if (ASource.FTrackPies[i].FSeriesItmIndex = ThisItem) then
               DrawText(i);
             end
             else
              DrawText(i);
          end;
        end
        else
        begin
          Error;
        end;

      finally
        Serit.Free;
        Overlaps.Free;
      end;

      Writer.ResetCanvas(Self);
    end; { End DrawPie }

  begin { DoDraw }
    DrawPie;
  end;

  procedure ComputeSize;
  { Compute the pie size. Original value cannot be held if space not allows }
  var
    Cnt: integer;
    R: TRect;
    MaxW, MaxH: integer;
    TotWidth, TotHeight: integer;
    F : single;
    i, L : integer;
    s, vs, ps : string;
    PMarg : integer;
    HDist, WDist : integer;
    Pst : single;
  begin
   { Compute max text length. Used with pins}
   OuterWidth := Width;
   OuterHeight := Height;
   WDist := 0;
   HDist := 0;
   MaxText := 0;
   if poPinText in Options then
     PMarg := PinMarg
   else
     PMarg := 0;
    if poPinText in Options then
    begin
      MaxText := -MaxInt;
      for I := 0 to Writer.Series[0].Count-1 do
      begin
        S := '';
        Vs := '';
        Ps := '';

        if Chart.Percentages then
          Pst := Writer.Series[0].Items[i].Value
        else
          Pst := Writer.Series[0].Items[i].Pst;

        if poPrintNames in Options then
          S := S + Writer.Series[0].Items[i].Name + ' ';
        if (poPrintValues in Options) and not Chart.Percentages then
          Vs := FormatNum(Writer.Series[0].Items[i].Value, ValuePrecision, True);
        if poPrintPercentages in Options then
          Ps := FormatNum(Pst, ValuePrecision) + '%';
        if (poPrintValues in Options) and not Chart.Percentages and (poPrintPercentages in Options) then
        begin
          S := S + Vs + ' (' + ps + ')';
        end
        else if (poPrintValues in Options) and not Chart.Percentages then
          S := S + Vs + ' '
        else if poPrintPercentages in Options then
          S := S + Ps + ' ';
        Delete(S, Length(S), 1);
        L := Canvas.TextWidth(S);
        if L > MaxText then
          MaxText := L;
      end;
      WDist := MaxText + PMarg;
      HDist := PMarg;
      if PMarg <> 0 then
       HDist := HDist + Canvas.TextHeight('X');
      OuterWidth := WDist*2 + Width;
      OuterHeight := HDist*2 + Height;
    end;

    Cnt := Chart.VisibleGraphCount(TCWPie);
    R := Writer.GraphRect;
    if R.Width >= R.Height then
    begin
      MaxW := R.Width div Cnt - (Spacing * Cnt);
      MaxH := R.Height - Spacing * 2 + FTitleSpace - ShadowSize;
      F := OuterHeight / OuterWidth;
      if OuterWidth > MaxW then
      begin
        OuterWidth := MaxW;
        OuterHeight := Round(OuterWidth * F);
      end;
      if OuterHeight > MaxH then
      begin
        OuterHeight := MaxH;
        OuterWidth := Round(OuterHeight / F);
      end;
      if OuterHeight < 50 then
      begin
        OuterHeight := 50;
        OuterWidth := Round(OuterHeight / F);
      end;

      F := Height / Width;
      Wdth := OuterWidth-(WDist*2);
      Hght := Round(Wdth * F);
      TotWidth := OuterWidth * Cnt + Cnt * Spacing - Spacing;
      StartPt.X := R.Left + (R.Width - TotWidth) div 2 + WDist;
      StartPt.Y := R.CenterPoint.Y - Hght div 2 + FTitleSpace div 2 -ShadowSize;
    end
    else
    begin
      MaxW := R.Width - (Spacing * Cnt);
      MaxH := R.Height div Cnt - Spacing * 2 - FTitleSpace;
      F := OuterHeight / OuterWidth;
      if OuterHeight > MaxH then
      begin
        OuterHeight := MaxH;
        OuterWidth := Round(OuterHeight / F);
      end;

      if OuterWidth > MaxW then
      begin
        OuterWidth := MaxW;
        OuterHeight := Round(OuterWidth * F);
      end;
      if OuterWidth < 50 then
      begin
        OuterWidth := 50;
        OuterHeight := Round(OuterWidth * F);
      end;

      F := Height / Width;
      Wdth := OuterWidth-(WDist*2);
      Hght := Round(Wdth * F);
      TotHeight := OuterHeight * Cnt + Cnt * (Spacing + FTitleSpace) - Spacing;
      StartPt.Y := R.Top + (R.Height - TotHeight) div 2 - ShadowSize + HDist;
      StartPt.X := R.CenterPoint.X - Wdth div 2;
    end;

  end;

  procedure DrawTitle(SeriesIndex : integer);
  var
    R : TRect;
    S : string;
    X, Y : integer;
  begin
    if ShadowPie = 0 then
      Exit;
    if not FCanDrawtext then
     Exit;
    if Anim then
     if not SliceFinished then
        Exit;

    R := Writer.Series[SeriesIndex].PieTitleRect;
    S := Writer.Series[SeriesIndex].Title;
    Canvas.Font.Assign(SeriesTitleFont);
    X := R.CenterPoint.X - Canvas.TextWidth(S) div 2;
    Y := R.CenterPoint.Y - Canvas.TextHeight(S) div 2;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(X, Y, S);
    Canvas.Font.Assign(Font);
  end;

  function GetNextVisible(AIndex : integer) : integer;
  var
    i : integer;
  begin
    Result := -1;
    for I := AIndex to Writer.Chart.SeriesDefs.Count-1 do
    begin
      if Writer.Chart.SeriesDefs[i].Visible then
      begin
       Result := i;
       Break
      end;
    end;
  end;

begin
  if Writer = nil then
    Exit;
  if not InChart then
    Exit;
  inherited Draw;
  Anim := (Writer.InState(stAnimating));
  SeriesFinished := false;
  Canvas.Font.Assign(Font);
  if Style = psDisc then
    ShadowPie := 0
  else
    ShadowPie := -1;
  if ShadowPie >= 0 then
    ShadowCnt := 2
  else
    ShadowCnt := 1;
  FTitleSpace := GetTitleSpace;
  IsDrawn := false;
  if Anim and (Writer.FAnimInfo.NextSeries = -1) then
  begin
    Writer.FAnimInfo.NextSeries := GetNextVisible(0);
    Writer.FAnimInfo.AnimInit := True;
  end;

  try
    for ShadowLoop := 1 to ShadowCnt do
    begin
     FNumPaint := 0;
     ComputeSize;

     for i := 0 to Writer.Count - 1 do
      if (Series[i].Visible) and InChart(i) then
      begin
        inc(FNumPaint);
        if Anim and not (Writer.FAnimInfo.NextSeries = i) then
          Continue;

        DoDraw(Series[i]);
        IsDrawn := True;
        if (poPrintSeriesTitles in Options) and (FTitleSpace > 0) and (Writer.Series[i].Title <> '') then
          DrawTitle(i);
      end;
      if ShadowPie <> -1 then
       ShadowPie := 1;
    end;
    if not IsDrawn then
      Writer.ClearState(stAnimating)
    else
    if SeriesFinished then
    begin
      Writer.FAnimInfo.AnimInit := True;
      Writer.FAnimInfo.NextSeries := GetNextVisible(Writer.FAnimInfo.NextSeries + 1);
      if (Writer.FAnimInfo.NextSeries = -1) then
      begin
        Writer.ClearState(stAnimating);
      end;
    end;
    if (AnimationPause <> 0) and SliceFinished and Writer.InState(stAnimating) then
    begin
        Writer.FAnimInfo.Paused := True;
    end;

  finally
    FGDIP.Free;
  end;
end;

procedure TCWPie.SetInternalActiveColor(SeriesIndex, ItemIndex : integer; Value : TColor);
var
  S : string;
  Indx : integer;
begin
   S := Writer.Series[SeriesIndex].SeriesItems[ItemIndex].FName;
   Indx := Writer.Chart.Categories.IndexOf(S);
   if Indx <> -1 then
     Writer.Chart.Categories.Items[Indx].FColor := Value
   else
     Writer.Chart.Categories.Items[ItemIndex].FColor := Value;
end;

function TCWPie.GetInternalActiveColor(SeriesIndex, ItemIndex : integer): TColor;
var
  R , G, B : Byte;
  S : string;
  Indx : integer;
begin
  Result := clBlack;
  if Writer = nil then
   Exit;
  if ItemIndex > Writer.Chart.Categories.Count-1 then
  begin
     if csDesigning in ComponentState then
      begin
        R := Random(255);
        G := Random(255);
        B := Random(255);
        Result := RGB(R,G,B);
      end
      else
       Result := clBlack;
  end
  else
  begin
   S := Writer.Series[SeriesIndex].SeriesItems[ItemIndex].FName;
   Indx := Writer.Chart.Categories.IndexOf(S);
   if Indx <> -1 then
     Result := Writer.Chart.Categories.Items[Indx].Color
   else
     Result := Writer.Chart.Categories.Items[ItemIndex].Color;
  end;
end;

function TCWPie.GetActualSize: integer;
var
 i : integer;
begin
  Result := 0;
  if (Writer.Count > 0) then
  for I := 0 to Writer.Count-1 do
  begin
    Result := Series[i].PieRect.Width;
    if Result <> 0 then
      Break;
    {Some series might be invisible. All rects are alike }

  end;
end;

function TCWPie.GetWidth : integer;
begin
  Result := PieSize;
end;

function TCWPie.GetHeight : integer;
begin
  Result := Round(Width * (90-Slope) / 90);
end;

function TCWPie.GetTitleSpace : integer;
var
 i : integer;
 H : integer;
begin
  Result := 0;
  if not (poPrintSeriesTitles in Options) then
    Exit;
  if (DoughnutSize > 0) and (poPrintTitlesInDoughnut in Options) then
    Exit;
  try
   Canvas.Font.Assign(SeriesTitleFont);
   for I := 0 to Writer.Chart.SeriesDefs.Count-1 do
   begin
     if Writer.Chart.SeriesDefs[i].Title <> '' then
     begin
       H := Canvas.TextHeight(Writer.Chart.SeriesDefs[i].Title) + 5;
       if H > Result then
         Result := H;
     end;
   end;
  finally
    Canvas.Font.Assign(Font);
  end;

end;

{ TCWSeriesDef ---------------------------------------------------- }

constructor TCWSeriesDef.Create(Collection: TCollection);
begin
  FVisible := True;
  FDataCache := TStringList.Create;
  inherited;
end;

destructor TCWSeriesDef.Destroy;
begin
  FDataCache.Free;
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
    FValueScale := wSrc.ValueScale;
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
  if ValueScale = vsValueScale1 then
    Result := Writer.FValueAxis
  else
    Result := Writer.FValueAxis2;
end;

procedure TCWSeriesDef.WriteCache;
var
  i : integer;
begin
  for I := 0 to DataCache.Count-1 do
    begin
      Writer.Series[Index].FSeriesItems[i].FValue := StrToFloat(DataCache.ValueFromIndex[i], Fmt);
      Writer.Series[Index].FSeriesItems[i].FName := DataCache.Names[i];
    end;
    Writer.LoadFromCache;
    Writer.Execute;
end;

procedure TCWSeriesDef.SetVisible(const Value : Boolean);
begin
   if Value = FVisible then
    Exit;
   FVisible := Value;
   if Writer = nil then
     Exit;
   if not (csLoading in Chart.ComponentState) then
   begin
    Writer.FActiveValAx := ActualAxis;
    if Writer.ActiveValueScale.ValueSpanFromData then
    begin
      Writer.SetHighLow;
    end;
    Writer.RefreshChart;
    if Writer.ActiveGraph is TCWPie then
      Writer.CorrectPie;
   end;
end;

procedure TCWSeriesDef.SetValueScale(const Value: TValueScaleNumber);
var
 i : integer;
 OneFound : Boolean;
 V : TValueScaleNumber;
begin
  if FValueScale = Value then
    Exit;
  if Value = vsNone then
    Exit;
  V := Value;
  if Value = vsValueScale2 then
  begin
    OneFound := false;
    with Collection as TCWSeriesDefs do
    for I := 0 to Count-1 do
     if (Items[i].ValueScale = vsValueScale1) and (I <> Index) then
       OneFound := True;
    if not OneFound then
      V := vsValueScale1;
  end;
  FValueScale := V;
  if Writer = nil then
    Exit;
  if not (csLoading in Chart.ComponentState) then
   begin
    Writer.RefreshChart;
   end;
end;

procedure TCWSeriesDef.SetColor(const Value : TColor);
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

procedure TCWSeriesDef.SetGraph(const Value: TCWGraph);
var
  i: integer;
begin
  if FGraph = Value then
    Exit;
  if csLoading in Chart.componentState then
  begin
    FGraph := Value;
    if Chart is TCWCategoryBarChart then
    begin
       FValueScale := vsValueScale1
    end;
    Value.FWID := AddToWList(Chart.Writer,  Value);
    Value.FWriter := Chart.Writer;
    Exit;
  end;
  if Value = nil then
    ShowGWError(msg_GraphNotAssigned);
  if (Chart is TCWPieChart) and not (Value is TCWPie) then
  begin
      ShowGWError(msg_PieGeneral);
  end;
  if (Chart is TCWCategoryBarChart) and not (Value is TCWBar) then
  begin
   if (csDesigning in Writer.ComponentState) or (Chart.AlternativeGraph = nil) then
    ShowGWError(msg_BarsOnly);
  end;

  if (Value is TCWPie) and not (Chart is TCWPieChart) then
  {Consider open up, making it possible to mix pies and curces/bars?}
  begin
     if Chart.FAlternativeGraph = nil then
      ShowGWError(msg_PieGeneral);
  end;

  if (Value is TCWCurve) and (Writer <> nil) and not (Writer.FNameAxis.IsXAxis) then
  begin
    ShowGWMessage(msg_HorizontalCurves);
    Exit;
  end;

  Value.FWID := AddToWList(Chart.Writer,  Value);
  Value.FWriter := Chart.Writer;

  Value.FreeNotification(Chart);

  FGraph := Value;
  if Value is TCWPie then
  begin
    with Collection as TCWSeriesDefs do
    for I := 0 to Count-1 do
     begin
       Items[i].FGraph := Value;
       Items[i].FValueScale := vsNone;
     end;
  end
  else if Value is TCWBar then
  begin
    with Collection as TCWSeriesDefs do
    begin
      for I := 0 to Count-1 do
      begin
      if (Items[i].Graph is TCWBar) and not (Items[i].Graph = Value)then
         Items[i].FGraph := Value;
      end;
    end;
  end
  else
  begin
    with Collection as TCWSeriesDefs do
    for I := 0 to Count-1 do
    begin
     if (Items[i].Graph is TCWCurve) and not (Items[i].Graph = Value)then
         Items[i].FGraph := Value;
    end;
  end;
  if (FGraph = nil) and (Writer <> nil) then
  begin
    Writer.Invalidate;
  end;
  if Chart.IsActive then
  begin
      Writer.RefreshChart;
  end;
end;

procedure TCWSeriesDef.SetTitle(const Value : string);
begin
  if Value = FTitle then
   Exit;
  FTitle := Value;
  if Chart.IsActive and not (csLoading in Writer.ComponentState) then
  begin
    if Chart.Legends <> nil then
      Writer.RefreshChart;
  end;
end;

function TCWSeriesDef.GetData : TStringlist;
begin
    if Chart = nil then
     Result := nil
    else
     Result := FDataCache;
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

function TCWSeriesDefs.GetChart : TCWChart;
begin
  Result := Owner as TCWChart;
end;

procedure TCWSeriesDefs.SetItem(AIndex: integer; const Value: TCWSeriesDef);
begin
  inherited SetItem(AIndex, Value);
end;

procedure TCWSeriesDefs.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin

  if TCWChart(Owner).IsActive and (TCWChart(Owner).Writer.Count > 0) then
  begin
    if csDestroying in TCWChart(Owner).ComponentState then
      Exit;
    if (Action =  cnAdded) then
    begin
      TCWChart(Owner).Writer.GoBackError;
      ShowGWError(-1, 'Cannot add series definitions on a running chart.');
    end
    else
     ShowGWError(-1, 'Cannot delete series definitions on a running chart.');
  end;

  if (Action =  cnAdded) and (TCWChart(Owner) is TCWCategoryBarChart) then
    with Item as TCWSeriesDef do
    begin
       FValueScale := vsValueScale1
    end
  else if (Action =  cnAdded) and (TCWChart(Owner) is TCWPieChart) then
   with Item as TCWSeriesDef do
       FValueScale := vsNone;
  inherited;
end;

procedure TCWSeriesDefs.Update(Item : TCollectionItem);
begin
  if (csLoading in TCWChart(Owner).ComponentState)
  or (csDestroying in TCWChart(Owner).ComponentState) then
    Exit;

  if not TCWChart(Owner).IsActive then
   Exit;

  TCWChart(Owner).Writer.RefreshChart;
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

function TCWSeriesDefs.IndexOfGraphType(AGraphType : TClass) : integer;
var
  loop : integer;
begin
  Result := -1;
  loop := 0;
  while (Result = -1) and (loop < Count) do
  begin
    if Items[loop].Graph <> nil  then
    begin
      if Items[loop].Graph is AGraphType then
       Result := loop;
    end
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
  if (Operation = opRemove)
  and ((AComponent is TCWChart) or (AComponent is TCWChart)) then
  begin
    FWriter := nil;
  end;
  inherited;
end;

function TCWSectionDefs.GetWriter : TChartWriter;
begin
  if FWriter <> nil then
    Result := FWriter
  else
   Result := GetWriterFromWList(FWID);
end;

function TCWSectionDefs.GetSectionType: TNameSectionType;
begin
  Result := stUndefined;
  if ClassType = TCWValueSectionDefs then
  begin
    if Sections.Count > 0 then
      Result := stLiterals;
  end
  else
    with Self as TCWNameSectionDefs do
    begin
      if (AutoSections = autNotUsed) and (DateTimeTemplate = ttNotused) then
        Result := stLiterals
      else
       if AutoSections <> autNotUsed then
        Result := stAutoSections
      else if (DateTimeTemplate <> ttNotUsed) then
        Result := stDateTimeTemplate;
    end;
end;

function TCWSectionDefs.GetOwnerChart : TCWChart;
begin
  Result := nil;
  if Writer <> nil then
    Result := Writer.Chart;
end;

procedure TCWSectionDefs.SetCaptionHorzMargin(const Value: integer);
begin
  if Value < 0 then
    Exit;
  if Value > 50 then
    Exit;
  FCaptionHorizMargin := Value;
  if (Writer <> nil) and not(csLoading in Writer.componentState) then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWSectionDefs.SetCaptionVertMargin(const Value: integer);
begin
  if Value < 0 then
    Exit;
  if Value > 50 then
    Exit;
  FCaptionVertMargin := Value;
  if (Writer <> nil) and not (csLoading in Writer.ComponentState) then
    Writer.RefreshChart;
end;

procedure TCWSectionDefs.SetVisible(const Value: Boolean);
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
      if ClassType = TCWNameSectionDefs then
        Writer.ClearSections(stNameScale, False)
      else
        Writer.ClearSections(stValueScale1, False);
    end;
    Writer.RefreshChart;
    if Writer.ActiveGraph is TCWPie then
      Writer.CorrectPie;
  end;
end;

procedure TCWSectionDefs.SetProlongedLines(const Value : Boolean);
begin
  if Value = FProlongedLines then
   Exit;
  FProlongedLines := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWSectionDefs.SetShowLines(const Value : Boolean);
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

procedure TCWSectionDefs.SetCaptionLayout(const Value : TCaptionLayout);
begin
  if Value = FCaptionLayout then
    Exit;
  FCaptionLayout := Value;
  if Writer <> nil then
  begin
   if csLoading in Writer.ComponentState then
     Exit;
   Writer.RefreshChart;
  end;
end;

procedure TCWSectionDefs.AddSection(const StartValue, EndValue,
 LongCaption, ShortCaption : string);
var
  Sect : TCWSectionItem;
begin
   Sect := Sections.Add;
   Sect.FStartValue := StartValue;
   Sect.FEndValue := EndValue;
   Sect.FLongCaption := LongCaption;
   Sect.FShortCaption := ShortCaption;
   try
     DoCheck;
   except
     Sections.Delete(Sections.Count-1);
     raise;
   end;
   if Writer <> nil then
    Writer.RefreshChart;
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

procedure TCWSectionItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
var
  Kind : integer;
begin
  if (Action in [cnDeleting, cnExtracting]) and not (csDestroying in TCWSectionDefs(Owner).ComponentState) then
  begin
    if Owner is TCWNameSectionDefs then
      Kind := 0
    else
      Kind := 1;
    if ((Action = cnExtracting) and (Count = 1)) or (Action = cnDeleting) then
     PostMessage(TCWSectionDefs(Owner).Writer.Handle, WM_REFRESHCHART, Item.Index, Kind);
  end;
end;

procedure TCWSectionItems.Update(Item : TCollectionItem);
begin
   inherited;
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
  Nt: TSpanType;
begin

  if (Writer = nil) or (csLoading in componentState) or
    (SectionType = stLiterals) then
    Exit;
  if Writer.Chart = nil  then
    Exit;
  if not Writer.IsTimeSpan then
    Exit;
  if SectionType = stUndefined then
    Exit;
  Err := False;
  Err2 := False;
  Nt := Writer.Chart.SpanType;

  if SectionType = stDateTimeTemplate then
  begin
    Templ := DateTimeTemplate;
    if Templ = ttNotUsed then
      Exit;
    { These nt spans lack time information }
    if Nt in [ntMonthSpan,ntDateSpan] then
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
    if Nt = ntMonthSpan then
    begin
      case Aut of
        { Not practical, ok with weeks }
        autDays .. autWeeks:
          Err2 := True;
      end;
    end
    else if Nt = ntHourSpan then
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
    ShowGWError(msg_SpanTypeTemplate)
  end
  else if Err2 then
  begin
    ShowGWError(msg_SpanTypeAutoSect);
  end;
end;

procedure TCWNameSectionDefs.SetDateTimeTemplate(const Value: TDateTimeTemplate);
begin
  if FDateTimeTemplate = Value then
    Exit;
  FDateTimeTemplate := Value;
  if FDateTimeTemplate <> ttNotUsed then
    FAutoSections := autNotUsed;
  if Writer <> nil then
    Writer.RefreshChart;
end;

procedure TCWNameSectionDefs.SetAutoSections(const Value: TAutoSections);
begin
  if FAutoSections = Value then
    Exit;
  FAutoSections := Value;
  if FAutoSections <> autNotUsed then
    FDateTimeTemplate := ttNotUsed;
  if Writer <> nil then
  begin
    if FAutoSections = autNotUsed then
      Writer.FNameSections.Clear;
    Writer.RefreshChart;
  end;
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

procedure TCWSectionItem.CheckDataType(const Value: string);
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
  else if Section.Writer.IsTimeSpan
  and (Section.SectionType = stLiterals) then
  begin
      if not TryStrToDateTime(Value, Dt, Fmt) then
        ShowGWError(msg_InvalidDateTime, Value);
  end
  else if Writer.Chart.GetNameType = ntNumberSpan then
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

procedure TCWSectionItem.SetStartValue(const Value: string);
begin
  if (Writer = nil) or (csLoading in Section.componentState) then
  begin
    FStartValue := Value;
    Exit;
  end;
  CheckDataType(Value);
  FStartValue := Value;
  if Writer.Count > 0 then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWSectionItem.SetEndValue(const Value: string);
begin
  if (Writer = nil) or (csLoading in Section.componentState) then
  begin
    FEndValue := Value;
    Exit;
  end;
  CheckDataType(Value);
  FEndValue := Value;
  if Writer.Count > 0 then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWSectionItem.SetLongCaption(const Value : string);
begin
  if FLongCaption = Value then
    Exit;
  FLongCaption := Value;
  if (Writer <> nil) and (Writer.Count > 0) then
    Writer.RefreshChart;
end;

procedure TCWSectionItem.SetShortCaption(const Value : string);
begin
  if FShortCaption = Value then
    Exit;
  FShortCaption := Value;
  if (Writer <> nil) and (Writer.Count > 0) then
    Writer.RefreshChart;
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
      Writer.RefreshChart;
  inherited;
end;

{ TCWLegendItem -------------------------------------------------- }

constructor TCWLegendItem.Create(Collection: TCollection);
begin
  inherited;
end;

procedure TCWLegendItem.SetLegend(const Value: TCWLegend);
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
    Value.FWriter := C.Writer;
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
  Value.FWriter := C.Writer;

  if FLegend <> nil then
  if FLegend.Writer <> nil then
  begin
    FLegend.Writer.RefreshChart;
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
  if (Operation = opRemove) and
  ((AComponent is TCWChart) or (AComponent is TCWChart)) then
  begin
    FWriter := nil;
  end;
  inherited;
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
      if Writer.FNameAxis.IsXAxis then
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

procedure TCWLegend.SetPointItemIndex(const Value: integer);
begin
  if Value < 0 then
    Exit;
  FPointItemIndex := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

function TCWLegend.GetWriter : TChartWriter;
begin
  if FWriter <> nil then
    Result := FWriter
  else
    Result := GetWriterFromWList(FWID);
end;

procedure TCWLegend.SetVisible(const Value: Boolean);
begin
  if Value = FVisible then
    Exit;
  FVisible := Value;
  if not(csLoading in componentState) and (Writer <> nil) then
  begin
    Writer.RefreshChart
  end;
end;

procedure TCWLegend.SetTransparency(const Value: integer);
begin
  if (Value < 0) or (Value > 255) then
    Exit;
  if Value = FTransparency then
    Exit;
  FTransparency := Value;
  if Writer <> nil then
    Writer.DoRepaint;
end;

procedure TCWLegend.SetContents(const Value : TLegendContents);
begin
   if Value = Contents then
     Exit;
   FContents := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.RefreshChart;
end;

procedure TCWLegend.SetContentFlow(const Value : TContentFlow);
begin
   if Value = ContentFlow then
     Exit;
   FContentFlow := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.RefreshChart;
end;

procedure TCWLegend.SetBullets(const Value : TLegendBullets);
begin
   if Value = Bullets then
     Exit;
   FBullets := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.RefreshChart;
end;

procedure TCWLegend.SetBorder(const Value : Boolean);
begin
   if Value = Border then
     Exit;
   FBorder := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.DoRepaint;
end;

procedure TCWLegend.SetAnchoring(const Value : TLegendAnchoring);
begin
   if Value = Anchoring then
     Exit;
   FAnchoring := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.RefreshChart;
end;

procedure TCWLegend.SetAlignment(const Value : TLegendAlignment);
begin
   if Value = Alignment then
     Exit;
   FAlignment := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.RefreshChart;
end;

procedure TCWLegend.SetHorizMargins(const Value : integer);
begin
   if Value = HorizMargins then
     Exit;
   FHorizMargins := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.RefreshChart;
end;

procedure TCWLegend.SetVertMargins(const Value : integer);
begin
   if Value = VertMargins then
     Exit;
   FVertMargins := Value;
   if csLoading in ComponentState then
     Exit;
   if GetOwnerChart <> nil then
     Writer.RefreshChart;
end;


procedure TCWLegend.SetPointSeriesIndex(const Value: integer);
var
  V : integer;
begin
  V := Value;
  if Value < -1 then
    V := -1;
  FPointSeriesIndex := V;
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  if GetOwnerChart <> nil then
    Writer.RefreshChart;
end;

procedure TCWLegend.SetPointValue(const Value: string);
var
  E: single;
begin
  if not TryStrToFloat(Value, E, Fmt) then
    raise Exception.Create('Invalid floating point value');
  FPointValue := Value;
end;

procedure TCWLegend.SetPointName(const Value: string);
var
  V: string;
begin
  if csLoading in ComponentState then
  begin
    FPointName := Value;
    Exit;
  end;
  V := Value;
  V := Trim(Value);
  FPointName := V;
  if GetOwnerChart <> nil then
    Writer.RefreshChart;
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
      if Writer.FNameAxis.IsXAxis then
        Result := apRight
      else
        Result := apBottom;
  end;
end;

procedure TCWLegend.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  if GetOwnerChart <> nil then
    Writer.RefreshChart;
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

  procedure SetBreak;
  begin
    if sl.Count > 0 then
      sl[sl.Count - 1] := '...'
    else
      sl.Add('...');
  end;

begin
  if not Visible then
    Exit;
  if (Anchoring = anSeriesOutside) then
    if not Writer.FNameAxis.IsXAxis then
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

    if (coNameSpan in Elems) and (Writer.IsTimeSpan or
      (Writer.Chart.GetNameType = ntNumberSpan)) then
    begin
      if not PointMatch(i, -1) then
        Continue;

      if Writer.IsTimeSpan then
      begin
        D1 := DateTimeToStr(Writer.FSeriesData[i].FSeriesItems[Writer.FSeriesData[i].FirstItem].RealDate, Fmt);
        D2 := DateTimeToStr(Writer.FSeriesData[i].FSeriesItems[Writer.FSeriesData[i].LastItem].RealDate, Fmt);
        FContentList[i].Add(D1 + ' - ' + D2);
        W := Canvas.TextWidth(D1 + ' - ' + D2);
      end
      else if Writer.Chart.GetNameType = ntNumberSpan then
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

    if Assigned(FOnContent) then
    begin
      S := '';
      FOnContent(Self, i, S);
      if Trim(S) <> '' then
      begin
        W := Canvas.TextWidth(S);
        if (ElemFlow = cfTopBottom) then
        begin
          if W > FWidth then
            FWidth := W;
          FHeight := FHeight + FLineHeight;
        end
        else
          FWidth := FWidth + W + 5;
        FContentList[i].Add(S);
      end;
    end;

    sl := FContentList[i];
    UseItmClrs := Writer.FSeriesData[i].UseCategoryColors;

    if (coName in Elems) or (coValue in Elems) or (Anchoring = anPointInside) then
    begin
      for j := 0 to Writer.FSeriesData[i].FSeriesItems.Count - 1 do
      begin
        if Anchoring = anSeriesOutside then
          if j < Writer.FSeriesData[i].FSeriesItems.Count - 1 then
            Continue;

        if not PointMatch(i, j) then
          Continue;

        if (Anchoring = anPointInside)
        and not (coName in Elems) and not (coValue in Elems)
        then
          Break;

        if not(Anchoring in [anPointInside, anSeriesOutside]) then
        begin
         if Alignment = laLeftOrTop then
         begin
            if (Top + FHeight + FLineHeight > Writer.GraphRect.Bottom) and
              (ElemFlow = cfTopBottom) then
            begin
              SetBreak;
              OverflowVert := True;
              Break;
            end
            else if (Left + FWidth > Writer.GraphRect.Right)
              and (ElemFlow = cfLeftRight) then
            begin
              SetBreak;
              OverflowHorz := True;
              Break;
            end;
         end
         else if Alignment = laRightOrBottom then
         begin
            if (Top - FHeight - FLineHeight < Writer.GraphRect.Top) and
              (ElemFlow = cfTopBottom) then
            begin
              SetBreak;
              OverflowVert := True;
              Break;
            end
            else if (Left - FWidth < Writer.GraphRect.Left)
              and (ElemFlow = cfLeftRight) then
            begin
              SetBreak;
              OverflowHorz := True;
              Break;
            end;
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
            if Chart.ColorUsage = cuOnItems then
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
    Result := clBlack;
    if Chart.FSeriesDefs.Items[SerIndx].Graph is TCWBar then
      with Chart.FSeriesDefs.Items[SerIndx].Graph as TCWBar do
      begin
        if Chart.ColorUsage = cuOnSeries then
          Result := Chart.FSeriesDefs.Items[SerIndx].Color
        else
        begin
          Result := Chart.Categories.Items[ItemIndx].Color;
        end;
      end
    else if Chart.FSeriesDefs.Items[SerIndx].Graph is TCWPie then
      with Chart.FSeriesDefs.Items[SerIndx].Graph as TCWPie do
      begin
        if ItemIndx < Chart.Categories.Count then
          Result := Chart.Categories.Items[ItemIndx].Color;
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
    if not Writer.FNameAxis.IsXAxis then
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
    begin
      if Assigned(FOnOverflow) then
        FOnOverflow(Self);
      Exit;
    end;
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
    Canvas.Pen.Style := psSolid;
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
              if Writer.FNameAxis.IsXAxis then
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

          if PointItemIndex = -1 then
            Continue;
          Points := GetPoints(i);
          P := Points[PointItemIndex];
          if (Chart.FSeriesDefs.Items[i].Graph is TCWBar) and
            (TCWBar(Chart.FSeriesDefs.Items[i].Graph).Layout = blSideBySide) then
          begin
            if not Writer.GetTrackBar(i, PointItemIndex, Tb) then
              Continue;
            if Writer.FNameAxis.IsXAxis then
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

        UseItmClrMode := Writer.FSeriesData[i].UseCategoryColors;
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
            not UseItmClrMode))) and not (FContents = []) then
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
        Canvas.Brush.Style := bsClear;
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
  FSum := ASource.FSum;
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
            Cnt := 1;
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

  if Writer.Chart.GetNameType in [ntGeneral, ntCategory] then
  begin
      Un := Writer.NameFloatUnit;
      X := -1;
      if Writer.FNameAxis.Position = apLeft then
      begin
        StartFrom := Writer.GetPosRect.Top;
      end
      else if Writer.FNameAxis.Position = apTop then
      begin
        StartFrom := Writer.GetPosRect.Left;
      end
      else if Writer.FNameAxis.Position = apBottom then
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
  else if Writer.IsTimeSpan then
  begin
    Dt := StrToDateTime(AName, Fmt);
    if Writer.Chart.GetNameType in [ntMonthSpan,ntDateSpan] then
      X := Writer.PosFromDate(Dt)
    else if Writer.Chart.GetNameType = ntHourSpan then
      X := Writer.PosFromHour(Dt)
    else if Writer.Chart.GetNameType = ntMinuteSpan then
      X := Writer.PosFromMinute(Dt)
    else if Writer.Chart.GetNameType = ntSecondSpan then
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

  Y := Writer.PosFromValue(AVal, TValueAxis(Writer.FActiveValAx));
  if Writer.FActiveValAx.IsXAxis then
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

function TSeries.AsStrings(TimeType : TSaveTimeType; IncludeItemProps: Boolean = False): TStringList;
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
    S := FormatNum(V, 0);
    if Writer.IsTimeSpan then
    begin
      if TimeType = ttUnix then
      begin
        Dt := StrToDateTime(Itm.FOrigName, Fmt);
        UxTime := DateTimeToUnix(Dt);
        STime := IntToStr(UxTime);
      end
      else if TimeType = ttISO8601 then
      begin
        Dt := StrToDateTime(Itm.FOrigName, Fmt);
        STime := DateToISO8601(Dt);
      end
      else {Local}
      begin
        STime := Itm.FOrigName;
      end;
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
  else if Writer.InView(TCWBar) <> nil then
     Result := FBarPoints
  else if Writer.InView(TCWCurve) <> nil then
     Result := FPoints
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

function TSeries.GetUseCategoryColors: Boolean;
begin
   Result := Writer.Chart.ColorUsage = cuOnItems;
end;

function TSeries.GetPieTitleRect: TRect;
begin
  Result := FPieRect;
  Result.Bottom := FPieRect.Top;
  Result.Top := Result.Bottom - TCWPie(Graph).FTitleSpace;
end;

function TSeries.GetEndDate: TDateTime;
begin
  if (ItemCount > 1) and (Items[ItemCount - 1].RealDate <> 0)  then
    Result := Items[ItemCount - 1].RealDate
  else
    Result := FEndDate;
end;

function TSeries.GetStartDate: TDateTime;
begin
  if (ItemCount > 1) and (Items[0].RealDate <> 0)  then
    Result := Items[0].RealDate
  else
    Result := FStartDate;
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
  if not Writer.IsTimeSpan then
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
  if not Writer.IsTimeSpan then
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

function TCWMargins.CheckMargin(const OldValue, NewValue: integer): Boolean;
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
  if not(csLoading in Writer.componentState) and (Writer <> nil) then
  begin
    Writer.RefreshChart;
  end;
end;

procedure TCWMargins.SetLeft(const Value: integer);
begin
  if not CheckMargin(FLeft, Value) then
    Exit;
  FLeft := Value;
  if Writer <> nil then
   UpdateValue;
end;

procedure TCWMargins.SetTop(const Value: integer);
begin
  if not CheckMargin(FTop, Value) then
    Exit;
  FTop := Value;
  if Writer <> nil then
   UpdateValue;
end;

procedure TCWMargins.SetRight(const Value: integer);
begin
  if not CheckMargin(FRight, Value) then
    Exit;
  FRight := Value;
  if Writer <> nil then
  begin
   Writer.FOrigGraphSize.cx := FRight;
   UpdateValue;
  end;
end;

procedure TCWMargins.SetBottom(const Value: integer);
begin
  if not CheckMargin(FBottom, Value) then
    Exit;
  FBottom := Value;
  if Writer <> nil then
  begin
    Writer.FOrigGraphSize.cy := FBottom;
    UpdateValue;
  end;
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

procedure TChartWriter.WMERROR(var Msg: Tmessage);
var
 M : TMsg;
begin
  if (ChartList.FPrevChart.Chart = nil) or
   ((ChartList.FPrevChart.Chart <> nil ) and (ChartList.FPrevChart.Chart <> Chart)) then
    InternalClear;
  if ChartList.FPrevChart.Chart <> nil then
  begin
    ClearState(stOverflow);
    try
      SetChartAs(ChartList.FPrevChart.Chart, ChartList.FPrevChart.Graph);
    except
      PeekMessage(M, Handle, WM_ERROR, WM_ERROR, PM_REMOVE);
      InternalClear;
      Repaint;
      raise;
    end;
  end
  else
    Repaint;
end;

procedure TChartWriter.WMAFTERBUILD(var Msg : TMessage);
begin
   if Assigned(FAfterBuildChart) then
     FAfterBuildChart(Chart);
end;

procedure TChartWriter.WMREFRESHCHART(var Msg : TMessage);
begin
  RefreshChart;
end;

procedure TChartWriter.WMLOADFILE(var Msg: Tmessage);
var
  FName : string;
begin
try
  if FNeedsIds then
  begin
    CreateIds;
    Chart.InnerMargins.FWriter := FChart.Writer;
    Chart.GraphMargins.FWriter := FChart.Writer;
    FNeedsIds := false;
  end;
   if (csDesigning in ComponentState) and not LiveGraphs then
   Exit;

  if (csDesigning in ComponentState) and (Chart.FileName = '')
  and (Chart.Dataset = nil) then
  begin
    RenderDesigner;
    Chart.ClearCache;
    Chart.SaveCache;
    Exit;
  end;

  if Chart.IsCached then
  begin
     Clear;
     LoadFromCache;
     Execute;
     DoRepaint;
     Exit;
  end;

  if (Chart.FileName = '') and (Chart.Dataset = nil) then
  begin
    if Assigned(FOnLoadFile) then
    begin
       FName := '';
       FOnLoadFile(Self, FName);
       Chart.FFileName := FName;
    end;
  end;

  if (Chart.FileName <> '')  then
  begin
    Clear;
    begin
     if not FileExists(Chart.FileName) then
      ShowGWError(msg_NoFile, Chart.FileName);
     if GetFileType(Chart.FileName) <> 1 then
      ShowGWError(msg_RichDesign);
     LoadFromFile(Chart.FileName);
     Chart.SaveCache;
    end;
  end
  else if (Chart.Dataset <> nil)
  and (Chart.NameDBField <> '') and (Chart.ValueDBFields <> '') then
  begin
    Clear;
    begin
     AddSeries(Chart.Dataset, Chart.NameDBField, Chart.ValueDBFields);
     Chart.SaveCache;
     Execute;
     DoRepaint;
    end;
  end
  else if Assigned(Chart) and (FChart.FCWRFileName <> '') then
  begin
    LoadDataFromCWRFile(FChart.FCWRFileName, True); {ReloadChart only}
  end
  else if Assigned(Chart) and Assigned(FChart.FOnGetData) then
  begin
    Clear;
    FChart.FOnGetData(Chart);
    Chart.SaveCache;
  end
  else
    Clear;
  if MSG.WParam = 1 then {when loading}
  begin
   if Assigned(ChartList.FOnChange) then
     ChartList.FOnChange(ChartList);
  end;

 except
   GoBackError;
   raise;
 end;

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

procedure TChartWriter.NWndProc(var Messg: TMessage);
begin
        with Messg do begin
        case Msg of
                WM_EnterSizeMove:
                begin
                   SaveSelBM;
                   FMouseSizing := True;
                end;
                WM_ExitSizeMove:
                begin
                  FMouseSizing := false;
                  RefreshChart;
                end;

        end;{case}
        Result := CallWindowProc(FOldWndHandler, FFormHandle, Msg, wParam, lParam);
        end;{with}
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
  FInternalChartList := TInternalChartList.Create;
  FChartList := TChartList.Create;
  FChartList.FWriter := Self;
  FContractionBase := TData.Create;
  FNameList := TStringList.Create;
  FNameSections := TSections.Create;
  FValueSections := TSections.Create;
  FTrackBars := TTrackBars.Create;
  FHWBM := TBitmap.Create;
  FYears := TYears.Create;
  FMouseInfo := miBoth;
  FRulers := ruNone;
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
  FContractionType := ctExplicit;
  FDynaSectStart := -1;
  FDynaSectEnd := -1;
  FWallAngle := 90;
  FLastMousePos := Point(0, 0);
  FValueAxis := TValueAxis.Create;
  FValueAxis.FWriter := Self;
  FValueAxis2 := TValueAxis2.Create;
  FActiveValAx := FValueAxis;
  FValueAxis2.FWriter := Self;
  FNameAxis := TNameAxis.Create;
  FNameAxis.FWriter := Self;
  FValueAxis.Position := apLeft;
  FValueAxis2.Position := apRight;
  FNameAxis.Position := apBottom;
  FSelBM := Vcl.Graphics.TBitmap.Create;
  FAlfaBM := Vcl.Graphics.TBitmap.Create;
  FLastMouseX := MaxInt;
  FMinContraction := 1;
  FLiveResize := True;
  { Hook into form's windowproc to catch wm_Enter/ExitSizeMove}
  if Owner is TForm then
  begin
    FFormHandle := TForm(Owner).Handle;
    FOldWndHandler := Pointer(GetWindowLong(FFormHandle, GWL_WNDPROC));
    FWndHandlerPtr:= MakeObjectInstance(NWndProc);
    SetWindowLong(FFormHandle, GWL_WNDPROC, longint(FWndHandlerPtr));
  end;

  with AnimationTuner do
  begin
    Delay := 10;
    FastFreq := 0;
    MediumFastFreq := 20;
    MediumSlowFreq := 5;
    SlowFreq := 1;
    AngleIncrement := 0.2;
  end;

  FBall := Vcl.Graphics.TBitmap.Create;
  FBall.Width := 8;
  FBall.Height := 8;
  FBall.Canvas.Brush.Color := RGB(153, 180, 209);
  FBall.Canvas.FillRect(Rect(0, 0, 8, 8));
  Clr := RGB(223, 231, 240);
  FAppEvent := TApplicationEvents.Create(Self);
  FAppEvent.OnMessage := AppMsg;
  { Catching key strokes, so that keydn/keyup is processed even if comp has no focus}
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
  ResetCanvas(nil);
end;

procedure TChartWriter.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FBorder then
    Params.Style := Params.Style or ws_Border;
end;

procedure TChartWriter.SetBorder(const Value: Boolean);
begin
  if Value = FBorder then
    Exit;
  FBorder := Value;
  RecreateWnd;
end;

procedure TChartWriter.SetFixedGraphWidth(const Value: integer);
begin
   if csLoading in ComponentState then
   begin
     FFixedGraphWidth := Value;
     Exit;
   end;
   if Value = FFixedGraphWidth then
     Exit;
   if (Value < 0) or ((Value > 0) and (Value < 100))then
     ShowGWError(msg_FixedGraphWidth);
   FFixedGraphWidth := Value;
end;

procedure TChartWriter.SetFixedGraphHeight(const Value: integer);
begin
   if csLoading in ComponentState then
   begin
     FFixedGraphHeight := Value;
     Exit;
   end;
   if Value = FFixedGraphHeight then
     Exit;
   if (Value < 0) or ((Value > 0) and (Value < 100))then
     ShowGWError(msg_FixedGraphWidth);
   FFixedGraphHeight := Value;
end;

destructor TChartWriter.Destroy;
begin
  FInternalChartList.Free;
  FChartList.Free;
  if csDesigning in ComponentState then
    FDsgnData.Free;
  FValueAxis.Free;
  FNameAxis.Free;
  FValueAxis2.Free;
  FSeriesData.Free;
  FContractionBase.Free;
  FNameList.Free;
  FBall.Free;
  FBallSmall.Free;
  FNameSections.Free;
  FValueSections.Free;
  FYears.Free;
  FTrackBars.Free;
  FSelBM.Free;
  FAlfaBM.Free;
  FHWBM.Free;
  if Owner is TForm then
  begin
    SetWindowLong(FFormHandle, gwl_WndProc, Longint(FOldWndHandler));
    if FWndHandlerPtr <> nil then
     FreeObjectInstance(FWndHandlerPtr);
  end;
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

  if InState(stInternalAction) or ActiveValueScale.ValueSpanFromData then
    Exit;
  if (LowVal > ASeries.FMin) or (HighVal < ASeries.FMax) then
  begin
    if LowVal > ASeries.FMin then
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
  H, L : single;
begin
  Result := True;
  H := -MaxInt;
  L := MaxInt;
  for i := 0 to Count - 1 do
  begin
    if FSeriesData[i].FMax > H  then
      H := FSeriesData[i].FMax ;
    if FSeriesData[i].FMin < L  then
      L := FSeriesData[i].FMin ;
  end;
  if (LowVal > L) or (HighVal < H) then
  begin
    if LowVal > L then
      ErrorNumber := LowVal
    else
      ErrorNumber := HighVal;
    Result := False;
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

procedure TChartWriter.RecomputeHighLowValues;
var
  i, j : integer;
  H, L, S : single;
begin
  H := -Maxint;
  L := MaxInt;
  S := 0;

  for I := 0 to Count-1 do
  begin
    for J := 0 to Series[i].FSeriesItems.Count-1 do
    begin
      if Series[i].FSeriesItems[j].Value > H then
        H := Series[i].FSeriesItems[j].Value;
      if Series[i].FSeriesItems[j].Value < L then
        L := Series[i].FSeriesItems[j].Value;
      S := S + Series[i].FSeriesItems[j].Value;
    end;
    Series[i].FMax := H;
    Series[i].FMin := L;
    Series[i].FSum := S;
    H := -Maxint;
    L := MaxInt;
    S := 0;
  end;
end;

procedure TChartWriter.RefreshChart;
begin
  if Chart = nil then
    Exit;
  if not(csLoading in componentState) and not InState(stAnimating) then
  begin
    FUpdateKinds := FUpdateKinds + [ukRestrict, ukLabelFreq];
    RestrictToClient(False);
    if (FixedGraphWidth = 0) or (FixedGraphHeight = 0) then
      SetLabelFreqs;
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
  Tick : LongWord;
  AP : LongWord;
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
  SaveSelBM;

  if ActiveGraph is TCWBar then
    Boost := TCWAxisGraph(ActiveGraph).AnimationBooster
  else
    Boost := 0;

  if Boost < 0 then
    Delay := Delay + Abs(Boost * 10);

  while InState(stAnimating) do
  begin
    Repaint;
    if (ActiveGraph is TCWPie) or (anGrow in TCWAxisGraph(ActiveGraph).Animations) then
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
    begin
      if (TCWBar(ActiveGraph).AnimationPause <> 0) and FAnimInfo.Paused and
        (TCWBar(ActiveGraph).Layout = blSideBySide) and
        (FAnimInfo.NextHorz < Count * LS.Count) then
      with ActiveGraph as TCWBar do
      begin
       if AnimationPause > 0 then
       begin
         AP := LongWord(AnimationPause);
         Tick := GetTickCount + AP;
         while GetTickCount < Tick do
          begin end;
          // Application.ProcessMessages;
           {ProcessMessages paints a black screen after approx 1000 ms. Why?}
         FAnimInfo.Paused := false;
       end
       else
       begin
        SetState(stAnimationPause);
        SaveSelBM;
        while InState(stAnimationPause) do
        begin
          Application.ProcessMessages; {Manual resume. No black screen here!?}
        end;
       end;
      end;
    end
    else if FAnimInfo.Paused then
    with ActiveGraph do
    begin
       if AnimationPause > 0 then
       begin
         AP := LongWord(AnimationPause);
         Tick := GetTickCount + AP;
         while GetTickCount < Tick do
         begin end;
         FAnimInfo.Paused := false;
       end
       else
       begin
        SetState(stAnimationPause);
        SaveSelBM;
        while InState(stAnimationPause) do
        begin
          Application.ProcessMessages;
        end;
        FAnimInfo.Paused := false;
       end;
    end;
  end;

  ClearState(stAnimationPause);
  ClearState(stResumeAnimation);
  Chart.FHasAnimated := True;
  Repaint;
end;

procedure TChartWriter.InitAnimation;
var
  i : integer;
  Denied : Boolean;
  G : TCWGraph;
begin
  if Chart.HasAnimated then
    Exit;
  if not Chart.AnimationEnabled then
    Exit;
  if Chart.AllEqual = nil then
    Exit;
  if InState(stLimbo) then
    Exit;
  if csDesigning in ComponentState then
    Exit;
  if VisibleCount = 0 then
    Exit;
  G := InView(TCWCurve);
  if G <> nil then
    if TCWCurve(G).LineShape = lsBezier then
      Exit;


  if (ActiveGraph is TCWCurve) then
  with ActiveGraph as TCWCurve do
  begin
     Denied := false;
     for I := 0 to FSeriesStyles.Count-1 do
       if FSeriesStyles.Items[i].Style in [csClientArea, csBaseLineArea, csNeighborArea] then
       begin
         Denied := True;
         Break;
       end;
     if Denied then Exit;
     if Style in [csClientArea, csBaseLineArea, csNeighborArea] then
       Exit;
  end;
  if (ActiveGraph is TCWPie) or (ActiveGraph is TCWCurve) then
    FAnimInfo.NextSeries := -1
  else
    FAnimInfo.NextSeries := 0;
  FAnimInfo.NextHorz := 0;
  FAnimInfo.NextVert := 0;
  FAnimInfo.NextItem := 0;
  FAnimInfo.NextAngle := 0;
  FAnimInfo.LastXY := 0;
  FAnimInfo.Stopped := False;
  FAnimInfo.StartPause := (ActiveGraph is TCWBar);
  SetState(stAnimating);
end;

procedure TChartWriter.CorrectPie;
begin
    with ActiveGraph as TCWPie do
    begin
    {3D pies does not diplay correctly unless this trick is done.
    Unclear reason. Should be corrected i DrawPie.}
      if Style = psDisc then
      begin
        FStyle := psFlat;
        Repaint;
        FStyle := psDisc;
        Repaint;
      end;
    end;
end;

procedure TChartWriter.ClearState(AState: TState);
begin
  Exclude(FStates, AState);
  if AState = stZoomed then
  begin
    Chart.FZoomStart := -1;
    Chart.FZoomEnd := -1;
  end;
end;

procedure TChartWriter.SetState(const AState: TState);
begin
  Include(FStates, AState);
end;

procedure TChartWriter.Unzoom;
var
  PIndex : integer;
  ZItm : TZoomLogItem;
begin
  if Chart = nil then
    Exit;
  if not InState(stZoomed) then
    Exit;
  PIndex := FScrollIndex;
  Chart.FZoomStart := -1;
  Chart.FZoomEnd := -1;
  if Chart.FZoomLog.Count = 1 then
  begin
    Chart.FZoomLog.Clear;
    ClearState(stZoomed);
  end
  else if Chart.FZoomLog.Count > 1 then
  begin
    ZItm := Chart.FZoomLog[Chart.FZoomLog.Count-2];
    Chart.FZoomStart := ZItm.FStart;
    Chart.FZoomEnd := ZItm.FEnd;
    Chart.FZoomStart := ZItm.FStart;
    Chart.FZoomLog.Delete(Chart.FZoomLog.Count-1);
  end;
  FRulerX := -1;
  FRulerY := -1;
  if Chart.FZoomStart = -1 then
   ClearState(stZoomed);
  Reload;

  ScrollIndex := PIndex;
  Chart.ClearCache;
  Chart.SaveCache;
end;

procedure TChartWriter.ResetContraction;
var
  CT : TContractionType;
begin
  if Contraction = FMinContraction then
    Exit;
  CT := FContractionType;
  FContractionType := ctExplicit;
  Contraction := FMinContraction;
  FContractionType := CT;
end;

function TChartWriter.IsContractionIdle : Boolean;
begin
  Result := Contraction = FMinContraction;
end;

function TChartWriter.BaseNameInterval: integer;
var
  Dt1, Dt2: TDateTime;
  Itm1, Itm2: TSeriesItem;
begin
  Result := 1;
  if Chart = nil then
   Exit;
  if Chart.FOrigData.Count = 0 then
    Exit;
  Itm1 := Chart.FOrigData[0].FSeriesItems[0];
  Itm2 := Chart.FOrigData[0].FSeriesItems[1];
  if IsTimeSpan then
  begin
    Dt1 := StrToDateTime(Itm1.FName, Fmt);
    Dt2 := StrToDateTime(Itm2.FName, Fmt);
    case Chart.SpanType of
      ntMonthSpan:
        Result := MonthsBetweenEx(Dt2, Dt1);
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
  if Chart <> nil then
    Chart.FOrigData.Clear;
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
  begin
    DoRepaint;
    Exit;
  end;
  { Prevent user to set this state when OnActivateGraph is executing }
  if not InState(stUpdating) then
    Exit;
  if FUpdateKinds = [] then
  begin
    ClearState(stUpdating);
    DoRepaint;
    Exit;
  end;
  ClearState(stUpdating);
  if ukRestrict in FUpdateKinds then
    RestrictToClient(False);
  if ukLabelFreq in FUpdateKinds then
    SetLabelFreqs;
  if ukScroll in FUpdateKinds then
    DoScrollTo(FScrollIndex);
  FUpdateKinds := [];
  DoRepaint;
end;

procedure TChartWriter.CancelUpdate;
begin
  if not InState(stUpdating) then
    Exit;
  ClearState(stUpdating);
  FUpdateKinds := [];
  DoRepaint;
end;

procedure TChartWriter.GoBackError(DoReload : Boolean = false);
var
  Msg : TMsg;
begin
  PeekMessage(Msg, Handle, WM_ERROR, WM_ERROR, PM_REMOVE);
  if (csDesigning in ComponentState) or not DoReload or (Chart = nil) then
  begin
    LiveGraphs := false;
    InternalClear;
  end
  else if DoReload then {AddSeries errors}
    Chart.Reload
end;

procedure TChartWriter.Reload;
var
  i: integer;
  Ser: TSeries;
  Start, Stop : integer;
  Anim : Boolean;
begin
  if (csLoading in componentState) then
    Exit;
  if Count = 0 then
    Exit;
  FSeriesData.Clear;
  if InState(stZoomed) then
  begin
    Start := Chart.FZoomStart;
    Stop := Chart.FZoomEnd;
  end
  else
  begin
    Start := -1;
    Stop := -1
  end;
  for i := 0 to Chart.FOrigData.Count - 1 do
  begin
    Ser := TSeries.Create;
    Ser.Assign(Chart.FOrigData[i], Start, Stop);
    FSeriesData.Add(Ser);
  end;
  RecomputeHighLowValues;
  Anim := Chart.AnimationEnabled;
  Chart.AnimationEnabled := false;
  FContractionBase.Clear;
  Execute;
  Chart.AnimationEnabled := Anim;

end;

function TChartWriter.IsPointVisible(AScrollIndex: integer): Boolean;
var
  EndPt: integer;
begin
  EndPt := FScrollIndex + FSeriesData[0].FSeriesItems.Count;
  Result := (AScrollIndex <= EndPt) and (AScrollIndex >= FScrollIndex);
end;

function TChartWriter.IsAxisChart(AChart : TCWChart = nil) : Boolean;
begin
   Result := false;
   if AChart = nil then
     AChart := Chart;
   if AChart = nil then
     Exit;
   Result := not (AChart is TCWPieChart);
end;

function TChartWriter.CanScroll(ScrollType: TScrollType): Boolean;
begin
  Result := False;
  if Count = 0 then
    Exit;
  if not Scrollable then
    Exit;
  if csDestroying in componentState then
    Exit;
  if Chart.FOrigData.Count = 0 then
    Exit;
  case ScrollType of
    stNext, stNextPage, stLast:
      Result := not IsPointVisible(Chart.FOrigData[0].FSeriesItems.Count);
    stPrev, stPrevPage, stFirst:
      Result := not IsPointVisible(0);
  end;
end;

function TChartWriter.CanScrollBy(Delta: integer): Boolean;
begin
  Result := False;
  if Count = 0 then
    Exit;
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
  if Count = 0 then
    Exit;
   if Chart.FOrigData.Count = 0 then
    Exit;
  if AScrollIndex = FScrollIndex then
    Exit
  else if not Scrollable then
    Exit
  else if AScrollIndex < 0 then
    Exit
  else if AScrollIndex > FScrollIndex then
  begin
    if IsPointVisible(Chart.FOrigData[0].Count - 1) then
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
  if Count = 0 then
   Result := false
  else
   Result := IsPointVisible(Chart.FOrigData[0].Count - 1) and IsPointVisible(0)
end;

procedure TChartWriter.SetScrollIndex(const Value: integer);
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
    begin
      Result := MinPointSpacing;
    end;
end;

function TChartWriter.GetCurrentPage : integer;
var
  W  : integer;
  ItmSpace : integer;
  ItmsPrPage : integer;
  TotalPages : integer;
begin
  Result := 1;
  if not CanScroll(stNext) and not CanScroll(stPrev) then
    Exit;
  if FNameAxis.IsXAxis then
  begin
    W := GraphRect.Width;
  end
  else
  begin
    W := GraphRect.Height;
  end;
  ItmSpace := GetScrollPointSpacing;
  ItmsPrPage := W div ItmSpace;
  TotalPages := (Chart.FOrigData[0].Count + ItmsPrPage - 1) div ItmsPrPage;
  Result := (ScrollIndex + ItmsPrPage - 1) div ItmsPrPage + 1;
  if Result > TotalPages then
    Result := TotalPages;
end;

function TChartWriter.GetPageCount : integer;
var
  W  : integer;
  ItmSpace : integer;
  ItmsPrPage : integer;
begin
  Result := 1;
  if not CanScroll(stNext) and not CanScroll(stPrev) then
    Exit;
  if FNameAxis.IsXAxis then
  begin
    W := GraphRect.Width;
  end
  else
  begin
    W := GraphRect.Height;
  end;
  ItmSpace := GetScrollPointSpacing;
  ItmsPrPage := W div ItmSpace;
  Result := (Chart.FOrigData[0].Count+ItmsPrPage-1) div ItmsPrPage;
end;

function TChartWriter.GetPageStart : integer;
begin
  Result := ScrollIndex;
end;

function TChartWriter.GetPageEnd : integer;
begin
  Result := FScrollIndex + Series[0].Count-1
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
  if InState(stUpdating) or InState(stLimbo) then
    Exit;
  Chart.FHasAnimated := false;
  FScrolling := True;
  ClearState(stZoomed);
  APointSpacing := GetScrollPointSpacing;
  PIndx := FScrollIndex;
  TheSeries := TList<TSeries>.Create;
  try
    for i := 0 to Chart.FOrigData.Count - 1 do
    begin
      Ser := Chart.FOrigData[i];
      NewSer := TSeries.Create;
      NewSer.FWriter := Self;
      NewSer.FIndent := 0;
      NewSer.FExdent := 0;
      if FNameAxis.IsXAxis then
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
    if Assigned(FOnDataChange) then
      FOnDataChange(Self);
  finally
    TheSeries.Free;
    FScrolling := False;
  end;
  if InState(stAnimating) and (PIndx <> -1) then
      DoAnimation;
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
  if FNameAxis.IsXAxis then
    W := GetPosRect.Width
  else
    W := GetPosRect.Height;
  ScrollTo(Chart.FOrigData[0].Count - floor(W / GetScrollPointSpacing));
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
  if FNameAxis.IsXAxis then
  begin
    W := GraphRect.Width;
  end
  else
  begin
    W := GraphRect.Height;
  end;
  Delta := FScrollIndex + round(W / GetScrollPointSpacing);
  if Delta + round(W / GetScrollPointSpacing) > Chart.FOrigData[0].Count - 1 then
  begin
    Delta := Chart.FOrigData[0].Count - round(W / GetScrollPointSpacing);
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
  if FNameAxis.IsXAxis then
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
  if GraphElement = Img_GraphOnly then
  begin
    RSource := GraphRect;
  end
  else if GraphElement = Img_GraphAndLabels then
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

function TChartWriter.GetAbsoluteItemCount : integer;
begin
  Result := 0;
  if (Chart = nil) or (Count = 0) then
    Exit;
  Result := LongestSeries.ItemCount;
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
      AddSection(stNameScale, SectStart, SectEnd, MN, ShortMonthName(M),
        stSection).FIsAuto := True;
      Exit;
    end;
    ThisD := ThisD + 1;
    DecodeDate(ThisD, Y, M, D);
    FindFirst;
    SectEnd := DateToStr(ThisD, Fmt);
    AddSection(stNameScale, SectStart, SectEnd, MN, SN, stSection)
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
      AddSection(stNameScale, SectStart, SectEnd, MN, SN, stSection)
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
    Y : integer;
    YS : string;

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
      Y := YearOf(LastD);
      YS := Copy(IntToStr(Y), 3, 2);
      AddSection(stNameScale, SectStart, SectEnd, IntToStr(Y), YS,
        stSection).FIsAuto := True;
      Exit;
    end;
    ThisD := ThisD + 1;
    FindFirst(YearOf(ThisD));
    SectEnd := DateToStr(ThisD, Fmt);
    Y := YearOf(ThisD);
    YS := Copy(IntToStr(Y), 3, 2);
    AddSection(stNameScale, SectStart, SectEnd, IntToStr(Y), YS,
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
      Y := YearOf(ThisD);
      YS := Copy(IntToStr(Y), 3, 2);
      AddSection(stNameScale, SectStart, SectEnd, IntToStr(Y), YS,
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
      AddSection(stNameScale, SectStart, SectEnd, WN, IntToStr(WeekOf(LastD)),
        stSection).FIsAuto := True;
      Exit;
    end;
    ThisD := ThisD + 1;
    FindFirst(WeekOf(ThisD));
    WN := WeekName(NW);
    SectEnd := DateTimeToStr(ThisD, Fmt);
    AddSection(stNameScale, SectStart, SectEnd, WN, IntToStr(NW), stSection)
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
      AddSection(stNameScale, SectStart, SectEnd, WN, IntToStr(WeekOf(ThisD)),
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
      AddSection(stNameScale, SectStart, SectEnd, Cap1, Cap2, stSection)
        .FIsAuto := True;
      Exit;
    end;

    WN := DayName(True, ThisD);
    WN2 := DayName(False, ThisD);
    SetCaps(ThisD);
    FindFirst(DayOfTheWeek(ThisD)); { Now we are past the first date }
    SectEnd := DateTimeToStr(ThisD, Fmt);
    AddSection(stNameScale, SectStart, SectEnd, Cap1, Cap2, stSection)
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
      AddSection(stNameScale, SectStart, SectEnd, Cap1, Cap2, stSection)
        .FIsAuto := True;
      SectStart := SectEnd;
    until ThisD >= LastD;
  end;

begin
  if not IsTimeSpan then
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

function TChartWriter.GetDataAttributes(AFileName : string) : TDataAttributes;
var
  sl, sl2 : TStringList;
  s : string;
  Indx, Indx2 : integer;
  i : integer;
  IsGeneral : Boolean;
  LeadingType : TNameType;
  LeadingInterval : integer;
  Cnt : integer;
  Interv : integer;
  DA, DA2 : TDataAttributes;
  HV, LV : single;
  Stop : Boolean;
  Pst : Boolean;
  TT : string;
  P : integer;
begin
   LeadingType := ntGeneral;
   LeadingInterval := 0;
   HV := -Maxint;
   LV := MaxInt;
   DA.ItemCount := 0;
   sl := TStringList.Create;
   sl2 := TStringList.Create;
   try
   sl.DefaultEncoding := TEncoding.Utf8;
   sl2.DefaultEncoding := TEncoding.Utf8;
   sl.LoadFromFile(AFileName);
   s := sl[0];
   if not SameText(S, SignCWD) then
     ShowGWError(msg_InvalidFile);
   sl.Delete(0);
   S := sl[0];
   if S.StartsWith('[TIME TYPE') then
   begin
    S := Trim(S);
    P := Pos('=', S);
    TT := Copy(S, P+1, Length(S)-P-1);
    if SameText(TT, 'UNIX') then
      DA.TimeType := ttUnix
    else if SameText(TT, 'ISO8601') then
      DA.TimeType := ttISO8601
    else
      DA.TimeType := ttLocal;
    sl.Delete(0);
   end;
   Pst := SameText(sl[0], '[PERCENTAGES]');
   if Pst then
    sl.Delete(0);
   DA.ItemAttributes := (sl.IndexOf('[ITEMCOLORS]') <> -1);
   DA.ItemImages := (sl.IndexOf('[IMAGEFILENAMES]') <> -1);
   DA.MainTitle := (sl.IndexOf('[MAINTITLE]') <> -1);
   DA.SeriesColors := (sl.IndexOf('[COLORS]') <> -1);
   DA.SeriesTitles := (sl.IndexOf('[SERIESTITLES]') <> -1);
   Indx := sl.IndexOf('[GRAPHS]');
   if Indx <> -1 then
   begin
     Indx2 := sl.IndexOf('[ENDGRAPHS]');
     if Indx2 <> -1 then
     for I := Indx+1 to Indx2-1 do
     begin
       if SameText(sl[i], 'Curve') then
       begin
        if i = Indx+1 then
          DA.Graph1 := 'Curve'
        else
          DA.Graph2 := 'Curve';
       end
       else if SameText(sl[i], 'Bar') then
       begin
        if i = Indx+1 then
          DA.Graph1 := 'Bar'
        else
          DA.Graph2 := 'Bar';
       end
       else if SameText(sl[i], 'Pie') then
       begin
        if i = Indx+1 then
          DA.Graph1 := 'Pie'
        else
          DA.Graph2 := 'Pie';
       end;
       if I = Indx+1 + 2 then {Only 2 allowed}
        Break;
     end;
   end;

   Indx := sl.IndexOf('[DATA]');
   if Indx <> -1 then
   begin
     for I := 0 to Indx do
       sl.Delete(0);
   end;
   {Now, only data is left}
   Indx := 0;
   IsGeneral := false;
   Cnt := 0;
   Stop := false;
   repeat
     sl2.Clear;
     for I := Indx to sl.Count-1 do
     begin
      Stop := (i = sl.Count-1);
      if sl[i] = '' then
      begin
        Indx := i + 1;
        DA2 := GetSeriesAttributes(sl2, DA.TimeType, Interv);
        DA.NameType := Da2.NameType;
        DA.HighValue := DA2.HighValue;
        DA.LowValue := DA2.LowValue;
        if HV > DA.HighValue then
          DA.HighValue := HV
        else
          HV := DA.HighValue;

        if LV < DA.LowValue then
          DA.LowValue := LV
        else
          LV := DA.LowValue;

        if sl2.Count > DA.ItemCount then
          DA.ItemCount := sl2.Count;

        if IsGeneral then
        begin
          inc(Cnt);
          Continue;
        end;

        if DA.NameType = ntGeneral then
          IsGeneral := True;
        if Cnt = 0 then
        begin
          LeadingType := DA.NameType;
          LeadingInterval := Interv;
        end
        else if (DA.NameType <> LeadingType) or (Interv <> LeadingInterval) then
        begin
          IsGeneral := True;
        end;
        inc(Cnt);
        Break;
      end
      else
       sl2.Add(sl[i]);
     end;
   until Stop;
   DA.SeriesCount := Cnt + 1;
   if (DA.NameType = ntGeneral) and DA.ItemAttributes then
     DA.NameType := ntCategory;
   Result := DA;

   finally
     sl.Free;
     sl2.Free;
   end;
end;

procedure TChartWriter.CreateChartFromDataset(ADataset : TDataset);
begin

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
      and (Chart.NameScale.OverflowAction = ovCompression)
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
  AMax, AMin : single;

  procedure SetMinMax(Indx : integer);
  begin
     AMax := FSeriesData[i].FMax;
     AMin := FSeriesData[i].FMin
  end;

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
     if Itm.ValueScale = vsValueScale1 then
     begin
       if Chart.ValueScale1.ValueSpanFromData
       or not InSpan(Series[i],Chart.ValueScale1.FValueLow, Chart.ValueScale1.FValueHigh, ErrNumber) then
       begin
         SetMinMax(I);
         if AMax > H1 then
           H1 := AMax;
         if AMin < L1 then
           L1 := AMin;
         DoH1 := True;
       end
     end
     else
     begin
       if Chart.ValueScale2.ValueSpanFromData
       or not InSpan(Series[i],Chart.ValueScale2.FValueLow, Chart.ValueScale2.FValueHigh, ErrNumber) then
       begin
         SetMinMax(i);
         if AMax > H2 then
           H2 := AMax;
         if AMin < L2 then
           L2 := AMin;
         DoH2 := True;
      end;
     end;
   end;

   if DoH1 then
   begin
       Chart.ValueScale1.FValueHigh := H1;
       Chart.ValueScale1.FValueLow := L1;
   end;
   if DoH2 then
   begin
       Chart.ValueScale2.FValueHigh := H2;
       Chart.ValueScale2.FValueLow := L2;
   end;
   if Chart.ValueScale1.ScaleRounding and Chart.ValueScale1.ValueSpanFromData then
   begin
      Chart.ValueScale1.FValueHigh := Ceil(Chart.ValueScale1.FValueHigh);
      Chart.ValueScale1.FValueLow := Floor(Chart.ValueScale1.FValueLow);
   end;
   if Chart.ValueScale2.ScaleRounding and Chart.ValueScale2.ValueSpanFromData then
   begin
      Chart.ValueScale2.FValueHigh := Ceil(Chart.ValueScale2.FValueHigh);
      Chart.ValueScale2.FValueLow := Floor(Chart.ValueScale2.FValueLow);
   end;

end;

procedure TChartWriter.Execute;
var
  i, j: integer;
  LastSerInd: integer;
  LastYear: integer;
  Ser: TSeries;
  SerItm: TSeriesItem;
  St : TSpanType;

  procedure CheckFlow(Items: TSeriesItems);
  var
    i: integer;
    LastD: TDateTime;
    LastN: integer;
    Dt: TDateTime;
    n: integer;
  begin
    if (Chart.GetNameType in [ntGeneral, ntCategory]) then
      Exit;
    if FNumberInterval = 0 then
      Exit;
    try
      LastD := 0;
      LastN := 0;
      if IsTimeSpan then
        LastD := StrToDateTime(Items[0].FName, Fmt)
      else
        LastN := StrToInt(Items[0].FName);

      for i := 1 to Items.Count - 1 do
      begin
        if Chart.GetNameType = ntDateSpan then
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
        else if Chart.GetNameType = ntMonthSpan then
        begin
          Dt := StrToDate(Items[i].FName, Fmt);
          if Dt < LastD then
          begin
            ShowGWError(msg_GreaterPred, '(Date ' + Items[i].FName + ')');
          end;
          if (MonthsBetweenEx(Dt, LastD) <> FNumberInterval) and
            not InState(stInternalAction) then
            ShowGWError(msg_DateIntervals);
          LastD := StrToDate(Items[i].FName, Fmt)
        end
        else if IsTimeSpan then
        begin
          Dt := StrToDateTime(Items[i].FName, Fmt);
          if Dt < LastD then
          begin
            ShowGWError(msg_GreaterPred, '(Date ' + Items[i].FName + ')');
          end;
          if Chart.GetNameType = ntHourSpan then
            n := HoursBetween(Dt, LastD)
          else if Chart.GetNameType = ntMinuteSpan then
            n := MinutesBetween(Dt, LastD)
          else
            n := SecondsBetween(Dt, LastD);

          if (n <> FNumberInterval) and not InState(stInternalAction) then
            ShowGWError(msg_DateIntervals);
          LastD := StrToDateTime(Items[i].FName, Fmt)
        end
        else if Chart.GetNameType = ntNumberSpan then
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
           ShowGWError(msg_TitleNotFound, FSeriesData[I].FLinkTitle);
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
  ClearState(stOverflow);
  if (FSeriesData.Count = 0) then
  begin
    Clear;
    ShowGWError(msg_NoSeries);
  end;

  if (Chart is TCWCategoryBarChart) then
  begin
     if (Chart.NameScale.OverflowAction in [ovCompression, ovContraction]) then
       ShowGWError(msg_GeneralCompression);
  end
  else if (Chart is TCWGeneralChart) then
  begin
     if (Chart.NameScale.OverflowAction = ovContraction) then
       ShowGWError(msg_ContractionDenied);
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

    {Set item names if empty}
    if Chart.Categories.Count > 0 then
    begin
     for i := 0 to Chart.Categories.Count-1 do
     begin
       if (Chart.Categories.Items[i].FCategoryName = '') and (i < FSeriesData[0].FSeriesItems.Count) then
       begin
         Chart.Categories.Items[i].FCategoryName := FSeriesData[0].FSeriesItems[i].FName;
       end;
     end;
    end;

    LinkTitles;

    FLeapdateCount := 0;
    if not FScrolling then
      for i := 0 to FSeriesData.Count - 1 do
      begin
        if FSeriesData[i].Count = 1 then
        begin
          { Series must at least contain two values}
          ShowGWError(msg_TwoValues);
        end;
      end;

    St := ntDateSpan;
    if FSeriesData.Count > 0 then
     St := DetectSpanType(0);

    for i := 0 to FSeriesData.Count - 1 do
    begin
      Ser := FSeriesData[i];
      if i > 0 then
        if DetectSpanType(i) <> St then
        begin
          ShowGWError(msg_SpanTypeConflict);
        end;

      CheckFlow(Ser.FSeriesItems);
      if Chart.GetNameType in [ntDateSpan] then
      begin
        NormaliseDates(Ser, i);
      end
      else if not (Chart.GetNameType in [ntGeneral, ntCategory]) then
        for j := 0 to FSeriesData[i].Count - 1 do
        begin
          SerItm := FSeriesData[i].FSeriesItems[j];
          SerItm.FVisible := True;
        end;
    end;

    if FLeapdateCount > 0 then
      InsertLeapDummies;

    FLongestSeries := LongestSeries;

    CreateSpan;
    if Chart.GetNameType = ntDateSpan then
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

    if (FContractionBase.Count = 0) and (Chart.GetNameType <> ntCategory) then
    begin
      if InState(stZoomed) then
        for i := 0 to FSeriesData.Count - 1 do
        begin
          Ser := TSeries.Create;
          Ser.Assign(FSeriesData[i], -1, -1);
          FContractionBase.Add(Ser);
        end
        else
        for i := 0 to Chart.FOrigData.Count - 1 do
        begin
          Ser := TSeries.Create;
          Ser.Assign(Chart.FOrigData[i], -1, -1);
          FContractionBase.Add(Ser);
        end;
    end;
    RestrictToClient(False);
    SetLabelFreqs;
    ClearState(stUpdating);
    FInSerInd := -1;
    FInItemInd := -1;
    FRulerVisible := False;
    CancelBeacons;
    if (InView(TCWCurve) <> nil) and not FNameAxis.IsXAxis then
    begin
      ShowGWMessage(msg_HorizontalCurves);
      Chart.FAxisOrientation := alBottomLeft;
      SetPosition(alBottomLeft);
    end;
    if Assigned(FOnDataChange) and
     (not Chart.IsCached or (Chart.NameScale.OverflowAction = ovScrolling)) then
      FOnDataChange(Self);
    Invalidate;
  except
    raise;
  end;
end;

procedure TChartWriter.WMMouseLeave(var Message: TWMMouse);
begin
  FHintText := '';
  Repaint;
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

function TChartWriter.PointsFromRuler(Vertical: Boolean; X, Y: integer;
var APos: TPoint; var SeriesDefs: TSeriesInfoItems): Boolean;
{ Finds all points along the name axis that are within a given space of xy }
var
  i, j, k: integer;
  Ser: TSeries;
  Gr: TRect;
  Pt: TPoint;
  UnInt: integer;
  TempDefs: TSeriesInfoItems;
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
  S : string;
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

    if ViewMode = vmSelecting then
      MI := miName
    else
      MI := MouseInfo;
    try
      if ItmInd > FSeriesData[SerInd].FSeriesItems.Count - 1 then
        ItmInd := FSeriesData[SerInd].FSeriesItems.Count - 1;
      SerItm := FSeriesData[SerInd].FSeriesItems[ItmInd];
      if ActiveGraph is TCWPie then
        Prec := TCWPie(ActiveGraph).ValuePrecision
      else if Chart.SeriesDefs[SerInd].FValueScale = vsValueScale1 then
        Prec := Chart.ValueScale1.ValuePrecision
      else
        Prec := Chart.ValueScale2.ValuePrecision;
    except
      Exit;
    end;
    case MI of
      miName:
        if IsTimeSpan then
          FHintText := GetTimeStr(Self, MouseTimeFormat, SerItm.RealDate)
        else if (Chart.GetNameType = ntNumberSpan) and (Chart.NameScale.NumSpanPrecision > 0) then
        begin
          FHintText := GetNumPrecisionString(SerItm.FName, Chart.NameScale.NumSpanPrecision);
        end
        else
          FHintText := GetMultiLine(SerItm.FName);
      miValue:
        FHintText := FormatNum(SerItm.Value, Prec, True);
      miBoth:
        begin
          if IsTimeSpan then
            S := GetTimeStr(Self, MouseTimeFormat, SerItm.RealDate)
          else if (Chart.GetNameType = ntNumberSpan) and (Chart.NameScale.NumSpanPrecision > 0) then
          begin
            S := GetNumPrecisionString(SerItm.FName, Chart.NameScale.NumSpanPrecision);
          end
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
    if (G <> nil) then
    begin
      FHintColor := G.InternalActiveColor[SerInd, ItmInd];
    end;
  end;

begin
  Result := False;
  if Chart = nil then
    Exit;
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
  if Chart = nil then
    Exit;
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
    if FNameAxis.IsXAxis then
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
    if (FNameAxis.IsXAxis) then
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
  if Chart= nil then
    Exit;
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
  if Chart.SpanType <> ntDateSpan then
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
  if Chart = nil then
    Exit;
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
  if FNameAxis.IsXAxis then
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
  if Chart = nil then
    Exit;
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
  if csDesigning in ComponentState then
    Exit;
  if Chart = nil then
    Exit;
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
  end
  else if InState(stAnimating) then
  begin
    if Key = vk_Escape then
    begin
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
var
 AnimEnabl : Boolean;
begin
  if (FMouseSizing and not LiveReSize) and not (ActiveGraph is TCWPie) then
    Exit;
  if (FixedGraphWidth <> 0) and (FixedGraphHeight <> 0) then
  begin
    inherited;
    Exit;
  end;
  if (Chart = nil) or (VisibleCount = 0) then
  begin
    inherited;
    Exit;
  end;

  AnimEnabl := Chart.AnimationEnabled;
  Chart.AnimationEnabled := false;
  try
  if not (csLoading in ComponentState) then
  begin
    if ActiveGraph is TCWBar then
     TCWBar(ActiveGraph).FBarWidth := TCWBar(ActiveGraph).FOrigBarWidth;
    RestrictToClient(False);
    if not InState(stLimbo) then
      SetLabelFreqs;
    DoRepaint;
    if Chart <> nil then
     if (Chart.NameScale.OverflowAction = ovScrolling) then
       DoScrollTo(FScrollindex);
  end;
  finally
    Chart.AnimationEnabled := AnimEnabl;
  end;
  inherited;
end;

procedure TChartWriter.Loaded;
begin
  if (csDesigning in ComponentState) and (Chart <>nil) then
  begin
    CreateIds;
    Chart.InnerMargins.FWriter := FChart.Writer;
    Chart.GraphMargins.FWriter := FChart.Writer;
  end;
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
    Indx := InternalChartList.IndexOf(AComponent as TCWChart);
    if Indx <> -1 then
    begin
      InternalChartList.Delete(Indx);
      ChartList.Organize;
    end;
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
  Invalidate;
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
  AnimEnabl : Boolean;
  ZItm : TZoomLogItem;
begin
  if (ActiveGraph is TCWPie) then
  begin
    Exit;
  end;
  if InState(stLimbo) then
    Exit;
  StInd := 0;
  NdInd := 0;
  FViewMode := vmNormal;
  FHintText := '';
  Series := TList<TSeries>.Create;
  AnimEnabl := Chart.AnimationEnabled;
  Chart.AnimationEnabled := false;
  try
  for i := 0 to FSeriesData.Count - 1 do
  begin
    Ser := FSeriesData[i];
    NewSer := TSeries.Create;
    NewSer.FLeapdateCount := Ser.FLeapdateCount;
    NewSer.FWriter := Self;
    NewSer.FIndent := 0;
    NewSer.FExdent := 0;

    TheSer := Chart.FOrigData[i];
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
    Chart.FZoomStart := StInd;
    Chart.FZoomEnd := NdInd;
    ZItm.FStart := StInd;
    ZItm.FEnd := NdInd;
    ZItm.FEnd := NdInd;
    FContractionBase.Clear;
    Chart.ValueScale1.FValueIntervals := 1;
    Execute;
    Chart.FZoomLog.Add(ZItm);

  finally
    ClearState(stInternalAction);
    FScrollIndex := PIndex;
  end;
  Series.Free;
  FRulerX := -1;
  FRulerY := -1;

  if ActiveGraph is TCWBar then
  begin
    DoRepaint;
  end
  else
    Repaint;
  finally
    Chart.AnimationEnabled := AnimEnabl;
  end;
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

function TChartWriter.IsAnimating : Boolean;
begin
  Result := InState(stAnimating) or InState(stInitAnimation);
end;

function TChartWriter.IsCompressed : Boolean;
begin
  Result := FCompressed;
end;

function TChartWriter.ComputeTextPos(X, Y: integer; ALabel: string;
AnAxis: TAxisObject): TPoint;
var
  R: TRect;
  AFont: TFont;
  LabelKind: TLabelKind;
  n: integer;
begin
  R := GraphRect;
  if AnAxis is TNameAxis then
  begin
    LabelKind := lkName;
    AFont := Chart.NameScale.Font;
  end
  else
  begin
    if AnAxis is TValueAxis2 then
    begin
      LabelKind := lkValue2;
      AFont := Chart.ValueScale2.Font
    end
    else
    begin
      LabelKind := lkValue;
      AFont := Chart.ValueScale1.Font
    end;
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
        if AnAxis.Writer.Chart.NameScale.Qualifier <> '' then
         Y := Y - Canvas.TextHeight(AnAxis.Writer.Chart.NameScale.Qualifier) - c_QualifierMargin * 2;
      end
      else if AFont.Orientation = 450 then
      begin
        n := GetTextDiag(Canvas, ALabel);
        n := round(sqrt(n * n / 2));
        Y := AnAxis.FLabelTextRect.Top + n - c_HookSpace ;
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
        Y := AnAxis.FLabelTextRect.Bottom;// - 2;
      end;
    end;

    X := X- GetTextWidth(LabelKind, ALabel) div 2;
    if (AFont.Orientation = 0) then
      Result := Point(X , Y)
    else
    begin
      //Y := Y + c_HookMargin;
      Result := Point(X, Y);
    end;

    { If falls outside client area, try to press it inside }
    if AnAxis is TNameAxis then
    begin
      if (X <= CWBoundsRect.Left) and (FNameUnit > Canvas.TextWidth(ALabel) + 3)
      then
        X := CWBoundsRect.Left
      else if (X >= CWBoundsRect.Right - Canvas.TextWidth(ALabel)) and
        (FNameUnit > Canvas.TextWidth(ALabel) + 3) then
        X := CWBoundsRect.Right - Canvas.TextWidth(ALabel) - 3;
      Result := Point(X, Y);
    end;
  end
  else {YAxis}
  begin
    if AnAxis.Position = apRight then
    begin
      if AnAxis.Position = apRight then
        X := AnAxis.FLabelTextRect.Left
      else
       X := AnAxis.FLabelTextRect.Right - GetTextHeight(LabelKind)
       - GetTextWidth(LabelKind, ALabel) - c_QualifierMargin - c_HookSize
    end
    else
    begin
      if AnAxis.Position = apRight then
        X := AnAxis.FLabelTextRect.Left
      else
        X := AnAxis.FLabelTextRect.Right - GetTextWidth(LabelKind, ALabel);
    end;
    Y := Y- GetTextHeight(LabelKind) div 2;
    //Result := Point(X, (Y - GetTextHeight(LabelKind) div 2));
    Result := Point(X, Y);

    if AnAxis is TNameAxis then
    begin
      if (Y <= CWBoundsRect.Top) and (FNameUnit > Canvas.TextHeight(ALabel) + 3)
      then
        Y := CWBoundsRect.Top
      else if (Y >= CWBoundsRect.Bottom - Canvas.TextHeight(ALabel)) and
        (FNameUnit > Canvas.TextHeight(ALabel) + 3) then
        Y := CWBoundsRect.Bottom - Canvas.TextHeight(ALabel) - 3;
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
    if FNameAxis.IsXAxis then
    begin
      Diff := ThisPt.X - LastPt.X
    end
    else
      Diff := ThisPt.Y - LastPt.Y;
    DoDiff := True;
    if (Diff <> Space) and DoDiff then
    begin
      Diff := Space - Diff;
      if FNameAxis.IsXAxis then
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
    if FNameAxis.IsXAxis then
      MinSpace := ThisPt.X - LastRealPt.X
    else
      MinSpace := ThisPt.Y - LastRealPt.Y;
    if (MinSpace < Space) and (MinSpace <> 0) then
    begin
      Space := MinSpace;
      Result := False;
    end;
  end;

begin
  if ActiveGraph is TCWPie then
    Exit;
  if InView(TCWBar) = nil then
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if Chart.FSeriesDefs.Items[i].ValueScale = vsValueScale1 then
        FActiveValAx := FValueAxis
      else
        FActiveValAx := FValueAxis2;
      FSeriesData[i].FPoints.Clear;
      for j := FSeriesData[i].FirstItem to FSeriesData[i].LastItem do
      begin
        FSeriesData[i].FPoints.Add(FSeriesData[i].PosFromNamVal(FSeriesData[i].FSeriesItems[j]
          .FName, FSeriesData[i].FSeriesItems[j].Value));
        if J > 0 then
      end;
    end
  else
  begin
    Un := NameFloatUnit;
    Adds := 0;

    TCWBar(ActiveGraph).GetBarSpace(Space, Cpr, B);
    for i := 0 to FSeriesData.Count - 1 do
    begin
      if Chart.FSeriesDefs.Items[i].ValueScale in [vsValueScale1, vsNone] then
        FActiveValAx := FValueAxis
      else
        FActiveValAx := FValueAxis2;
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

    if (Adds <> 0) and not Recalc then
    begin
      if FNameAxis.IsXAxis then
        FRightSpace := FRightSpace - Adds
      else
        FBottomSpace := FBottomSpace - Adds;
      SetLabelFreqs(False);
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
  Start, Stop : integer;
begin
  if (Chart.GetNameType = ntCategory) or
    InState(stInternalContraction) or Scrollable or
    InState(stUserContraction) then
  begin
    Exit;
  end;

  if FSeriesData[0].FSeriesItems.Count = Chart.FOrigData[0].FSeriesItems.Count then
    Exit;

  if InState(stZoomed) then
  begin
    Start := Chart.FZoomStart;
    Stop := Chart.FZoomEnd;
  end
  else
  begin
    Start := -1;
    Stop :=  -1
  end;
  FSeriesData.Clear;
  for i := 0 to Chart.FOrigData.Count-1 do
  begin
    OrigSer := TSeries.Create;
    OrigSer.Assign(Chart.FOrigData[i], Start, Stop);
    FSeriesData.Add(OrigSer);
  end;
  RecomputeHighLowValues;

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
  G: TCWAxisGraph;
  HasContracted: Boolean;
  Compr : Boolean;

  function GetContraction(Axis: TScaleType): integer;
  var
    ThisCount: single;
    Spacing: integer;
    NewCount: single;
    MaxSpace: integer;
  begin
    Result := 1;
    MaxSpace := AMaxSpace;
    if Axis = stNameScale then
    begin
      ThisCount := FNameList.Count;
      Spacing := G.MinPointSpacing;
      NewCount := MaxSpace / Spacing;
      if NewCount <= 0 then
      begin
        SetState(stLimbo);
        Exit;
      end;
      Result := Ceil(ThisCount / NewCount);
      if LongestSeries.FSeriesItems.Count div Result < 2 then
        SetState(stLimbo);
    end;
  end;

  procedure SetSpaces(IsValue: Boolean);
  begin
    if IsValue then
    begin
       Ax := FValueAxis;
    end
    else
      Ax := FNameAxis;
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
    VS : TCWValueScale;
    W : single;
  begin
    Result := True;
    for I := 0 to Chart.AxisCount-1 do
    begin
      if I = 0 then
      begin
       FActiveValAx := FValueAxis;
       VS := Chart.FValueScale1;
      end
      else
      begin
       FActiveValAx := FValueAxis2;
       VS := Chart.FValueScale2;
      end;

      SetSpaces(True);
      Cnt := ValueCount;
      if Cnt = 0 then
      begin
        Result := False;
        SetState(stLimbo);
        Exit;
      end;
      VS.FValueUnit := Trunc(AMaxSpace / Cnt);
      if VS.FValueUnit <= 1 then
      { Increase intervals to make it fit}
      begin
        W := AMaxSpace / GetTextHeight(lkValue);
        {Use text height instead of value count. This is not quite exact,
        but using count slows down DrawLabels when value span is big.}
        if W = 0 then
        begin
          SetState(stLimbo);
          Exit;
        end;
        W := ActiveValueScale.ValueCount / W;
        if W < 0 then
          W := 1
        else if
          W < ActiveValueScale.ValueIntervals then
            W := ActiveValueScale.ValueIntervals;
        ActiveValueScale.FValueIntervals := W;

        if ActiveValueScale.ValueCount > 0 then
          VS.FValueUnit := Round(AMaxSpace / ActiveValueScale.ValueCount)
        else
          VS.FValueUnit := 0;

        if VS.FValueUnit <= 1 then
            SetState(stLimbo);
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
      if FNameAxis.IsXAxis then
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
  begin
    Result := (Width < 50) or (Height < 50);
  end;

  procedure DoQuery;
  var
    G : TCWAxisGraph;
    QR : integer;
    Response : TSpaceResponse;
    Auto : Boolean;
  begin
    if Scrollable then
      Exit;
    Auto := false;
    G := ActiveGraph as TCWAxisGraph;
    if G is TCWCurve then
      if (Chart.NameScale.OverflowAction in [ovCompression, ovContraction])
      or (TCWCurve(G).MinPointSpacing = 0) then
        Exit;

    if G is TCWBar then
    begin
       TCWBar(G).FBarWidth := TCWBar(G).FOrigBarWidth;
       Auto := TCWBar(G).AutoSize;
       if not Auto then
        if (Chart.NameScale.OverflowAction in [ovCompression, ovContraction])
        or TCWBar(G).Compressing then
        begin
          Exit;
        end;
    end;

    if (G is TCWCurve) and (TCWCurve(ActiveGraph).MinPointSpacing = 0) then
      Exit;
    Response := srRefuseExcept;
    QR := G.QuerySpace;
    if QR = 0 then
      Exit;
    if G is TCWBar then
    begin
      if not Auto then
      begin
       if (QR > TCWBar(G).FBarWidth)  then
        QR := 0;
      end
      else
      begin
        if QR <> -1 then
        begin
          TCWBar(G).FBarWidth := QR;
          Exit;
        end
        else
        begin
          if (Chart.NameScale.OverflowAction in [ovCompression, ovContraction])
          or TCWBar(G).Compressing then
            Exit;
        end;
      end;
    end
    else
    begin
      if QR > TCWCurve(G).FMinPointSpacing then
        QR := 0;
    end;
    if (QR <> 0) and Assigned(FOnQuerySpace) then
    begin
       if QR = -1 then
         SetState(stOverflow);
       FOnQuerySpace(G, QR, Response);
    end;

    if QR = 0 then
      Exit;

    if (Response = srAccept) and not (QR = -1) then
    begin
     if G is TCWBar then
      TCWBar(G).FBarWidth := QR
     else
      TCWCurve(G).FMinPointSpacing := QR;
    end
    else if Response = srRefuseExcept then
    begin
     SetState(StOverflow);
     GoBackError;
     ShowGWError(msg_NoSpace);
    end
    else
    begin
       SetState(StOverflow);
       GoBackError;
       Abort;
    end;
  end;

{ Computes the base for the graphrect.
  The unit sizes ar in whole numbers, though float values are used in actutal
  processing. }

begin
  if InState(stUpdating) then
    Exit;
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
  begin
    if (ActiveGraph is TCWPie) and TCWPie(ActiveGraph).Animation then
      InitAnimation;
    Exit;
  end;
  Canvas.Font.Assign(Font);

  FRightSpace := FOrigGraphSize.cx;
  FBottomSpace := FOrigGraphSize.cy;
  if FUserContraction > 0 then
    SetState(stUserContraction);
  AssignOrigData; {Sets contraction to 1, unless user contraction}
  ClearState(stUserContraction);

  CreateSections;
  Chart.CreateLegendContent;

  if (FixedGraphWidth > 0) and (FixedGraphHeight > 0) then
  begin
    Exit;
  end
  else if (FixedGraphHeight > 0) and (FixedGraphWidth = 0) then
  begin
    Chart.ValueScale1.FValueIntervals := Chart.ValueScale1.FUserIntervals;
    Chart.ValueScale2.FValueIntervals := Chart.ValueScale2.FUserIntervals;
    CheckValueSpace;
    FPosRect := GetPosRect;
    PostMessage(Handle, WM_AFTERBUILD, 0, 0);
    Exit;
  end;

  if not (csDesigning in ComponentState) or
   ((ActiveGraph is TCWBar) and TCWBar(ActiveGraph).AutoSize) then
    DoQuery;

  Chart.ValueScale1.FValueIntervals := Chart.ValueScale1.FUserIntervals;
  if not (Chart is TCWCategoryChart) then
    Chart.ValueScale2.FValueIntervals := Chart.ValueScale2.FUserIntervals;

  SetSpaces(False);
  Un := AMaxSpace / (FNameList.Count);
  if Un < 1 then
  begin
    if (G is TCWCurve) and (TCWCurve(G).FMinPointSpacing = 0) then
      Un := 0;
  end;
  Compr := false;
  if G is TCWBar then
   Compr := TCWBar(G).Compressing
  else if G is TCWCurve then
    Compr := (Chart.NameScale.OverflowAction = ovCompression);

  MinSpace := G.MinPointSpacing;
  FCompressed := false;
  if Compr or ((Un >= MinSpace) and not((Contraction > 1) and Scrollable)) then
  begin
    FNameUnit := round(Un);
    if Un > 0 then
      CheckMaxSpacing;
    if Compr
    and not (((Un >= MinSpace) and not((Contraction > 1) and Scrollable)))
    then
      FCompressed := True;
  end
  else if Scrollable then
  begin
    ScrollTo(FScrollIndex);
    FNameUnit := round(Un);
  end
  else
  begin { Not enough space}
    if (Chart.GetNameType in [ntCategory, ntGeneral]) or (Chart.NameScale.OverflowAction = ovNone) then
    { No Contraction of ntCategory, ntGeneral }
    begin
      SetState(stOverflow);
      GoBackError;
      ShowGWError(msg_NoSpace);
    end;
    { Internal Contraction always uses avg }
    FirstTime := not InState(stInternalContraction);
    SetState(stInternalContraction);
    if FirstTime then
      FContractionCount := 0;

    Rate := GetContraction(stNameScale);
    if InState(stLimbo) then
      Exit;
    if Rate = Contraction then
    begin
      inc(Rate);
    end;
    if FirstTime then
      DoContraction(Rate, ctExplicit)
    else
    begin
      DoContraction(Rate, ctIncremental);
    end;
    HasContracted := True;
    FUserContraction := 0;
    inc(FContractionCount);
    FMinContraction := Contraction;
  end;
  if FixedGraphHeight = 0 then
   CheckValueSpace;
  if not HasContracted then
    FMinContraction := 1;
  if not HasContracted and (ActiveGraph is TCWAxisGraph)
    and (Chart.AllEqual <> nil) and not InState(stLimbo) then
  begin
    if ActiveGraph is TCWBar then
    begin
      if (TCWBar(ActiveGraph).FAnimations <> []) then
      begin
        InitAnimation;
      end;
    end
    else if (ActiveGraph is TCWCurve) and TCWCurve(ActiveGraph).Animation then
      InitAnimation;
  end;

  FPosRect := GetPosRect;
  PostMessage(Handle, WM_AFTERBUILD, 0, 0);
  ClearState(stOverflow);
end;

procedure TChartWriter.DrawBorders;
var
  R: TRect;

begin
  if Chart = nil then
    Exit;
  if ActiveGraph is TCWPie then
  begin
    if poClientBorder in TCWPie(ActiveGraph).Options then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(GraphRect);
    end;
  end
  else if Chart.GraphBorders = gbAllSides then
  begin
    R := GraphRect;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Style := bsClear;
    inc(R.Bottom);
    inc(R.Right);
    Canvas.Rectangle(R);
  end;
  ResetCanvas(nil);
end;

procedure TChartWriter.Paint;
var
  R: TRect;
  W, W2: integer;
  MP: TPoint;
  i: integer;
  UniqueGraphs : TList<TCWGraph>;

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
      if FNameAxis.IsXAxis then
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
      if FNameAxis.IsXAxis then
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
      Canvas.Draw(CWBoundsRect.Left, CWBoundsRect.Top, FSelBM);
      RedrawPoints;
    end;
    if (FHintText = '') and not Assigned(FOnMouseInfo) then
    begin
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
          ResetCanvas(nil);
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
    Canvas.Brush.Color := Chart.GraphBGColor;
    Canvas.Rectangle(R);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(R.Left + 15, R.Top + 1, FHintText);
    Canvas.Brush.Color := FHintColor;
    Canvas.Pen.Color := clBlack;

    Canvas.Rectangle(
    Rect(R.Left + 3, R.Top + 3, R.Left + 12, R.Bottom -3));
    ResetCanvas(nil);
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
      if FNameAxis.IsXAxis then
      begin
        APos.Y := APos.Y - GraphRect.Top;
        HW := GraphRect.Height;
        APos.Y := GraphRect.Height - APos.Y;
        Result := (ActiveValueScale.ValueHigh - ActiveValueScale.ValueLow) * APos.Y / HW + ActiveValueScale.ValueLow;

      end
      else
      begin
        APos.X := APos.X - GraphRect.Left;
        HW := GraphRect.Width;
        APos.X := GraphRect.Width - APos.X;
        Result := (ActiveValueScale.ValueHigh - ActiveValueScale.ValueLow) * APos.X / HW - ActiveValueScale.ValueHigh;
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
          W := GetTextWidth(lkInfo, HintInfo.Text[i]) + 8;
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
      if FNameAxis.IsXAxis then
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
    Canvas.Draw(CWBoundsRect.Left, CWBoundsRect.Top, FSelBM);
    if not ShowRuler then
      HintInfo.SerCount := 0;
    if HintInfo.SerCount = 0 then
    begin
      Exit;
    end;
    if Rulers = ruValues then
    begin
      if (MouseInfo = miName) then
      begin
        Exit;
      end;
      MP := ScreenToClient(MOuse.CursorPos);
      V := ValueFromPos(MP);
      S := FormatNum(V, ValuePrecision);

      if FNameAxis.IsXAxis then
      begin
        Canvas.TextOut(GraphRect.Left + 3, MP.Y + 3, S);
      end
      else
      begin
        Canvas.TextOut(MP.X + 3, GraphRect.Top + 3, S);
      end;
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
      ColorBlend(Canvas.Handle, R, clCream, 128);
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := clBlack;
      Canvas.Rectangle(R);
    end;

    for i := 0 to HintInfo.Text.Count - 1 do
    begin
      if not Handled then
      begin
        S := HintInfo.Text[i];
        Canvas.Brush.Style := bsClear;
        Canvas.TextOut(R.Left+15, R.Top + (H * i), S);
        Canvas.Brush.Color := HintInfo.Clrs[i];
        Canvas.Pen.Color := clBlack;
        Canvas.Rectangle(
          Rect(R.Left + 3, R.Top+(H*i) + 3, R.Left + 12,
          R.Top + (H*I) + GetTextHeight(lkInfo)-3));
      end;
      ResetCanvas(nil);
      if RulerGuideLines then
      begin
        Canvas.Pen.Color := Canvas.Font.Color;
        if FNameAxis.IsXAxis then
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
    ResetCanvas(nil);
    FLastMousePos := MP;
  end;

  procedure ProcessRuler;
  var
    Pt: TPoint;
    Res: Boolean;
    Cnt: integer;
    Inf: TRulerHintInfo;
    Defs: TSeriesInfoItems;
    Def: TSeriesInfo;
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
        Res := PointsFromRuler(FNameAxis.IsXAxis, FRulerX, FRulerY, Pt, Defs);
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
    Canvas.Draw(CWBoundsRect.Left, CWBoundsRect.Top, FSelBM);
    R.Top := GraphRect.Top;
    R.Bottom := GraphRect.Bottom;
    if FNameAxis.IsXAxis then
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
    if FNameAxis.IsXAxis then
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
    FNameAxis.DrawAxis;
    FValueAxis.DrawAxis;
    if Chart <> nil then
    begin
     if Chart.AxisCount = 2 then
      FValueAxis2.DrawAxis;
    end;
  end;

begin
  if (Chart = nil)  then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    Exit;
  end;

  if (FMouseSizing and not LiveResize) and not (ActiveGraph is TCWPie) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    Canvas.Draw(0,0, FSelBM);
    Exit;
  end;

  if OverflowError then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    R := GraphRect;
    Canvas.Brush.Color := Chart.GraphBGColor;
    Canvas.FillRect(R);
    DrawEssentials;
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

    SetState(stPainting);
    if (Chart = nil) or InState(stLimbo) then
    {Limb indicates that the component window has ben shrinked,
    to a size that makes the chart unrendable}
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
      Canvas.Draw(0, 0, FSelBM);
      ClearState(stPainting);
      Exit;
    end;
    try
      if InState(stAnimating) and not InState(stInitAnimation) and (VisibleCount > 0) then
      begin
        if InState(stAnimationPause) or InState(stResumeAnimation) then
        begin
          Canvas.Draw(0, 0, FSelBM);
          ClearState(stResumeAnimation);
          if InState(stAnimationPause) then
            Exit;
        end;
        {The animations loop}
        UniqueGraphs := TList<TCWGraph>.Create;
        try
        for I := 0 to Chart.SeriesDefs.Count-1 do
        begin
         if UniqueGraphs.IndexOf(Chart.SeriesDefs[i].Graph) <> -1 then
          Continue;
         Chart.SeriesDefs[i].Graph.Draw;
         UniqueGraphs.Add(Chart.SeriesDefs[i].Graph);
        end;
        finally
          UniqueGraphs.Free;
        end;
        DrawBorders;
        Exit;
      end;
      if ViewMode = vmSelected then
      { Selected draw the saved bitmap (selBM) and do not need running
        the whole paint procedure }
      begin
        R := SelRect;
        Canvas.Draw(CWBoundsRect.Left, CWBoundsRect.Top, FSelBM);
        FAlfaBM.SetSize(R.Width, R.Height);

        FAlfaBM.Canvas.CopyRect(Rect(0, 0, FAlfaBM.Width, FAlfaBM.Height),
          Canvas, R);
        DrawAlphaBlend(FAlfaBM.Canvas.Handle, Rect(0, 0, FAlfaBM.Width,
          FAlfaBM.Height), 64);
        Canvas.Draw(R.Left, R.Top, FAlfaBM);
        Exit;
      end;
      ResetCanvas(nil);
      Canvas.FillRect(ClientRect);

      R := GraphRect;
      Canvas.Brush.Color := Chart.GraphBGColor;
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
      {Draw borders according to the GraphBorders settings}
      if not (ActiveGraph is TCWPie) then
      begin
       Canvas.Brush.Color := Color;
       FValueAxis.Draw;
       {Draw labels, sections and axis lines of the value axis};
       if Chart.ValAx2Graph <> nil  then
       {If a secondary value scale is involved, draw labels, sections
       and lines for that scale}
         FValueAxis2.Draw;
       FNameAxis.Draw;
      end;

      if InState(stInitAnimation) and not InState(stAnimating) then
      begin
         {When an animation is on it's way, stop here to make present an empty
         graph which wil be filled in the animation loop}
        Chart.DrawTitle;
        Exit;
      end;

      if VisibleCount > 0 then
      begin
        UniqueGraphs := TList<TCWGraph>.Create;
        {Unique graphs lists the graphs uniquely instanciated.
         If, for instance there are to similar instances of a curve only one of them are drawn}
        try
          for I := 0 to Chart.Seriesdefs.Count-1 do
          {Loop SeriesDefs and draw each unique graph}
          begin
            if UniqueGraphs.IndexOf(Chart.SeriesDefs[i].Graph) <> -1 then
              Continue;
            Chart.SeriesDefs[i].Graph.Draw;
            UniqueGraphs.Add(Chart.SeriesDefs[i].Graph);
            if Assigned(FAfterDrawGraph) then
            begin
               FAfterDrawGraph(Chart.SeriesDefs[i].Graph, Canvas);
               ResetCanvas(nil);
            end;
          end;
        finally
          UniqueGraphs.Free;
        end;
         {Draw legends, qualifiers and title}
        Chart.DrawLegends;
        Chart.DrawQualifiers;
      end;
      Chart.DrawTitle;

      if not MouseInfoControl then
        PostMessage(Handle, WM_SAVESELBM, 0, 0);
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

procedure TChartWriter.CheckImage;
var
  i : integer;
begin
   if (Chart is TCWCategoryChart) and (Chart.Categories.Count > 0) then
  {To apply images all must be the same size and all categories must have
  an image assigned.}
  begin
     if Assigned(Chart.Categories.Items[0].FImage.Graphic) and Chart.NameScale.AllowImages then
     begin
       FImageSize.cx := Chart.Categories.Items[0].Image.Width;
       FImageSize.cy := Chart.Categories.Items[0].Image.Height;
     end;
     if FImageSize.cx > 0 then
     for I := 1 to Chart.Categories.Count-1 do
     begin
      if Assigned(Chart.Categories.Items[i].Image.Graphic) then
      begin
        if (Chart.Categories.Items[i].Image.Width <> FImageSize.cx)
        or (Chart.Categories.Items[i].Image.Height <> FImageSize.cy) then
        begin
           FImageSize.cx := 0;
           FImageSize.cy := 0;
           Break;
        end;
      end
      else
      begin
        FImageSize.cx := 0;
        FImageSize.cy := 0;
        Break;
      end;
     end;
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
    if ((toName in Chart.TextTilting) and (LabelKind = lkName)) or
      ((toValue in Chart.TextTilting) and (LabelKind = lkValue)) or
      ((toSection in Chart.TextTilting) and (LabelKind = lkValueSection)) or
      ((toSection in Chart.TextTilting) and (LabelKind = lkNameSection)) then
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
  FImageSize.cx := 0;
  FImageSize.cy := 0;
  if Chart is TCWPieChart then
  begin
    CheckImage;
    Exit;
  end;
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
    FActiveValAx := FValueAxis;
    E := ActiveValueScale.ValueHigh;
    S := FormatNum(E, ValuePrecision, True);
    FWidestValue := S;
    E := ActiveValueScale.ValueLow;
    S := FormatNum(E, ValuePrecision, True);
    if Canvas.TextWidth(S) > Canvas.TextWidth(FWidestValue) then
      FWidestValue := S;
    DoEvent(lkValue);

    FActiveValAx := FValueAxis2;
    E := ActiveValueScale.ValueHigh;
    S := FormatNum(E, ValuePrecision, True);
    FWidestValue2 := S;
    E := ActiveValueScale.ValueLow;
    S := FormatNum(E, ValuePrecision, True);
    if Canvas.TextWidth(S) > Canvas.TextWidth(FWidestValue2) then
      FWidestValue2 := S;
    DoEvent(lkValue2);
    FActiveValAx := FValueAxis;

    for i := Ser.FirstItem to Ser.LastItem do
    begin
      S := Ser.FSeriesItems[i].FName;

      if IsTimeSpan then
        S := GetTimeStr(Self, Chart.TimeFormat, StrToDateTime(S, Fmt))
      else if (Chart.GetNameType = ntNumberSpan) and (Chart.NameScale.NumSpanPrecision >0 ) then
        S := GetNumPrecisionString(S,Chart.NameScale.NumSpanPrecision );

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
    CheckImage;
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

procedure TChartWriter.SetLabelFreqs(ComputePts: Boolean = True);
var
  Num: single;
  F : TNotifyEvent;

  procedure EnableFontChange(F : TFont; var Event : TNotifyEvent;  Enable : Boolean);
  begin
     if Enable then
       F.OnChange := Event
     else
     begin
       Event := F.OnChange;
       F.OnChange := nil;
     end;
  end;

  function GetFreq(Ax: TAxisObject; Count: integer; LblKind: TLabelKind;
  Angle: integer): integer;
  var
    n: integer;
    W : integer;
  begin
    if Ax.IsXAxis then
    begin
      w := GraphPrintRect.Width;
      n := GetTextWidth(LblKind, Angle) + c_LabelXMarg;
      if n = 0 then
      begin
        Result := 1;
        Exit;
      end;
      Num := w / n;
    end
    else
    begin
      w := GraphPrintRect.Height;
      n := GetTextHeight(LblKind, Angle);
      if n = 0 then
      begin
        Result := 1;
        Exit;
      end;
      Num := w / n;
    end;
    if Num = 0 then
      Result := 1
    else
    begin
      Result := Ceil(Count / Num);
    end;

    if (Result > 1) and (((LblKind = lkNameSection) and not FUseNamShortNames)
      or ((LblKind = lkValueSection) and not FUseValShortNames)) and Ax.IsXAxis then
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
    if (Result > Chart.TextTiltThreshold) and (FImageSize.cx = 0) then
    begin
      Res := Result;
      Result := GetFreq(Ax, Count, LblKind, 450);
      Angl := 450;
      if Result > Chart.TextTiltThreshold then
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
        begin
          EnableFontChange(Chart.NameScale.Font, F, False);
          Chart.NameScale.Font.Orientation := Angl;
          EnableFontChange(Chart.NameScale.Font, F, True);
        end;
      lkValue:
        if Ax.IsXAxis then
        begin
          EnableFontChange(Chart.ValueScale1.Font, F, False);
          Chart.ValueScale1.Font.Orientation := Angl;
          EnableFontChange(Chart.ValueScale1.Font, F, True);

        end;
      lkValue2:
        if Ax.IsXAxis then
        begin
          EnableFontChange(Chart.ValueScale2.Font, F, False);
          Chart.ValueScale2.Font.Orientation := Angl;
          EnableFontChange(Chart.ValueScale2.Font, F, True);
        end;
      lkNameSection:
        if (NameSectionDefs <> nil) and Ax.IsXAxis then
        begin
          EnableFontChange(Chart.NameSectiondefs.Font, F, False);
          NameSectionDefs.Font.Orientation := Angl;
          EnableFontChange(Chart.NameSectiondefs.Font, F, True);
        end;
      lkValueSection:
        if (ValueSectionDefs <> nil) and Ax.IsXAxis then
        begin
          EnableFontChange(Chart.ValueSectiondefs.Font, F, False);
          ValueSectionDefs.Font.Orientation := Angl;
          EnableFontChange(Chart.ValueSectiondefs.Font, F, True);
        end;
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

    EnableFontChange(Chart.NameScale.Font, F, False);
    Chart.NameScale.Font.Orientation := 0;
    EnableFontChange(Chart.NameScale.Font, F, True);

    EnableFontChange(Chart.ValueScale1.Font, F, False);
    Chart.ValueScale1.Font.Orientation := 0;
    EnableFontChange(Chart.ValueScale1.Font, F, True);

    EnableFontChange(Chart.ValueScale2.Font, F, False);
    Chart.ValueScale2.Font.Orientation := 0;
    EnableFontChange(Chart.ValueScale1.Font, F, True);

    if (NameSectionDefs <> nil) then
    begin
      EnableFontChange(NameSectionDefs.Font, F, False);
      NameSectionDefs.Font.Orientation := 0;
      EnableFontChange(NameSectionDefs.Font, F, True);
    end;
    if (ValueSectionDefs <> nil) then
    begin
      EnableFontChange(ValueSectionDefs.Font, F, False);
      ValueSectionDefs.Font.Orientation := 0;
      EnableFontChange(ValueSectionDefs.Font, F, True);
    end;

    FUseNamShortNames := False;
    FUseValShortNames := False;
    FNameLabelFreq := GetOrient(FNameAxis, NameCount, lkName,
      (toName in Chart.TextTilting));
    FActiveValax := FValueAxis;
    FValueLabelFreq := GetOrient(FValueAxis, FValueAxis.Count, lkValue,
      (toValue in Chart.TextTilting));
    if Chart.ValAx2Graph <> nil then
    begin
      FActiveValax := FValueAxis2;
      FValueLabelFreq2 := GetOrient(FValueAxis2, FValueAxis2.Count, lkValue2,
        (toValue in Chart.TextTilting));
    end;
    FActiveValax := FValueAxis;
    FNameSectionFreq := GetOrient(FNameAxis, FNameSections.Count, lkNameSection,
      (toSection in Chart.TextTilting));
    FValueSectionFreq := GetOrient(FValueAxis, FValueSections.Count, lkValueSection,
      (toSection in Chart.TextTilting));
  finally
    dec(FCallCounter);
    if FCallCounter = 0 then
      ClearState(stLabelFreqs);
  end;
  FPosRect := GetPosRect;
   if ComputePts then
  begin
    ComputePoints;
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
  if Chart = nil then
    Exit;
  Dt1 := Now; { To keep the compiler satisfied }
  Dt2 := Now;
  for i := 0 to FSeriesData.Count - 1 do
  begin
    if IsTimeSpan then
    begin
      Dt1 := FSeriesData[i].ToDate(0);
      Dt2 := FSeriesData[i].ToDate(1);
    end;
    if FSeriesData[i].Count > 1 then
    begin

      if Chart.GetNameType = ntMonthSpan then
        Result := MonthsBetweenEx(Dt2, Dt1)
      else if Chart.GetNameType = ntDateSpan then
        Result := DaysBetween(Dt1, Dt2)
      else if Chart.GetNameType = ntHourSpan then
        Result := HoursBetween(Dt1, Dt2)
      else if Chart.GetNameType = ntMinuteSpan then
        Result := MinutesBetween(Dt1, Dt2)
      else if Chart.GetNameType = ntSecondSpan then
        Result := SecondsBetween(Dt1, Dt2)
      else if Chart.GetNameType = ntNumberSpan then
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
  if FNameAxis.Position = Posit then
    Result := FNameAxis
  else if FValueAxis.Position = Posit then
    Result := FValueAxis
  else if ((Chart <> nil) and (Chart.ValAx2Graph <> nil)) and (FValueAxis2.Position = Posit) then
    Result := FValueAxis2
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
  if Values.Count > 360 then
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
  begin
    ShowGWError(msg_SpanTypeDtFormatMismatch);
  end;
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

procedure CheckMonthEquality(ASeries: TSeries; Writer: TChartWriter);
var
  Span: integer;
  Dt: TDateTime;
begin
  if Writer.InState(stInternalAction) then
    Exit;
  if not TryStrToDate(ASeries.FSeriesItems[1].FName, Dt, Fmt) then
    ShowGWError(msg_SpanTypeDtFormatMismatch);
  Span := MonthsBetweenEx(StrToDate(ASeries.FSeriesItems[1].FName, Fmt),
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
  if Writer.Chart.GetNameType = ntHourSpan then
    Span := HoursBetween(Dt1, Dt2)
  else if Writer.Chart.GetNameType = ntMinuteSpan then
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
   Canvas.Font.Assign(Chart.TitleFont);
   Result := Canvas.TextHeight(Chart.Title) + 13; {Fixed margins}
  finally
    Canvas.Font.Assign(F);
    F.Free;
  end;
end;

function TChartWriter.GetNumPrecisionString(Num : string; NumSpanPrec: Integer) : string;
var
 P : integer;
begin
    Result := Num;
    P := Length(Result) - NumSpanPrec;
    if P = 0 then
      Result := '0' + Fmt.DecimalSeparator + Num
    else if P >= 1 then
    begin
      Insert(Fmt.DecimalSeparator, Result, P + 1);
    end;
    {Remove trailing zeroes after comma}
    P := Length(Result);
    if (Result <> '0') and (Pos(Fmt.DecimalSeparator, Result) <> 0) then
    while Result[P] = '0' do
    begin
      Delete(Result, P, 1);
      P := Length(Result);
    end;

  if Pos(Fmt.DecimalSeparator, Result) = Length(Result) then
   Delete(Result, Length(Result), 1);
end;

function TChartWriter.GetValPrecision: integer;
begin
  Result := ValuePrecision;
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
  if (Chart.GetNameType in [ntGeneral, ntCategory]) then
    Exit;
  if IsTimeSpan then
  begin
    n := StrToDateTime(AName, Fmt);
  end
  else
    n := StrToFloat(AName, Fmt);

  for i := StartWith to FNameList.Count - 1 do
  begin
    if IsTimeSpan then
    begin
      N2 := StrToDateTime(FNameList[i], Fmt);
    end
    else
      N2 := StrToFloat(FNameList[i], Fmt);
    if (N2 > n) then
    begin
      if IsTimeSpan and (i > 0) then
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
      if IsTimeSpan and (i > 0) then
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

function TChartWriter.GetImageFilename(AFilename : string;
CategoryIndex : integer) : string;
var
 Dir : string;
 ImageType : string;
begin
   ImageType := '.BMP';
   Dir := ExtractFileDir(AFileName);
   Result := Dir + '\' + Categories.Items[CategoryIndex].FCategoryName + ImageType;
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
  S : string;
  IsAlternative : Boolean;

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

  procedure SetColors(Colors: TCWCategories; Props: TStringList);
  var
    i, Indx, Indx2: integer;
    sl: TStringList;
    Clr: TCWCategory;
    ImgFileName : string;
  begin
    Indx2 := 0;
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
      Indx := Props.IndexOf('{ITEMCOLOR' + IntToStr(0) + '}');
      while Indx <> -1 do
      begin
        sl.Clear;
        for i := 1 to 3 do
        begin
          sl.Add(Props[Indx + i]);
        end;
        Clr := Colors.Add;
        Clr.FCategoryName := sl.Values[C_Ident];
        Clr.FColor := TColor(StrToInt(sl.Values[C_Color]));
        ImgFileName := sl.Values[C_ImageFileName];
        if ImgFileName <> '' then
        begin
         try
          Clr.Image.Bitmap.TransparentMode := tmAuto;
          Clr.Image.LoadFromFile(ImgFileName);
          Clr.Image.Bitmap.PixelFormat := pf24Bit;
          Clr.Image.Bitmap.Transparent := True;
         except
           GoBackError;
           raise;
         end;
        end;
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
        for i := 1 to 26 do
        begin
          sl.Add(Props[Indx + i]);
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
        Leg.FPointName := sl.Values[C_PointName];
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

  procedure SetCommons(Props : TStringList; AScale : TCWScale);
  begin
      AScale.FQualifier := Props.Values[C_Qualifier];
      AScale.FShowLabels := Boolean(StrToInt(Props.Values[C_ShowLabels]));
      AScale.FShowDividerLines := Boolean(StrToInt(Props.Values[C_ShowDividerLines]));
      AScale.FMinLabelSpacing := StrToInt(Props.Values[C_MinLabelSpacing]);
      EncodeFont(AScale.FFont, Props.Values[C_Font]);
      EncodePen(AScale.FPen, Props.Values[C_Pen]);
  end;


  procedure SetValSpans(Props: TStringList; Ax: TCWValueScale);
  var
    sl : TStringList;
    Indx : integer;
    i : integer;
  begin
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
    if Ax = Chart.ValueScale1 then
    begin
      Ax.FValuePrecision := StrToInt(Props.Values[C_ValuePrecision1]);
      Indx := Props.IndexOf('{VALUESCALE1}');
      if Indx =  -1 then
        Exit;
      for I := Indx+1 to Props.Count-1 do
      begin
        if SameText(Props[i],'{VALUESCALE2}') then
          Break;
        sl.Add(Props[i]);
      end;
    end
    else
    begin
      Ax.FValuePrecision := StrToInt(Props.Values[C_ValuePrecision2]);
      Indx := Props.IndexOf('{VALUESCALE2}');
      if Indx = -1 then
        Exit;
      for I := Indx+1 to Props.Count-1 do
        sl.Add(Props[i]);
    end;
    Ax.FValueSpanFromData :=
      Boolean(Ord(StrToInt(sl.Values[C_ValueSpanFromData])));
    Ax.FValueIntervals := StrToFloat(sl.Values[C_ValueIntervals], Fmt)/1000;
    Ax.FuserIntervals := Ax.FValueIntervals;
    Ax.FValueLow := StrToFloat(sl.Values[C_ValueLow], Fmt)/1000;
    Ax.FValueHigh := StrToFloat(sl.Values[C_ValueHigh], Fmt)/1000;
    SetCommons(sl, Ax);
    finally
      sl.Free;
    end;
  end;

  procedure SetInnerMargins(Props : TstringList; Marg : TCWMargins);
  begin
    Marg.FLeft := StrToInt(Props.Values[C_InnerLeftMargin]);
    Marg.FRight := StrToInt(Props.Values[C_InnerRightMargin]);
    Marg.FTop := StrToInt(Props.Values[C_InnerTopMargin]);
    Marg.FBottom := StrToInt(Props.Values[C_InnerBottomMargin]);
  end;

  procedure SetGraphMargins(Props : TstringList; Marg : TCWMargins);
  begin
    Marg.FLeft := StrToInt(Props.Values[C_GraphLeftMargin]);
    Marg.FRight := StrToInt(Props.Values[C_GraphRightMargin]);
    Marg.FTop := StrToInt(Props.Values[C_GraphTopMargin]);
    Marg.FBottom := StrToInt(Props.Values[C_GraphBottomMargin]);
  end;

  procedure SetNameScale(Props: TStringList; Ax: TCWNameScale);
  var
    sl : TStringList;
    Indx : integer;
    Indx2 : integer;
    i : integer;
  begin
    sl := TStringList.Create;
    sl.DefaultEncoding := TEncoding.Utf8;
    try
      Indx := Props.IndexOf('{NAMESCALE}');
      Indx2 := Props.IndexOf('{VALUESCALE1}');
      for I := Indx+1 to Indx2-1 do
        sl.Add(Props[i]);
      Ax.FOverflowAction :=  TOverflowAction(Ord(StrToInt(sl.Values[C_OverflowAction])));
      Ax.FNumspanPrecision := StrToInt(sl.Values[C_NumSpanPrecision]);
      Ax.FAllowImages := Boolean(StrToInt(sl.Values[C_AllowImages]));
      SetCommons(Sl, Ax);
    finally
      sl.Free;
    end;
  end;

  procedure SetAnims(Props : TStringList; G : TCWGraph);
  begin
     G.FAnimation := Boolean(StrToInt(Props.Values[C_Animation]));
     G.FAnimationSpeed := TAnimationSpeed(StrToInt(Props.Values[C_AnimationSpeed]));
     G.FAnimationPause := StrToInt(Props.Values[C_AnimationPause]);
     if G is TCWBar then
     with G as TCWBar do
     begin
       FAnimations := [];
       if StrToInt(Props.Values[C_AnGrow]) = 1 then
         FAnimations := FAnimations + [anGrow];
       if StrToInt(Props.Values[C_AnFlow]) = 1 then
         FAnimations := FAnimations + [anFlow];
     end;
  end;

  procedure SetStats(Props : TStringList; G : TCWAxisGraph);
  begin
    G.FStatLine := TStatLine(StrToInt(Props.Values[C_StatLine]));
    G.FStatLineWidth := StrToInt(Props.Values[C_StatLineWidth]);
    G.FSMAPeriods := StrToInt(Props.Values[C_SMAPeriods]);
    G.FStatBackgroundBlending := StrToInt(Props.Values[C_StatBackgroundBlending]);
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
    B.FDrawBaseLine := Boolean(StrToInt(Props.Values[C_DrawBaseLine]));
    B.FItemSpacing := StrToInt(Props.Values[C_ItemSpacing]);
    B.FSeriesSpacing := StrToInt(Props.Values[C_SeriesSpacing]);
    B.FBarWidth := StrToInt(Props.Values[C_BarWidth]);
    B.FOrigBarWidth := B.FBarWidth;
    B.FCubeDepth := StrToInt(Props.Values[C_CubeDepth]);
    B.FCubeAngle := StrToInt(Props.Values[C_CubeAngle]);
    B.FBarStyle := TBarStyle(StrToInt(Props.Values[C_BarStyle]));
    B.FLayout := TBarLayout(StrToInt(Props.Values[C_BarLayout]));
    B.FShowQualifier := Boolean(StrToInt(Props.Values[C_ShowQualifier]));
    B.FAutoSize := Boolean(StrToInt(Props.Values[C_AutoSize]));
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
    EncodeFont(B.FFont, Props.Values[C_Font]);
    B.FKeepFontColor := Boolean(StrToInt(Props.Values[C_KeepFontColor]));
    SetAnims(Props, B);
    SetStats(Props, B);
    if IsAlternative then
      FChart.FAlternativeGraph := B;
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
    if Boolean(StrToInt(Props.Values[C_poPrintValues])) then
      P.FOptions := P.FOptions + [poPrintValues];
    if Boolean(StrToInt(Props.Values[C_poPrintPercentages])) then
      P.FOptions := P.FOptions + [poPrintPercentages];
    if Boolean(StrToInt(Props.Values[C_poClientBorder])) then
      P.FOptions := P.FOptions + [poClientBorder];
    if Boolean(StrToInt(Props.Values[C_poPrintSeriesTitles])) then
      P.FOptions := P.FOptions + [poPrintSeriesTitles];
    if Boolean(StrToInt(Props.Values[C_poTextBackground])) then
      P.FOptions := P.FOptions + [poTextBackground];
    if Boolean(StrToInt(Props.Values[C_poPinText])) then
      P.FOptions := P.FOptions + [poPinText];
    if Boolean(StrToInt(Props.Values[C_poPrintTitleInDoughnut])) then
      P.FOptions := P.FOptions + [poPrintTitlesInDoughnut];
    if Boolean(StrToInt(Props.Values[C_poAllowImages])) then
      P.FOptions := P.FOptions + [poAllowImages];

    P.FSliceSpacing := StrToFloat(Props.Values[C_PieSliceSpacing], Fmt);
    P.FPieSize := StrToInt(Props.Values[C_PieSize]);
    P.FDoughnutSize := StrToInt(Props.Values[C_DoughnutSize]);
    P.FSlope := StrToInt(Props.Values[C_Slope]);
    P.FStartAngle := StrToInt(Props.Values[C_StartAngle]);
    P.FStyle := TPieStyle(StrToInt(Props.Values[C_Style]));
    P.FDiscDepth := StrToInt(Props.Values[C_DiscDepth]);
    P.FKeepFontColor := Boolean(StrToInt(Props.Values[C_KeepFontColor]));
    EncodeFont(P.Font, Props.Values[C_Font]);
    EncodeFont(P.SeriesTitleFont, Props.Values[C_SeriesTitleFont]);
    SetAnims(Props, P);
    if IsAlternative then
      FChart.FAlternativeGraph := P;
  end;

  procedure CreateCurve(Props: TStringList);
  var
    c: TCWCurve;
    Indx, Indx2: integer;
    i: integer;
    sl: TStringList;
    St: TCWSeriesStyle;
    O : TGraphName;
    G : TCWGraph;
  begin
    G := GetGraphByName(Props.Values[C_Name]);
    if G <> nil then
      Exit;
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
    c.FLineShape := TLineShape(StrToInt(Props.Values[C_LineShape]));
    c.FSmoothLines := Boolean(StrToInt(Props.Values[C_SmoothLines]));
    c.FAreaOutline := Boolean(StrToInt(Props.Values[C_AreaOutLine]));
    c.FAreaOutlineColor := TColor(StrToInt(Props.Values[C_AreaOutLineColor]));
    c.FBaseLineValue := StrToFloat(Props.Values[C_CurveBaseLineValue], Fmt);
    c.FDrawBaseLine := Boolean(StrToInt(Props.Values[C_DrawBaseLine]));
    c.FMinPointSpacing := StrToInt(Props.Values[C_MinPointSpacing]);
    c.FMaxPointSpacing := StrToInt(Props.Values[C_MaxPointSpacing]);
    c.FKeepFontColor := Boolean(StrToInt(Props.Values[C_KeepFontColor]));
    EncodeBrush(c.FAreaBrush, Props.Values[C_CurveBrush]);
    EncodeFont(c.FFont, Props.Values[C_Font]);
    SetAnims(Props, C);
    SetStats(Props, C);

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
    if IsAlternative then
      FChart.FAlternativeGraph := c;
  end;

  procedure SetSects(Props: TStringList; SD: TCWSectionDefs);
  var
    i, Indx, Indx2: integer;
    sl: TStringList;
    S: TCWSectionItem;
    E : single;
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
        if SD is TCWValueSectionDefs then
        begin
          E := StrToFloat(sl.Values[C_StartValue], Fmt);
          E := E/1000;
          S.FStartValue := FloatToStr(E, Fmt);
          E := StrToFloat(sl.Values[C_EndValue], Fmt);
          E := E/1000;
          S.FEndValue := FloatToStr(E, Fmt);
        end
        else
        begin
          S.FStartValue := sl.Values[C_StartValue];
          S.FEndValue := sl.Values[C_EndValue];
        end;

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
    C.FPercentages := Boolean(StrToInt(Props.Values[C_Percentages]));
    C.FSpanType := TSpanType(StrToInt(Props.Values[C_SpanType]));
    C.FAxisOrientation := TAxisOrientation
      (StrToInt(Props.Values[C_AxisOrientation]));
    C.FWallWidth := StrToInt(Props.Values[C_WallWidth]);
    C.FWallBorderColor := TColor(StrToInt(Props.Values[C_WallBorderColor]));
    C.FWallColor := TColor(StrToInt(Props.Values[C_WallColor]));
    C.FGradientWall := Boolean(StrToInt(Props.Values[C_GradientWall]));
    C.FTextTiltThreshold := StrToInt(Props.Values[C_TextTiltThreshold]);
    EnCodeFont(C.FTitleFont, Props.Values[C_TitleFont]);
    C.TitleAlignment := TAlignment(StrToInt(Props.Values[C_TitleAlignment]));
    C.FTimeFormat := Props.Values[C_TimeFormat];
    C.FMouseTimeFormat := Props.Values[C_MouseTimeFormat];
    C.FGraphBGColor := TColor(StrToInt(Props.Values[C_GraphBGColor]));
    C.FGraphBorders := TGraphBorders(StrToInt(Props.Values[C_GraphBorders]));

    if Boolean(StrToInt(Props.Values[C_toName])) then
      C.FTextTilting := C.FTextTilting + [toName]
    else
      C.FTextTilting := C.FTextTilting - [toValue];
    if Boolean(StrToInt(Props.Values[C_toValue])) then
      C.FTextTilting := C.FTextTilting + [toValue]
    else
      C.FTextTilting := C.FTextTilting - [toValue];

    SetInnerMargins(Props, C.InnerMargins);
    SetGraphMargins(Props, C.GraphMargins);
    SetColors(C.Categories, Props);
    SetNameScale(Props, C.NameScale);
    SetValSpans(Props, C.ValueScale1);
    SetValSpans(Props, C.ValueScale2);

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
        SD.FValueScale :=  TValueScaleNumber(StrToInt(sl.Values[C_ValueAxis]));
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
    for i := 0 to PropFile.FGraphs.Count - 1 do
    begin
      if (PropFile.FGraphs[i][0] = '[SPANCHART]') then
      begin
       FChart := TCWSpanChart.Create(Owner);
       Break;
      end
      else if (PropFile.FGraphs[i][0] = '[GENERALCHART]') then
      begin
       FChart := TCWGeneralChart.Create(Owner);
       Break;
      end
      else if (PropFile.FGraphs[i][0] = '[CATEGORYBARCHART]') then
      begin
       FChart := TCWCategoryBarChart.Create(Owner);
       Break;
      end
      else if (PropFile.FGraphs[i][0] = '[PIECHART]') then
      begin
       FChart := TCWPIEChart.Create(Owner);
       Break;
      end;
    end;

    S := PropFile.FProps.Values['Chart'];
    IsAlternative := false;

    for i := 0 to PropFile.FGraphs.Count - 1 do
    begin
      if PropFile.FGraphs[i][0] = '[ALTERNATIVE]' then
        IsAlternative := True;
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
      else if (PropFile.FGraphs[i][0] = '[SPANCHART]')
      or (PropFile.FGraphs[i][0] = '[GENERALCHART]')
      or (PropFile.FGraphs[i][0] = '[CATEGORYBARCHART]')
      or (PropFile.FGraphs[i][0] = '[PIECHART]')
      then
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
          if IsTimeSpan then
          begin
            STime := PropFile.ItemName[i, j];
            if PropFile.FTimeType = ttUnix then
            begin
              UxTime := StrToInt(STime);
              Dt := UnixToDateTime(UxTime);
              STime := DateToStr(Dt, Fmt);
            end
            else if PropFile.FTimeType = ttISO8601 then
            begin
              Dt := ISO8601ToDate(STime);
              STime := DateToStr(Dt, Fmt);
            end;
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
  ThisChart : TCWChart;

  function GetTimeInterval(T1, T2: TDateTime; ASpanType: TSpanType): integer;
  begin
    Result := 0;
    case ASpanType of
      ntMonthSpan:
        Result := MonthsBetweenEx(T2, T1);
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
  ThisChart := Chart;
  try
    if GetFileType(AFileName) = 0 then {Rich}
    begin
      if csDesigning in ComponentState then
        ShowGWError(msg_RichDesign);
      if Chart <> nil then
        Chart.SaveCache;
      ResetIDS;
      LoadFromPropFile(AFileName);
      CreateIDs;
      Chart.InnerMargins.FWriter := Chart.Writer;
      Chart.GraphMargins.FWriter := Chart.Writer;
      ClearState(stZoomed);
      Chart.FCWRFileName := AFileName;
      SetPosition(Chart.AxisOrientation);
      if DoExecute and (Count > 0) then
      begin
        Execute;
        if ActiveGraph is TCWPie then
          CorrectPie
        else
         DoRepaint;
      end;
      if Assigned(ChartList.FOnChange) then
        ChartList.FOnChange(Chart);

      Exit;
    end;
    if (Chart = nil) or (ActiveGraph=nil) then
    begin
      ShowGWError(msg_NoActiveGraph);
    end;
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
      Chart.ClearCache;
      Chart.SaveCache;
    finally
      Files.Free;
      ClearState(stPainting);  { Aborts paint method}
    end;
    Chart.FFileName := AFileName;
    ClearState(stZoomed);
    if DoExecute then
    begin
     Execute;
     if ActiveGraph is TCWPie then
        CorrectPie
     else
       DoRepaint;
    end;
    if Assigned(FonDataChange) then
      FOnDataChange(Self);
  except on E:Exception do
  begin
     if ThisChart <> nil then
     begin
       FChart := ThisChart;
       LoadFromcache;
       Execute;
     end
     else
      GoBackError;
     ShowGWError(-1, 'An error occurred when opening the file: ' + E.Message);
  end;
  end;
end;

procedure TChartWriter.DoLoadFiles(AFileName: string; Files: TFiles);
var
  WorkSl: TStringList;
  S: string;
  FirstIndex: integer;
  i, j: integer;
  TimSpan : Boolean;
  Indx, Indx2 : integer;
  Clr : TCWCategory;
  Cnt : integer;
  Chk :Boolean;
  P : integer;
  TT : string;
  TimeTyp : TSaveTimeType;

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

  function DecodeISO(S: string): string;
  var
    P: integer;
    S2: string;
    Dt : TDateTime;
  begin
    P := Pos('=', S);
    S2 := Copy(S, 1, P - 1);
    Dt := ISO8601ToDate(S2);
    S2 := DateTimeToStr(Dt, Fmt);
    Delete(S, 1, P - 1);
    Result := S2 + S;
  end;

  procedure AddFile(FromIndex, ToIndex: integer);
  var
    i: integer;
    sl: TStringList;
    S: string;
    S2: string;
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
      S2 := WorkSl.KeyNames[i];
      if TimSpan then
      begin
        try
        if TimeTyp = ttUnix then
        begin
            S := DecodeUX(S);
        end
        else if TimeTyp = ttISO8601 then
        begin
            S := DecodeIso(S);
        end;
        except
            ShowGWError(msg_InvalidDateTime, S2);
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
  TimSpan := false;
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
    S := WorkSL[0];
    if S.StartsWith('[TIME TYPE') then
    begin
      S := Trim(S);
      P := Pos('=', S);
      TT := Copy(S, P+1, Length(S)-P-1);
      if SameText(TT, 'UNIX') then
        TimeTyp := ttUnix
      else if SameText(TT, 'ISO8601') then
        TimeTyp := ttISO8601
      else
        TimeTyp := ttLocal;
      WorkSl.Delete(0);
      TimSpan := True;
    end
    else
      TimeTyp := ttLocal;

    Indx := WorkSl.IndexOf('[PERCENTAGES]');
    Chart.FPercentages := (Indx <> -1);

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
        Chart.Categories.Clear;
        for I := Indx+1 to Indx2-1 do
        begin
         Clr := Chart.Categories.Add;
         Clr.FColor := GetColor(WorkSL[i]);
        end;
      end;
    end;

    Indx := WorkSl.IndexOf('[IMAGEFILENAMES]');
    if Indx <> -1 then
    begin
      Indx2 := WorkSl.IndexOf('[ENDIMAGEFILENAMES]');
      if Indx2 = -1 then
       ShowGWError(msg_FileFormat, AFileName);
      if not Chk then
      begin
        j := 0;
        for I := Indx+1 to Indx2-1 do
        begin
         Clr := Chart.Categories.Items[j];
         Clr.FImageFileName := WorkSL[i];
         try
          Clr.Image.LoadFromFile(Clr.FImageFileName);
         except
           GoBackError;
           raise;
         end;
         inc(j);
        end;
      end;
    end;

    Indx := WorkSl.IndexOf('[DATA]');
    if Indx <> -1 then
    begin
     Inc(Indx);
     FirstIndex := Indx;
    end
    else
    begin
     Indx := 0;
     FirstIndex := 0;
    end;

   { if (Indx = 0) and TimSpan  then
      Inc(Indx);
    if Indx = -1 then
      FirstIndex := 0
    else
      FirstIndex := Indx;
    }
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
    if AnOption in Chart.TextTilting then
    begin
      S := AValue + '=1';
    end;
    PropList.Add(S);
  end;

begin
  PropList.Add(Props);
  S := C_Chart + '=' + Chart.Name;
  PropList.Add(S);
  S := C_LiveResize + '=' + IntToStr(Ord(LiveResize));
  PropList.Add(S);
end;

procedure TChartWriter.SetProps(const PropList: TStringList);
var
  i: integer;

begin
{Delete?}
  for i := 0 to PropList.Count - 1 do
  begin
  end;
end;

procedure TChartWriter.LoadDataFromCWRFile(AFileName: TFileName; DoExecute : Boolean = True);
var
  i : integer;
  SL : TStringList;
  DataSl : TStringList;
  Indx, Indx2 : integer;
  S : string;
  TimeType : TSaveTimeType;
  P : integer;
  TT : string;
  V : single;
  STime : string;
  UXTime : int64;
  Dt : TDateTime;
begin
  Clear;
  if GetFileType(Chart.FCWRFileName) <> 0 then
   ShowGWError(msg_RichDesign);
  SL := TStringList.Create;
  DataSl := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    SL.Delete(0); {Sign}
    S := SL[0];
    if S.StartsWith('[TIME TYPE') then
    begin
      S := Trim(S);
      P := Pos('=', S);
      TT := Copy(S, P+1, Length(S)-P-1);
      if SameText(TT, 'UNIX') then
        TimeType := ttUnix
      else if SameText(TT, 'ISO8601') then
        TimeType := ttISO8601
      else
        TimeType := ttLocal;
      sl.Delete(0);
    end
    else
      TimeType := ttLocal;

    Indx := SL.IndexOf('[DATA]');
    if Indx = - 1 then
    begin
      ShowGWError(msg_NoDataFound);
    end;
    Indx2 := SL.IndexOf(Objects);
    if Indx2 = - 1 then
    begin
      ShowGWError(msg_MissingTerminator);
    end;
    for I := Indx + 1 to Indx2-1 do
    begin
     S := SL[i];
     if (S = EOF) or (S = '') then
     begin
       AddSeries(DataSl);
       DataSL.Clear;
       Continue;
     end;

     V := StrToFloat(sl.ValuefromIndex[i], Fmt) / 1000;
     if IsTimeSpan then
     begin
          STime := sl.KeyNames[i];
          if TimeType = ttUnix then
          begin
            UxTime := StrToInt(STime);
            Dt := UnixToDateTime(UxTime);
            STime := DateToStr(Dt, Fmt);
          end
          else if TimeType = ttISO8601 then
          begin
            Dt := ISO8601ToDate(STime);
            STime := DateToStr(Dt, Fmt);
          end;
          sl[i] := STime + sl.NameValueSeparator + FormatNum(V, ValuePrecision);
     end
     else
       sl.ValueFromIndex[i] := FormatNum(V, ValuePrecision);
     DataSL.Add(sl[i]);
    end;
  finally
    sl.Free;
    DataSl.Free;
  end;
  if DoExecute then
    Execute;
end;

procedure TChartWriter.LoadFromDatabase(ADataset : TDataset;
 NameField, ValueFields : string; ATitle : string = ''; DoExecute : Boolean = True);
begin
  Clear;
  AddSeries(ADataSet, NameField, ValueFields, ATitle);
  if DoExecute then
    Execute;
end;

procedure TChartWriter.SaveToFile(AFileName: TFileName; FileFormat : TSaveFormat =sfDataOnly;
 TimeType : TSaveTimeType = ttLocal);
{ Saves data only }
var
  i: integer;
  sl, saveSL: TStringList;
  G: TCWGraph;
  CCnt, BCnt, PCnt: integer;
  S: string;
  ImgFileName : string;

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

  procedure SetBarAnimOpt(AnOption: TAnimation ; AnimOptions: TAnimations;
  AValue: string);
  begin
    S := AValue + '=0';
    if AnOption in AnimOptions then
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

  procedure GetValSpans(Valspan: TCWValueScale; Num : integer);
  begin
    begin
      saveSL.Add('{VALUESCALE' + IntToStr(Num) + '}');
      SetKeyVal(saveSL, C_ValueSpanFromData, IntToStr(Ord(ValSpan.ValueSpanFromData)));
      SetKeyVal(saveSL, C_ValueIntervals, FormatNum(ValSpan.ValueIntervals*1000,
        ValSpan.ValuePrecision));
      SetKeyVal(saveSL, C_ValueHigh, FormatNum(ValSpan.ValueHigh*1000, ValSpan.ValuePrecision));
      SetKeyVal(saveSL, C_ValueLow, FormatNum(ValSpan.ValueLow*1000, ValSpan.ValuePrecision));
      if Num = 1 then
        SetKeyVal(saveSL, C_ValuePrecision1, IntToStr(ValSpan.ValuePrecision))
      else
        SetKeyVal(saveSL, C_ValuePrecision2, IntToStr(ValSpan.ValuePrecision));
    end;
  end;

  procedure GetNameScale(ANameScale : TCWNameScale);
  begin
     saveSL.Add('{NAMESCALE}');
     SetKeyVal(saveSL, C_OverflowAction, IntToStr(Ord(Chart.Namescale.FOverflowAction)));
     SetKeyVal(saveSL, C_NumSpanPrecision, IntToStr(Chart.Namescale.FNumSpanPrecision));
     SetKeyVal(saveSL, C_AllowImages, IntToStr(Ord(Chart.Namescale.FAllowImages)));
  end;

  procedure GetGraphMargins(Marg : TCWMargins);
  begin
    SetKeyVal(saveSL, C_GraphLeftMargin, IntToStr(Chart.GraphMargins.Left));
    SetKeyVal(saveSL, C_GraphTopMargin, IntToStr(Chart.GraphMargins.Top));
    SetKeyVal(saveSL, C_GraphRightMargin, IntToStr(Chart.GraphMargins.Right));
    SetKeyVal(saveSL, C_GraphBottomMargin, IntToStr(Chart.GraphMargins.Bottom));
  end;

  procedure GetInnerMargins(Marg : TCWMargins);
  begin
    SetKeyVal(saveSL, C_InnerLeftMargin, IntToStr(Chart.InnerMargins.Left));
    SetKeyVal(saveSL, C_InnerTopMargin, IntToStr(Chart.InnerMargins.Top));
    SetKeyVal(saveSL, C_InnerRightMargin, IntToStr(Chart.InnerMargins.Right));
    SetKeyVal(saveSL, C_InnerBottomMargin, IntToStr(Chart.InnerMargins.Bottom));
  end;

  procedure GetCommons(Commons : TCWScale);
  var
    S : string;
  begin
    {Commons}
    SetKeyVal(saveSL, C_Qualifier, Commons.FQualifier);
    SetKeyVal(saveSL, C_ShowDividerLines, IntToStr(Ord(Commons.FShowDividerlines)));
    SetKeyVal(saveSL, C_ShowLabels, IntToStr(Ord(Commons.FShowLabels)));
    SetKeyVal(saveSL, C_MinLabelSpacing, IntToStr(Commons.FMinLabelSpacing));
    S := DecodeFont(Commons.Font);
    SetKeyVal(saveSL, C_Font, S);
    S := DecodePen(Commons.Pen);
    SetKeyVal(saveSL, C_Pen, S);
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
       SetKeyVal(saveSL, C_ValueAxis, IntToStr(Ord(Chart.SeriesDefs.Items[i].ValueScale)));
       SetKeyVal(saveSL, C_Visible, IntToStr(Ord(Chart.SeriesDefs.Items[i].Visible)));
     end;
  end;

  procedure GetColors(Clrs: TCWCategories);
  var
    i: integer;
    ImgFileName : string;
  begin
    if FImageSize.cx = 0 then
      CheckImage;
    for i := 0 to Clrs.Count - 1 do
    begin
      saveSL.Add('{ITEMCOLOR' + IntToStr(i) + '}');
      SetKeyVal(saveSL, C_Ident, Clrs.Items[i].CategoryName);
      SetKeyVal(saveSL, C_Color, IntToStr(Clrs.Items[i].Color));
      ImgFileName := '';
      if FImageSize.cx > 0 then
      begin
        ImgFileName := GetImageFileName(AFileName, i);
        Clrs.Items[i].Image.Bitmap.SaveToFile(ImgFileName);
      end;
      SetKeyVal(saveSL, C_ImageFileName, ImgFileName);
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
      SetKeyVal(saveSL, C_PointName, Leg.PointName);
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
    E : single;
  begin
    for i := 0 to S.Sections.Count - 1 do
    begin
      saveSL.Add('{SECTION' + IntToStr(i) + '}');
      if S is TCWValueSectionDefs then
      begin
        E := StrToFloat(S.Sections.Items[i].StartValue, Fmt);
        E := E * 1000;
        SetKeyVal(saveSL, C_StartValue, FloatToStr(E, Fmt));

        E := StrToFloat(S.Sections.Items[i].EndValue, Fmt);
        E := E * 1000;
        SetKeyVal(saveSL, C_EndValue, FloatToStr(E, Fmt));
      end
      else
      begin
       SetKeyVal(saveSL, C_StartValue, S.Sections.Items[i].StartValue);
       SetKeyVal(saveSL, C_EndValue, S.Sections.Items[i].EndValue);
      end;
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

  procedure SetAnimations(G : TCWGraph);
  begin
    SetKeyVal(saveSL, C_Animation, IntToStr(Ord(G.Animation)));
    SetKeyVal(saveSL, C_AnimationSpeed, IntToStr(Ord(G.AnimationSpeed)));
    SetKeyVal(saveSL, C_AnimationPause, IntToStr(G.AnimationPause));
  end;

  procedure SetStats(G : TCWAxisGraph);
  begin
    SetKeyVal(saveSL, C_StatLine, IntToStr(Ord(G.StatLine)));
    SetKeyVal(saveSL, C_StatLineWidth, IntToStr(G.StatLineWidth));
    SetKeyVal(saveSL, C_SMAPeriods, IntToStr(G.SMAPeriods));
    SetKeyVal(saveSL, C_StatBackgroundBlending, IntToStr(G.StatBackgroundBlending));
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
    SetKeyVal(saveSL, C_LineShape, IntToStr(Ord(c.LineShape)));
    SetKeyVal(saveSL, C_SmoothLines, IntToStr(Ord(c.SmoothLines)));
    SetKeyVal(saveSL, C_AreaOutline, IntToStr(Ord(c.AreaOutLine)));
    SetKeyVal(saveSL, C_AreaOutlineColor, IntToStr(Ord(c.AreaOutlineColor)));
    SetKeyVal(saveSL, C_UseSeriesStyles, IntToStr(Ord(c.UseSeriesStyles)));
    SetKeyVal(saveSL, C_CurveBaseLineValue, FormatNum(c.BaseLineValue,
      ValuePrecision));
    SetKeyVal(saveSL, C_DrawBaseline, IntToStr(Ord(C.DrawBaseLine)));
    SetKeyVal(saveSL, C_MinPointSpacing, IntToStr(c.MinPointSpacing));
    SetKeyVal(saveSL, C_MaxPointSpacing, IntToStr(c.MaxPointSpacing));
    SetKeyVal(saveSL, C_Font, DecodeFont(c.Font));
    SetKeyVal(saveSL, C_KeepFontColor, IntToStr(Ord(c.KeepFontColor)));
    SetAnimations(C);
    SetStats(C);

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
    SetKeyVal(saveSL, C_ShowQualifier, IntToStr(Ord(B.ShowQualifier)));
    SetKeyVal(saveSL, C_AutoSize, IntToStr(Ord(B.AutoSize)));
    SetKeyVal(saveSL, C_DrawBaseline, IntToStr(Ord(B.DrawBaseLine)));
    SetKeyVal (saveSL, C_Font, DecodeFont(B.Font));
    SetKeyVal(saveSL, C_KeepFontColor, IntToStr(Ord(B.KeepFontColor)));
    SetBarOpt(boBaseLine, B.Options, C_boBaseLine);
    SetBarOpt(boOutLines, B.Options, C_boOutLines);
    SetBarOpt(boText, B.Options, C_boText);
    SetBarTextOpt(tcName, B.TextContents, C_tcName);
    SetBarTextOpt(tcValue, B.TextContents, C_tcValue);
    SetBarTextOpt(tcTitle, B.TextContents, C_tcTitle);
    SetBarTextOpt(tcPercentage, B.TextContents, C_tcPercentage);
    SetBarAnimOpt(anFlow, B.Animations, C_AnFlow);
    SetBarAnimOpt(anGrow, B.Animations, C_AnGrow);
    SetAnimations(B);
    SetStats(B);
  end;

  procedure GetPieProps(P: TCWPie);
  begin
    saveSL.Add('[PIE]');
    inc(PCnt);
    SetKeyVal(saveSL, C_Name, P.Name);
    SetKeyVal(saveSL, C_PieSize, IntToStr(P.PieSize));
    SetKeyVal(saveSL, C_Slope, IntToStr(P.Slope));
    SetKeyVal(saveSL, C_DoughnutSize, IntToStr(P.DoughnutSize));
    SetKeyVal(saveSL, C_DiscDepth, IntToStr(P.DiscDepth));
    SetKeyVal(saveSL, C_Style, IntToStr(Ord(P.Style)));
    SetKeyVal(saveSL, C_StartAngle, IntToStr(P.StartAngle));
    SetKeyVal(saveSL, C_PieSliceSpacing, FormatNum(P.SliceSpacing, 1));
    SetKeyVal (saveSL, C_Font, DecodeFont(P.Font));
    SetKeyVal (saveSL, C_SeriesTitleFont, DecodeFont(P.SeriesTitleFont));
    SetKeyVal(saveSL, C_KeepFontColor, IntToStr(Ord(P.KeepFontColor)));
    SetPieOpt(poPrintPercentages, P.Options, C_poPrintPercentages);
    SetPieOpt(poPrintNames, P.Options, C_poPrintNames);
    SetPieOpt(poClientBorder, P.Options, C_poClientBorder);
    SetPieOpt(poPrintSeriesTitles, P.Options, C_poPrintSeriesTitles);
    SetPieOpt(poPrintValues, P.Options, C_poPrintValues);
    SetPieOpt(poTextBackground, P.Options, C_poTextBackground);
    SetPieOpt(poPinText, P.Options, C_poPinText);
    SetPieOpt(poPrintTitlesInDoughnut, P.Options, C_poPrintTitleInDoughnut);
    SetPieOpt(poAllowImages, P.Options, C_poAllowImages);
    SetAnimations(P);
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
    if IsTimeSpan then
    begin
    if TimeType = ttUnix then
     saveSL.Add('[TIME TYPE=UNIX]')
    else if TimeType = ttLocal then
     saveSL.Add('[TIME TYPE=LOCAL]')
    else if TimeType = ttISO8601 then
     saveSL.Add('[TIME TYPE=ISO8601]');
    end;

    try
     if Chart.Percentages then
      Savesl.Add('[PERCENTAGES]');
     if FileFormat = sfDataExtended then
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

        if Chart.Categories.Count > 0 then
         Savesl.Add('[ITEMCOLORS]');
        for i := 0 to Chart.Categories.Count-1 do
        begin
           Clr := Chart.Categories.Items[i].FColor;
           R := GetRValue(Clr);
           Gr := GetGValue(Clr);
           B := GetBValue(Clr);
           Savesl.Add(IntToStr(R) + ',' + IntToStr(Gr) + ',' + IntToStr(B));
        end;
        if Chart.Categories.Count > 0 then
        begin
          Savesl.Add('[ENDITEMCOLORS]');
        end;

        if (Chart.Categories.Count > 0) and (FImageSize.cx > 0) then
        begin
         Savesl.Add('[IMAGEFILENAMES]');
         for i := 0 to Chart.Categories.Count-1 do
         begin
           ImgFileName := GetImageFileName(AFileName, i);
           Chart.Categories.Items[i].Image.SaveToFile(ImgFileName);
           Savesl.Add(ImgFileName);
         end;
         Savesl.Add('[ENDIMAGEFILENAMES]');
        end;

        Savesl.Add('[GRAPHS]');
        if ActiveGraph is TCWCurve then
        begin
         Savesl.Add('Curve');
         Savesl.Add('Bar');
        end
        else if ActiveGraph is TCWBar then
        begin
         Savesl.Add('Bar');
        end
        else
        begin
         Savesl.Add('Pie');
         Savesl.Add('Bar');
        end;
        Savesl.Add('[ENDGRAPHS]');

        Savesl.Add('[DATA]');
     end;

     for i := 0 to Chart.FOrigData.Count - 1 do
     begin
        sl := Chart.FOrigData[i].AsStrings(TimeType, False);
        saveSL.AddStrings(sl);
        if i < Chart.FOrigData.Count-1 then
         saveSL.Add('');
        sl.Free;
     end;
     SaveSl.SaveToFile(AFileName);
    finally
      saveSL.Free;
    end;
  end;

begin
  if (FSeriesData.Count = 0) and (FileFormat in [sfDataOnly, sfDataExtended]) then
    ShowGWError(msg_NothingToSave);
  if FileFormat in [sfDataOnly, sfDataExtended] then
  begin
    DoSaveData;
    Exit;
  end;

  if ActiveGraph = nil then
    ShowGWError(msg_NoActiveGraph);
  saveSL := TStringList.Create;
  saveSL.DefaultEncoding := TEncoding.Utf8;

  saveSL.Add(SignCWR);
  if IsTimeSpan then
  begin
    if TimeType = ttUnix then
      saveSL.Add('[TIME TYPE=UNIX]')
    else if TimeType = ttLocal then
      saveSL.Add('[TIME TYPE=LOCAL]')
    else if TimeType = ttISO8601 then
      saveSL.Add('[TIME TYPE=ISO8601]');
  end;

  { Get general props }
  GetProps(saveSL);
  saveSL.Add('[Data]');

  { Get series data }
  try
    if Chart.FOrigData.Count = 0 then
      saveSL.Add(EOF)
    else
    for i := 0 to Chart.FOrigData.Count - 1 do
    begin
      sl := Chart.FOrigData[i].AsStrings(TimeType, False);
      saveSL.AddStrings(sl);
      saveSL.Add(EOF);
      sl.Free;
    end;

    { Get graph objects }
    BCnt := 0;
    CCnt := 0;
    PCnt := 0;
    saveSL.Add(Objects);
    G := nil;
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

    if (Chart.FAlternativeGraph <> nil) and (G <> nil) and G.InChart then
    begin
      SaveSl.Add('[ALTERNATIVE]');
      for i := 0 to Chart.SeriesDefs.Count - 1 do
      begin
        G := Chart.FAlternativeGraph;
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
    if Chart is TCWSpanChart then
      saveSL.Add('[SPANCHART]')
    else if Chart is TCWGeneralChart then
      saveSL.Add('[GENERALCHART]')
    else if Chart is TCWCategoryBarChart then
      saveSL.Add('[CATEGORYBARCHART]')
    else
      saveSL.Add('[PIECHART]');
    saveSL.Add(C_Name + '=' + Chart.Name);
    saveSL.Add(C_Title + '=' + Chart.Title);
    saveSL.Add(C_SpanType + '=' + IntToStr(Ord(Chart.FSpanType)));
    saveSL.Add(C_Percentages + '=' + IntToStr(Ord(Chart.FPercentages)));
    saveSL.Add(C_AxisOrientation + '=' + IntToStr(Ord(Chart.FAxisOrientation)));
    saveSL.Add(C_WallWidth + '=' + IntToStr(Chart.FWallWidth));
    saveSL.Add(C_WallBorderColor + '=' + IntToStr(Chart.FWallBorderColor));
    saveSL.Add(C_WallColor + '=' + IntToStr(Chart.FWallColor));
    saveSL.Add(C_GradientWall + '=' + IntToStr(Ord(Chart.FGradientWall)));
    saveSL.Add(C_TextTiltThreshold + '=' + IntToStr(Chart.FTextTiltThreshold));
    if Chart.NameSectionDefs <> nil then
      saveSL.Add(C_NameSectionDefs + '=' + Chart.NameSectionDefs.Name);
    if Chart.ValueSectionDefs <> nil then
    saveSL.Add(C_ValueSectionDefs + '=' + Chart.ValueSectionDefs.Name);
    saveSL.Add(C_TitleFont + '=' + DecodeFont(Chart.TitleFont));
    saveSL.Add(C_TitleAlignment + '=' + IntToStr(Ord(Chart.FTitleAlignment)));
    saveSl.Add(C_TimeFormat + '=' + Chart.TimeFormat);
    saveSl.Add(C_MouseTimeFormat + '=' + Chart.MouseTimeFormat);
    saveSl.Add(C_GraphBGColor + '=' + IntToStr(Chart.FGraphBGColor));
    saveSl.Add(C_GraphBorders + '=' + IntToStr(Ord(Chart.FGraphBorders)));

    S := C_toName +'=0';
    if toName in Chart.FTextTilting then
      S := C_toName +'=1';
    saveSL.Add(S);

    S := C_toValue +'=0';
    if toValue in Chart.FTextTilting then
      S := C_toValue +'=1';
    saveSL.Add(S);

    GetInnerMargins(Chart.InnerMargins);
    GetGraphMargins(Chart.GraphMargins);
    GetSeriesDefs;
    GetColors(Chart.Categories);
    GetNameScale(Chart.NameScale);
    GetCommons(Chart.NameScale);
    GetValspans(Chart.ValueScale1, 1);
    GetCommons(Chart.ValueScale1);
    GetValspans(Chart.ValueScale2, 2);
    GetCommons(Chart.ValueScale2);
    saveSL.Add(EOF);

    saveSL.SaveToFile(AFileName);
  finally
    saveSL.Free;
  end;
end;

procedure TChartWriter.LoadFromCache;
var
  i : integer;
begin
  Clear;
  for I := 0 to Chart.SeriesDefs.Count-1 do
  begin
     AddSeries(Chart.SeriesDefs[i].FDataCache);
  end;
end;

function TChartWriter.GetScrollable: Boolean;
begin
  Result := False;
  if (Chart = nil) then
    Exit;
  if (ActiveGraph = nil) or (ActiveGraph is TCWPie) then
    Exit;
  Result := Chart.NameScale.OverflowAction = ovScrolling;
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
    GoBackError(True);
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
  Chart.FSpanType := DetectSpanType(FSeriesData.Count - 1);
  try
    if (Chart.GetNameType = ntNumberSpan) and (ASeries.Count > 1) then
    begin
      CheckNumberEquality(ASeries, Self);
    end
    else if (Chart.GetNameType = ntMonthSpan) and (ASeries.Count > 1) then
      CheckMonthEquality(ASeries, Self)
    else if (Chart.GetNameType = ntDateSpan) and (ASeries.Count > 1) then
    begin
      CheckDateEquality(ASeries, Self);
    end
    else if IsTimeSpan then
      CheckTimeEquality(ASeries, Self)
    else if (Chart.GetNameType in [ntGeneral, ntCategory]) then
      CheckGeneralEquality(ASeries, Self);
  except
    GoBackError(True);
    raise;
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
      GoBackError(True);
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
  Chart.FSpanType := DetectSpanType(FSeriesData.Count - 1);
  try
    if (Chart.GetNameType = ntNumberSpan) and (Ser.Count > 1) then
    begin
      CheckNumberEquality(Ser, Self);
    end
    else if (Chart.GetNameType = ntDateSpan) and (Ser.Count > 1) then
    begin
      CheckDateEquality(Ser, Self);
    end
    else if (Chart.GetNameType = ntMonthSpan) and (Ser.Count > 1) then
    begin
      CheckMonthEquality(Ser, Self);
    end
    else if IsTimeSpan and (Ser.Count > 1) then
    begin
      CheckTimeEquality(Ser, Self);
    end
    else if (Chart.GetNameType in [ntGeneral, ntCategory]) then
    begin
      CheckGeneralEquality(Ser, Self);
    end;

  except
    GoBackError(True);
    raise;
  end;
  SetState(stUpdating);
end;

procedure TChartWriter.AddSeries(ASeries: TSeries; ATitle : string; AGraph : TCWGraph;
  AColor : TColor; AValueAxis : TValueScaleNumber);
begin
    Chart.AddSeriesDef(ATitle, AGraph, AColor, AValueAxis);
    AddSeries(ASeries, False);
end;

procedure TChartWriter.AddSeries(Values: TStringList; ATitle : string; AGraph : TCWGraph;
  AColor : TColor; AValueAxis : TValueScaleNumber);
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
       Chart.FSpanType := DetectSpanType(FSeriesData.Count - 1);
       try
        if (Chart.GetNameType = ntNumberSpan) and (SerList[i].Count > 1) then
        begin
          CheckNumberEquality(SerList[i], Self);
        end
        else if (Chart.GetNameType = ntDateSpan) and (SerList[i].Count > 1) then
        begin
          CheckDateEquality(SerList[i], Self);
        end
        else if (Chart.GetNameType = ntMonthSpan) and (SerList[i].Count > 1) then
        begin
          CheckMonthEquality(SerList[i], Self);
        end
        else if IsTimeSpan and (SerList[i].Count > 1) then
        begin
          CheckTimeEquality(SerList[i], Self);
        end
        else if (Chart.GetNameType in [ntGeneral, ntCategory]) then
        begin
          CheckGeneralEquality(SerList[i], Self);
        end;
       except
        GoBackError(True);
        raise;
     end;
     SetState(stUpdating);
    end;
  finally
    sl.Free;
    SerList.Free;
  end;

end;

procedure TChartWriter.MakeTimeSpan(ASpanType: TSpanType);
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
        if ASpanType = ntHourSpan then
          H2 := HoursBetween(Lowest, H)
        else if ASpanType = ntMinuteSpan then
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
        if ASpanType = ntHourSpan then
          H2 := HoursBetween(Highest, H)
        else if ASpanType = ntMinuteSpan then
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

procedure TChartWriter.MakeMonthSpan;
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
    { Find the Series with the lowest date part. Use the year of this as the logical year.}

    Finalize(FLogYearMap);
    setlength(FLogYearMap, 1);
    LowestDP := '1231';
    LowestSer := 0;
    for i := 0 to FSeriesData.Count - 1 do
    begin
      Dt := StrToDate(FSeriesData[i].FSeriesItems[0].FName, Fmt);
      DecodeDate(Dt, Y, M, D);
      DP := ExpandDM(M) + ExpandDM(D);
      if DP < LowestDP then
      begin
        LowestSer := i;
        LowestDP := DP;
      end;
    end;
    Dt := FSeriesData[LowestSer].ToDate(0);
    DecodeDate(Dt, Y, M, D);
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
        n := MonthsBetweenEx(Dt, Lowest);
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
        n := MonthsBetweenEx(Highest, Dt);
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

procedure TChartWriter.CreateSpan;
var
  LSer: TSeries;
  Ser, OrigSer: TSeries;
  i, j: integer;
  SerItm: TSeriesItem;

  procedure Check(ASeries: TSeries);
  var
    i, j: integer;
  begin
    if IsTimeSpan or (ActiveGraph is TCWPie) then
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
  if (Chart.GetNameType = ntMonthSpan) then
    MakeMonthSpan
  else if (Chart.GetNameType = ntDateSpan) then
    MakeDateSpan
  else if IsTimeSpan then
    MakeTimeSpan(Chart.SpanType)
  else if (Chart.GetNameType = ntNumberSpan) then
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

  if Chart.FOrigData.Count = 0 then
    for i := 0 to FSeriesData.Count - 1 do
    begin
      OrigSer := TSeries.Create;
      OrigSer.Assign(FSeriesData[i], -1, -1);
      Chart.FOrigData.Add(OrigSer);
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
  if (Chart.GetNameType in [ntGeneral, ntCategory]) then
  begin
    Result := FSeriesData[0];
    Exit;
  end;
  Longest := 0;

  if IsTimeSpan then
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

procedure TChartWriter.DeleteSection(OnAxis: TScaleType; Index: integer);
begin

  if OnAxis = stNameScale then
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
  if Chart is TCWPieChart then
    Exit;
  if (NameSectionDefs <> nil) and NameSectionDefs.Visible then
    CreateNameSections;
  if (ValueSectionDefs <> nil) and ValueSectionDefs.Visible
    then
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
  ClearSections(stValueScale1, False);
  if (ValueSectionDefs.Sections.Count = 0) then
    Exit;
  Itms := ValueSectionDefs.Sections;
  for i := 0 to Itms.Count - 1 do
  begin
    StartVal := Itms.Items[i].FStartValue;
    EndVal := Itms.Items[i].FEndValue;
    if StartVal = EndVal then
      SectType := stLine
    else
      SectType := stSection;
    AddSection(stValueScale1, StartVal, EndVal, Itms.Items[i].FLongCaption,
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
    ClearSections(stNameScale, True);
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
    AddSection(stNameScale, AStart, AEnd, LCap, SCap, SectType);
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
      if not (Chart.GetNameType in [ntGeneral, ntCategory]) then
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

      AddSection(stNameScale, StartVal, EndVal, Itms.Items[i].FLongCaption,
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
  ClearSections(stNameScale, False);
  if (NameSectionDefs.Sections.Count = 0) and
    (NameSectionDefs.SectionType <> stAutoSections) then
    Exit;
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
    if (Templ = ttNotUsed) or not IsTimeSpan then
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

function TChartWriter.AddSection(OnScale: TScaleType;
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
    ClearSections(stNameScale, True);
    GoBackError;
    ShowGWError(Code, Text);
  end;

  function DoAdd(Reduced: Boolean): TSection;
  begin
    Sec := TSection.Create;
    Sec.FIsReduced := Reduced;
    Sec.FWriter := Self;
    if OnScale = stNameScale then
    begin
      Sec.FOwner := FNameAxis;
      Sec.FIndex := NameSectionCount
    end
    else
    begin
      Sec.FOwner := FValueAxis;
      Sec.FIndex := ValueSectionCount
    end;
    Sec.FStartVal := AStart;
    Sec.FEndVal := AEnd;
    Sec.FLongCaption := LongCaption;
    Sec.FShortCaption := ShortCaption;
    Sec.FSectionType := SectionType;
    if OnScale = stNameScale then
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
  if OnScale = stValueScale1 then
  begin
    E1 := StrToFloat(AStart, Fmt);
    E2 := StrToFloat(AEnd, Fmt);
    if E1 > E2 then
      ShowGWError(msg_StartEndValue);
  end
  else
  begin
    if IsTimeSpan then
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
          ClearSections(stNameScale, True);
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
    else if Chart.GetNameType = ntNumberSpan then
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

procedure TChartWriter.ClearSections(OnAxis: TScaleType; Both: Boolean);
begin
  if (OnAxis = stNameScale) or Both then
    FNameSections.Clear;
  if (OnAxis = stValueScale1) or Both then
    FValueSections.Clear;
end;

function TChartWriter.SpaceOf(Position: TAxisPosition): integer;
var
  Ax: TAxisObject;
begin
  Result := 0;
  if ActiveGraph is TCWPie then
  begin
    if Position = apTop then
     Result := Result + GetTitleSpace;
    if Chart <> nil then
    begin
     Result := Result + Chart.Legends.WidestLegend(Position);
    end;
    Exit;
  end;
  Ax := AxisOf(Position);
  if Ax <> nil then
  begin
    if Ax.OpposedSections then
      Result := Ax.LabelSpace + Ax.GetQualifierSpace
    else
      Result := Ax.SectionSpace + Ax.LabelSpace + Ax.GetQualifierSpace;
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
     if (Position in [apTop, apBottom]) and (Result = 0) then
      Result := Result + GetScaleExtension;
  end;
end;

function TChartWriter.GetWorkRect: TRect;
var
  D, H, W : integer;
begin
  Result := CWBoundsRect;
  Result.Left := Chart.InnerMargins.Left;
  Result.Top := Chart.InnerMargins.Top;
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
  if FNameAxis.IsXAxis then
  begin
    Result := GetPosRect.Width / (NameCount - 1)
  end
  else
  begin
    Result := GetPosRect.Height / (NameCount - 1);
  end;
end;

function TChartWriter.GetTextWidth(LabelKind: TLabelKind; AText: string;
Angle: integer = 0): integer;
var
  S: string;
  HV, HN, HVS, HNS: string;
  WV, WN, WVS, WNS: string;
begin
  WN := FWidestName;
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
    if (FImageSize.cx > 0) and Chart.NameScale.AllowImages then
      S := ''
    else if not(toName in Chart.TextTilting) then
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
    if not(toValue in Chart.TextTilting) then
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
    if not(toSection in Chart.TextTilting) then
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
    if not(toSection in Chart.TextTilting) then
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
    FHWBM.Canvas.Font.Assign(Chart.NameScale.Font)
  else if (LabelKind = lkValue) then
    FHWBM.Canvas.Font.Assign(Chart.ValueScale1.Font)
  else if (LabelKind = lkValue2) then
    FHWBM.Canvas.Font.Assign(Chart.ValueScale2.Font)
  else if (LabelKind = lkNameSection) and (NameSectionDefs <> nil) then
    FHWBM.Canvas.Font.Assign(NameSectionDefs.Font)
  else if (LabelKind = lkValueSection) and (ValueSectionDefs <> nil) then
    FHWBM.Canvas.Font.Assign(ValueSectionDefs.Font);
  if (FImageSize.cx > 0) and Chart.NameScale.AllowImages and (LabelKind = lkName) then
  begin
    Result := FImageSize.cx;
  end
  else if Angle = 450 then
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
      if (FImageSize.cy > 0) and Chart.NameScale.AllowImages then
        S := ''
      else if not(toName in Chart.TextTilting) then
        S := FTallestName
      else if (Angle <> 0) then
        S := FWidestName
      else
        S := FTallestName;
    end
    else if (LabelKind = lkValue) then
    begin
      if not(toValue in Chart.TextTilting) then
        S := FTallestValue
      else if (Angle <> 0) then
        S := FWidestValue
      else
        S := FTallestValue;
    end
    else if (LabelKind = lkValue2) then
    begin
      if not(toValue in Chart.TextTilting) then
        S := FTallestValue2
      else if (Angle <> 0) then
        S := FWidestValue2
      else
        S := FTallestValue2;
    end
    else if LabelKind = lkNameSection then
    begin
      if not(toSection in Chart.TextTilting) then
        S := FTallestNameSection
      else if (Angle <> 0) then
        S := FWidestNameSection
      else
        S := FTallestNameSection;
    end
    else
    begin
      if not(toSection in Chart.TextTilting) then
        S := FTallestValueSection
      else if (Angle <> 0) then
        S := FWidestValueSection
      else
        S := FTallestValueSection;
    end;

    if LabelKind = lkInfo then
      FHWBM.Canvas.Font.Assign(Font)
    else if (LabelKind = lkName) then
      FHWBM.Canvas.Font.Assign(Chart.NameScale.Font)
    else if (LabelKind = lkValue) then
      FHWBM.Canvas.Font.Assign(Chart.ValueScale1.Font)
    else if (LabelKind = lkValue2) then
      FHWBM.Canvas.Font.Assign(Chart.ValueScale2.Font)
    else if (LabelKind = lkNameSection) and (NameSectionDefs <> nil) then
      FHWBM.Canvas.Font.Assign(NameSectionDefs.Font)
    else if (LabelKind = lkValueSection) and (ValueSectionDefs <> nil) then
      FHWBM.Canvas.Font.Assign(ValueSectionDefs.Font);
    GetTextMetrics(FHWBM.Canvas.Handle, TM);
    FInternalLeading := TM.tmInternalLeading;
    sl.Text := S;
    H := 0;
    n := 0;
    if (FImageSize.cy > 0) and Chart.NameScale.AllowImages and (LabelKind = lkName) then
      Result := FImageSize.cy
    else if Angle = 0 then
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

procedure TChartWriter.SetLabelFont(const LabelKind: TLabelKind);
begin
  if LabelKind = lkInfo then
    Canvas.Font.Assign(Font)
  else if (LabelKind = lkName) then
    Canvas.Font.Assign(Chart.NameScale.Font)
  else if (LabelKind = lkValue) then
    Canvas.Font.Assign(Chart.ValueScale1.Font)
  else if (LabelKind = lkValue2) then
    Canvas.Font.Assign(Chart.ValueScale2.Font)
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

function TChartWriter.GetNameSections(Index: integer): TSection;
begin
  Result := FNameAxis.Sections[Index];
end;

function TChartWriter.GetNameSectionCount: integer;
begin
  Result := FNameAxis.SectionCount;
end;

function TChartWriter.GetValueSections(Index: integer): TSection;
begin
  Result := FValueAxis.Sections[Index];
end;

function TChartWriter.GetValueSectionCount: integer;
begin
  Result := FValueAxis.SectionCount;
end;

procedure TChartWriter.ResetCanvas(AGraph : TCWGraph);
begin
  if AGraph = nil then
    Canvas.Font.Assign(Font)
  else
    Canvas.Font.Assign(AGraph.Font);
  Canvas.Brush.Assign(Brush);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
end;

procedure TChartWriter.SaveSelBM;
begin
  if FRulerVisible and not(ViewMode = vmSelecting) and (InView(TCWCurve) = nil)
  then
    Exit;
  FSelBM.SetSize(CWBoundsRect.Width, CWBoundsRect.Height);
  FSelBM.Canvas.CopyRect(Rect(0, 0, FSelBM.Width, FSelBM.Height), Canvas,
    CWBoundsRect);
end;

procedure TChartWriter.SetInfoControl(const Value: Boolean);
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
  YL := ActiveValueScale.ValueLow;
  YH := ActiveValueScale.ValueHigh;
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
      YL := YL + ActiveValueScale.ValueIntervals;
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

function TChartWriter.GetOverflowError: Boolean;
begin
  Result := InState(stOverflow);
end;

function TChartWriter.IsTimeSpan: Boolean;
begin
  Result := false;
  if Chart = nil then
    Exit;
  if (Chart is TCWCategoryBarChart) or (Chart is TCWPieChart) or (Chart is TCWGeneralChart) then
    Exit;
  Result := not (Chart.SpanType = ntNumberSpan);
end;

function TChartWriter.HasWall: Boolean;
begin
  Result := false;
  if Chart = nil then
    Exit;
  if Chart is TCWPieChart then
    Exit;
  if ActiveGraph is TCWPie then
   Exit;
  Result := (Chart.FWallWidth > 0) and
    (Chart.AxisOrientation = alBottomLeft) and (Chart.GraphBorders <> gbNone) and
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
   if (Chart = nil) then
     Exit;
   if IsAxisChart then
     Result := Chart.WallColor
   else
     ShowGWError(msg_PrivateProp, C_WallColor);
end;

procedure TChartWriter.SetWallColor(const Value: TColor);
begin
   if Chart = nil then
     Exit;
   if IsAxisChart then
    Chart.WallColor := Value
  else
    ShowGWError(msg_PrivateProp, C_WallColor);

end;

function TChartWriter.GetWallBorderColor : TColor;
begin
   Result := clBlack;
   if (Chart = nil) then
     Exit;
   if IsAxisChart then
     Result := Chart.WallBorderColor
   else
     ShowGWError(msg_PrivateProp, C_WallBorderColor);
end;

procedure TChartWriter.SetWallBorderColor(const Value: TColor);
begin
   if Chart = nil then
     Exit;
   if IsAxisChart then
    Chart.WallBorderColor := Value
  else
    ShowGWError(msg_PrivateProp, C_WallBorderColor);
end;

function TChartWriter.GetGradientWall : Boolean;
begin
   Result := false;
   if (Chart = nil) then
     Exit;
   if IsAxisChart then
     Result := Chart.GradientWall
   else
    ShowGWError(msg_PrivateProp, C_GradientWall);
end;

procedure TChartWriter.SetGradientWall(const Value: Boolean);
begin
  if Chart = nil then
    Exit;
  if IsAxisChart then
    Chart.GradientWall := Value
  else
    ShowGWError(msg_PrivateProp, C_GradientWall);
end;

function TChartWriter.GetTextTilting : TTextOrientations;
begin
   Result := [];
   if Chart = nil then
     Exit;
   if IsAxisChart then
     Result := Chart.TextTilting
   else
     ShowGWError(msg_PrivateProp, C_TextTilting);
end;

procedure TChartWriter.SetTextTilting(const Value: TTextOrientations);
begin
    if Chart = nil then
      Exit;
    if IsAxisChart then
      Chart.TextTilting := Value
    else
      ShowGWError(msg_PrivateProp, C_TextTilting);
end;

procedure TChartWriter.SetTextTiltThreshold(const Value: integer);
begin
    if Chart = nil then
      Exit;
    if IsAxisChart then
      Chart.TextTiltThreshold := Value
    else
      ShowGWError(msg_PrivateProp, C_TextTiltThreshold);
end;

function TChartWriter.GetTextTiltThreshold : Integer;
begin
   Result := 1;
   if Chart = nil then
     Exit;
   if IsAxisChart then
     Result := Chart.TextTiltThreshold
   else
     ShowGWError(msg_PrivateProp, C_TextTiltThreshold);
end;

procedure TChartWriter.SetTitleFont(const Value: TFont);
begin
  if Chart <> nil then
     Chart.TitleFont := Value;
end;

function TChartWriter.GetTitleFont : TFont;
begin
   Result := Font;
   if Chart <> nil then
     Result := Chart.TitleFont;
end;

procedure TChartWriter.SetTitleAlignment(const Value: TAlignment);
begin
   if Chart <> nil then
      Chart.TitleAlignment := Value
end;

function TChartWriter.GetTitleAlignment : TAlignment;
begin
   Result := taLeftJustify;
   if Chart <> nil then
     Result := Chart.TitleAlignment;
end;

procedure TChartWriter.SetTimeFormat(const Value: string);
begin
  if Chart = nil then
    Exit;
  if Chart is TCWSpanChart then
      Chart.TimeFormat := Value
  else
    ShowGWError(msg_PrivateProp, C_TimeFormat);
end;

function TChartWriter.GetTimeFormat : string;
begin
    Result := '';
    if Chart = nil then
      Exit;
    if Chart is TCWSpanChart then
      Result := Chart.TimeFormat
    else
      ShowGWError(msg_PrivateProp, C_TimeFormat);
end;

function TChartWriter.GetSpanType : TSpanType;
begin
   Result := ntDateSpan;
   if Chart = nil then
     Exit;
   if (Chart is TCWSpanChart) then
   begin
      Result := TCWSpanChart(Chart).SpanType;
   end
   else
     ShowGWError(msg_PrivateProp, C_SpanType);
end;

procedure TChartWriter.SetSpanType(const Value: TSpanType);
begin
    if Chart = nil then
      Exit;
     if (Chart is TCWSpanChart) then
      TCWSpanChart(Chart).SpanType := Value
    else
      ShowGWError(msg_PrivateProp, C_SpanType);
end;

function TChartWriter.GetPercentages : Boolean;
begin
   Result := False;
   if Chart = nil then
     Exit;
   if (Chart is TCWCategoryChart) then
   begin
      Result := TCWCategoryChart(Chart).Percentages;
   end
   else
     ShowGWError(msg_PrivateProp, C_Percentages);
end;

procedure TChartWriter.SetPercentages(const Value: Boolean);
begin
    if Chart = nil then
      Exit;
     if (Chart is TCWCategoryChart) then
      TCWCategoryChart(Chart).Percentages := Value
    else
      ShowGWError(msg_PrivateProp, C_Percentages);
end;

function TChartWriter.GetAlternativeGraph : TCWGraph;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  if IsAxisChart then
      Result := Chart.AlternativeGraph
  else
     ShowGWError(msg_PrivateProp, C_AlternativeGraph);
end;

procedure TChartWriter.SetAlternativeGraph(const Value: TCWGraph);
begin
   if Chart <> nil then
    Chart.AlternativeGraph := Value;
end;

function TChartWriter.GetCategories : TCWCategories;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  if Chart is TCWCategoryChart then
  begin
     Result := Chart.Categories;
  end
  else
    ShowGWError(msg_PrivateProp, C_Categories);
end;

function TChartWriter.GetSelRect: TRect;
var
  pStart, PEnd: integer;

begin
  try
    Result := Rect(0, 0, 0, 0);
    if not(ViewMode in [vmSelected]) then
      Exit;
    if FNameAxis.IsXAxis then
    begin
      { Add unitsize to fill the rect completely }
      pStart := XFromName(Names[FDynaSectStart]);
      PEnd := XFromName(Names[FDynaSectEnd]) + FNameAxis.UnitSize;
      Result := Rect(pStart, GraphPrintRect.Top, PEnd, GraphPrintRect.Bottom);
    end
    else
    begin
      pStart := YFromName(Names[FDynaSectStart]);
      PEnd := YFromName(Names[FDynaSectEnd]) + FNameAxis.UnitSize;
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
  Result := (ActiveGraph <> nil) and (Count > 0) and not InState(stUpdating)
  and (VisibleCount > 0);
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

procedure TChartWriter.SetGraphBorders(const Value: TGraphBorders);
begin
  if Chart = nil then
    Exit;
  if IsAxisChart then
    Chart.GraphBorders := Value
  else
    ShowGWError(msg_PrivateProp, C_GraphBorders);
end;

function TChartWriter.GetGraphBorders : TGraphBorders;
begin

  Result := gbNone;
  if Chart = nil then
    Exit;
  if Chart is TCWPieChart then
    Exit;
  if IsAxisChart then
    Result := Chart.GraphBorders
  else
    ShowGWError(msg_PrivateProp, C_GraphBorders);
end;

function TChartWriter.GetCWBoundsRect : TRect;
begin
  Result := ClientRect;
  if (Chart = nil) or (Chart is TCWPieChart) or (Count = 0)then
    Exit;
  if FixedGraphWidth <> 0 then
   Result.Right := FixedGraphWidth;
  if FixedGraphHeight <> 0 then
   Result.Bottom := FixedGraphHeight;
end;

function TChartWriter.GetGraphWidth: integer;
begin
  Result := CWBoundsRect.Width - Chart.InnerMargins.Right - Chart.InnerMargins.Left - FRightSpace -
    SpaceOf(apRight) - SpaceOf(apLeft);
end;

function TChartWriter.GetGraphHeight: integer;
begin
  Result := CWBoundsRect.Height - Chart.InnerMargins.Bottom - Chart.InnerMargins.Top - FBottomSpace -
    SpaceOf(apBottom) - SpaceOf(apTop);
end;

function TChartWriter.GetGraphLeft: integer;
begin
  Result := Chart.InnerMargins.Left + SpaceOf(apLeft);
end;

function TChartWriter.GetGraphTop: integer;
begin
  Result := Chart.InnerMargins.Top + SpaceOf(apTop);
end;


function TChartWriter.GetMouseTimeFormat: string;
begin
  if Chart <> nil then
    Result := Chart.MouseTimeFormat;
end;

procedure TChartWriter.SetMouseTimeFormat(const Value: string);
begin
  if Chart <> nil then
    Chart.MouseTimeFormat := Value;
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

procedure TChartWriter.SetNameLabelFreq(const Value: integer);
begin
  if (Value < 1) or (Value = FNameLabelFreq) then
    Exit;
  FNameLabelFreq := Value;
  Invalidate;
end;

procedure TChartWriter.SetValueLabelFreq(const Value: integer);
begin
  if (Value < 1) or (Value = FValueLabelFreq) then
    Exit;
  FValueLabelFreq := Value;
  Invalidate;
end;

procedure TChartWriter.SetNameUnit(const Value: integer);
begin
  if (Value < 1) or (Value = FNameUnit) then
    Exit;
  FNameUnit := Value;
  if not(csLoading in componentState) then
  begin
    RestrictToClient(False);
    SetLabelFreqs;
  end;
  Invalidate;
end;

procedure TChartWriter.SetValueSpan(LowValue, HighValue: single);
begin
   if (Chart <> nil) then
  begin
    ActiveValueScale.SetValueSpan(LowValue, HighValue);
  end;
end;

procedure TChartWriter.SetHighLow;
{ Detects high and low values when ValueSpanFromData is set }
begin
  if Count = 0 then
    Exit;

  if (Chart is TCWSpanChart) then
  begin
     Chart.ValueScale1.SetHighLow;
  end;
end;

procedure TChartWriter.SetValuePrecision(const Value: integer);
begin
    if (Chart <> nil) then
  begin
    if ActiveGraph is TCWPie then
      TCWPie(ActiveGraph).FValuePrecision := Value
    else
     ActiveValueScale.FValuePrecision := Value;
  end;
end;

procedure TChartWriter.SetAxisOrientation(const Value: TAxisOrientation);
begin
   if Chart = nil then
     Exit;
   if IsAxisChart then
     Chart.AxisOrientation := Value
   else
     ShowGWError(msg_PrivateProp, C_AxisOrientation);
end;

procedure TChartWriter.SetBezierMargin(const Value: integer);
begin
  if (Value < 0) or (Value > 50) or (Value = FBezierMargin) then
    Exit;
  FBezierMargin := Value;
  if csLoading in ComponentState then
    Exit;
  if (Chart <> nil) and (Chart.IsActive) then
    RefreshChart;
end;

function TChartWriter.GetValueScale1 : TCWValueScale;
begin
   Result := nil;
   if Chart = nil then
     Exit;
   if IsAxisChart then
    Result := Chart.ValueScale1
   else
    ShowGWError(msg_PrivateProp, C_ValueScale1);
end;

function TChartWriter.GetValueScale2 : TCWValueScale;
begin
   Result := nil;
   if Chart = nil then
     Exit;
   if not (Chart is TCWCategoryChart) then
    Result := Chart.ValueScale2
   else
    ShowGWError(msg_PrivateProp, C_ValueScale2);
end;

function TChartWriter.GetNameScale : TCWNameScale;
begin
   Result := nil;
   if Chart = nil then
     Exit;
   if IsAxisChart then
    Result := Chart.NameScale
   else
    ShowGWError(msg_PrivateProp, C_NameScale);
end;


function TChartWriter.GetValuePrecision: integer;
begin
  Result := 0;
  if (Chart <> nil) then
  begin
    if ActiveGraph is TCWPie then
      Result := TCWPie(ActiveGraph).ValuePrecision
    else
      Result := ActiveValueScale.ValuePrecision;
  end;
end;

function TChartWriter.GetValueFloatUnit : single;
begin
  Result := 0;
  if Chart = nil then
   Exit;
  if IsAxisChart then
  begin
    if FActiveValAx = FValueAxis then
      Result := Chart.ValueScale1.ValueFloatUnit
    else
      Result := Chart.ValueScale2.ValueFloatUnit;
  end
  else
   ShowGWError(msg_PrivateProp, C_ValueFloatUnit);
end;

function TChartWriter.DetectSpanType(SeriesIndex: integer): TSpanType;
var
  S1, S2: string;
  Dt1, Dt2: TDateTime;
  N1, N2: integer;
  Y, M, D, H, MM, Sec, Ms: word;
  y2, M2, D2, H2, Mm2, Sec2, Ms2: word;
begin
  Result := ntDateSpan;
  if (Chart is TCWCategoryBarChart) or (Chart is TCWPieChart) or (Chart is TCWGeneralChart)then
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
      begin
        if  (D=1) and (D2 = 1) then
          Result := ntMonthSpan
        else
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

function TChartWriter.GetSeriesAttributes(ASeries : TStringList; Timetype : TSaveTimeType;
 var Intervals : integer): TDataAttributes;
var
  i : integer;
  S1, S2 : string;
  N1, N2 : integer;
  Dt1, Dt2 : TDateTime;
  Inter : integer;
  Y, M, D, H, MM, Sec, Ms: word;
  TimeOK : Boolean;
  HV, LV, V : single;

  function GetTimeInterval(TSpan : TSpanType; D1, D2 : TDateTime) : integer;
  begin
   if Tspan = ntMonthSpan then
       Result := MonthsBetweenEx(Dt2, Dt1)
   else if Tspan = ntDateSpan then
       Result := DaysBetween(Dt2, Dt1)
    else if TSpan = ntHourSpan then
       Result := HoursBetween(Dt2, Dt1)
    else if TSpan = ntMinuteSpan then
       Result := MinutesBetween(Dt2, Dt1)
    else
       Result := SecondsBetween(Dt2, Dt1);
  end;

begin
   Result.NameType := ntGeneral;
   Inter := 0;
   if ASeries.Count < 2 then
   begin
     Exit;
   end;
   S1 := Aseries.Names[0];
   S2 := Aseries.Names[1];
   HV := StrToFloat(ASeries.ValueFromIndex[0], Fmt) / 1000;
   LV := HV;
   V := StrToFloat(ASeries.ValueFromIndex[1], Fmt) / 1000;
   if V > HV then
     HV := V;
   if V < LV then
     LV := V;

   if not (TimeType in [ttUnix, ttISO8601]) then
   begin
    if TryStrToInt(S1, N1) then
    begin
      if TryStrToInt(S2, N2) then
      {Possibly numberspan}
      begin
        Inter := N2-N1;
        Result.NameType := ntNumberSpan;
        for i := 2 to ASeries.Count-1 do
        begin
           N1 := N2;
           S2 := Aseries.Names[i];
           V := StrToFloat(ASeries.ValueFromIndex[i], Fmt) / 1000;
           if V > HV then
            HV := V;
           if V < LV then
            LV := V;
           if Result.NameType = ntGeneral then
             Continue;
           if not TryStrToInt(S2, N2) then
           begin
             Result.NameType := ntGeneral;
           end;
           if N2-N1 <> Inter then
           begin
             Result.NameType := ntGeneral;
           end;
        end;
      end;
      Exit;
    end;
   end;

   TimeOk := false;
   if TimeType = ttUnix then
   begin
     Dt1 := UnixToDateTime(StrToInt(S1));
     Dt2 := UnixToDateTime(StrToInt(S2));
     TimeOK := True;
   end
   else if TimeType = ttISO8601 then
   begin
     Dt1 := ISO8601ToDate(S1);
     Dt2 := ISO8601ToDate(S2);
     TimeOK := True;
   end
   else
   begin
     if TryStrToDateTime(S1, Dt1, Fmt) then
       if TryStrToDateTime(S2, Dt2, Fmt) then
         TimeOK := True;
   end;
   if TimeOK then
   begin
      {Possibly time span}
      DecodeDateTime(Dt1, Y, M, D, H, MM, Sec, Ms);
      if (H = 0) and (MM = 0) and (Sec = 0) and (Ms = 0) then
        { Looks like a date span or a month span}
      begin
         if D = 1 then
         begin
          DecodeDateTime(Dt2, Y, M, D, H, MM, Sec, Ms);
          if D = 1 then
            Result.NameType := ntMonthSpan
          else
            Result.NameType := ntDateSpan;
         end
         else
          Result.NameType := ntDateSpan;
      end
      else if (H <> 0) and (MM = 0) and (Sec = 0) and (Ms = 0) then
        Result.NameType := ntHourSpan
      else if (MM <> 0) and (Sec = 0) and (MS = 0) then
        Result.NameType := ntMinuteSpan
      else if (Sec <> 0) and (MS = 0) then
        Result.NameType := ntSecondSpan;

      Inter := GetTimeInterval(Result.NameType, Dt2, Dt1);

      for i := 2 to ASeries.Count-1 do
      begin
       Dt1 := Dt2;
       S2 := Aseries.Names[i];
       V := StrToFloat(ASeries.ValueFromIndex[i], Fmt) / 1000;
       if V > HV then
         HV := V;
       if V < LV then
         LV := V;
       if Result.NameType = ntGeneral then
         Continue;
       if TimeType = ttUnix then
        Dt2 := UnixToDateTime(StrToInt(S2))
       else if TimeType = ttISo8601 then
        Dt2 := ISO8601ToDate(S2)
       else if not TryStrToDateTime(S2, Dt2) then
       begin
         Result.NameType := ntGeneral;
       end;
       if GetTimeInterval(Result.Nametype, Dt2, Dt1) <> Inter then
       begin
         Result.NameType := ntGeneral;
       end;
      end;
   end
   else
   begin
     for i := 2 to ASeries.Count-1 do
     begin
        S1 := Aseries.Names[i];
        V := StrToFloat(ASeries.ValueFromIndex[i], Fmt) / 1000;
        if V > HV then
          HV := V;
        if V < LV then
          LV := V;
     end;
   end;
   Intervals := Inter;
   Result.LowValue := LV;
   Result.HighValue := HV;
end;

function TChartWriter.GetNameSectionDefs: TCWNameSectionDefs;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  if IsAxisChart then
    Result := Chart.NameSectionDefs
  else
    ShowGWError(msg_PrivateProp, C_NameSectionDefs);
end;

procedure TChartWriter.SetNameSectionDefs(const Value: TCWNameSectionDefs);
begin
  if Chart = nil then
    Exit;
  if IsAxisChart then
    Chart.NameSectionDefs := Value
  else
    ShowGWError(msg_PrivateProp, C_NameSectionDefs);
end;

function TChartWriter.GetValueSectionDefs: TCWValueSectionDefs;
begin
  Result := nil;
  if Chart = nil then
    Exit;
  if IsAxisChart then
    Result := Chart.ValueSectionDefs
  else
    ShowGWError(msg_PrivateProp, C_ValueSectionDefs);
end;

procedure TChartWriter.SetValueSectionDefs(const Value: TCWValueSectionDefs);
begin
  if Chart = nil then
    Exit;
  if IsAxisChart then
    Chart.ValueSectionDefs := Value
  else
    ShowGWError(msg_PrivateProp, C_ValueSectionDefs);
end;

procedure TChartWriter.SetGraphBGColor(const Value: TColor);
begin
  if Chart <> nil then
    Chart.GraphBGColor := Value;
end;

function TChartWriter.GetGraphBGColor : TColor;
begin
  Result := clWindow;
  if Chart <> nil then
    Result := Chart.GraphBGColor;
end;

procedure TChartWriter.SetCentered(const Value: Boolean);
begin
  if Value = FCentered then
    Exit;
  FCentered := Value;
  if not (csLoading in ComponentState) then
   RefreshChart;
end;

procedure TChartWriter.SetChartAs(AChart : TCWChart; AsGraph : TCWGraph);
var
  Replaced : Boolean;
begin
   if AChart = nil then
   begin
     Chart := nil;
     Exit;
   end;

   if (AChart = Chart) and (Chart <> nil)
    and ((Chart.AllEqual = AsGraph) or (Chart.AllEqual = nil)) then
    {If not AllEqual there wil be no way back to the originals}
     Exit;

   Replaced := false;
   BeginUpdate;
   try

     ChartList.FPrevChart.FChart := FChart;
     if FChart <> nil then
      ChartList.FPrevChart.FGraph := FChart.AllEqual
     else
      ChartList.FPrevChart.FGraph := nil;
     AChart.FWriter := Self;
     if AChart.Writer = nil then
     begin
       AChart.InnerMargins.FWriter := AChart.Writer;
       AChart.GraphMargins.FWriter := AChart.Writer;
     end;

     ResetIds;
     AChart.ReplaceGraphs(AsGraph);
     if AChart = Chart then
     begin
      Chart.FHasAnimated := False;
      CreateIDS;
      if Count = 0 then
        Exit;
       if (Contraction <> 1) or (AChart.NameScale.OverflowAction = ovScrolling) then
         Reload
       else
       begin
         RefreshChart;
         if ActiveGraph is TCWPie then
            CorrectPie;
       end;
       Replaced := True;
       Exit;
     end;
     Chart := AChart;
   finally
     EndUpdate;
     if Replaced then {Else SetChart triggers the event}
     begin
       ChartList.FItemIndex := ChartList.IndexOf(Chart, AsGraph);
        if Assigned(ChartList.FOnChange) then
         ChartList.FOnChange(ChartList);
     end;
   end;

end;

procedure TChartWriter.SetChart(const Value: TCWChart);
var
  Msg : TMessage;
  M : TMSG;
  G1, G2 : TCWGrapH;
begin
  if Value = FChart then
    Exit;
  if csLoading in ComponentState then
  begin
    FChart := Value;
    FChart.FHasAnimated := false;
    if FChart <> nil then
    begin
      SetPosition(Value.AxisOrientation);
      FChart.FreeNotification(Self);
      PostMessage(Handle, WM_LOADFILE, 1, 0);
      FNeedsIDs := True;
      FChart.FWID := AddToWList(Self,  Value);
      FChart.FWriter:= Self;
      FChart.InnerMargins.FWriter := FChart.Writer;
      FChart.GraphMargins.FWriter := FChart.Writer;
    end;
    Exit;
  end;

  if Value <> nil then
  begin
    if (Value.SeriesDefs.Count = 0) then
      ShowGWError(msg_NoActiveGraph)
    else
    begin
      Value.GetUniqueGraphs(G1, G2);
      if G1 = nil then
        ShowGWError(msg_NoActiveGraph);
    end;
  end;

  if (Value <> nil) and (Value.FileName <> '') and not Value.IsCached then
  begin
    if GetFileType(Value.FileName) <> 1 then
      ShowGWError(msg_RichDesign)
  end;

  if not PeekMessage(M, Handle, WM_LOADFILE, WM_LOADFILE, PM_REMOVE)
  {The user might have changed the original chart}
  and (FChart <> nil) then
  begin
    FChart.ClearCache;
    FChart.SaveCache;
    ChartList.FPrevChart.FChart := FChart;
    ChartList.FPrevChart.FGraph := FChart.AllEqual;
  end
  else
  begin
    ChartList.FPrevChart.FChart := nil;
    ChartList.FPrevChart.FGraph := nil;
  end;

  if FChart <> nil then
    ResetIds;
  FChart := Value;
  if FChart <> nil then
  begin
   FChart.FWriter := self;
   if FChart.FZoomStart <> -1 then
     SetState(stZoomed)
   else
     ClearState(stZoomed);
   if (Chart is TCWCategoryBarChart) then
   begin
     Chart.NameScale.FOverflowAction := ovNone;
   end;

   try
    CreateIDS;
   except
    FChart := nil;
    GoBackError;
    raise;
   end;
   FChart.FWID := AddToWList(Self,  Value);
   FChart.FWriter := Self;
   FChart.InnerMargins.FWriter := FChart.Writer;
   FChart.GraphMargins.FWriter := FChart.Writer;

   SetPosition(Value.AxisOrientation);
   FChart.FHasAnimated := false;
   FChart.FreeNotification(Self);
   if not (csDesigning in ComponentState) or ((csDesigning in ComponentState) and LiveGraphs) then
   begin
     if Chart.IsCached then
     begin
       LoadFromCache;
       Execute;
     end
     else if (FChart.Dataset <> nil) or (FChart.FileName <> '')
     or ((csDesigning in ComponentState) and LiveGraphs) then
     begin
      WMLOADFILE(Msg);
     end
     else if Assigned(FChart.FOnGetData) then
     begin
       Clear;
       FChart.FOnGetData(Chart);
       Chart.ClearCache;
       Chart.SaveCache;
     end
     else
     begin
       Clear;
     end;
   end;
  end
  else
  begin
    ChartList.FItemIndex := -1;
    Clear;
    if Assigned(ChartList.FOnChange) then
      ChartList.FOnChange(ChartList);
    Exit;
  end;

  if Chart is TCWPieChart then
  begin
    if TCWPie(ActiveGraph).Style = psDisc then
      CorrectPie
    else
      DoRepaint;
  end
  else
    DoRepaint;

  if ChartList.IndexOf(FChart, ActiveGraph) = -1 then
    ChartList.FItemIndex := ChartList.IndexOf(FChart, nil)
  else
    ChartList.FItemIndex := ChartList.IndexOf(FChart, ActiveGraph);
  if Assigned(ChartList.FOnChange) then
    ChartList.FOnChange(ChartList);
end;

procedure TChartWriter.SetPosition(const Orientation: TAxisOrientation);
begin
  if Orientation = alBottomLeft then
  begin
    FNameAxis.Position := apBottom;
    FValueAxis.Position := apLeft;
    FValueAxis2.Position := apRight;
  end
  else if Orientation = alBottomRight then
  begin
    FNameAxis.Position := apBottom;
    FValueAxis.Position := apRight;
    FValueAxis2.Position := apLeft;
  end
  else if Orientation = alLeftTop then
  begin
    FNameAxis.Position := apLeft;
    FValueAxis.Position := apTop;
    FValueAxis2.Position := apBottom;
  end
  else if Orientation = alTopLeft then
  begin
    FNameAxis.Position := apTop;
    FValueAxis.Position := apLeft;
    FValueAxis2.Position := apRight;
  end
  else if Orientation = alTopRight then
  begin
    FNameAxis.Position := apTop;
    FValueAxis.Position := apRight;
    FValueAxis2.Position := apLeft;
  end
  else if Orientation = alRightTop then
  begin
    FNameAxis.Position := apRight;
    FValueAxis.Position := apTop;
    FValueAxis2.Position := apBottom;
  end
  else if Orientation = alLeftBottom then
  begin
    FNameAxis.Position := apLeft;
    FValueAxis.Position := apBottom;
    FValueAxis2.Position := apTop;
  end
  else if Orientation = alRightBottom then
  begin
    FNameAxis.Position := apRight;
    FValueAxis.Position := apBottom;
    FValueAxis2.Position := apTop;
  end;
end;

procedure TChartWriter.SetRulers(const Value: TRulers);
begin
  if Value = FRulers then
    Exit;
  FRulers := Value;
  FRulerVisible := False;
  Invalidate;
end;

function TChartWriter.GetWallWidth: integer;
begin
  Result := 0;
  if Chart = nil then
    Exit;
  if not IsAxisChart then
    ShowGWError(msg_PrivateProp, C_WallWidth);
  if HasWall then
    Result := Chart.WallWidth;
end;

procedure TChartWriter.SetWallWidth(const Value: integer);
begin
  if Chart = nil then
    Exit;
  if not IsAxisChart then
    ShowGWError(msg_PrivateProp, C_WallWidth);
  Chart.WallWidth := Value;
end;

function TChartWriter.GetAxisOrientation : TAxisOrientation;
begin
  Result := alBottomLeft;
  if Chart = nil then
    Exit;
  if not IsAxisChart then
    ShowGWError(msg_PrivateProp, C_AxisOrientation);
   Result := Chart.AxisOrientation;
end;

function TChartWriter.GetScaleExtension : integer;
{Called from SpaceOf apBottom/apTop when space is 0. Adds a little margin at the bottom,
to ensure that the value scale is fully visible}
begin
  Result := 0;
  if Chart = nil then
    Exit;
  if FNameAxis.IsXAxis then
  begin
    Result := GetTextHeight(lkValue);
  end;
end;

function TChartWriter.RulerPoints: TRulerPoints;
var
  Vert: Boolean;
  Defs: TSeriesInfoItems;
  P: TPoint;
  i: integer;
begin
  Finalize(Result);
  if (Rulers = ruNone) or (Rulers = ruValues) then
    Exit;
  Vert := FNameAxis.IsXAxis;
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
  for i := 0 to Chart.FOrigData[0].Count - 1 do
  begin
    S := Chart.FOrigData[0].FSeriesItems[i].FName;
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
  if not IsTimeSpan then
    Exit;
  for i := 0 to Chart.FOrigData[0].Count - 1 do
  begin
    Dt := Chart.FOrigData[0].FSeriesItems[i].FRealDate;
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
  if not IsTimeSpan then
    Exit;
  for i := 0 to Chart.FOrigData[0].Count - 1 do
  begin
    Dt := Chart.FOrigData[0].FSeriesItems[i].FRealDate;
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
  SeriesDefs: TSeriesInfoItems;
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
  SeriesDefs: TSeriesInfoItems;
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
  Defs: TSeriesInfoItems;
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
    if not PointsFromRuler(FNameAxis.IsXAxis, APos.X, APos.Y, P, Defs) then
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
      Ang := DegToRad(FSeriesData[i].FTrackPies[j].EndAngle);
      LastAng := DegToRad(FSeriesData[i].FTrackPies[j].StartAngle);
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
    DoContraction(Rate, ContractType);
  finally
    ClearState(stUserContraction);
  end;
end;

procedure TChartWriter.DoRepaint;
var
  vm: TViewMode;
begin

  if csDestroying in componentState then
    Exit;

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

  if (InState(stAnimating)) and not (csDesigning in ComponentState) then
  begin
    DoAnimation;
    Exit;
  end;

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

procedure TChartWriter.DoContraction(Rate: integer;
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
    if IsTimeSpan then
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
  if Chart.GetNameType in [ntCategory, ntGeneral] then
    ShowGWError(msg_ContractCategory);
  if (Rate = 1) and not(ContractType = ctIncremental) and not InState(stUserContraction) then
  begin
    ClearState(stInternalContraction);
    ClearState(stUserContraction);
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
      if (Rate < FMinContraction) and (ContractType = ctExplicit) and not InState(stInternalContraction) then
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

function TChartWriter.CanContract : integer;
begin
    Result := 0;
    if Chart = nil then
      Result := con_NoChart
    else if Chart.GetNameType in [ntCategory, ntGeneral] then
      Result := con_SpanChartOnly
    else if Chart.NameScale.OverflowAction = ovScrolling then
    begin
    if CanScroll(stNext) or CanScroll(stPrev) then
      Result:= con_Paged;
    end;
end;

procedure TChartWriter.SetContraction(const Value: integer);
begin
  if Chart.GetNameType in [ntCategory, ntGeneral] then
    ShowGWError(msg_ContractCategory);

  if Chart.NameScale.OverflowAction = ovScrolling then
  begin
    if CanScroll(stNext) or CanScroll(stPrev) then
      ShowGWError(msg_ContractPaged);
  end;

  if Value < FMinContraction then
    ShowGWError(msg_DecreaseContraction);
  if (Value < 0) or (FSeriesData.Count = 0) or InState(stLimbo) then
    Exit;
  FViewMode := vmNormal;
  FDynaSectStart := -1;
  FDynaSectEnd := -1;
  SetState(stUserContraction);
  try
    DoContraction(Value, FContractionType);
    FUserContraction := Value;
  finally
    ClearState(stUserContraction);
  end;
  DoRepaint;
end;

function TChartWriter.XFromName(AValue: string): integer;
{ Returns the X-coordinate of AValue }
var
  P: TPoint;
begin
  P := FLongestSeries.PosFromNamVal(AValue, ActiveValueScale.ValueLow);
  Result := P.X;
end;

function TChartWriter.YFromName(AValue: string): integer;
{ Returns the Y-coordinate of AValue }
var
  P: TPoint;
begin
  P := FLongestSeries.PosFromNamVal(AValue, ActiveValueScale.ValueHigh);
  Result := P.Y;
end;

function TChartWriter.GetGraphPrintRect: TRect;
{ GraphRect - GraphMargs }
var
  BM : integer;
  G : TCWGraph;
begin
  BM := 0;
  G := InView(TCWCurve);
  if G <> nil then
    if (G is TCWCurve) and (TCWCurve(G).LineShape = lsBezier) then
      BM := BezierMargin;
  Result := GraphRect;
  Result.Left := Result.Left + Chart.GraphMargins.Left;
  Result.Top := Result.Top + Chart.GraphMargins.Top + BM;
  Result.Right := Result.Right - Chart.GraphMargins.Right;
  Result.Bottom := Result.Bottom - Chart.GraphMargins.Bottom - BM;
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
  if FNameAxis.IsXAxis then
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
  Result := FNameAxis.PosFromDate(ADate);
end;

function TChartWriter.PosFromHour(ADate: TDateTime): integer;
begin
  Result := FNameAxis.PosFromHour(ADate);
end;

function TChartWriter.PosFromMinute(ADate: TDateTime): integer;
begin
  Result := FNameAxis.PosFromMinute(ADate);
end;

function TChartWriter.PosFromSecond(ADate: TDateTime): integer;
begin
  Result := FNameAxis.PosFromSecond(ADate);
end;

function TChartWriter.PosFromNumber(ANumber: single): integer;
begin
  Result := FNameAxis.PosFromNumber(ANumber);
end;

function TChartWriter.GetActiveValAx : TCWValueScale;
begin
   Result := nil;
   if Chart = nil then
     Exit;
   if FActiveValAx = FValueAxis then
     Result := Chart.ValueScale1
   else
     Result := Chart.ValueScale2;
end;

procedure TChartWriter.CreateIDS;
var
  C : TCWChart;
  i : integer;
begin
   //DeleteFromWList(Self);
   C := Chart;
   if C = nil then
     Exit;
   for I := 0 to C.Legends.Count-1 do
     begin
       if C.Legends.Items[i].Legend <> nil then
       begin
        C.Legends.Items[i].Legend.FWID := AddToWList(Self, C.Legends.Items[i].Legend);
        C.Legends.Items[i].Legend.FWriter := Self;
        C.FreeNotification(C.Legends.Items[i].Legend);
       end;
     end;
   for I := 0 to C.SeriesDefs.Count-1 do
   begin
    if C.SeriesDefs[i].Graph <> nil then
    begin
     C.SeriesDefs[i].Graph.FWID := AddToWList(Self, C.SeriesDefs[i].Graph);
     C.SeriesDefs[i].Graph.FWriter := Self;
     C.FreeNotification(C.SeriesDefs[i].Graph);
    end;
   end;
   if C.NameSectionDefs <> nil then
   begin
     C.NameSectionDefs.FWID := AddToWList(Self, C.NameSectionDefs);
     C.NameSectionDefs.FWriter := Self;
     C.FreeNotification(C.NameSectionDefs);
   end;
   if C.ValueSectionDefs <> nil then
   begin
     C.ValueSectionDefs.FWID := AddToWList(Self, C.ValueSectionDefs);
     C.ValueSectionDefs.FWriter := Self;
     C.FreeNotification(C.ValueSectionDefs);
   end;
   C.FWID := AddToWList(Self, C);
   C.FWriter := Self;
end;

procedure TChartWriter.ResetIDS;
var
  C : TCWChart;
  i : integer;
begin
   C := Chart;
   if C = nil then
     Exit;
   for I := 0 to C.Legends.Count-1 do
     begin
       if C.Legends.Items[i].Legend <> nil then
       begin
        C.Legends.Items[i].Legend.FWriter := nil;
       end;
     end;
   for I := 0 to C.SeriesDefs.Count-1 do
   begin
    if C.SeriesDefs[i].Graph <> nil then
    begin
     C.SeriesDefs[i].Graph.FWriter := nil;
    end;
   end;
   if C.NameSectionDefs <> nil then
   begin
     C.NameSectionDefs.FWriter := nil;
   end;
   if C.ValueSectionDefs <> nil then
   begin
     C.ValueSectionDefs.FWriter := nil;
   end;
   C.FInnerMargins.FWriter := nil;
   C.FGraphMargins.FWriter := nil;
end;

procedure TChartWriter.AddDsgnSeries(ASpanType : TSpanType; indx : integer);
var
 sl : TStringList;

 function RandomValue(L, H : single) : single;
begin
  Result := Random * (H - L) + L;
end;

function GetFlowValue(ItmCnt, ItmIndex : integer; var Cnt : integer) : single;
var
   SerCnt : integer;
   V : single;
   Span : single;
   Start : single;
   L, H : single;
   VScale : TCWValueScale;
 begin
    SerCnt := Chart.SeriesDefs.Count;
    if Chart.SeriesDefs[Indx].ValueScale = vsValueScale1 then
      VScale := Chart.ValueScale1
    else
      VScale := Chart.ValueScale2;
    if not VScale.ValueSpanFromData then
    begin
      Span := VScale.ValueHigh - VScale.ValueLow;
      L := VScale.ValueLow;
      H := VScale.ValueHigh;
    end
    else
    begin
      Span := 30;
      L := 0;
      H := 30;
    end;
    Start := L + (SerCnt-Indx) * (Span* 0.05);
    if ItmIndex < Round(ItmCnt*70/100) then {Increasing}
      Cnt := ItmIndex
    else
    begin
      dec(Cnt); {Declining}
      if Cnt <= 0 then
      Cnt := 1;
    end;

    V := Cnt / ItmCnt * 100; {pst}
    V := Span* V /100 + Start; {value}
    V := RandomValue(V-V*0.05, V + V*0.05);
    if V < L then
     V := L
    else if V > H then
      V := H;
    Result := V;
  end;

 procedure AddCategories;
 var
   i : integer;
   Cnt : integer;
   V : single;
   S : string;
 begin
   if Chart.Categories.Count > 1 then
     Cnt := Chart.Categories.Count
   else
     Cnt := 10;

   for I := 1 to Cnt do
     begin
        V := Indx*5 + Random(10);
        if V < 2 then
        V := 2;
        S := FLoatToStr(V,Fmt);
        if (Chart.Categories.Count > 1) and (Chart.Categories.Items[i-1].FCategoryName <> '') then
          sl.Values[Chart.Categories.Items[i-1].FCategoryName] := S
        else
          sl.Values[Chr(i+64)] := S;
     end;
 end;


 procedure AddGeneral;
 var
   i : integer;
   x : integer;
   V : single;
   Cnt : integer;
 const
   ItmCnt = 50;
 begin
    X := 10;
    for I := 1 to ItmCnt do
     begin
        V := GetFlowValue(ItmCnt, I, Cnt);
        sl.Values[IntToStr(X)] := FloatToStr(V, Fmt);
        if Odd(I) then
          X := X + 1 + 10
        else
          X := X  + 5 + 10;
     end;
 end;

 procedure AddDateSpan;
 var
   i : integer;
   Dt : TDateTime;
   V : single;
   Cnt : integer;
 const
   ItmCnt = 182;
 begin
    Dt := ToDay-365;
    for I := 1 to ItmCnt do
     begin
        V := GetFlowValue(ItmCnt, I, Cnt);
        sl.Values[DateToStr(Dt, Fmt)] := FloatToStr(V, Fmt);
        Dt := Dt + 1;
     end;
 end;

 procedure AddMonthSpan;
 var
   i : integer;
   Dt : TDateTime;
   Y, M, D : Word;
   Cnt : integer;
   V : single;
 const
   ItmCnt = 12;
 begin
    DecodeDate(Today, Y, M, D);
    Dt := EncodeDate(Y, M, 1);
    for I := 1 to ItmCnt do
     begin
        V := GetFlowValue(ItmCnt, I, Cnt);
        sl.Values[DateToStr(Dt, Fmt)] := FloatToStr(V, Fmt);
        DecodeDate(Dt, Y, M, D);
        Inc(M);
        if M = 13 then
        begin
          M := 1;
          Inc(Y);
        end;
        Dt := EncodeDate(Y, M, D);
     end;
 end;

 procedure AddHourSpan;
 var
   i : integer;
   Dt : TDateTime;
   Y,M,D,H,MM,S,MS : word;
   V : single;
   Cnt : integer;
 const
   ItmCnt = 24;
 begin
    Dt := Now;
    DecodeDateTime(Dt, Y, M, D, H, MM, S, MS);
    Dt := EncodeDateTime(Y, M, D, H, 0, 0, 0);
    IncHour(Dt, -24);
    for I := 1 to ItmCnt do
     begin
        V := GetFlowValue(ItmCnt, I, Cnt);
        sl.Values[DateToStr(Dt, Fmt)] := FloatToStr(V, Fmt);
        Dt := IncHour(Dt);
     end;
 end;

procedure AddMinuteSpan;
 var
   i : integer;
   Dt : TDateTime;
   Y,M,D,H,MM,S,MS : word;
   Cnt : integer;
   V : single;
 const
   ItmCnt = 60;
 begin
    Dt := Now;
    DecodeDateTime(Dt, Y, M, D, H, MM, S, MS);
    Dt := EncodeDateTime(Y, M, D, H, MM, 0, 0);
    IncMinute(Dt, -60);
    for I := 1 to 60 do
     begin
        V := GetFlowValue(ItmCnt, I, Cnt);
        sl.Values[DateToStr(Dt, Fmt)] := FloatToStr(V, Fmt);
        Dt := IncMinute(Dt);
     end;
 end;

 procedure AddSecondSpan;
 var
   i : integer;
   Dt : TDateTime;
   Y,M,D,H,MM,S,MS : word;
   Cnt : integer;
   V : single;
 const
   ItmCnt = 60;
 begin
    Dt := Now;
    DecodeDateTime(Dt, Y, M, D, H, MM, S, MS);
    Dt := EncodeDateTime(Y, M, D, H, MM, S, 0);
    IncSecond(Dt, -60);
    for I := 1 to ItmCnt do
     begin
        V := GetFlowValue(ItmCnt, I, Cnt);
        sl.Values[DateToStr(Dt, Fmt)] := FloatToStr(V, Fmt);
        Dt := IncSecond(Dt);
     end;
 end;

 procedure AddNumberSpan;
var
   i : integer;
   Cnt : integer;
   V : single;
const
  ItmCnt = 50;
 begin
    for I := 1 to ItmCnt do
     begin
        V := GetFlowValue(ItmCnt, I, Cnt);
        sl.Values[IntToStr(i)] := FloatToStr(V, Fmt);
      end;
 end;

begin
  sl := TStringList.Create;
  sl.DefaultEncoding := TEncoding.Utf8;
  if Chart is TCWCategoryChart then
    AddCategories
  else if Chart is TCWGeneralChart then
    AddGeneral
  else
  case ASpanType of
    ntMonthSpan: AddMonthSpan;
    ntDateSpan: AddDateSpan;
    ntHourSpan: AddHourSpan;
    ntMinuteSpan: AddMinuteSpan;
    ntSecondSpan: AddSecondSpan;
    ntNumberSpan: AddNumberSpan;
  end;
  FDsgnData.Add(sl);
end;

procedure TChartWriter.RenderDesigner(Deleted : integer = 0);
var
  i : integer;
  Err : Boolean;

begin
   if not LiveGraphs then
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
       AddDsgnSeries(Chart.SpanType, I);
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
//System.ReportMemoryLeaksOnShutdown := True;
StrictProtection := True;

finalization
WriterList.Free;

end.

