unit form_New;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, System.Actions, Vcl.ActnList, ChartWriter, Vcl.ComCtrls;

type
  TSeriesData = class
    Title : string;
    Color : TColor;
    Graph : string;
    ValueScale : integer;
  end;

  TCatData = class
    Name : string;
    Color : TColor;
  end;

  Tfrm_New = class(TForm)
    bOK: TButton;
    Button1: TButton;
    Shape1: TShape;
    sb: TStatusBar;
    ActionList1: TActionList;
    acAdd: TAction;
    acUpdate: TAction;
    acDelete: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    pc: TPageControl;
    tabSeries: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    sbMoveUp: TSpeedButton;
    sbMoveDown: TSpeedButton;
    Label6: TLabel;
    cbGraphs: TComboBox;
    lbSeries: TListBox;
    eSeriesTitle: TLabeledEdit;
    cbSeriesColor: TColorBox;
    cbValueScale: TComboBox;
    cbAltGraph: TComboBox;
    tabGeneral: TTabSheet;
    Label5: TLabel;
    eTitle: TLabeledEdit;
    cbChartType: TComboBox;
    sbAdd: TSpeedButton;
    sbUpdate: TSpeedButton;
    sbDelete: TSpeedButton;
    tabCats: TTabSheet;
    lbItems: TListBox;
    eItemName: TEdit;
    cbChartItemColor: TColorBox;
    Label9: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    sbAddCat: TSpeedButton;
    sbUpdateCat: TSpeedButton;
    sbDeleteCat: TSpeedButton;
    sbNext: TSpeedButton;
    sbPrevious: TSpeedButton;
    acAddCat: TAction;
    acUpdateCat: TAction;
    acDeleteCat: TAction;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    cbxCorrelation: TCheckBox;
    acNext: TAction;
    acPrev: TAction;
    acOK: TAction;
    cbxIncludeNSections: TCheckBox;
    cbxIncludeVSections: TCheckBox;
    eNumLegends: TEdit;
    Label10: TLabel;
    cbSpanType: TComboBox;
    Label11: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acAddUpdate(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure acUpdateUpdate(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveUpUpdate(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveDownUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbSeriesClick(Sender: TObject);
    procedure lbSeriesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbChartTypeChange(Sender: TObject);
    procedure acAddCatExecute(Sender: TObject);
    procedure acUpdateCatExecute(Sender: TObject);
    procedure acDeleteCatExecute(Sender: TObject);
    procedure acAddCatUpdate(Sender: TObject);
    procedure acUpdateCatUpdate(Sender: TObject);
    procedure acDeleteCatUpdate(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure cbxCorrelationClick(Sender: TObject);
    procedure cbAltGraphChange(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
    procedure acNextUpdate(Sender: TObject);
    procedure acPrevUpdate(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure acPrevExecute(Sender: TObject);
  private
    { Private declarations }
    FErrorText : string;
    procedure DoAdd;
    procedure DoUpdate;
    procedure DoDelete;
    procedure MoveUp;
    procedure MoveDown;
    function CanAdd : Boolean;
    function CanUpdate : Boolean;
    function CanDelete : Boolean;
    function CanMoveUp : Boolean;
    function CanMoveDown : Boolean;
    procedure AssembleData(D : TSeriesData);
    procedure AssembleCat(D : TCatData);
    function IsDataValid(Adding : Boolean) : boolean;
    function IsCatDupName(Adding : Boolean) : Boolean;
    procedure Fill;
    procedure FillCat;
    function HasAlike(Index : integer) : Boolean;
    procedure ClearSeries;
    procedure ClearCats;
    procedure InitSeries;
    procedure CheckEquality;
    function MainOK : Boolean;
    function SeriesOK : Boolean;
    function CatOK : Boolean;

  public
    { Public declarations }
    procedure Reset;
  end;
var
  frm_New: Tfrm_New;

const
 E_NoUpdates = '';
 C_Span = 0;
 C_General = 1;
 C_Pie = 2;
 C_Bar = 3;

 S_Month = 0;
 S_Date = 1;
 S_Hour = 2;
 S_Minute = 3;
 S_Second = 4;
 S_Number = 5;

implementation

{$R *.dfm}

const
 E_NoError = '';
 E_PieAlone = 'Pie cannot be mixed with other graph types';
 E_Scale1First = 'The first series must be assigned to Value scale 1';
 E_MissingSeriesTitle = 'Missing series title';
 E_MissingCategoryName = 'Missing category name';
 E_DupSeriesTitle = 'Duplicate series title';
 E_DupCatName = 'Duplicate category name';
 E_Relevant = 'Scale is relevant!';
 E_OneScale2 = 'You can only assign one graph to value scale 2';
 //E_NoUpdates = 'No updates available';


procedure Tfrm_New.acAddCatExecute(Sender: TObject);
var
  D : TCatData;
begin
  D := TCatData.Create;
  AssembleCat(D);
  lbItems.AddItem(D.Name, D);
  lbItems.ItemIndex := lbItems.Count-1;
end;

procedure Tfrm_New.acAddCatUpdate(Sender: TObject);
begin
  if Trim(eItemName.Text) = '' then
  begin
    acAddCat.Enabled := false;
    FErrorText := E_MissingCategoryName;
  end
  else if IsCatDupName(True) then
    acAddCat.Enabled := false
  else
    acAddCat.Enabled := True;

end;

procedure Tfrm_New.acAddExecute(Sender: TObject);
begin
  DoAdd;
end;

procedure Tfrm_New.acAddUpdate(Sender: TObject);
begin
 acAdd.Enabled := CanAdd;
end;

procedure Tfrm_New.acDeleteCatExecute(Sender: TObject);
var
  Indx : integer;
begin
  lbItems.Items.Objects[lbItems.ItemIndex].Free;
  Indx := lbItems.ItemIndex;
  lbItems.DeleteSelected;
  if Indx = lbItems.Count then
   Dec(Indx);
  lbItems.ItemIndex := Indx;

end;

procedure Tfrm_New.acDeleteCatUpdate(Sender: TObject);
begin
   acDeleteCat.Enabled := CanDelete;
end;

procedure Tfrm_New.acDeleteExecute(Sender: TObject);
begin
  DoDelete;
end;

procedure Tfrm_New.acDeleteUpdate(Sender: TObject);
begin
  acDelete.Enabled := CanDelete;
end;

procedure Tfrm_New.acMoveDownExecute(Sender: TObject);
begin
  MoveDown;
end;

procedure Tfrm_New.acMoveDownUpdate(Sender: TObject);
begin
  acMoveDown.Enabled := CanMoveDown;
end;

procedure Tfrm_New.acMoveUpExecute(Sender: TObject);
begin
  MoveUp;
end;

procedure Tfrm_New.acMoveUpUpdate(Sender: TObject);
begin
  acMoveUp.Enabled := CanMoveUp;
end;

procedure Tfrm_New.acNextExecute(Sender: TObject);
var
  CurTab : TTabSheet;
begin
   FErrorText := '';
   CurTab := pc.ActivePage;
   tabSeries.TabVisible := false;
   tabCats.TabVisible := false;
   tabGeneral.TabVisible := false;
   FErrorText := '';

   if CurTab = tabGeneral then
   begin
       tabSeries.TabVisible := True;
   end
   else if CurTab = tabSeries then
   begin
       tabCats.TabVisible := True;
   end;
end;

procedure Tfrm_New.acNextUpdate(Sender: TObject);
begin
   if pc.ActivePage = tabGeneral then
     acNext.Enabled := MainOK
   else if pc.ActivePage = tabSeries then
   begin
    if (cbChartType.ItemIndex in [c_Pie, c_Bar]) then
      acNext.Enabled := SeriesOK
    else
     acNext.Enabled := False;
   end
   else
    acNext.Enabled := False;
end;

procedure Tfrm_New.acOKUpdate(Sender: TObject);
begin
  acOK.Enabled := MainOk and SeriesOK and CatOK;
end;

procedure Tfrm_New.acPrevExecute(Sender: TObject);
var
  CurTab : TTabSheet;
begin
   CurTab := pc.ActivePage;
   tabSeries.TabVisible := false;
   tabCats.TabVisible := false;
   tabGeneral.TabVisible := false;

   if CurTab = tabSeries then
   begin
       tabGeneral.TabVisible := True;
   end
   else if CurTab = tabCats then
   begin
       tabSeries.TabVisible := True;
   end;
end;

procedure Tfrm_New.acPrevUpdate(Sender: TObject);
begin
   acPrev.Enabled := pc.ActivePage <> tabGeneral;
end;

procedure Tfrm_New.acUpdateCatExecute(Sender: TObject);
var
  D : TCatData;
begin
   D := lbItems.Items.Objects[lbItems.ItemIndex] as TCatData;
   AssembleCat(D);
   lbItems.Repaint;
end;

procedure Tfrm_New.acUpdateCatUpdate(Sender: TObject);
var
  D : TCatData;

begin
  if lbItems.ItemIndex = -1 then
   acUpdateCat.Enabled := false
  else if Trim(eItemName.Text) = '' then
  begin
    acUpdateCat.Enabled := false;
    FErrorText := E_MissingCategoryName;
  end
  else if IsCatDupName(False) then
  begin
    D := lbItems.Items.Objects[lbItems.ItemIndex] as TCatData;
    acUpdateCat.Enabled := D.Color <> cbChartItemColor.Selected;
  end
  else
    acUpdateCat.Enabled := True;

  sb.SimpleText := FErrorText;

end;

procedure Tfrm_New.acUpdateExecute(Sender: TObject);
begin
  DoUpdate;
end;

procedure Tfrm_New.acUpdateUpdate(Sender: TObject);
begin
    acUpdate.Enabled := CanUpdate;
    cbGraphs.Enabled := not (cbChartType.ItemIndex in [C_Pie, C_Bar]);
    cbAltGraph.Enabled := (lbSeries.Count > 0) and not (cbChartType.ItemIndex = C_Pie);
    sb.SimpleText := FErrorText;
end;

procedure Tfrm_New.AssembleData(D: TSeriesData);
begin
   with D do
   begin
     Title := eSeriesTitle.Text;
     Color := cbSeriesColor.Selected;
     Graph := cbGraphs.Text;
     if CbChartType.ItemIndex = c_Pie then
       ValueScale := -1
     else
       ValueScale := cbValueScale.ItemIndex;
   end;
end;

procedure Tfrm_New.AssembleCat(D: TCatData);
begin
   with D do
   begin
     Name := eItemName.Text;
     Color := cbChartItemColor.Selected;
   end;
end;

function Tfrm_New.IsCatDupName(Adding : Boolean) : Boolean;
var
  I, Indx : integer;
  D : TCatData;
begin
  Result := false;
  if Indx = -1 then
    Exit;
  Indx := lbItems.ItemIndex;
  for I := 0 to lbItems.Count-1 do
    begin
      D := lbItems.Items.Objects[i] as TCatData;
      if SameText(Trim(eItemName.Text), D.Name) then
      begin
        Result := True;
        if not Adding and (I <> Indx) then
          FErrorText := E_DupCatName;
      end;
    end;
end;

function Tfrm_New.IsDataValid(Adding : Boolean) : Boolean;
var
  i : integer;
  Indx : integer;
  D : TSeriesData;
  Err : Boolean;
begin
   Result := false;
   if Trim(eSeriesTitle.Text) = '' then
   begin
     FErrorText := E_MissingSeriesTitle;
     Exit;
   end;
   if (cbValueScale.ItemIndex = 2) and (cbGraphs.ItemIndex <> 2) then
   {Irrelevant only allowed with pies}
     Exit;

   if (cbGraphs.ItemIndex = 2)
   and (cbChartType.ItemIndex <> C_Pie)
   then
   begin
     FErrorText := E_PieAlone;
     Exit;
   end;



   if Adding and (lbSeries.Count = 0) then
   begin
     Result := True;
     FErrorText := E_NoError;
     Exit;
   end;


   Indx := lbSeries.ItemIndex;
   Err := false;
   for I := 0 to lbSeries.Count-1 do
   begin
       D := lbSeries.Items.Objects[i] as TSeriesData;
       if Adding and SameText(Trim(eSeriesTitle.Text), D.Title) then
       begin
         Err := True;
         FErrorText := E_DupSeriesTitle;
         Break;
       end;

       if i = indx then
         continue;

       if not Adding and SameText(Trim(eSeriesTitle.Text), D.Title) then
       begin
         Err := True;
         FErrorText := E_DupSeriesTitle;
         Break;
       end;

       if (cbValueScale.ItemIndex = 1) and (D.ValueScale = 1) then
       { Only on scale 2 allowed}
       begin
         Err := True;
         FErrorText := E_OneScale2;
         Break;
       end;

   end;
   if Err then
     Exit;

   if HasAlike(Indx) then
   begin
     FErrorText := E_NoUpdates;
     Exit;
   end;
   FErrorText := '';
   Result := True;
end;

procedure Tfrm_New.lbItemsClick(Sender: TObject);
begin
  FillCat;
end;

procedure Tfrm_New.lbItemsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  D : TCatData;
  R : TRect;
begin
   D := lbItems.Items.Objects[Index] as TCatData;
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
     Brush.Color := D.Color;
     R := Rect;
     R.Top := Rect.Top + 2;
     R.Bottom := Rect.Bottom-2;
     R.Right := R.Left + 14;
     R.Left := Rect.Left + 2;
     Rectangle(R);
     Brush.Style := bsClear;
     TextOut(Rect.Left + 16, Rect.Top, D.Name)
   end;
end;

procedure Tfrm_New.lbSeriesClick(Sender: TObject);
begin
  Fill;
end;

procedure Tfrm_New.lbSeriesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  D : TSeriesData;
  R : TRect;
begin
   D := lbSeries.Items.Objects[Index] as TSeriesData;
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
     Brush.Color := D.Color;
     R := Rect;
     R.Top := Rect.Top + 2;
     R.Bottom := Rect.Bottom-2;
     R.Right := R.Left + 14;
     R.Left := Rect.Left + 2;
     Rectangle(R);
     Brush.Style := bsClear;

     TextOut(Rect.Left + 16, Rect.Top, D.Title + ' (' + D.Graph + ')')
   end;
end;

procedure TFrm_New.DoAdd;
var
  D : TSeriesData;
begin
  D := TSeriesData.Create;
  AssembleData(D);
  lbSeries.AddItem(D.Title, D);
  lbSeries.ItemIndex := lbSeries.Count-1;
  CheckEquality;
end;

procedure Tfrm_New.DoUpdate;
var
 D : TSeriesData;
 Indx : integer;
begin
  Indx := lbSeries.ItemIndex;
  D := lbSeries.Items.Objects[lbSeries.ItemIndex] as TSeriesData;
  AssembleData(D);
  lbSeries.Repaint;
  lbSeries.ItemIndex := Indx;
  CheckEquality;
end;

procedure Tfrm_New.DoDelete;
var
 Indx : integer;
begin
  Indx := lbSeries.ItemIndex;
  lbSeries.Items.Objects[lbSeries.ItemIndex].Free;
  lbSeries.Items.Delete(lbSeries.ItemIndex);
  if Indx =  lbSeries.Count then
   lbSeries.ItemIndex := Indx-1;
  if lbSeries.Count = 0 then
    cbAltGraph.ItemIndex := 0;

end;

procedure Tfrm_New.MoveUp;
var
  lb : TListBox;
  Indx : integer;
begin
   if pc.ActivePage = TabSeries then
   lb :=  lbSeries
  else
   lb := lbItems;
   Indx := lb.ItemIndex - 1;
   lb.Items.Move(lb.ItemIndex, lb.ItemIndex-1);
   lb.ItemIndex := Indx;
end;

procedure Tfrm_New.MoveDown;
var
 lb : TListBox;
 Indx : integer;
begin
  if pc.ActivePage = TabSeries then
   lb :=  lbSeries
  else
   lb := lbItems;
  Indx := lb.ItemIndex + 1;
  lb.Items.Move(lb.ItemIndex, Indx);
  lb.ItemIndex := Indx;
end;

function Tfrm_New.CanAdd : Boolean;
begin
   Result := IsDataValid(True) and not cbxCorrelation.Checked;
end;

function Tfrm_New.CanUpdate : Boolean;
begin
   Result := IsDataValid(false) and (lbSeries.ItemIndex <> -1);
end;

function Tfrm_New.CanDelete : Boolean;
var
  lb : TListBox;
begin
   if pc.ActivePage = TabSeries then
   lb :=  lbSeries
  else
   lb := lbItems;
  if pc.ActivePage = TabSeries then
     Result := (lb.ItemIndex <> -1) and not cbxCorrelation.Checked
  else
     Result := (lb.ItemIndex <> -1)
end;

function Tfrm_New.CanMoveUp : Boolean;
var
  lb : TListBox;
begin
   if pc.ActivePage = TabSeries then
   lb :=  lbSeries
  else
   lb := lbItems;
   Result := lb.ItemIndex > 0;
 if (pc.ActivePage = TabSeries) and Result then
   Result := not cbxCorrelation.Checked;
end;

function Tfrm_New.CanMoveDown : Boolean;
var
  lb : TListBox;
begin
  if pc.ActivePage = TabSeries then
   lb :=  lbSeries
  else
   lb := lbItems;
   Result := lb.ItemIndex < lb.Count-1;
  if (pc.ActivePage = TabSeries) and Result then
   Result := not cbxCorrelation.Checked;
end;

procedure Tfrm_New.Fill;
var
 D : TSeriesData;
begin
 if lbSeries.ItemIndex = -1 then
   Exit;
 D := lbSeries.Items.Objects[lbSeries.ItemIndex] as TSeriesData;
 eSeriesTitle.Text := D.Title;
 cbSeriesColor.Selected := D.Color;
 cbGraphs.ItemIndex := cbGraphs.Items.IndexOf(D.Graph);
 cbValueScale.ItemIndex := D.ValueScale;
end;

procedure Tfrm_New.FillCat;
var
 D : TCatData;
begin
 if lbItems.ItemIndex = -1 then
   Exit;
 D := lbItems.Items.Objects[lbItems.ItemIndex] as TCatData;
 eItemName.Text := D.Name;
 cbChartItemColor.Selected := D.Color;
end;

function Tfrm_New.HasAlike(Index : integer) : Boolean;
var
  i : integer;
  D : TSeriesData;
begin
  Result := false;
  for I := 0 to lbSeries.Count-1 do
  begin
    D := lbSeries.Items.Objects[I] as TSeriesData;
    if SameText(D.Title, Trim(eSeriesTitle.Text))
    and (D.Color = cbSeriesColor.Selected)
    and (D.Graph = cbGraphs.Text)
    and (D.ValueScale = cbValueScale.ItemIndex) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure Tfrm_New.cbAltGraphChange(Sender: TObject);
begin
  CheckEquality;
end;

procedure Tfrm_New.cbChartTypeChange(Sender: TObject);
begin
   if CbChartType.ItemIndex  in [C_Span,C_General] then
     cbValueScale.ItemIndex := 0;
   if CbChartType.ItemIndex = C_Pie then {Pie}
   begin
     ClearSeries;
     cbxCorrelation.Enabled := false;
     cbxIncludeVSections.Enabled := false;
     cbxIncludeNSections.Enabled := false;
     eNumLegends.Enabled := false;
     cbValueScale.ItemIndex := 2; {Not relevant}
     cbAltGraph.ItemIndex := 0;
     cbGraphs.ItemIndex := 2;
   end
   else if cbGraphs.ItemIndex = C_Pie then {Was pie}
   begin
     ClearSeries;
     cbGraphs.ItemIndex := 0;
   end;
   if CbChartType.ItemIndex  in [C_Span,C_General] then
   begin
     cbxCorrelation.Enabled := True;
     cbxIncludeVSections.Enabled := True;
     cbxIncludeNSections.Enabled := True;
     eNumLegends.Enabled := True;
   end
   else if CbChartType.ItemIndex = C_Bar then
   begin
     cbxCorrelation.Enabled := False;
     cbxIncludeVSections.Enabled := True;
     cbxIncludeNSections.Enabled := True;
     eNumLegends.Enabled := True;
     cbGraphs.ItemIndex := 1;
     cbGraphs.Enabled := false;
   end;

   if CbChartType.ItemIndex = C_Span then
   begin
     cbSpanType.Enabled := True;
     cbSpanType.ItemIndex := S_Date;
   end
   else
   begin
     cbSpanType.Enabled := False;
     cbSpanType.ItemIndex := -1;
   end;
end;

procedure Tfrm_New.cbxCorrelationClick(Sender: TObject);
begin
  ClearSeries;
  InitSeries;
  if cbxCorrelation.Checked then
  begin
    cbxIncludeVSections.Enabled := false;
    cbxIncludeNSections.Enabled := false;
    cbGraphs.ItemIndex := 1;
    acAddExecute(nil);
    cbGraphs.ItemIndex := 0;
    cbValueScale.ItemIndex := 1;
    cbSeriesColor.Selected := clSilver;
    eSeriesTitle.Text := 'Series 2';
    acAddExecute(nil);
  end
  else
  begin
    cbxIncludeVSections.Enabled := True;
    cbxIncludeNSections.Enabled := True;
  end;
end;

procedure Tfrm_New.FormCreate(Sender: TObject);
begin
  tabSeries.TabVisible := false;
  tabCats.TabVisible := false;
end;

procedure Tfrm_New.FormDestroy(Sender: TObject);
begin
  ClearSeries;
  ClearCats;
end;

procedure Tfrm_New.ClearSeries;
var
 I : integer;
begin
    for I := 0 to lbSeries.Count-1 do
     lbSeries.Items.Objects[i].Free;
    lbSeries.Clear;
    InitSeries;
end;

procedure Tfrm_New.ClearCats;
var
 I : integer;
begin
    for I := 0 to lbItems.Count-1 do
     lbItems.Items.Objects[i].Free;
    lbItems.Clear;
end;

procedure Tfrm_New.InitSeries;
begin
  eSeriesTitle.Text := 'Series 1';
  cbGraphs.ItemIndex := 0;
  cbSeriesColor.Selected := clBlack;
  cbValueScale.ItemIndex := 0;
  cbAltGraph.ItemIndex := 0;
end;

procedure TFrm_New.CheckEquality;
var
  i : integer;
  G : string;
  D : TSeriesData;
  Mixed : Boolean;
begin
  Mixed := false;
  for I := 0 to lbSeries.Count-1 do
    begin
      D := lbSeries.Items.Objects[i] as TSeriesData;
      if i = 0 then
      begin
        G := D.Graph;
        continue;
      end;
      if not Sametext(G, D.Graph) then
      begin

        Mixed := True;
        Break;
      end;
    end;
  cbAltGraph.Enabled := not Mixed and not (cbChartType.ItemIndex = c_Pie);
  if not cbAltGraph.Enabled or SameText(G, cbAltGraph.Text) then
    cbAltGraph.ItemIndex := 0;
end;

function TFrm_New.MainOK : Boolean;
begin
  Result := (Trim(eTitle.Text) <> '') and not SameText(Trim(eTitle.Text), '<Title>');
end;

function TFrm_New.SeriesOK : Boolean;
begin
  Result := lbSeries.Count > 0;
end;

function TFrm_New.CatOK : Boolean;
begin
   Result := ((lbItems.Count > 0) and (cbChartType.ItemIndex in [C_Pie, C_Bar]))
   or (cbChartType.ItemIndex in [c_Span, c_General]);
end;

procedure TFrm_New.Reset;
begin
  eTitle.Text := '<Title>';
  cbxCorrelation.Checked := false;
  cbxIncludeNSections.Checked := false;
  cbxIncludeVSections.Checked := false;
  eNumLegends.Text := '0';
  cbChartType.ItemIndex := C_general;
  cbSpanType.ItemIndex := -1;
  ClearSeries;
  ClearCats;
  eItemName.Text := 'Category 1';
  cbChartItemColor.Selected := clBlack;
end;


end.
