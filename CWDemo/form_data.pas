unit form_data;
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ValEdit, Vcl.StdCtrls,
  Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ComCtrls, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, ChartWriter, Vcl.ExtCtrls;

type
  TfrmData = class(TForm)
    lbSeries: TListBox;
    vlData: TValueListEditor;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    acSaveChanges: TAction;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    panSpan: TPanel;
    eStartNum: TEdit;
    eSpanCount: TEdit;
    eIntervals: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    acGetSpan: TAction;
    dtpStartDate: TDateTimePicker;
    lblSpantype: TLabel;
    Label4: TLabel;
    dtpStartTime: TDateTimePicker;
    ToolButton2: TToolButton;
    lbEndSpan: TLabel;
    Label5: TLabel;
    acRender: TAction;
    ToolButton3: TToolButton;
    acClearData: TAction;
    ToolButton4: TToolButton;
    procedure lbSeriesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acSaveChangesUpdate(Sender: TObject);
    procedure acSaveChangesExecute(Sender: TObject);
    procedure vlDataDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure acGetSpanUpdate(Sender: TObject);
    procedure acGetSpanExecute(Sender: TObject);
    procedure dtpStartTimeChange(Sender: TObject);
    procedure dtpStartDateChange(Sender: TObject);
    procedure eStartNumChange(Sender: TObject);
    procedure eSpanCountChange(Sender: TObject);
    procedure acRenderExecute(Sender: TObject);
    procedure acRenderUpdate(Sender: TObject);
    procedure acClearDataUpdate(Sender: TObject);
    procedure acClearDataExecute(Sender: TObject);
    procedure vlDataValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure vlDataKeyPress(Sender: TObject; var Key: Char);
    procedure vlDataStringsChange(Sender: TObject);
  private
    { Private declarations }
    FChanged : Boolean;
    FCacheChanged : Boolean;
    FSeriesIndex : integer;
    function IsListValid : Boolean;
  public
    { Public declarations }
    FFilling : Boolean;
    procedure FillSeries;
    procedure FillData;
    procedure SetEndOfSpan;
    procedure SaveChanges;
    property IsChanged : Boolean read FChanged;
    function IsRendered : Boolean;
    function HasData : Boolean;
  end;

var
  frmData: TfrmData;

implementation
uses frmMain, DateUtils;

{$R *.dfm}

function AddWholeMonths(startDate: TDate; monthsToAdd: Integer): TDate;
var
  year, month, day: Word;
begin
  DecodeDate(startDate, year, month, day);
  Inc(month, monthsToAdd);
  Inc(year, (month - 1) div 12);
  month := ((month - 1) mod 12) + 1;
  Result := EncodeDate(year, month, 1);
end;


function TfrmData.IsListValid : Boolean;
var
  i : integer;
begin
  Result := false;
  if vlData.Strings.Count < 2 then
    Exit;
  Result := True;
  for I := 0 to vlData.Strings.Count-1 do
    begin
      if (vlData.Strings.ValueFromIndex[I] = '')
      or (vlData.Strings.ValueFromIndex[I] = FormatSettings.DecimalSeparator) then
      begin
        Result := false;
        Break;
      end
      else if vlData.Strings.KeyNames[I] = '' then
      begin
        Result := false;
        Break;
      end;
    end;
end;

procedure TfrmData.FillSeries;
var
  i : integer;
begin
  lbSeries.Clear;
  for I := 0 to form_main.CW.Chart.SeriesDefs.Count-1 do
    lbSeries.Items.Add(form_main.CW.Chart.SeriesDefs[i].Title);
  lbSeries.ItemIndex := 0;
  FSeriesIndex := 0;
end;

procedure TfrmData.FormShow(Sender: TObject);
begin
  FillSeries;
  FillData;
  if form_Main.CW.Chart is TCWGeneralChart then
  begin
    vlData.KeyOptions := [keyEdit, keyAdd, keyUnique]
  end
  else
   vlData.KeyOptions := [keyUnique]
end;

procedure TfrmData.lbSeriesClick(Sender: TObject);
begin
   if IsChanged then
   begin
    ShowMessage('Update buffer before changing series');
    lbSeries.ItemIndex := FSeriesIndex;
   end;
   FillData;
   FSeriesIndex := lbSeries.ItemIndex;
end;

procedure TfrmData.vlDataDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  R : TRect;
  C : TCWCategory;
  Cats : TCWCategories;
begin
   if ACol = 0 then
   begin
     vlData.Canvas.Brush.Style := bsSolid;
     vlData.Canvas.Brush.Color := vlData.Color;
     vlData.Canvas.FillRect(Rect);
     if ARow = 0 then
     begin
       vlData.Canvas.TextOut(Rect.Left + 2, Rect.Top +2, 'Name');
       Exit;
     end;
     if (form_main.CW.Chart is TCWGeneralChart) and (ARow > 0) then
     begin
       if vlData.Strings.Count >= Arow then
        vlData.Canvas.TextOut(Rect.Left + 2, Rect.Top +2,
          vlData.Strings.KeyNames[Arow-1]);
     end;
     if form_main.CW.Chart is TCWCateGoryChart then
       Cats := TCWCateGoryChart(form_main.CW.Chart).Categories
     else if form_main.CW.Chart is TCWPieChart then
       Cats := TCWPieChart(form_main.CW.Chart).Categories
     else
       Cats := nil;

     if (Cats <> nil) and (Cats.Count > 0) then
     begin
        C := vlData.Strings.Objects[ARow-1] as TCWCategory;
        vlData.Canvas.Brush.Color := C.Color;
        R := Rect;
        R.Top := Rect.Top + 2;
        R.Bottom := Rect.Bottom-2;
        R.Right := R.Left + 14;
        R.Left := Rect.Left + 2;
        vlData.Canvas.Rectangle(R);
        vlData.Canvas.Brush.Style := bsClear;
        vlData.Canvas.TextOut(Rect.Left + 16, Rect.Top, C.CategoryName);
     end
     else if form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Count > 0 then
     begin
        vlData.Canvas.Brush.Style := bsClear;
        vlData.Canvas.TextOut(Rect.Left + 2, Rect.Top +2,
        form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.KeyNames[ARow-1]);
     end;
   end
   else
   begin
     vlData.Canvas.Brush.Style := bsSolid;
     vlData.Canvas.Brush.Color := vlData.Color;
     vlData.Canvas.FillRect(Rect);
     if ARow = 0 then
     begin
       vlData.Canvas.TextOut(Rect.Left + 2, Rect.Top +2, 'Value');
     end
     else
     begin

       if form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Count > 0 then
       begin
         vlData.Canvas.Brush.Style := bsClear;
         vlData.Canvas.TextOut(Rect.Left + 2, Rect.Top +2,
          vlData.Strings.ValueFromIndex[Arow-1]);
       end

       else if (form_main.CW.Chart is TCWGeneralChart) and (ARow > 0) then
       begin
         vlData.Canvas.Brush.Style := bsClear;
         if vlData.Strings.Count >= Arow then
          vlData.Canvas.TextOut(Rect.Left + 2, Rect.Top +2,
           vlData.Strings.ValueFromIndex[Arow-1]);
       end;
     end;
   end;

end;

procedure TfrmData.vlDataKeyPress(Sender: TObject; var Key: Char);
var
  S : string;
  DS : char;
  HasSep : Boolean;
begin
   if vlData.Col = 0 then
     Exit;
   HasSep := false;
   DS := #0;
   if vlData.Strings.Count > 0 then
   begin
    DS := FormatSettings.DecimalSeparator;
    S := vlData.Strings.ValueFromIndex[vlData.Row-1];
    HasSep := Pos(DS, S) <> 0;
   end;

   if not HasSep then
   begin
   if not (Key in['0'..'9', DS, #8]) then
     Key := #0;
   end
   else
   if not (Key in['0'..'9', #8]) then
     Key := #0;

end;

procedure TfrmData.vlDataStringsChange(Sender: TObject);
begin
   if FFilling then
    Exit;
   FChanged := True;
end;

procedure TfrmData.vlDataValidate(Sender: TObject; ACol, ARow: Integer;
  const KeyName, KeyValue: string);
var
  E : single;
begin
   if FFilling then
     Exit;
   if not TryStrToFloat(KeyValue, E) and (ACol = 1) then
     raise Exception.Create('Invalid value. Floats or integers only.');
end;

procedure TfrmData.acClearDataExecute(Sender: TObject);
begin
    form_Main.CW.Clear;
    Filldata;
end;

procedure TfrmData.acClearDataUpdate(Sender: TObject);
begin
  acClearData.Enabled := HasData;
end;

procedure TfrmData.acGetSpanExecute(Sender: TObject);
var
  Cnt : integer;
  Intr : integer;

  procedure FillDates;
  var
    i : integer;
    Dt : TDate;
    S : string;
  begin
     Dt := dtpStartDate.Date;
     for I := 1 to Cnt do
       begin
         S := DateToStr(Dt);
         S := S + '=0';
         form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Add(S);
         Dt := Dt + Intr;
       end;
       FillData;
  end;

  procedure FillMonths;
  var
    i : integer;
    Dt : TDate;
    S : string;
  begin
     Dt := dtpStartDate.Date;
     for I := 1 to Cnt do
       begin
         S := DateToStr(Dt);
         S := S + '=0';
         form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Add(S);
         Dt := AddWholeMonths(Dt, Intr);
       end;
       FillData;
  end;

  procedure FillTimes;
  var
    i : integer;
    Dt : TDateTime;
    S : string;
    T : TTime;
    sp : TSpanType;
  begin
     Dt := DateOf(dtpStartDate.Date);
     T := dtpStartTime.Time;
     Sp := form_main.CW.SpanType;
     if SP = ntHourSpan then
      Dt := EncodeDateTime(
         YearOf(Dt), MonthOf(Dt), DayOf(Dt), HourOf(T), 0, 0, 0)
     else if SP = ntMinuteSpan then
      Dt := EncodeDateTime(
         YearOf(Dt), MonthOf(Dt), DayOf(Dt), HourOf(T), MinuteOf(T), 0, 0)
     else
      Dt := EncodeDateTime(
         YearOf(Dt), MonthOf(Dt), DayOf(Dt), HourOf(T), MinuteOf(T), SEcondOf(T), 0);

     for I := 1 to Cnt do
       begin
         S := DateTimeToStr(Dt);
         S := S + '=0';
         form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Add(S);
         if SP = ntHourSpan then
           DT := IncHour(Dt, Intr)
         else if Sp = ntMinuteSpan then
           DT := IncMinute(Dt, Intr)
         else
           DT := IncSecond(Dt, Intr);
       end;
       FillData;
  end;

  procedure FillNumbers;
  var
    i : integer;
    S : string;
    N : integer;
  begin
   N := StrToInt(eStartNum.Text);
   for I := 1 to Cnt do
   begin
     S := IntToStr(N);
     S := S + '=0';
     form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Add(S);
     N := N + Intr;
   end;
   FillData;
  end;


begin
   if vlData.Strings.Count > 0 then
   begin
   if MessageDlg('Warning! If you recreate the span, all values will be lost. Continue?',
     mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrNo then
     Exit;
   end;
   Cnt := StrToInt(eSpanCount.Text);
   Intr := StrToInt(eIntervals.Text);

   form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Clear;
   vlData.Strings.Clear;
   if form_main.CW.SpanType = ntDateSpan then
   begin
       FillDates;
   end
   else if form_main.CW.SpanType in [ntHourSpan .. ntSecondSpan] then
   begin
       FillTimes;
   end
   else if form_main.CW.SpanType = ntMonthSpan then
     FillMonths
   else
     FillNumbers;
   FChanged := True;
end;

procedure TfrmData.SetEndofSpan;
var
 Num : single;
 Cnt : integer;
 Intr : integer;
 DT : TDateTime;
 T : TTime;
 Sp : TSpanType;
begin
   if (eSpanCount.Text = '')
   or (eIntervals.Text = '') then
     Exit;

   sp := form_main.CW.SpanType;
   Cnt := StrToInt(eSpanCount.Text);
   Intr := StrToInt(eIntervals.Text);
   if form_main.CW.IsTimeSpan then
   begin
     T := dtpStartTime.Time;
     Dt := DateOf(dtpStartDate.Date);
     if sp = ntMonthSpan then
     begin
       Dt := AddWholeMonths(Dt, (Cnt-1)*Intr);
     end
     else if sp = ntDateSpan then
     begin
       Dt := Dt + (Cnt-1)*Intr;
     end
     else if sp = ntHourSpan then
     begin
       Dt := EncodeDateTime(
         YearOf(Dt), MonthOf(Dt), DayOf(Dt), HourOf(T), 0, 0, 0);
       Dt := IncHour(Dt,(Cnt-1)*Intr);
     end
     else if sp = ntMinuteSpan then
     begin
       Dt := EncodeDateTime(
        YearOf(Dt), MonthOf(Dt), DayOf(Dt), HourOf(T), MinuteOf(T), 0, 0);
       Dt := IncMinute(Dt, (Cnt-1)*Intr);
     end
     else if sp = ntSecondSpan then
     begin
       Dt := EncodeDateTime(
         YearOf(Dt), MonthOf(Dt), DayOf(Dt), HourOf(T), MinuteOf(T), SecondOf(T), 0);
       Dt := IncSecond(Dt, (Cnt-1)*Intr);
     end;
     lbEndSpan.Caption := DateTimeToStr(Dt);
   end
   else
   begin
     if eStartNum.Text <> '' then
     begin
      Num := StrToFloat(eStartNum.Text) + Cnt*Intr - Intr;
      lbEndSpan.Caption := FormatNum(Num, 3);
     end;
   end;
end;

procedure TfrmData.acGetSpanUpdate(Sender: TObject);
var
 Enabl: Boolean;
begin
   if not (form_main.CW.Chart is TCWSpanChart) then
   begin
     acGetSpan.Enabled := false;
     Exit;
   end;

   Enabl := (eSpanCount.Text <> '') and (eIntervals.Text <> '');
   if Enabl then
   begin
     Enabl := StrToInt(eSpanCount.Text) >= 2;
   end;
   if Enabl and (form_main.CW.SpanType = ntMonthSpan) then
     Enabl := DayOf(DtpStartDate.Date) = 1;
   if Enabl then
     Enabl := not HasData;
   acGetSpan.Enabled := Enabl;
end;

procedure TfrmData.acRenderExecute(Sender: TObject);
begin
  form_main.CW.Clear;
  form_main.CW.LoadFromCache;
  form_main.CW.Execute;
  FCacheChanged := false;
  form_main.OnLoadChart;
end;

procedure TfrmData.acRenderUpdate(Sender: TObject);
var
  i : integer;
  Enabl : Boolean;
begin
  Enabl := True;
  for I := 0 to form_main.CW.Chart.SeriesDefs.Count-1 do
    if form_main.CW.Chart.SeriesDefs[i].DataCache.Count = 0 then
    begin
      Enabl := false;
      Break
    end;

  if Enabl then
    Enabl := FCacheChanged;

  acRender.Enabled := Enabl;
end;

procedure TfrmData.acSaveChangesExecute(Sender: TObject);
begin
   SaveChanges;
end;

procedure TfrmData.acSaveChangesUpdate(Sender: TObject);
begin
  acSaveChanges.Enabled := IsChanged and IsListValid;
end;

procedure TfrmData.dtpStartDateChange(Sender: TObject);
var
 Y, M, D : word;
 Dt : TDate;
begin
  if form_main.CW.SpanType = ntMonthSpan then
  begin
    Dt := dtpStartDate.Date;
    DecodeDate(Dt, Y, M, D);
    if D <> 1 then
      Dt := EncodeDate(Y, M, 1);
    dtpStartDate.Date := Dt;
  end;
  SetEndOfSpan;
end;

procedure TfrmData.dtpStartTimeChange(Sender: TObject);
begin
   if form_main.CW.SpanType = ntHourSpan then
     dtpStartTime.Time := EncodeTime(HourOf(dtpStartTime.Time), 0, 0, 0)
   else if form_main.CW.SpanType = ntMinuteSpan then
     dtpStartTime.Time := EncodeTime(HourOf(dtpStartTime.Time), MinuteOf(dtpStartTime.Time), 0, 0)
   else
     dtpStartTime.Time := EncodeTime(HourOf(dtpStartTime.Time), MinuteOf(dtpStartTime.Time),
      SecondOf(dtpStartTime.Time), 0);
   SetEndOfSpan;
end;

procedure TfrmData.eSpanCountChange(Sender: TObject);
begin
   SetEndOfSpan;
end;

procedure TfrmData.eStartNumChange(Sender: TObject);
begin
  SetEndOfSpan;
end;

procedure TfrmData.FillData;
var
  i : integer;
  Clrs : TCWCategories;
  S : string;
  Dt : TDate;
  SD : TCWSeriesDef;
begin
 FFilling := True;
 try
   vlData.Strings.Clear;
   vlData.Align := alBottom;
   SD := form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex];
   if (SD.DataCache.Count = 0)
   and HasData
   then
    form_main.CW.Chart.SaveCache;

   if (form_main.CW.Chart is TCWCategoryChart)
    and (form_main.CW.Categories.Count > 0) and not HasData then
   {New}
   begin
      Clrs := form_main.CW.Categories;
      for I := 0 to Clrs.Count-1 do
      begin
        S := Clrs[i].CategoryName;
        if SD.DataCache.Count = Clrs.Count then
          S := S + '=' + SD.DataCache.ValueFromIndex[i]
        else
        begin
         S := S + '=0';
         SD.DataCache.Add(S);
        end;
        vlData.Strings.AddObject(S, Clrs[i]);
      end;
   end
   else
   begin
     if (form_main.CW.Chart is TCWCategoryChart) and
     (form_main.CW.Categories.Count > 0) then
     begin
       for I := 0 to form_main.CW.Categories.Count-1 do
      begin
        S := SD.DataCache[i];
        vlData.Strings.AddObject(S, form_main.CW.Categories[i]);
      end;
     end
     else
     vlData.Strings.Assign(SD.DataCache);
   end;
   if (form_main.CW.Chart is TCWCategoryChart) and
   (form_main.CW.Categories.Count > 0) then
   begin
     panSpan.Visible := false;
     vlData.Align := alClient;
   end
   else if (form_main.CW.Chart is TCWGeneralChart) then
   begin
     panSpan.Visible := false;
     vlData.Align := alClient;
   end
   else if (form_main.CW.Chart is TCWSpanChart) then
   begin
     panSpan.Visible := True;
     eStartNum.Visible := form_main.CW.SpanType = ntNumberSpan;
     dtpStartDate.Visible := form_main.CW.SpanType in [ntMonthSpan .. ntSecondSpan];
     dtpStartTime.Visible := form_main.CW.SpanType in [ntHourSpan .. ntSecondSpan];

     if not HasData then
     begin
       dtpStartDate.Date := ToDay;
       if form_main.CW.SpanType = ntNumberSpan then
         lblSpanType.Caption := 'Number span'
       else if form_main.CW.SpanType = ntMonthSpan then
       begin
         lblSpanType.Caption := 'Month span';
         Dt := dtpStartDate.Date;
         Dt := IncMonth(Dt);
         Dt := EncodeDate(YearOf(Dt), MonthOf(Dt), 1);
         dtpStartDate.Date := Dt;
       end
       else if form_main.CW.SpanType = ntDateSpan then
         lblSpanType.Caption := 'Date span'
       else if form_main.CW.SpanType = ntHourSpan then
         lblSpanType.Caption := 'Hour span'
       else if form_main.CW.SpanType = ntMinuteSpan then
         lblSpanType.Caption := 'Minute span'
       else
         lblSpanType.Caption := 'Second span';
       SetEndOfSpan;
     end
     else
     begin
       panSpan.Visible := false;
     end;
     vlData.Align := alClient;
   end;

   FChanged := false;
   FCacheChanged := false;
 finally
   FFilling := false;
 end;
end;

procedure TfrmData.SaveChanges;
begin
  form_main.CW.Chart.SeriesDefs[lbSeries.ItemIndex].DataCache.Assign(vlData.Strings);
  FCacheChanged := True;
  FChanged := false;
end;

function TfrmData.IsRendered : Boolean;
begin
  Result := not FCacheChanged;
end;

function TfrmData.HasData : Boolean;
begin
  Result := form_Main.CW.Count > 0;
end;


end.
