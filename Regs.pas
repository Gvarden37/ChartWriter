unit Regs;
interface
uses classes, DesignIntf, DesignEditors, DesignMenus, SysUtils, ChartWriter, VCL.Dialogs;

type
 TGWLiveGraphs = class(TComponentEditor)
  function GetVerbCount : integer; override;
  function GetVerb(Index : integer) : string; override;
  procedure ExecuteVerb(Index : integer); override;
  procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  //procedure Edit; override;
 end;

procedure Register;


implementation
uses Vcl.Controls;

procedure Register;
begin
  RegisterComponents('ChartWriter', [TChartWriter, TCWCurve, TCWBar, TCWPie,
    TCWNameSectionDefs, TCWValueSectionDefs, TCWLegend, TCWSpanChart, TCWGeneralChart, TCWCategoryBarChart, TCWPieChart]);
  RegisterComponentEditor(TChartWriter, TGWLiveGraphs);
end;

function TGWLiveGraphs.GetVerb(Index : integer) : string;
begin
   Result := 'Live graphs';
end;

procedure TGWLiveGraphs.ExecuteVerb(Index : integer);
var
  C : TCWChart;
begin
    TChartWriter(Component).LiveGraphs := not TChartWriter(Component).LiveGraphs;
    if TChartWriter(Component).Chart = nil then
      Exit;
    if not TChartWriter(Component).LiveGraphs then
    begin
       TChartWriter(Component).Clear;
       Exit;
    end;

    if TChartWriter(Component).Chart.FileName <> '' then
    begin
      TChartWriter(Component).LoadFromFile(TChartWriter(Component).Chart.FileName);
      Exit;
    end
    else
      TChartWriter(Component).RenderDesigner;
end;

function TGWLiveGraphs.GetVerbCount : integer;
begin
  Result := 1;
end;

procedure TGWLiveGraphs.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  case Index of
    0: AItem.Checked := TChartWriter(Component).LiveGraphs;
  end;
end;


end.
