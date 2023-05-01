unit Reg;
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
    TCWNameSectionDefs, TCWValueSectionDefs, TCWLegend, TCWAxisChart, TCWPieChart]);
  RegisterComponentEditor(TChartWriter, TGWLiveGraphs);
end;

function TGWLiveGraphs.GetVerb(Index : integer) : string;
begin
  if Index = 0 then
   Result := 'Live graphs'
  else
   Result := 'Load from file';
end;

procedure TGWLiveGraphs.ExecuteVerb(Index : integer);
var
  Dlg : TFileOpenDialog;
begin
 if Index = 0 then
 begin
  TChartWriter(Component).LiveGraphs := not TChartWriter(Component).LiveGraphs;
  if not TChartWriter(Component).LiveGraphs then
  begin
    TChartWriter(Component).InitDesigner := false;
    TChartWriter(Component).DsgnRealData := false;
  end;
   TChartWriter(Component).Repaint;
 end
 else
 begin
    Dlg := TFileOpenDialog.Create(Component);
    try
      if Dlg.Execute then
      begin
        TChartWriter(Component).LiveGraphs := False;
        TChartWriter(Component).LoadFromFile(Dlg.FileName);
      end;
      finally
        Dlg.Free;
      end;
    TChartWriter(Component).DsgnRealData := True;
    TChartWriter(Component).LiveGraphs := True;
    TChartWriter(Component).Repaint;
 end;
end;

function TGWLiveGraphs.GetVerbCount : integer;
begin
  Result := 2;
end;

procedure TGWLiveGraphs.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  case Index of
    0: AItem.Checked := TChartWriter(Component).LiveGraphs;
  end;
end;


end.
