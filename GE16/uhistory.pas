unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, UTool;

type

  { THistory }

  TTools = record
    FirstX: Integer;
    FirstY: Integer;
    LastX: Integer;
    LastY: Integer;
    LastTool: TTool;
    Coords: Array of TPoint;
    SRand: Array of Integer;
  end;

  THistory = class
    private
      FActionsCount: Integer;
      FTools: Array of TTools;
    public
      procedure Start(X, Y: Integer);
      procedure Stop(X, Y: Integer);
      procedure Add(AToolInfo: TToolInfo);
      procedure Draw(AImage: TCanvas; i: Integer);
      procedure Undo(AImage: TCanvas);
      procedure Redo(AImage: TCanvas);
      procedure Clear;
  end;

var
  History: THistory;

implementation

{ THistory }

procedure THistory.Start(X, Y: Integer);
begin
  If Tool is TDropperTool then Exit;
  Inc(FActionsCount);
  SetLength(FTools, FActionsCount);
  FTools[FActionsCount-1].FirstX := X;
  FTools[FActionsCount-1].FirstY := Y;
end;

procedure THistory.Stop(X, Y: Integer);
begin
  FTools[FActionsCount-1].LastX := X;
  FTools[FActionsCount-1].LastY := Y;
end;

procedure THistory.Add(AToolInfo: TToolInfo);
begin
  If (Tool is TDropperTool) or (FActionsCount = 0) then Exit;
  FTools[FActionsCount-1].LastTool := AToolInfo.Create;
  FTools[FActionsCount-1].LastTool.PenWidth := Tool.PenWidth;
  FTools[FActionsCount-1].LastTool.PenColor := Tool.PenColor;
  FTools[FActionsCount-1].LastTool.PenStyle := Tool.PenStyle;
  FTools[FActionsCount-1].LastTool.BrushColor := Tool.BrushColor;
  FTools[FActionsCount-1].LastTool.BrushStyle := Tool.BrushStyle;
  if Tool is TPenTool then
    FTools[FActionsCount-1].Coords := Tool.DrawingPoints;
  if Tool is TSprayTool then
  begin
    FTools[FActionsCount-1].Coords := Tool.DrawingPoints;
    FTools[FActionsCount-1].SRand := Tool.SpraySeed;
  end;
end;

procedure THistory.Draw(AImage: TCanvas; i: Integer);
var
  j: Integer;
begin
  if FTools[i].LastTool is TPenTool then
  begin
    FTools[i].LastTool.StartDrawing(AImage, mbLeft, FTools[i].FirstX, FTools[i].FirstY);
    FTools[i].LastTool.Draw(AImage, FTools[i].Coords);
  end
  else
  if FTools[i].LastTool is TSprayTool then
    For j := 0 to Length(FTools[i].Coords)-2 do
      FTools[i].LastTool.Draw(AImage, FTools[i].SRand[j],
        FTools[i].Coords[j].X, FTools[i].Coords[j].Y)
  else
  begin
    FTools[i].LastTool.StartDrawing(AImage, mbLeft, FTools[i].FirstX, FTools[i].FirstY);
    FTools[i].LastTool.Draw(AImage, FTools[i].LastX, FTools[i].LastY);
  end;
end;

procedure THistory.Undo(AImage: TCanvas);
var
  i: integer;
begin
  if FActionsCount = 0 then Exit;
  Dec(FActionsCount);
  MainInterface.Clear;
  For i := 0 to FActionsCount-1 do
    Draw(AImage, i);
end;

procedure THistory.Redo(AImage: TCanvas);
begin
  if FActionsCount = Length(FTools) then Exit;
  Inc(FActionsCount);
  Draw(AImage, FActionsCount-1);
end;

procedure THistory.Clear;
begin
  History.Free;
  History := THistory.Create;
end;

initialization
  History := THistory.Create;
  History.FActionsCount := 0;
end.
