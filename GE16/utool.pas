unit UTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, ExtCtrls, Graphics, Controls, FPCanvas;

type

  { IMainInterface }

  IMainInterface = Interface
    procedure PickColor(Button: TMouseButton; X, Y: Integer);
    procedure Clear;
  end;

  { TTool }

  TCoordinates = Array of TPoint;
  TSRand = Array of Integer;

  TTool = class
    private
      FDrawingState: Boolean;
      FDrawingPoints: TCoordinates;
      FSpraySeed: TSRand;
      FPenColor: TColor;
      FPenStyle: TPenStyle;
      FPenWidth: Integer;
      FBrushColor: TColor;
      FBrushStyle: TBrushStyle;
      function GetState: Boolean;
    public
      procedure Draw(AImage: TCanvas; X, Y: Integer); virtual; abstract;
      procedure Draw(AImage: TCanvas; ARSeed: Integer; X, Y: Integer); virtual; abstract; overload;
      procedure Draw(AImage: TCanvas; ACoords: TCoordinates); virtual; abstract; overload;
      procedure StartDrawing(AImage: TCanvas; Button: TMouseButton;
        X, Y: Integer); virtual; abstract;
      procedure StopDrawing;
    published
      property Active: Boolean read GetState;
      property PenColor: TColor read FPenColor write FPenColor;
      property PenStyle: TPenStyle read FPenStyle write FPenStyle;
      property PenWidth: Integer read FPenWidth write FPenWidth;
      property BrushColor: TColor read FBrushColor write FBrushColor;
      property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
      property DrawingPoints: TCoordinates read FDrawingPoints;
      property SpraySeed: TSRand read FSpraySeed;
  end;

  TToolInfo = class of TTool;

  { TInstant }

  TInstant = class(TTool)
    public
      procedure StartDrawing(AImage: TCanvas; Button: TMouseButton;
        X, Y: Integer); override;
  end;

  { TFigure }

  TFigure = class(TTool)
    private
      FLastX: Integer;
      FLastY: Integer;
    public
      procedure StartDrawing(AImage: TCanvas; Button: TMouseButton;
        X, Y: Integer); override;
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
  end;

  { TPenTool }

  TPenTool = class(TInstant)
    private
      FPointsCount: Integer;
    public
      procedure StartDrawing(AImage: TCanvas; Button: TMouseButton;
        X, Y: Integer); override;
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
      procedure Draw(AImage: TCanvas; ACoords: TCoordinates); override; overload;
  end;

  { TFillTool }

  TFillTool = class(TInstant)
    public
      procedure StartDrawing(AImage: TCanvas; Button: TMouseButton;
        X, Y: Integer); override;
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
  end;

  { TDropperTool }

  TDropperTool = class(TInstant)
    public
      procedure StartDrawing(AImage: TCanvas; Button: TMouseButton;
        X, Y: Integer); override;
  end;

  { TSprayTool }

  TSprayTool = class(TInstant)
    private
      FPointsCount: Integer;
    public
      procedure StartDrawing(AImage: TCanvas; Button: TMouseButton;
        X, Y: Integer); override;
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
      procedure Draw(AImage: TCanvas; ARSeed: Integer; X, Y: Integer); override; overload;
  end;

  { TLineTool }

  TLineTool = class(TFigure)
    public
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
  end;

  { TRectTool }

  TRectTool = class(TFigure)
    public
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
  end;

  { TTriangleTool }

  TTriangleTool = class(TFigure)
    public
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TFigure)
    public
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
  end;

  { TStarTool }

  TStarTool = class(TFigure)
    public
      procedure Draw(AImage: TCanvas; X, Y: Integer); override;
  end;

var
  Tool: TTool;
  MainInterface: IMainInterface;

implementation

{ TTool }

function TTool.GetState: Boolean;
begin
  Result := FDrawingState;
end;

procedure TTool.StopDrawing;
begin;
  FDrawingState := False;
 end;

{ TInstant }

procedure TInstant.StartDrawing(AImage: TCanvas; Button: TMouseButton;
    X, Y: Integer);
begin
  FDrawingState := True;
  With AImage do
  begin
    Pen.Color := FPenColor;
    Pen.Style := FPenStyle;
    Pen.Width := FPenWidth;
    MoveTo(X, Y);
  end;
end;

procedure TPenTool.StartDrawing(AImage: TCanvas; Button: TMouseButton; X,
  Y: Integer);
begin
  Inherited;
  FPointsCount := 1;
  SetLength(FDrawingPoints, FPointsCount);
end;

procedure TPenTool.Draw(AImage: TCanvas; X, Y: Integer);
begin
  FDrawingPoints[FPointsCount-1].X := X;
  FDrawingPoints[FPointsCount-1].Y := Y;
  AImage.Polyline(FDrawingPoints);
  Inc(FPointsCount);
  SetLength(FDrawingPoints, FPointsCount);
end;

procedure TPenTool.Draw(AImage: TCanvas; ACoords: TCoordinates);
begin
  SetLength(ACoords, Length(ACoords)-1);
  AImage.Polyline(ACoords);
end;

procedure TFillTool.StartDrawing(AImage: TCanvas; Button: TMouseButton;
    X, Y: Integer);
begin
  Draw(AImage, X, Y);
end;

procedure TFillTool.Draw(AImage: TCanvas; X, Y: Integer);
begin
  With AImage do
  begin
    Brush.Color := FPenColor;
    Brush.Style := bsSolid;
    FloodFill(X, Y, AImage.Pixels[X, Y], TFillStyle.fsSurface);
  end;
end;

procedure TDropperTool.StartDrawing(AImage: TCanvas; Button: TMouseButton;
    X, Y: Integer);
begin
  MainInterface.PickColor(Button, X, Y);
end;

procedure TSprayTool.StartDrawing(AImage: TCanvas; Button: TMouseButton;
    X, Y: Integer);
begin
  Inherited;
  FPointsCount := 1;
  SetLength(FDrawingPoints, FPointsCount);
  SetLength(FSpraySeed, FPointsCount);
  Draw(AImage, X, Y);
end;

procedure TSprayTool.Draw(AImage: TCanvas; X, Y: Integer);
var
  dX, dY, i: Integer;
begin
  Randomize;
  FSpraySeed[FPointsCount-1] := RandSeed;
  FDrawingPoints[FPointsCount-1].X := X;
  FDrawingPoints[FPointsCount-1].Y := Y;
  For i := 1 to 30*FPenWidth do
  begin
    dX := Random(10*FPenWidth + 1)-5*FPenWidth;
    dY := Random(10*FPenWidth + 1)-5*FPenWidth;
    if (dX*dX + dY*dY) <= 10*FPenWidth then
      AImage.Pixels[X-dX, Y-dY] := FPenColor;
  end;
  Inc(FPointsCount);
  SetLength(FDrawingPoints, FPointsCount);
  SetLength(FSpraySeed, FPointsCount);
end;

// History redraw

procedure TSprayTool.Draw(AImage: TCanvas; ARSeed: Integer; X, Y: Integer);
var
  dX, dY, i: Integer;
begin
  RandSeed := ARSeed;
  For i := 1 to 30*FPenWidth do
  begin
    dX := Random(10*FPenWidth + 1)-5*FPenWidth;
    dY := Random(10*FPenWidth + 1)-5*FPenWidth;
    if (dX*dX + dY*dY) <= 10*FPenWidth then
      AImage.Pixels[X-dX, Y-dY] := FPenColor;
  end;
end;

{ TFigure }

procedure TFigure.StartDrawing(AImage: TCanvas; Button: TMouseButton;
    X, Y: Integer);
begin
  FDrawingState := True;
  FLastX := X;
  FLastY := Y;
end;

procedure TFigure.Draw(AImage: TCanvas; X, Y: Integer);
begin
  With AImage do
  begin
    Pen.Color := FPenColor;
    Pen.Style := FPenStyle;
    Pen.Width := FPenWidth;
    Brush.Color := FBrushColor;
    Brush.Style := FBrushStyle;
  end;
end;

procedure TEllipseTool.Draw(AImage: TCanvas; X, Y: Integer);
begin
  Inherited;
  AImage.Ellipse(FLastX, FLastY, X, Y);
end;

procedure TRectTool.Draw(AImage: TCanvas; X, Y: Integer);
begin
  Inherited;
  AImage.Rectangle(FLastX, FLastY, X, Y);
end;

procedure TStarTool.Draw(AImage: TCanvas; X, Y: Integer);
var
  VertexDist: Double;
  Points: Array [0..4] of TPoint;
begin
  Inherited;
  VertexDist := Sqrt(Abs((X-FLastX)*(X-FLastX) + (FLastY-Y)*(FLastY-Y)));

  Points[0].X := FLastX + (X - FLastX);
  Points[0].Y := FLastY + (Y -FLastY) - Trunc(VertexDist);

  Points[1].X := FLastX + (X - FLastX) + Trunc(VertexDist*Sin(Pi/5));
  Points[1].Y := FLastY + (Y -FLastY) + Trunc(VertexDist*Cos(Pi/5));

  Points[2].X := FLastX + (X - FLastX) - Trunc(VertexDist*Cos(Pi/10));
  Points[2].Y := FLastY + (Y -FLastY) - Trunc(VertexDist*Sin(Pi/10));

  Points[3].X := FLastX + (X - FLastX) + Trunc(VertexDist*Cos(Pi/10));
  Points[3].Y := FLastY + (Y -FLastY) - Trunc(VertexDist*Sin(Pi/10));

  Points[4].X := FLastX + (X - FLastX) - Trunc(VertexDist*Sin(Pi/5));
  Points[4].Y := FLastY + (Y -FLastY) + Trunc(VertexDist*Cos(Pi/5));

  AImage.Polygon(Points);
 end;

procedure TTriangleTool.Draw(AImage: TCanvas; X, Y: Integer);
var
  Points: Array [0..2] of TPoint;
begin
  Inherited;
  Points[0].X := X; Points[0].Y := Y;
  Points[1].X := FLastX+((X-FLastX) div 2); Points[1].Y := FLastY;
  Points[2].X := FLastX; Points[2].Y := Y;
  AImage.Polygon(Points);
end;

procedure TLineTool.Draw(AImage: TCanvas; X, Y: Integer);
begin
  Inherited;
  AImage.MoveTo(X, Y);
  AImage.LineTo(FLastX, FLastY);
end;

initialization
  Tool := TTool.Create;
end.

