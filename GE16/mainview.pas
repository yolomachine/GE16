unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Buttons, Grids, Spin, EditBtn, Crt,
  UTool, UHistory, FPCanvas, syncobjs, types;

type

  { TMainForm }

  TMainForm = class(TForm, IMainInterface)
    BStarTool: TSpeedButton;

    MainMenu:  TMainMenu;
    MenuFile:  TMenuItem;
    MenuExit:  TMenuItem;
    MenuAbout: TMenuItem;
    MenuEdit:  TMenuItem;
    MenuClear: TMenuItem;
    MenuResize: TMenuItem;
    MenuSave: TMenuItem;
    MenuUndo:  TMenuItem;
    MenuRedo:  TMenuItem;

    BitMap: TBitMap;
    PaintBox: TPaintBox;
    SaveDialog: TSaveDialog;
    SCrollBox: TScrollBox;
    STFillStyle: TStaticText;
    STLineWidth: TStaticText;
    STLineStyle: TStaticText;
    Tools:  TPanel;
    Footer: TPanel;

    BPenTool: TSpeedButton;
    BLineTool: TSpeedButton;
    BRectangleTool: TSpeedButton;
    BTriangleTool: TSpeedButton;
    BEllipseTool: TSpeedButton;
    BFloodFill: TSpeedButton;
    BDropper: TSpeedButton;
    BSprayTool: TSpeedButton;

    ColorDialog: TColorDialog;
    MainColor: TShape;
    AltColor:  TShape;
    DGPalette: TDrawGrid;

    LineWidth: TSpinEdit;
    LineStyle: TComboBox;
    FillStyle: TComboBox;

    STToolName: TStaticText;
    TrayIcon1: TTrayIcon;

    procedure ControlChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuResizeClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure BToolClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxInvalidate(Sender: TObject);
    procedure Clear;
    procedure ColorChange;
    procedure DrawingColorsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DGPaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DGPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PickColor(Button: TMouseButton; X, Y: Integer);
    procedure LineWidthChange(Sender: TObject);
    procedure FillStyleChange(Sender: TObject);
    procedure LineStyleChange(Sender: TObject);
  end;

var
  MainForm: TMainForm;
  CurrentToolID: Integer;
  ToolSet: Array [0..8] of TToolInfo;
  ColorSet: Array of Array of TColor;

implementation

uses
  USize;

{$R *.lfm}

{ TMainForm }

{ Menu }

procedure TMainForm.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.MenuSaveClick(Sender: TObject);
var
  BMP: TBitMap;
begin
  BMP := TBitMap.Create;
  BMP.Assign(BitMap);
  if SaveDialog.Execute then
    BMP.SaveToFile(SaveDialog.FileName);
  BMP.Free;
  if Length(SaveDialog.FileName) = 0 then Exit;
  MainForm.Caption := SaveDialog.FileName;
  SaveDialog.FileName := '';
end;

procedure TMainForm.MenuRedoClick(Sender: TObject);
begin
  History.Redo(BitMap.Canvas);
  PaintBox.Invalidate;
end;

procedure TMainForm.MenuResizeClick(Sender: TObject);
begin
  ResizeForm.ShowModal;
end;

procedure TMainForm.MenuUndoClick(Sender: TObject);
begin
  History.Undo(BitMap.Canvas);
  PaintBox.Invalidate;
end;

procedure TMainForm.MenuAboutClick(Sender: TObject);
begin
  ShowMessage('Graphics Editor' + sLineBreak + 'Гоменюк Александр, Б8103а, 2015');
end;

procedure TMainForm.MenuClearClick(Sender: TObject);
begin
  Clear;
  History.Clear;
  MainForm.Caption := 'Graphics Editor';
end;

 { Draw }

procedure TMainForm.Clear;
begin
  BitMap.Free;
  BitMap := TBitMap.Create;
  With BitMap do
  begin
    Height := PaintBox.Height;
    Width := PaintBox.Width;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(-1, -1, PaintBox.Width+1, PaintBox.Height+1);
  end;
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxInvalidate(Sender: TObject);
var
  Point: TPoint;
  WorldBounds: TRect;
begin
  Point := Mouse.CursorPos;
  Point := PaintBox.ScreenToClient(Point);
  if Tool.Active and (Tool is TInstant) then
    Tool.Draw(BitMap.Canvas, Point.X, Point.Y);
  WorldBounds := Bounds(0, 0, PaintBox.Width, PaintBox.Height);
  PaintBox.Canvas.CopyRect(WorldBounds, BitMap.Canvas, WorldBounds);
  if Tool.Active and (Tool is TFigure) then
    Tool.Draw(PaintBox.Canvas, Point.X, Point.Y);
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Pos('*', MainForm.Caption) = 0 then
    MainForm.Caption := MainForm.Caption + '*';
  Tool.StartDrawing(BitMap.Canvas, Button, X, Y);
  History.Start(X, Y);
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Tool.Active then
    PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Tool.StopDrawing;
  History.Stop(X, Y);
  History.Add(ToolSet[CurrentToolID]);
  if Tool is TFigure then
    Tool.Draw(BitMap.Canvas, X, Y);
end;

 { Interface }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if Screen.Width < 1024 then MainForm.Width := MainForm.Constraints.MinWidth;
  if Screen.Height < 768 then MainForm.Height := MainForm.Constraints.MinHeight;
  PaintBox.Width := MainForm.Width;
  PaintBox.Height := MainForm.Height;

  ToolSet[0] := TPenTool;
  ToolSet[1] := TLineTool;
  ToolSet[2] := TRectTool;
  ToolSet[3] := TEllipseTool;
  ToolSet[4] := TFillTool;
  ToolSet[5] := TDropperTool;
  ToolSet[6] := TSprayTool;
  ToolSet[7] := TTriangleTool;
  ToolSet[8] := TStarTool;

  BitMap := TBitMap.Create;
  With BitMap do
  begin
    Width := PaintBox.Width;
    Height := PaintBox.Height;
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(-1, -1, BitMap.Width+1, BitMap.Height+1);
  end;

  SetLength(ColorSet, DGPalette.RowCount, DGPalette.ColCount);
  DGPalette.FocusRectVisible := False;
  SCrollBox.DoubleBuffered := True;

  BPenTool.Click;
  MainInterface := Self;
  PaintBox.Invalidate;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState = wsMinimized) or
     (MainForm.Visible = False) then TrayIcon1.Visible := True;
  if MainForm.Visible = True then TrayIcon1.Visible := False;
end;

procedure TMainForm.ControlChange(Sender: TObject);
begin
  MainForm.ActiveControl := ScrollBox;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MResult: Integer;
begin
  if (MessageDlg('Are you sure you want to leave?',
                    mtConfirmation,
                    mbYesNo, 0) = mrNo) then CloseAction := caNone
  else
  if not (Pos('*', MainForm.Caption) = 0) then
  begin
    MResult := MessageDlg('Graphics Editor',
                       'Image has been changed. Would you like to save changes?',
                        mtConfirmation, [mbYes, mbNo, mbCancel], '');
      if MResult = mrYes then MenuSaveClick(Sender)
      else
      if MResult = mrCancel then CloseAction := caNone;
  end;
end;

procedure TMainForm.DrawingColorsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
    TShape(Sender).Brush.Color := ColorDialog.Color;
  ColorChange;
end;

procedure TMainForm.DGPaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Palette: Text;
  R, G, B: Byte;
  Row: Integer;
  Col: Integer;
begin
  AssignFile(Palette, 'Palette.txt');
  Reset(Palette);
  For Row := 0 to DGPalette.RowCount-1 do
  begin
    For Col := 0 to DGPalette.ColCount-1 do
    begin
      Read(Palette, R, G, B);
      DGPalette.Canvas.Brush.Color := RGBToColor(R, G, B);
      DGPalette.Canvas.FillRect(DGPalette.CellRect(Col, Row));
      ColorSet[Row, Col] := DGPalette.Canvas.Brush.Color;
    end;
    ReadLn(Palette);
  end;
  CloseFile(Palette);
end;

procedure TMainForm.DGPaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CellSize: Integer;
begin
  CellSize := DGPalette.DefaultColWidth;
  if Button = mbLeft then
    MainColor.Brush.Color := ColorSet[Y div CellSize, X div CellSize]
  else
    AltColor.Brush.Color := ColorSet[Y div CellSize, X div CellSize];
  ColorChange;
end;

procedure TMainForm.PickColor(Button: TMouseButton; X, Y: Integer);
begin
  if Button = mbLeft then
    MainColor.Brush.Color := BitMap.Canvas.Pixels[X, Y]
  else
    AltColor.Brush.Color := BitMap.Canvas.Pixels[X, Y];
  ColorChange;
end;

procedure TMainForm.ColorChange;
begin
  Tool.PenColor := MainColor.Brush.Color;
  Tool.BrushColor := AltColor.Brush.Color;
end;

procedure TMainForm.FillStyleChange(Sender: TObject);
begin
  Tool.BrushStyle := TFPBrushStyle(FillStyle.ItemIndex);
end;

procedure TMainForm.LineStyleChange(Sender: TObject);
begin
  Tool.PenStyle := TFPPenStyle(LineStyle.ItemIndex);
end;

procedure TMainForm.LineWidthChange(Sender: TObject);
begin
  Tool.PenWidth := LineWidth.Value;
end;

 { Tools }

procedure TMainForm.BToolClick(Sender: TObject);
begin
  Tool := ToolSet[TButton(Sender).Tag].Create;
  CurrentToolID := TButton(Sender).Tag;
  STToolName.Caption := 'Current Tool: ' + TButton(Sender).Hint;
  ColorChange;
  LineWidthChange(Sender);
  LineStyleChange(Sender);
  FillStyleChange(Sender);
end;

end.


