unit USize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls;

type

  { TResizeForm }

  TResizeForm = class(TForm)
    BConfirm: TButton;
    BCancel: TButton;
    STWidth: TStaticText;
    STHeight: TStaticText;
    WidthSpin: TSpinEdit;
    HeightSpin: TSpinEdit;
    procedure ButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  end;

var
  ResizeForm: TResizeForm;

implementation

uses
  MainView;

{$R *.lfm}

{ TResizeForm }

procedure TResizeForm.FormActivate(Sender: TObject);
begin
  With MainForm do
  begin
    WidthSpin.MaxValue := Screen.Width;
    HeightSpin.MaxValue := Screen.Height;
    WidthSpin.Value := PaintBox.Width;
    HeightSpin.Value := PaintBox.Height;
  end;
end;

procedure TResizeForm.ButtonClick(Sender: TObject);
begin
  if Sender = BConfirm then
    With MainForm do
    begin
      BitMap.Canvas.Pen.Style := psSolid;
      BitMap.Canvas.Pen.Color := clWhite;
      BitMap.Canvas.Brush.Color := clWhite;
      BitMap.Canvas.Brush.Style := bsSolid;
      BitMap.Width := WidthSpin.Value;
      BitMap.Height := HeightSpin.Value;
      if (PaintBox.Width < BitMap.Width) then
        BitMap.Canvas.Rectangle(PaintBox.Width, 0,
          BitMap.Width, PaintBox.Height);
      if (PaintBox.Height < BitMap.Height) then
        BitMap.Canvas.Rectangle(0, PaintBox.Height,
          BitMap.Width, BitMap.Height);
      PaintBox.Width := BitMap.Width;
      PaintBox.Height := BitMap.Height;
      MainForm.Width := PaintBox.Width;
      MainForm.Height := PaintBox.Height;
      PaintBox.Invalidate;
    end;
  Close;
end;

end.

