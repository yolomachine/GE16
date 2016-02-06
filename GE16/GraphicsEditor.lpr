program GraphicsEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainView, UTool, UHistory, USize
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Graphics Editor';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TResizeForm, ResizeForm);
  Application.Run;
end.

