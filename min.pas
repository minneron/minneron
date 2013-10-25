{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphi}{$i xpc.inc}{$H+}
program min;
uses xpc, mnml, mned, cw, cx, fx, kvm, sysutils, kbd, impworld, custapp;

type
  TMinApp  = class(TCustomApplication)
    private
      ed  : TEditor;
    public
      procedure Initialize; override;
      procedure DoRun; override;
    published
      property editor : TEditor read ed write ed;
    end;


procedure TMinApp.Initialize;
  var
    okay : boolean;
  begin
    ed := TEditor.Create(self);
    ed.x := 5;
    ed.y := 3;
    ed.h := ed.h div 2 + 1;
    ed.w := 80;
    okay := false;
    if ParamCount = 0 then
      writeln( 'usage : min <filename> ')
    else if not ed.Load( ParamStr( 1 )) then
      writeln( 'unable to load file: ', paramstr( 1 ))
    else
      okay := true;
    if not okay then terminate;
  end;


procedure TMinApp.DoRun;
  begin
    if keypressed then ed.OnKeyPress(readkey);
    if ed.dirty then ed.Redraw;
    mnml.step;
    if ed.done and mnml.done then Terminate;
  end; { TMinApp.DoRun }

var app : TMinApp;
begin
  impworld.world.manageKeyboard := false;
  app := TMinApp.Create(nil);
  CustomApplication := app;
  app.Initialize;
  bg('k'); fg('K'); fillscreen('!@#$%^&*(){}][/=+?-_;:');
  app.Run;
  app.Free;
  fg(7); bg(0); clrscr; showcursor;
end.
