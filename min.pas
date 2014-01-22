{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphi}{$i xpc.inc}{$H+}
program min;
uses xpc, mnml, mned, cw, cx, fx, kvm, sysutils, kbd,
  impworld, custapp, uminneron, cli, ub4vm;

type
  TMinApp  = class(TCustomApplication)
    private
      ed : TEditor;
      km : TKeyMap;
      b4 : TB4VM;
      err: string;
    public
      procedure Initialize; override;
      procedure MakeKeyMap;
      procedure DoRun; override;
      procedure Redraw;
    published
      property editor : TEditor read ed write ed;
    end;

procedure TMinApp.Initialize;
  begin
    ed := TEditor.Create(self);
    ed.x := 5;
    ed.y := 2;
    ed.h := ed.h div 2 + 1;
    ed.w := 64;

    b4 := TB4VM.Create(self);

    if ParamCount = 0 then
      err := 'usage : min <filename>'
    else if not ed.Load( ParamStr( 1 )) then
      err := 'unable to load file: ' + paramstr( 1 )
    else err := '';
    if err = '' then MakeKeyMap else terminate
  end;

procedure TMinApp.MakeKeyMap;
  begin
    km := TKeyMap.Create(self);
    km.cmd[ ^L ] := self.redraw;
    ed.AddDefaultKeys( km );
  end;

procedure TMinApp.DoRun;
  begin
    km.HandleKeys;
    if ed.dirty then ed.Redraw;
    mnml.step;
    if ed.done and mnml.done then Terminate;
  end;

procedure TMinApp.Redraw;
 begin
   bg('k'); fg('K'); fillscreen('!@#$%^&*(){}][/=+?-_;:');
 end;

var app : TMinApp;
begin
  impworld.world.manageKeyboard := false;
  app := TMinApp.Create(nil);
  CustomApplication := app;
  app.Initialize;
  if app.err = '' then
    begin
      bg('k'); fg('K'); fillscreen('!@#$%^&*(){}][/=+?-_;:');
      app.Run; app.Free;
      fg('w'); bg('k'); clrscr; showcursor;
    end
  else writeln(app.err)
end.
