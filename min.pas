{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphi}{$i xpc.inc}{$H+}
program min;
uses xpc, cx, mnml, mned, cw, fx, kvm, sysutils, kbd,
  impworld, uminneron, cli, ub4vm, ukm, uapp;

type
  TMinApp  = class(uapp.TCustomApp)
    private
      ed : TEditor;
      b4 : TB4VM;
    public
      function  init : boolean; override;
      procedure step; override;
      procedure draw; override;
      procedure keys(km : ukm.TKeyMap); override;
    end;

function TMinApp.init : boolean;
  begin
    ed := TEditor.Create(self);
    ed.x := 5; ed.y := 2; ed.h := ed.h div 2 + 1; ed.w := 64;
    b4 := TB4VM.Create(self);
    if ParamCount = 0 then
      err := 'usage : min <filename>'
    else if not ed.Load( ParamStr( 1 )) then
      err := 'unable to load file: ' + paramstr( 1 )
    else err := '';
    result := err = '';
  end;

procedure TMinApp.keys(km : ukm.TKeyMap);
  begin
    ed.AddDefaultKeys( km );
    km.cmd[ ^C ] := self.quit;
    km.cmd[ ^L ] := self.draw;
  end;

procedure TMinApp.step;
  begin
    if ed.dirty then ed.Redraw;
    mnml.step;
    if ed.done and mnml.done then Terminate;
  end;

procedure TMinApp.draw;
 begin
   bg('k'); fg('K'); fillscreen('!@#$%^&*(){}][/=+?-_;:');
   ed.dirty := true;
 end;

begin
  impworld.world.manageKeyboard := false;
  uapp.run(TMinApp.Create);
end.
