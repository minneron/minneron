{ minneron
----------------------------------------------------------------
Copyright (c) 2014 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphiunicode}{$i xpc.inc}
program min;
uses xpc, cx, mnml, mned, cw, fx, kvm, sysutils, kbd,
  impworld, cli, ub4vm, ukm, uapp;

type
  TMinApp  = class(uapp.TCustomApp)
    private
      ed : TEditor;
      b4 : TB4VM;
    public
      procedure init; override;
      procedure step; override;
      procedure draw; override;
      procedure keys(km : ukm.TKeyMap); override;
    end;

procedure TMinApp.init;
  begin
    ed := TEditor.Create(self);
    ed.x := 5; ed.y := 2; ed.h := ed.h div 2 + 1; ed.w := 75;
    b4 := TB4VM.Create(self);
    if ParamCount = 1 then
      if not ed.Load( ParamStr( 1 )) then
	fail( utf8encode('unable to load file: ' +
			 utf8decode(ansistring(paramstr( 1 )))))
      else ed.status := 'welcome to minneron.'
    else ok
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
    if ed.done and mnml.done then quit;
  end;

procedure TMinApp.draw;
  begin
    fx.fillscreen($e819, '░'); //#$2591); //'░'
    //bg($e8); fg($13); fx.fillscreen('#!@#$%^&*(){}][/=+?-_;:');
    fx.txtline(0, 0, kvm.xMax, 0, $43);
    ed.dirty := true;
  end;

begin
  impworld.world.manageKeyboard := false;
  uapp.run(TMinApp);
end.
