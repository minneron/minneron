{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphi}{$i xpc.inc}{$H+}
program min;
uses xpc, mnml, mned, cw, cx, kvm, sysutils, kbd, impworld;

var ed : mned.TEditor;

function init : boolean;
  begin
    result := false;
    ed := TEditor.Create;
    if ParamCount = 0 then
      writeln( 'usage : min <filename> ')
    else if not ed.Load( ParamStr( 1 )) then
      writeln( 'unable to load file: ', paramstr( 1 ))
    else
      begin
        ed.init;
        result := true;
      end;
  end;

procedure step;
  begin
    if not keypressed then sleep(1);
    mnml.step;
    impworld.step;
    if keypressed then
      begin
        ed.onkeypress;
        ed.draw;
      end;
  end;

procedure draw;
  begin
    ed.draw;
    impworld.draw;
  end;

function done: boolean;
  begin
    result := ed.done and impworld.done;
  end;

procedure exit;
  begin
    ed.Destroy;
  end;

begin
  if init then
    begin
      repeat
        step;
        draw;
      until done;
      exit;
    end
end.
