{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphi}{$i xpc.inc}{$H+}
program min;
uses xpc, mnml, mned, cw, cx, kvm, sysutils, kbd, impworld;

var
  ed  : TEditor;
  edi : IMorph;

function init : boolean;
  begin
    result := false;
    ed  := TEditor.Create;
    if ParamCount = 0 then
      writeln( 'usage : min <filename> ')
    else if not ed.Load( ParamStr( 1 )) then
      writeln( 'unable to load file: ', paramstr( 1 ))
    else
      begin
        ed.init;
        edi := ed;
        world.add(edi);
        focus := edi;
        result := true;
      end;
  end;

function done: boolean;
  begin
    result := ed.done and mnml.done;
  end;

procedure exit;
  begin
    ed.Destroy;
  end;

begin
  if init then
    repeat mnml.step until done;
  exit;
end.
