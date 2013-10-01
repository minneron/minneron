{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphi}{$i xpc.inc}{$H+}
program min;
uses xpc, mnml, mn, cw, cx, kvm, sysutils, kbd;

var ed : mn.TEditor;
begin
  ed := TEditor.Create;
  kvm.ClrScr;
  if ParamCount = 0 then
    writeln( 'usage : min <filename> ')
  else if ed.Load( ParamStr( 1 )) then
    begin
      ed.init;
      ed.draw;
      repeat
        if not keypressed then sleep(1);
        if keypressed then
          begin
            ed.onkeypress;
            ed.draw;
          end;
        mnml.step;
      until ed.done;
      cw.cwriteln( '|w|!k' );
      kvm.ClrScr;
      ed.Destroy;
    end
  else writeln( 'unable to load file: ', paramstr( 1 ))
end.
