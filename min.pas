{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$i xpc.inc}
program min;
uses min_ed,
  var ed : editor;
begin
  crt.clrscr;
  ed := editor.create;
  if paramcount = 0 then
    writeln( 'usage : min <filename>' )
  else if ed.load( paramstr( 1 )) then
  begin
    ed.run;
    ed.destroy;
    cwriteln( '|w|!k' );
    crt.clrscr;
  end
  else writeln( 'unable to load file: ', paramstr( 1 ));
end.
