{ a little screen saver thing.
  this is mostly for testing the multi-tasking stuff }
{$mode delphi}
unit mnrnd;
interface uses
  xpc, fs, stri, num, cw, ui, kvm, kbd, cli,
  tiles, vorunati, sysutils, mnml, mnbuf;

  var cmd_rnd : TCmdId;
  procedure EmitRandomChars;

implementation

  procedure EmitRandomChars;
  var x, y : byte;
  begin
    x := kvm.maxX div 2 + 1
         + round((random-0.5) * (random( kvm.Width )));
    { random * random will give a strong bias to top of screen }
    y := round(random * random * random( kvm.Width ));
    kvm.Fg( random( 16 )); kvm.Bg( 0 );
    kvm.GotoXY( x, y );
    write( chr( random( 128 - 32 ) + 32 ));
    if kbd.KeyPressed then
      begin
        kbd.ReadKey;
        ShowCursor;
      end
    else mnml.launch(cmd_rnd);
  end;

initialization
  mnml.define(cmd_rnd, @EmitRandomChars, 'rnd');
end.
