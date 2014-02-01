{$mode delphi}
program tok;
uses uapp, mned, ukm, utok, ui, kvm;

type TTokEd = class (uapp.TCustomApp)
  cmd : ui.ZInput;
  procedure init; override;
  procedure step; override;
  procedure keys(km : ukm.TKeyMap); override;
  procedure DelegateKey( ext : boolean; ch : char );
  procedure OnCmdAccept( s :  string );
end;

procedure TTokEd.init;
  begin
    cmd := ui.ZInput.Create(self);
    cmd.x := xMax div 2 - 16;
    cmd.y := yMax div 2;
    cmd.dlen := 32; cmd.maxlen := cmd.dlen;
    cmd.tcol := $11f3; cmd.work := '';
    cmd.OnAccept := self.OnCmdAccept;
  end;

procedure TTokEd.keys(km : ukm.TKeyMap);
  var ch: char;
  begin
    for ch := #0 to #225 do km.crt[ ch ] := DelegateKey;
    km.cmd[ ^C ] := self.quit;
  end;

//  !! copied directly from TEditor.DelegateKey :/
procedure TTokEd.DelegateKey( ext : boolean; ch : char );
  begin
    if ext then cmd.handlestripped(ch)
    else cmd.handle(ch);
  end;

procedure TTokEd.OnCmdAccept( s : string );
  begin

  end;

procedure TTokEd.step;
  begin
    if cmd.is_dirty then cmd.Show;
  end;

begin
  uapp.run(TTokEd);
end.
