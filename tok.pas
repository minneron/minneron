{$mode delphi}
program tok;
uses uapp, mned, ukm;

type TTokEd = class (uapp.TCustomApp)
  procedure init; override;
  procedure step; override;
  procedure keys(km : ukm.TKeyMap); override;
end;

procedure TTokEd.init;
  begin
  end;

procedure TTokEd.keys(km : ukm.TKeyMap);
  begin
    km.cmd[ ^C ] := self.quit;
  end;

procedure TTokEd.step;
  begin
  end;

begin
  uapp.run(TTokEd);
end.
