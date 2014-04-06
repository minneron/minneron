{$mode delphiunicode}{$i xpc.inc}
program impforth;
uses xpc, uapp, ukm, uimpforth, ui, cw, kvm,cli;

type
  TForthApp = class (uapp.TCustomApp)
    public
      cmd : ui.ZInput;
      imp : TImpForth;
      procedure init; override;
      procedure keys(km : ukm.TKeyMap); override;
      procedure step; override;
      procedure prompt;
      procedure OnCmdAccept( s :  string );
    end;

procedure TForthApp.init;
  begin
    imp := TImpForth.Create(self);
    imp.AddOp('bye', quit);
    imp.AddOp('clear', kvm.work.ClrScr);
    kvm.clrscr; 
    cmd := ui.ZInput.Create(self);
    cmd.OnAccept := self.OnCmdAccept;
    cmd.y := kvm.yMax; cmd.is_dirty:=true;
    gotoxy(0,kvm.yMax-1); prompt;
  end;

procedure TForthApp.keys(km : ukm.TKeyMap);
  begin
    cmd.keys( km );
    km.cmd[ ^C ] := quit;
  end;

procedure TForthApp.OnCmdAccept( s : TStr );
  begin
    imp.Send(s); prompt;
  end;


procedure TForthApp.prompt;
  begin
    cmd.work := ''; writeln; cwriteln('|cok|w')
  end;

procedure TForthApp.step;
  begin
    if cmd.is_dirty then cmd.Show;
    if not imp.NeedsInput then imp.EvalNextToken;
  end;

begin
  uapp.Run(TForthApp);
end.
