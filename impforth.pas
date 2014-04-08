{$mode delphiunicode}{$i xpc.inc}
program impforth;
uses xpc, uapp, ukm, uimpforth, uimpshell, ui, cw, kvm,cli;

type
  TForthApp = class (uapp.TCustomApp)
    public
      imp : TImpForth;
      cmd : TImpShell;
      procedure Init; override;
      procedure Keys(km : ukm.TKeyMap); override;
      procedure Step; override;
      procedure OnImpChange;
    end;

procedure TForthApp.init;
  begin
    imp := TImpForth.Create(self);
    imp.AddOp('bye', quit);
    imp.AddOp('drop', imp.data.drop);
    imp.AddOp('swap', imp.data.swap);
    imp.AddOp('clear', kvm.work.ClrScr);
    imp.OnChange := self.OnImpChange;
    cmd  := TImpShell.Create(self, imp);
    _views.extend([cmd]);
    kvm.clrscr;
  end;

procedure TForthApp.OnImpChange;
  begin
    cmd.smudge;
  end;

procedure TForthApp.keys(km : ukm.TKeyMap);
  begin
    cmd.keys(km);
    km.cmd[ ^C ] := quit;
  end;

procedure TForthApp.step;
  begin
    if not imp.NeedsInput then imp.EvalNextToken;
  end;

begin
  uapp.Run(TForthApp);
end.
