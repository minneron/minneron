{$mode delphiunicode}{$i xpc.inc}
program impforth;
uses xpc, uapp, ukm, ui, cw, kvm, cli,
  uimpforth, uimpwords, uimpshell;

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
    imp := TImpForth.Create(self); TImpWords.Create(imp);
    imp.AddOp('bye', quit);
    imp.OnChange := self.OnImpChange;
    cmd := TImpShell.Create(self, imp);
    // --TODO : tview/zobj.center --------
    cmd.x := self.w div 2 - cmd.w div 2;
    cmd.y := self.h div 2 - cmd.h div 2;
    // -----------------------------------
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
