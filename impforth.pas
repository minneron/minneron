{$mode delphiunicode}{$i xpc.inc}
program impforth;
uses xpc, classes, uapp, ukm, ui, utv, cw, kvm, cli,cx,
  uimpforth, uimpwords, uimpshell;

type
  TForthApp = class (uapp.TCustomApp)
    public
      imp : TImpForth;
      cmd : TImpShell;
      scr : utv.TTermView;
      procedure Init; override;
      procedure Keys(km : ukm.TKeyMap); override;
      procedure Step; override;
      procedure OnImpChange;
    end;

procedure TForthApp.init;
  var term : TImpTerm;
  begin
    imp := TImpForth.Create(self);
    term := TImpTerm.Create(imp); term.view.resize(64, 16);
    scr := term.view;
    with imp do begin
      addOp('bye', quit);
      mount('term', term);
      mount('forth', TForthWords);
      OnChange := OnImpChange;
    end;
    cmd := TImpShell.Create(self, imp);
    // --TODO : tview/zobj.center --------
    cmd.x := self.w div 2 - cmd.w div 2;
    cmd.y := self.h div 2 - cmd.h div 2;
    // -----------------------------------
    _views.extend([cmd, scr]);
    kvm.clrscr;
  end;

procedure TForthApp.OnImpChange;
  begin cmd.smudge;
  end;

procedure TForthApp.keys(km : ukm.TKeyMap);
  begin
    cmd.keys(km);
    km.cmd[ ^C ] := quit;
    km.cmd[ ^L ] := scr.smudge;
  end;

procedure TForthApp.step;
  begin
    if not imp.NeedsInput then imp.EvalNextToken;
  end;

begin
  uapp.Run(TForthApp);
end.
