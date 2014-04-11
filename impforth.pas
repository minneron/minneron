{$mode delphiunicode}{$i xpc.inc}
program impforth;
uses xpc, classes, uapp, ukm, ui, utv, cw, kvm, cli,cx,
  uimpforth, uimpwords, uimpshell;

type
  TForthApp = class (uapp.TCustomApp)
    public
      imp : TImpForth; // active
      trm : TTermView; // active, visible
      ish : TImpShell; // active, visible
      procedure Init; override;
      procedure Keys(km : ukm.TKeyMap); override;
    end;

procedure TForthApp.init;
  begin
    imp := TImpForth.Create(self);
    trm := TTermView.Create(self);

    ish := TImpShell.Create(self, imp);
    // --TODO : tview/zobj.center --------
    ish.x := self.w div 2 - ish.w div 2;
    ish.y := self.h div 2 - ish.h div 2;
    // -----------------------------------
    with imp do begin
      addOp('bye', quit);
      mount('forth', TForthWords);
      mount('term', TTermWords);
      TTermWords(modules['term']).term := trm;
      OnChange := ish.smudge;
    end;
    _views.extend([ish, trm]);
    kvm.clrscr;
  end;

procedure TForthApp.keys(km : ukm.TKeyMap);
  begin
    ish.keys(km);
    km.cmd[ ^C ] := quit;
    km.cmd[ ^L ] := trm.smudge;
  end;

begin
  uapp.Run(TForthApp);
end.
