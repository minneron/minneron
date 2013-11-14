{$mode delphi}
program impforth;
uses custapp, uimpforth, ui, uminneron, cw, kvm,cli;

type
  TForthApp = class (TCustomApplication)
    public
      cmd : ui.ZInput;
      imp : TImpForth;
      km  : TKeyMap;
      procedure Initialize; override;
      procedure DoRun; override;
      procedure OnCmdAccept( s :  string );
      procedure DelegateKey( ext : boolean; ch : char );
    end;

procedure TForthApp.Initialize;
  var ch : char;
  begin
    km := TKeyMap.Create(self);
    for ch := #0 to #225 do km.crt[ ch ] := DelegateKey;
    km.cmd[ ^C ] := Terminate;

    imp := TImpForth.Create(self);
    imp.AddOp('bye', Terminate);
    imp.AddOp('clear', kvm.work.ClrScr);

    cmd := ui.ZInput.Create(self);
    cmd.OnAccept := self.OnCmdAccept;
    cmd.y := kvm.maxy;

    kvm.ClrScr;
    cw.cxy(37, 0, kvm.maxy-1, 'ok');
  end;

procedure TForthApp.OnCmdAccept( s : string );
  begin
    imp.Send(s);
    cmd.work := '';
    writeln;
  end;

//  !! copied directly from TEditor.DelegateKey :/
procedure TForthApp.DelegateKey( ext : boolean; ch : char );
  begin
    if ext then cmd.handlestripped(ch)
    else cmd.handle(ch);
  end;

var stepcount : cardinal = 0;
procedure TForthApp.DoRun;
  begin
    km.HandleKeys;
    if cmd.is_dirty then cmd.Show;
    if not imp.NeedsInput then imp.EvalNextToken;
  end;

var app : TForthApp;
begin
  app := TForthApp.Create(Nil);
  app.Initialize;
  app.Run;
  app.Free;
end.
