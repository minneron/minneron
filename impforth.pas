{$mode delphi}
program impforth;
uses custapp, uimpforth, ui, uminneron;

type
  TForthApp = class (TCustomApplication)
    public
      cmd : ui.ZInput;
      imp : TImpForth;
      km  : TKeyMap;
      procedure Initialize; override;
      procedure DoRun; override;
      procedure DelegateKey( ext : boolean; ch : char );
    end;

procedure TForthApp.Initialize;
  var ch : char;
  begin
    km := TKeyMap.Create(self);
    cmd := ui.ZInput.Create(self);
    for ch := #0 to #225 do km.crt[ ch ] := DelegateKey;
    km.cmd[ ^C ] := Terminate;
    imp.CreateOp('bye', Terminate);
    WriteLn('ok');
  end;

//  !! copied directly from TEditor.DelegateKey :/
procedure TForthApp.DelegateKey( ext : boolean; ch : char );
  begin
    if ext then cmd.handlestripped(ch)
    else cmd.handle(ch);
  end;

procedure TForthApp.DoRun;
  begin
    km.HandleKeys;
  end;

var app : TForthApp;
begin
  app := TForthApp.Create(Nil);
  app.Initialize;
  app.Run;
  app.Free;
end.
