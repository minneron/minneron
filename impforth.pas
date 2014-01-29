{$mode delphi}
program impforth;
uses uapp, ukm, uimpforth, ui, uminneron, cw, kvm,cli;

type
  TForthApp = class (uapp.TCustomApp)
    public
      cmd : ui.ZInput;
      imp : TImpForth;
      function  init : boolean; override;
      procedure keys(km : ukm.TKeyMap); override;
      procedure step; override;
      procedure prompt;
      procedure DelegateKey( ext : boolean; ch : char );
      procedure OnCmdAccept( s :  string );
    end;

function TForthApp.init : boolean;
  begin
    imp := TImpForth.Create(self);
    imp.AddOp('bye', Terminate);
    imp.AddOp('clear', kvm.work.ClrScr);
    kvm.clrscr; 
    cmd := ui.ZInput.Create(self);
    cmd.OnAccept := self.OnCmdAccept;
    cmd.y := kvm.yMax; cmd.is_dirty:=true;
    gotoxy(0,kvm.yMax-1); prompt;
    result := true;
  end;

procedure TForthApp.keys(km : ukm.TKeyMap);
  var ch : char;
  begin
    for ch := #0 to #225 do km.crt[ ch ] := DelegateKey;
    km.cmd[ ^C ] := Terminate;
  end;

procedure TForthApp.OnCmdAccept( s : string );
  begin
    imp.Send(s); prompt;
  end;

//  !! copied directly from TEditor.DelegateKey :/
procedure TForthApp.DelegateKey( ext : boolean; ch : char );
  begin
    if ext then cmd.handlestripped(ch)
    else cmd.handle(ch);
  end;

procedure TForthApp.prompt;
  begin
    cmd.work := ''; writeln; cwriteln('|cok|w')
  end;

procedure TForthApp.step;
  begin
    km.HandleKeys;
    if cmd.is_dirty then cmd.Show;
    if not imp.NeedsInput then imp.EvalNextToken;
  end;

var app : TForthApp;
begin
  uapp.Run(TForthApp.Create);
end.
