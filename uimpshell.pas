{$mode delphi}
unit uimpshell;
interface uses custapp, uimpforth;

type
  TImpShellApp = class (TCustomApplication)
    public
      imp : TImpForth;
      procedure Initialize; override;
      procedure AddOp( const iden : TTokStr; thunk : TThunk );
      procedure DoRun; override;
      procedure Welcome; virtual;
      procedure Refill; virtual;
      procedure Respond; virtual;
    end;

implementation

{-- TImpShellApp ----------------------------------------------}

procedure TImpShellApp.Initialize;
  begin
    imp := TImpForth.Create(self);
  end;

procedure TImpShellApp.AddOp( const iden : TTokStr; thunk : TThunk );
  begin
    imp.AddOp(iden, thunk);
  end;

procedure TImpShellApp.DoRun;
  begin
    Welcome;
    repeat
      if imp.NeedsInput then Refill;
      imp.EvalNextToken;
      if imp.HasOutput then Respond;
    until terminated;
  end;

procedure TImpShellApp.Welcome;
  begin
    writeln(brand, ' ', verMaj, '.', verMin);
  end;

procedure TImpShellApp.Refill;
  var s : string;
  begin
    repeat write('> '); readln(s)
    until length(s) > 0;
    imp.send(s);
  end;

procedure TImpShellApp.Respond;
  begin
    if imp.msg <> '' then writeln(imp.msg);
    imp.msg := '';
  end;

begin
end.
