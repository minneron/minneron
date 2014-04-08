{$mode delphiunicode}{$i xpc}
unit uimpshell;
interface uses xpc, custapp, classes, variants, uimpforth,
  utv, kvm, kbd, ui, stacks, cw, ustr, ukm, num, fx;

type
  { this is a bare-bones application that reads input with readln }
  TImpShellApp = class (custapp.TCustomApplication)
    public
      imp : TImpForth;
      procedure Initialize; override;
      procedure AddOp( const iden : TTokStr; thunk : TThunk );
      procedure DoRun; override;
      procedure Welcome; virtual;
      procedure Refill; virtual;
      procedure Respond; virtual;
   end;

  { this is a more advanced widget that uses the kvm and ui units }
  TImpShell  = class (utv.TView)
    protected
      cmd : ui.ZInput;
    public
      imp : TImpForth;
      constructor Create(AOwner : TComponent; aImp : TImpForth);
        reintroduce;
      procedure Keys(km : ukm.TKeyMap);
      procedure Push;
      procedure Pop;
      procedure Smudge;
      procedure OnCmdAccept( s :  string );
    published
      procedure Render; override;
    end;

implementation

{-- TImpShell -------------------------------------------------}

constructor TImpShell.Create(aOwner : TComponent; aImp : TImpForth);
  begin
    inherited create(aOwner);
    imp := aImp;
    cmd := ui.zinput.Create(self);
    cmd.x := xMax div 2 - 16; cmd.y := yMax div 2;
    cmd.w := 32; cmd.maxlen := 64;
    cmd.OnAccept := self.OnCmdAccept;
    _views.extend([cmd]); resize( 64, 18 );
  end;

procedure TImpShell.keys(km : ukm.TKeyMap);
  begin
    cmd.keys(km);
    with km do begin
      cmd[ ^O ] := self.pop;
      cmd[ ^U ] := self.push;
      cmd[ ^L ] := self.smudge;
    end
  end;

procedure TImpShell.Render;

  procedure drawLine(x, y : word; linno : byte; val : variant);
    begin
      cxy($19e8, x, y, n2s(linno) + ':');
      cxy($190f, x+2, y, rpad(cwesc(val), 32, ' '));
    end;

  procedure up(x, y : word; vals : GStack<variant>);
    var i : integer;
    begin
      for i := 9 downto 0 do drawLine(x, y+8-i, i, vals.GetItem( i, '' ))
    end;

  procedure dn(x, y : word; vals : GStack<variant>);
    var i : integer;
    begin
      for i := 0 to 4 do drawLine(x, y+i, i, vals.GetItem( i, '' ))
    end;

  begin { Render }
    fx.rectangle( cmd.x-3, cmd.y-11, cmd.x+32, cmd.y+6, $19e8 );
    up(cmd.x-2, cmd.y - 9, imp.data);
    cxy($e819, cmd.x-2, cmd.y, '> ');
    dn(cmd.x-2, cmd.y + 1, imp.side);
  end;

procedure TImpShell.OnCmdAccept( s : string );
  begin imp.Send(s); smudge; cmd.reset; cmd.work := '';
  end;

procedure TImpShell.Smudge;
  begin inherited smudge; if assigned(cmd) then cmd.smudge;
  end;

procedure TImpShell.Push;
  begin
    try imp.side.push(cmd.work); cmd.work := imp.data.popGet('');
    except on EStackOverflow do imp.data.shift end;
    smudge;
  end;

procedure TImpShell.Pop;
  begin
    try imp.data.push(cmd.work); cmd.work := imp.side.popGet('');
    except on EStackOverflow do imp.side.shift end;
    smudge;
  end;


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
