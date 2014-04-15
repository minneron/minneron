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
      procedure AddOp( const iden : TStr; thunk : TThunk );
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
      constructor Create(AOwner : TComponent; aImp : TImpForth);
        reintroduce;
      procedure Update; override;
      procedure Render; override;
      procedure RestoreCursor; override;
    public
      imp : TImpForth;
      procedure Keys(km : ukm.TKeyMap);
      procedure Smudge;
      procedure Push;
      procedure Pop;
      procedure OnCmdAccept( s :  string );
    end;

implementation

{-- TImpShell -------------------------------------------------}

constructor TImpShell.Create(aOwner : TComponent; aImp : TImpForth);
  begin
    inherited create(aOwner);
    imp := aImp;
    resize( 16, 8 );
    cmd := ui.zinput.Create(self);
    cmd.x := 2; cmd.y := 2 * self.h div 3;
    cmd.w := self.w-2; cmd.maxlen := 64;
    cmd.OnAccept := self.OnCmdAccept;
    _views.extend([cmd]);
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

procedure TImpShell.Update;
  begin
    if not imp.NeedsInput then imp.EvalNextToken;
    inherited Update;
  end;

procedure TImpShell.Render;
  procedure drawLine(x, y : word; linno : byte; val : variant);
    type TStrFn = function(s : TStr; len:cardinal; ch: TChr=' ') : TStr;
    var gutfg, txtfg : byte; sfn : TStrFn = @ustr.rpad;
    begin
      if val = null then begin gutfg := $20; val :='' end
      else gutfg := $e8;
      if varIsNumeric(val)
	then begin txtfg := $0b; sfn := @ustr.lpad end
        else begin txtfg := $0a; sfn := @ustr.rpad end;
      cxy(($19 shl 8) + gutfg, x, y, n2s(linno) + ':');
      cxy(($20 shl 8) + txtfg, x+2, y, sfn(cwesc(val), self.w-2, ' '));
    end;

  procedure up(n : word; vals : GStack<variant>);
    var i : integer;
    begin for i := 0 to n-1 do drawLine(0, i, n-1-i, vals.GetItem(n-1-i, null))
    end;

  procedure dn(y, n : word; vals : GStack<variant>);
    var i : integer;
    begin for i := 0 to n-1 do drawLine(0, y+i, i, vals.GetItem(i, null))
    end;

  begin { Render }
    kvm.hidecursor;
    up(cmd.y, imp.data);
    cxy($e819, 0, cmd.y, '> ');
    dn(cmd.y+1, h - cmd.y, imp.side);
  end;



procedure TImpShell.RestoreCursor;
  begin gotoxy(_x + cmd.x + cmd.cpos, _y + cmd.y)
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

procedure TImpShellApp.AddOp( const iden : TStr; thunk : TThunk );
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
