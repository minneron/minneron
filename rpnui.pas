{$mode delphiunicode}{$i xpc}
program rpnui;
uses xpc, sysutils,
  uapp, utv, kvm, kbd, ui, stacks, cw, ustr, ukm, num, fx;

type
  TApp = class (uapp.TCustomApp)
    data, side : GStack<variant>;
    cmd	       : ui.ZInput;
    procedure init; override;
    procedure step; override;
    procedure draw; override;
    procedure hide;
    procedure keys(km : ukm.TKeyMap); override;
    procedure OnCmdAccept( s :  string );
    procedure push;
    procedure pop;
  end;	       

procedure draw(x, y : word; linno : byte; val : variant);
  begin
    cxy($19e8, x, y, n2s(linno) + ':');
    cxy($190f, x+2, y, rpad(cwesc(val), 32, ' '));
  end;

function getitem(vals:GStack<variant>; k : integer; d:variant):variant;
  begin
    if k < vals.count then result := vals[k] else result := d;
  end;
  
procedure up(x, y : word; vals : GStack<variant>);
  var i : integer;
  begin
    for i := 9 downto 0
    do draw(x, y+8-i, i, getitem(vals, i, ''))
  end;

procedure dn(x, y : word; vals : GStack<variant>);
  var i : integer;
  begin
    for i := 0 to 4
    do draw(x, y+i, i, getitem(vals, i, ''))
  end;

procedure TApp.init;
  begin
    data := GStack<variant>.Create(18);
    side := GStack<variant>.Create(18);
    cmd  := ui.zinput.Create(self);
    cmd.x := xMax div 2 - 16; cmd.y := yMax div 2;
    cmd.dlen := 31; cmd.maxlen := cmd.dlen;
    cmd.OnAccept := self.OnCmdAccept;
  end;

procedure TApp.step;
  begin
    if cmd.is_dirty then draw;
  end;
  
procedure TApp.draw;
  begin
    fx.rectangle( cmd.x-3, cmd.y-11, cmd.x+32, cmd.y+6, $19e8 );
    up(cmd.x-2, cmd.y - 9, data);
    cxy($e819, cmd.x-2, cmd.y, '> ');
    dn(cmd.x-2, cmd.y + 1, side);
    cmd.is_dirty := true; cmd.show;
  end;

procedure TApp.keys(km : ukm.TKeyMap);
  begin
    cmd.keys(km);
    with km do begin
      cmd[ ^C ] := self.quit;
      cmd[ ^O ] := self.pop;
      cmd[ ^U ] := self.push;
      cmd[ ^L ] := self.draw;
      cmd[ ^I ] := self.hide;
    end;
  end;

procedure TApp.OnCmdAccept( s : string );
  begin
    data.push(s);
    cmd.reset; cmd.work := ''; 
    draw;
  end;

procedure TApp.hide;
  begin
    fx.fillscreen($e819, '░'); //#$2591); //'░'
  end;
  
procedure TApp.Push;
  begin
    try side.push(cmd.work); cmd.work := data.popGet('');
    except on EStackOverflow do data.shift
    end;
    draw;
  end;

procedure TApp.Pop;
  begin
    try data.push(cmd.work); cmd.work := side.popGet('');
    except on EStackOverflow do data.shift
    end;
    draw;
  end;

begin
  uapp.Run(TApp)
end.
