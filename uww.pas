{$i xpc.inc}{$mode delphi}
unit uww;
interface uses xpc, classes, ugeom2d, cw{for debug};

type
  TWordWrap = class (TComponent)
    private
      _gw,                 // gap width (space between items)
      _cx, _cy,            // current x, y
      _ww, _lh : cardinal; // wrap width and line height
    published
      constructor create(aOwner : TComponent); override;
      procedure reset;
      procedure place(item : ugeom2d.IBounds2D);
      procedure debugdraw;
      property width : cardinal read _ww write _ww;
    end;

implementation

constructor TWordWrap.Create(aOwner : TComponent);
  begin
    inherited create(aOwner);
    _cx := 0; _cy := 0; _ww := 64; _lh := 1; _gw := 1;
  end;

procedure TWordWrap.Reset;
  begin
    _cx := 0; _cy := 0;
  end;

procedure TWordWrap.Place(item : ugeom2d.IBounds2D);
  var gap : byte;
  begin //  prove this word wrap algorithm works
    if _cx = 0 then gap := 0 else gap := _gw;
    if (item.w + gap) >= (_ww - _cx - 1) then
      begin { item is wider than distance to edge, so wrap. }
	if (_cx = 0) then ok else _cy += _lh;
	_cx := 0; item.x := 0; item.y := _cy;
      end
    else item.x := _cx + gap;
    _cx += item.w + gap;
  end;

procedure TWordWrap.debugdraw;
  begin
    cxy(1, _cx, _cy, '>')
  end;

initialization
end.