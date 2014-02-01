{$mode delphi}
unit uww;
interface uses classes, ugeom2d;

type
  TWordWrap = class (TComponent)
    private
      _cx, _cy,            // current x, y
      _ww, _lh : cardinal; // wrap width and line height
    published
      constructor create(aOwner : TComponent); override;
      procedure reset;
      procedure place(item : ugeom2d.IBounds2D);
      property width : cardinal read _ww write _ww;
    end;

implementation

constructor TWordWrap.Create(aOwner : TComponent);
  begin
    inherited create(aOwner);
    _cx := 0; _cy := 0; _ww := 64; _lh := 1;
  end;

procedure TWordWrap.Reset;
  begin
    _cx := 0; _cy := 0;
  end;

procedure TWordWrap.Place(item : ugeom2d.IBounds2D);
  begin //  prove this word wrap algorithm works
    { wrap when item is wider than the distance to the edge. }
    if (item.w > _ww - _cx) then
      begin
	if (_cx<>0) then _cy += _lh;
	_cx := 0; item.x := 0; item.y := _cy;
      end
    else
      begin
	item.x := _cx; _cx += item.w;
      end
  end;

initialization
end.