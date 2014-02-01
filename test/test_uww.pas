{$mode delphi}{$H+}
{$i test_uww.def }
implementation uses uww, ugeom2d, uminneron;

var
  g2 : ugeom2d.IBounds2D;
  ww : uww.TWordWrap;

procedure setup;
  begin
    if assigned(ww) then ww.free;
    ww := TWordWrap.Create(Nil);
  end;

procedure test_first_line;
  begin
    ww.width := 32;
    g2 := TView.Create(Nil); g2.w := 10; g2.h := 1;
    ww.place(g2); chk.equal(g2.x, 0); chk.equal(g2.y, 0);
    ww.place(g2); chk.equal(g2.x,10); chk.equal(g2.y, 0);
    ww.place(g2); chk.equal(g2.x,20); chk.equal(g2.y, 0);
    ww.place(g2); chk.equal(g2.x, 0); chk.equal(g2.y, 1);
  end;

finalization
  if assigned(ww) then ww.Free;
end.
