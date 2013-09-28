{$mode delphi}{$H+}
{$i test_buffers.def }
implementation uses buf;

var
  b : TBuffer;

procedure setup;
  begin
    if assigned(b) then b.Free;
    b := TBuffer.Create(64, 32);
  end;
  
procedure test_arraylike;
  begin
    chk.equal(0, b.length);
    b.AddLine('abc');
    chk.equal(1, b.length);
    chk.equal('abc', b[0]);
  end;

procedure test_delete;
  begin
    b.AddLine('abc');
    b.AddLine('efg');
    b.AddLine('hig');
    b.DelLine(1);
    chk.equal(2, b.length);
    chk.equal('abc', b[0]);
    chk.equal('hig', b[1]);
  end;

finalization
  if assigned(b) then b.Free;
end.
