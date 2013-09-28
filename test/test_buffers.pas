{$mode delphi}{$H+}
{$i test_buffers.def }
implementation uses buf;

var
  b : TBuffer;

procedure setup;
  begin
    if assigned(b) then b.Free;
    b := TBuffer.Create;
  end;
  
procedure test_arraylike;
  begin
    chk.equal(0, b.length);
    b.AddLine('abc');
    chk.equal(1, b.length);
    chk.equal('abc', b[0]);
  end;

finalization
  if assigned(b) then b.Free;
end.
