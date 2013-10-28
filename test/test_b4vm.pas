{$mode delphi}{$H+}
{$i test_b4vm.def }
implementation uses ub4vm;

var
  b4 : TB4VM;
  vm : array of TB4VM;
  i  : integer;

procedure setup;
  begin
    if assigned(b4) then b4.Free;
    b4 := TB4VM.Create(Nil);
  end;
  

finalization
  if assigned(b4) then b4.Free;
end.
