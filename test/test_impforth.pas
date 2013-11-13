{$i test_impforth.def }
implementation uses uimpforth;

var imp : TImpForth;
  
procedure setup;
  begin
    if assigned(imp) then imp.Free;
    imp := TImpForth.Create(Nil);
  end;

procedure test_send;
  begin
    imp.Send('');
    imp.Send(' ');
    imp.Send('NOP');
    imp.Send('BYE');
  end;

finalization
  if assigned(imp) then imp.Free;
end.
