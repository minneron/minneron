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
    chk.equal('',      imp.NextToken);
    imp.Send('test  the lexer');
    chk.equal('test',  imp.NextToken);
    chk.equal('the',   imp.NextToken);
    chk.equal('lexer', imp.NextToken);
    chk.equal('',      imp.NextToken);
  end;

procedure test_EvalNextToken;
  begin
    // if the buffer's empty it should still silently do nothing.
    imp.EvalNextToken;
  end;

finalization
  if assigned(imp) then imp.Free;
end.
