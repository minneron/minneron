{$mode delphi}{$H+}
{$i test_uimpndk.def }
implementation uses dndk, undk, uimpforth, uimpndk;

var
  imp	: TImpForth;
  ndk	: dndk.IBase;
  ind	: uimpndk.TNdkWords;
  edge	: dndk.IEdge;
  edges	: dndk.TEdges;
  eid	: Int32;

procedure setup;
  begin
    ndk := undk.open(':memory:');
    imp := TImpForth.Create(nil);
    ind := TNdkWords.Create(nil);
    ind.imp := imp;
    ind.ndk := ndk;
    imp.Mount('ndk', ind);
  end;


procedure test_edge;
  begin
    imp.data.push3('sub','rel','obj');
    imp.eval('+e');
    eid  := imp.data.pop;
    edge := ndk.edges[eid];
    chk.equal(eid, edge.eid);
    chk.equal('sub', edge.sub.s);
    chk.equal('rel', edge.rel.s);
    chk.equal('obj', edge.obj.s);
  end;

finalization
  if assigned(imp) then imp.free
end.
