{$mode delphi}{$H+}
{$i test_undk.def }
implementation uses dndk, undk, udb;

var
  ndk	: dndk.IBase;
  eid,
  nid	: integer;
  node	: dndk.INode;
  edge	: dndk.IEdge;
  nodes	: dndk.INodes;
  edges	: dndk.IEdges;

procedure setup;
  begin
    ndk := undk.open(':memory:')
  end;

procedure test_add;
  begin
    chk.equal(ndk.e('a','b','c').eid, 1);
    chk.equal(ndk.e('a','b','c').eid, 2);
  end;

procedure test_edge;
  begin
    ndk.e('a', 'b', 'c');
    eid  := ndk.e('a', 'b', 'c').eid;
    edge := ndk.edges[eid];
    chk.equal(eid, edge.eid);
    chk.equal('a', edge.sub.s);
    chk.equal('b', edge.rel.s);
    chk.equal('c', edge.obj.s);
  end;

procedure test_query;
  begin
    eid := ndk.e('a','b','c').eid;
    eid := ndk.e('x','y','z').eid;
    edges := ndk.q('~','~','~');
    chk.equal(2, edges.len);
    chk.equal('a', edges[0].sub.s);
    chk.equal('z', edges[1].obj.s);
  end;

{ procedure test_erase;  todo}
{   begin }
{   end; }

procedure test_put_node;
  begin
    ndk.a('n0','v0');
    chk.equal(1, ndk.q('n0', ':=', 'v0').len);
  end;

procedure test_build_node;
  begin
    ndk.e('boy', ':=', 'boy (some assembly required)');
    ndk.e('boy','name','fred');
    ndk.e('boy','loves','girl');
    ndk.e('girl','loves','boy');
    node := ndk['boy'];
    chk.equal(node[':='].s, 'a boy (some assembly required)');
    chk.equal(node.oe.len, 2);
    chk.equal(node.ie.len, 3);
    chk.equal(node.oe[0].sub.s, 'boy');
    chk.equal(node.oe[0].rel.s, ':=');
    chk.equal(node.oe[0].obj.s, 'a boy (some assembly required)');
    chk.equal(node.oe[1].rel.s, 'name');
    chk.equal(node.oe[2].rel.s, 'loves');
    chk.equal(node.ie[0].rel.s, 'loves');
  end;

procedure test_build_empty;
  begin
    node := ndk['ghost'];
    chk.equal(node[':='].s, '');
    chk.equal(node.oe.len, 0);
    chk.equal(node.key.s, 'ghost');
  end;

end.
