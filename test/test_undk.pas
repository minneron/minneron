{$mode delphi}{$H+}
{$i test_undk.def }
implementation uses dndk, undk, udb;

var
  ndk	: dndk.IBase;
  eid	: integer;
  node	: dndk.INode;
  edge	: dndk.IEdge;
  edges	: dndk.TEdges;

procedure setup;
  begin
    ndk := undk.open(':memory:')
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
  var builtin_count : integer;
  begin
    edges := ndk.q('~','~','~');
    builtin_count := length(edges);
    ndk.e('a','test-query','c');
    ndk.e('x','test-query','z');
    edges := ndk.q('~','~','~');
    chk.equal(2, length(edges)-builtin_count);
    edges := ndk.q('~','test-query','~');
    chk.equal('a', edges[0].sub.s);
    chk.equal('z', edges[1].obj.s);
  end;

{ procedure test_erase;  todo}
{   begin }
{   end; }

procedure test_node_io;
  begin
    chk.equal(0, length(ndk.q('n0', ':=', '')));
    chk.equal('', ndk.v('n0').s);
    ndk.a('n0','v0');
    chk.equal(1, length(ndk.q('n0', ':=', 'v0')));
    chk.equal('v0', ndk.v('n0').s);
  end;

procedure test_build_node;
  begin
    ndk.a('boy','a boy (some assembly required)');
    ndk.e('boy','name','fred');
    ndk.e('boy','loves','girl');
    ndk.e('girl','loves','boy');
    node := ndk['boy'];
    chk.equal(node.key.s, 'boy');
    chk.equal(node.val.s, 'a boy (some assembly required)');
    chk.equal(node[':='].s, 'a boy (some assembly required)');
    chk.equal(length(node.oe), 3);
    chk.equal(length(node.ie), 1);
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
    chk.equal(length(node.oe), 0);
    chk.equal(node.key.s, 'ghost');
  end;

end.
