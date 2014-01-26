{$mode delphi}{$H+}
{$i test_undk.def }
implementation uses undk, udb;

var
  ndk  : undk.TNodak;
  eid,
  nid  : cardinal;
  node : undk.INode;
  edge : undk.IEdge;

procedure setup;
  begin
    if assigned(ndk) then begin ndk.close; ndk.free end;
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
    edge := ndk[eid];
    chk.equal(eid, edge.eid);
    chk.equal('a', edge.sub);
    chk.equal('b', edge.rel);
    chk.equal('c', edge.obj);
  end;

procedure test_query;
  begin
    eid := ndk.e('a','b','c');
    eid := ndk.e('x','y','z');
    data := ndk.q('~','~','~');
    chk.equal(2, length(data));
    chk.equal('a', data[0].sub.val);
    chk.equal('z', data[1].obj.val);
  end;

{ procedure test_erase;  todo}
{   begin }
{   end; }

procedure test_put_node;
  begin
    ndk.n('n0','v0');
    chk(1, length(ndk.q('n0', ':=', 'v0')));
  end;

procedure test_build_node;
  begin
    ndk.e('boy', ':=', 'boy (some assembly required)')
       .e('boy','name','fred')
       .e('boy','loves','girl')
       .e('girl','loves','boy');
    node := ndk['boy'];
    chk.equal(node[':='].str, 'a boy (some assembly required)');
    chk.equal(node.oe.len, 2);
    chk.equal(node.ie.len, 3);
    chk.equal(node.oe[0].sub.str, 'boy');
    chk.equal(node.oe[0].rel.str, ':=');
    chk.equal(node.oe[0].obj.str, 'a boy (some assembly required)');
    chk.equal(node.oe[1].rel.str, 'name');
    chk.equal(node.oe[2].rel.str, 'loves');
    chk.equal(node.ie[0].rel.str, 'loves');
  end;

procedure test_build_empty;
  begin
    node := ndk['ghost'];
    chk.equal(node[':='], null);
    chk.equal(node.o.len, 0);
    chk.equal(node.val, 'ghost');
  end;

begin
  if assigned(ndk) then ndk.free;
end.
