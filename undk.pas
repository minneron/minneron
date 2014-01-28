{$i xpc.inc}{$mode delphi}{$H+}
unit undk;
interface uses xpc, dndk;

  function open(path : string) : dndk.IBase;

implementation uses classes, udb;

type
  TNodakRepo = class (TComponent, dndk.IBase)
    function e(sub, rel, obj : string) : IEdge;    // store edge
    function f(eid : integer) : IEdge;            // fetch edge
    function q(sub,rel,obj : string) : IEdges;     // query edges
    function n(key : string) : INode;             // node name -> nid
    function a(key, val : string) : IBase;        // assign = e(key,':=',val)
    function v(key : string) : ICell;             // v(key) = n(key).val
  end;

function TNodakRepo.e(sub, rel, obj : string) : IEdge;
  begin
    result := nil
  end;

function TNodakRepo.f(eid : integer) : IEdge;
  begin
    result := nil
  end;

function TNodakRepo.q(sub,rel,obj : string) : IEdges;
  begin
    result := nil
  end;

function TNodakRepo.n(key : string) : INode;
  begin
    result := nil
  end;

function TNodakRepo.a(key, val : string) : IBase;
  begin
    result := nil
  end;

function TNodakRepo.v(key : string) : ICell;
  begin
    result := nil
  end;

function open(path : string) : dndk.IBase;
  begin
    result := TNodakRepo.Create(Nil);
  end;

begin
end.
