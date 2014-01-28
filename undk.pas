{$i xpc.inc}{$mode delphi}{$H+}
unit undk;
interface uses xpc, dndk;

  function open(path : string) : dndk.IBase;

implementation uses classes, udb;
type
  TBase = class (TInterfacedObject, IBase)
    protected
      _base : IBase;
      property base : IBase read _base implements IBase;
    end;
  TCell	= class (TBase, ICell)
    public
      function s : string;
      function i : integer;
      function b : boolean;
      function n : INode;
    end;
  TEdge = class (TBase, IEdge)
    public
      function eid : integer;
      function sub : ICell;
      function rel : ICell;
      function obj : ICell;
    end;
  TNodakRepo = class (TComponent, IBase)
    function e(sub, rel, obj : string) : IEdge;   // store edge
    function f(eid : integer) : IEdge;            // fetch edge
    function q(sub,rel,obj : string) : IEdges;    // query edges
    function n(key : string) : INode;             // node name -> nid
    function a(key, val : string) : IBase;        // assign = e(key,':=',val)
    function v(key : string) : ICell;             // v(key) = n(key).val
  end;

{-- TCell --}

function TCell.s : string;
  begin
    result := ''
  end;

function TCell.i : integer;
  begin
    result := 0
  end;

function TCell.b : boolean;
  begin
    result := false
  end;

function TCell.n : INode;
  begin
    result := nil
  end;


{-- TEdge --}

function TEdge.eid : integer;
  begin
    result := 0
  end;

function TEdge.sub : ICell;
  begin
    result := nil
  end;

function TEdge.rel : ICell;
  begin
    result := nil
  end;

function TEdge.obj : ICell;
  begin
    result := nil
  end;


{-- TNodakRepo --}

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


{-- unit interface --}

function open(path : string) : dndk.IBase;
  begin
    result := TNodakRepo.Create(Nil);
  end;

begin
end.
