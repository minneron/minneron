{$i xpc.inc}{$mode delphi}{$H+}
unit undk;
interface uses xpc, dndk;

  function open(path : string) : dndk.IBase;

implementation uses classes, udb;

type
  TNodakRepo = class (TComponent, IBase)
    protected
      _dbc : udb.TDatabase;
    public
      constructor New(ndkPath : string);
      function e(sub, rel, obj : string) : IEdge;   // store edge
      function f(eid : integer) : IEdge;            // fetch edge
      function q(sub,rel,obj : string) : IEdges;    // query edges
      function n(key : string) : INode;             // node name -> nid
      function a(key, val : string) : IBase;        // assign = e(key,':=',val)
      function v(key : string) : ICell;             // v(key) = n(key).val
    end;
  TBase	= class (TComponent, IBase)
    protected
      _base : IBase;
    public
      constructor New(aBase : IBase);
      property base:IBase read _base implements IBase;
    end;

type
  TCell	= class (TBase, ICell)
    protected
      _value : variant;
    public
      constructor New(aValue : variant);
      function s : string;
      function i : integer;
      function b : boolean;
      function n : INode;
    end;
  TEdge = class (TBase, IEdge)
    protected
      _eid : integer;
      _sub, _rel, _obj : string;
    public
      constructor New(aBase: IBase; aEid : integer; aSub, aRel, aObj : string);
      function eid : integer;
      function sub : ICell;
      function rel : ICell;
      function obj : ICell;
    end;
{-- TCell --}

constructor TCell.New(aValue : variant);
  begin
    _value := aValue
  end;

function TCell.s : string;
  begin
    result := string(_value)
  end;

function TCell.i : integer;
  begin
    result := integer(_value)
  end;

function TCell.b : boolean;
  begin
    result := boolean(_value)
  end;

function TCell.n : INode;
  begin
    result := _base[_value]
  end;


{-- TBase --}

constructor TBase.New(aBase: IBase);
  begin
    inherited Create(Nil);
    _base := aBase;
  end;

{-- TEdge --}

constructor TEdge.New(aBase: IBase; aEid : integer; aSub, aRel, aObj : string);
  begin
    _eid := aEid; _sub := aSub; _rel := aRel; _obj := aObj;
    inherited New(aBase)
  end;

function TEdge.eid : integer;
  begin
    result := _eid
  end;

function TEdge.sub : ICell;
  begin
    result := _base.v(_sub)
  end;

function TEdge.rel : ICell;
  begin
    result := _base.v(_rel)
  end;

function TEdge.obj : ICell;
  begin
    result := _base.v(_obj)
  end;


{-- TNodakRepo --}

constructor TNodakRepo.New(ndkPath : string);
  begin
    inherited Create(Nil);
    _dbc := udb.connect(ndkPath); //  todo: error handling
    {$i nodak-sql2pas.inc}
  end;

function TNodakRepo.e(sub, rel, obj : string) : IEdge;
  begin
    _dbc.RunSQL('insert into trip (sub, rel, obj) '
                +'values (:sub, :rel, :obj)', [sub, rel, obj]);
    result := TEdge.New(self, -MaxInt, sub, rel, obj);
  end;

function TNodakRepo.f(eid : integer) : IEdge;
  begin
    result := TEdge.New(self, -MaxInt, 'sub', 'rel', 'obj');
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
    result := TCell.New(key)
  end;

{-- unit interface --}

function open(path : string) : dndk.IBase;
  begin
    result := TNodakRepo.New(path);
  end;

begin
end.
