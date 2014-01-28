{$i xpc.inc}{$mode delphi}{$H+}
unit undk;
interface uses xpc, dndk;

  function open(path : string) : dndk.IBase;

implementation uses classes, udb, sysutils;

type

  TNodakRepo = class (TComponent, IBase)
    protected
      _dbc : udb.TDatabase;
    public
      constructor New(ndkPath : string);
      function e(sub, rel, obj : string) : IEdge;   // store edge
      function f(eid : integer) : IEdge;            // fetch edge
      function q(sub,rel,obj : string) : TEdges;    // query edges
      function n(key : string) : INode;             // node name -> nid
      function a(key, val : string) : IEdge;        // assign = e(key,':=',val)
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
  var rs : udb.TRecordset;
  begin
    _dbc.RunSQL('insert into trip (sub, rel, obj) '
                +'values (:sub, :rel, :obj)', [sub, rel, obj]);
    rs := _dbc.Query('select v from meta where k=:k',['last_insert_rowid']);
    result := TEdge.New(self,rs['v'], sub, rel, obj);
    rs.Free;
  end;

function TNodakRepo.f(eid : integer) : IEdge;
  var rs : udb.TRecordset;
  begin
    rs := _dbc.Query('select id,sub,rel,obj from trip where id=:eid',[eid]);
    if rs.recordcount = 0 then
      raise Exception.Create('no edge with eid=' + IntToStr(eid));
    result := TEdge.New(self, rs['id'], rs['sub'], rs['rel'], rs['obj']);
    rs.Free;
  end;

function sqlEsc(s : string) : string;
  begin
    result := ''''
      + sysutils.StringReplace(s, '''', '''''', [rfReplaceAll])
      + '''';
  end;

function TNodakRepo.q(sub,rel,obj : string) : TEdges;
  var sql : string = ''; rs : udb.TRecordSet; i : cardinal = 0;
  begin
    sql := 'select id, sub, rel, obj from trip where 1';
    if sub <> '~' then sql += ' and sub=' + sqlEsc(sub);
    if sub <> '~' then sql += ' and rel=' + sqlEsc(rel);
    if sub <> '~' then sql += ' and obj=' + sqlEsc(obj);
    sql += ' order by id';
    rs := _dbc.Query(sql,[]);
    SetLength(result, rs.RecordCount);
    while not rs.EOF do
      begin
        result[i] := TEdge.New(self, rs['id'], rs['sub'], rs['rel'], rs['obj']);
        rs.Next; i += 1;
      end;
    rs.Free;
  end;

function TNodakRepo.n(key : string) : INode;
  begin
    result := nil
  end;

function TNodakRepo.a(key, val : string) : IEdge;
  begin
    result := self.e(key, '', val);
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
