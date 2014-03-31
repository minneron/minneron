{$i xpc.inc}{$mode delphi}{$H+}
unit undk;
interface uses xpc, dndk, classes, udb, sysutils, fs;

  function open(path : string) : dndk.IBase;

type

  TNodakRepo = class (TComponent, IBase)
    protected
      _dbc  : udb.TDatabase;
    public
      constructor Open(ndkPath : string);
      function e(sub, rel, obj : string; seq:integer=0) : IEdge;   // store edge
      function f(eid : integer) : IEdge;            // fetch edge
      function q(sub,rel,obj : string) : TEdges;    // query edges
      function w(sub,rel,obj : string) : TEdges;    // write edges (to debug)
      function n(key : string) : INode;             // node name -> nid
      function a(key, val : string) : IEdge;        // assign = e(key,':=',val)
      function v(key : string) : ICell;             // v(key) = n(key).val
    published
      property dbc : udb.TDatabase read _dbc;
      property nodes[s : string] : INode read n; default;
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
      _eid, _seq : integer;
      _sub, _rel, _obj : string;
    public
      constructor New(aBase: IBase; aEid,aSeq : integer; aSub, aRel, aObj : string);
      function eid : integer;
      function sub : ICell;
      function rel : ICell;
      function obj : ICell;
      function GetSeq : integer;
      procedure SetSeq(val : integer);
    end;

  TNode = class (TBase, INode)
    protected
      _nid : integer; _key : string;
    public
      constructor New(aBase: IBase; aKey : string);
      function nid : integer;
      function key : ICell;
      function val : ICell;
      function ie : TEdges;
      function oe : TEdges;
      function qe(s : string) : TEdges;
      function q1(s : string) : ICell;
      property any[s : string] : ICell  read q1; default;
    end;

implementation

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
    result := _base[self.s]
  end;


{-- TBase --}

constructor TBase.New(aBase: IBase);
  begin
    inherited Create(Nil);
    _base := aBase;
  end;

{-- TEdge --}

constructor TEdge.New(aBase: IBase; aEid, aSeq : integer; aSub, aRel, aObj : string);
  begin
    _eid := aEid; _seq := aSeq; _sub := aSub; _rel := aRel; _obj := aObj;
    inherited New(aBase)
  end;

function TEdge.eid : integer;
  begin
    result := _eid
  end;

function TEdge.sub : ICell; begin result := TCell.New(_sub) end;
function TEdge.rel : ICell; begin result := TCell.New(_rel) end;
function TEdge.obj : ICell; begin result := TCell.New(_obj) end;

function TEdge.GetSeq : integer;
  begin
    result := _seq
  end;

procedure TEdge.SetSeq( val : integer );
  begin
    _seq := val;
    with _base as TNodakRepo do
      _dbc.RunSQL('update edge set seq=:seq where eid=:eid', [_seq, _eid]);
  end;

{-- TNode --}

constructor TNode.New(aBase: IBase; aKey : string);
  begin
    _base := aBase; _key := aKey;
  end;

function TNode.nid : integer; begin result := _nid; end;
function TNode.key : ICell; begin result := TCell.New(_key) end;
function TNode.val : ICell; begin result := self[':='] end;

function TNode.ie : TEdges; begin result := _base.q('~', '~', _key); end;
function TNode.oe : TEdges; begin result := _base.q(_key, '~', '~'); end;

function TNode.qe(s : string) : TEdges;
  begin
    result := _base.q(_key, s, '~');
  end;

function TNode.q1(s : string) : ICell;
  var edges : TEdges;
  begin
    edges := self.qe(s);
    if length(edges) > 0 then result := edges[length(edges)-1].obj
    else result := TCell.New('');
    //raise Exception.Create('No match for ('+_key+')['+s+']');
  end;

{-- TNodakRepo --}

constructor TNodakRepo.Open(ndkPath : string);
  var isnew : boolean;
  begin
    inherited Create(Nil);
    isnew := not fs.exists(ndkPath);
    _dbc := udb.connect(ndkPath); //  todo: error handling
    if isnew then begin
      {$i min-sql2pas.inc}
    end;
  end;

function TNodakRepo.e(sub, rel, obj : string; seq : integer=0) : IEdge;
  var rs : udb.TRecordset;
  begin
    _dbc.RunSQL('insert into trip (sub, rel, obj, seq) '
		+'values (:sub, :rel, :obj, :seq)', [sub, rel, obj, seq]);
    rs := _dbc.Query('select v from meta where k=:k',['last_insert_rowid']);
    result := TEdge.New(self, rs['v'], seq, sub, rel, obj);
    rs.Free;
  end;

function TNodakRepo.f(eid : integer) : IEdge;
  var rs : udb.TRecordset;
  begin
    rs := _dbc.Query('select eid,sub,rel,obj,seq from trip where eid=:eid',[eid]);
    if rs.recordcount = 0 then
      raise Exception.Create('no edge with eid=' + IntToStr(eid));
    result := TEdge.New(self, rs['eid'], rs['seq'],
			rs['sub'], rs['rel'], rs['obj']);
    rs.Free;
  end;

function sqlEsc(s : string) : string;
  begin
    result:= ''''+sysutils.StringReplace(s,'''','''''',[rfReplaceAll])+'''';
  end;

function TNodakRepo.q(sub,rel,obj : string) : TEdges;
  var sql : string = ''; rs : udb.TRecordSet; i : cardinal = 0;
  begin
    sql := 'select eid, sub, rel, obj, seq from trip';
    if (sub <> '~') or (rel <> '~') or (obj <> '~') then sql += ' where (1=1)';
    if sub <> '~' then sql += ' and sub=' + sqlEsc(sub);
    if rel <> '~' then sql += ' and rel=' + sqlEsc(rel);
    if obj <> '~' then sql += ' and obj=' + sqlEsc(obj);
    sql += ' order by seq, eid';
    rs := _dbc.Query(sql,[]);
    SetLength(result, rs.RecordCount);
    while not rs.EOF do
      begin
	result[i] := TEdge.New(self, rs['eid'], rs['seq'],
			       rs['sub'], rs['rel'], rs['obj']);
        rs.Next; i += 1;
      end;
    rs.Free;
  end;
  
function TNodakRepo.w(sub,rel,obj : string) : TEdges;
  var i : cardinal = 0;
  begin
    result := self.q(sub,rel,obj);
    writeln;
    writeln(format('-- ("%s","%s","%s")? --', [sub,rel,obj]));
    while i < length(result) do
      begin
        writeln(format('  "%s","%s","%s"',
                       [result[i].sub.s,result[i].rel.s,result[i].obj.s]));
        i += 1
      end;
    writeln('-- end of results.');
  end;

function TNodakRepo.n(key : string) : INode;
  begin
    result := TNode.New(self, key)
  end;

function TNodakRepo.a(key, val : string) : IEdge;
  begin
    result := self.e(key, ':=', val)
  end;

function TNodakRepo.v(key : string) : ICell;
  begin
    result := self[key].val
  end;

{-- unit interface --}

function Open(path : string) : dndk.IBase;
  begin
    result := TNodakRepo.Open(path);
  end;

begin
end.
