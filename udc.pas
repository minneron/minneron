{$i xpc.inc}{$mode delphi}
unit udc;
interface uses xpc, classes, sqldb, udb, num;
type
  TDbCursor = class (TComponent) // TODO : ICursor
    protected
      _rs : udb.TRecordSet;
      _kf : string;
      _mk : integer; // can't really use bookmarks because they disappear on refresh :/
      _hr : boolean; // allow hiding rows?
      _hf : string; // if so, use this field as a flag
      fMarkChanged : TNotifyEvent;
    public
      procedure ToTop;
      procedure ToEnd;
      function AtTop : boolean;
      function AtEnd : boolean;
      procedure Next;
      procedure Prev;
      function  AtMark : boolean;
      function  RowIsVisible : boolean;
      procedure ToMark;
      procedure Toggle;
      procedure SetMark(id : integer);
      procedure SetItem(key : string; value: variant);
      function  GetItem(key : string): variant;
   published
      property OnMarkChanged : TNotifyEvent read fMarkChanged write fMarkChanged;
      property RecordSet : udb.TRecordSet read _rs write _rs;
      property KeyField : string  read _kf write _kf;
      property canHideRows : boolean  read _hr write _hr;
      property hideFlag : string  read _hf write _hf;
      property Mark : integer read _mk write SetMark;
      property Item[ key : string ] : variant
         read GetItem write SetItem; default;
    end;

implementation
{---------------------------------------------------------------}
{ TDbCursor                                                     }
{---------------------------------------------------------------}
procedure TDbCursor.SetMark(id : integer );
  begin
    _mk := id;
    if assigned(fMarkChanged) then fMarkChanged(self);
  end;

procedure TDbCursor.ToMark;
  begin
    _rs.locate(keyField, _mk, [])
  end;

procedure TDbCursor.ToTop;
  begin
    ToMark; _rs.First; SetMark(_rs[keyField]);
  end;

procedure TDbCursor.ToEnd;
  begin
    ToMark; _rs.Last; SetMark(_rs[keyField]);
  end;

function TDbCursor.AtTop : boolean;
  begin
    ToMark; result := _rs.BOF;
  end;

function TDbCursor.AtEnd : boolean;
  begin
    ToMark; result := _rs.EOF;
  end;

function TDBCursor.RowIsVisible : boolean;
  begin
    result := (not CanHideRows) or (_rs[hideFlag]=0)
  end;

procedure TDbCursor.Next;
  begin
    ToMark;
    repeat _rs.Next until _rs.EOF or RowIsVisible;
    if RowIsVisible then SetMark(_rs[keyField]);
  end;

procedure TDbCursor.Prev;
  begin
    ToMark;
    repeat _rs.Prior until _rs.BOF or RowIsVisible;
    if RowIsVisible then SetMark(_rs[keyField]);
  end;

function TDbCursor.AtMark : boolean;
  begin
    result := _rs[keyField] = _mk;
  end;

procedure TDbCursor.Toggle;
  var olid, nid : integer; sql : string;
  begin
    ToMark;
    olid := _rs['olid'];
    nid  := _rs['nid'];
    sql := _rs.sql.text;
    if _rs['collapsed'] then
      _rs.sql.text := 'delete from outline_collapse where olid=' + n2s(olid)
        +  ' and collapse=' + n2s(nid)
    else
      _rs.sql.text := 'insert into outline_collapse values (' + n2s(olid)
        +   ' , ' + n2s(nid) + ')';
    _rs.ExecSQL;
    // ! not sure why i have to cast this:
    TSQLTransaction(_rs.Transaction).Commit;
    _rs.Execute(sql);
    SetMark(_mk); // MarkChanged;
  end;

procedure TDbCursor.SetItem(key : string; value: variant);
  begin
    ToMark; _rs.Edit; _rs[key] := value; _rs.Post;
  end;

function TDbCursor.GetItem(key : string): variant;
  begin
    ToMark; result := _rs[key];
  end;

begin
  RegisterClass(TDbCursor);
end.
