{$i xpc.inc}{$mode delphi}
unit uminneron;
interface
uses classes, udboutln, kvm, xpc, cli, num, sqldb, custapp, cw, math;

type
  TView = class(TComponent)
    protected
      _x, _y, _w, _h : cardinal;
      _bg, _fg : byte;
      procedure SetX(value : cardinal);
      procedure SetY(value : cardinal);
      procedure SetW(value : cardinal);
      procedure SetH(value : cardinal);
    published
      procedure Nudge(dx, dy : integer);
      procedure Redraw;
      procedure Render(term :  ITerm); virtual;
      procedure Resize(new_w, new_h : cardinal); virtual;
      property x : cardinal read _x write SetX;
      property y : cardinal read _y write SetY;
      property w : cardinal read _w write SetW;
      property h : cardinal read _h write SetH;
      constructor Create( aOwner : TComponent ); override;
    end;
  
  // A class with its own video ram buffer:
  TTermView = class (TView, ITerm)
    protected
      _gridterm : TGridTerm;
    published
      property term : TGridTerm read _gridterm implements ITerm;
      constructor Create( aOwner : TComponent ); override;
      procedure Render(term :  ITerm); override;
      procedure Resize(new_w, new_h : cardinal); override;
    end;

  TDbCursor = class (TComponent) // TODO : ICursor
    protected
      _rs : TRecordSet;
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
      property RecordSet : TRecordSet read _rs write _rs;
      property KeyField : string  read _kf write _kf;
      property canHideRows : boolean  read _hr write _hr;
      property hideFlag : string  read _hf write _hf;
      property Mark : integer read _mk write SetMark;
      property Item[ key : string ] : variant
         read GetItem write SetItem; default;
    end;

  TDbTreeGrid = class (TView)
    protected
      _top : cardinal;
      _cur : TDbCursor;
    published
      procedure Render(term :  ITerm); override;
      property DataCursor : TDbCursor read _cur write _cur;
   end;

  TStepper = class (TComponent)
    protected
      fstep : TNotifyEvent;
      procedure DoStep; virtual;
    public
      procedure Step(times:cardinal=1);
    published
      property OnStep : TNotifyEvent read fStep write fStep;
    end;

  TData = class (TComponent)
    protected
      _kind : integer;
    published
      property kind : integer read _kind write _kind;
    end;

implementation

constructor TView.Create( aOwner : TComponent );
  begin
    inherited Create( aOwner );
    _x := 0; _y := 0; _w := 30; _h := 10;
    _bg := $FC; _fg := $0;
  end;

procedure TView.SetX(value : cardinal);
  begin
    _x := value;
  end;

procedure TView.SetY(value : cardinal);
  begin
    _y := value;
  end;

procedure TView.SetW(value : cardinal);
  begin
    _w := value;
  end;

procedure TView.SetH(value : cardinal);
  begin
    _h := value;
  end;
  
procedure TView.Nudge(dx, dy : integer);
  begin
    _x += dx;
    _y += dy;
  end;


procedure TView.Redraw;
  var term : kvm.ITerm;
  begin
    term := kvm.work;
    kvm.work := kvm.TSubTerm.Create(term, _x, _y, _w, _h);
    bg(_bg); fg(_fg);
    try
      self.Render(term);
    finally
      kvm.work := term;
    end
  end;

procedure TView.Render(term : ITerm);
  begin
    ClrScr;
  end;

procedure TView.Resize(new_w, new_h : cardinal);
  begin
    _w := new_w;
    _h := new_h;
  end;

constructor TTermView.Create( aOwner : TComponent );
  begin
    inherited Create( aOwner );
    _gridterm := TGridTerm.Create(0, 0);
  end;

procedure TTermView.Resize(new_w, new_h : cardinal);
  begin
    inherited Resize(new_w, new_h);
    _gridterm.grid.Resize(new_w, new_h);
  end;

procedure TTermView.Render(term : ITerm);
  var endy, endx, x, y : byte; old : TTextAttr; cell : TTermCell;
  begin
    endy := min(_h-1, term.maxY);
    endx := min(_w-1, term.maxX);
    for y := 0 to endy do
      begin
	term.gotoxy(_x, _y+y);
        for x := 0 to endx do
          begin
            cell := _gridterm.grid[x,y];
	    term.emit(cell.ch);
          end;
      end;
  end;

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

{---------------------------------------------------------------}
{ TDbTreeGrid                                                   }
{---------------------------------------------------------------}
procedure TDbTreeGrid.Render(term : ITerm);
  var
    depth : integer = -1;
    sigil : char = ' ';
    i     : integer;
    count : cardinal =  0;
    rs    : TRecordSet;
begin
  bg('b'); fg('W');

  rs := _cur.RecordSet;
  rs.open;
  rs.first;
  while not rs.eof do
    begin
      if rs['depth'] > depth then pass
      else if rs['depth'] < depth then pass
      else pass;
      depth := rs['depth'];

      if rs['collapsed']=1 then sigil := '+'
      else if rs['leaf']=1 then sigil := ' '
      else sigil := '-';

      if rs['hidden'] = 0 then
        begin
          if _cur.AtMark then bg('b') else bg('k');
          gotoxy(0,count);
          { draw the outline controls }
          if rs['depth'] > 0 then
            for i := 1 to rs['depth'] do write('  ');
          write(sigil +  ' ');
          { draw the node itself }
          fg('c'); write(rs['kind'],' ');
          fg('W');
          write(rs['node']);

          { fill in the rest of the line }
          for i := 3 to kvm.work.maxx - (
            length(rs['node']) + length(rs['kind'])
            + rs['depth'] * 2) do write(' ');

          inc(count);
        end;
      rs.Next;
    end;
  bg('k');
  for i := count to kvm.maxy do
    begin
      gotoxy(0,i);
      clreol;
    end;
end;


procedure TStepper.DoStep;
  begin
  end;

procedure TStepper.Step(times:cardinal=1);
  var i : integer;
  begin
    if times > 0 then for i := 0 to times do
      begin
        DoStep;
        if Assigned(fStep) then fStep(self);
      end;
  end;

initialization
  RegisterClass(TView);
  RegisterClass(TTermView);
  RegisterClass(TDbTreeGrid);
  RegisterClass(TStepper);
  RegisterClass(TData);
end.
