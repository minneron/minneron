{$i xpc.inc}{$mode delphi}
unit uminneron;
interface
uses classes, kvm, udboutln, xpc, kbd, cli, num, sqldb;

type
  TView = class(TComponent)
    protected
      _x, _y, _w, _h : cardinal;
      _bg, _fg : byte;
      procedure SetX(value : cardinal);
      procedure SetY(value : cardinal);
      procedure SetW(value : cardinal);
      procedure SetH(value : cardinal);
      procedure Render(term :  ITerm); virtual;
    public
      procedure Redraw;
    published
      property x : cardinal read _x write SetX;
      property y : cardinal read _y write SetY;
      property w : cardinal read _w write SetW;
      property h : cardinal read _h write SetH;
      constructor Create( aOwner : TComponent ); override;
    end;

  // TODO : probably set up state transitions, contexts...?
  TCommandEvent = procedure of object;
  TKeyCommander = class (TComponent)
    protected
      _keymap : array[ char ] of TCommandEvent;
      procedure SetKeyCmd( ch : char; cmd : TCommandEvent );
      procedure DoNothing;
    public
      constructor Create( aOwner : TComponent ); override;
      procedure HandleKeys;
    published
      property KeyMap[ ch : char ] : TCommandEvent write SetKeyCmd;
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
      procedure Next;
      procedure Prev;
      function  AtMark : boolean;
      function  RowIsVisible : boolean;
      procedure ToMark;
      procedure Toggle;
      procedure SetMark(id : integer);
   published
      property OnMarkChanged : TNotifyEvent read fMarkChanged write fMarkChanged;
      property RecordSet : TRecordSet read _rs write _rs;
      property KeyField : string  read _kf write _kf;
      property canHideRows : boolean  read _hr write _hr;
      property hideFlag : string  read _hf write _hf;
      property Mark : integer read _mk write SetMark;
    end;

  TDbTreeGrid = class (TView)
    protected
      _top : cardinal;
      _cur : TDbCursor;
    published
      procedure Render(term :  ITerm); override;
      property DataCursor : TDbCursor read _cur write _cur;
   end;

  TTermView = class (TView)
    protected
      // TODO :expose something like crt for use by vm, etc
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
function TDBCursor.RowIsVisible : boolean;
  begin
    result := CanHideRows and (_rs[hideFlag]=0)
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
  clrscr;
  bg('b'); fg('W');clrscr;

  rs := _cur.RecordSet;
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
	  if _cur.AtMark then bg('g') else bg('b');
	  gotoxy(0,count);
	  if rs['depth'] > 0 then for i := 1 to rs['depth'] do write('  ');
	  write(sigil +  ' ' + rs['node']);
	  for i := 2 to kvm.work.maxx - (length(rs['node'])
	    + rs['depth'] * 2) do write(' ');

	  inc(count);
	end;
      rs.Next;
    end;
end;

{---------------------------------------------------------------}
{ TKeyCommander                                                 }
{---------------------------------------------------------------}
constructor TKeyCommander.Create( aOwner : TComponent );
  var ch : char;
  begin
    inherited Create( aOwner );
    for ch := #0 to #255 do _keymap[ch] := DoNothing;
  end;

procedure TKeyCommander.SetKeyCmd( ch : char; cmd : TCommandEvent );
  begin
    _keymap[ch] := cmd;
  end;

procedure TKeyCommander.DoNothing;
  begin
    pass
  end;

procedure TKeyCommander.HandleKeys;
  var ch : char;
  begin
    if kbd.KeyPressed then
      begin
        // TODO: separate map for extended keys.
        if kbd.ReadKey(ch) = #0 then kbd.ReadKey(ch);
        _keymap[ch]();
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
  RegisterClass(TKeyCommander);
  RegisterClass(TTermView);
  RegisterClass(TDbTreeGrid);
  RegisterClass(TStepper);
  RegisterClass(TData);
end.
