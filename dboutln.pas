{$mode delphi}{$i xpc.inc}
program dboutln;
uses xpc, cli, udb, udc, uapp, classes, kvm, cw, umin,
     ustr, db, sqldb, fx, ukm, kbd, num, undk, fs;

type
  TDBMenu = class (umin.TTermView)
    public
      rs : TRecordSet;
      key: string;
      OnSave : procedure (val:variant) of object;
      procedure Choose(nid:Integer);
    end;
  TDbOutlnApp  = class(uapp.TCustomApp)
    protected
      dbc  : udb.TDatabase;
      curs : udc.TDbCursor;
      view : TDbTreeGrid;
      rsOutln : TRecordSet;
      typeMenu : TDBMenu;
    public
      procedure init; override;
      procedure keys(km : TKeyMap); override;
      procedure ChooseType;
      procedure Redraw;
      procedure OnCursorChange( Sender : TObject );
      procedure OnTypeMenuSave(val:variant);
    end;


procedure TDbOutlnApp.init;
  begin
    if not fs.exists('minneron.sdb') then undk.open('minneron.sdb');
    dbc := udb.connect('minneron.sdb');
    rsOutln := dbc.query(
      'SELECT olid, nid, kind, node, depth, collapsed, hidden, leaf '+
        'FROM outline');
    curs := udc.TDbCursor.Create(dbc).Attach(rsOutln, 'nid');
    curs.canHideRows := true; curs.hideFlag := 'hidden';
    curs.OnMarkChanged := self.OnCursorChange;

    typeMenu := TDBMenu.Create(self);
    typeMenu.rs := dbc.query('SELECT * FROM kinds ORDER BY kind');
    typeMenu.key := 'knd';
    typeMenu.OnSave := self.OnTypeMenuSave;

    view := TDbTreeGrid.Create(dbc);
    with view do
      begin x := 5; y := 2; h := 32; datacursor := curs
      end;
    hidecursor; self.redraw;
  end;

procedure TDbOutlnApp.keys(km : TKeyMap);
  begin
    km.cmd[ ^P ] := curs.Prev;
    km.cmd[ ^N ] := curs.Next;
    km.cmd['p'] := curs.Prev;
    km.cmd['n'] := curs.Next;
    km.cmd['['] := curs.ToTop;
    km.cmd[']'] := curs.ToEnd;
    km.cmd[ ^C ] := self.Quit;
    km.cmd[ ^I ] := curs.Toggle;
    km.cmd[ ^T ] := self.ChooseType;
    km.cmd[ ^L ] := self.Redraw;
  end;

procedure TDbOutlnApp.Redraw;
  begin
    bg('k'); fg('K'); fillscreen('!@#$%^&*(){}][/=+?-_;:');
    view.Redraw;
  end;

procedure TDbOutlnApp.OnCursorChange( Sender : TObject );
  begin
    view.Redraw;
  end;

function vinc(var i:integer):integer;
  begin
    result := i; i := i+1;
  end;

function incv(var i:integer):integer;
  begin
    i := i+1; result := i;
  end;

type tbytes = array of byte;
function bytes(data : array of byte):tbytes;
  var i :integer;
  begin
    setlength(result, length(data));
    for i := 0 to high(data) do result[i]:=data[i];
  end; { bytes }

procedure TDBOutLnApp.ChooseType;
  begin
    typeMenu.Choose(curs['nid']);
    Redraw;
  end;

procedure TDbOutlnApp.OnTypeMenuSave(val:variant);
  begin dbc.RunSQL(
    'UPDATE node SET knd=:knd WHERE nid=:nid', [val, curs['nid']]);
  end;


procedure TDBMenu.Choose(nid : integer);
  var
    _rs : TRecordSet;    // the data to choose from
    _cr : TDbCursor;     //
    _ws : array of byte; // column widths
    done:boolean=false; cancel:boolean = false;
  procedure SetUp;
    begin
      _rs := rs.Open.First;
      _cr := TDbCursor.Create(self).Attach(_rs, key);
      _ws := bytes([0, 16]);
    end;
  procedure DrawMenu;
    var i : integer; f : TField;
    begin
      cwrite('|@0000|!K|W|$'); i := 0;
      for f in _rs.fields do write(rfit(f.DisplayName, _ws[vinc(i)]));
      _rs.First;
      while not _rs.EOF do
        begin
          i:=0; cwriteln('|k');
          if _cr.AtMark then bg('B') else bg('w');
          for f in _rs.fields do write(rfit(f.DisplayText, _ws[vinc(i)]));
          _rs.Next;
        end;
      _cr.ToMark;
    end; { DrawMenu }
  procedure interact;
    var ch : char;
    begin repeat until keypressed;
      case readkey(ch) of
	'n', ^N : _cr.Next;    ^M : done := true;
	'p', ^P : _cr.Prev;    ^C: cancel := true;
        else cwritexy(15, 0, '|Gch: |g' +ch)
      end
    end; { interact }
  procedure TearDown;
    begin _cr.RecordSet:=nil; _cr.Free;
    end;
  begin { choosetype }
    SetUp;
    repeat DrawMenu; Interact until cancel or done;
    if (not cancel) and assigned(OnSave) then OnSave(rs[key]);
    TearDown;
  end;

begin
  uapp.run(TDbOutlnApp);
end.
