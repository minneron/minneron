{$mode delphi}{$i xpc.inc}
program dboutln;
uses xpc, cli, udb, udc, udv, uapp, classes, kvm, cw, utv,
     num, fx, ukm, kbd, undk, fs;

type
  TDbOutlnApp  = class (uapp.TCustomApp)
    protected
      cur      : TDbCursor;
      dbc      : udb.TDatabase;
      treegrid     : udv.TDbTreeGrid;
      typeMenu : udv.TDBMenu;
      pageMenu : udv.TDBMenu;
      rsOutln  : udb.TRecordSet;
    public
      procedure Init; override;
      procedure keys(km : TKeyMap); override;
      procedure ChooseType;
      procedure ChoosePage;
      procedure Redraw;
      procedure OnCursorChange( Sender : TObject );
      procedure OnChooseType(val:variant);
      procedure OnToggle;
    end;


procedure TDbOutlnApp.init;
  begin
    if not fs.exists('minneron.sdb') then undk.open('minneron.sdb');
    dbc := udb.connect('minneron.sdb');
    rsOutln := dbc.query(
      'SELECT olid, nid, kind, node, depth, collapsed, hidden, leaf '+
        'FROM outline');
    cur := TDbCursor.Create(dbc);
    cur.Attach(rsOutln, 'nid');
    cur.canHideRows := true; cur.hideFlag := 'hidden';
    cur.OnMarkChanged := self.OnCursorChange;

    // TODO: eventually i'll build a little lazarus-like RAD thing
    // to manage these components as data rather than code.
    //
    typeMenu := TDBMenu.Create(self);
    typeMenu.rs := dbc.query('SELECT * FROM kinds ORDER BY kind');
    typeMenu.key := 'knd';
    typeMenu.OnSave := self.OnChooseType;
    //
    pageMenu := TDBMenu.Create(self);
    pageMenu.rs := dbc.query(
      'select nid, val as page from node natural join kinds where kind=:k',
		     ['Page']);
    pageMenu.key := 'nid';
    // pageMenu.OnSave := self.OnChoosePage;

    treegrid := TDbTreeGrid.Create(dbc);
    with treegrid do
      begin x := 5; y := 2; h := 32; datacursor := cur
      end;
    hidecursor; self.redraw;
  end;

procedure TDbOutlnApp.keys(km : TKeyMap);
  begin
    km.cmd[ ^P ] := cur.Prev;
    km.cmd[ ^N ] := cur.Next;
    km.cmd['p'] := cur.Prev;
    km.cmd['n'] := cur.Next;
    km.cmd['['] := cur.ToTop;
    km.cmd[']'] := cur.ToEnd;
    km.cmd[ ^C ] := self.Quit;
    km.cmd[ ^I ] := self.OnToggle;
    km.cmd[ ^T ] := self.ChooseType;
    km.cmd[ ^O ] := self.ChoosePage;
    km.cmd[ ^L ] := self.Redraw;
  end;

procedure TDbOutlnApp.Redraw;
  begin
    bg('k'); fg('K'); fillscreen('!@#$%^&*(){}][/=+?-_;:');
    treegrid.Redraw;
  end;

procedure TDbOutlnApp.OnCursorChange( Sender : TObject );
  begin
    treegrid.Redraw;
  end;


procedure TDBOutLnApp.ChooseType;
  begin
    typeMenu.Choose;
    Redraw;
  end;

procedure TDBOutLnApp.ChoosePage;
  begin
    pageMenu.Choose;
    Redraw;
  end;

procedure TDbOutlnApp.OnChooseType(val:variant);
  begin dbc.RunSQL(
    'UPDATE node SET knd=:knd WHERE nid=:nid', [val, cur['nid']]);
  end;

procedure TDbOutLnApp.OnToggle;
  var olid, nid : integer; sql : TStr;
  begin
    olid := cur['olid']; nid  := cur['nid'];
    if cur['collapsed'] then sql :=
      'delete from outline_collapse where olid= :olid and collapse = :nid'
    else sql := 'insert into outline_collapse values (:olid, :nid)';
    dbc.RunSQL(sql, [olid, nid]);
    rsOutln.Open; treegrid.redraw; // refresh the data and display
  end;


begin
  uapp.run(TDbOutlnApp);
end.
