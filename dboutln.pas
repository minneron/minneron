{$mode delphi}{$i xpc.inc}
program dboutln;
uses xpc, cli, udb, udc, udv, uapp, classes, kvm, cw, utv,
     num, fx, ukm, kbd, undk, fs;

type
  TDbOutCursor = class (udc.TDbCursor)
    public
      procedure Toggle;
    end;
  TDbOutlnApp  = class (uapp.TCustomApp)
    protected
      curs     : TDbOutCursor;
      dbc      : udb.TDatabase;
      view     : udv.TDbTreeGrid;
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
    end;


procedure TDbOutlnApp.init;
  begin
    if not fs.exists('minneron.sdb') then undk.open('minneron.sdb');
    dbc := udb.connect('minneron.sdb');
    rsOutln := dbc.query(
      'SELECT olid, nid, kind, node, depth, collapsed, hidden, leaf '+
        'FROM outline');
    curs := TDbOutCursor.Create(dbc);
    curs.Attach(rsOutln, 'nid');
    curs.canHideRows := true; curs.hideFlag := 'hidden';
    curs.OnMarkChanged := self.OnCursorChange;

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
    km.cmd[ ^O ] := self.ChoosePage;
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
    'UPDATE node SET knd=:knd WHERE nid=:nid', [val, curs['nid']]);
  end;

procedure TDbOutCursor.Toggle;
  var olid, nid : integer; sql : TStr;
  begin
    ToMark;
    olid := _rs['olid'];
    nid  := _rs['nid'];
    if _rs['collapsed'] then
      sql := 'delete from outline_collapse where olid= :olid ' +
	     'and collapse = :nid '
    else sql := 'insert into outline_collapse values (:olid, :nid)';
    _rs.dbc.RunSQL(sql, [olid, nid]);
    SetMark(_mk);
  end;


begin
  uapp.run(TDbOutlnApp);
end.
