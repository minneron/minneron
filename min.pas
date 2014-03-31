{ minneron
----------------------------------------------------------------
Copyright (c) 2014 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphiunicode}{$i xpc.inc}
program min;
uses xpc, cx, mnml, mned, cw, fx, kvm, sysutils, kbd, dndk,
  impworld, cli, ub4vm, udb, udc, udv, ukm, utv, uapp, undk, fs;

type
  TMinApp  = class(uapp.TCustomApp)
    protected
      ed : mned.TEditor;
      tg : udv.TDbTreeGrid;
      b4 : TB4VM;
      cur : TDbCursor;
      dbc : udb.TDatabase;
      ndk : dndk.IBase;
      typeMenu : udv.TDBMenu;
      pageMenu : udv.TDBMenu;
      rsOutln  : udb.TRecordSet;
      focus : utv.TView;
      km_ed, km_tg : ukm.TKeyMap;
    public
      procedure Init; override;
      procedure Step; override;
      procedure Draw; override;
      procedure Keys(km : ukm.TKeyMap); override;
      procedure ChooseType;
      procedure OtherWindow;
      procedure ChoosePage;
      procedure OnCursorChange( Sender : TObject );
      procedure OnChooseType(val:variant);
      procedure OnToggle;
    end;

procedure TMinApp.Init;
  begin
    ndk := undk.open('minneron.sdb');
    dbc := udb.connect('minneron.sdb');
    rsOutln := dbc.query('SELECT olid,nid,kind,node,depth,collapsed,hidden,leaf '+
        'FROM outline');
    cur := TDbCursor.Create(dbc);
    cur.Attach(rsOutln, 'nid');
    cur.canHideRows := true; cur.hideFlag := 'hidden';
    cur.OnMarkChanged := self.OnCursorChange;

    // TODO: eventually i'll build a little lazarus-like RAD thing
    // to manage these components as data rather than code.
    typeMenu := TDBMenu.Create(self);
    typeMenu.rs := dbc.query('SELECT * FROM kinds ORDER BY kind');
    typeMenu.key := 'knd';
    typeMenu.OnSave := self.OnChooseType;
    pageMenu := TDBMenu.Create(self);
    pageMenu.rs := dbc.query(
      'select nid, val as page from node natural join kinds where kind=:k',
		     ['Page']);
    pageMenu.key := 'nid';

    tg := TDbTreeGrid.Create(dbc);
    with tg do begin x := 1; y := 2; h := 32; w := 18;datacursor := cur end;

    ed := TEditor.Create(self);
    ed.x := 20; ed.y := 2; ed.h := kvm.height-4; ed.w := kvm.width - 21;
    b4 := TB4VM.Create(self);

    if ParamCount = 1 then
      if not ed.Load( ParamStr( 1 )) then
	fail( utf8encode('unable to load file: ' +
			 utf8decode(ansistring(paramstr( 1 )))))
      else ed.status := 'welcome to minneron.'
    else ok
  end;

procedure TMinApp.keys(km : ukm.TKeyMap);
  begin
    km_tg := ukm.TKeyMap.Create(self);
    km_tg.cmd[ ^P ] := cur.Prev;
    km_tg.cmd[ ^N ] := cur.Next;
    km_tg.cmd['p'] := cur.Prev;
    km_tg.cmd['n'] := cur.Next;
    km_tg.cmd['['] := cur.ToTop;
    km_tg.cmd[']'] := cur.ToEnd;
    km_tg.cmd[ ^C ] := self.Quit;
    km_tg.cmd[ ^I ] := self.OnToggle;
    km_tg.cmd[ ^T ] := self.ChooseType;
    km_tg.cmd[ ^O ] := self.OtherWindow;
    km_tg.cmd[ ^G ] := self.ChoosePage;
    km_tg.cmd[ ^L ] := self.Draw;
    //  clean up keyboard focus handling!!
    km_ed := km;
    ed.AddDefaultKeys( km_ed );
    km_ed.cmd[ ^C ] := self.Quit;
    km_ed.cmd[ ^L ] := self.Draw;
    km_ed.cmd[ ^O ] := self.OtherWindow;
    km_ed.cmd[ ^G ] := self.ChoosePage;
    self.focus := ed;
  end;

procedure TMinApp.step;
  begin
    if ed.dirty then ed.Redraw;
    mnml.step;
    if ed.done and mnml.done then quit;
  end;

procedure TMinApp.draw;
  begin
    fx.fillscreen($e819, '░'); //#$2591); //'░'
    //bg($e8); fg($13); fx.fillscreen('#!@#$%^&*(){}][/=+?-_;:');
    fx.txtline(0, 0, kvm.xMax, 0, $43);
    ed.dirty := true; tg.Redraw;
  end;

procedure TMinApp.OtherWindow;
  begin
    // focus.defocus;
    if focus = ed then
      begin
	focus := tg; keymap := km_tg; kvm.HideCursor;
      end
    else
      begin
	focus := ed; keymap := km_ed; kvm.ShowCursor;
      end;
    // focus.focus;
  end;

// TODO : OnCursorChange should be part of the gridview itself.
procedure TMinApp.OnCursorChange( Sender : TObject );
  begin
    tg.Redraw;
  end;

procedure TMinApp.ChooseType;
  begin
    typeMenu.Choose;
    self.draw;
  end;

procedure TMinApp.ChoosePage;
  begin
    pageMenu.Choose;
    self.draw;
  end;

procedure TMinApp.OnChooseType(val:variant);
  begin dbc.RunSQL(
    'UPDATE node SET knd=:knd WHERE nid=:nid', [val, cur['nid']]);
  end;

procedure TMinApp.OnToggle;
  var olid, nid : integer; sql : TStr;
  begin
    olid := cur['olid']; nid  := cur['nid'];
    if cur['collapsed'] then sql :=
      'delete from outline_collapse where olid= :olid and collapse = :nid'
    else sql := 'insert into outline_collapse values (:olid, :nid)';
    dbc.RunSQL(sql, [olid, nid]);
    rsOutln.Open; tg.redraw; // refresh the data and display
  end;

begin
  impworld.world.manageKeyboard := false;
  uapp.run(TMinApp);
end.
