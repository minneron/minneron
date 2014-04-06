{ minneron
----------------------------------------------------------------
Copyright (c) 2014 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphiunicode}{$i xpc.inc}
program min;
uses xpc, cx, mnml, mned, cw, fx, kvm, sysutils, kbd, dndk, ustr,
  impworld, cli, ub4vm, udb, udc, udv, ukm, utv, uapp, undk, fs,
  strutils, ui;

type
  TMinApp = class (uapp.TCustomApp)
    protected
      ed : mned.TEditor;
      mb : ui.Zinput; // minibuffer (for entering commands)
      tg : udv.TDbTreeGrid;
      b4 : TB4VM;
      cur : TDbCursor;
      dbc : udb.TDatabase;
      ndk : dndk.IBase;
      typeMenu : udv.TDBMenu;
      pageMenu : udv.TDBMenu;
      rsOutln  : udb.TRecordSet;
      focus : utv.TView;
      km_ed, km_tg, km_mb : ukm.TKeyMap;
    public
      procedure Init; override;
      procedure Step; override;
      procedure Draw; override;
      procedure Keys(km : ukm.TKeyMap); override;
      procedure ChooseType;
      procedure OtherWindow;
      procedure ChoosePage;
      procedure SavePage;
      procedure OnCursorChange(Sender : TObject);
      procedure OnChooseType(val:variant);
      procedure LoadPage(pg:TStr);
      procedure OnToggle;
      procedure REPL;
    end;

procedure TMinApp.Init;
  var vsplit : integer = 0;
  begin
    ndk := undk.open('minneron.sdb');
    dbc := (ndk as TNodakRepo).dbc;
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
    vsplit := kvm.ymax - 1;
    tg := TDbTreeGrid.Create(dbc);
    with tg do begin x := 1; y := 2; h := vsplit-y; w := 18;datacursor := cur end;
    ed := TEditor.Create(self);
    ed.x := 20; ed.y := 2; ed.w := kvm.width - 21;
    ed.h := vsplit-ed.y-1;  //  why is this different than tg?
    b4 := TB4VM.Create(self);
    mb := ZInput.default(1, kvm.ymax - 1, kvm.xmax-2, kvm.xmax-2 );
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
    with km_tg do begin
      cmd[ ^P ] := cur.Prev;
      cmd[ ^N ] := cur.Next;
      cmd['p'] := cur.Prev;
      cmd['n'] := cur.Next;
      cmd['['] := cur.ToTop;
      cmd[']'] := cur.ToEnd;
      cmd[ ^C ] := self.Quit;
      cmd[ ^I ] := self.OnToggle;
      cmd[ ^T ] := self.ChooseType;
      cmd[ ^O ] := self.OtherWindow;
      cmd[ ^G ] := self.ChoosePage;
      cmd[ ^L ] := self.Draw;
      cmd[ ^X ] := self.REPL;
    end;
    km_mb := ukm.TKeyMap.Create(self);
    with km_mb do begin
      cmd[ ^C ] := self.Quit;
      cmd[ ^O ] := self.OtherWindow;
    end;
    //  clean up keyboard focus handling!!
    km_ed := km; ed.keys( km_ed );
    with km_ed do begin
      cmd[ ^C ] := self.Quit;
      cmd[ ^L ] := self.Draw;
      cmd[ ^O ] := self.OtherWindow;
      cmd[ ^G ] := self.ChoosePage;
      cmd[ ^S ] := self.SavePage;
      cmd[ ^X ] := self.REPL;
    end;
    self.focus := ed;
  end;

procedure TMinApp.step;
  begin
    ed.update;
    mnml.step;
    if ed.done and mnml.done then quit;
  end;

procedure TMinApp.draw;
  begin
    fx.fillscreen($e819, '░'); //#$2591); //'░'
    //bg($e8); fg($13); fx.fillscreen('#!@#$%^&*(){}][/=+?-_;:');
    fx.txtline(0, 0, kvm.xMax, 0, $43);
    ed.dirty := true; tg.Redraw;
    mb.show;
  end;

procedure TMinApp.OtherWindow;
  begin
    // focus.defocus;
    if focus = ed then
      begin
	focus := tg; keymap := km_tg; kvm.HideCursor;
      end
    else if focus = tg then
      begin
	focus := mb; keymap := km_mb; kvm.ShowCursor;
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

procedure TMinApp.ChoosePage;
  begin
    if pageMenu.Choose <> null then self.LoadPage(pageMenu.rs['page']);
    self.draw;
  end;

procedure TMinApp.SavePage;
  var p : string;
  begin
    p := ed.path;  // encapsulate this!
    if ustr.startswith(p, 'ndk://') then
      begin
	p := midstr(p, 7, length(p)-6);
	cwriteln('|g(|w' + p + '|g)'); hitakey;
	ndk.a(p, ed.buffer.text);
      end
    else ed.save;
  end;

procedure TMinApp.LoadPage(pg:TStr);
  begin
    // TODO: encapsulate all this
    ed.path := 'ndk://' + pg;
    ed.buffer.loadfromstring(ndk.v(pg).s);
    ed.led.work := ed.buffer[ 0 ];
    ed.dirty := true;
  end;

procedure TMinApp.ChooseType;
  begin
    typeMenu.Choose;
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

procedure TMinApp.REPL;
  begin

  end;


begin
  impworld.world.manageKeyboard := false;
  uapp.run(TMinApp);
end.
