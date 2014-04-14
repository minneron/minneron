{ minneron
----------------------------------------------------------------
Copyright (c) 2014 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphiunicode}{$i xpc.inc}
program min;
uses xpc, cx, mnml, mned, cw, fx, kvm, sysutils, kbd, dndk, ustr,
  impworld, cli, ub4vm, udb, udc, udv, ukm, utv, uapp, undk, fs,
  strutils, ui, uimpforth, uimpshell, uimpwords, uimpndk, rings;


type
  TFocusRing = rings.GRing<TView>;
  TFocusCursor = rings.IRingCursor<TView>;
  TMinApp = class (uapp.TCustomApp)
    protected
      ed : mned.TEditor;
      tg : udv.TDbTreeGrid;
      b4 : TB4VM;
      cur : TDbCursor;
      dbc : udb.TDatabase;
      ndk : dndk.IBase;
      imp : TImpForth;
      ish : TImpShell;
      itv : utv.TTermView; // itrm.view
      typeMenu : udv.TDBMenu;
      pageMenu : udv.TDBMenu;
      rsOutln  : udb.TRecordSet;
      focus, oldfocus : utv.TView;
      km_ed, km_tg, km_sh : ukm.TKeyMap;
      _focusables : TFocusRing;
      _focus : TFocusCursor;
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
      procedure ShellOn;  procedure ShellOff;
    end;

procedure TMinApp.Init; { 1/3 }
  var vsplit : integer = 0;
  begin
    // TODO: eventually i'll build a little lazarus-like RAD thing
    // to manage these components as data rather than code.

    { sqlite / nodak database connection }
    ndk := undk.open('minneron.sdb');
    dbc := (ndk as TNodakRepo).dbc;

    { split between outline and editor }
    vsplit := kvm.ymax - 1;

    { the editor widget }
    ed := TEditor.Create(self);
    ed.x := 20; ed.y := 2; ed.w := kvm.width - 21;
    ed.h := vsplit-ed.y-1;  //  why is this different than tg?

    { the outliner widget }
    rsOutln := dbc.query(
	 'SELECT olid,nid,kind,node,depth,collapsed,hidden,leaf '+
	 'FROM outline');
    cur := TDbCursor.Create(dbc);
    cur.Attach(rsOutln, 'nid');
    cur.canHideRows := true; cur.hideFlag := 'hidden';
    cur.OnMarkChanged := self.OnCursorChange;
    tg := TDbTreeGrid.Create(dbc);
    with tg do begin
      x := 1; y := 2; h := vsplit-y; w := 18; datacursor := cur
    end;

    { the type menu that pops up on ^T from the outliner }
    typeMenu := TDBMenu.Create(self);
    typeMenu.rs := dbc.query('SELECT * FROM kinds ORDER BY kind');
    typeMenu.key := 'knd';
    typeMenu.OnSave := self.OnChooseType;
{ procedure TMinApp.Init  2/3 }

    { a menu for selecting pages (on ^G }
    pageMenu := TDBMenu.Create(self);
    pageMenu.rs := dbc.query(
       'SELECT nid, val AS page FROM node NATURAL JOIN kinds '+
       'WHERE kind=:k', ['Page']);
    pageMenu.key := 'nid';

    { a virtual machine  - not actually used yet! }
    b4 := TB4VM.Create(self);

    { impshell (the stack based ui widget) }
    imp := TImpForth.Create(self);
    imp.addOp('bye', self.quit);
    imp.mount('term', TTermWords);
    imp.mount('forth', TForthWords);
    imp.mount('ndk', TNdkWords);
    TNdkWords(imp.modules['ndk']).ndk := ndk;
    ish := TImpShell.Create(self, imp);
    ish.resize(16,8); ish.center(kvm.width div 2, kvm.height div 2);
    imp.OnChange := ish.smudge; // so it updates the stack view

    { impterm (for showing results/text/etc) }
    itv := utv.TTermView.Create(self);
    itv.resize(xpc.min(kvm.width, 64), xpc.min(kvm.height, 16));
    itv.x := 0; itv.y := kvm.height - itv.h;
    TTermWords(imp.modules['term']).term := itv;

    { set up component rendering  }
    ish.visible := false; itv.visible := false;
    _views.extend([ ed, tg, ish, itv ]);

    _focusables := TFocusRing.Create;
    _focusables.extend([ ed, tg ]);
    _focus := _focusables.MakeCursor;
    _focus.ToTop;

{ procedure TMinApp.Init  3/3 }

    { focus ring }

    { handle command line arguments }
    if ParamCount = 1 then
      if not ed.Load( ParamStr( 1 )) then
	fail( u2a('unable to load file: ' + paramstr( 1 )))
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
      cmd[ ^U ] := self.ShellOn;
    end;
    km_sh := ukm.TKeyMap.Create(self);
    ish.keys( km_sh );
    with km_sh do begin
      cmd[ ^U ] := ShellOff;
    end;
    //  clean up keyboard focus handling!!
    km_ed := km; ed.keys( km_ed );
    with km_ed do begin
      cmd[ ^C ] := self.Quit;
      cmd[ ^L ] := self.Draw;
      cmd[ ^O ] := self.OtherWindow;
      cmd[ ^G ] := self.ChoosePage;
      cmd[ ^S ] := self.SavePage;
      cmd[ ^U ] := self.ShellOn;
    end;
    self.focus := ed;
  end;

procedure TMinApp.step;
  begin
    mnml.step;
    if ed.done and mnml.done then quit;
  end;

procedure TMinApp.draw;
  var child : TView;
  begin
    fx.fillscreen($e819, '░'); //#$2591); //'░'
    //bg($e8); fg($13); fx.fillscreen('#!@#$%^&*(){}][/=+?-_;:');
    fx.txtline(0, 0, kvm.xMax, 0, $43);
    for child in _views do child.smudge;
  end;

procedure TMinApp.OtherWindow;
  begin
//    _focus.value.GetBlur;
    _focus.MoveNext; if _focus.AtClasp then _focus.MoveNext;
//    _focus.value.GetFocus;

    // so horrible! get a real 'focus' concept!
    if _focus.value.equals(ed)
      then begin keymap := km_ed; kvm.ShowCursor end
      else begin keymap := km_tg; kvm.HideCursor end
  end;

// TODO : OnCursorChange should be part of the gridview itself.
procedure TMinApp.OnCursorChange( Sender : TObject );
  begin tg.smudge;
  end;

procedure TMinApp.ChoosePage;
  begin
    if pageMenu.Choose <> null then self.LoadPage(pageMenu.rs['page']);
    self.smudge;
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
    ed.smudge;
  end;

procedure TMinApp.ChooseType;
  begin typeMenu.Choose; self.smudge;
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
    rsOutln.Open; tg.smudge; // refresh the data and display
  end;

{ show and hide the impforth shell with ^U }
//  calling otherwindow in these two is just a hack so that ShellOff
  // restores the keyboard handler.
procedure TMinApp.ShellOn;
  begin otherwindow; oldfocus := focus; itv.show; ish.show; keymap := km_sh;
  end;
procedure TMinApp.ShellOff;
  begin focus := oldfocus; otherwindow; itv.hide; ish.hide; self.smudge;
  end;


begin
  impworld.world.manageKeyboard := false;
  uapp.run(TMinApp);
end.
