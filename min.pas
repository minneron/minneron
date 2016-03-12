{ minneron
----------------------------------------------------------------
Copyright (c)x 2014 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphiunicode}{$i xpc.inc}
program min;
uses xpc, cx, mnml, mned, cw, fx, kvm, sysutils, kbd, dndk, ustr,
  impworld, cli, ub4vm, udb, udc, udv, ukm, utv, uapp, undk, fs,
  strutils, ui, uimpforth, uimpshell, uimpwords, uimpndk, rings,
  classes, umsg, ug2d, num;


type
  TEdgeMenu = class (utv.TGridView)
    protected _base : IBase; _node : INode;_edges : TEdges;
    public
      constructor Create( aOwner : TComponent ); override;
      function EdgeCount : word;
      procedure DeleteAt(gx, gy: word);
      procedure RenderCell(gx, gy: word); virtual; abstract;
      procedure LoadData; virtual; abstract;
    published
      property base : IBase read _base write _base;
      property node : INode read _node write _node;
    end;
  TEdgeMenuI = class (TEdgeMenu)
    public
      constructor Create( aOwner: TComponent ); override;
      procedure LoadData; override;
      procedure RenderCell(gx, gy: word); override;
    end;
  TEdgeMenuO = class (TEdgeMenu)
    public
      constructor Create( aOwner : TComponent ); override;
      procedure LoadData; override;
      procedure RenderCell(gx, gy: word); override;
    end;

constructor TEdgeMenu.Create( aOwner : TComponent );
  begin inherited;
    _gw := 2; _gh := 0; _deleteAt := self.DeleteAt;
    _RenderCell := RenderCell;
  end;

function TEdgeMenu.EdgeCount : word;
  begin if assigned(node) then result := length(_edges) else result := 0
  end;

procedure TEdgeMenu.DeleteAt(gx,gy : word );
  begin
    if assigned(_edges) then begin
      _edges[_igy].del; self.LoadData
    end else ok
  end;

constructor TEdgeMenuI.Create( aOwner : TComponent );
  begin inherited; _cellw := bytes([20,10]);
  end;

constructor TEdgeMenuO.Create( aOwner : TComponent );
  begin inherited;  _cellw := bytes([10,20]);
  end;

procedure TEdgeMenuI.LoadData;
  begin if assigned(_node) then _edges := _node.ie; _gh := EdgeCount; smudge;
  end;

procedure TEdgeMenuO.LoadData;
  begin if assigned(_node) then _edges := _node.oe; _gh := EdgeCount; smudge;
  end;

procedure TEdgeMenuI.RenderCell(gx,gy:word);
  begin emit(boolstr(gx=0, _edges[gy].sub.s, _edges[gy].rel.s))
  end;

procedure TEdgeMenuO.RenderCell(gx,gy:word);
  begin emit(boolstr(gx=0, _edges[gy].rel.s, _edges[gy].obj.s))
  end;

type
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
      _ies, _oes : TEdgeMenu;
      rsOutln  : udb.TRecordSet;
      km_ed, km_tg, km_sh : ukm.TKeyMap;
    public
      procedure Init; override;
      procedure Step; override;
      procedure Draw; override;
      procedure Keys(km : ukm.TKeyMap); override;
      procedure ChooseType;
      procedure OtherWindow;
      procedure ChoosePage;
      procedure SavePage;
      procedure OnNavMsg( m : TMsg );
      procedure OnCursorChange(Sender : TObject);
      procedure OnChooseType(val:variant);
      procedure LoadPage(pg:TStr);
      procedure OnToggle( msg : TMsg );
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
    ed.h := 22; //  of by 1, even accounting for status line :(

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
    ish.resize(16,8); ish.center((kvm.width div 2) - 16,
				 kvm.height div 2);
    imp.OnChange := ish.smudge; // so it updates the stack view

    { impterm (for showing results/text/etc) }
    itv := utv.TTermView.Create(self);
    itv.resize(xpc.min(kvm.width, 32), xpc.min(kvm.height, 9));
    itv.x := ish.x + ish.w; itv.y := ish.y;
    TTermWords(imp.modules['term']).term := itv;


{ procedure TMinApp.Init  3/3 }

    { incoming and outgoing links }
    _ies := TEdgeMenuI.Create(self);
    _ies.MoveTo(ed.x, ed.y + ed.h + 1);

    _oes := TEdgeMenuO.Create(self);
    _oes.MoveTo(ed.x + _ies.w + 2, ed.y + ed.h + 1);

    { set up component rendering  }
    ish.visible := false; itv.visible := false;
    _views.extend([ ed, tg, itv, _ies, _oes ]);
    _views.append(ish); // make sure shell is on top

    { focus ring }
    _focusables.extend([ ed, tg, _ies, _oes ]);
    _focus.ToTop; _focus.value.gainfocus;

    { message handler }
    umsg.subscribe( chan_nav, self.OnNavMsg );
    umsg.subscribe( chan_cmd, self.OnToggle );


    { handle command line arguments }
    if ParamCount = 1 then
      if not ed.Load( ParamStr( 1 )) then
	fail( u2a('unable to load file: ' + paramstr( 1 )))
      else ed.status := 'welcome to minneron.'
    else ok
  end;

procedure TMinApp.keys(km : ukm.TKeyMap);

  procedure globalkeys(km : ukm.TKeyMap);
    begin
      with km do begin
	msg[ ^P ] := msg_nav_up;       msg[ ^N ] := msg_nav_dn;
	msg[ ^I ] := msg_cmd_toggle;   msg[ ^D ] := msg_cmd_delete;

	cmd[ ^C ] := self.Quit;        cmd[ ^O ] := self.OtherWindow;
	cmd[ ^G ] := self.ChoosePage;  cmd[ ^L ] := self.Draw;
	cmd[ ^U ] := self.ShellOn;
      end;
    end;

  begin
    //  clean up keyboard focus handling!!

    km_tg := ukm.TKeyMap.Create(self);
    with km_tg do begin
      msg[ 'p' ] := msg_nav_up;       msg[ 'n' ] := msg_nav_dn;
      msg[ '[' ] := msg_nav_top;      msg[ ']' ] := msg_nav_end;
      cmd[ ^T ] := self.ChooseType;
    end;

    km_sh := ukm.TKeyMap.Create(self);
    ish.keys( km_sh );
    with km_sh do begin
      cmd[ ^U ] := ShellOff;
    end;

    km_ed := km; ed.keys( km_ed );
    with km_ed do begin
      cmd[ ^S ] := self.SavePage;
    end;

    globalkeys(km_tg);
    globalkeys(km_ed);
  end;

procedure TMinApp.step;
  begin mnml.step
  end;

procedure TMinApp.draw;
  var child : TView;
  begin
    fx.fillscreen($e819, '░'); //#$2591); //'░'
    //bg($e8); fg($13); fx.fillscreen('#!@#$%^&*(){}][/=+?-_;:');
    fx.txtline(0, 0, kvm.xMax, 0, $43);
    for child in _views do child.smudge;
  end;

procedure TMinApp.OnNavMsg( m : TMsg );
  begin _focus.value.handle(m)
  end;


procedure TMinApp.OtherWindow;
  begin
    _focus.value.LoseFocus;
    _focus.MoveNext; if _focus.AtClasp then _focus.MoveNext;
    _focus.value.GainFocus;

    // so horrible! get a real 'focus' concept!
    if _focus.value.equals(ed)
      then begin keymap := km_ed; kvm.ShowCursor end
    else begin keymap := km_tg; kvm.HideCursor end;

    _focus.value.restorecursor;
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
	ndk.a(p, ed.buffer.text);
      end
    else ed.save;
  end;

procedure TMinApp.LoadPage(pg:TStr);
  var node : INode;
  begin
    // TODO: encapsulate all this
    ed.path := 'ndk://' + pg;
    ed.loadfromstr(ndk.v(pg).s);
    node := ndk.n(pg);
    _oes.node := node; _ies.node := node;
  end;

procedure TMinApp.ChooseType;
  begin typeMenu.Choose; self.smudge;
  end;

procedure TMinApp.OnChooseType(val:variant);
  begin dbc.RunSQL(
    'UPDATE node SET knd=:knd WHERE nid=:nid', [val, cur['nid']]);
  end;

procedure TMinApp.OnToggle( msg : TMsg );
  var olid, nid : integer; sql : TStr;
  begin
    if _focus.value.equals(self.tg) then begin
      olid := cur['olid']; nid  := cur['nid'];
      if cur['collapsed'] then sql :=
	'delete from outline_collapse ' +
	'where olid= :olid and collapse = :nid'
      else sql := 'insert into outline_collapse values (:olid, :nid)';
      dbc.RunSQL(sql, [olid, nid]);
      rsOutln.Open; tg.smudge; // refresh the data and display
    end
    else _focus.value.handle(msg)
  end;


{ show and hide the impforth shell with ^U }

var _oldKeyMap : ukm.TKeyMap; _oldfocus : cardinal;
procedure TMinApp.ShellOn;
  begin
    _oldKeyMap := keymap; keymap := km_sh;
    _oldfocus := _focus.index; _focus.value.LoseFocus;
    _focusables.append(ish); _focus.ToEnd; ish.GainFocus;
    itv.show; ish.show;
  end;

procedure TMinApp.ShellOff;
  begin
    itv.hide; ish.hide; self.smudge;
    ish.LoseFocus; _focusables.drop; _focus.ToEnd;
    _focus.MoveTo(_oldfocus); _focus.value.GainFocus;
    keymap := _oldKeyMap;
  end;


begin
  impworld.world.manageKeyboard := false;
  uapp.run(TMinApp);
end.
