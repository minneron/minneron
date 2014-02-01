{$mode delphi}
program dboutln;
uses xpc, udb, udc, uapp, classes, kvm, cw, uminneron,
     ustr, db, sqldb, fx, ukm, kbd, num;

type
  TDbOutlnApp  = class(uapp.TCustomApp)
    protected
      dbc  : udb.TDatabase;
      curs : udc.TDbCursor;
      view : TDbTreeGrid;
      rsOutln,
      rsKinds  : TRecordSet;
    public
      procedure init; override;
      procedure keys(km : TKeyMap); override;
      procedure ChooseType;
      procedure Redraw;
      procedure OnCursorChange( Sender : TObject );
    end;


procedure TDbOutlnApp.init;
  begin
    dbc := udb.connect('minneron.sdb');
    rsOutln := dbc.query(
      'SELECT olid, nid, kind, node, depth, collapsed, hidden, leaf '+
        'FROM outline');
    rsKinds := dbc.query('SELECT * FROM kinds ORDER BY kind');

    curs := TDbCursor.Create(dbc).Attach(rsOutln, 'nid');
    curs.canHideRows := true; curs.hideFlag := 'hidden';
    curs.OnMarkChanged := self.OnCursorChange;

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
  end;
procedure TDBOutLnApp.ChooseType;
  var
    {@loop} done:boolean=false; cancel:boolean = false; ch : char;
      {@loop} _cr : TDbCursor;
      {@draw} _rs : TRecordSet;
      {@draw} _ws : array of byte;
      {@draw} i : integer; f : TField;
    {@save} q : TSqlQuery;
  begin
    {+init} _rs := rsKinds.Open.First;
            _cr := TDbCursor.Create(self).Attach(_rs, 'knd');
    {-init} _ws := bytes([0, 16]);
    {+loop}
    repeat
      {+draw} cwrite('|@0000|!K|W|$'); i := 0;
      {=head} for f in _rs.fields do
	        write(rfit(f.DisplayName, _ws[vinc(i)]));
      {+body}   _rs.First; while not _rs.EOF do begin
      {+line}   i:=0; cwriteln('|k');
                if _cr.AtMark then bg('B') else bg('w');
      {=each}   for f in _rs.fields do
                  write(rfit(f.DisplayText, _ws[vinc(i)]));
      {-line}   _rs.Next;
      {-body} end;
      {-draw} _cr.ToMark;
      repeat until keypressed;
      case readkey(ch) of
	'n', ^N : _cr.Next;    ^M : done := true;
	'p', ^P : _cr.Prev;    ^C: cancel := true;
        else cwritexy(15, 0, '|Gch: |g' +ch)
      end
    until cancel or done;
    {-loop}
    {=free} _cr.RecordSet:=nil; _cr.Free; _rs:=nil;
    {+save}
    if not cancel then begin
      q := TSQLQuery.Create(nil);
      q.Database := dbc; q.Transaction := dbc.Transaction;
      q.sql.text := 'UPDATE node SET knd=:knd WHERE nid=:nid';
      q.ParamByName('knd').AsInteger := rsKinds['knd'];
      q.ParamByName('nid').AsInteger := curs['nid'];
      q.ExecSQL; dbc.Transaction.Commit;
    end;
    {-save}
    Redraw;
  end;

begin
  uapp.run(TDbOutlnApp);
end.
