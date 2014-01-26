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
      function  init : boolean; override;
      procedure keys(km : TKeyMap); override;
      procedure ChooseType;
      procedure Redraw;
      procedure OnCursorChange( Sender : TObject );
    end;


function TDbOutlnApp.init : boolean;
  begin
    hidecursor;
    dbc := udb.connect('minneron.sdb');
    rsOutln := dbc.query(
      'SELECT olid, nid, kind, node, depth, collapsed, hidden, leaf '+
        'FROM outline');
    rsKinds := dbc.query('SELECT * FROM kinds ORDER BY kind');

    curs := TDbCursor.Create(dbc);
    curs.RecordSet := rsOutln;
    curs.KeyField := 'nid';
    curs.canHideRows := true;
    curs.hideFlag := 'hidden';
    curs.Mark := rsOutln['nid'];
    view := TDbTreeGrid.Create(dbc);
    view.x := 15;
    view.y := 5;
    view.datacursor := curs;
    curs.OnMarkChanged := self.OnCursorChange;
    self.redraw;
    result := true;
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

procedure TDBOutLnApp.ChooseType;
  var rs : TRecordSet; f : TField; i : integer; c : TDbCursor;
  var done:boolean; ch : char; q : TSqlQuery;
  const widths : array [0..1] of byte = (0, 16);
  begin
    clrscr;
    rs := rsKinds; rs.Open; rs.First;

    c := TDbCursor.Create(dbc);
    c.RecordSet := rs; c.KeyField := 'knd'; c.Mark := rs['knd'];

    repeat
      gotoxy(0,0);

      { draw column headers }
      bg('K'); fg('W');
      i := 0;
      for f in rs.fields do write(rfit(f.DisplayName, widths[vinc(i)]));
      WriteLn;

      { draw rows }
      fg('k');
      rs.First;
      while not rs.EOF do
        begin
          if c.AtMark then bg('B') else bg('w');
          { draw fields }
          i:=0;
          for f in rs.fields do
            write(rfit(f.DisplayText, widths[vinc(i)]));
          WriteLn;
          rs.Next;
        end;

      c.ToMark;
      repeat until keypressed;
      case readkey(ch) of
        ^N : c.Next;
        ^P : c.Prev;
        'n': c.next;
        'p': c.prev;
        ^M : begin
               done := true;
             end;
        ^C : done := true;
        else cwritexy(15, 0, '|Gch: |g' +ch);
      end;
    until done;
    c.RecordSet := nil;
    c.Free;

    q := TSQLQuery.Create(nil);
    q.Database := dbc;
    q.Transaction := dbc.Transaction;
    q.sql.text := 'UPDATE node SET knd=:knd WHERE nid=:nid';
    q.ParamByName('knd').AsInteger := rsKinds['knd'];
    q.ParamByName('nid').AsInteger := curs['nid'];
    q.ExecSQL;
    dbc.Transaction.Commit;

    Redraw;
  end;

begin
  uapp.run(TDbOutlnApp.Create);
end.
