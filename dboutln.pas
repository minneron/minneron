{$mode delphi}
program dboutln;
uses xpc, udboutln, custapp, classes, kvm, cw, uminneron,
     stri, db, sqldb, cli, kbd, num;

const
  kCMD_CREATE_NODE = 00;
  kCMD_CREATE_TYPE = 01;
  kCMD_INSERT_NODE = 02;
  kCMD_NUDGE_NEXT  = 03;
  kCMD_NUDGE_PREV  = 04;

type
  TDbOutlnApp  = class(TCustomApplication)
    protected
      cmdr : TKeyCommander;
      curs : TDbCursor;
      view : TDbTreeGrid;
      rsOutln,
      rsKinds  : TRecordSet;
    public
      procedure Initialize; override;
      procedure DoRun; override;
      procedure Quit;
      procedure ChooseType;
      procedure Redraw;
      procedure OnCursorChange( Sender : TObject );
    end;

var
  dbc : udboutln.TDatabase;

procedure fillscreen( chars : string );
  var y, x, len : cardinal; line : string;
  begin
    len := length(chars);
    setlength(line, kvm.width);
    for y := 0 to kvm.maxY do
      begin
        gotoxy(0,y);
        for x := 1 to kvm.width do line[x] := chars[random(len)+1];
        write(line);
      end
  end;

procedure TDbOutlnApp.Initialize;
  begin
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
    cmdr := TKeyCommander.Create(dbc);
    with cmdr do begin
      keyMap[^P] := curs.Prev;
      keyMap[^N] := curs.Next;
      keyMap['p'] := curs.Prev;
      keyMap['n'] := curs.Next;
      keyMap['['] := curs.ToTop;
      keyMap[']'] := curs.ToEnd;
      keyMap[^C ] := self.Quit;
      keymap[^I ] := curs.Toggle;
      keymap[^T ] := self.ChooseType;
      keyMap[^L ] := self.Redraw;
    end;
    Redraw;
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

procedure TDbOutlnApp.DoRun;
  begin
    cmdr.HandleKeys;
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
    rs := rsKinds;
    rs.Open;
    rs.First;

    c := TDbCursor.Create(dbc);
    c.RecordSet := rs;
    c.KeyField := 'knd';
    c.Mark := rs['knd'];

    repeat
      gotoxy(0,0);

      { draw column headers }
      bg('K');fg('W');
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

procedure TDbOutlnApp.Quit;
  begin
    Terminate;
  end;

var app : TDbOutlnApp;
begin
  dbc := connect('minneron.sdb');
  App := TDbOutlnApp.Create(dbc);
  hidecursor;
  App.Initialize;
  App.Run;
  dbc.Free;
  clrscr; fg(7); bg(0); showcursor;
end.
