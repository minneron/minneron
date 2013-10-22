{$mode delphi}
program dboutln;
uses xpc, udboutln, custapp, classes, kvm, cw, uminneron;

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
      rs   : TRecordSet;
    public
      procedure Initialize; override;
      procedure DoRun; override;
      procedure CmdCreateNode(Sender : TObject);
      procedure CmdQuit(Sender : TObject);
      procedure CmdRedraw(Sender : TObject);
    end;

var
  db : TDatabase;


  
procedure TDbOutlnApp.Initialize;
  begin
    rs := db.query('select nid, kind, node, depth, collapsed, '+
		   'hidden, leaf from outline');
    curs := TDbCursor.Create(db);
    curs.RecordSet := rs;
    curs.KeyField := 'nid';
    curs.canHideRows := true;
    curs.hideFlag := 'hidden';
    curs.Mark := rs['nid'];
    view := TDbTreeGrid.Create(db);
    view.datacursor := curs;
    curs.OnMarkChanged := view.CmdRedraw;
    cmdr := TKeyCommander.Create(db);
    with cmdr do begin
      keyMap['p'] := curs.CmdPrev;
      keyMap['n'] := curs.CmdNext;
      keyMap['['] := curs.CmdToTop;
      keyMap[']'] := curs.CmdToEnd;
      keyMap['q'] := self.CmdQuit;
      keyMap[^C ] := self.CmdQuit;

      keyMap[^L ] := self.CmdRedraw;
    end;
    clrscr;
    view.Redraw;
  end;

procedure TDbOutlnApp.DoRun;
  begin
    cmdr.HandleKeys;
  end;

procedure TDbOutlnApp.CmdCreateNode(sender : TObject);
  begin
  end;

procedure TDbOutlnApp.CmdQuit(Sender :TObject);
  begin
    Terminate;
  end;

procedure TDbOutlnApp.CmdRedraw(Sender :TObject);
  begin
    view.Redraw;
  end;
  
var app : TDbOutlnApp;
begin
  db := connect('minneron.sdb');
  App := TDbOutlnApp.Create(db);
  App.Initialize;
  App.Run;
  db.Free;
  clrscr; fg(7); bg(0);
end.
