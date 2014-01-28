//-----------------------------------------------------------------------
//
// classes to make working with SqlDb nicer outside of lazarus.
//
//-----------------------------------------------------------------------
{$mode delphi}{$i xpc.inc}
unit udb;
interface
uses db, sqldb, sqlite3conn,
  sysutils, // for exceptions
  classes; // for TStringList

type
  TRecordSet = class (TSqlQuery)
    constructor Create(dbc : TSqlConnection;  query : string); reintroduce;
    function Execute(q : string) : TRecordSet;
    function Open: TRecordSet; reintroduce;
    function First: TRecordSet; reintroduce;
  end;
  TDatabase = class (TSqlite3Connection)
    function Query(sql : string) : TRecordSet;
    procedure RunSQL(sql : string; args : array of variant);
  end;
  function connect(const path : string) : TDatabase;

implementation

//- - [ TRecordSet ]  - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRecordSet.Create(dbc : TSQLConnection; query : string);
  begin
    inherited Create(dbc);
    self.database := dbc;
    self.sql.text := query;
  end;

function TRecordSet.Open : TRecordSet;
  begin
    inherited open; result := self;
  end;

function TRecordSet.First : TRecordSet;
  begin
    inherited first; result := self;
  end;

function TRecordSet.Execute(q : string) : TRecordSet;
  begin
    self.sql.text := q;
    self.open;
    result := self;
  end;

//- - [ TDatabase ] - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDatabase.Query(sql : string) : TRecordSet;
  begin
    result := TRecordSet.Create(self, sql);
    result.Transaction := self.Transaction;
    result.Open;
  end;

procedure TDatabase.RunSQL(sql : string; args : array of variant);
  var param : TParam; c, i : cardinal; s : string; rs : TRecordSet;
  begin
    rs := TRecordSet.Create(self, sql);
    rs.Transaction := self.Transaction;
    rs.ParseSQL := true;
    c := rs.params.count;
    if c <> length(args) then
      raise Exception('query expects ' + IntToStr(c)
                    +' but ' + IntToStr(length(args)) + ' were supplied');
    if c > 0 then begin
      for i := 0 to (c-1) do rs.params[i].AsString := args[i];
    end;
    rs.ExecSQL;
    rs.Free;
  end;

//- - [ misc routines ] - - - - - - - - - - - - - - - - - - - - - - - - -

function connect(const path : string) : TDatabase;
  begin
    result := TDatabase.Create(Nil);
    result.DatabaseName := path;
    result.Transaction := TSqlTransaction.Create(result);
    result.Open;
  end;

initialization
end.
