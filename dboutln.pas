{$mode delphi}{$i xpc.inc}
program dboutln;
uses db, sqldb, sqlite3conn,
     classes, // for TStringList
     xpc;

//-----------------------------------------------------------------------

// classes to make working with SqlDb nicer outside of lazarus.
type
  TRecordSet = class (TSqlQuery)
    constructor Create(dbc : TSqlConnection;  query : string); reintroduce;
  end;
  TDbHelper = class helper for TSqlConnection
    function Query(sql : string) : TRecordSet;
  end;

//- - [ TRecordSet ]  - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRecordSet.Create(dbc : TSQLConnection; query : string);
  begin
    inherited Create(dbc);
    self.database := dbc;
    self.sql := TStringList.Create;
    self.sql.add(query);
  end;

//- - [ TDbHelper ] - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDbHelper.Query(sql : string) : TRecordSet;
  begin
    result := TRecordSet.Create(self, sql);
    result.Transaction := self.Transaction;
    result.Open;
  end;

//- - [ misc routines ] - - - - - - - - - - - - - - - - - - - - - - - - -

function connect(const path : string) : TSqlConnection;
  begin
    result := TSqlite3Connection.Create(Nil);
    result.DatabaseName := path;
    result.Transaction := TSqlTransaction.Create(result);
    result.Open;
  end;

//-----------------------------------------------------------------------

var
  db  : TSqlConnection;
  rs  : TRecordSet;
  i   : integer;
  depth: integer = -1;
  sigil : char = ' ';
begin
  db := connect('minneron.sdb');
  rs := db.query('select kind, node, depth, collapsed,'
                 +' hidden, leaf from outline');
  while not rs.eof do
    begin
      if rs['depth'] > depth then pass
      else if rs['depth'] < depth then pass
      else pass;
      depth := rs['depth'];

      if rs['collapsed']=1 then sigil := '+'
      else if rs['leaf']=1 then sigil := ' '
      else sigil := '-';

      if rs['hidden'] = 0 then
        begin
          if rs['depth'] > 0 then for i := 1 to rs['depth'] do write('  ');
          writeln(sigil, ' ', rs['node']);
        end;
      rs.Next;
    end;
end.
