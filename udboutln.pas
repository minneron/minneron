//-----------------------------------------------------------------------
//
// classes to make working with SqlDb nicer outside of lazarus.
//
//-----------------------------------------------------------------------
{$mode delphi}{$i xpc.inc}
unit udboutln;
interface
uses db, sqldb, sqlite3conn,
     classes, // for TStringList
     xpc;

type
  TRecordSet = class (TSqlQuery)
    constructor Create(dbc : TSqlConnection;  query : string); reintroduce;
  end;
  TDatabase = class (TSqlite3Connection)
    function Query(sql : string) : TRecordSet;
  end;
  function connect(const path : string) : TDatabase;

implementation

//- - [ TRecordSet ]  - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TRecordSet.Create(dbc : TSQLConnection; query : string);
  begin
    inherited Create(dbc);
    self.database := dbc;
    self.sql := TStringList.Create;
    self.sql.add(query);
  end;

//- - [ TDatabase ] - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDatabase.Query(sql : string) : TRecordSet;
  begin
    result := TRecordSet.Create(self, sql);
    result.Transaction := self.Transaction;
    result.Open;
  end;

//- - [ misc routines ] - - - - - - - - - - - - - - - - - - - - - - - - -

function connect(const path : string) : TDatabase;
  begin
    result := TDatabase.Create(Nil);
    result.DatabaseName := path;
    result.Transaction := TSqlTransaction.Create(result);
    result.Open;
  end;

//-----------------------------------------------------------------------

procedure main;
  var
    db	  : TDatabase;
    rs	  : TRecordSet;
    i	  : integer;
    depth : integer = -1;
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
  end; { main }

initialization
end.
