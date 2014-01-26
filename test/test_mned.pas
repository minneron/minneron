{$mode delphi}{$H+}
{$i test_mned.def }
implementation uses mned;

var ed : mned.TEditor;
  
procedure setup;
  begin
    ed := TEditor.Create(nil)
  end;

procedure test_empty;
  begin
    chk.equal(ed.value, '');
  end;

procedure test_status;
  begin
    chk.equal(ed.status, '');
    ed.status := 'hello world';
    chk.equal(ed.status, 'hello world');
  end;

begin
end.
