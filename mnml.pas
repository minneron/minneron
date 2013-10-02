{ minneron main loop }
{$mode delphi}
unit mnml;
interface uses sysutils, vorunati, arrays;
  type
    TVoRun = function : vor of object;
    TThunk = procedure;
    TToken = string[31];
    TCmdId = cardinal;

  procedure define( out cmd : TCmdId; thunk : TThunk; token : TToken );
  procedure launch( const cmd : TCmdId ); overload;
  procedure launch( const cmd : TToken ); overload;
  procedure launch( task : IVorTask ); overload;
  procedure step;
  function  done : boolean;
  procedure loop;

implementation
var
  queue  : array of TCmdId;
  tasks	 : GArray<IVorTask>;
  thunks : array of TThunk;
  tokens : array of TToken;


procedure define( out cmd : TCmdId; thunk : TThunk; token : TToken );
  begin
    cmd := length(thunks);
    setlength(thunks, cmd+1);
    setlength(tokens, cmd+1);
    thunks[cmd] := thunk;
    tokens[cmd] := token;
  end;


procedure launch( const cmd  : TCmdId );
  begin
    setlength(queue, length(queue)+1);
    queue[cmd] := cmd;
  end;

procedure launch( const cmd  : TToken );
  var i : cardinal; found : boolean = false;
  begin
    for i := 0 to length(queue)-1 do
      if tokens[i]=cmd then
        begin
          launch( i );
          found := true;
        end;
    if not found then
      raise Exception.Create('undefined: ' + cmd );
  end;

procedure launch( task : IVorTask );
  begin
    tasks.Append( task );
  end;

procedure step;
  var i : byte; a : IVorTask; n : cardinal;
  begin
    n := length(queue);
    if n > 0 then for i := 0 to n - 1 do thunks[queue[i]]();
    if length(queue) > n then
      { shift newly launched commands to start of the array }
      for i := n to length(queue) - 1 do queue[i-n] := queue[i];
    { now truncate the array so only the new items remain }
    SetLength(queue, Length(queue)-n);

    { dispatch to all tasks }
    i := 0;
    n := tasks.length;
    while i < tasks.length do
      begin
	a := tasks[ i ];
	if a.state = vo then begin
	  a.step;
	  if a.state in [vo, ru] then inc(i)
	  else begin
	    Dec(n);
	    tasks[ i ] := tasks[ n ];
	    tasks[ n ] := nil;
	  end
	end else inc(i) { was inactive, skip over for now }
      end;
    tasks.length := n;
  end; { Step }

procedure loop;
  begin
    repeat step until done
  end;

function done : boolean; inline;
  begin
    done := length(queue) = 0;
  end;

initialization
  tasks := (GArray<IVorTask>).Create;
finalization
  SetLength(tokens, 0);
  SetLength(thunks, 0);
  SetLength(queue,  0);
end.
