{ editor widget for minneron }
{$mode delphi}{$i xpc.inc}{$H+}
unit mned;
interface uses xpc, classes, fs, ustr, num, cw, ui, kvm, kbd, fx,
  tiles, vorunati, sysutils, mnml, mnbuf, mnrnd, impworld, cli, ukm,
  uminneron;

type
  TEditor = class (TView)
    protected
      buf               : TBuffer;
      filename          : string;
      _status           : string;
      topline, position : cardinal;
      state             : vor;
    published { basic interface }
      led               : ui.zinput;  // led = (L)ine (ED)itor
      constructor Create(aOwner : TComponent); override;
      function Load( path : string ) : boolean;
      procedure SaveAs( path : string );
      procedure Save;
      procedure AddDefaultKeys( km : TKeyMap );
      procedure DelegateKey( ext : Boolean; ch : char);
      function value : string;
    public {  morph interface (removing this) }
      done              : boolean;
      dirty             : boolean;
      procedure Render( term : ITerm) ; override;
    public { cursor movement commands }
      procedure PrevLine;
      procedure NextLine;
      procedure ToTop;
      procedure ToEnd;
      procedure PrevPage;
      procedure NextPage;
    public { line manipulation commands }
      procedure Newline;
      procedure DeleteNextChar;
    private { misc internal methods }
      procedure updateCamera;
      procedure keepInput;
      procedure CursorMoved;
      procedure TellUser(msg : string);
    public
      property status : string read _status write TellUser;
    end;

implementation


constructor TEditor.Create( aOwner : TComponent );
  begin
    inherited Create( aOwner );
    x := 0;
    y := 0;
    w := kvm.width;
    h := kvm.height;
    self.buf := TBuffer.create(w, h - 1);
    self.buf.addline('');
    topline := 0;
    position := 0;
    filename := '';
    done := false;
    dirty := true;
    self.led := ui.ZInput.Create(aOwner);
    self.ToTop;
  end;

procedure TEditor.TellUser(msg : string);
  begin
    _status := msg;
  end;

function TEditor.value : string;
  begin
    result := self.buf.ToString;
  end;


{ file methods }

function TEditor.Load( path : string ) : boolean;
  begin
    result := false;
    try
      buf.LoadFromFile(path);
      self.filename := path;
      result := true;
    except
      on e:EFileNotFound do TellUser('invalid path:' + path);
    end;
  end;

procedure TEditor.Save;
  begin
    buf.SaveToFile(self.filename);
    TellUser(filename + ' saved.');
  end;

procedure TEditor.SaveAs( path : string );
  var oldname : string;
  begin
    oldname := self.filename;
    self.filename := path;
    self.filename := oldname
  end;

 { drawing routine }
procedure TEditor.Render( term : ITerm );
  var ypos, line : cardinal;
  procedure draw_curpos;
    begin
      cwritexy( 0, 0,
               '|!b' +
               '|B[|C' + flushrt( n2s( self.position ), 6, '.' ) +
               '|w/|c' + flushrt( n2s( self.buf.length ), 6, '.' ) +
               '|B]|Y ' + self.status +
               '|%' );
      self.status := '';
    end;

  procedure draw_gutter( s : string );
    var color : char = 'c';
    begin
      if line = position then color := 'C';
      cwritexy( 0, ypos, '|k|!' + color + s + '|!k|w' );
    end;

  procedure PlaceEditor;
    begin
      { This simply positions the input widget. }
      with self.led do begin
	x := term.wherex - self.x;
	y := term.wherey - self.y;
        tcol := $080f;
	dlen := self.w - x;
      end;
    end;

  procedure draw_line(s:string);
    begin
      cwrite(s + '|!k|%' );
    end;

  begin { TEditor.draw }
    if dirty then
      begin
        dirty := false;
        HideCursor;
        cwrite('|w|!b');
        //todo  fillbox( 1, 1, kvm.maxX, kvm.maxY, $0F20 );
        draw_curpos;
        ypos := 1; // line 0 is for the status / cursor position
        line := topline;
	if buf.length > 0 then
	  repeat
	    draw_gutter( flushrt( n2s( line ), 3, ' ' ));
	    if line = position then PlaceEditor
	    else draw_line(buf[line]);
	    inc( ypos ); inc(line)
	  until ( ypos >= self.h ) or ( line = buf.length )
	else ypos := 2;
        { fill in extra space if the file is too short }
        while ypos < self.h do begin
          cwritexy( 0, ypos, '|!k|%' );
          inc( ypos )
        end;
	led.show;
        // ShowCursor;
      end;
  end;

procedure TEditor.updatecamera;
  var screenline : word;
  begin
    assert(topline <= position );
    screenline := position - topline;
    if ( screenline < 5 ) and ( topline > 1 ) then
      begin
        dec(topline)
        //  scrolldown1(1,80,y1,y2,nil);
        //  scrolldown1(1,80,14,25,nil);
      end
    else if ( screenline > self.h - 5 )
      and ( self.topline < self.buf.length ) then
    begin
      inc( topline );
      //  scrollup1(1,80,y1,y2,nil);
      //  scrollup1(1,80,14,25,nil);
    end
  end;

{  cursor movement interface }
procedure TEditor.ToTop;
  begin
    if self.buf.length = 0 then exit;
    position := 0;
    topline := 0;
    led.work := buf[ 0 ];
  end;

procedure TEditor.ToEnd;
  var i : byte;
  begin
    position := self.buf.length - 1;
    topline := position;
    for i := kvm.yMax div 2 downto 1 do dec(topline);
  end;

procedure TEditor.PrevLine;
  begin
    keepInput;
    if self.position > 0 then
      begin
	dec(self.position);
	CursorMoved;
      end;
  end;

procedure TEditor.NextLine;
  begin
    keepInput;
    if self.position + 1 < self.buf.length then
      begin
	inc(self.position);
	CursorMoved;
      end;
  end;

procedure TEditor.PrevPage;
  var c : byte;
  begin
    for c := 1 to h do PrevLine;
  end;

procedure TEditor.NextPage;
  var c : byte;
  begin
    for c := 1 to h do NextLine;
  end;

{ zinput integration }

procedure TEditor.keepInput;
  begin
    buf[position] := led.value
  end;

procedure TEditor.CursorMoved;
  begin
    self.dirty := true;
    updateCamera;
    self.led.work := self.buf[self.position]
  end;

{ multi-line editor commands }
procedure TEditor.Newline;
  begin
    buf.InsLine(position, led.str_to_end );
    led.del_to_end;
    NextLine;
    led.to_start;
  end;

procedure TEditor.DeleteNextChar;
  begin
    if led.at_end and (position + 1 < buf.length) then
      begin
	led.work += buf.GetLine(position+1);
	buf.DelLine(position+1);
      end
    else led.del
  end;

{ event stuff }

procedure TEditor.DelegateKey( ext : boolean; ch : char );
  begin
    if ext then led.handlestripped(ch)
    else led.handle(ch);
    self.dirty := true;
  end;

procedure TEditor.AddDefaultKeys( km : TKeyMap );
  var ch : widechar;
  begin
    for ch := #0 to #225 do km.crt[ ch ] := DelegateKey;
    for ch := #$EE00  to #$EEFF do km.crt[ ch ] := DelegateKey;
    km.cmd[ ^N ] := NextLine;
    km.cmd[ ^P ] := PrevLine;
    km.cmd[ ^M ] := Newline;
    km.cmd[ ^D ] := DeleteNextChar;
    km.cmd[ ^S ] := Save;
    km.cmd[ ^V ] := NextPage;
    km.cmd[ ^U ] := PrevPage;
    km.cmd[ kbd.UP ] := PrevLine;
    km.cmd[ kbd.DOWN ] := PrevLine;
    km.cmd[ kbd.HOME ] := ToTop;
    km.cmd[ kbd.END_ ] := ToEnd;
    km.cmd[ kbd.PgUp ] := PrevPage;
    km.cmd[ kbd.PgDn ] := NextPage;
  end;

initialization
end.
