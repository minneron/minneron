// mned : console mode text editor widget for minneron.
//
// Copyright © 2014 Michal J Wallace http://tangentstorm.com/
// Available for use under the MIT license. See LICENSE.txt
{$mode delphi}{$i xpc.inc}{$H+}
unit mned;
interface uses xpc, classes, fs, ustr, num, cw, ui, kvm, kbd, fx,
  vorunati, sysutils, mnml, mnbuf, impworld, cli, utv, ukm, umsg, ug2d;

type
  TEditor = class (utv.TGridView)
    protected
      buf               : TBuffer;
      filename          : string;
      _status           : string;
      state             : vor;
    public { basic TView interface }
      led               : ui.zinput;  // led = (L)ine (ED)itor
      constructor Create(aOwner : TComponent); override;
      function Load( path : string ) : boolean;
      procedure LoadFromStr( s : TStr );
      procedure SaveAs( path : string );
      procedure Save;
      procedure Keys( km : TKeyMap );
      function value : string;
      procedure Render; override;
      procedure RestoreCursor; override;
      procedure Handle( msg : umsg.TMsg ); override;
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
      function LineCount : word;
      procedure keepInput;
      procedure CursorMoved;
      procedure TellUser(msg : TStr);
    public
      property status : TStr read _status write TellUser;
      property buffer : mnbuf.TBuffer read buf write buf;
      property path : TStr read filename write filename;
    end;

implementation


constructor TEditor.Create( aOwner : TComponent );
  begin inherited;
    _GetRowCount := LineCount;
    self.buf := TBuffer.create(self);
    self.buf.addline('');
    _vgy := 0; _igy := 0; filename := '';
    self.led := ui.ZInput.Create(self);
    _views.Append(self.led);
    self.ToTop;
  end;

procedure TEditor.TellUser(msg : string);
  begin _status := msg;
  end;

function TEditor.value : string;
  begin result := self.buf.ToString;
  end;

function TEditor.LineCount : word;
  begin result := self.buf.length
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

procedure TEditor.LoadFromStr( s : TStr );
  begin
    buffer.loadFromString(s);
    _vgy := 0; _igy := 0;
    led.work := buffer[ 0 ];
    smudge;
  end;

procedure TEditor.Save;
  begin
    buf.SaveToFile(self.filename);
    TellUser(filename + ' saved.');
  end;

procedure TEditor.SaveAs( path : string );
//  var oldname : string;
  begin
//  TODO : SaveAs
//    oldname := self.filename;
//    self.filename := path;
//    self.filename := oldname
  end;

 { drawing routine }
procedure TEditor.Render;
  var ypos, line : cardinal;
  const gutw = 3;
  procedure draw_curpos;
    begin
      cwritexy( 0, 0,
               '|!b' +
               '|B[|C' + flushrt( n2s( _igy ), 6, '.' ) +
               '|w/|c' + flushrt( n2s( self.buf.length ), 6, '.' ) +
	       '|B]|Y ' + cwpad(self.status, self.w - 15, ' ') +
               '|%' );
      self.status := '';
    end;

  procedure draw_gutter( num : cardinal );
    var color : char = 'c';
    begin
      if line = _igy then color := 'C';
      cwritexy( 0, ypos, '|k|!' + color +
	       flushrt( n2s( num ), gutw, ' ' ));
    end;

  procedure PlaceEditor;
    begin
      { This simply positions the input widget. }
      with self.led do begin
	x := gutw; y := ypos;
	if self._focused then tcol := $080f else tcol := $ea0f;
	acol := $0800; // arrow color
	w := self.w - gutw; smudge;
      end;
    end;

  procedure draw_line(s:string);
    begin cwrite(cwpad('|!k|w' + s + '|!k', self.w));
    end;

  begin { TEditor.Render }
    HideCursor; cwrite('|w|!b'); draw_curpos;
    line := _vgy; ypos := 1; // line 0 is status bar
    if buf.length > 0 then
      repeat
	draw_gutter( line );
	if line = _igy then PlaceEditor
	else draw_line(buf[line]);
	inc( ypos ); inc(line)
      until ( ypos = self.h-1 ) or ( line = buf.length )
    else ypos := 2;
    { fill in extra space if the file is too short }
    while ypos < self.h-1 do begin
      cwritexy( 0, ypos, '|!k|%' );
      inc( ypos )
    end;
  end;

procedure TEditor.RestoreCursor;
  begin gotoxy(_x + led.x + led.cpos, _y+led.y);
  end;

procedure TEditor.Handle( msg : umsg.TMsg );
  begin
    case msg.code of
      k_nav_up  : self.PrevLine;
      k_nav_dn  : self.NextLine;
      k_nav_top : self.ToTop;
      k_nav_end : self.ToEnd;
      else ok
    end
  end;


{  cursor movement interface }
procedure TEditor.ToTop;
  begin
    if self.buf.length = 0 then exit;
    _igy := 0;
    _vgy := 0;
    led.work := buf[ 0 ];
  end;

procedure TEditor.ToEnd;
  var i : byte;
  begin
    _igy := max(0, self.buf.length - 1);
    _vgy := _igy;
    for i := kvm.yMax div 2 downto 1 do dec(_vgy);
  end;

procedure TEditor.PrevLine;
  begin
    keepInput;
    if _igy > 0 then
      begin
	dec(_igy);
	CursorMoved;
      end;
  end;

procedure TEditor.NextLine;
  begin
    keepInput;
    if _igy + 1 < self.buf.length then
      begin
	inc(_igy);
	CursorMoved;
      end;
  end;

procedure TEditor.PrevPage;
  var c : byte;
  begin for c := 1 to h do PrevLine;
  end;

procedure TEditor.NextPage;
  var c : byte;
  begin for c := 1 to h do NextLine;
  end;

{ zinput integration }

procedure TEditor.keepInput;
  begin buf[_igy] := led.value
  end;

procedure TEditor.CursorMoved;
  begin smudge; updateCamera; self.led.work := self.buf[_igy]
  end;

{ multi-line editor commands }
procedure TEditor.Newline;
  begin
    buf.InsLine(_igy, led.str_to_end );
    led.del_to_end;
    NextLine;
    led.to_start;
  end;

procedure TEditor.DeleteNextChar;
  begin
    if led.at_end and (_igy + 1 < buf.length) then
      begin
	led.work += buf.GetLine(_igy+1);
	buf.DelLine(_igy+1);
      end
    else led.del
  end;

{ event stuff }

procedure TEditor.Keys( km : TKeyMap );
  begin
    led.keys(km);
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
