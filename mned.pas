{ editor widget for minneron }
{$mode delphi}{$I xpc.inc}{$H+}
unit mned;
interface uses xpc, fs, stri, num, cw, ui, kvm, kbd, cli,
  tiles, vorunati, sysutils, mnml, mnbuf, mnrnd, impworld;

type
  TEditor = class (TMorph)
    buf               : ITextTile;
    filename          : string;
    message           : string;
    x, y, h, w        : integer;
    topline, position : cardinal;
    led               : ui.zinput;  // led = (L)ine (ED)itor
    state             : vor;
    dirty             : boolean;
  public { basic interface }
    constructor Create;
    function Load( path : string ) : boolean;
    function Save_as( path : string ) : boolean;
    function Save : boolean;
    procedure Init;
    function Done : boolean;
  public { morph interface }
    function OnKeyPress( ch : char ) : boolean; override;
    procedure Draw; override;
  public { cursor movement commands }
    procedure arrowup;
    procedure arrowdown;
    procedure home;
    procedure _end;
    procedure pageup;
    procedure pagedown;
  public { line manipulation commands }
    procedure newline;
    procedure delete;
  private { misc internal methods }
    procedure updateCamera;
    procedure grabLine;
    procedure keepInput;
    procedure moveInput;
    procedure parse( var txt : text );
  end;

implementation

constructor TEditor.Create;
  begin
    inherited;
    x := 0;
    y := 0;
    w := kvm.width;
    h := kvm.height;
    self.buf := TBuffer.create(w, h - 1);
    topline := 0;
    position := 0;
    filename := '';
    dirty := true;
    message  := 'welcome to minneron.';
  end;

{ file methods }

procedure TEditor.parse( var txt : text );
  var line : string;
  begin
    while not eof( txt ) do begin
      readln( txt, line );
      self.buf.AddLine( line );
    end;
    // li.print( lisnode.create( self.buf ));
  end;

function TEditor.Load( path : string ) : boolean;
  var txt : text;
  begin
    result := fs.exists( path );
    if result then
    begin
      //  need to check for io errors in here
      assign( txt, path );
      reset( txt );
      self.parse( txt );
      close( txt );
      self.filename := path;
    end
    else message := 'couldn''t load "' + path + '"';
  end; { TEditor.load }

function TEditor.save : boolean;
  var txt: text; i : cardinal;
  begin
    assign( txt, self.filename );
    rewrite( txt );
    for i := 0 to self.buf.length -1 do writeln(txt, buf[i]);
    close( txt );
    result := true; // TODO error checking
    message := filename + ' saved.';
  end;

function TEditor.save_as( path : string ) : boolean;
  var oldname : string;
  begin
    oldname := self.filename;
    self.filename := path;
    result := self.save;
    if not result then self.filename := oldname
  end;

 { drawing routine }
procedure TEditor.Draw;
  var ypos, line : cardinal;
  procedure draw_curpos;
    begin
      cwritexy( 0, 0,
               '|!b' +
               '|B[|C' + flushrt( n2s( self.position ), 6, '.' ) +
               '|w/|c' + flushrt( n2s( self.buf.length ), 6, '.' ) +
               '|B]|Y ' + self.message +
               '|%' );
      self.message := '';
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
        x := cw.cur.x;
        y := cw.cur.y;
        tcol := $080f;
        dlen := cw.max.x - cw.cur.x
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
        repeat
          draw_gutter( flushrt( n2s( line ), 3, ' ' ));
          if line = position then PlaceEditor
          else draw_line(buf[line]);
          inc( ypos ); inc(line)
        until ( ypos >= self.h ) or ( line = buf.length );
        { fill in extra space if the file is too short }
        while ypos < self.h do begin
          cwritexy( 0, ypos, '|!K|%' );
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
procedure TEditor.home;
  begin
    if self.buf.length = 0 then exit;
    position := 0;
    topline := 0;
    led.work := buf[ 0 ];
  end;

procedure TEditor._end;
  var i : byte;
  begin
    position := self.buf.length - 1;
    topline := position;
    for i := kvm.maxY div 2 downto 1 do dec(topline);
  end;

procedure TEditor.grabLine;
    begin
      self.led.work := self.buf[self.position]
    end;

  procedure TEditor.arrowup;
    begin
      keepInput;
      if self.position > 0 then
        begin
          dec(self.position);
          moveInput;
        end;
      grabLine;
    end;

  procedure TEditor.arrowdown;
    begin
      keepInput;
      if self.position + 1 < self.buf.length then
        begin
          inc(self.position);
          moveInput;
        end;
      grabLine;
    end;

  procedure TEditor.pageup;
    var c : byte;
    begin
      for c := 1 to h do arrowup;
    end;

  procedure TEditor.pagedown;
    var c : byte;
    begin
      for c := 1 to h do arrowdown;
    end;

{ zinput integration }
  procedure TEditor.keepInput;
  begin
    buf[position] := led.value
  end;
  procedure TEditor.moveInput;
  begin
    updateCamera;
  end;

{ multi-line editor commands }
  procedure TEditor.newline;
    begin
      buf.InsLine(position, led.str_to_end );
      led.del_to_end;
      arrowdown;
      led.to_start;
    end;

  procedure TEditor.delete;
    begin
      if led.at_end and (position + 1 < buf.length) then
        begin
          led.work += buf.GetLine(position+1);
          buf.DelLine(position+1);
        end
      else led.del
    end;

{ event stuff }

procedure TEditor.Init;
  begin
    self.led := ui.zinput.create;
    self.home;
  end;

function TEditor.OnKeyPress( ch : char ) : boolean;
  begin
    result := true;
    case ch of
      ^C : self.halt;
      ^R : begin HideCursor; mnml.launch(cmd_rnd); end;
      ^N : arrowdown;
      ^P : arrowup;
      ^M : newline;
      ^D : delete;
      ^S : save;
      ^V : pagedown;
      ^U : pageup;
      #0 : case kbd.readkey(ch) of
             #72 : arrowup; // when you press the UP arrow!
             #80 : arrowdown; // when you press the DOWN arrow!
             #71 : home;
             #79 : _end;
             #73 : pageup;
             #81 : pagedown;
             ^M  : newline;
             else led.handlestripped( ch );
           end;
      else led.handle( ch );
    end;
    led.isdone := false;
    dirty := true;
  end;

function TEditor.Done : boolean;
  begin
    result := self.state = TI
  end;

initialization
end.
