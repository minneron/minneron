{ editor widget for minneron }
{$mode delphi}{$I xpc.inc}{$H+}
unit mned;
interface uses xpc, fs, stri, num, cw, ui, kvm, kbd, cli,
  tiles, vorunati, sysutils, mnml, mnbuf, mnrnd, impworld;

type
  
  TEditor = class (TMorph)
    buf               : TBuffer;
    filename          : string;
    status            : string;
    x, y, h, w        : integer;
    topline, position : cardinal;
    led               : ui.zinput;  // led = (L)ine (ED)itor
    state             : vor;
    dirty             : boolean;
  public { basic interface }
    constructor Create;
    function Load( path : string ) : boolean;
    function SaveAs( path : string ) : boolean;
    function Save : boolean;
    function Done : boolean;
  public { morph interface }
    function OnKeyPress( ch : char ) : boolean; override;
    procedure Draw; override;
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
    self.led := ui.zinput.create;
    self.ToTop;
    TellUser('welcome to minneron.');
  end;

procedure TEditor.TellUser(msg : string);
  begin
    status := msg;
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

function TEditor.Save : boolean;
  begin
    buf.SaveToFile(self.filename);
    result := true; // TODO error checking
    TellUser(filename + ' saved.');
  end;

function TEditor.SaveAs( path : string ) : boolean;
  var oldname : string;
  begin
    oldname := self.filename;
    self.filename := path;
    result := self.Save;
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
    for i := kvm.maxY div 2 downto 1 do dec(topline);
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

function TEditor.OnKeyPress( ch : char ) : boolean;
  begin
    result := true;
    case ch of
      ^C : self.halt;
      ^R : begin HideCursor; mnml.launch(cmd_rnd); end;
      ^N : NextLine;
      ^P : PrevLine;
      ^M : Newline;
      ^D : DeleteNextChar;
      ^S : Save;
      ^V : NextPage;
      ^U : PrevPage;
      #0 : case kbd.readkey(ch) of
             #72 : PrevLine; // when you press the UP arrow!
             #80 : NextLine; // when you press the DOWN arrow!
             #71 : ToTop;
             #79 : ToEnd;
             #73 : PrevPage;
             #81 : NextPage;
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
