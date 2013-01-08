{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
----------------------------------------------------------------
Initial version forked from cedit.pas
---------------------------------------------------------------}
{$i xpc.inc}
program mn;
uses ll, li, fs, stri, num, cw, crt, buf, ui, kbd, cli;

  type
    editor = class
      buf : buf.buffer;
      x, y, h, w : integer;
      topline, position : buf.buffer.cursor;
      led : ui.zinput;  // led = Line EDitor
      constructor create;
      function load( path : string ) : boolean;
      procedure show;
      procedure run;

     protected { cursor movement commands }
      procedure arrowup;
      procedure arrowdown;
      procedure home;
      procedure _end;
      procedure pageup;
      procedure pagedown;

     protected { line manipulation commands }
      procedure newline;
    end;

  constructor editor.create;
  begin
    inherited;
    x := 1;
    y := 1;
    w := crt.windMaxX;
    h := crt.windMaxY;
    self.buf := buffer.create;
    topline := self.buf.make_cursor;
    position := self.buf.make_cursor;
  end;

  function editor.load( path : string ) : boolean;
    var txt : text; line : string;
  begin
    result := fs.exists( path );
    if result then begin
      //  need to check for io errors in here
      assign( txt, path );
      reset( txt );
      while not eof( txt ) do begin
	readln( txt, line );
	self.buf.append( stringtoken.create( line ));
      end;
      close( txt );
    end;
  end;

  procedure editor.show;
    var
      ypos : cardinal;
      cur  : buffer.cursor;

    procedure show_curpos;
    begin
      cwritexy( 1, 1,
                '|B[|C' + flushrt( n2s( self.position.index ), 6, '.' ) +
                '|w/|c' + flushrt( n2s( self.buf.count ), 6, '.' ) +
		'|B]' +
                '|%' );
    end;

    procedure show_nums;
    begin
      cwritexy( 1, ypos, '|Y|!m' );
      write( flushrt( n2s( cur.index ), 3, ' ' ));
      cwrite( '|!k|w' );
    end;

    procedure show_edit( line : string );
    begin
      { This simply positions the input widget. }
      with self.led do begin
	x := crt.wherex;
	y := crt.wherey;
	tcol := $0E; // bright yellow
	dlen := crt.windmaxx - crt.wherex;
      end;
      // debug: clear to eol w/blue bg to show where control should be
      // cwrite( '|!b|%' );
      led.show;
    end;

    procedure show_line( line : string );
    begin
      cwrite( stri.trunc( line, cw.scr.w - cw.cur.x ));
      cwrite( '|%' ); // clreol
    end;

  var line : string = '';

  begin
    // clrscr; //  fillbox( 1, 1, crt.windmaxx, crt.windmaxy, $0F20 );
    show_curpos;
    ypos := 2;
    cur := self.buf.make_cursor;
    cur.move_to( self.topline );
    repeat
      if cur.value.inheritsfrom( li.strnode ) then
      begin
	show_nums;
	line := li.strnode( cur.value ).str;
	if cur.index = position.index then show_edit( line )
	else show_line( line );
      end;
      inc( ypos )
    until ( ypos = self.h ) or ( not cur.move_next );
    while ypos < self.h do begin
      cwritexy( 1, ypos, '|%' );
      inc( ypos )
    end
  end;


  procedure editor.home;
  begin
    if self.buf.first = nil then exit;
    position.to_top;
    topline.to_top;
    if position.value.inheritsfrom( li.strnode ) then
      led.work := li.strnode( position.value ).str
    else
      led.work := '<<marker>>';
  end;

  procedure editor._end;
    var i : byte;
  begin
    self.position.to_end;
    self.topline.to_end;
    for i := crt.windmaxy div 2 downto 1 do
      self.topline.move_prev;
  end;

  procedure editor.pageup;
    var c : byte;
  begin for c := 1 to h do arrowup;
  end;

  procedure editor.pagedown;
    var c : byte;
  begin for c := 1 to h do arrowdown;
  end;


  procedure editor.run;
    var done : boolean = false; ch : char;
  begin
    self.led := ui.zinput.create;
    self.home;
    repeat
      show; led.show;
      case kbd.readkey(ch) of
	^C, #27 : done := true;
	^N	: arrowdown;
	^P	: arrowup;
	^M	: newline;
	#0	: case kbd.readkey(ch) of
		    #72	: arrowup; // when you press the UP arrow!
		    #80	: arrowdown; // when you press the DOWN arrow!
		    #71	: home;
		    #79	: _end;
		    #73	: pageup;
		    #81	: pagedown;
		    else led.handlestripped( ch ); led.show;
		  end;
	else led.handle( ch ); led.show;
      end
    until done;
  end;

  procedure editor.arrowup;
  begin
    li.strnode(self.position.value).str := led.value;
    if self.position.move_prev then
    begin
      if self.position.index - self.topline.index < 5 then
          if self.topline.index > 1 then
             self.topline.move_prev;
      //  scrolldown1(1,80,y1,y2,nil);
      //  scrolldown1(1,80,14,25,nil);
    end
    else self.position.move_next;
    led.work := li.strnode(self.position.value).str;
  end;


  procedure editor.arrowdown;
    var screenline : word;
  begin
    li.strnode(self.position.value).str := led.value;
    if self.position.move_next then
      begin
        assert( self.topline.index <= self.position.index );
        screenline := self.position.index - self.topline.index;
	if ( screenline > self.h - 5 ) and ( self.topline.index < self.buf.count ) then
           self.topline.move_next
          //  scrollup1(1,80,y1,y2,nil);
          //  scrollup1(1,80,14,25,nil);
      end;
    led.work := li.strnode(self.position.value).str;
  end;


  procedure editor.newline;
  begin
    position.inject_next( strnode.create( led.str_to_end ));
    led.del_to_end;
    arrowdown;
    led.to_start
  end;


  var ed : editor;
begin
  crt.clrscr;
  ed := editor.create;
  if paramcount = 0 then
    writeln( 'usage : mn <filename>' )
  else if ed.load( paramstr( 1 )) then
  begin
    ed.run;
    ed.destroy;
    cwriteln( '|w|!k' );
  end
  else writeln( 'unable to load file: ', paramstr( 1 ));
end.
