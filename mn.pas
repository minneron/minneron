{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
----------------------------------------------------------------
Initial version forked from cedit.pas
---------------------------------------------------------------}
{$i xpc.inc}
program mn;
uses ll, li, fs, stri, num, cw, crt, buf;

  type
    editor = class
      buf : buf.buffer;
      x, y, h, w : integer;
      topline, position : buf.buffer.cursor;
      constructor create;
      function load( path : string ) : boolean;
      procedure show;
      procedure run;

      { cursor movement commands }
      procedure arrowup;
      procedure arrowdown;
      procedure home;
      procedure _end;
      procedure pageup;
      procedure pagedown;
    end;

  constructor editor.create;
  begin
    inherited;
    x := 1;
    y := 1;
    w := crt.windMaxX;
    h := crt.windMaxY;
    //pause( 'windmaxy = ' + n2s( h ));
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

    procedure show_highlight;
    begin
      if cur.index = position.index
        then cwrite( '|!b' )
        else cwrite( '|!k' )
    end;

    procedure show_nums;
    begin
      cwritexy( 1, ypos, '|Y|!m' );
      write( flushrt( n2s( cur.index ), 3, ' ' ));
      cwrite( '|w' );
    end;

    procedure show_line( line : string );
    begin
      cwrite( stri.trunc( line, cw.scr.w - cw.cur.x ));
      cwrite( '|%' ); // clreol
    end;

  begin
    // clrscr; //  fillbox( 1, 1, crt.windmaxx, crt.windmaxy, $0F20 );
    show_curpos;
    ypos := 2;
    cur := self.buf.make_cursor;
    cur.move_to( self.topline );
    repeat
      show_nums;
      show_highlight;

      if cur.value.inheritsfrom( li.strnode ) then
        show_line( li.strnode( cur.value ).str );

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
    self.position.to_top;
    self.topline.to_top;
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
    self.home;
    repeat
      show;
      ch := crt.readkey;
      case ch of
	#27, ^C	: done := true;
	^N	: arrowdown;
	^P	: arrowup;
	^A	: home;
	^E	: _end;
	#0	: case crt.readkey of
		    #72	: arrowup; // when you press the UP arrow!
		    #80	: arrowdown; // when you press the DOWN arrow!
		    #71	: home;
		    #79	: _end;
		    #73	: pageup;
		    #81	: pagedown;
		  end;
	else;
      end
    until done;
  end;

  procedure editor.arrowup;
  begin
    if self.position.move_prev then
    begin
      if self.position.index - self.topline.index < 5 then
          if self.topline.index > 1 then
             self.topline.move_prev;
      //  scrolldown1(1,80,y1,y2,nil);
      //  scrolldown1(1,80,14,25,nil);
    end
    else self.position.move_next;
  end;


  procedure editor.arrowdown;
    var screenline : word;
  begin
    if self.position.move_next then
      begin
        assert( self.topline.index <= self.position.index );
        screenline := self.position.index - self.topline.index;
	if ( screenline > self.h - 5 ) and ( self.topline.index < self.buf.count ) then
           self.topline.move_next
          //  scrollup1(1,80,y1,y2,nil);
          //  scrollup1(1,80,14,25,nil);
      end
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
