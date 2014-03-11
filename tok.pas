{ this is a token-centric editor. }
{$mode delphiunicode}{$i xpc.inc}
program tok;
uses xpc, uapp, mned, ukm, utok, ui, kvm, ug2d, uww, num,
     dndk, undk, udb, udc, umin, mnbuf, classes, cw, cli;

type TToken = record
		x, y : cardinal; c : word;
	      end;
type TTokEd = class (uapp.TCustomApp)
  protected
    ndk : dndk.IBase;
    cmd : ui.ZInput;
    here : string;
    hist : array of string;
    wrap : uww.TWordWrap;
    buf : mnbuf.TBuffer;
    _dirty: boolean;
  public
    procedure init; override;
    procedure done; override;
    procedure step; override;
    procedure draw; override;
    procedure keys(km : ukm.TKeyMap); override;
    procedure LoadPage(key : string);
    procedure DelegateKey( ext : boolean; ch : char );
    procedure OnCmdAccept( s :  string );
    procedure OnSpace;
  end;

procedure TTokEd.init;
  begin
    ndk := undk.open('stuff.ndk');
    cmd := ui.ZInput.Create(self);
    cmd.x := xMax div 2 - 16; cmd.y := yMax div 2;
    cmd.dlen := 32; cmd.maxlen := cmd.dlen;
    cmd.tcol := $11f3; cmd.work := '';
    cmd.OnAccept := self.OnCmdAccept;
    wrap := uww.TWordWrap.Create(self); wrap.width := kvm.width;
    buf := TBuffer.Create(64, kvm.height);
    self.loadPage('home');
  end;

procedure TTokEd.done;
  begin
    wrap.Free;
  end;

procedure TTokEd.keys(km : ukm.TKeyMap);
  var ch: char;
  begin
    for ch := #0 to #225 do km.crt[ ch ] := DelegateKey;
    km.cmd[ ^C ] := self.quit;
    km.cmd[ #32 ] := self.OnSpace;
  end;

//  !! copied directly from TEditor.DelegateKey :/
procedure TTokEd.DelegateKey( ext : boolean; ch : char );
  begin
    if ext then cmd.handlestripped(ch) else cmd.handle(ch);
  end;

procedure TTokEd.OnCmdAccept( s : string );
  begin
    buf.addline(s); cmd.reset; _dirty := true;
  end;
  
procedure TTokEd.onspace;
  begin
    oncmdaccept(cmd.work);
  end;


procedure TTokEd.loadPage(key : string);
  var edges : TEdges; i : cardinal;
  begin
    buf.clear; edges := ndk.w(key,'[wd]','~');
    cwriteln('|B' + n2s(length(edges)) + ' |c words on page.');
    hitakey;
    if length(edges) > 0 then begin
      for i := 0 to high(edges) do buf.addline(edges[i].obj.s)
    end;
    _dirty := true;
  end;

procedure TTokEd.step;
  begin
    if _dirty then begin cmd.is_dirty := true; draw end;
    if cmd.is_dirty then cmd.Show;
  end;

procedure TTokEd.draw;
  var word : string; y : byte = 0;
      tok : ug2d.IBounds2D;
  begin
    gotoxy(0,y); clrscr; wrap.reset;
    tok := umin.TView.Create(Nil); tok.h := 1;
    for word in buf.tostrings do begin
      tok.w := length(word); wrap.place(tok);
      if tok.y > y then begin y := tok.y; gotoxy(0,y) end
      else if tok.x = 0 then ok
      else write(' ');
      write(word)
    end; wrap.debugdraw;
    _dirty := false;
  end;
  
begin
  uapp.run(TTokEd);
end.
