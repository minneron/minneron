{ this is a token-centric editor. }
{$i xpc.inc}{$mode delphi}
program tok;
uses uapp, mned, ukm, utok, ui, kvm,
     dndk, undk, udb, udc, uminneron, mnbuf, classes;

type TTokEd = class (uapp.TCustomApp)
  protected
    ndk : dndk.IBase;
    cmd : ui.ZInput;
    here : string;
    hist : array of string;
    page : mnbuf.TBuffer;
    dirty: boolean;
  public
    procedure init; override;
    procedure step; override;
    procedure draw; override;
    procedure keys(km : ukm.TKeyMap); override;
    procedure DelegateKey( ext : boolean; ch : char );
    procedure OnCmdAccept( s :  string );
    procedure OnSpace;
  end;

procedure TTokEd.init;
  begin
    page := TBuffer.Create(64, kvm.height);
    ndk := undk.open('stuff.ndk');
    cmd := ui.ZInput.Create(self);
    cmd.x := xMax div 2 - 16; cmd.y := yMax div 2;
    cmd.dlen := 32; cmd.maxlen := cmd.dlen;
    cmd.tcol := $11f3; cmd.work := '';
    cmd.OnAccept := self.OnCmdAccept;
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
    page.addline(s); dirty := true;
  end;
  
procedure TTokEd.onspace;
  var tmp : string;
  begin
    tmp := cmd.work; cmd.reset;
    oncmdaccept(tmp);
    draw;
  end;

procedure TTokEd.step;
  begin
    if cmd.is_dirty then cmd.Show;
  end;


procedure TTokEd.draw;
  var line : string;
  begin
    gotoxy(0,0); clrscr;
    for line in page.tostrings do write(line);
    if dirty then begin end; dirty := false;
  end;
  
begin
  uapp.run(TTokEd);
end.
