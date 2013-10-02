{$i xpc.inc}{$mode delphi}{$h+}
unit impworld;
interface uses
  xpc,      // pass, general compatability
  mnml,
  arrays,
  vorunati,
  sysutils, // AppendStr, FormatDateTime
  kvm,      // fg, gotoxy, clreol
  cw,       // cxy(color, x, y, string)
  num,      // n2s
  kbd;      // keypressed/readkey

  procedure step;
  procedure draw;
  function done : boolean;

implementation

{-- dynamic types -----------------}
type
  TObj = class { base type for all objetcs }
    public
      constructor Create;
      function Str: string; virtual;
    end;

constructor TObj.Create;
  begin
  end;

function TObj.Str : string;
  begin
    result := '$';
  end;

{-- fixed-size data blocks --------}
type
  Block = array[ 0 .. 1023 ] of byte;
  Drive = file of block;

{-- variable-sized buffers --------}
type
  TBytes = GArray<Byte>;


{-- display -----------------------}
type
  Point = object
    x, y : integer;
  end;

  Quad = object( Point )
    w, h : integer;
    function x2 : integer;
    function y2 : integer;
  end;

function Quad.x2 : integer;
  begin
    x2 := x + w
  end;

function Quad.y2 : integer;
  begin
    y2 := y + h
  end;

{-- tagged data types -------------}
type
  TTagged = class(TObj)
    public
      tag :longint;
    end;

  TSymbol = class(TTagged)
    public
      name : string[32];
    end;

  TToken = class(TTagged)
    public
      sym : TSymbol;
      line, column, span : longint;
    end;

{-- Tuples ---------------------------}
type
  TypeKind = ( tkSimple, tkTkUnion, tkFunction, tkSchema );
  TFieldDef = class;

  TTypeDef  = class(TObj)
    public
      size : Word;
      kind : TypeKind;
      numFields : byte;
      first : TFieldDef;
    end;

  TFieldDef = class(TObj)
    public
      next : TFieldDef;
      name : TSymbol;
    end;

  TTuple = class(TObj)
    public
      meta : TTypeDef;
      data : TBytes;
    end;

{-- actors ------------------------}
const
  cmd_quit  =  -1;
  cmd_step  =  -2;
  cmd_draw  =  -3;
  cmd_hide  =  -4;
  cmd_show  =  -5;
  cmd_help  =  -6;
  kGroupMaxSlot = 15;

type
  TMessage = class;
  TActor = class(TObj)
    active,           { wants update() }
    alive,            { exists but not alive triggers gc }
    visible,          { to allow hide/show }
    exists : boolean; { turn off everything at once }
    constructor Create;
    procedure Update; virtual;
    procedure Render; virtual;
    function Handle( msg : TMessage ):boolean; virtual;
  end;

  TGroup = class (TActor)
    members : array[ 0 .. kGroupMaxSlot ] of TActor;
    count   : byte;
    constructor Create;
    procedure Add( a : TActor );
    function Handle( msg : TMessage ):boolean; override;
  end;

  TMorph = class (TGroup)
    bounds : Quad;
    colors : word; { foreground and background }
    constructor Create;
    procedure Draw; virtual;
  end;

  TMessage = class (TTagged)
    sym : TSymbol;
    sender: TActor;
    args: TTuple;
  end;


constructor TActor.Create;
  begin
    alive  := true;
    active := true;
    exists := true;
    visible := false;
  end;

procedure TActor.Render;
  begin
  end;

procedure TActor.Update;
  begin
  end;

function TActor.Handle( msg : TMessage ):boolean;
  begin
    Handle := true;
    case msg.tag of
      cmd_quit: active := false;
      cmd_draw: Render;
      cmd_step: Update;
      else handle := false
    end
  end;

constructor TGroup.Create;
  begin
    self.count := 0;
  end;

procedure TGroup.Add( a : TActor );
  begin
    if self.count < kGroupMaxSlot then
      begin
        self.members[count] := a;
        inc(self.count);
      end
  end;

function TGroup.Handle( msg: TMessage ):boolean;
  var handled : boolean; i : byte = 0;
  begin
    handled := false;
    while not handled and (i < self.count) do
      begin
        inc(i);
        handled := self.members[i].handle(msg)
      end;
    handle := handled
  end;

constructor TMorph.Create;
  begin
    inherited Create;
    bounds.x := 0;
    bounds.y := 0;
    bounds.w := 1;
    bounds.h := 1;
    visible := true;
  end;

procedure TMorph.Draw;
  begin
    WriteLn('morph')
  end;

{-- ClockMorph -------------}
type
  TClockMorph = class ( TMorph )
    color : byte;
    constructor Create;
    procedure Render; override;
    function Str:string; override;
  end;

constructor TClockMorph.Create;
  begin
    inherited Create;
    color := $13; { cyan on blue }
  end;

function TClockMorph.Str: string;
  begin
    result := FormatDateTime('MM.DD.YY hh:mm:ssam/pm', Now);
  end;

procedure TClockMorph.Render;
  begin
    cw.cxy( color, bounds.x, bounds.y, self.str )
  end;

{-- stack -------------------}
type
  TInt32Array = GArray<Int32>;
  TStack      = class
    slots : TInt32Array;
    count : byte;
    procedure Push( val : longint );
    function  Pop : longint;
    function  tos : longint;
    function  nos : longint;
    procedure Swap;
    procedure Dup;
    procedure Over;
    procedure Rot;
  end;

procedure TStack.Push( val : longint );
  begin
    slots[count] := val; inc(count)
  end;

function TStack.Pop : longint;
  begin
    Dec(count); Pop := slots[count];
  end;

function TStack.tos : longint; inline;
  begin
    tos := slots[count-1]
  end;

function TStack.nos : longint; inline;
  begin
    nos := slots[count-2]
  end;


procedure TStack.Dup;
  begin
    Push(tos)
  end;

procedure TStack.Swap;
  var tmp : longint;
  begin
    tmp := tos;
    slots[ count-1 ] := nos;
    slots[ count-2 ] := tmp;
  end;

procedure TStack.Over;
  begin
    Push(tos)
  end;

procedure TStack.Rot;
  var tmp : longint;
  begin
    tmp := slots[count-3];
    slots[count-3] := slots[count-2];
    slots[count-2] := slots[count-1];
    slots[count-1] := tmp;
  end;

{-- virtual machine ------------}
type
  OpCode = (opNop, opNot, opXor, opAnd,
            opDup, opDrp, opPsh, opPop,
            opSwp, opRot,
            opFrk, opSpn, opSnd, opYld,
            opAdd, opSub, opMul, opDvm,
            opInc, opDec, opShr, opShl,
            opCmp, opGT,  opLT,  opEq, opIn,
            opJmp, opEls, opRet, opZex,
            opNxt, opGet, opPut );
type
  TMachine  = class( TMorph )
    public
      ibuf, obuf : string; { input/output buffers (255 chars) }
      ip, rp, wp : byte;
      data, addr : TStack;
      memory     : TBytes;
      procedure Update; override;
      procedure Render; override;
      procedure RunOp( op:OpCode );
    end;

procedure TMachine.RunOp( op:OpCode );
  var temp : longint;
  begin
    with data do case op of
      opNop : begin end;
      opNot : push(not pop);
      opXor : push(pop xor pop);
      opAnd : push(pop and pop);
      opDup : dup;
      opDrp : temp := pop;
      opPsh : addr.push(pop);
      opPop : push(addr.pop);
      opSwp : swap;
      opRot : rot;
      opFrk : begin {-- todo: fork --} end;
      opSpn : begin {-- todo: spawn --} end;
      opAdd : push(pop + pop);
      opSub : push(pop - pop);
      opMul : push(pop * pop);
      opDvm :
        begin
          addr.push( tos mod nos );
          push( pop div pop );
          push( addr.pop );
        end;
      opInc : push(succ(pop));
      opDec : push(pred(pop));
      opShl : push(pop shl pop);
      opShr : push(pop shr pop);
      opCmp : begin
                temp := pop - pop;
                if temp > 0 then push(1)
                else if temp < 0 then push(-1)
                else push(0)
              end;
      opGt : if pop > pop then push(-1) else push(0);
      opLt : if pop < pop then push(-1) else push(0);
      opEq : if pop = pop then push(-1) else push(0);
      opIn : begin end;{--todo-- if (pop mod 32) in set32(pop)
                         then push(-1) else push(0); }
      opJmp: ip := pop;
      opEls: if pop = 0 then begin {---todo-- ip:= memory(ip) --} end
                        else inc(ip);
      opRet: ip := addr.pop;
      opZex: if tos = 0 then begin temp := pop; ip := addr.pop end;
      opNxt: if addr.tos = 0
               then begin temp:=pop; temp := addr.pop end
               else begin addr.over; ip := pop end;
      opGet: push(memory[pop]);
      opPut: memory[pop] := pop;
      opSnd: begin end; {-- todo --}
      opYld: begin end; {-- todo --}
    end
  end;

procedure TMachine.Update;
  begin
  end;

procedure TMachine.Render;
  var i, j : integer;
  begin
    for i := 32 to 64 do for j := 8 to 16 do
      cw.cxy( random(8), i, j, 'x' );
  end;

{-- concurrency --------------------}

type
  TActors = GArray<TActor>;
var
  actors : TActors;

procedure launch(this:TActor);
  begin
    actors.append(this);
  end;


{-- event system ---------}
const
  evt_keydn = -25;
  evt_keyup = -26;
  evt_mosdn = -27;
  evt_mosup = -28;
  evt_mosmv = -29;

type
  TEvent  = class (TMessage)
    public
      data : integer;
      constructor Create(etag: integer; e:integer);
    end;

constructor TEvent.Create(etag:integer; e:integer);
  begin
    tag  := etag;
    data := e;
  end;


{-- simple dictionary lookup ----}

type
  TEntry =  class;
  TDict  = class(TObj)
    public
      nextdict : TDict;
      latest   : TEntry;
      constructor Create;
      procedure Define( name : string; value : TObj );
      function Lookup( s : string; var item : TObj ): boolean;
    end;

  TEntry = class
    public
      prev : TEntry;
      name : string[32];
      item : TObj;
    end;

constructor TDict.Create;
  begin
    nextdict := nil;
    latest := nil;
  end;

procedure TDict.Define( name: string; value : TObj );
  var en : TEntry;
  begin
    en := TEntry.Create;
    en.prev := latest;
    en.name := name;
    en.item := value;
    latest := en;
  end;

function TDict.Lookup( s : string; var item : TObj): boolean;
  var en : TEntry; found : boolean;
  begin
    en := latest;
    found := false;
    while Assigned(en) and not found do
      if en.name = s then
        begin
          item  := en.item;
          found := true;
        end
      else
        begin
        end;
    lookup := found;
  end;

{-- interpreter widget ---}

type
  TShellMorph = class( TMorph )
    curpos : byte;
    cmdstr : string;
    vm     : TMachine;
    clock  : TClockMorph;
    words  : TDict;
    constructor Create;
    procedure Invoke( cmd : string );
    procedure Clear;
    function Handle( msg : TMessage ):boolean; override;
    procedure Render; override;
    destructor Destroy; override;
  end;

constructor TShellMorph.Create;
  begin
    inherited Create;
    vm := TMachine.Create; launch(vm);
    clock := TClockMorph.Create;
    clock.bounds.x := 0; clock.bounds.y := 0;
    launch(clock);
    self.Clear;
    words := TDIct.Create;
  end;

procedure TShellMorph.invoke( cmd : string );
  var o : TObj;
  begin
    if words.Lookup(cmd, o) then
      begin
        kvm.fg('g');
        writeln( o.Str );
      end
    else
      begin
        gotoxy(0, maxY); writeln; { to scroll }
        gotoxy(0, maxY-1);
        kvm.fg('r'); write('unknown command: ');
        kvm.fg('Y'); write(cmd); kvm.clreol; writeln;
      end;
    gotoxy(0,0); clreol; { clear top line after the scroll }
  end;

procedure TShellMorph.Clear;
  begin
    cmdstr := '';
    curpos := 1;
  end;

function TShellMorph.Handle( msg : TMessage ) : boolean;
  var ch : char;
  begin
    if msg.tag = evt_keydn then with TEvent(msg) do
      begin
        handle := true;
        ch := chr(data);
        case ch of
          ^C : halt;
          ^H : if length(cmdstr) > 0 then
                 begin
                   SetLength(cmdstr, length(cmdstr)-1);
                   dec(curpos);
                 end
               else pass;
          ^M : begin
                 self.Invoke(cmdstr); self.Clear
               end;
        else
          cmdstr := cmdstr + ch;
          inc(curpos)
        end
      end
    else handle := false;
  end;

procedure TShellMorph.Render;
  begin
    cw.cxy($1e, 0, kvm.MaxY, '> ');
    cw.cxy($1f, 2, kvm.MaxY, cmdstr); clreol;
    kvm.gotoxy( 1 + curpos, kvm.MaxY );
  end;

destructor TShellMorph.Destroy;
  begin
    self.words.Free;
    vm.alive := false;
    clock.alive := false;
    inherited Destroy;
  end;

{-- main program ---------}
var
  focus : TMorph;


procedure Step;
  var i : byte; ch : char; a : TActor; msg:TEvent; numactors : cardinal;
  begin
    // TODO: without this next line (at least on freebsd)
    // it won't readkey. why not!?!
    if not keypressed then sleep(50);

    if keypressed then
      case kbd.ReadKey(ch) of
        #0 : case kbd.ReadKey(ch) of kbd.ESC : halt; end;
      else
        msg := TEvent.Create(evt_keydn, ord(ch));
        if not focus.handle(msg) then pass; {-- todo global keymap --}
        msg.Free;
      end; { case }

    { dispatch to all actors }
    i := 0;
    numActors := actors.length;
    while i < actors.length do
      begin
        a := actors[ i ];
        if a.active then begin
          a.Update;
          if a.alive then inc(i)
          else begin
            Dec(numActors);
            a.Free;
            actors[ i ] := actors[ numActors ];
            actors[ numActors ] := nil;
          end
        end else inc(i) { was inactive, skip over for now }
      end;
    actors.length := numActors;
  end;

procedure Draw;
  var i:cardinal;
  begin
    for i := 0 to actors.length - 1 do
      if actors[i].Visible then actors[i].Render
  end;

function Done : Boolean;
  begin
    result := actors.length = 0;
  end;

initialization

  kvm.ClrScr;
  actors := TActors.Create;
  focus := TShellMorph.Create;
  launch(focus);

finalization

  cw.cwriteln( '|w|!k' );
  kvm.ClrScr;

end.
