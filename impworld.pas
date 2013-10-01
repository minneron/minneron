{$i xpc.inc}{$mode objfpc}{$h+}
unit world;
uses
  xpc,      // pass, general compatability
  sysutils, // AppendStr, FormatDateTime
  kvm,      // fg, gotoxy, clreol
  cw,       // cxy(color, x, y, string)
  num,      // n2s
  kbd;      // keypressed/readkey


{-- dynamic types -----------------}
type
  Obj = ^BaseObj; { base type for all objects }
  BaseObj = object
    constructor Create;
    function Str: string; virtual;
    destructor Destroy; virtual;
  end;

constructor BaseObj.Create;
  begin
  end;

function BaseObj.Str : string;
  begin
    Str := '$';
  end;

destructor BaseObj.Destroy;
  begin
  end;

{-- fixed-size data blocks --------}
type
  Block = array[ 0 .. 1023 ] of byte;
  Drive = file of block;

{-- variable-sized buffers --------}
type
  Bytes  = array[ 0..0 ] of byte;
  Buffer = ^Bytes;


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
  Tagged  = ^TaggedObj;
  Symbol  = ^SymbolObj;   { for symbol/lookup tables }
  Token   = ^TokenObj;    { for parsing long texts }

  TaggedObj = object(BaseObj)
    tag : longint;
  end;

  SymbolObj = object(TaggedObj)
    name : string[32];
  end;

  TokenObj = object(TaggedObj)
    sym : Symbol;
    line, column, span : longint;
  end;

{-- Tuples ---------------------------}
type
  TypeDef  = ^TypeDefObj;
  FieldDef = ^FieldDefObj;
  Tuple    = ^TupleObj;    { generic record/struct }

  TypeKind = ( tkSimple, tkTkUnion, tkFunction, tkSchema );

  TypeDefObj  = object(BaseObj)
    size : Word;
    kind : TypeKind;
    numFields : byte;
    first : FieldDef;
  end;

  FieldDefObj = object(BaseObj)
    next : FieldDef;
    name : Symbol;
  end;

  TupleObj = object(TokenObj)
    meta : TypeDef;
    data : Buffer;
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
  Actor = ^ActorObj;
  Group = ^GroupObj;
  Morph = ^MorphObj;
  Message  = ^MessageObj;  { for message passing }

  ActorObj = object(BaseObj)
    active,           { wants update() }
    alive,            { exists but not alive triggers gc }
    visible,          { to allow hide/show }
    exists : boolean; { turn off everything at once }
    constructor Create;
    procedure Update; virtual;
    procedure Render; virtual;
    function Handle( msg : Message ):boolean; virtual;
  end;

  GroupObj = object( ActorObj )
    members : array[ 0 .. kGroupMaxSlot ] of Actor;
    count   : byte;
    constructor Create;
    procedure Add( a : Actor );
    function Handle( msg : Message ):boolean; virtual;
  end;

  MorphObj = object( GroupObj )
    bounds : Quad;
    colors : word; { foreground and background }
    constructor Create;
    procedure Draw; virtual;
  end;

  MessageObj = object(TaggedObj)
    sym : Symbol;
    sender: Actor;
    args: Tuple;
  end;


constructor ActorObj.Create;
  begin
    alive  := true;
    active := true;
    exists := true;
    visible := false;
  end;

procedure ActorObj.Render;
  begin
  end;

procedure ActorObj.Update;
  begin
  end;

function ActorObj.Handle( msg : Message ):boolean;
  begin
    Handle := true;
    case msg^.tag of
      cmd_quit: active := false;
      cmd_draw: Render;
      cmd_step: Update;
      else handle := false
    end
  end;

constructor GroupObj.Create;
  begin
    self.count := 0;
  end;

procedure GroupObj.Add( a : Actor );
  begin
    if self.count < kGroupMaxSlot then
      begin
        self.members[count] := a;
        inc(self.count);
      end
  end;

function GroupObj.Handle( msg: Message ):boolean;
  var handled : boolean; i : byte = 0;
  begin
    handled := false;
    while not handled and (i < self.count) do
      begin
        inc(i);
        handled := self.members[i]^.handle(msg)
      end;
    handle := handled
  end;

constructor MorphObj.Create;
  begin
    ActorObj.Create;
    bounds.x := 0;
    bounds.y := 0;
    bounds.w := 1;
    bounds.h := 1;
    visible := true;
  end;

procedure MorphObj.Draw;
  begin
    WriteLn('morph')
  end;

{-- ClockMorph -------------}
type
  ClockMorph = ^ClockObj;
  ClockObj = object( MorphObj )
    color : byte;
    constructor Create;
    procedure Render; virtual; { override }
    function Str:string; virtual; { override }
  end;

constructor ClockObj.Create;
  begin
    inherited Create;
    color := $13; { cyan on blue }
  end;

function ClockObj.Str: string;
  begin
    result := FormatDateTime('MM.DD.YY hh:mm:ssam/pm', Now);
  end;

procedure ClockObj.Render;
  begin
    cw.cxy( color, bounds.x, bounds.y, self.str )
  end;

{-- stack -------------------}
type
  Stack    = ^StackObj;
  StackObj = object
    slots : array[ 0..254 ] of longint;
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

procedure StackObj.Push( val : longint );
  begin
    slots[count] := val; inc(count)
  end;

function StackObj.Pop : longint;
  begin
    Dec(count); Pop := slots[count];
  end;

function StackObj.tos : longint; inline;
  begin
    tos := slots[count-1]
  end;

function StackObj.nos : longint; inline;
  begin
    nos := slots[count-2]
  end;


procedure StackObj.Dup;
  begin
    Push(tos)
  end;

procedure StackObj.Swap;
  var tmp : longint;
  begin
    tmp := tos;
    slots[ count-1 ] := nos;
    slots[ count-2 ] := tmp;
  end;

procedure StackObj.Over;
  begin
    Push(tos)
  end;

procedure StackObj.Rot;
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
  Machine  = ^MachineObj;
  MachineObj = object( MorphObj )
    ibuf, obuf : string; { input/output buffers (255 chars) }
    ip, rp, wp : byte;
    data, addr : stack;
    memory     : buffer;
    procedure Update; virtual; { override; }
    procedure Render; virtual; { override; }
    procedure RunOp( op:OpCode );
  end;

procedure MachineObj.RunOp( op:OpCode );
  var temp : longint;
  begin
    with data^ do case op of
      opNop : begin end;
      opNot : push(not pop);
      opXor : push(pop xor pop);
      opAnd : push(pop and pop);
      opDup : dup;
      opDrp : temp := pop;
      opPsh : addr^.push(pop);
      opPop : push(addr^.pop);
      opSwp : swap;
      opRot : rot;
      opFrk : begin {-- todo: fork --} end;
      opSpn : begin {-- todo: spawn --} end;
      opAdd : push(pop + pop);
      opSub : push(pop - pop);
      opMul : push(pop * pop);
      opDvm :
        begin
          addr^.push( tos mod nos );
          push( pop div pop );
          push( addr^.pop );
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
      opEls: if pop = 0 then begin {---todo-- ip:= memory^(ip) --} end
                        else inc(ip);
      opRet: ip := addr^.pop;
      opZex: if tos = 0 then begin temp := pop; ip := addr^.pop end;
      opNxt: if addr^.tos = 0
               then begin temp:=pop; temp := addr^.pop end
               else begin addr^.over; ip := pop end;
      opGet: push(memory^[pop]);
      opPut: memory^[pop] := pop;
      opSnd: begin end; {-- todo --}
      opYld: begin end; {-- todo --}
    end
  end;

procedure MachineObj.Update;
  begin
  end;

procedure MachineObj.Render;
  var i, j : integer;
  begin
    for i := 32 to 64 do for j := 8 to 16 do
      cw.cxy( random(8), i, j, 'x' );
  end;

{-- concurrency --------------------}

var actors : array[ 0 .. 254 ] of Actor;
    tokens : array[ 0 .. 254 ] of Token;
    numActors : byte;
    numTokens : byte;

procedure Register(this:Actor);
  begin
    if numActors < 255 then
      begin
        actors[numActors] := this;
        inc(numActors);
      end
    else
      begin
        Dispose(this, Destroy);
        Writeln('out of actor slots');
      end
  end;


{-- event system ---------}
const
  evt_keydn = -25;
  evt_keyup = -26;
  evt_mosdn = -27;
  evt_mosup = -28;
  evt_mosmv = -29;

type
  Event    = ^EventObj;
  EventObj = object(MessageObj)
    data : integer;
    constructor Create(etag: integer; e:integer);
  end;

constructor EventObj.Create(etag:integer; e:integer);
  begin
    tag  := etag;
    data := e;
  end;


{-- simple dictionary lookup ----}

type
  Dict  = ^DictObj;
  Entry = ^EntryObj;

  DictObj = object(BaseObj)
    nextdict : Dict;
    latest   : Entry;
    constructor Create;
    procedure Define( name : string; value : obj );
    function Lookup( s : string; var item : Obj ): boolean;
  end;

  EntryObj = object
    prev : Entry;
    name : string[32];
    item : obj;
  end;

constructor DictObj.Create;
  begin
    nextdict := nil;
    latest := nil;
  end;

procedure DictObj.Define( name: string; value : Obj );
  var en : Entry;
  begin
    en := New(Entry);
    en^.prev := latest;
    en^.name := name;
    en^.item := value;
    latest := en;
  end;

function DictObj.Lookup( s : string; var item : Obj): boolean;
  var en : Entry; found : boolean;
  begin
    en := latest;
    found := false;
    while Assigned(en) and not found do
      if en^.name = s then
        begin
          item  := en^.item;
          found := true;
        end
      else
        begin
        end;
    lookup := found;
  end;

{-- interpreter widget ---}

type
  Shell = ^ShellObj;
  ShellObj = object( MorphObj )
    curpos : byte;
    cmdstr : string;
    vm     : Machine;
    clock  : ClockMorph;
    words  : Dict;
    constructor Create;
    procedure Invoke( cmd : string );
    procedure Clear;
    function Handle( msg : Message ):boolean; virtual; { override }
    procedure Render; virtual;   { override }
    destructor Destroy; virtual; { override }
  end;

constructor ShellObj.Create;
  begin
    inherited Create;
    vm := New(Machine, Create); register(vm);
    clock := New(ClockMorph, Create);
    clock^.bounds.x := 0; clock^.bounds.y := 0;
    Register(clock);
    self.Clear;
    words := New(Dict, Create);
  end;

procedure ShellObj.invoke( cmd : string );
  var o : obj;
  begin

    if words^.Lookup(cmd, o) then
      begin
        kvm.fg('g');
        writeln( o^.Str );
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

procedure ShellObj.Clear;
  begin
    cmdstr := '';
    curpos := 1;
  end;

function ShellObj.Handle( msg : Message ) : boolean;
  var ch : char;
  begin
    if msg^.tag = evt_keydn then with Event(msg)^ do
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

procedure ShellObj.Render;
  begin
    cw.cxy($1e, 0, kvm.MaxY, '> ');
    cw.cxy($1f, 2, kvm.MaxY, cmdstr); clreol;
    kvm.gotoxy( 1 + curpos, kvm.MaxY );
  end;

destructor ShellObj.Destroy;
  begin
    dispose(self.words, Destroy);
    vm^.alive := false;
    clock^.alive := false;
    inherited Destroy;
  end;

{-- main program ---------}
var
  focus : Morph;

procedure Create;
  begin
    kvm.ClrScr;
    numActors := 0; numTokens := 0;
    focus := New(Shell, Create);
    Register(focus);
  end;


procedure Update;
  var i : byte; ch : char; a : Actor; msg:Event;
  begin

    // TODO: without this next line (at least on freebsd)
    // it won't readkey. why not!?!
    if not keypressed then sleep(50);

    if keypressed then
      case kbd.ReadKey(ch) of
        #0 : case kbd.ReadKey(ch) of kbd.ESC : halt; end;
      else
        msg := New(Event, Create(evt_keydn, ord(ch)));
        if not focus^.handle(msg) then pass; {-- todo global keymap --}
        Dispose(msg, Destroy)
      end; { case }

    { dispatch to all actors }
    i := 0;
    while i < numActors do
      begin
        a := actors[ i ];
        if a^.active then begin
          a^.Update;
          if a^.alive then inc(i)
          else begin
            Dec(numActors);
            Dispose(a, Destroy);
            actors[ i ] := Actors[ numActors ];
            actors[ numActors ] := nil;
          end
        end else inc(i) { was inactive, skip over for now }
      end;
  end;

procedure Render;
  var i : byte;
  begin
    if numActors > 0 then
      for i := 0 to numActors-1 do
        if actors[ i ]^.Visible then actors[ i ]^.Render
  end;

procedure Destroy;
  begin
    clrscr; writeln('terminated cleanly.');
  end;

begin
  Create;
  repeat
    Update;
    Render;
  until numActors = 0;
  Destroy;
end.
