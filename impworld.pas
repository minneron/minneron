{$i xpc.inc}{$mode delphi}{$h+}
unit impworld;
interface uses
  xpc,      // pass, general compatability
  mnml,
  cli,
  arrays,
  vorunati,
  sysutils, // AppendStr, FormatDateTime
  variants,
  kvm,      // fg, gotoxy, clreol
  cw,       // cxy(color, x, y, string)
  num,      // n2s
  kbd;      // keypressed/readkey

type
  IMessage = interface
    function GetTag : integer;
    function GetData : variant;
    property tag : integer read GetTag;
    property data : variant read GetData;
  end;
  IActor = interface (IVorTask)
    function Send( msg : IMessage ):boolean;
    function GetVisible: boolean;
    function GetExists: boolean;
    function GetActive: boolean;
    function GetAlive: boolean;
    property visible: boolean read GetVisible;
    property exists:boolean read GetExists;
    property active:boolean read GetActive;
    property alive:boolean read GetAlive;
    procedure Halt;
    procedure Draw;
    function ToString : String;
  end;
  IGroup = interface (IActor)
    procedure Add( child : IActor );
  end;
  IMorph = interface (IGroup)
  end;

const
  cmd_quit = -1;
  cmd_step = -2;
  cmd_draw = -3;
  cmd_hide = -4;
  cmd_show = -5;
  cmd_help = -6;
  evt_keych = -25;
//  evt_keyup = -26;
//  evt_mosdn = -27;
//  evt_mosup = -28;
//  evt_mosmv = -29;

type
  TTagged = class(TInterfacedObject, IMessage)
    protected
      _tag : integer;
      _data : variant;
    public
      function GetTag : integer;
      function GetData : variant;
      property tag : integer read GetTag;
      property data : variant read GetData;
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

  TypeKind = ( tkSimple, tkTkUnion, tkFunction, tkSchema );
  TFieldDef = class;

  TTypeDef  = class
    public
      size : Word;
      kind : TypeKind;
      numFields : byte;
      first : TFieldDef;
    end;

  TFieldDef = class
    public
      next : TFieldDef;
      name : TSymbol;
    end;

  TTuple = class
    public
      meta : TTypeDef;
      data : TBytes;
    end;

  TMessage = class (TTagged, IMessage)
    sym : TSymbol;
    sender: IActor;
    args: TTuple;
  end;

  TEvent  = class (TMessage)
    public
      constructor Create(etag: integer; edata:variant);
    end;


  TActor = class (TVorunati<IMessage,void>, IActor)
    _active,           { wants update() }
    _alive,            { exists but not alive triggers gc }
    _visible,          { to allow hide/show }
    _exists : boolean; { turn off everything at once }
    constructor Create;
    procedure step; override;
    procedure Draw; virtual;
    procedure Halt;
    function Send( msg : IMessage ):boolean; override;
    function GetVisible : boolean;
    function GetExists : boolean;
    function GetActive : boolean;
    function GetAlive : boolean;
    function ToString : string; override;
  end;

  TGroup = class (TActor, IGroup)
    children : GArray<IActor>;
    constructor Create;
    procedure Add( a : IActor );
    function Send( msg : IMessage ):boolean; override;
  end;


  Point = object
    x, y : integer;
  end;

  Quad = object( Point )
    w, h : integer;
    function x2 : integer;
    function y2 : integer;
  end;

  TMorph = class (TGroup, IMorph)
    bounds : Quad;
    colors : word; { foreground and background }
    constructor Create;
    procedure Draw; override;
    function Send( msg : IMessage ):boolean; override;
    function OnKeyPress( ch :  char ):boolean; virtual;
  end;

  TWorld = class (TMorph)
    public
      manageKeyboard : boolean;
      constructor Create;
      procedure Step; override;
      procedure Draw; override;
      function Done : boolean;
    end;


var
  world	: TWorld;
  focus	: IMorph;


implementation

{-- fixed-size data blocks --------}
type
  Block = array[ 0 .. 1023 ] of byte;
  Drive = file of block;

{-- variable-sized buffers --------}
type
  TBytes = GArray<Byte>;


{-- display -----------------------}

function Quad.x2 : integer;
  begin
    x2 := x + w
  end;

function Quad.y2 : integer;
  begin
    y2 := y + h
  end;

{-- tagged data types -------------}

function TTagged.GetTag : integer;
  begin
    result := _tag
  end;

function TTagged.GetData : variant;
  begin
    result := _data
  end;



constructor TActor.Create;
  begin
    inherited Create;
    _alive  := true;
    _active := true;
    _exists := true;
    _visible := false;
  end;

function TActor.ToString;
  begin
    WriteStr(result, 'Actor(', self.ClassName, ')');
  end;

procedure TActor.Draw;
  begin
  end;

procedure TActor.Step;
  begin
  end;

procedure TActor.Halt;
  begin
    _alive := false;
  end;

function TActor.Send( msg : IMessage ):boolean;
  begin
    result := true;
    case msg.tag of
      cmd_quit: _active := false;
      cmd_draw: Draw;
      cmd_step: Step;
      else result := false
    end
  end;

function TActor.GetVisible; begin  result := _visible end;
function TActor.GetExists;  begin  result := _exists end;
function TActor.GetActive;  begin  result := _active end;
function TActor.GetAlive;   begin  result := _alive end;



constructor TGroup.Create;
  begin
    self.children := (GArray<IActor>).Create;
  end;

procedure TGroup.Add( a : IActor );
  begin
    children.append(a);
  end;

function TGroup.Send( msg: IMessage ):boolean;
  var handled : boolean; i : byte = 0;
  begin
    handled := false;
    while not handled and (i < self.children.length) do
      begin
	handled := self.children[i].send(msg);
        inc(i);
      end;
    result := handled
  end;

constructor TMorph.Create;
  begin
    inherited Create;
    bounds.x := 0;
    bounds.y := 0;
    bounds.w := 1;
    bounds.h := 1;
    _visible := true;
  end;

procedure TMorph.Draw;
  begin
    WriteLn('morph')
  end;

function TMorph.Send( msg : IMessage ):boolean;
  begin
    result := inherited;
    if not result then
      case msg.tag of
        evt_keych : result := self.OnKeyPress( msg.data );
        //  evt_keyup = -26;
        //  evt_mosdn = -27;
        //  evt_mosup = -28;
        //  evt_mosmv = -29;
        else result := false;
      end;
  end; { TMorph.Send }

function TMorph.OnKeyPress( ch : char ):boolean;
  begin
    result := false
  end;


{-- ClockMorph -------------}
type
  TClockMorph = class ( TMorph )
    dirty : boolean;
    color, last, text : string;
    constructor Create;
    procedure Draw; override;
    function ToString:string; override;
  end;

constructor TClockMorph.Create;
  begin
    inherited Create;
    color := '|!b|B';
    dirty := true;
  end;

function TClockMorph.ToString: string;
  begin
    result := FormatDateTime('MM.DD.YY HH:mm', Now);
  end;

procedure TClockMorph.Draw;
  begin
    text := self.ToString;
    if text <> last then dirty := true;
    if dirty then cwritexy( bounds.x, bounds.y, self.color + text );
    dirty := false;
    last := text;
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
      procedure Step; override;
      procedure Draw; override;
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

procedure TMachine.Step;
  begin
  end;

procedure TMachine.Draw;
  var i, j : integer;
  begin
    for i := 32 to 64 do for j := 8 to 16 do
      cw.cxy( random(8), i, j, 'x' );
  end;

{-- concurrency --------------------}



{-- event system ---------}

constructor TEvent.Create(etag:integer; edata:variant);
  begin
    inherited Create;
    _tag  := etag;
    _data := edata;
  end;


{-- simple dictionary lookup ----}

type
  TEntry =  class;
  TDict  = class
    public
      nextdict : TDict;
      latest   : TEntry;
      constructor Create;
      procedure Define( name : string; value : TObject);
      function Lookup( s : string; var item : TObject ): boolean;
    end;

  TEntry = class
    public
      prev : TEntry;
      name : string[32];
      item : TObject;
    end;

constructor TDict.Create;
  begin
    nextdict := nil;
    latest := nil;
  end;

procedure TDict.Define( name: string; value : TObject);
  var en : TEntry;
  begin
    en := TEntry.Create;
    en.prev := latest;
    en.name := name;
    en.item := value;
    latest := en;
  end;

function TDict.Lookup( s : string; var item : TObject): boolean;
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
    function OnKeyPress( ch : char ):boolean; override;
    procedure Draw; override;
    destructor Destroy; override;
  end;

constructor TShellMorph.Create;
  begin
    inherited Create;
    clock := TClockMorph.Create;
    clock.bounds.x := 0; clock.bounds.y := 0;
    world.add(clock);
    self.Clear;
    words := TDIct.Create;
  end;

procedure TShellMorph.invoke( cmd : string );
  var o : TObject;
  begin
    if words.Lookup(cmd, o) then
      begin
        kvm.fg('g');
        writeln( o.ToString );
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

function TShellMorph.OnKeyPress( ch : char ) : boolean;
  begin
    result := true;
    case ch of
      ^C : _alive := false;
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
  end;

procedure TShellMorph.Draw;
  begin
    cw.cxy($1e, 0, kvm.MaxY, '> ');
    cw.cxy($1f, 2, kvm.MaxY, cmdstr); clreol;
    kvm.gotoxy( 1 + curpos, kvm.MaxY );
  end;

destructor TShellMorph.Destroy;
  begin
    self.words.Free;
    vm.halt;
    clock.halt;
    inherited Destroy;
  end;

{-- main program ---------}


procedure HandleKeys;
  var ch : char; msg:IMessage;
  begin
    // TODO: without this next line (at least on freebsd)
    // it won't readkey. why not!?!
    if not keypressed then sleep(5);
    if keypressed then
      case kbd.ReadKey(ch) of
	^C : halt;
      else
        msg := TEvent.Create(evt_keych, ch);
	if assigned(focus) and not focus.send(msg) then
	  pass; {--  TODO: global keymap --}
      end; { case }
  end;


constructor TWorld.Create;
  begin
    inherited;
    manageKeyboard := true;
  end;

procedure TWorld.Step;
  var a : IActor;
  begin
    if ManageKeyboard then HandleKeys;
    for a in self.children do
      if a.active then a.Step;
    Draw;
  end;

procedure TWorld.Draw;
  var a : IActor;
  begin
    for a in self.children do
      if a.Visible then a.Draw;
  end;

function TWorld.Done : Boolean;
  begin
    result := children.length = 0;
  end;

var clock : TClockMorph;
initialization
  clock := TClockMorph.Create;
  clock.bounds.x := kvm.width - 25;
  kvm.ClrScr;
  world	:= TWorld.Create;
  world.add(clock);
  focus	:= world;
  launch(world);

finalization
  //world.Free; { nope. this will crash if we launch(world).}
  cw.cwriteln( '|w|!k' );
  kvm.ClrScr;
end.
