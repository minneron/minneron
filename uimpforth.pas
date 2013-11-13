{$mode delphi}
unit uimpforth;
interface uses gqueue, classes, custapp;

const
  tokLen = 15;
type
  TTokStr   = string[toklen];
  address   = 0..65535;
  cardinal  = address;
  integer   = int32;
  thunk	    = procedure of object;

  TCmdQueue = TQueue<integer>;

  TImpForth = class (TComponent)
    protected
      msg : string[ 255 ]; // input and output buffers
      refill,       { repopulates 'src' }
      respond : thunk;
    public

      constructor Create(aOwner : TComponent); override;

      {-- main public inteface --}
      procedure AddOp( const iden : TTokStr; code : thunk );
      procedure Send( s : string );
      procedure Eval;

      {-- these might wind up hidden... --}
      procedure pushdat( x:integer );
      procedure pushret( x:integer );
      procedure pass;
      procedure gosub;
      procedure step;
      procedure execute;
      procedure interpret;
      function lookup : boolean;
      procedure notfound;

      function IsNumber : boolean;
    end;

  TImpShellApp = class (TCustomApplication)
    public
      src  : string[255];
      imp  : TImpForth;
      procedure Initialize; override;
      procedure AddOp( const iden : TTokStr; code : thunk );
      procedure DoRun; override;
      procedure Welcome; virtual;
      procedure GetNext; virtual;
      procedure Refill; virtual;
      procedure Respond; virtual;
    end;


const
  prim	 = 0;
  quit	 : integer = -1;

type
  TWord	 = record
    // prev : address;
    word : TTokStr;
    code : address;
    data : cardinal
  end;

var
  brand	 : string  = 'ImpForth';
  verMaj : byte  = 0;
  verMin : byte = 1;



{-- the virtual machine ---------------------------------------}

var
  ram : array[ address  ] of integer;
  ops : array[ 0 .. 255 ] of thunk;

  sp : integer  = 0;
  rp : integer  = 1024;
  ip : cardinal = 0;

  inp   : byte;             // pointer into src
  token : string[ tokLen ]; // temp buffer for token
  which : cardinal;         // address of last looked-up word

  // for now, we're just using a simple array for the dictionary
  words  : array[ 0 .. 1023 ] of TWord;
  numwds : cardinal = 0;
  numops : cardinal = 0;

implementation

constructor TImpForth.Create(aOwner : TComponent);
  var i : integer;
  begin
    for i := high(ops) downto low(ops) do ops[i] := pass;
    for i := high(ram) downto low(ram) do ram[i] := 0;
  end;


{-- direct stack access from pascal ---------------------------}

procedure TImpForth.pushdat( x:integer );
begin
  dec(sp); ram[sp] := x;
end;

procedure TImpForth.pushret( x:integer );
begin
  dec(rp); ram[rp] := x;
end;


{-- primitive control flow  -----------------------------------}

procedure TImpForth.pass;
begin
end;

procedure TImpForth.gosub;
begin
  pushret( ip ); ip := ram[ip];
end;


{-- inner interpreter -----------------------------------------}

procedure TImpForth.step;
var op : integer;
begin
  op := ram[ip];
  case abs(op) of
    -1 : ops[abs(op)];
     0 : { do nothing } ;
    +1 : gosub;
  end;
  inc(ip);
end;

procedure TImpForth.execute;
begin
  repeat step until ram[ip] = quit
end;

procedure TImpForth.interpret;
begin
  with words[which] do
    if code = prim then ops[data] { run primitive directly }
    else begin                    { run from dictionary }
      pushdat(data); ip := code;
      gosub; execute;
    end;
end;

{-- outer interpreter -----------------------------------------}

function TImpForth.lookup : boolean;
  var found: boolean = false;
begin
  which := numwds; found := false;
  while (which > 0) and not found do begin
    dec(which);
    found := words[which].word = token;
  end;
  lookup := found;
end;

procedure TImpForth.notfound;
begin
  msg := token + '?';
end;


{-- default tokenizer -----------------------------------------}

function TImpForth.IsNumber : boolean;
begin
  {  todo }
  result := false;
end;



{-- dictionary-handlers ---------------------------------------}

procedure TImpForth.AddOp( const iden : TTokStr; code : thunk );
begin
  ops[numops] := code;
  with words[numwds] do begin
    // prev := ... ;
    word := iden;
    code := prim;
    data := numops;
  end;
  inc(numwds); inc(numops);
end;

procedure TImpForth.Send( s : string );
  begin
  end;

procedure TImpForth.Eval;
  begin
    if Lookup then Interpret
    else if IsNumber then begin {TODO } end
    else NotFound;
  end;

{-- TImpShellApp ----------------------------------------------}

procedure TImpShellApp.Initialize;
begin
  imp := TImpForth.Create(self);
end;

procedure TImpShellApp.AddOp( const iden : TTokStr; code : thunk );
begin
  imp.AddOp(iden, code);
end;

procedure TImpShellApp.DoRun;
begin
  Welcome;
  repeat
    GetNext;
    imp.Eval;
    Respond
  until terminated;
end;

procedure TImpShellApp.Welcome;
begin
  writeln(brand, ' ', verMaj, '.', verMin);
end;

procedure TImpShellApp.GetNext;
  function ignorable : boolean;
  begin
    ignorable := (inp > length(src)) or (src[inp] <= ' ')
  end;
begin
  while ignorable do
    if inp <= length(src) then inc(inp)
    else begin refill; inp := 0; end;
  token := '';
  repeat token := token + src[inp]; inc(inp)
  until ignorable or (inp > length(src));
end;

procedure TImpShellApp.Refill;
begin
  repeat write('> '); readln(src)
  until length(src) > 0;
end;

procedure TImpShellApp.Respond;
begin
  if imp.msg <> '' then writeln(imp.msg);
  imp.msg := '';
end;

begin
end.
