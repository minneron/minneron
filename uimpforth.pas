{$mode delphi}
unit uimpforth;
interface uses gqueue, classes, custapp;

const
  kTokLen = 15;
type
  address   = 0..65535;
  cardinal  = address;
  integer   = int32;
  TThunk    = procedure of object;

  TCmdQueue = TQueue<integer>;

  TInnerVM = class  (TComponent)
    protected
      sp, rp : integer;
      ip  : cardinal;
      ram : array[ address  ] of integer;
      ops : array[ 0 .. 255 ] of TThunk;
      numops : cardinal;
    public
      constructor Create(aOwner : TComponent); override;
      function AddOp( thunk : TThunk ): integer;
      procedure PushDat( x:integer );
      procedure PushRet( x:integer );
      procedure Pass;
      procedure GoSub;
      procedure Step;
      procedure Execute;
    end;

  TTokStr   = string[kToklen];

  TWord	 = record
    // prev : address;
    word : TTokStr;
    code : address;
    data : cardinal
  end;

  TImpForth = class (TComponent)
    protected
      vm  : TInnerVM;
      msg : string[ 255 ]; // input and output buffers
      tok : TTokStr;
      which : cardinal;    // address of last looked-up word
      // for now, we're just using a simple array for the dictionary
      words  : array[ 0 .. 1023 ] of TWord;
      numwds : cardinal;
    public
      constructor Create(aOwner : TComponent); override;

      {-- main public inteface --}
      procedure AddOp( const iden : TTokStr; thunk : TThunk );
      procedure Send( s : string );
      procedure Eval(const token : TTokStr);
      procedure Interpret;

      function Lookup  : boolean;
      function IsNumber : boolean;
      procedure NotFound;
    end;

  TImpShellApp = class (TCustomApplication)
    public
      src : string[255];
      inp : byte;             // pointer into src
      imp : TImpForth;
      procedure Initialize; override;
      procedure AddOp( const iden : TTokStr; thunk : TThunk );
      procedure DoRun; override;
      procedure Welcome; virtual;
      function NextToken : TTokStr; virtual;
      procedure Refill; virtual;
      procedure Respond; virtual;
    end;


const
  prim	 = 0;
  quit	 : integer = -1;
  brand	 : string  = 'ImpForth';
  verMaj : byte  = 0;
  verMin : byte = 1;


implementation

constructor TInnerVM.Create(aOwner : TComponent);
  var i : integer;
  begin
    inherited Create(aOwner);
    sp := 0; rp := 1024; ip := 0;
    numops := 0;
    for i := high(ops) downto low(ops) do ops[i] := pass;
    for i := high(ram) downto low(ram) do ram[i] := 0;
  end;

constructor TImpForth.Create(aOwner : TComponent);
  begin
    inherited Create(aOwner);
    vm := TInnerVM.Create(self);
    numwds := 0;
  end;


{-- direct stack access from pascal ---------------------------}

function TInnerVM.AddOp( thunk : TThunk ) : integer;
  begin
    result := numops;
    ops[numops] := thunk;
    inc(numops);
  end;

procedure TInnerVM.pushdat( x:integer );
  begin
    dec(sp); ram[sp] := x;
  end;

procedure TInnerVM.pushret( x:integer );
  begin
    dec(rp); ram[rp] := x;
  end;


{-- primitive control flow  -----------------------------------}

procedure TInnerVM.pass;
  begin
  end;

procedure TInnerVM.gosub;
  begin
    pushret( ip ); ip := ram[ip];
  end;


{-- inner interpreter -----------------------------------------}

procedure TInnerVM.Step;
  var op : integer;
  begin
    op := ram[ip];
    case abs(op) of
      -1 : ops[abs(op)];
      0 : { do nothing } ;
      +1 : GoSub;
    end;
    inc(ip);
  end;

procedure TInnerVM.Execute;
  begin
    repeat step until ram[ip] = quit
  end;


{-- outer interpreter -----------------------------------------}

procedure TImpForth.Interpret;
  begin
    with words[which] do
      if code = prim then vm.ops[data] { run primitive directly }
      else begin                    { run from dictionary }
        vm.pushdat(data); vm.ip := code;
        vm.gosub; vm.execute;
      end;
  end;

function TImpForth.LookUp : boolean;
  var found: boolean = false;
  begin
    which := numwds; found := false;
    while (which > 0) and not found do begin
      dec(which);
      found := words[which].word = tok;
    end;
    result := found;
  end;

procedure TImpForth.NotFound;
  begin
    msg := tok + '?';
  end;


{-- default tokenizer -----------------------------------------}

function TImpForth.IsNumber : boolean;
  begin
    {  todo }
    result := false;
  end;



{-- dictionary-handlers ---------------------------------------}

procedure TImpForth.AddOp( const iden : TTokStr; thunk : TThunk );
  var id : integer;
  begin
    id := vm.AddOp(thunk);
    with words[numwds] do begin
      word := iden;
      code := prim;
      data := id;
    end;
    inc(numwds);
  end;

procedure TImpForth.Send( s : string );
  begin
    { TODO}
  end;

procedure TImpForth.Eval(const token : TTokStr);
  begin
    tok := token;
    if Lookup then Interpret
    else if IsNumber then begin {TODO } end
    else NotFound;
  end;

{-- TImpShellApp ----------------------------------------------}

procedure TImpShellApp.Initialize;
  begin
    imp := TImpForth.Create(self);
  end;

procedure TImpShellApp.AddOp( const iden : TTokStr; thunk : TThunk );
  begin
    imp.AddOp(iden, thunk);
  end;

procedure TImpShellApp.DoRun;
  begin
    Welcome;
    repeat
      imp.Eval(NextToken);
      Respond
    until terminated;
  end;

procedure TImpShellApp.Welcome;
  begin
    writeln(brand, ' ', verMaj, '.', verMin);
  end;

function TImpShellApp.NextToken : TTokStr;
  function ignorable : boolean;
  begin
    ignorable := (inp > length(src)) or (src[inp] <= ' ')
  end;
begin
  while ignorable do
    if inp <= length(src) then inc(inp)
    else begin refill; inp := 0; end;
  result := '';
  repeat result := result + src[inp]; inc(inp)
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
