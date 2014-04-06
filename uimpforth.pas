
{$mode delphiunicode}
unit uimpforth;
interface uses xpc, gqueue, classes, sysutils;

const
  kTokLen = 15;
  prim	 = 0;
  quit	 : integer = -1;
  brand	 = 'ImpForth';
  verMaj : byte  = 0;
  verMin : byte = 1;

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

  TTokStr   = TStr;

  TWord	 = record
    // prev : address;
    word : TTokStr;
    code : address;
    data : cardinal
  end;

  TImpForth = class (TComponent)
    protected
      vm  : TInnerVM;
      tok : TTokStr;
      which : cardinal;    // address of last looked-up word
      // for now, we're just using a simple array for the dictionary
      words  : array[ 0 .. 1023 ] of TWord;
      numwds : cardinal;
      src : TStr;
    public
      NeedsInput, HasOutput : boolean;
      msg : TStr; // input and output buffers

      constructor Create(aOwner : TComponent); override;

      {-- main public inteface --}
      procedure AddOp( const iden : TTokStr; thunk : TThunk );
      procedure Send( s : string );
      procedure Eval( const token : TTokStr );
      procedure EvalNextToken;
      procedure Interpret;

      function NextToken : TTokStr; virtual;
      function Lookup  : boolean;
      function IsNumber : boolean;
      procedure NotFound;
    end;


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

procedure TImpForth.Send( s : TStr );
  begin
    self.src += s;
    self.NeedsInput := false;
  end;

function TImpForth.NextToken : TTokStr;
  var inp : cardinal = 1; // input pointer
  function OutsideToken : boolean;
    begin
      result := (inp > length(src)) or (src[inp] <= ' ')
    end;
  function InsideString : boolean;
    begin
      result := (inp <= length(src))
    end;
  begin
    self.tok := '';
    // skip whitespace:
    while InsideString and OutsideToken do inc(inp);
    if InsideString then
      begin // consume the next token
        repeat
          self.tok += src[inp];
          inc(inp)
	until OutsideToken;
	//  TODO: implement rightstr, etc for utf-8
        self.src := a2u(RightStr(u2a(src), length(src) - inp));
      end
    else self.NeedsInput := true; // only saw whitespace
    result := tok;
  end;


procedure TImpForth.Eval(const token : TTokStr);
  begin
    tok := token;
    if Lookup then Interpret
    else if IsNumber then begin {TODO } end
    else if tok <> '' then NotFound;
    if msg<>'' then begin writeln(msg); msg := '' end;
  end;

procedure TImpForth.EvalNextToken;
  begin
    NextToken;
    Eval(tok);
  end;

begin
end.
