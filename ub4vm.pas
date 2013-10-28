{$mode delphi}
unit ub4vm;
interface uses classes, arrays, uminneron, kvm;

type

  TInt32Array = GArray<Int32>;
  TByteArray  = GArray<Byte>;

  TStack = class
    slots : TInt32Array;
    count : byte;
    procedure Push( val : longint );
    function  Pop : longint;
    function  TOS : longint;
    function  NOS : longint;
    procedure Swap;
    procedure Dup;
    procedure Over;
    procedure Rot;
  end;

  OpCode = (opNop, opNot, opXor, opAnd,
            opDup, opDrp, opPsh, opPop,
            opSwp, opRot,
            opFrk, opSpn, opSnd, opYld,
            opAdd, opSub, opMul, opDvm,
            opInc, opDec, opShr, opShl,
            opCmp, opGT,  opLT,  opEq, opIn,
            opJmp, opEls, opRet, opZex,
            opNxt, opGet, opPut );

  TB4VM  = class( TComponent )
    public
      ibuf, obuf : string; { input/output buffers (255 chars) }
      ip, rp, wp : byte;
      data, addr : TStack;
      memory     : TByteArray;
      procedure RunOp( op:OpCode );
      procedure Step;
    end;

  TB4TermView  = class( TView )
    public
      vm : TB4VM;
      procedure Render(term : ITerm); override;
    end;

implementation

{ -- TStack -- }
procedure TStack.Push( val : longint );
  begin
    slots[count] := val; inc(count)
  end;

function TStack.Pop : longint;
  begin
    Dec(count); Pop := slots[count];
  end;

function TStack.TOS : longint; inline;
  begin
    tos := slots[count-1]
  end;

function TStack.NOS : longint; inline;
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

procedure TB4VM.RunOp( op:OpCode );
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

procedure TB4VM.Step;
  begin
  end;

procedure TB4TermView.Render(term :  ITerm);
  var i : integer;
  begin
    //  TODO : have output go to a cached virtual screen
    // !! it's 'rendering' this way so that any output it does goes
    //    to the sub terminal
    for i := 1 to 100 do vm.step;
  end;

initialization
end.
