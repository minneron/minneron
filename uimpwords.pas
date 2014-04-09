// this implements the a small core of basic forth words
// using free pascal variants for cells.

{$mode delphiunicode}
unit uimpwords;
interface uses xpc, classes, kvm, uimpforth;

type
  TImpWords = class (TComponent)
    protected
      data, side : TimpStack;
      imp : TImpForth;
    public
      constructor Create(aImp :  TImpForth); reintroduce;
      procedure OpADD;
      procedure OpSUB;
      procedure OpMUL;
      procedure OpDIV;
    end;

implementation

constructor TImpWords.Create(aImp : TImpForth);
  begin
    inherited Create(aImp);
    imp := aImp; data := imp.data; side := imp.side;
    imp.AddOp('drop', imp.data.drop);
    imp.AddOp('swap', imp.data.swap);
    imp.AddOp('clear', kvm.work.ClrScr);
    imp.AddOp('+', self.opADD);
    imp.AddOp('-', self.opSUB);
    imp.AddOp('*', self.opMUL);
    imp.AddOp('%', self.opDIV);
  end;

procedure TImpWords.opADD;
  begin data.push(data.pop + data.pop)
  end;

procedure TImpWords.opMUL;
  begin data.push(data.pop * data.pop)
  end;

procedure TImpWords.opSUB;
  var x, y : variant;
  begin data.pop2(x,y); data.push(y - x);
  end;

procedure TImpWords.opDIV;
  var x, y : variant;
  begin data.pop2(x,y); data.push(y div x);
  end;


initialization
end.
