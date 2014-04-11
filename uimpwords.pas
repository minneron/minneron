// this implements the a small core of basic forth words
// using free pascal variants for cells.

{$mode delphiunicode}{$i xpc.inc}
unit uimpwords;
interface uses xpc, classes, kvm, uimpforth;

type
  TForthWords = class (TImpModule)
   published
      procedure Attach; override;
      procedure OpADD;
      procedure OpSUB;
      procedure OpMUL;
      procedure OpDIV;
   end;

  TTermWords = class (TImpModule)
    public term : kvm.ITerm;
      constructor Create(aOwner : TComponent); override;
      procedure attach; override;
      procedure gettextattr; procedure getw; procedure geth;
      procedure settextattr;
    published { this is just kvm.ITerm, but using the stack }
      procedure xMax; procedure yMax;
      procedure xCur; procedure yCur;
      procedure clrscr; procedure clreol;
      procedure newln; procedure scrollup;
      procedure fg; procedure bg; procedure emit; procedure goxy;
      procedure insln; procedure delln;
      procedure resize;
    end;

implementation

procedure TForthWords.Attach;
  begin
    imp.AddOp('drop', imp.data.drop);
    imp.AddOp('swap', imp.data.swap);
    imp.AddOp('+', self.opADD);
    imp.AddOp('-', self.opSUB);
    imp.AddOp('*', self.opMUL);
    imp.AddOp('%', self.opDIV);
  end;

procedure TForthWords.opADD;
  begin data.push(data.pop + data.pop)
  end;

procedure TForthWords.opMUL;
  begin data.push(data.pop * data.pop)
  end;

procedure TForthWords.opSUB;
  var x, y : variant;
  begin data.pop2(x,y); data.push(y - x);
  end;

procedure TForthWords.opDIV;
  var x, y : variant;
  begin data.pop2(x,y); data.push(y div x);
  end;

constructor TTermWords.Create(aOwner : TComponent);
  begin inherited Create(aOwner); term := kvm.work
  end;
procedure TTermWords.Attach;
  procedure ao(s:TStr;meth:TThunk);
    begin imp.addop(s,meth) end;
  begin ao('getw',getw); ao('geth',geth);
    ao('xMax',xMax); ao('yMax',yMax); ao('xCur',xCur); ao('yCur',yCur);
    ao('clrscr',clrscr); ao('clreol',clreol); ao('newln',newln);
    ao('scrollup',scrollup); ao('fg',fg); ao('bg',bg); ao('emit',emit);
    ao('insln',insln); ao('delln',delln); ao('goxy',goxy); ao('rsz',resize);
  end;
// accessors
procedure TTermWords.settextattr; begin term.textattr := data.pop end;
procedure TTermWords.gettextattr; begin data.push(term.textattr) end;
procedure TTermWords.getw; begin data.push(term.width) end;
procedure TTermWords.geth; begin data.push(term.height) end;
// published method wrappers (they all just delegate to .term)
procedure TTermWords.xMax; begin data.push(term.xMax) end;
procedure TTermWords.yMax; begin data.push(term.yMax) end;
procedure TTermWords.xCur; begin data.push(term.wherex) end;
procedure TTermWords.yCur; begin data.push(term.wherey) end;
procedure TTermWords.clrscr; begin term.ClrScr; end;
procedure TTermWords.clreol; begin term.ClrEol; end;
procedure TTermWords.newln; begin term.NewLine; end;
procedure TTermWords.scrollup; begin term.ScrollUp; end;
procedure TTermWords.fg; begin term.fg(data.pop) end;
procedure TTermWords.bg; begin term.Bg(data.pop) end;
procedure TTermWords.emit; begin term.emit(data.pop); end;
procedure TTermWords.insln; begin term.InsLine; end;
procedure TTermWords.delln; begin term.DelLine; end;
procedure TTermWords.goxy; var x,y : variant;
  begin data.pop2(x,y); term.gotoxy(x,y) end;
procedure TTermWords.resize; var x,y : variant;
  begin data.pop2(x,y); term.resize(x,y); end;

initialization
end.
