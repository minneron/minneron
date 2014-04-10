// this implements the a small core of basic forth words
// using free pascal variants for cells.

{$mode delphiunicode}{$i xpc.inc}
unit uimpwords;
interface uses xpc, classes, kvm, utv, uimpforth;

type
  TForthWords = class (TImpModule)
   published
      procedure Attach; override;
      procedure OpADD;
      procedure OpSUB;
      procedure OpMUL;
      procedure OpDIV;
   end;

  TImpTerm = class (TImpModule)
    public term : kvm.ITerm; view : utv.TTermView;
      constructor Create(aOwner : TComponent); override;
      procedure attach; override;
      procedure gettextattr; procedure getw; procedure geth;
      procedure settextattr;
    published { this is just kvm.ITerm, but using the stack }
      procedure xMax; procedure yMax;
      procedure xCur; procedure yCur;
      procedure xPos; procedure yPos;
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

constructor TImpTerm.Create(aOwner : TComponent);
  begin inherited Create(aOwner);
    view := utv.TTermView.Create(self); view.name := 'view'; term:=view.term;
  end;
procedure TImpTerm.Attach;
  procedure ao(s:TStr;meth:TThunk);
    begin imp.addop(s,meth) end;
  begin ao('getw',getw); ao('geth',geth); ao('xPos',xPos); ao('yPos',yPos);
    ao('xMax',xMax); ao('yMax',yMax); ao('xCur',xCur); ao('yCur',yCur);
    ao('clrscr',clrscr); ao('clreol',clreol); ao('newln',newln);
    ao('scrollup',scrollup); ao('fg',fg); ao('bg',bg); ao('emit',emit);
    ao('insln',insln); ao('delln',delln); ao('goxy',goxy); ao('rsz',resize);
  end;
// accessors
procedure TImpTerm.settextattr; begin term.textattr := data.pop end;
procedure TImpTerm.gettextattr; begin data.push(term.textattr) end;
procedure TImpTerm.getw; begin data.push(term.width) end;
procedure TImpTerm.geth; begin data.push(term.height) end;
procedure TImpTerm.xPos; begin data.push(view.x) end;
procedure TImpTerm.yPos; begin data.push(view.y) end;
// published method wrappers (they all just delegate to .term)
procedure TImpTerm.xMax; begin data.push(term.xMax) end;
procedure TImpTerm.yMax; begin data.push(term.yMax) end;
procedure TImpTerm.xCur; begin data.push(term.wherex) end;
procedure TImpTerm.yCur; begin data.push(term.wherey) end;
procedure TImpTerm.clrscr; begin term.ClrScr; view.smudge end;
procedure TImpTerm.clreol; begin term.ClrEol; view.smudge end;
procedure TImpTerm.newln; begin term.NewLine; view.smudge end;
procedure TImpTerm.scrollup; begin term.ScrollUp; view.smudge end;
procedure TImpTerm.fg; begin term.fg(data.pop) end;
procedure TImpTerm.bg; begin term.Bg(data.pop) end;
procedure TImpTerm.emit; begin term.emit(data.pop); view.smudge end;
procedure TImpTerm.insln; begin term.InsLine; view.smudge end;
procedure TImpTerm.delln; begin term.DelLine; view.smudge end;
procedure TImpTerm.goxy; var x,y : variant;
  begin data.pop2(x,y); term.gotoxy(x,y) end;
procedure TimpTerm.resize; var x,y : variant;
  begin data.pop2(x,y); term.resize(x,y); end;

initialization
end.
