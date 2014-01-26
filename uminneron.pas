{$i xpc.inc}{$mode delphi}
unit uminneron;
interface
uses xpc, classes, udb, udc, kvm, cli, num, sqldb, cw, math;

type
  TView = class(TComponent)
    protected
      _x, _y, _w, _h : cardinal;
      _bg, _fg : byte;
      procedure SetX(value : cardinal);
      procedure SetY(value : cardinal);
      procedure SetW(value : cardinal);
      procedure SetH(value : cardinal);
    published
      procedure Nudge(dx, dy : integer);
      procedure Redraw;
      procedure Render(term :  ITerm); virtual;
      procedure Resize(new_w, new_h : cardinal); virtual;
      property x : cardinal read _x write SetX;
      property y : cardinal read _y write SetY;
      property w : cardinal read _w write SetW;
      property h : cardinal read _h write SetH;
      constructor Create( aOwner : TComponent ); override;
    end;
  
  // A class with its own video ram buffer:
  TTermView = class (TView, ITerm)
    protected
      _gridterm : TGridTerm;
    published
      property term : TGridTerm read _gridterm implements ITerm;
      constructor Create( aOwner : TComponent ); override;
      procedure Render(term :  ITerm); override;
      procedure Resize(new_w, new_h : cardinal); override;
    end;

  TDbTreeGrid = class (TView)
    protected
      _top : cardinal;
      _cur : TDbCursor;
    published
      procedure Render(term :  ITerm); override;
      property DataCursor : TDbCursor read _cur write _cur;
   end;

  TStepper = class (TComponent)
    protected
      fstep : TNotifyEvent;
      procedure DoStep; virtual;
    public
      procedure Step(times:cardinal=1);
    published
      property OnStep : TNotifyEvent read fStep write fStep;
    end;


implementation

constructor TView.Create( aOwner : TComponent );
  begin
    inherited Create( aOwner );
    _x := 0; _y := 0; _w := 30; _h := 10;
    _bg := $FC; _fg := $0;
  end;

procedure TView.SetX(value : cardinal);
  begin
    _x := value;
  end;

procedure TView.SetY(value : cardinal);
  begin
    _y := value;
  end;

procedure TView.SetW(value : cardinal);
  begin
    _w := value;
  end;

procedure TView.SetH(value : cardinal);
  begin
    _h := value;
  end;
  
procedure TView.Nudge(dx, dy : integer);
  begin
    _x += dx;
    _y += dy;
  end;


procedure TView.Redraw;
  var term : kvm.ITerm;
  begin
    term := kvm.work;
    kvm.work := kvm.TSubTerm.Create(term, _x, _y, _w, _h);
    bg(_bg); fg(_fg);
    try
      self.Render(term);
    finally
      kvm.work := term;
    end
  end;

procedure TView.Render(term : ITerm);
  begin
    ClrScr;
  end;

procedure TView.Resize(new_w, new_h : cardinal);
  begin
    _w := new_w;
    _h := new_h;
  end;

constructor TTermView.Create( aOwner : TComponent );
  begin
    inherited Create( aOwner );
    _gridterm := TGridTerm.Create(0, 0);
  end;

procedure TTermView.Resize(new_w, new_h : cardinal);
  begin
    inherited Resize(new_w, new_h);
    _gridterm.grid.Resize(new_w, new_h);
  end;

procedure TTermView.Render(term : ITerm);
  var endy, endx, x, y : byte; cell : TTermCell;
  begin
    endy := min(_h-1, term.yMax);
    endx := min(_w-1, term.xMax);
    for y := 0 to endy do
      begin
	term.gotoxy(_x, _y+y);
        for x := 0 to endx do
          begin
            cell := _gridterm.grid[x,y];
	    term.emit(cell.ch);
          end;
      end;
  end;

{---------------------------------------------------------------}
{ TDbTreeGrid                                                   }
{---------------------------------------------------------------}
procedure TDbTreeGrid.Render(term : ITerm);
  var
    depth : integer = -1;
    sigil : char = ' ';
    i     : integer;
    count : cardinal =  0;
    rs    : TRecordSet;
begin
  bg('b'); fg('W');

  rs := _cur.RecordSet;
  rs.open;
  rs.first;
  while not rs.eof do
    begin
      if rs['depth'] > depth then pass
      else if rs['depth'] < depth then pass
      else pass;
      depth := rs['depth'];

      if rs['collapsed']=1 then sigil := '+'
      else if rs['leaf']=1 then sigil := ' '
      else sigil := '-';

      if rs['hidden'] = 0 then
        begin
          if _cur.AtMark then bg('b') else bg('k');
          gotoxy(0,count);
          { draw the outline controls }
          if rs['depth'] > 0 then
            for i := 1 to rs['depth'] do write('  ');
          write(sigil +  ' ');
          { draw the node itself }
          fg('c'); write(rs['kind'],' ');
          fg('W');
          write(rs['node']);

          { fill in the rest of the line }
          for i := 3 to kvm.xMax - (
            length(rs['node']) + length(rs['kind'])
            + rs['depth'] * 2) do write(' ');

          inc(count);
        end;
      rs.Next;
    end;
  bg('k');
  for i := count to kvm.yMax do
    begin
      gotoxy(0,i);
      clreol;
    end;
end;


procedure TStepper.DoStep;
  begin
  end;

procedure TStepper.Step(times:cardinal=1);
  var i : integer;
  begin
    if times > 0 then for i := 0 to times do
      begin
        DoStep;
        if Assigned(fStep) then fStep(self);
      end;
  end;

initialization
  RegisterClass(TView);
  RegisterClass(TTermView);
  RegisterClass(TDbTreeGrid);
  RegisterClass(TStepper);
end.
