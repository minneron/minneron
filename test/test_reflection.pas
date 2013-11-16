{$mode delphi}{$H+}
{$i test_reflection.def }
implementation uses classes, ureflection;

type
  TWhatever = class (TComponent)
    protected
      _int      : integer;
      _OnChange : TNotifyEvent;
      procedure SetInt(value : integer);
    published
      property int : integer read _int write SetInt;
      property OnChange : TNotifyEvent write _OnChange;
    end;

procedure TWhatever.SetInt(value : integer);
  begin
    _int := value;
    if assigned(_OnChange) then _OnChange(self);
  end;

var
  obj : TWhatever;
  rfl : IReflection;

procedure setup;
  begin
    if Assigned(obj) then obj.Free;
    obj := TWhatever.Create(nil);
  end;

procedure test_reflect;
  begin
    rfl := reflect(obj);
  end;

finalization
  if Assigned(obj) then obj.Free;
end.
