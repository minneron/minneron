unit ureflection;
interface uses classes, typinfo, rttiutils;

type
  IReflection = interface
    function GetInstance : TObject;
    procedure SetInstance(obj : TObject);
    property instance : TObject read GetInstance write SetInstance;
  end;

  TReflection = class (TComponent, IReflection)
    protected
      _instance : TObject;
    published
      function GetInstance : TObject;
      procedure SetInstance(obj : TObject);
      property instance : TObject read GetInstance write SetInstance;
    end;

  function reflect(obj : TObject ) : IReflection;

implementation

function TReflection.GetInstance : TObject;
  begin
    result := _instance
  end;

procedure TReflection.SetInstance(obj : TObject);
  begin
    _instance := obj
  end;

  
function reflect(obj : TObject ) : IReflection;
  begin
    result := TReflection.Create(nil);
    result.instance := obj;
  end;

begin
end.
