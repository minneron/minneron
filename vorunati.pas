{$mode delphi}
unit vorunati;
interface
type
  // A vorunati ('vor') value represents one of four possible
  // states of a program's lifecycle.
  vor = (
    vo,     // vo : program is busy (mnemonic: 'void')
    ru,     // ru : program needs input (to 'resolve uncertainty')
    na,     // na : program encountered an error
    ti );   // ti : program is terminated ("T is for terminate")
  
  // IVorTask represents a task in one of the four states.
  IVorTask = interface
    procedure Step;
    procedure Run; // Take some steps. Perhaps until state <> vo.
    function GetState : vor;
    property state : vor read GetState;
  end;

  // ITake<I> allows an object to act as a sink for values of type I.
  // result := true indicates that the value was accepted.
  ITake<I> = interface
    function Send( ival : I ) : boolean;
  end;
  
  // IGive<O> allows an object to act as a source for values of type O.
  IGive<O> = interface
    function Poll(out oval : O ) : boolean;
  end;

  // 'void' is a dummy class to fill the Take or Give slot (or both)
  // when the task doesn't need it. Since the constructor is abstract,
  // you can't actually instantiate a 'void'.
  void = class
    public constructor Create; virtual; abstract;
  end;

// TVorunati<I,O> is a default implementation of all three of the
// above interfaces, intended to be used as a base class. It
// ignores all I values sent to it, produces no O values when
// polled, and terminates successfully after the first step.
//
// It is therefore equivalent to the program:
//
// begin
// end.
//
type
  TVorunati<I,O> = class (TInterfacedObject, IVorTask, ITake<I>, IGive<O>)
    private
      _state : vor;
    public
      constructor Create;
      function GetState : vor; virtual;
      function Poll(out oval : O ) : boolean; virtual;
      function Send( ival : I ) : boolean; virtual;
      procedure Step; virtual;
      procedure Run; virtual; // default version steps while state = vo.
    end;

type
  TVorTask = class (TVorunati<void, void>)
  end;

// TVorLift is a way to lift/wrap simple functions into the vorunati system.
type
  TVorFunc = function : vor of object;
  TVorLift = class (TVorunati<void, void>)
    private
      _step : TVorFunc;
    public
      constructor Create( stepFunc : TVorFunc );
      procedure Step; override;
    end;
  

{$IFDEF FUTURE}/////////////////////////////////////////////////
type
  // act a queue for values of type A
  TFIFOVor<A> = class (TVorunati<A,A>)
    constructor Create(src : IGive<A>; snk : ITake<A>);
  end;

  // a stack for values of type A
  TLIFOVor<A>= class (TVorunati<A,A>)
    constructor Create(src : IGive<A>; snk : ITake<A>);
  end;

  // wrap a normal function
  TVorFunc<A,B>= class (TVorunati<A,B>)
    type TFuncAB = function( aval : A ) : B;
    constructor Create(f : TFuncAB);
  end;

  // wrap a normal procedure
  TVorProc<A> = class (TVorunati<A,void>)
    type TProcA = procedure( aval : A );
    constructor Create(p : TProcA);
  end;

  // a TVorVar acts like a variable
  TVorVar<V> = class (TVorunati<V,V>)
  private
    value : V;
  public 
    constructor Create(val : V);
    function Poll(out val : V) : boolean; override;
    function Send(val : V) : boolean; override;
  end;

  // a TVorConst acts like a constant
  TVorConst<K> = class (TVorunati<void,K>)
    value : K;
    constructor Create(const val : K);
    function Poll(out aval : K) : boolean; override;
  end;

  // A type that will compose two variables
  TCompose<A,B,C> = class (TVorunati<A,C>)
    constructor Create(ab : TVorunati<A,B>; bc : TVorunati<B,C>);
  end;

{$ENDIF}////////////////////////////////////////////////////////
  
implementation

{-- default implementation: a worker that halts immediately --}

constructor TVorunati<I,O>.Create;
  begin
    _state := vo { all processes start in the vo state }
  end;

function TVorunati<I,O>.GetState : vor;
  begin
    result := _state
  end;

function TVorunati<I,O>.Poll( out oval : O) : boolean;
  begin
    result := false
  end;

function TVorunati<I,O>.Send( ival : I ): boolean;
  begin
    result :=  false
  end;

procedure TVorunati<I,O>.Step;
  begin
    _state := ti // default implementation terminates immediately
  end;

procedure TVorunati<I,O>.Run;
  begin
    while _state = vo do Step
  end;

constructor TVorLift.Create( stepFunc : TVorFunc );
  begin
    _step := stepFunc
  end;
  
procedure TVorLift.Step;
  begin
    _state := _step
  end;
  
initialization
end.
