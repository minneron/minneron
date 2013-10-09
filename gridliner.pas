program gridliner;
uses arrays, dom;

type
  ICmd = interface
    procedure invoke;
    procedure undo;
    function undoable : boolean;
  end;  

  TApp = class
    protected
      _history : GArray<ICommand>;
    public
      procedure Draw( gc : TGraphicsContext );
    end;

  ISeq<T> = interface { this is already pretty close to sq.pas... }
    first : T;
    final : T;
    after( const val : T ) : T;
    before( const val : T ) : T;
    aheadOfBy( const val : T; by : integer ) : T;
    {  TODO: .iter : IEnumerator }
  end;

  TDocSeq = class { a Sequence for traversing document nodes }
    public
    end;

  TStylist = class 
    public
    end;

  TKeyMap = class  { maps key (sequence?) -> ICmd }
    public
    end;

  TOutlinerApp = class (TApp)
    public
      saveCmd  : ICmd;
      outline  : dom.TXMLDocument;
      keymap   : TKeyMap;
      stylist  : TStylist;
    end;
begin
end.
