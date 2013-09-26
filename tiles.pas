{ This unit sets up the base types and interfaces
  used in minneron's text display system }
{$mode objfpc}{$h+}
unit tiles;
interface

type

  // A 2d rectangle with no particular location.
  ITile	= interface
    function GetW : cardinal;
    function GetH : cardinal;
    procedure SetW( value : cardinal );
    procedure SetH( value : cardinal );
    procedure Resize( w, h : cardinal );
    property w : cardinal read GetW write SetW;
    property h : cardinal read GetH write SetH;
  end;

  // a 2d rectangle made of strings
  ITextTile = interface (ITile)
    function GetColorString( i : cardinal ) : string;
    function GetPlainString( i : cardinal ) : string;
    procedure SetPlainString( i : cardinal; s : string );
    property plainStrings[ i : cardinal ] : string
      read GetPlainString write SetPlainString;
    property colorStrings[ i : cardinal ] : string
      read GetColorString;
  end;

  TTile = class (TInterfacedObject, ITile)
    private
       _w, _h : cardinal;
    public
      constructor Create( w, h : cardinal );
    public { ITile }
      function GetW : cardinal;
      function GetH : cardinal;
      procedure SetW( value : cardinal );
      procedure SetH( value : cardinal );
      procedure Resize( w, h : cardinal ); virtual;
    end;

  // Abstract TextTile with a GetColorString that
  // delegates to GetPlainString
  TTextTile = class (TTile, ITextTile)
    public { ITextTile }
      function GetColorString( i : cardinal ) : string; virtual;
      function GetPlainString( i : cardinal ) : string;
        virtual; abstract;
      procedure SetPlainString( i : cardinal; s : string );
        virtual; abstract;
    end;

implementation

constructor TTile.Create( w, h : cardinal );
  begin
    inherited create;
    self.Resize(w, h);
  end;

function TTile.GetW : cardinal;
  begin
    result := _w
  end;

function TTile.GetH : cardinal;
  begin
    result := _h
  end;

procedure TTile.Resize( w, h : cardinal );
  begin
    _w := w; _h := h;
  end;

procedure TTile.SetW( value : cardinal );
  begin
    Resize(value, _h)
  end;

procedure TTile.SetH( value : cardinal );
  begin
    Resize(_w, value)
  end;

function TTextTile.GetColorString( i : cardinal ) : string;
  begin
    result := GetPlainString(i)
  end;

initialization
end.
