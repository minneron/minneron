// mnbuf : abstract multi-line text buffer for minneron
//
// Copyright © 2014 Michal J Wallace http://tangentstorm.com/
// Available for use under the MIT license. See LICENSE.txt
//
{$mode delphiunicode}{$i xpc.inc}{$H+}
unit mnbuf;
interface uses xpc, rings, sysutils, fs, classes;
type
  token = TStr;
  anchor = TObject;
  EFileNotFound = class(Exception)
  end;
  TMessageEvent = procedure (sender: TObject; s:TStr) of object;
  TBuffer = class (TComponent)
    private type
      TTextNodes = GRing<TStr>;
    private
      nodes  : TTextNodes;
    public
      constructor Create( aOwner : TComponent ); override;
      function init( w, h : cardinal ) : TBuffer;
      function NewCursor:IRingCursor<TStr>;
    public
      procedure LoadFromFile(path:TStr);
      procedure SaveToFile(path:TStr);
      procedure LoadFromStrings(strings : TStrings);
      procedure LoadFromString(s : TStr);
      function ToStrings : TStrings;
      function ToString : TStr; reintroduce;
    public
      procedure Clear;
      function GetLength : cardinal;
      function GetColorString( i : cardinal ) : TStr;
      function GetLine(i:cardinal) : TStr;
      procedure SetLine(i:cardinal; s:TStr);
      procedure InsLine(i:cardinal; s:TStr);
      procedure AddLine(s:TStr);
      procedure DelLine(i:cardinal);
      property length : cardinal read GetLength;
      property lines[i:cardinal]:TStr
        read GetLine write SetLine; default;
      property strings : TStrings read ToStrings write LoadFromStrings;
      property text : TStr read ToString write LoadFromString;
      property colorStrings[ i : cardinal ] : TStr
        read GetColorString;
    end;

  { GSpan : a pair of virtual tokens used for spans/selections }
  type GSpan<t> = class
    public type
      tag = class( anchor )
        public
          is_start, is_end : boolean;
          span : GSpan<T>;
        end;
    public
      start_tag, end_tag : tag;
      obj : T;
    end;

implementation

constructor TBuffer.Create( aOwner : TComponent );
  begin
    inherited; nodes := TTextNodes.Create;
  end;

function TBuffer.Init( w, h : cardinal ) : TBuffer;
  begin nodes := TTextNodes.Create; result := self;
  end;

function TBuffer.NewCursor:IRingCursor<TStr>;
  begin
    result := nodes.MakeCursor
  end;

function TBuffer.GetColorString( i : cardinal ) : string;
  begin result := GetLine(i)
  end;


procedure TBuffer.LoadFromFile( path : TStr );
  var txt : text; line: TStr;
  begin
    if fs.exists( path ) then
      begin
        //  need to check for io errors in here
        AssignFile( txt, u2a( path ));
        Reset( txt );
        while not eof( txt ) do begin
          readln( txt, line );
          self.AddLine( line );
        end;
        CloseFile( txt );
      end
    else raise EFileNotFound.Create(utf8encode(path))
  end;

procedure TBuffer.SaveToFile( path : TStr );
  var txt: text; i : cardinal;
  begin
    AssignFile( txt, u2a( path ));
    Rewrite( txt );
    for i := 0 to self.length -1 do writeln(txt, self[i]);
    CloseFile( txt );
  end;

{ TStrings conversion routines (.strings property) }

procedure TBuffer.LoadFromStrings( strings : TStrings );
  var line: ansistring;
  begin
    for line in strings do self.AddLine(utf8decode(line));
  end;

function TBuffer.ToStrings : TStrings;
  var i : cardinal;
  begin
    result := TStringList.Create;
    if length > 0 then for i := 0 to self.length -1 do
      result.Add(utf8encode(self[i]));
  end;

procedure TBuffer.LoadFromString( s : TStr );
  var ts : TStringList; i : integer;
  begin
    ts := TStringList.Create; ts.text := s;
    self.clear;
    if ts.count > 0 then for i := 0 to ts.count-1 do self.AddLine(ts[i]);
    ts.Free;
  end;


function TBuffer.ToString : TStr;
  var i, len : cardinal;
  begin
    len := self.length;
    if len > 0 then result := self[0] + ^J else result := '';
    if len > 1 then
      for i := 1 to self.length -1 do result += self[i] + ^J;
  end;

procedure TBuffer.Clear;
  begin
    while self.length > 0 do self.DelLine(0)
  end;

{ ITextTile implementation for TBuffer }
function TBuffer.GetLength : cardinal;
  begin
    result := nodes.Length;
  end;

function TBuffer.GetLine(i:cardinal) : TStr;
  begin
    result := nodes[i];
  end;

procedure TBuffer.SetLine(i:cardinal; s:TStr);
  begin
    nodes[i] := s;
  end;

procedure TBuffer.AddLine(s:TStr);
  begin
    nodes.append(s)
  end;

procedure TBuffer.InsLine(i:cardinal; s:TStr);
  begin
    nodes.InsertAt(i, s)
  end;

procedure TBuffer.DelLine(i:cardinal);
  begin
    nodes.DeleteAt(i);
  end;

end.
