// mnbuf : abstract multi-line text buffer for minneron
//
// Copyright © 2014 Michal J Wallace http://tangentstorm.com/
// Available for use under the MIT license. See LICENSE.txt
//
{$mode delphi}{$i xpc.inc}{$H+}
unit mnbuf;
interface uses xpc, rings, tiles, sysutils, fs, classes;
type
  token = string;
  anchor = TObject;
  EFileNotFound = class(Exception)
  end;
  TMessageEvent = procedure (sender: TObject; s:string) of object;
  TBuffer = class (TTextTile)
    private type
      TTextNodes = GRing<string>;
    private
      nodes  : TTextNodes;
      _path  : string;
    public
      constructor Create( w, h : cardinal );
      function  NewCursor:IRingCursor<string>;
    public
      procedure LoadFromFile(path:string);
      procedure SaveToFile(path:string);
      procedure LoadFromStrings( strings : TStrings );
      function ToStrings : TStrings;
      function ToString : string; override;
    public
      procedure Clear;
      function  GetLength : cardinal; override;
      function  GetLine(i:cardinal) : string; override;
      procedure SetLine(i:cardinal; s:string); override;
      procedure InsLine(i:cardinal; s:string); override;
      procedure AddLine(s:string); override;
      procedure DelLine(i:cardinal); override;
      property length : cardinal read GetLength;
      property lines[i:cardinal]:string
        read GetLine write SetLine; default;
      property strings : TStrings read ToStrings write LoadFromStrings;
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

constructor TBuffer.Create( w, h : cardinal );
  begin
    inherited Create( w, h );
    nodes := TTextNodes.Create;
  end;

function TBuffer.NewCursor:IRingCursor<string>;
  begin
    result := nodes.MakeCursor
  end;



procedure TBuffer.LoadFromFile( path : string );
  var txt : text; line: string;
  begin
    if fs.exists( path ) then
      begin
        //  need to check for io errors in here
        assign( txt, path );
        reset( txt );
        while not eof( txt ) do begin
          readln( txt, line );
          self.AddLine( line );
        end;
        close( txt );
      end
    else raise EFileNotFound.Create(path)
  end;

procedure TBuffer.SaveToFile( path : string );
  var txt: text; i : cardinal;
  begin
    assign( txt, path );
    rewrite( txt );
    for i := 0 to self.length -1 do writeln(txt, self[i]);
    close( txt );
  end;

{ TStrings conversion routines (.strings property) }

procedure TBuffer.LoadFromStrings( strings : TStrings );
  var line: string;
  begin
    for line in strings do self.AddLine(line);
  end;

function TBuffer.ToStrings : TStrings;
  var i : cardinal;
  begin
    result := TStringList.Create;
    if length > 0 then for i := 0 to self.length -1 do result.Add(self[i]);
  end;

function TBuffer.ToString : string;
  var i, len : cardinal;
  begin
    len := self.length;
    if len > 0 then result := self[0] else result := '';
    if len > 1 then
      for i := 1 to self.length -1 do appendstr(result, self[i]);
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

function TBuffer.GetLine(i:cardinal) : string;
  begin
    result := nodes[i];
  end;

procedure TBuffer.SetLine(i:cardinal; s:string);
  begin
    nodes[i] := s;
  end;

procedure TBuffer.AddLine(s:string);
  begin
    nodes.append(s)
  end;

procedure TBuffer.InsLine(i:cardinal; s:string);
  begin
    nodes.InsertAt(i, s)
  end;

procedure TBuffer.DelLine(i:cardinal);
  begin
    nodes.DeleteAt(i);
  end;

end.
