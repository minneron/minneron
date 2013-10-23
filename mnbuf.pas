{$mode delphi}{$i xpc.inc}{$H+}
unit mnbuf;
interface uses xpc, rings, tiles, sysutils, fs;
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
    public
      procedure Load(path:string);
    public
      function  GetLength : cardinal; override;
      function  GetLine(i:cardinal) : string; override;
      procedure SetLine(i:cardinal; s:string); override;
      procedure InsLine(i:cardinal; s:string); override;
      procedure AddLine(s:string); override;
      procedure DelLine(i:cardinal); override;
      property length : cardinal read GetLength;
      property lines[i:cardinal]:string
	read GetLine write SetLine; default;
    end;

  { GSpan : a pair of virtual tokens used for spans/selections }
  type GSpan<t> = class
    public type
      tag = class( anchor )
	public
          is_start, is_end : boolean;
          span		 : GSpan<T>;
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


procedure TBuffer.Load( path : string );
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
    else raise EFileNotFound(path)
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
