{$mode delphi}{$i xpc.inc}{$H+}
unit mnbuf;
interface uses xpc, rings, tiles;
type
  token = string;
  anchor = TObject;

  TBuffer = class (TTextTile)
    private type
      TTextNodes  = GRing<string>;
    private
      nodes : TTextNodes;
    public
      constructor Create( w, h : cardinal );
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
