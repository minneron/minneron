// Another version, using xoxo, sax, and rings.
// In the DOM, each node has exactly one parent,
// but I want to reuse the same nodes in several
// trees. Simple example: outline nodes can contain
// html markup.
{$mode delphi}{$i xpc.inc}
unit uoutdoc;
interface
uses classes, xpc, rings, kvm, kbd, sax, sax_xml, dom, cw,
     stri, cx, stacks, xmlwrite, sysutils;

type

  TOutNode = class (GElement<TDomNode>)
  end;

  TOutline = GElement<TOutNode>;
  TCursor  = IRingCursor<TDomNode>;

  TOutDoc  = class (TComponent)
    protected
      _cur : TCursor;
      _out : TOutline;
      _dom : TXmlDocument;
      _stk : GStack<TDomNode>;
      _ctx : TDomNode;
      procedure push(newCtx : TDomElement);
      procedure pushChild(newCtx : TDomElement);
      procedure pop;
      function  ThisElement : TDomElement;
    public
      constructor Create( aOwner : TComponent ); override;
    public { interface for building the outline structure }
      // When building, you should call each of these in turn:
      procedure StartList;
      procedure StartItem;
      procedure CloseItem;
      procedure CloseList;
    public { sax-style content building. }
      procedure StartTag(tag : string);
      procedure SetAttr(key, val : widestring);
      function  AddText(text : string) : TDomText;
      procedure CloseTag(tag: string);
    property
      xmlDoc : TXmlDocument read _dom;
    end;

  TSaxLoader = class (TComponent)
    protected
      _doc    : TOutDoc;
      _path   : string;
    public
      property path : string read _path write _path;
      function Load : TOutDoc; overload;
      function Load( aPath : string ) : TOutDoc; overload;
    public { sax events }
      procedure OnChars(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
      //procedure OnComment(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
      procedure OnEndElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
      //procedure OnEndPrefixMapping(Sender: TObject; const Prefix: SAXString);
      procedure OnWhitespace(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
      //procedure OnInstruction(Sender: TObject; const Target, Data: SAXString);
      //procedure OnkippedEntity(Sender: TObject; const Name: SAXString);
      procedure OnStartElement(Sender: TObject; const ns, LocalName, QName: SAXString;
                               Attrs: TSAXAttributes);
      //procedure OnStartPrefixMapping(Sender: TObject; const Prefix, URI: SAXString);
      //procedure OnNotationDec(Sender: TObject; const Name, PublicID, SystemID: SAXString);
      //procedure OnUnparsedEntity(Sender: TObject;
      //                           const Name, PublicID, SystemID, NotationName: SAXString);
      //function ResolveEntity(Sender: TObject; const PublicID, SystemID: SAXString):TSAXInputSource;
      //procedure OnError(Sender: TObject; AException: ESAXParseException);
      //procedure OnFatalErrorEvent(Sender: TObject; AException: ESAXParseException);
      //procedure OnWarning(Sender: TObject; AException: ESAXParseException);
    end;

implementation

constructor TOutDoc.Create( aOwner : TComponent );
  begin
    inherited Create(aOwner);
    _dom := TXmlDocument.Create;
    _ctx := _dom;
    _stk := GStack<TDomNode>.Create(16);
  end;

procedure TOutDoc.Push(newCtx: TDomElement);
  begin
    _stk.Push(_ctx);
    _ctx := newCtx;
  end;

procedure TOutDoc.PushChild(newCtx: TDomElement);
  begin
    _ctx.AppendChild(newCtx);
    push(newCtx);
  end;

procedure TOutDoc.Pop;
  begin
    _ctx := _stk.Pop;
  end;

{ outline structure routines }
procedure TOutDoc.StartList;
  begin
    pushChild(_dom.CreateElement('ul'));
    cwriteln('|b<|Bul|b>');
  end;

procedure TOutDoc.StartItem;
  begin
    pushChild(_dom.CreateElement('li'));
    cwriteln('|b<|Bli|b>');
  end;

procedure TOutDoc.CloseItem;
  begin
    pop;
    cwriteln('|b</li>');
  end;

procedure TOutDoc.CloseList;
  begin
    pop;
    cwriteln('|b</ul>');
  end;

{ content building routines }
function TOutDoc.ThisElement : TDomElement; inline;
  begin
    if _ctx is TDomElement then result := TDomElement(_ctx)
    else raise Exception.Create('error: outside element context');
  end;

procedure TOutDoc.SetAttr(key, val : widestring);
  begin
    ThisElement.SetAttribute(key, val);
    cwriteln(' |y' + utf8encode(key) + '|r=|y"|Y' + utf8encode(val) + '|y"' );
  end;

procedure TOutDoc.StartTag(tag : string);
  begin
    pushChild(_dom.CreateElement(Utf8Decode(tag)));
    cwriteln('|r<|R' + tag + '|r>');
  end;

procedure TOutDoc.CloseTag(tag : string);
  begin
    pop;
    cwriteln('|r</' + tag + '>');
  end;

function TOutDoc.AddText(text : string) : TDomText;
  begin
    result := _dom.CreateTextNode(text);
    ThisElement.AppendChild(result);
    cwriteln('|w' + text)
  end;


function TSaxLoader.Load : TOutDoc;
  var reader : TSaxXmlReader;
  begin
    reader := TSaxXmlReader.Create;
    reader.OnCharacters := self.OnChars;
    reader.OnStartElement := self.OnStartElement;
    reader.OnEndElement := self.OnEndElement;
    reader.OnIgnorableWhitespace := self.OnWhitespace;
    _doc := TOutDoc.Create(nil);
    reader.Parse(Utf8Decode(_path));
    reader.Free;
    result := _doc;
  end;

function TSaxLoader.Load( aPath : string ) : TOutDoc;
  begin
    _path := aPath;
    result := self.load;
  end;

procedure TSaxLoader.OnChars
  (Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
  var s : string; i : integer;
  begin
    SetLength(s, alength);
    for i := 1 to alength do s[i] := ch[i-1];
    _doc.AddText(s);
  end;

procedure TSaxLoader.OnWhitespace
  (Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
  begin
    pass
  end;

function w2s (s:widestring):string;
  begin
    result := utf8encode(s)
  end;

procedure TSaxLoader.OnStartElement
  (Sender: TObject; const NS, LocalName, QName: SAXString; Attrs: TSAXAttributes);
  var i : cardinal;
  begin
    if (localname = 'ul') then _doc.StartList
    else if (localname='li') then _doc.StartItem
    else _doc.StartTag(w2s(localname));
    if assigned(attrs) then with attrs do
      if length > 0 then for i := 0 to pred(length) do
        begin
          _doc.SetAttr(localnames[i], values[i]);
        end;
  end;

procedure TSaxLoader.OnEndElement(Sender: TObject;
                                  const NamespaceURI, LocalName, QName: SAXString);
  begin
    if (localname = 'ul') then _doc.CloseList
    else if(localname='li') then _doc.CloseItem
    else _doc.CloseTag(w2s(localname));
  end;

procedure main;
  var loader: TSaxLoader; doc : TOutDoc;
  begin
    loader := TSaxLoader.Create(nil);
    doc := loader.Load('junk.xoxo');
    cwrite('|w');
    xmlwrite.WriteXmlFile(doc.xmlDoc, 'saved.xoxo');
  end;

initialization
end.
