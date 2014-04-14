{$mode delphi}{$i xpc.inc}
unit uimpndk;
interface uses xpc, uimpforth, dndk;

type
  TNdkWords = class (TImpModule)
    protected
      _ndk : IBase;
      procedure Connected;
    published
      property ndk : IBase read _ndk write _ndk;
      procedure Attach; override;
      procedure AddEdge;
    end;

implementation

procedure TNdkWords.Attach;
  begin imp.addop('+e', self.addEdge);
  end;

procedure TNdkWords.Connected;
  begin
    if not assigned(_ndk) then raise EImpError.Create('not connected')
  end;

procedure TNdkWords.AddEdge;
  var sv,rv,ov : variant; sub, rel, obj : TStr;
  begin connected;
    imp.data.pop3(sv,rv,ov);
    sub := sv; rel := rv; obj := ov;
    imp.data.push(ndk.e(sub,rel,obj).eid)
  end;

begin
end.
