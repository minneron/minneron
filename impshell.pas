{$mode delphi}
program impshell;
uses uimpforth;
var app : TImpShellApp; // TForthApp;
begin
  app := TImpShellApp.Create(Nil);
  app.Initialize;
  app.AddOp('bye', app.Terminate);
  app.Run;
  app.Free;
end.