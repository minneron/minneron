// This is a bare-bones shell that does not use the uapp framework.
// it's just an infinite loop that waits for the user to enter a
// line, and then executes all the tokens on that line.

{$mode delphi}
program impshell;
uses uimpshell;
var app : TImpShellApp; // TForthApp;
begin
  app := TImpShellApp.Create(Nil);
  app.Initialize;
  app.AddOp('bye', app.Terminate);
  app.Run;
  app.Free;
end.
