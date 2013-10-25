{ minneron
----------------------------------------------------------------
Copyright (c) 2012 Michal J Wallace. All rights reserved.
---------------------------------------------------------------}
{$mode delphi}{$i xpc.inc}{$H+}
program min;
uses xpc, mnml, mned, cw, cx, kvm, sysutils, kbd, impworld, custapp;

type
  TMinApp  = class(TCustomApplication)
    private
      ed  : TEditor;
      edi : IMorph;
    public
      procedure Initialize; override;
      procedure DoRun; override;
      destructor Destroy; override;
    end;


procedure TMinApp.Initialize;
  var
    okay : boolean;
  begin
    okay := false;
    ed := TEditor.Create;
    if ParamCount = 0 then
      writeln( 'usage : min <filename> ')
    else if not ed.Load( ParamStr( 1 )) then
      writeln( 'unable to load file: ', paramstr( 1 ))
    else
      begin
        edi := ed;
	impworld.world.add(edi);
	impworld.focus := edi;
        okay := true;
      end;
    if not okay then terminate;
  end;


procedure TMinApp.DoRun;
  begin
    mnml.step;
    if ed.done and mnml.done then Terminate;
  end;


destructor TMinApp.Destroy;
  begin
    ed.Destroy;
    inherited Destroy;
  end;


var app : TMinApp;
begin
  app := TMinApp.Create(nil);
  app.Initialize;
  app.Run;
  app.Free;
end.
