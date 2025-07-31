unit TestTeraContext;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry
, Tera.Context
;

type

{ TTestTeraContext }
  TTestTeraContext= class(TTestCase)
  private
    FContext: TContext;
    FStringValue: TStringValue;

    procedure ErrorNotArray;
  protected
  public
  published
    procedure TestContextCreate;
    procedure TestStringValueCreate;
    { #todo -ogcarreno : Implement more tests }
  end;

implementation

{ TTestTeraContext }

procedure TTestTeraContext.ErrorNotArray;
begin
  FStringValue.AsArray;
end;

procedure TTestTeraContext.TestContextCreate;
begin
  FContext:= TContext.Create;
  try
    AssertEquals('Context is empty', 0, FContext.Count);
  finally
    FContext.Free;
  end;
end;

procedure TTestTeraContext.TestStringValueCreate;
begin
  FStringValue:= TStringValue.Create('Bob');
  try
    AssertEquals('AsString returns Bob', 'Bob', FStringValue.AsString);
    AssertException(
      'Exception: not an array',
      Exception,
      @ErrorNotArray,
      'not an array'
    );
  finally
    FStringValue.Free;
  end;
end;

initialization
  RegisterTest(TTestTeraContext);
end.

