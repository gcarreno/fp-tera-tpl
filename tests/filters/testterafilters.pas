unit TestTeraFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry
, Tera.Filters
;

type

{ TTestFilters }
  TTestFilters= class(TTestCase)
  private
  protected
  public
  published
    procedure TestFilterUpper;
    procedure TestFilterLower;
    procedure TestFilterTrim;
    procedure TestFilterLength;
    procedure TestFilterUnknown;
  end;

implementation

{ TTestFilters }

procedure TTestFilters.TestFilterUpper;
var
  lFilterFunc: TFilterFunc;
  lFound: Boolean;
begin
  lFilterFunc:= nil;
  lFound:= FilterRegistry.TryGetData('upper', lFilterFunc);
  AssertTrue('Found upper function', lFound);
  AssertNotNull('lFilterFunc is not nil', lFilterFunc);
  AssertEquals('Result is uppercase', 'BOB', lFilterFunc('bOb'));
end;

procedure TTestFilters.TestFilterLower;
var
  lFilterFunc: TFilterFunc;
  lFound: Boolean;
begin
  lFilterFunc:= nil;
  lFound:= FilterRegistry.TryGetData('lower', lFilterFunc);
  AssertTrue('Found lower function', lFound);
  AssertNotNull('lFilterFunc is not nil', lFilterFunc);
  AssertEquals('Result is lowercase', 'bob', lFilterFunc('bOb'));
end;

procedure TTestFilters.TestFilterTrim;
var
  lFilterFunc: TFilterFunc;
  lFound: Boolean;
begin
  lFilterFunc:= nil;
  lFound:= FilterRegistry.TryGetData('trim', lFilterFunc);
  AssertTrue('Found trim function', lFound);
  AssertNotNull('lFilterFunc is not nil', lFilterFunc);
  AssertEquals('Result is trimmed', 'Bob', lFilterFunc(' Bob '));
end;

procedure TTestFilters.TestFilterLength;
var
  lFilterFunc: TFilterFunc;
  lFound: Boolean;
begin
  lFilterFunc:= nil;
  lFound:= FilterRegistry.TryGetData('length', lFilterFunc);
  AssertTrue('Found length function', lFound);
  AssertNotNull('lFilterFunc is not nil', lFilterFunc);
  AssertEquals('Result is length 3', '3', lFilterFunc('Bob'));
end;

procedure TTestFilters.TestFilterUnknown;
var
  lFilterFunc: TFilterFunc;
  lFound: Boolean;
begin
  lFilterFunc:= nil;
  lFound:= FilterRegistry.TryGetData('none', lFilterFunc);
  AssertFalse('Not found none function', lFound);
  AssertNull('lFilterFunc is nil', lFilterFunc);
end;

initialization
  RegisterTest(TTestFilters);
end.

