unit TestTeraEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry
, Tera.Engine
, Tera.Context
;

type

{ TTestTeraTemplate }
  TTestTeraTemplate= class(TTestCase)
  private
    FTemplate: TTeraEngine;
  protected
  public
  published
    procedure TestTemplateCreate;
    procedure TestTemplateVariable;
    procedure TestTemplateInclude;
    procedure TestTemplateIfTrue;
    procedure TestTemplateIfFalse;
    procedure TestTemplateFor;
    { #todo -ogcarreno : Implement more tests }
  end;

implementation

{ TTestTeraTemplate }

procedure TTestTeraTemplate.TestTemplateCreate;
var
  lTplFolder, lExpectedFolder: String;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/templates';

  lExpectedFolder:= ExtractFileDir(ParamStr(0));
  lExpectedFolder += '/templates/';

  FTemplate:= TTeraEngine.Create(lTplFolder);
  try
    AssertEquals('Base Dir', lExpectedFolder, FTemplate.BaseDir);
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateVariable;
var
  lTplFolder, lResult: String;
  lContext: TContext;
  lIndex: Integer;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lResult:= '';

  FTemplate:= TTeraEngine.Create(lTplFolder);
  try
    lContext:= TContext.Create;
    try
      lContext.Add('variable', TStringValue.Create('value'));
      FTemplate.Parse('variable.tpl');
      lResult:= FTemplate.Render(lContext);
      AssertEquals('Variable is value', 'value'+LineEnding, lResult);
    finally
      for lIndex:= 0 to Pred(lContext.Count) do
        lContext.Data[lIndex].Free;
      lContext.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateInclude;
var
  lTplFolder, lResult: String;
  lContext: TContext;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lResult:= '';

  FTemplate:= TTeraEngine.Create(lTplFolder);
  try
    lContext:= TContext.Create;
    try
      FTemplate.Parse('include.tpl');
      lResult:= FTemplate.Render(lContext);
      AssertEquals('Include is Sub Template', 'Sub Template'+LineEnding+LineEnding, lResult);
    finally
      lContext.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateIfTrue;
var
  lTplFolder, lExpected, lResult: String;
  lContext: TContext;
  lIndex: Integer;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lExpected:= 'Hello, Supreme Overlord Bob!';

  lResult:= '';

  FTemplate:= TTeraEngine.Create(lTplFolder);
  try
    lContext:= TContext.Create;
    try
      lContext.Add('name', TStringValue.Create('Bob'));
      FTemplate.Parse('if.tpl');
      lResult:= FTemplate.Render(lContext);
      AssertEquals('If result', lExpected+LineEnding+LineEnding, lResult);
    finally
      for lIndex:= 0 to Pred(lContext.Count) do
        lContext.Data[lIndex].Free;
      lContext.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateIfFalse;
var
  lTplFolder, lExpected, lResult: String;
  lContext: TContext;
  lIndex: Integer;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lExpected:= 'Hello, Alice!';

  lResult:= '';

  FTemplate:= TTeraEngine.Create(lTplFolder);
  try
    lContext:= TContext.Create;
    try
      lContext.Add('name', TStringValue.Create('Alice'));
      FTemplate.Parse('if.tpl');
      lResult:= FTemplate.Render(lContext);
      AssertEquals('If result', lExpected+LineEnding+LineEnding, lResult);
    finally
      for lIndex:= 0 to Pred(lContext.Count) do
        lContext.Data[lIndex].Free;
      lContext.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateFor;
var
  lTplFolder, lExpected, lResult: String;
  lContext: TContext;
  lArrayValue: TArrayValue;
  lIndex: Integer;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lExpected:=
    'List:' + LineEnding +
    '- one' + LineEnding +
    '- two' + LineEnding;

  lResult:= '';

  FTemplate:= TTeraEngine.Create(lTplFolder);
  try
    lContext:= TContext.Create;
    try
      lArrayValue:= TArrayValue.Create;
      lArrayValue.Items.Append('one');
      lArrayValue.Items.Append('two');
      lContext.Add('items', lArrayValue);

      FTemplate.Parse('array.tpl');
      lResult:= FTemplate.Render(lContext);
      AssertEquals('Array result', lExpected+LineEnding, lResult);
    finally
      for lIndex:= 0 to Pred(lContext.Count) do
        lContext.Data[lIndex].Free;
      lContext.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

initialization
  RegisterTest(TTestTeraTemplate);
end.

