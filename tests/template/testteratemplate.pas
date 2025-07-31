unit TestTeraTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry
, Tera.Template
, Tera.Context
;

type

{ TTestTeraTemplate }
  TTestTeraTemplate= class(TTestCase)
  private
    FTemplate: TTemplateEngine;
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

  FTemplate:= TTemplateEngine.Create(lTplFolder);
  try
    AssertEquals('Base Dir', lExpectedFolder, FTemplate.BaseDir);
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateVariable;
var
  lTplFolder, lResult: String;
  lCtx: TContext;
  lIndex: Integer;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lResult:= '';

  FTemplate:= TTemplateEngine.Create(lTplFolder);
  try
    lCtx:= TContext.Create;
    try
      lCtx.Add('variable', TStringValue.Create('value'));
      lResult:= FTemplate.Render('variable.tpl', lCtx);
      AssertEquals('Variable is value', 'value'+LineEnding, lResult);
    finally
      for lIndex:= 0 to Pred(LCtx.Count) do
        lCtx.Data[lIndex].Free;
      lCtx.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateInclude;
var
  lTplFolder, lResult: String;
  lCtx: TContext;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lResult:= '';

  FTemplate:= TTemplateEngine.Create(lTplFolder);
  try
    lCtx:= TContext.Create;
    try
      lResult:= FTemplate.Render('include.tpl', lCtx);
      AssertEquals('Include is Sub Template', 'Sub Template'+LineEnding, lResult);
    finally
      lCtx.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateIfTrue;
var
  lTplFolder, lExpected, lResult: String;
  lCtx: TContext;
  lIndex: Integer;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lExpected:= '  Hello, Supreme Overlord Bob!';

  lResult:= '';

  FTemplate:= TTemplateEngine.Create(lTplFolder);
  try
    lCtx:= TContext.Create;
    try
      lCtx.Add('name', TStringValue.Create('Bob'));
      lResult:= FTemplate.Render('if.tpl', lCtx);
      AssertEquals('If result', lExpected+LineEnding, lResult);
    finally
      for lIndex:= 0 to Pred(LCtx.Count) do
        lCtx.Data[lIndex].Free;
      lCtx.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateIfFalse;
var
  lTplFolder, lExpected, lResult: String;
  lCtx: TContext;
  lIndex: Integer;
begin
  lTplFolder:= ExtractFileDir(ParamStr(0));
  lTplFolder += '/../tests/templates';

  lExpected:= '  Hello, Alice!';

  lResult:= '';

  FTemplate:= TTemplateEngine.Create(lTplFolder);
  try
    lCtx:= TContext.Create;
    try
      lCtx.Add('name', TStringValue.Create('Alice'));
      lResult:= FTemplate.Render('if.tpl', lCtx);
      AssertEquals('If result', lExpected+LineEnding, lResult);
    finally
      for lIndex:= 0 to Pred(LCtx.Count) do
        lCtx.Data[lIndex].Free;
      lCtx.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

procedure TTestTeraTemplate.TestTemplateFor;
var
  lTplFolder, lExpected, lResult: String;
  lCtx: TContext;
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

  FTemplate:= TTemplateEngine.Create(lTplFolder);
  try
    lCtx:= TContext.Create;
    try
      lArrayValue:= TArrayValue.Create;
      lArrayValue.Items.Append('one');
      lArrayValue.Items.Append('two');
      lCtx.Add('list', lArrayValue);
      lResult:= FTemplate.Render('array.tpl', lCtx);
      AssertEquals('Array result', lExpected, lResult);
    finally
      for lIndex:= 0 to Pred(LCtx.Count) do
        lCtx.Data[lIndex].Free;
      lCtx.Free;
    end;
  finally
    FTemplate.Free;
  end;
end;

initialization
  RegisterTest(TTestTeraTemplate);
end.

