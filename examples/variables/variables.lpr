program variables;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  { you can add units after this }
  Tera.Engine,
  Tera.Context;

var
  Engine: TTeraEngine;
  Ctx: TContext;
  Index: Integer;
  TplFolder: String;
begin
  TplFolder:= ExtractFileDir(ParamStr(0));
  TplFolder += '/../templates';
  Engine := TTeraEngine.Create(TplFolder);
  try
    Ctx := TContext.Create;
    try
      Ctx.Add('name', TStringValue.Create('Bob'));
      Ctx.Add('role', TStringValue.Create('Pascal Wizard'));

      Engine.Parse('variables.tpl');
      WriteLn(Engine.Render(Ctx));
    finally
      for Index:= 0 to Pred(Ctx.Count) do
        TTemplateValue(Ctx.Data[Index]).Free;
      Ctx.Free;
    end;
  finally
    Engine.Free;
  end;
end.

