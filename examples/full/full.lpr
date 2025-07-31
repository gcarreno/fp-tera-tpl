program Full;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  { you can add units after this }
  Tera.Engine,
  Tera.Filters,
  Tera.Context;

var
  Engine: TTeraEngine;
  Ctx: TContext;
  Langs: TArrayValue;
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

      Langs:= TArrayValue.Create;
      Langs.Items.Add('Pascal');
      Langs.Items.Add('Go');
      Ctx.Add('languages', Langs);

      Engine.Parse('full.tpl');
      WriteLn(Engine.Render(Ctx));
    finally
      for Index:= 0 to Pred(Ctx.Count) do
        Ctx.Data[Index].Free;
      Ctx.Free;
    end;
  finally
    Engine.Free;
  end;
end.

