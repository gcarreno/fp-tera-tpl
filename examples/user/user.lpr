program user;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  { you can add units after this }
  Tera.Template,
  Tera.Context;

var
  Engine: TTemplateEngine;
  Ctx: TContext;
  Index: Integer;
begin
  Engine := TTemplateEngine.Create('templates');
  try
    Ctx := TContext.Create;
    try
      Ctx.Add('name', TStringValue.Create('Bob'));
      Ctx.Add('role', TStringValue.Create('Pascal Wizard'));

      WriteLn(Engine.Render('user.tpl', Ctx));
    finally
      for Index:= 0 to Pred(Ctx.Count) do
        TTemplateValue(Ctx.Data[Index]).Free;
      Ctx.Free;
    end;
  finally
    Engine.Free;
  end;

end.

