unit Tera.Template;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Tera.Context
;

type
{ TTemplateEngine }
  TTemplateEngine = class
  private
    FBaseDir: string;
    function EvalTemplate(const Content: string; Ctx: TContext): string;
    function ReadTemplateFile(const FileName: string): string;
  public
    constructor Create(const ABaseDir: string);
    function Render(const TemplateName: string; Ctx: TContext): string;
  end;

implementation

{ TTemplateEngine }

constructor TTemplateEngine.Create(const ABaseDir: string);
begin
  FBaseDir := ExpandFileName(IncludeTrailingPathDelimiter(ABaseDir));
end;

function TTemplateEngine.ReadTemplateFile(const FileName: string): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FBaseDir + FileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TTemplateEngine.EvalTemplate(const Content: string; Ctx: TContext): string;
var
  i, startVar, endVar: Integer;
  Output, Key: string;
begin
  i := 1;
  Output := '';
  while i <= Length(Content) do
  begin
    if (i < Length(Content)) and (Content[i] = '{') and (Content[i+1] = '{') then
    begin
      Inc(i, 2);
      startVar := i;
      while (i <= Length(Content)) and not ((Content[i] = '}') and (Content[i+1] = '}')) do
        Inc(i);
      endVar := i;
      Key := Trim(Copy(Content, startVar, endVar - startVar));
      if Ctx.IndexOf(Key) >= 0 then
        Output += Ctx.KeyData[Key].AsString
      else
        Output += '{{' + Key + '}}'; // fallback: leave as-is
      Inc(i, 2);
    end
    else
    begin
      Output += Content[i];
      Inc(i);
    end;
  end;
  Result := Output;
end;

function TTemplateEngine.Render(const TemplateName: string; Ctx: TContext): string;
var
  Raw: string;
begin
  Raw := ReadTemplateFile(TemplateName);
  Result := EvalTemplate(Raw, Ctx);
end;

end.
