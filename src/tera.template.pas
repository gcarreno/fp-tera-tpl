unit Tera.Template;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Tera.Context
, Tera.Filters
;

type
{ TTemplateEngine }
  TTemplateEngine = class
  private
    FBaseDir: String;

    function ReadTemplateFile(const AFileName: String): String;
    function RenderBlock(const ALines: TStringList; var AIndex: Integer; ACtx: TContext): String;
    function EvalExpr(const AExpr: String; ACtx: TContext): Boolean;
    function ReplaceVars(const ALine: String; ACtx: TContext): String;
  public
    constructor Create(const ABaseDir: String);

    function Render(const ATemplateName: String; ACtx: TContext): String;

    property BaseDir: String
      read FBaseDir;
  end;

implementation

type
  TInstructionType = (itUnknown, itInclude, itIf, itEndIf, itFor, itEndFor);

{ TTemplateEngine }

constructor TTemplateEngine.Create(const ABaseDir: String);
begin
  FBaseDir := ExpandFileName(IncludeTrailingPathDelimiter(ABaseDir));
end;

function TTemplateEngine.ReadTemplateFile(const AFileName: String): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FBaseDir + AFileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TTemplateEngine.EvalExpr(const AExpr: String; ACtx: TContext): Boolean;
var
  Left, Right: String;
  PosEq: Integer;
begin
  { #todo -ogcarreno : Implement all comparisons }
  PosEq:= Pos('==', AExpr);
  if PosEq > 0 then
  begin
    Left := Trim(Copy(AExpr, 1, PosEq - 1));
    Right := Trim(Copy(AExpr, PosEq + 2, Length(AExpr)));
    if (ACtx.IndexOf(Left) >= 0) then
      Result := ACtx.KeyData[Left].AsString = StringReplace(Right, '"', '', [rfReplaceAll])
    else
      Result := False;
  end
  else
    Result := False;
end;

function TTemplateEngine.ReplaceVars(const ALine: String; ACtx: TContext): String;
var
  i, j, startVar, endVar: Integer;
  Output, Key, Filtered, VarName: string;
  PipeParts: TStringList;
  FilterFunc: TFilterFunc;
begin
  i := 1;
  Output := '';
  while i <= Length(ALine) do
  begin
    if (i < Length(ALine)) and (ALine[i] = '{') and (ALine[i+1] = '{') then
    begin
      Inc(i, 2);
      startVar := i;
      while (i <= Length(ALine)) and not ((ALine[i] = '}') and (ALine[i+1] = '}')) do
        Inc(i);
      endVar := i;
      Key := Trim(Copy(ALine, startVar, endVar - startVar));
      Inc(i, 2);

      // Apply filters
      PipeParts := TStringList.Create;
      try
        PipeParts.Delimiter := '|';
        PipeParts.StrictDelimiter := True;
        PipeParts.DelimitedText := Key;

        VarName := Trim(PipeParts[0]);
        if ACtx.IndexOf(VarName) >= 0 then
          Filtered := ACtx.KeyData[VarName].AsString
        else
          Filtered := '';

        for j := 1 to Pred(PipeParts.Count) do
        begin
          if FilterRegistry.TryGetData(Trim(PipeParts[j]), FilterFunc) then
            Filtered := FilterFunc(Filtered)
          else
            Filtered += ' | UnknownFilter:' + PipeParts[j];
        end;

        Output += Filtered;
      finally
        PipeParts.Free;
      end;
    end
    else
    begin
      Output += ALine[i];
      Inc(i);
    end;
  end;
  Result := Output;
end;

function TTemplateEngine.RenderBlock(const ALines: TStringList;
  var AIndex: Integer; ACtx: TContext): String;
var
  Line, Tmp, VarName, ListName, Item: String;
  SubCtx: TContext;
  SubList: TStringList;
  Cond: Boolean;
  Buf: TStringList;
  lIndex: Integer;
  InstructionType: TInstructionType;
begin
  Result := '';
  Buf := TStringList.Create;
  try
    while AIndex < ALines.Count do
    begin
      Line := Trim(ALines[AIndex]);

      if Line.StartsWith('{% include "') then
        InstructionType:= itInclude
      else
        if Line.StartsWith('{% if ') then
          InstructionType:= itIf
        else
          if Line.StartsWith('{% endif %}') then
            InstructionType:= itEndIf
          else
            if Line.StartsWith('{% for ') then
              InstructionType:= itFor
            else
              if Line.StartsWith('{% endfor %}') then
                InstructionType:= itEndFor
              else
                InstructionType:= itUnknown;

      case InstructionType of
        itInclude:
        begin
          Tmp := Copy(Line, 13, Length(Line) - 16);
          Result += Render(Tmp, ACtx);
        end;
        itIf: // <=============================== The offending one
        begin
          Cond := EvalExpr(Copy(Line, 7, Length(Line) - 9), ACtx);
          Inc(AIndex);
          if Cond then
          begin
            Tmp:= RenderBlock(ALines, AIndex, ACtx);
            if Tmp.Contains('{% else %}') then
            begin
              Delete(Tmp, Pos('{% else %}', Tmp), Length(Tmp));
            end;
            Result += Tmp;
          end
          else
          begin
            while not ALines[AIndex].Trim.StartsWith('{% else %}') do Inc(AIndex);
            Inc(Aindex);
            Result += RenderBlock(ALines, AIndex, ACtx);
          end;
        end;  // ===============================> The offending one
        itFor:
        begin
          Tmp := Copy(Line, 8, Length(Line) - 10);
          VarName := Trim(Copy(Tmp, 1, Pos(' in ', Tmp) - 1));
          ListName := Trim(Copy(Tmp, Pos(' in ', Tmp) + 4, Length(Tmp)));

          if ACtx.IndexOf(ListName) >= 0 then
          begin
            SubList := ACtx.KeyData[ListName].AsArray;
            Inc(AIndex);
            Buf.Clear;
            while not ALines[AIndex].Trim.StartsWith('{% endfor %}') do
            begin
              Buf.Add(ALines[AIndex]);
              Inc(AIndex);
            end;
            for Item in SubList do
            begin
              SubCtx := TContext.Create;
              try
                SubCtx.Add(VarName, TStringValue.Create(Item));
                lIndex:= 0;
                Result += RenderBlock(Buf, lIndex, SubCtx);
              finally
                for lIndex:= 0 to Pred(SubCtx.Count) do
                  SubCtx.Data[lIndex].Free;
                SubCtx.Free;
              end;
            end;
          end;
          while not ALines[AIndex].Trim.StartsWith('{% endfor %}') do Inc(AIndex);
        end;
        itEndIf, itEndFor:
        begin
          Break;
        end;
        otherwise
          Result += ReplaceVars(ALines[AIndex], ACtx) + LineEnding;
      end;

      Inc(AIndex);
    end;
  finally
    Buf.Free;
  end;
end;

function TTemplateEngine.Render(const ATemplateName: String; ACtx: TContext): String;
var
  Lines: TStringList;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ReadTemplateFile(ATemplateName);
    i := 0;
    Result := RenderBlock(Lines, i, ACtx);
  finally
    Lines.Free;
  end;
end;

end.
