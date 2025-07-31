unit Tera.Engine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, Tera.Context
, Tera.Filters
, Tera.ExpressionParser
;

type
{ TNodeKind }
  TNodeKind = (
    nkRoot,
    nkBlock,
    nkText, nkVariable,
    nkInclude,
    nkIf, nkElse, nkEndIf,
    nkFor, nkEndFor
  );

{ TToken }
  TToken = record
    Kind: TNodeKind;
    Content: String;
  end;

{ TTokenArray }
  TTokenArray = specialize TArray<TToken>;

// Forward
  TASTNode = class;

{ TASTNodeArray }
  TASTNodeArray = specialize TArray<TASTNode>;

{ TASTNode }
  TASTNode = class(TObject)
  private
    FKind: TNodeKind;
    FContent: String;
    FChildren: TASTNodeArray;
  protected
  public
    constructor Create(const AKind: TNodeKind; const AContent: String = '');
    destructor Destroy; override;

    procedure AddChild(const ANode: TASTNode);

    property Kind: TNodeKind read FKind write FKind;
    property Content: String read FContent write FContent;
    property Children: TASTNodeArray read FChildren write FChildren;
  published
  end;

{ TTeraEngine }
  TTeraEngine = class(TObject)
  private
    FBaseDir: String;
    FRoot: TASTNode;

    function LoadTemplateFile(const AFileName: String): String;
    function Tokenize(const Input: String): TTokenArray;
    function ParseTokens(const ATokens: TTokenArray): TASTNode;
    function RenderAST(const ANode: TASTNode; const AContext: TContext): String;
    function ReplaceVars(const ALine: String; AContext: TContext): String;
  public
    constructor Create(const ABaseDir: String);
    destructor Destroy; override;

    procedure Parse(const ATemplateName: String);
    function Render(const AContext: TContext): String;

    property BaseDir: String
      read FBaseDir;
    property Root: TASTNode
      read FRoot;
  end;



implementation

uses
  TypInfo
;

{ TASTNode }

constructor TASTNode.Create(const AKind: TNodeKind; const AContent: String);
begin
  FKind := AKind;
  FContent := AContent;
  SetLength(FChildren, 0);
end;

destructor TASTNode.Destroy;
var
  lIndex: Integer;
begin
  for lIndex := 0 to High(FChildren) do
    FChildren[lIndex].Free;
  inherited Destroy;
end;

procedure TASTNode.AddChild(const ANode: TASTNode);
begin
  SetLength(FChildren, Length(FChildren) + 1);
  FChildren[High(FChildren)] := ANode;
end;

{ TTeraEngine }

constructor TTeraEngine.Create(const ABaseDir: String);
begin
  FBaseDir := ExpandFileName(IncludeTrailingPathDelimiter(ABaseDir));
end;

destructor TTeraEngine.Destroy;
begin
  Root.Free;
  inherited Destroy;
end;

function TTeraEngine.LoadTemplateFile(const AFileName: String): String;
var
  SL: TStringList;
begin
  if not FileExists(FBaseDir + AFileName) then
    raise Exception.CreateFmt('include file not found: "%s"', [
      FBaseDir + AFileName
    ]);

  SL := TStringList.Create;
  try
    SL.LoadFromFile(FBaseDir + AFileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TTeraEngine.Tokenize(const Input: String): TTokenArray;
var
  lIndex: Integer;
  lState: (stText, stTag);
  lTagStart, lTagEnd, lBuffer, lTagContent: String;
  lToken: TToken;
  lTokens: TTokenArray = nil;

  procedure EmitToken(const AKind: TNodeKind; const AValue: String);
  begin
    lToken.Kind := AKind;
    lToken.Content := AValue;
    SetLength(lTokens, Length(lTokens) + 1);
    lTokens[High(lTokens)] := lToken;
  end;

begin
  lIndex := 1;
  lBuffer := '';
  lState := stText;
  //SetLength(lTokens, 0);

  while lIndex <= Length(Input) do
  begin
    if lState = stText then
    begin
      if (Input[lIndex] = '{') and (lIndex + 1 <= Length(Input)) and ((Input[lIndex + 1] = '%') or (Input[lIndex + 1] = '{')) then
      begin
        // Emit any buffered text
        if lBuffer <> '' then
        begin
          EmitToken(nkText, lBuffer);
          lBuffer := '';
        end;

        // Start tag
        if Input[lIndex + 1] = '%' then
        begin
          lTagStart := '{%';
          lTagEnd := '%}';
        end
        else
        begin
          lTagStart := '{{';
          lTagEnd := '}}';
        end;

        lState := stTag;
        Inc(lIndex, 2); // Skip '{%' or '{{'
        lTagContent := '';
      end
      else
      begin
        lBuffer := lBuffer + Input[lIndex];
        Inc(lIndex);
      end;
    end
    else if lState = stTag then
    begin
      if (Input[lIndex] = lTagEnd[1]) and (lIndex + 1 <= Length(Input)) and (Input[lIndex + 1] = lTagEnd[2]) then
      begin
        // Close tag
        lTagContent := Trim(lTagContent);

        if lTagStart = '{{' then
          EmitToken(nkVariable, lTagContent)
        else
        begin
          if Pos('if ', lTagContent) = 1 then
            EmitToken(nkIf, Trim(Copy(lTagContent, 4, Length(lTagContent))))
          else if lTagContent = 'else' then
            EmitToken(nkElse, '')
          else if lTagContent = 'endif' then
            EmitToken(nkEndIf, '')
          else if Pos('for ', lTagContent) = 1 then
            EmitToken(nkFor, Trim(Copy(lTagContent, 5, Length(lTagContent))))
          else if lTagContent = 'endfor' then
            EmitToken(nkEndFor, '')
          else if Pos('include ', lTagContent) = 1 then
          begin
            // crude "quote strip"
            lBuffer := Trim(Copy(lTagContent, 9, Length(lTagContent)));
            lBuffer := StringReplace(lBuffer, '"', '', [rfReplaceAll]);
            lBuffer := StringReplace(lBuffer, '''', '', [rfReplaceAll]);
            EmitToken(nkInclude, lBuffer);
            lBuffer:= '';
          end
          else
            EmitToken(nkText, '{%' + lTagContent + '%}'); // Unknown tag fallback
        end;

        lState := stText;
        Inc(lIndex, 2); // Skip '%}' or '}}'
      end
      else
      begin
        lTagContent := lTagContent + Input[lIndex];
        Inc(lIndex);
      end;
    end;
  end;

  // Final flush
  if (lBuffer <> '') and (lState = stText) then
    EmitToken(nkText, lBuffer);

  Result:= lTokens;
end;

function TTeraEngine.ParseTokens(const ATokens: TTokenArray): TASTNode;
var
  lIndex: Integer;
  lRoot, lCurrentNode, lNewNode: TASTNode;
  lToken: TToken;

   type
     TNodeKindSet = set of TNodeKind;

  function ParseBlock(EndKinds: TNodeKindSet): TASTNode;
  var
    lBlockNode, lChild: TASTNode;
    lTokens: TTokenArray;
    lInput: String;
  begin
    lBlockNode := TASTNode.Create(nkBlock);
    while (lIndex <= High(ATokens)) and not (ATokens[lIndex].Kind in EndKinds) do
    begin
      case ATokens[lIndex].Kind of
        nkText:
          begin
            lChild := TASTNode.Create(nkText);
            lChild.Content := ATokens[lIndex].Content;
            SetLength(lBlockNode.FChildren, Length(lBlockNode.FChildren) + 1);
            lBlockNode.FChildren[High(lBlockNode.FChildren)]:= lChild;
            Inc(lIndex);
          end;
        nkVariable:
          begin
            lChild := TASTNode.Create(nkVariable);
            lChild.Content := ATokens[lIndex].Content;
            SetLength(lBlockNode.FChildren, Length(lBlockNode.FChildren) + 1);
            lBlockNode.FChildren[High(lBlockNode.FChildren)]:= lChild;
            Inc(lIndex);
          end;
        nkInclude:
          begin
            lChild := TASTNode.Create(nkInclude);
            lChild.Content := ATokens[lIndex].Content;
            lInput:= LoadTemplateFile(lChild.Content);
            lTokens:= Tokenize(lInput);
            lNewNode:= ParseTokens(lTokens);
            SetLength(lChild.FChildren, Length(lChild.FChildren) + 1);
            lChild.FChildren[High(lChild.FChildren)]:= lNewNode;
            SetLength(lBlockNode.FChildren, Length(lBlockNode.FChildren) + 1);
            lBlockNode.FChildren[High(lBlockNode.FChildren)]:= lChild;
            Inc(lIndex);
          end;
        nkIf:
          begin
            lChild := TASTNode.Create(nkIf);
            lChild.Content := ATokens[lIndex].Content;
            Inc(lIndex);
            // Parse true branch
            lNewNode:= ParseBlock([nkElse, nkEndIf]);
            SetLength(lChild.FChildren, Length(lChild.FChildren) + 1);
            lChild.FChildren[High(lChild.FChildren)]:= lNewNode;

            // Check for else
            if (lIndex <= High(ATokens)) and (ATokens[lIndex].Kind = nkElse) then
            begin
              Inc(lIndex);
              lNewNode := ParseBlock([nkEndIf]);
              SetLength(lChild.FChildren, Length(lChild.FChildren) + 1);
              lChild.FChildren[High(lChild.FChildren)]:= lNewNode;
            end;

            // Expect endif
            if (lIndex <= High(ATokens)) and (ATokens[lIndex].Kind = nkEndIf) then
              Inc(lIndex)
            else
              raise Exception.Create('Expected {% endif %}');

            SetLength(lBlockNode.FChildren, Length(lBlockNode.FChildren) + 1);
            lBlockNode.FChildren[High(lBlockNode.FChildren)]:= lChild;
          end;
        nkFor:
          begin
            lChild := TASTNode.Create(nkFor);
            lChild.Content := ATokens[lIndex].Content;
            Inc(lIndex);
            lNewNode:= ParseBlock([nkEndFor]);
            SetLength(lChild.FChildren, Length(lChild.FChildren) + 1);
            lChild.FChildren[High(lChild.FChildren)]:= lNewNode;
            if (lIndex <= High(ATokens)) and (ATokens[lIndex].Kind = nkEndFor) then
              Inc(lIndex)
            else
              raise Exception.Create('Expected {% endfor %}');
            SetLength(lBlockNode.FChildren, Length(lBlockNode.FChildren) + 1);
            lBlockNode.FChildren[High(lBlockNode.FChildren)]:= lChild;
          end
        else
          raise Exception.Create('Unexpected token in block: ' + GetEnumName(TypeInfo(TNodeKind), Ord(ATokens[lIndex].Kind)));
      end;
    end;
    Result := lBlockNode;
  end;

begin
  lIndex := 0;
  lRoot := ParseBlock([]);
  Result := lRoot;
end;

function TTeraEngine.RenderAST(const ANode: TASTNode;
  const AContext: TContext): String;
var
  lSubNode: TASTNode = nil;
  lVarName, lListName, lListEntry: String;
  lContext: TContext;
  lExpressionParser: TExpressionParser;
  lExpressionNode: TExpressionNode;
  lIndex: Integer;
begin
  Result:= '';

  case ANode.FKind of
    nkBlock:
    begin
      for lSubNode in ANode.FChildren do
        Result += RenderAST(lSubNode, AContext);
    end;
    nkInclude:
    begin
      for lSubNode in ANode.FChildren do
        Result += RenderAST(lSubNode, AContext);
    end;
    nkIf:
    begin
      lExpressionParser:= TExpressionParser.Create;
      try
        lExpressionNode:= lExpressionParser.Parse(ANode.FContent);
        try
          if lExpressionParser.Evaluate(lExpressionNode, AContext) then
          begin
            for lIndex:= 0 to High(ANode.FChildren[0].FChildren) do
            begin
              lSubNode:= ANode.FChildren[0].FChildren[lIndex];
              if (lIndex = 0) and (lSubNode.FKind = nkText) then
                lSubNode.FContent:= TrimLeft(lSubNode.FContent);
              Result += RenderAST(lSubNode, AContext);
            end;
          end
          else
          begin
            if Length(ANode.FChildren) > 1 then
            begin
              for lIndex:= 0 to High(ANode.FChildren[1].FChildren) do
              begin
                lSubNode:= ANode.FChildren[1].FChildren[lIndex];
                if (lIndex = 0) and (lSubNode.FKind = nkText) then
                  lSubNode.FContent:= TrimLeft(lSubNode.FContent);
                Result += RenderAST(lSubNode, AContext);
              end;
            end;
          end;
        finally
          lExpressionNode.Free;
        end;
      finally
        lExpressionParser.Free;
      end;
    end;
    nkFor:
    begin
      lVarName := Trim(Copy(ANode.FContent, 1, Pos(' in ', ANode.FContent) - 1));
      lListName := Trim(Copy(ANode.FContent, Pos(' in ', ANode.FContent) + 4, Length(ANode.FContent)));
      lIndex:= AContext.IndexOf(lListName);
      if lIndex >= 0 then
      begin
        for lListEntry in TArrayValue(AContext.Data[AContext.IndexOf(lListName)]).Items do
        begin
          lContext:= TContext.Create;
          try
            lContext.Add(lVarName, TStringValue.Create(lListEntry));
            for lIndex:= 0 to High(ANode.FChildren[0].FChildren) do
            begin
              lSubNode:= ANode.FChildren[0].FChildren[lIndex];
              if (lIndex = 0) and (lSubNode.FKind = nkText) then
                lSubNode.FContent:= TrimLeft(lSubNode.FContent);
              Result += RenderAST(lSubNode, lContext);
            end;
          finally
            for lIndex:= 0 to Pred(lContext.Count) do
              lContext.Data[lIndex].Free;
            lContext.Free;
          end;
        end;
      end;
    end;
    nkText:
    begin
      Result += ANode.FContent;
    end;
    nkVariable:
    begin
      Result += ReplaceVars(ANode.FContent, AContext);
    end;
    otherwise
      // Do nothing
  end;
end;

function TTeraEngine.ReplaceVars(const ALine: String;
  AContext: TContext): String;
var
  j: Integer;
  Output, Filtered, VarName: string;
  PipeParts: TStringList;
  FilterFunc: TFilterFunc;
begin
  Output := '';

  // Apply filters
  PipeParts := TStringList.Create;
  try
    PipeParts.Delimiter := '|';
    PipeParts.StrictDelimiter := True;
    PipeParts.DelimitedText := ALine;

    VarName := Trim(PipeParts[0]);
    if AContext.IndexOf(VarName) >= 0 then
      Filtered := AContext.KeyData[VarName].AsString
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
  Result := Output;
end;

procedure TTeraEngine.Parse(const ATemplateName: String);
var
  lInput: String;
  lTokens: TTokenArray;
begin
  lInput:= LoadTemplateFile(ATemplateName);
  lTokens:= Tokenize(lInput);
  FRoot:= ParseTokens(lTokens);
end;

function TTeraEngine.Render(const AContext: TContext): String;
begin
  Result:= RenderAST(FRoot, AContext);
end;

end.

