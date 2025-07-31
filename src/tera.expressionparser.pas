unit Tera.ExpressionParser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
, Classes
, Variants
, Tera.Context
;

type
  TExpressionNodeType = (entBinary, entUnary, entLiteral, entVariable);

  TBinaryOperator = (boEqual, boNotEqual, boAnd, boOr, boLess, boLessEqual,
                     boGreater, boGreaterEqual);
  TUnaryOperator = (uoNot);

{ TExpressionNode }
  TExpressionNode = class(TObject)
  private
    FNodeType: TExpressionNodeType;
    FBinOp: TBinaryOperator;
    FLeft: TExpressionNode;
    FRight: TExpressionNode;
    FUniOp: TUnaryOperator;
    FOperand: TExpressionNode;
    FLiteralValue: String;
    FVariableName: String;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    property NodeType: TExpressionNodeType read FNodeType write FNodeType;
    property BinOp: TBinaryOperator read FBinOp write FBinOp;
    property Left: TExpressionNode read FLeft write FLeft;
    property Right: TExpressionNode read FRight write FRight;
    property UniOp: TUnaryOperator read FUniOp write FUniOp;
    property Operand: TExpressionNode read FOperand write FOperand;
    property LiteralValue: String read FLiteralValue write FLiteralValue;
    property VariableName: String read FVariableName write FVariableName;
  published
  end;

{ TExpressionParser }
  TExpressionParser = class
  private
    FInput: String;
    FPos: Integer;

    function PeekChar: Char;
    function NextChar: Char;
    function Match(const AString: String): Boolean;
    function SkipWhitespace: Boolean;
    function ParsePrimary: TExpressionNode;
    function ParseUnary: TExpressionNode;
    function ParseComparison: TExpressionNode;
    function ParseEquality: TExpressionNode;
    function ParseLogical: TExpressionNode;
  public
    function Parse(const AString: String): TExpressionNode;
    function Evaluate(const ANode: TExpressionNode; const AContext: TContext): Variant;
  end;

implementation

{ TExpressionNode }

constructor TExpressionNode.Create;
begin
  FLeft:= nil;
  FRight:= nil;
  FOperand:= nil;
end;

destructor TExpressionNode.Destroy;
begin
  if Assigned(FLeft) then
    FLeft.Free;

  if Assigned(FRight) then
    FRight.Free;

  inherited Destroy;
end;

function TExpressionParser.PeekChar: Char;
begin
  if FPos > Length(FInput) then Exit(#0);
  Result := FInput[FPos];
end;

function TExpressionParser.NextChar: Char;
begin
  if FPos > Length(FInput) then Exit(#0);
  Result := FInput[FPos];
  Inc(FPos);
end;

function TExpressionParser.Match(const AString: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(AString);
  if Copy(FInput, FPos, Len) = AString then
  begin
    Inc(FPos, Len);
    Result := True;
  end
  else
    Result := False;
end;

function TExpressionParser.SkipWhitespace: Boolean;
begin
  Result := False;
  while PeekChar in [' ', #9, #10, #13] do
  begin
    NextChar;
    Result := True;
  end;
end;

function TExpressionParser.ParsePrimary: TExpressionNode;
var
  C: Char;
  Value: String;
  Node: TExpressionNode;
begin
  SkipWhitespace;
  C := PeekChar;

  if C = '"' then
  begin
    NextChar; // Skip "
    Value := '';
    while PeekChar <> '"' do
    begin
      Value += NextChar;
      if PeekChar = #0 then raise Exception.Create('unterminated string literal');
    end;
    NextChar; // Skip closing "
    Node := TExpressionNode.Create;
    Node.NodeType := entLiteral;
    Node.LiteralValue := Value;
    Exit(Node);
  end
  else if C in ['0'..'9'] then
  begin
    Value := '';
    while PeekChar in ['0'..'9', '.'] do
      Value += NextChar;
    Node := TExpressionNode.Create;
    Node.NodeType := entLiteral;
    Node.LiteralValue := Value;
    Exit(Node);
  end
  else if C = '(' then
  begin
    NextChar;
    Node := ParseLogical;
    SkipWhitespace;
    if PeekChar <> ')' then
      raise Exception.Create('expected ")"');
    NextChar;
    Exit(Node);
  end
  else
  begin
    Value := '';
    while PeekChar in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.'] do
      Value += NextChar;
    if Value = '' then raise Exception.Create('expected variable or value');
    Node := TExpressionNode.Create;
    Node.NodeType := entVariable;
    Node.VariableName := Value;
    Exit(Node);
  end;
end;

function TExpressionParser.ParseUnary: TExpressionNode;
begin
  SkipWhitespace;
  if Match('not') then
  begin
    Result := TExpressionNode.Create;
    Result.NodeType := entUnary;
    Result.UniOp := uoNot;
    Result.Operand := ParseUnary;
    Result.Right:= ParsePrimary;
  end
  else
    Result := ParsePrimary;
end;

function TExpressionParser.ParseComparison: TExpressionNode;
var
  Left, Right: TExpressionNode;
  Op: TBinaryOperator;
begin
  Left := ParseUnary;
  SkipWhitespace;

  if Match('<=') then Op := boLessEqual
  else if Match('>=') then Op := boGreaterEqual
  else if Match('<') then Op := boLess
  else if Match('>') then Op := boGreater
  else Exit(Left);

  Right := ParseUnary;

  Result := TExpressionNode.Create;
  Result.NodeType := entBinary;
  Result.BinOp := Op;
  Result.Left := Left;
  Result.Right := Right;
end;

function TExpressionParser.ParseEquality: TExpressionNode;
var
  Left, Right, NextNode: TExpressionNode;
  Op: TBinaryOperator;
begin
  Left := ParseComparison;
  SkipWhitespace;
  while True do
  begin
    if Match('==') then Op:= boEqual
    else if Match('!=') then Op:= boNotEqual
    else Break;
    Right := ParseComparison;
    NextNode := TExpressionNode.Create;
    NextNode.NodeType := entBinary;
    NextNode.BinOp := Op;
    NextNode.Left := Left;
    NextNode.Right := Right;
    Left := NextNode;
    SkipWhitespace;
  end;
  Result := Left;
end;

function TExpressionParser.ParseLogical: TExpressionNode;
var
  Left, Right, NextNode: TExpressionNode;
  Op: TBinaryOperator;
begin
  Left := ParseEquality;
  SkipWhitespace;
  while True do
  begin
    if Match('&&') then Op := boAnd
    else if Match('||') then Op := boOr
    else Break;
    Right := ParseEquality;
    NextNode := TExpressionNode.Create;
    NextNode.NodeType := entBinary;
    NextNode.BinOp := Op;
    NextNode.Left := Left;
    NextNode.Right := Right;
    Left := NextNode;
    SkipWhitespace;
  end;
  Result := Left;
end;

function TExpressionParser.Parse(const AString: String): TExpressionNode;
begin
  FInput := AString;
  FPos := 1;
  if Pos('=', FInput) > 0 then
  begin
    if (not (Pos('==', FInput) > 0)) and
       (not (Pos('!=', FInput) > 0)) and
       (not (Pos('>=', FInput) > 0)) and
       (not (Pos('<=', FInput) > 0)) then
      raise Exception.Create('assignment "=" not implemented')
    else
      Result := ParseLogical;
  end
  else
    Result := ParseLogical;
end;

function TExpressionParser.Evaluate(const ANode: TExpressionNode;
  const AContext: TContext): Variant;

  function GetVarValue(const AName: String): Variant;
  var
    lIndex: Integer;
  begin
    lIndex := AContext.IndexOf(AName);
    if lIndex = -1 then raise Exception.Create('unknown variable: ' + AName);
    Result := AContext.Data[lIndex].AsString;
  end;

  function StrToNumber(const S: String): Variant;
  begin
    if Pos('.', S) > 0 then Result := StrToFloat(S)
    else Result := StrToInt(S);
  end;

begin
  case ANode.NodeType of
    entLiteral: Result := ANode.LiteralValue;
    entVariable: Result := GetVarValue(ANode.VariableName);
    entUnary:
      case ANode.UniOp of
        uoNot: Result := not Evaluate(ANode.Operand, AContext);
      end;
    entBinary:
      case ANode.BinOp of
        boEqual: Result := Evaluate(ANode.Left, AContext) = Evaluate(ANode.Right, AContext);
        boNotEqual: Result := Evaluate(ANode.Left, AContext) <> Evaluate(ANode.Right, AContext);
        boAnd: Result := Evaluate(ANode.Left, AContext) and Evaluate(ANode.Right, AContext);
        boOr: Result := Evaluate(ANode.Left, AContext) or Evaluate(ANode.Right, AContext);
        boLess: Result := Evaluate(ANode.Left, AContext) < Evaluate(ANode.Right, AContext);
        boLessEqual: Result := Evaluate(ANode.Left, AContext) <= Evaluate(ANode.Right, AContext);
        boGreater: Result := Evaluate(ANode.Left, AContext) > Evaluate(ANode.Right, AContext);
        boGreaterEqual: Result := Evaluate(ANode.Left, AContext) >= Evaluate(ANode.Right, AContext);
      end;
  //otherwise
  //  raise Exception.Create('unknown node type');
  end;
end;

end.

