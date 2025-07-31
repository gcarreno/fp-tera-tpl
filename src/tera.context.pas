unit Tera.Context;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fgl
;

type
{ TTemplateValue }
  TTemplateValue = class
  private
  protected
  public
    function AsString: String; virtual; abstract;
    function AsArray: TStringList; virtual;
  published
  end;

{ TStringValue }
  TStringValue = class(TTemplateValue)
    private
    protected
      FValue: String;
    public
    constructor Create(const AValue: String);
    function AsString: String; override;

    property Value: String
      read FValue;
    published
  end;

{ TArrayValue }
  TArrayValue = class(TTemplateValue)
  private
  protected
    FItems: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function AsArray: TStringList; override;
    function AsString: String; override;

    property Items: TStringList
      read FItems;
  published
  end;

{ TContext }
  TContext = specialize TFPGMap<String, TTemplateValue>;

implementation

{ TTemplateValue }

function TTemplateValue.AsArray: TStringList;
begin
  Result:= nil;
  raise Exception.Create('not an array');
end;

{ TStringValue }

constructor TStringValue.Create(const AValue: String);
begin
  inherited Create;
  FValue:= AValue;
end;

function TStringValue.AsString: String;
begin
  Result := Value;
end;

{ TArrayValue }

constructor TArrayValue.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TArrayValue.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TArrayValue.AsArray: TStringList;
begin
  Result:= FItems;
end;

function TArrayValue.AsString: String;
begin
  Result := FItems.Text;
end;

end.
