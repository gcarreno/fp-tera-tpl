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
  public
    function AsString: string; virtual; abstract;
  end;

{ TStringValue }
  TStringValue = class(TTemplateValue)
    Value: string;
    constructor Create(const AValue: string);
    function AsString: string; override;
  end;

{ TContext }
  TContext = specialize TFPGMap<string, TTemplateValue>;

implementation

{ TStringValue }

constructor TStringValue.Create(const AValue: string);
begin
  inherited Create;
  Value := AValue;
end;

function TStringValue.AsString: string;
begin
  Result := Value;
end;

end.