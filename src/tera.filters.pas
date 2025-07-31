unit Tera.Filters;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fgl
;

type
  TFilterFunc = function(const AString: String): String;
  TFilterMap = specialize TFPGMap<String, TFilterFunc>;

var
  FilterRegistry: TFilterMap;

procedure RegisterDefaultFilters;
procedure RegisterFilter(const AName: String; Func: TFilterFunc);

implementation

function FunctionUpper(const S: String): String;
begin
  Result := UpperCase(S);
end;

function FunctionLower(const S: String): String;
begin
  Result := LowerCase(S);
end;

function FunctionTrim(const S: String): String;
begin
  Result := Trim(S);
end;

function FunctionTrimLeft(const S: String): String;
begin
  Result := TrimLeft(S);
end;

function FunctionTrimRight(const S: String): String;
begin
  Result := TrimRight(S);
end;

function FunctionLength(const S: String): String;
begin
  Result := IntToStr(Length(S));
end;

procedure RegisterFilter(const AName: String; Func: TFilterFunc);
begin
  if FilterRegistry.IndexOf(AName) = -1 then
    FilterRegistry.Add(AName, Func);
end;

procedure RegisterDefaultFilters;
begin
  RegisterFilter('upper', @FunctionUpper);
  RegisterFilter('lower', @FunctionLower);
  RegisterFilter('trim', @FunctionTrim);
  RegisterFilter('trimleft', @FunctionTrimLeft);
  RegisterFilter('trimright', @FunctionTrimRight);
  RegisterFilter('length', @FunctionLength);
end;

initialization
  FilterRegistry := TFilterMap.Create;
  RegisterDefaultFilters;

finalization
  FilterRegistry.Free;

end.
