program testteratpl;

{$mode objfpc}{$H+}

uses
  Interfaces
, Forms
, GuiTestRunner
, TestTeraFilters, TestTeraContext, TestTeraTemplate
;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

