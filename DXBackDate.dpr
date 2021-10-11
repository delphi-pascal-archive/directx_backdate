program DXBackDate;

uses
  Forms,
  DXBackDateU_Ger in 'DXBackDateU_Ger.pas' {DXBackForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDXBackForm, DXBackForm);
  Application.Run;
end.
