program MFB;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  StartFrame in 'StartFrame.pas' {frmStartPage: TFrame},
  OpenFirmware in 'OpenFirmware.pas' {frmOpenFirmware: TFrame},
  Vcl.Themes,
  Vcl.Styles,
  CompileFirmware in 'CompileFirmware.pas' {frmCompileFirmware: TFrame},
  FWZipConsts in 'fwzip\FWZipConsts.pas',
  FWZipCrc32 in 'fwzip\FWZipCrc32.pas',
  FWZipCrypt in 'fwzip\FWZipCrypt.pas',
  FWZipReader in 'fwzip\FWZipReader.pas',
  FWZipStream in 'fwzip\FWZipStream.pas',
  FWZipWriter in 'fwzip\FWZipWriter.pas',
  ScriptFrame in 'ScriptFrame.pas' {frmScripts: TFrame},
  HelpUnit in 'HelpUnit.pas' {HelpForm},
  FinishFrame in 'FinishFrame.pas' {frmFinish: TFrame},
  CryModule in 'CryModule.pas' {CryMode},
  GitHubChooiser in 'GitHubChooiser.pas' {GitHubRepositories: TFrame},
  CRCunit in 'CRCunit.pas',
  NativeXml in 'NativeXml402\nativexml\NativeXml.pas',
  sdDebug in 'NativeXml402\general\sdDebug.pas',
  sdSortedLists in 'NativeXml402\general\sdSortedLists.pas',
  sdStreams in 'NativeXml402\general\sdStreams.pas',
  sdStringTable in 'NativeXml402\general\sdStringTable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title:='MIUI Firmware Builder';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.CreateForm(TCryMode, CryMode);
  Application.Run;
end.
