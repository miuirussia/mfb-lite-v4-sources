unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, rkVistaPanel, rkAeroTabs,
  Vcl.ComCtrls, Vcl.ExtCtrls, dxGDIPlusClasses, TaskBar, System.Win.Registry, System.IniFiles,
  Vcl.Buttons, W7Classes, W7Images, UbuntuProgress, AppLauncher;

type
  TMainForm = class(TForm)
    lblInfoText: TLabel;
    pgcMainControl: TPageControl;
    TaskBar1: TTaskBar;
    btnBack: TSpeedButton;
    btnNext: TSpeedButton;
    btnCancel: TSpeedButton;
    W7Image2: TW7Image;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    dlgOpen1: TOpenDialog;
    uGeneralProgress: TUbuntuProgress;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure CreateStartPage;
    procedure CreateOpenFirmware;
    procedure CreateGitHubPage;
    procedure CreateCompileFirmware;
    procedure CreateScriptsPage;
    procedure CreateFinishPage;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure W7Image2Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function ExecuteCommand(OperationTitle:string; var CommandLine: string; var lWorkingDir: string; WaitShow:Boolean=False):Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    WorkDir, FirmwareFile, DataDir, aAppsDir, JavaHome, JavaCommand, OTAUpdateServer, CoreTool:string;
    MultiCompile:Boolean;
    PasswordList:TStringList;
    GitHubs, ULTInfo:TStringList;
    //AdditionalGit:TStringList;
    procedure AddToLog(State, LogValue:string);
    procedure SetProgress(Progress:Integer; Marquee:boolean=False);
    procedure ShowHelp(HelpIndex:string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses StartFrame, GitHubChooiser, OpenFirmware, CompileFirmware, ScriptFrame, HelpUnit, FinishFrame,
  CryModule;

const
   ProgramName='MFB (Lite) - ';

procedure TMainForm.ShowHelp(HelpIndex: string);
begin
   if FileExists(DataDir+'Help\'+HelpIndex+'.rtf') then begin
   HelpForm.HelpContent.Clear;
   HelpForm.HelpContent.Lines.LoadFromFile(DataDir+'Help\'+HelpIndex+'.rtf');
   HelpForm.Show;
   end else ShowMessage('Для данного раздела ещё не написана справка');
end;

{4 lines from there removed due to proprietary code from AndroTech}

procedure TMainForm.SetProgress(Progress: Integer; Marquee:boolean=False);
begin
  TaskBar1.ProgressValue:=Progress;
  if Marquee then TaskBar1.ProgressState:=TBPF_INDETERMINATE
  else TaskBar1.ProgressState:=TBPF_NORMAL;
  Application.ProcessMessages;
end;

procedure TMainForm.AddToLog(State, LogValue:string);
var
  Log:TStringList;
begin
  Log:=TStringList.Create;
  Log.Clear;
  if FileExists(WorkDir+'Log.log') then Log.LoadFromFile(WorkDir+'Log.log');
  Log.Add(State+'\ | '+FormatDateTime('dd.MM.yyyy hh.mm.ss', Now)+' | '+LogValue);
  Log.SaveToFile(WorkDir+'Log.log');
end;

procedure TMainForm.btnBackClick(Sender: TObject);
begin
  if (pgcMainControl.ActivePageIndex-1)>=0 then begin
    pgcMainControl.ActivePageIndex:=pgcMainControl.ActivePageIndex-1;
    lblInfoText.Caption:=pgcMainControl.Pages[pgcMainControl.ActivePageIndex].Caption;
  end;
  if (pgcMainControl.ActivePageIndex)>=(pgcMainControl.PageCount-1) then
  btnNext.Caption:='Финиш' else btnNext.Caption:='Далее';
  if pgcMainControl.ActivePageIndex<=0 then btnBack.Enabled:=False
  else btnBack.Enabled:=True;
  btnNext.Enabled:=True;
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

function TMainForm.ExecuteCommand(OperationTitle:string; var CommandLine: string; var lWorkingDir: string; WaitShow:Boolean=False):Boolean;
var
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
  ExitCode:Cardinal;
  TempCmd, TmpWorkDir:PWideChar;
  PointProgress:string;
  t:Integer;
begin
  MainForm.uGeneralProgress.Visible:=True;
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  StartupInfo.wShowWindow := SW_HIDE;
  TempCmd:=PWideChar(CommandLine);
  TmpWorkDir:=PWideChar(lWorkingDir);
  if not CreateProcess(nil,
                  TempCmd,
                  nil, nil, false, NORMAL_PRIORITY_CLASS, nil,
                  TmpWorkDir, //current dir
                  StartUpInfo, ProcessInfo) then MainForm.AddToLog('E','Programm malformed!')
                else
                while  WaitForSingleObject(ProcessInfo.hProcess, 100) = WAIT_TIMEOUT do begin
                  if WaitShow then begin
                   MainForm.uGeneralProgress.Mode:=pmMarquee;
                   MainForm.TaskBar1.ProgressState:=TBPF_INDETERMINATE;
                  end;
                  Application.ProcessMessages;
                end;
  GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
  if ExitCode<>0 then begin
   MainForm.AddToLog('E','Executing Failed!');
   MainForm.AddToLog('E','ExitCode: '+IntToStr(ExitCode));
   if WaitShow then begin
     MainForm.uGeneralProgress.Mode:=pmNormal;
     MainForm.TaskBar1.ProgressState:=TBPF_NORMAL;
   end;
   Result:=False
  end else begin
   MainForm.AddToLog('I',OperationTitle+'...Done!');
   if WaitShow then begin
     MainForm.uGeneralProgress.Mode:=pmNormal;
     MainForm.TaskBar1.ProgressState:=TBPF_NORMAL;
   end;
   Result:=True;
  end;
  MainForm.uGeneralProgress.Visible:=False;
end;

procedure TMainForm.CreateStartPage;
var
  TS : TTabsheet;
  frmStartPage:TfrmStartPage;
begin
  TS := TTabSheet.Create(pgcMainControl);
  TS.PageControl := pgcMainControl;
  TS.TabVisible := False;
  frmStartPage:=TfrmStartPage.Create(TS);
  frmStartPage.Parent:= TS;
  TS.Caption:='Начало';
  frmStartPage.Align:= alClient;
end;

procedure TMainForm.CreateGitHubPage;
var
  TS : TTabsheet;
  frmGitPage:TGitHubRepositories;
  GitFiles:TStringList;
begin
  TS := TTabSheet.Create(pgcMainControl);
  TS.PageControl := pgcMainControl;
  TS.TabVisible := False;
  frmGitPage:=TGitHubRepositories.Create(TS);
  frmGitPage.Parent:= TS;
  TS.Caption:='Настройка GitHub';
  frmGitPage.chklstGitHubsGroup.Items.LoadFromFile(WorkDir+'Settings\GitHub-names.conf');
  if not Assigned(GitHubs) then
  GitHubs:=TStringList.Create;
  GitHubs.Clear;
  GitHubs.LoadFromFile(WorkDir+'Settings\GitHub-gits.conf');
  ULTInfo:=TStringList.Create;
  ULTInfo.Clear;
  ULTInfo.LoadFromFile(WorkDir+'Settings\GitHub-ult.conf');
  frmGitPage.Align:= alClient;
end;

procedure TMainForm.CreateOpenFirmware;
var
  TS : TTabsheet;
  frmOpenFirmware:TfrmOpenFirmware;
begin
  TS := TTabSheet.Create(pgcMainControl);
  TS.PageControl := pgcMainControl;
  TS.TabVisible := False;
  frmOpenFirmware:=TfrmOpenFirmware.Create(TS);
  frmOpenFirmware.Parent:= TS;
  TS.Caption:='Открытие прошивки';
  frmOpenFirmware.Align:= alClient;
end;

procedure TMainForm.CreateFinishPage;
var
  TS : TTabsheet;
  frmFinishFrame:TfrmFinish;
begin
  TS := TTabSheet.Create(pgcMainControl);
  TS.PageControl := pgcMainControl;
  TS.TabVisible := False;
  frmFinishFrame:=TfrmFinish.Create(TS);
  frmFinishFrame.Parent:= TS;
  TS.Caption:='Готово';
  frmFinishFrame.Align:= alClient;
end;

procedure TMainForm.CreateScriptsPage;
var
  TS : TTabsheet;
  frmScripts:TfrmScripts;
begin
  TS := TTabSheet.Create(pgcMainControl);
  TS.PageControl := pgcMainControl;
  TS.TabVisible := False;
  frmScripts:=TfrmScripts.Create(TS);
  frmScripts.Parent:= TS;
  TS.Caption:='Репозиторий скриптов';
  frmScripts.Align:= alClient;
  frmScripts.init;
end;

procedure TMainForm.CreateCompileFirmware;
var
  TS : TTabsheet;
  frmCompileFirmware:TfrmCompileFirmware;
begin
  TS := TTabSheet.Create(pgcMainControl);
  TS.PageControl := pgcMainControl;
  TS.TabVisible := False;
  frmCompileFirmware:=TfrmCompileFirmware.Create(TS);
  frmCompileFirmware.Parent:= TS;
  TS.Caption:='Обработка прошивки';
  frmCompileFirmware.Align:= alClient;
  frmCompileFirmware.init;
end;



procedure TMainForm.btnNextClick(Sender: TObject);
var
  frmCompileFirmware:TfrmCompileFirmware;
  frmScripts:TfrmScripts;
  IniFiles:TIniFile;
  FirmwareVersion, ID1, ID2:string;
begin
  btnBack.Repaint;
  btnBack.Refresh;
  if (pgcMainControl.ActivePageIndex+1)<pgcMainControl.PageCount then begin
    pgcMainControl.ActivePageIndex:=pgcMainControl.ActivePageIndex+1;
  end else Close;
  btnNext.Enabled:=False;
  if (pgcMainControl.ActivePageIndex)=(pgcMainControl.PageCount-1) then begin
   btnNext.Caption:='Финиш';
   btnNext.Enabled:=True;
  end;
  if pgcMainControl.Pages[pgcMainControl.ActivePageIndex].Controls[0].Name='frmScripts' then begin
   btnNext.Enabled:=True;

  end;
  Caption:=ProgramName+pgcMainControl.Pages[pgcMainControl.ActivePageIndex].Caption;
  if pgcMainControl.ActivePageIndex<=0 then btnBack.Enabled:=False
  else btnBack.Enabled:=True;
  if pgcMainControl.Pages[pgcMainControl.ActivePageIndex].Controls[0].Name='frmCompileFirmware' then begin
      frmCompileFirmware:=pgcMainControl.Pages[pgcMainControl.ActivePageIndex].Controls[0] as TfrmCompileFirmware;
      frmCompileFirmware.Memo1.Lines.Clear;
      IniFiles:=TIniFile.Create(MainForm.DataDir+'Language.conf');
      FirmwareVersion:=IniFiles.ReadString('Language','Version','');
      ID1:=IniFiles.ReadString('Language','ID1','ru');
      ID2:=IniFiles.ReadString('Language','ID2','RU');
      IniFiles.Free;
      if FileExists(MainForm.DataDir+'\LanguageFiles_Main\Language.conf') then begin
         IniFiles:=TIniFile.Create(MainForm.DataDir+'\LanguageFiles_Main\Language.conf');
         FirmwareVersion:=IniFiles.ReadString('Language','Version','')+'-git';
         IniFiles.Free;
      end;
      frmCompileFirmware.Memo1.Lines.Add('Прошивка:');
      frmCompileFirmware.Memo1.Lines.Add(#9'Используется перевод для версии: '+ FirmwareVersion+' ('+ID1+'_'+ID2+')');
      if MultiCompile then frmCompileFirmware.Memo1.Lines.Add(#9'Папка компиляции: '+ FirmwareFile)
      else frmCompileFirmware.Memo1.Lines.Add(#9'Имя файла прошивки: '+ FirmwareFile);
      frmCompileFirmware.Memo1.Lines.Add(#9);
      frmCompileFirmware.Memo1.Lines.Add('Все готово для компиляции!');
      btnNext.Enabled:=False;
      btnBack.Enabled:=False;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GitHubs.Free;
  PasswordList.Free;
end;

function IsWin64: Boolean;
type
  TIsWow64Process = function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
var
  IsWow64Result: BOOL;              // Result from IsWow64Process
  IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference
begin
  Result := False;

  IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process'); // Do Not Localize
  if (Assigned(IsWow64Process) and IsWow64Process(GetCurrentProcess, IsWow64Result)) then
    Result := IsWow64Result;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  JavaVersion:string;
  Registry:TRegistry;
  i:Integer;
  IniFile:TIniFile;
begin
  {1 lines from there removed due to proprietary code from AndroTech}
  IniFile:=TIniFile.Create(WorkDir+'Settings\Settings.conf');
  //GitHubRepository:='git://github.com/miuirussia/miui-russian-translation.git';
  OTAUpdateServer:=IniFile.ReadString('Server', 'DownloadServer', 'http://ota.miuirussia.com/OTAS/')+IniFile.ReadString('Server', 'WorkName', 'Noned')+'/update.php';
  JavaHome:='Null';

  with TRegistry.Create(KEY_ALL_ACCESS) do
  begin
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\JavaSoft\Java Runtime Environment',False) then
      begin
       JavaVersion:=ReadString('CurrentVersion');
      end;
      CloseKey;
      if OpenKey('SOFTWARE\JavaSoft\Java Runtime Environment\'+JavaVersion,False) then
      begin
          JavaHome:=ReadString('JavaHome');
      end;
      CloseKey;
    finally
      free;
    end;
  end;

  with TRegistry.Create(KEY_ALL_ACCESS OR KEY_WOW64_64KEY) do
  begin
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\JavaSoft\Java Runtime Environment',False) then
      begin
       JavaVersion:=ReadString('CurrentVersion');
      end;
      CloseKey;
      if OpenKey('SOFTWARE\JavaSoft\Java Runtime Environment\'+JavaVersion,False) then
      begin
          JavaHome:=ReadString('JavaHome');
      end;
      CloseKey;
    finally
      free;
    end;
  end;
  WorkDir:=ExtractFilePath(Application.ExeName);
  DataDir:=IncludeTrailingBackslash(IncludeTrailingBackslash(WorkDir)+'Data');
  aAppsDir:=IncludeTrailingBackslash(IncludeTrailingBackslash(DataDir)+'aApps');
  //for i := 0 to 9 do AddonsChecked[i]:=False;
  AddToLog('I', 'WorkDir: '+WorkDir);
  AddToLog('I', 'DataDir: '+DataDir);
  AddToLog('I', 'aAppsDir: '+aAppsDir);
  AddToLog('I', 'JavaHome: '+JavaHome);
  if JavaHome='Null' then begin
    if dlgOpen1.Execute then if dlgOpen1.FileName<>'' then JavaHome:=ExtractFilePath(ExtractFilePath(dlgOpen1.FileName));
  end;

  if JavaHome='Null' then begin
    Application.Terminate;
    Exit;
  end;
  CreateStartPage;
  CreateGitHubPage;
  CreateScriptsPage;
  CreateOpenFirmware;
  CreateCompileFirmware;
  CreateFinishPage;
  pgcMainControl.ActivePageIndex:=0;
  Caption:=ProgramName+pgcMainControl.Pages[pgcMainControl.ActivePageIndex].Caption;
  btnBack.Enabled:=False;
  MultiCompile:=False;
  {AddonItegrateScriptList:=TStringList.Create;
  AddonIntegateList:=TStringList.Create;}
  PasswordList:=TStringList.Create;
  if IsWin64 then
  JavaCommand:=MainForm.JavaHome+'/bin/java -Xmx2048M'
  else JavaCommand:=MainForm.JavaHome+'/bin/java';
  CoreTool:=JavaCommand+' -jar "'+MainForm.aAppsDir+'\apktool.jar" ';
  {12 lines from there removed due to proprietary code from AndroTech
  Add you own PasswordList.Add();}
end;

procedure TMainForm.Image1Click(Sender: TObject);
begin
  ShowHelp(pgcMainControl.Pages[pgcMainControl.ActivePageIndex].Controls[0].Name);
end;

procedure TMainForm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const sc_dragmove = $f012;
begin
   releasecapture;
   MainForm.perform(wm_syscommand, sc_dragmove, 0);
end;

end.
