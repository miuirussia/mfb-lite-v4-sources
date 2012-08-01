unit CompileFirmware;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, TaskBar,
  UbuntuProgress, FWZipConsts, FWZipReader, FWZipWriter, System.ZLib, System.IniFiles, System.Masks, Winapi.ShellAPI, System.RegularExpressions,
  Winapi.WinInet, CRCunit, AlPhpRunner, ALStringList, NativeXml;

type
  TfrmCompileFirmware = class(TFrame)
    Label1: TLabel;
    Memo1: TMemo;
    btnStartCompile: TButton;
    chkNoApp: TCheckBox;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    cbbTimeZoneSelect: TComboBox;
    Label3: TLabel;
    cbbLangSelect: TComboBox;
    procedure btnStartCompileClick(Sender: TObject);
    function ExecuteCommand(OperationTitle:string; var CommandLine, lWorkingDir:string; WaitShow:Boolean=False):boolean;
    function CompileFile(FilePName:string):Boolean;
    procedure ExtractTranslation(From, Dect:string);
    function CheckTranslation(Trn:string):Boolean;
    function StartScript(ScriptsDir, FileFName ,WorkDir, DataDir, aAppsDir, PrepFileName, PrepFilePath, DecompiledPath, JavaHome, CoreTool:string; var BuildFile:Boolean):Boolean;
//    procedure ExtractAddon(Addon, Dest:string;Integrate:Boolean);
    procedure ExtractPrecompiled(From, Dest:string);
    procedure PatchAndroidPolicy(FileName:string);
    procedure PatchAndroidLang(FileName:string);
    procedure PatchTranslationUpdate(Path, FileNm:string);
    procedure BuildFirmware(FileName:string);
    procedure init;
    procedure cbbTimeZoneSelectChange(Sender: TObject);
  private
    { Private declarations }
    procedure OnProgress(Sender: TObject; const FileName: string;
      Percent, TotalPercent: Byte; var Cancel: Boolean);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses MainUnit,
     ALMultiPartFormDataParser,
     AlFcnFile,
     AlFcnMisc,
     ALFcnString,
     AlFcnMime,
     AlHttpCommon;

var
  UnixStartDate: TDateTime = 25569.0;
  TimeZone:string;

const
  arrLang:array[0..2] of string = ('ru','uk','en');
  arrRegions:array[0..2] of string = ('RU','UK','US');

procedure TfrmCompileFirmware.init;
var
  timezones:TNativeXml;
  timeNodes:TsdNodeList;
  SetFile:TIniFile;
  i:Integer;
begin
  timezones:=TNativeXml.Create(Self);
  timezones.LoadFromFile(MainForm.DataDir+'timezones.xml');
  timeNodes:=TsdNodeList.Create;
  timezones.Root.FindNodes('timezone', timeNodes);
  for i := 0 to timeNodes.Count-1 do begin
      cbbTimeZoneSelect.Items.Add(timeNodes[i].Value);
  end;

  timeNodes.Free;
  timezones.Free;

  SetFile:=TIniFile.Create(MainForm.WorkDir+'Settings\TimeZone.conf');
  cbbTimeZoneSelect.ItemIndex:=SetFile.ReadInteger('TimeZone','InstalledId',-1);
  cbbTimeZoneSelect.OnChange(Self);
  SetFile.Free;

end;

function TfrmCompileFirmware.ExecuteCommand(OperationTitle:string; var CommandLine: string; var lWorkingDir: string; WaitShow:Boolean=False):Boolean;
begin
  Result:=MainForm.ExecuteCommand(OperationTitle,CommandLine, lWorkingDir, WaitShow);
end;

procedure FindFiles(StartFolder, Mask: String; List: TStrings;
ScanSubFolders: Boolean = True);
var
  SearchRec: TSearchRec;
  FindResult: Integer;
begin
  List.BeginUpdate;
  try
    StartFolder:=IncludeTrailingBackslash(StartFolder);
    FindResult:=FindFirst(StartFolder+'*.*', faAnyFile, SearchRec);
    try
      while FindResult = 0 do with SearchRec do begin
        if (Attr and faDirectory)>0 then begin
          if ScanSubFolders and (Name<>'.') and (Name<>'..') then
          FindFiles(StartFolder+Name, Mask, List, ScanSubFolders);
        end else begin
          if MatchesMask(Name, Mask) then List.Add(StartFolder+Name);
        end;
        FindResult:=FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  finally
  List.EndUpdate;
  end;
end;

procedure FindFolders(StartFolder, Mask: String; List: TStrings;
ScanSubFolders: Boolean = False);
var
  SearchRec: TSearchRec;
  FindResult: Integer;
begin
  List.BeginUpdate;
  try
    StartFolder:=IncludeTrailingBackslash(StartFolder);
    FindResult:=FindFirst(StartFolder+'*', faDirectory, SearchRec);
    try
      while FindResult = 0 do with SearchRec do begin
        if ((SearchRec.Attr and faDirectory) = faDirectory) and
        (Name<>'.') and (Name<>'..') then
        begin
          if MatchesMask(Name, Mask) then List.Add(StartFolder+Name);
        end;
        FindResult:=FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  finally
    List.EndUpdate;
  end;
end;

function GetFileNameWOExt(fn:String):String;
begin
  Result := Copy(fn, 1, Length(fn)-Length(ExtractFileExt(fn)));
end;

function RandomPassword(PLen: Integer): string;
var
  str: string;
begin
  Randomize;
  str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = PLen)
end;

procedure PatchFilesAddons(FileName:string);
var
   CmdLine:string;
begin
   {4 lines from there removed due to proprietary code from AndroTech}
end;

procedure SetBuildPropVariable(Filename, Variable, Value:string);
var
   BuildProp:TStringList;
   i:Integer;
   Pattern:string;
   RegExpr:TRegEx;
   Replaced:Boolean;
begin
   BuildProp:=TStringList.Create;
   BuildProp.LoadFromFile(Filename);
   Pattern:=Variable+'=(.*)';
   RegExpr:=TRegEx.Create(Pattern);
   Replaced:=False;
   for i := 0 to BuildProp.Count-1 do begin
     if RegExpr.IsMatch(BuildProp[i]) then begin
       BuildProp[i]:=Variable+'='+Value;
       Replaced:=True;
     end;
   end;
   if not Replaced then BuildProp.Add(Variable+'='+Value);
   BuildProp.SaveToFile(Filename);
end;

function GetBuildPropVariable(Filename, Variable:string):string;
var
   BuildProp:TStringList;
   i:Integer;
   Pattern:string;
   RegExpr:TRegEx;
   M:TMatch;
begin
   BuildProp:=TStringList.Create;
   BuildProp.LoadFromFile(Filename);
   Pattern:=Variable+'=(.*)';
   RegExpr:=TRegEx.Create(Pattern);
   for i := 0 to BuildProp.Count-1 do begin
     if RegExpr.IsMatch(BuildProp[i]) then begin
       M:=RegExpr.Match(BuildProp[i]);
       Result:=M.Groups[1].Value;
     end;
   end;
end;

procedure PCHEExecutor(WorkDir, FileName:string);
const
  endofmethod='.end method';
var
  i, j, rm, orgid, id:Integer;
  Config:TIniFile;
  MTDFile, SmaliFile:TStringList;
  FileN, MethodName, MethodFile:string;
begin
{39 lines from there removed due to proprietary code from AndroTech}
end;

function FullDirectoryCopy(SourceDir, TargetDir: string; StopIfNotAllCopied:Boolean=False;
  OverWriteFiles: Boolean=True): Boolean;
var
  SR: TSearchRec;
  I: Integer;
begin
  Result := False;
  SourceDir := IncludeTrailingBackslash(SourceDir);
  TargetDir := IncludeTrailingBackslash(TargetDir);
  if not DirectoryExists(SourceDir) then Exit;
  if not ForceDirectories(TargetDir) then Exit;
  I := FindFirst(SourceDir + '*', faAnyFile, SR);
  try
    while I = 0 do
    begin
      if (SR.Name <> '') and (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        if SR.Attr = faDirectory then
          Result := FullDirectoryCopy(SourceDir + SR.Name, TargetDir + SR.NAME,
            StopIfNotAllCopied, OverWriteFiles)
        else if not (not OverWriteFiles and FileExists(TargetDir + SR.Name))
          then
          Result := CopyFile(Pchar(SourceDir + SR.Name), Pchar(TargetDir +
            SR.Name), False)
        else
          Result := True;
        if not Result and StopIfNotAllCopied then
          exit;
      end;
      I := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

procedure CopyFiles(aDirFrom, aDirTo, aMask: string);
var
  OpStruc: TSHFileOpStruct;
  FromBuf, ToBuf: array [0..128] of Char;
begin
  FillChar(FromBuf, SizeOf(FromBuf), 0);
  FillChar(ToBuf, SizeOf(ToBuf), 0);
  StrPCopy(FromBuf, aDirFrom+aMask);
  StrPCopy(ToBuf, aDirTo);
  try
    with OpStruc do
    begin
      wFunc:= FO_COPY;
      pFrom:= @FromBuf;
      pTo:= @ToBuf;
      fFlags:= FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SIMPLEPROGRESS;
      fAnyOperationsAborted:= False;
      hNameMappings:= nil;
      lpszProgressTitle:= 'Copyng files...';
    end;
    SHFileOperation(OpStruc);
    MainForm.AddToLog('I','Building translation for this file...Done!');
  except
    on e:Exception do
      MainForm.AddToLog('E','Error at copying files into '+aDirTo+#13+#13+e.Message);
  end;
end;

function ReplaceStringsInSmali(FileName, FolderToPath, OTAServer:string):Boolean ;
var
  IniFiles:TIniFile;
  SmaliFile:TStringList;
  SearchString, ReplaceString, FilePath:string;
  i:Integer;
begin
//  Result:=False;
  IniFiles:=TIniFile.Create(FileName);
  for i := 1 to IniFiles.ReadInteger('Smali', 'Count', 0) do begin
    SmaliFile:=TStringList.Create;
    SearchString:=IniFiles.ReadString(IntToStr(i),'SearchString','Null');
    ReplaceString:=IniFiles.ReadString(IntToStr(i),'ReplaceString','Null');
    FilePath:=FolderToPath+'\'+IniFiles.ReadString(IntToStr(i),'Path','Null');
    if FileExists(FilePath) then begin
      SmaliFile.LoadFromFile(FilePath);
      ReplaceString:=StringReplace(ReplaceString, '%OTAServer%', OTAServer, [rfReplaceAll]);
      SmaliFile.Text:=StringReplace(SmaliFile.Text, SearchString, ReplaceString, [rfReplaceAll]);
      SmaliFile.SaveToFile(FilePath);
      SmaliFile.Free;
    end;
  end;
  Result:=True;
end;

Function DelTree(DirName : string): Boolean;
var
  SHFileOpStruct : TSHFileOpStruct;
  DirBuf : array [0..255] of char;
begin
  try
    Fillchar(SHFileOpStruct,Sizeof(SHFileOpStruct),0) ;
    FillChar(DirBuf, Sizeof(DirBuf), 0 ) ;
    StrPCopy(DirBuf, DirName) ;
    with SHFileOpStruct do begin
      Wnd := 0;
      pFrom := @DirBuf;
      wFunc := FO_DELETE;
      fFlags := FOF_NOCONFIRMATION;
      fFlags := fFlags or FOF_SILENT;
    end;
    Result := (SHFileOperation(SHFileOpStruct) = 0) ;
   except
    Result := False;
  end;
end;

procedure TfrmCompileFirmware.ExtractPrecompiled(From: string; Dest: string);
var
  DeCryptFile:TFWZipReader;
  i:Integer;
begin
  DeCryptFile:=TFWZipReader.Create;
  DeCryptFile.LoadFromFile(MainForm.DataDir+'PrecompiledFiles\'+From+'.cryz');
  DeCryptFile.PasswordList.Clear;
  for i := 0 to MainForm.PasswordList.Count-1 do DeCryptFile.PasswordList.Add(MainForm.PasswordList[i]);
  ForceDirectories(MainForm.WorkDir+'TransltTmp');
  DeCryptFile.ExtractAll(MainForm.WorkDir+'TransltTmp');
  CopyFiles(MainForm.WorkDir+'TransltTmp\',Dest,'*');
  DelTree(MainForm.WorkDir+'TransltTmp');
  DeCryptFile.Free;
end;

procedure TfrmCompileFirmware.ExtractTranslation(From: string; Dect: string);
//var
  //DeCryptFile:TFWZipReader;
  //i:Integer;
begin
   CopyFiles(MainForm.DataDir+'LanguageFiles_Main\'+From+'\',Dect,'*');
end;

function TfrmCompileFirmware.CheckTranslation(Trn: string):Boolean;
var
  TranslationPath:string;
begin
  TranslationPath:=MainForm.DataDir+'LanguageFiles_Main\'+Trn;
  if DirectoryExists(TranslationPath) then Result:=True else Result:=False;
end;

procedure TfrmCompileFirmware.PatchTranslationUpdate(Path, FileNm:string);
var MainDir:string;
    PatchFile:TStringList;
    FilesToPath:TStringList;
    i:Integer;
begin
{125 lines from there removed due to proprietary code from AndroTech}
end;

{30 lines from there removed due to proprietary code from AndroTech}

function TfrmCompileFirmware.StartScript(ScriptsDir, FileFName, WorkDir, DataDir, aAppsDir, PrepFileName, PrepFilePath, DecompiledPath, JavaHome, CoreTool:string; var BuildFile:Boolean):Boolean;
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TALStringStream;
    aPhpRunnerEngine: TalPhpRunnerEngine;
    aServerVariablesLst: TalStringList;
    APostDataStrings: TalStringList;
    ADataResponse: TalStringList;
    ScriptList:TStringList;
    FileName:string;
    i,k:integer;
begin
{76 lines from there removed due to proprietary code from AndroTech}
end;

function TfrmCompileFirmware.CompileFile(FilePName: string):Boolean;
var
  PrepFileName, PrepFilePath, CmdLine, TranslationDest:string;
  DeleteFiles, CleanUpFiles:TStringList;
  ApkFile:TFWZipWriter;
  OrigApkFile, DestApkFile:TFWZipReader;
  BruteForce:boolean;
  BuildFile:Boolean;
  Regol:TRegEx;
  i, k:integer;
begin
  Result:=True;
  BuildFile:=True;
  PrepFileName:=ExtractFileName(FilePName);
  PrepFilePath:=ExtractFilePath(FilePName);
  if CheckTranslation(PrepFileName) then begin
    MainForm.AddToLog('-', '---------------- '+PrepFileName+' --------------------');
    MainForm.AddToLog('I', 'FileName initialized = '+PrepFileName);
    MainForm.AddToLog('I', 'FilePath initialized = '+PrepFilePath);
    DelTree(MainForm.WorkDir+'ApplicationSource\'+PrepFileName);
    ForceDirectories(MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'_Translation\');
    TranslationDest:=MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'_Translation\';
    ExtractTranslation(PrepFileName, TranslationDest);

    if DirectoryExists(MainForm.DataDir+'\Scripts') then begin
     MainForm.AddToLog('I', 'Jumping to script PHP engine...');
     StartScript(MainForm.DataDir+'\Scripts\','Precomp.script', MainForm.WorkDir, MainForm.DataDir, MainForm.aAppsDir, PrepFileName, PrepFilePath, 'none', MainForm.JavaCommand, MainForm.CoreTool, BuildFile);
     MainForm.AddToLog('I', 'Resuming core work...');
    end;
    if BuildFile then begin

      if DirectoryExists(TranslationDest+'Precomp') then begin
         MainForm.AddToLog('I', 'Avoiding APKTOOL errors. Starting...');
         ForceDirectories(MainForm.WorkDir+'PrecompileWork');
         DestApkFile:=TFWZipReader.Create;
         DestApkFile.LoadFromFile(PrepFilePath+PrepFileName+'.apk');
         DestApkFile.ExtractAll(MainForm.WorkDir+'PrecompileWork');
         DestApkFile.Free;
         FullDirectoryCopy(TranslationDest+'Precomp', MainForm.WorkDir+'PrecompileWork');
         ApkFile:=TFWZipWriter.Create;
         ApkFile.AddFolder('',MainForm.WorkDir+'PrecompileWork\','*');
         DeleteFile(FilePName+'.apk');
         ApkFile.BuildZip(FilePName+'.apk');
         ApkFile.Free;
         DelTree(MainForm.WorkDir+'PrecompileWork\');
         MainForm.AddToLog('I', 'Avoiding APKTOOL errors. Done!');
      end;

      if FileExists(TranslationDest+'\Smali.rep') then begin
         MainForm.AddToLog('I', 'Offset fix. Starting...');
         ForceDirectories(MainForm.WorkDir+'PrecompileWork');
         DestApkFile:=TFWZipReader.Create;
         DestApkFile.LoadFromFile(PrepFilePath+PrepFileName+'.apk');
         DestApkFile.ExtractAll(MainForm.WorkDir+'PrecompileWork');
         DestApkFile.Free;
         DowngradeVersion(MainForm.WorkDir+'PrecompileWork\classes.dex');
         ApkFile:=TFWZipWriter.Create;
         ApkFile.AddFolder('',MainForm.WorkDir+'PrecompileWork\','*');
         DeleteFile(FilePName+'.apk');
         ApkFile.BuildZip(FilePName+'.apk');
         ApkFile.Free;
         DelTree(MainForm.WorkDir+'PrecompileWork\');
         MainForm.AddToLog('I', 'Offset fix. Done!');
      end;

      if FileExists(TranslationDest+'\Smali.rep') or FileExists(TranslationDest+'\Info.pche') then
        CmdLine:=MainForm.CoreTool+' d "'+PrepFilePath+PrepFileName+'.apk" "'+MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'"'
      else CmdLine:=MainForm.CoreTool+' d -s "'+PrepFilePath+PrepFileName+'.apk" "'+MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'"';
      Result := ExecuteCommand('Getting sources from APK', CmdLine, MainForm.DataDir);
      PatchTranslationUpdate(IncludeTrailingBackslash(MainForm.WorkDir+'ApplicationSource\'+PrepFileName), PrepFileName);

      if PrepFileName='framework-res' then begin

      CleanUpFiles:=TStringList.Create;
      FindFolders(MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'\res', '*', CleanUpFiles);
      Regol:=TRegEx.Create('.*values-[a-df-z][a-df-z]\||.*values-[a-df-z][a-df-z]-[A-DF-Z][A-DF-Z]\|');
      for k := 0 to CleanUpFiles.Count-1 do begin
       if Regol.IsMatch(CleanUpFiles[k]+'|') then begin

          MainForm.AddToLog('I','Deleting '+CleanUpFiles[k]);
          DelTree(CleanUpFiles[k]);

       end;
      end;
      CleanUpFiles.Free;

      end;

      if DirectoryExists(MainForm.DataDir+'\Scripts') then begin
        MainForm.AddToLog('I', 'Jumping to script PHP engine...');
        StartScript(MainForm.DataDir+'\Scripts\','Compile.script', MainForm.WorkDir, MainForm.DataDir, MainForm.aAppsDir, PrepFileName, PrepFilePath, MainForm.WorkDir+'ApplicationSource\'+PrepFileName, MainForm.JavaCommand, MainForm.CoreTool, BuildFile);
        MainForm.AddToLog('I', 'Resuming core work...');
      end;

      if BuildFile then begin

        FullDirectoryCopy(TranslationDest+'Files', MainForm.WorkDir+'ApplicationSource\'+PrepFileName);
        DeleteFiles:=TStringList.Create;
        DeleteFiles.Clear;
        FindFiles(TranslationDest+'Files','*',DeleteFiles);
        for i := 0 to DeleteFiles.Count-1 do begin
          DeleteFiles[i]:=ExtractRelativePath(TranslationDest+'Files\', DeleteFiles[i]);
        end;
        if FileExists(TranslationDest+'\Smali.rep') then ReplaceStringsInSmali(TranslationDest+'Smali.rep', MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'\smali', MainForm.OTAUpdateServer);
        if FileExists(TranslationDest+'\Info.pche') then PCHEExecutor(MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'\smali\', TranslationDest+'Info.pche');
        ForceDirectories(MainForm.WorkDir+'CompiledFiles\');
        PatchFilesAddons(PrepFileName);
        CmdLine:=MainForm.CoreTool+' b "'+MainForm.WorkDir+'ApplicationSource\'+PrepFileName+'" "'+MainForm.WorkDir+'CompiledFiles\'+PrepFileName+'.apk"';
        if ExecuteCommand('Compiling sources to APK', CmdLine, MainForm.DataDir) then BruteForce:=False else BruteForce:=True;
        DestApkFile:=TFWZipReader.Create;
        DestApkFile.LoadFromFile(PrepFilePath+PrepFileName+'.apk');
        Result := not BruteForce;
        if not BruteForce then begin
          OrigApkFile:=TFWZipReader.Create;
          OrigApkFile.LoadFromFile(MainForm.WorkDir+'CompiledFiles\'+PrepFileName+'.apk');
          ForceDirectories(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest');
          ForceDirectories(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Orig');
          DestApkFile.ExtractAll(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest');
          OrigApkFile.ExtractAll(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Orig');
          OrigApkFile.Free;
          DeleteFile(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Orig\AndroidManifest.xml');
          DeleteFile(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest\classes.dex');
          DeleteFile(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest\resources.arsc');
          for i := 0 to DeleteFiles.Count-1 do begin
            CopyFile(PWideChar(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Orig\'+DeleteFiles[i]),PWideChar(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest\'+DeleteFiles[i]), False);
          end;
          FullDirectoryCopy(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Orig',MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest', False, False);
          CopyFiles(TranslationDest+'\Files\assets\', MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest\assets\','*');
        end else begin
          MainForm.AddToLog('I', 'Using brute-force method...Done!');
          DestApkFile.Free;
          ForceDirectories(MainForm.WorkDir+'ResolveConf');
          CmdLine:=MainForm.CoreTool+' d -s "'+PrepFilePath+PrepFileName+'.apk" "'+MainForm.WorkDir+'ResolveConf\'+PrepFileName+'"';
          ExecuteCommand('Getting sources from APK', CmdLine, MainForm.DataDir);
          PatchTranslationUpdate(IncludeTrailingBackslash(MainForm.WorkDir+'ResolveConf\'+PrepFileName), PrepFileName);
          CopyFiles(TranslationDest+'\Files\', MainForm.WorkDir+'ResolveConf\'+PrepFileName+'\','*');
          DestApkFile:=TFWZipReader.Create;
          DestApkFile.LoadFromFile(PrepFilePath+PrepFileName+'.apk');
          DestApkFile.ExtractAll(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest');
        end;
        DestApkFile.Free;
        if FileExists(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest\classes.dex') then
        UpperVersion(MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest\classes.dex');
        DeleteFile(FilePName+'.apk');
        CmdLine:=MainForm.aAppsDir+'7za a -tzip "'+FilePName+'_notalign.apk'+'" "'+MainForm.WorkDir+'MergeFiles\'+PrepFileName+'_Dest\*'+'" -mx0 -r';
        ExecuteCommand('Building APK', CmdLine, MainForm.aAppsDir, True);
        CmdLine:=MainForm.aAppsDir+'zipalign -v 4 "'+FilePName+'_notalign.apk'+'" "'+FilePName+'.apk'+'"';
        ExecuteCommand('Reworking code', CmdLine, MainForm.aAppsDir, True);
        DeleteFile(FilePName+'_notalign.apk');
        MainForm.AddToLog('I', 'Deleting application sources...Done!');
        DelTree(MainForm.WorkDir+'ApplicationSource\');
        DelTree(MainForm.WorkDir+'CompiledFiles\');
        DelTree(MainForm.WorkDir+'MergeFiles\');
      end;
    end;
  end;
  MainForm.AddToLog('-', '-------------------------------------------------------------------------------');
end;

Function GetUserFromWindows: string;
Var
   UserName : string;
   UserNameLen : Dword;
Begin
   UserNameLen := 255;
   SetLength(userName, UserNameLen) ;
   If GetUserName(PChar(UserName), UserNameLen) Then
     Result := Copy(UserName,1,UserNameLen - 1)
   Else
     Result := 'Unknown';
End;

function GetTimeZone:String;
var TIME_ZONE:_TIME_ZONE_INFORMATION;
    i, j:integer;
begin
GetTimeZoneInformation(TIME_ZONE);
i:=TIME_ZONE.Bias div 60;
i:= i * -1;
j:=TIME_ZONE.Bias mod 60;
j:=abs(j);
Result:=inttostr(i);
end;

 function DateTimeToUnix(ConvDate: TDateTime): Longint;
 begin
   //example: DateTimeToUnix(now);
  Result := Round((ConvDate - UnixStartDate) * 86400);
 end;

 function UnixToDateTime(USec: Longint): TDateTime;
 begin
   //Example: UnixToDateTime(1003187418);
  Result := (Usec / 86400) + UnixStartDate;
 end;

 function Translit(s: string): string;
const
  rus: string = 'абвгдеЄжзийклмнопрстуфхцчшщьыъэю€јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’÷„Ўў№џЏЁёя';
  lat: array[1..66] of string = ('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y', 'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f', 'kh', 'ts', 'ch', 'sh', 'shch', '''', 'y', '''', 'e', 'yu', 'ya', 'A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'Y', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F', 'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '''', 'Y', '''', 'E', 'Yu', 'Ya');
var
  p, i, l: integer;
begin
  Result := '';
  l := Length(s);
  for i := 1 to l do
  begin
    p := Pos(s[i], rus);
    if p<1 then Result := Result + s[i] else Result := Result + lat[p];
  end;
end;

function GetComputerNetName: string;
var
  buffer: array[0..255] of char;
  size: dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;

procedure TfrmCompileFirmware.PatchAndroidPolicy(FileName:string);
var
  CmdLine:string;
  TranslationFile:TStringList;
  ExtractAPK:TFWZipReader;
  BuildedAPK:TFWZipWriter;
  DestApkFile:TFWZipReader;
  ApkFile:TFWZipWriter;
begin
{47 lines from there removed due to proprietary code from AndroTech}
end;

procedure TfrmCompileFirmware.PatchAndroidLang(FileName:string);
var
  CmdLine:string;
  TranslationFile:TStringList;
  ExtractAPK:TFWZipReader;
  BuildedAPK:TFWZipWriter;
  DestApkFile:TFWZipReader;
  ApkFile:TFWZipWriter;
begin
{34 lines from there removed due to proprietary code from AndroTech}
end;

procedure TfrmCompileFirmware.BuildFirmware(FileName:string);
var
  FirmwareFile:TFWZipReader;
  BuildFirmwareFile:TFWZipWriter;
  ApkFileList, FrameWorkFile, FilesToDelete:TStringList;
  i:Integer;
  IniFiles, IniFile:TIniFile;
  CRC:string;
  t1, t2, tf: int64;
  BuildFile:Boolean;
  CmdLine, Publisher, unsignedPath, SignedPath, BoardModel, BPFingerprint, BPDescription, Support, PhoneModel, FirmwareVersion, Version:string;
begin
  MainForm.uGeneralProgress.Visible:=True;
  btnStartCompile.Enabled:=False;
  DelTree(MainForm.WorkDir+'Firmware');
  FirmwareFile:=TFWZipReader.Create;
  FirmwareFile.LoadFromFile(FileName);
  ForceDirectories(MainForm.WorkDir+'Firmware');
  FirmwareFile.OnProgress:=OnProgress;
  FirmwareFile.ExtractAll(MainForm.WorkDir+'Firmware');
  FirmwareFile.Free;
  if DirectoryExists(MainForm.WorkDir+'FirmwareUpdate') then CopyFiles(MainForm.WorkDir+'FirmwareUpdate', MainForm.WorkDir+'Firmware', '*');
  FilesToDelete:=TStringList.Create;
  FilesToDelete.LoadFromFile(MainForm.WorkDir+'Settings\FilesToDelete.conf');
  for i := 0 to FilesToDelete.Count-1 do begin
    if FileExists(MainForm.WorkDir+'Firmware\system\app\'+FilesToDelete[i]) then DeleteFile(MainForm.WorkDir+'Firmware\system\app\'+FilesToDelete[i]);
  end;
  FilesToDelete.Free;
  MainForm.TaskBar1.ProgressState:=TBPF_NOPROGRESS;
  MainForm.uGeneralProgress.Position:=0;
  FirmwareVersion:=GetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop', 'ro.build.version.incremental');
  PhoneModel:=GetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop', 'ro.product.device');
  BoardModel:=GetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop', 'ro.product.board');
  Version:=FirmwareVersion;
  MainForm.AddToLog('I','|          Phone model: '+PhoneModel);
  MainForm.AddToLog('I','|          Board model: '+BoardModel);
  MainForm.AddToLog('I','|     Firmware version: '+Version);
  MainForm.AddToLog('I','|   Firmware publisher: KDev Group');
  MainForm.AddToLog('I','|     Firmware support: KDev Group');
  MainForm.AddToLog('I','|    Firmware language: '+arrLang[cbbLangSelect.ItemIndex]+'-r'+arrRegions[cbbLangSelect.ItemIndex]);
  MainForm.AddToLog('I','|      Language author: KOJAN Development Group');
  CopyFiles(MainForm.DataDir+'LanguageFiles_Device\'+PhoneModel+'\', MainForm.DataDir+'LanguageFiles_Main\','*');
  ApkFileList:=TStringList.Create;
  FrameWorkFile:=TStringList.Create;
  ApkFileList.Clear;
  FrameWorkFile.Clear;
  FindFiles(MainForm.WorkDir+'Firmware','*.apk',ApkFileList);
  FindFiles(MainForm.WorkDir+'Firmware','framework-res.apk', FrameWorkFile);
  FindFiles(MainForm.DataDir+'Plugs','*.apk', FrameWorkFile);
  FindFiles(MainForm.WorkDir+'Firmware\system\framework','*.apk', FrameWorkFile);
  Version:=Version+'.kdg';

  for i := 0 to FrameWorkFile.Count-1 do begin
    CmdLine:=MainForm.CoreTool+' if "'+FrameWorkFile[i]+'"';
    ExecuteCommand('Installing framework ['+inttostr(i)+']', CmdLine, MainForm.DataDir);
  end;
  ExtractPrecompiled('FirmwareFolder', MainForm.WorkDir+'Firmware\');

  if DirectoryExists(MainForm.DataDir+'\Scripts') then begin
     MainForm.AddToLog('I', 'Jumping to script PHP engine...');
     StartScript(MainForm.DataDir+'\Scripts\','WorkWithFirmware.script', MainForm.WorkDir, MainForm.DataDir, MainForm.aAppsDir, 'none', 'none', 'none', MainForm.JavaCommand, MainForm.CoreTool, BuildFile);
     MainForm.AddToLog('I', 'Resuming core work...');
  end;

  if not chkNoApp.Checked then begin

    for i := 0 to ApkFileList.Count-1 do begin
      QueryPerformanceFrequency(tf);
      QueryPerformanceCounter(t1);
      MainForm.AddToLog('F',ExtractFileName(ApkFileList[i]));
      if not FileExists(MainForm.DataDir+'DisabledFiles\'+PhoneModel+'\'+GetFileNameWOExt(ExtractFileName(ApkFileList[i]))) then
      Memo1.Lines.Add('ѕеревод файла: ' + GetFileNameWOExt(ExtractFileName(ApkFileList[i]))+'. ќсталось: '+IntToStr(ApkFileList.Count-(i+1)));
      CompileFile(GetFileNameWOExt(ApkFileList[i]));
      MainForm.SetProgress(Round(((i+1)/ApkFileList.Count)*100));
      MainForm.uGeneralProgress.Position:=Round(((i+1)/ApkFileList.Count)*100);
      QueryPerformanceCounter(t2);
      memo1.Lines.Add('time = ' + floattostr((t2-t1) / tf));
      MainForm.AddToLog('I', 'time = ' + floattostr((t2-t1) / tf));
    end;

  end;
  DeleteFile(MainForm.WorkDir+'Firmware\META-INF\CERT.RSA');
  DeleteFile(MainForm.WorkDir+'Firmware\META-INF\CERT.SF');
  DeleteFile(MainForm.WorkDir+'Firmware\META-INF\MANIFEST.MF');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.product.locale.language', arrLang[cbbLangSelect.ItemIndex]);
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.product.locale.region', arrRegions[cbbLangSelect.ItemIndex]);
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.display.id', 'MIUI '+Version);
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','persist.sys.timezone',TimeZone);
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.config.ringtone', 'MI.ogg');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.config.notification_sound','FadeIn.ogg');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.config.alarm_alert', 'GoodMorning.ogg');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.config.sms_received_sound','FadeIn.ogg');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.config.sms_delivered_sound','MessageComplete.ogg');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.telephony.call_ring.delay','5000');
  {if false then begin
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','dalvik.vm.heapsize','48m');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.ext4fs','1');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','windowsmgr.max_events_per_sec','60');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.telephony.call_ring.multiple','false');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','dalvik.vm.verify-bytecode','false');
    if not ((PhoneModel='p990') or (PhoneModel='p999')) then
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','dalvik.vm.dexopt-flags','v=n,o=v');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.vold.umsdirtyratio','20');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','dalvik.vm.usemodule','com.kojan.dalvikvm');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.kernel.android.checkjni','0');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.HOME_APP_MEM','4096');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.HOME_APP_ADJ','1');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','wifi.supplicant_scan_interval','100');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.media.enc.jpeg.quality','100');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.telephony.call_ring.delay','0');
  end;   }
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.type','user');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.tags','release-keys');
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.date', FormatDateTime('dddddd' , Now)+' '+FormatDateTime('tt' , Now)+' GMT+'+GetTimeZone);
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.version.incremental', Version);
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.user', Translit(LowerCase(GetUserFromWindows)));
  SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.host', Translit(LowerCase(GetComputerNetName)));
  {if MainForm.AddonsChecked[3] then begin
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.service.swiqi.supported', 'true');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','persist.service.swiqi.enable', '1');
  end;
  if MainForm.AddonsChecked[4] then begin
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.semc.sound_effects_enabled', 'true');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.semc.xloud.supported', 'true');
    SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','persist.service.xloud.enable', '1');
  end;  }
  if FileExists(MainForm.WorkDir+'Settings\Firmware.conf') then begin
    IniFile:=TIniFile.Create(MainForm.WorkDir+'Settings\Firmware.conf');
    BPDescription:=IniFile.ReadString(PhoneModel,'Description','0');
    BPFingerprint:=IniFile.ReadString(PhoneModel,'Fingerprint','0');
    if (BPDescription<>'0') and (BPFingerprint<>'0') then begin
      SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.description',BPDescription);
      SetBuildPropVariable(MainForm.WorkDir+'Firmware\system\build.prop','ro.build.fingerprint',BPFingerprint);
    end;
    IniFile.UpdateFile;
    IniFile.Free;
  end;
  PatchAndroidPolicy(MainForm.WorkDir+'Firmware\system\framework\android.policy.jar');
  if FileExists(MainForm.WorkDir+'Firmware\system\framework\miui-framework.jar') then PatchAndroidLang(MainForm.WorkDir+'Firmware\system\framework\miui-framework.jar')
  else PatchAndroidLang(MainForm.WorkDir+'Firmware\system\framework\framework.jar');

  if DirectoryExists(MainForm.DataDir+'\Scripts\') then begin
     MainForm.AddToLog('I', 'Jumping to script PHP engine...');
     StartScript(MainForm.DataDir+'\Scripts\', 'PostBuildFirmware.script', MainForm.WorkDir, MainForm.DataDir, MainForm.aAppsDir, 'none', 'none', 'none', MainForm.JavaCommand, MainForm.CoreTool, BuildFile);
     MainForm.AddToLog('I', 'Resuming core work...');
  end;

  UnsignedPath:='miuirussia_'+PhoneModel+'_'+FirmwareVersion;
  BuildFirmwareFile:=TFWZipWriter.Create;
  BuildFirmwareFile.AddFolder('',MainForm.WorkDir+'Firmware','*');
  BuildFirmwareFile.BuildZip(IncludeTrailingBackslash(ExtractFilePath(FileName))+UnSignedPath+'_unsigned.zip');
  BuildFirmwareFile.OnProgress:=OnProgress;
  BuildFirmwareFile.Free;
  SignedPath:=IncludeTrailingBackslash(ExtractFilePath(FileName))+UnSignedPath+'_sgn.zip';
  CmdLine:=MainForm.JavaCommand+' -jar signapk.jar testkey.x509.pem testkey.pk8 "'+IncludeTrailingBackslash(ExtractFilePath(FileName))+UnSignedPath+'_unsigned.zip'+'" "'+ SignedPath +'"';
  if ExecuteCommand('Signing firmware', CmdLine, MainForm.aAppsDir, True) then DeleteFile(IncludeTrailingBackslash(ExtractFilePath(FileName))+UnSignedPath+'_unsigned.zip');
  CRC:=LowerCase(IntToHex(GetFileCRC(SignedPath), 8));
  RenameFile(SignedPath, IncludeTrailingBackslash(ExtractFilePath(FileName))+UnSignedPath+'_'+CRC+'_4.0.zip');
  MainForm.TaskBar1.ProgressState:=TBPF_NOPROGRESS;
  if DirectoryExists(MainForm.WorkDir+'ResolveConf') then ShowMessage('¬нимание!'+#10#13+'¬ прошивке есть ошибки перевода приложений.'+#10#13+' Ёто означает, что прошивка не официальна€ (или есть ошибки в переводе) и ответственность за еЄ работоспособность несет еЄ создатель, а не KDev Group!'+#10#13+'KDev Group не поощр€ет перевод сторонних прошивок, а так-же не разбираетс€ в файлах этих прошивок'+#10#13+'KDG разбирает ошибки только при условии перевода прошивки из официальных источников (miui.com и miuiandroid.com)');
  DelTree(MainForm.WorkDir+'Firmware');
  MainForm.btnNext.Enabled:=True;
  Beep;
  MainForm.uGeneralProgress.Visible:=False;
end;

procedure TfrmCompileFirmware.btnStartCompileClick(Sender: TObject);
var
  FirmwareList:TStringList;
  i:integer;
begin
  if MainForm.MultiCompile then begin
    FirmwareList:=TStringList.Create;
    FindFiles(IncludeTrailingBackslash(MainForm.FirmwareFile), '*.zip', FirmwareList);
    for i := 0 to FirmwareList.Count-1 do begin
      Memo1.Lines.Add(' омпил€ци€: '+FirmwareList[i]);
      BuildFirmware(FirmwareList[i]);
      Memo1.Lines.Add('«авершено!');
    end;
    FirmwareList.Free;
  end else BuildFirmware(MainForm.FirmwareFile);
end;

procedure TfrmCompileFirmware.cbbTimeZoneSelectChange(Sender: TObject);
var
  timezones:TNativeXml;
  timeNodes:TsdNodeList;
  SetFile:TIniFile;
  i:Integer;
begin
  timezones:=TNativeXml.Create(Self);
  timezones.LoadFromFile(MainForm.DataDir+'timezones.xml');
  timeNodes:=TsdNodeList.Create;
  timezones.Root.FindNodes('timezone', timeNodes);
  for i := 0 to timeNodes.Count-1 do begin
      if (cbbTimeZoneSelect.Text=timeNodes[i].Value) then begin
       TimeZone:=timeNodes[i].AttributeValueByName['id'];
       Application.ProcessMessages;
       Memo1.Lines.Add('„асовой по€с: '+TimeZone);
      end;
  end;

  timeNodes.Free;
  timezones.Free;

  SetFile:=TIniFile.Create(MainForm.WorkDir+'Settings\TimeZone.conf');
  SetFile.WriteInteger('TimeZone','InstalledId',cbbTimeZoneSelect.ItemIndex);
  SetFile.Free;
end;

procedure TfrmCompileFirmware.OnProgress(Sender: TObject; const FileName: string; Percent: Byte; TotalPercent: Byte; var Cancel: Boolean);
begin
  MainForm.uGeneralProgress.Position:=TotalPercent;
  MainForm.TaskBar1.ProgressState:=TBPF_NORMAL;
  MainForm.TaskBar1.ProgressValue:=TotalPercent;
  Application.ProcessMessages;
end;

end.
