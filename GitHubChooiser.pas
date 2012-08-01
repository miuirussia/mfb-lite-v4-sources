unit GitHubChooiser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  dxGDIPlusClasses, Vcl.ExtCtrls, Vcl.CheckLst, System.Masks, Winapi.ShellAPI;

type
  TGitHubRepositories = class(TFrame)
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    chklstGitHubsGroup: TCheckListBox;
    btnGetLangs: TButton;
    procedure btnGetLangsClick(Sender: TObject);
    procedure chklstGitHubsGroupClickCheck(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses MainUnit;

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
  if MatchesMask(Name, Mask) then
  List.Add(StartFolder+Name);
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
    //MainForm.AddToLog('I','Building translation for this file...Done!');
  except
    on e:Exception do
      MainForm.AddToLog('E','Error at copying files into '+aDirTo+#13+#13+e.Message);
  end;
end;

function GetFileNameWOExt(fn:String):String;
begin
  Result := Copy(fn, 1, Length(fn)-Length(ExtractFileExt(fn)));
end;

procedure TGitHubRepositories.btnGetLangsClick(Sender: TObject);
var
  i,j,k,z:integer;
  CmdLine:string;
  DirList, DirList2, DirList3:TStringList;
begin
  btnGetLangs.Enabled:=False;
  DelTree(MainForm.DataDir+'LanguageFiles_Git');
  DelTree(MainForm.DataDir+'LanguageFiles_Main');
  DelTree(MainForm.DataDir+'LanguageFiles_Device');
  ForceDirectories(MainForm.DataDir+'LanguageFiles_Main');
  ForceDirectories(MainForm.DataDir+'LanguageFiles_Device');
   for i := 0 to chklstGitHubsGroup.Items.Count-1 do begin
     if chklstGitHubsGroup.Checked[i] then begin
      DelTree(MainForm.DataDir+'LanguageFiles_Tmp');
      CmdLine:=MainForm.aAppsDir+'git.exe clone '+MainForm.GitHubs[i]+' "'+MainForm.DataDir+'LanguageFiles_Tmp"';
      MainForm.ExecuteCommand('Checking for new translation from GIT: '+MainForm.GitHubs[i], CmdLine, MainForm.aAppsDir, True);
      DelTree(MainForm.DataDir+'LanguageFiles_Tmp\.git');
      if MainForm.ULTInfo.Strings[i]='no' then CopyFiles(MainForm.DataDir+'LanguageFiles_Tmp\', MainForm.DataDir+'LanguageFiles_Main\', '*')
      else begin
        DirList:=TStringList.Create;
        FindFolders(MainForm.DataDir+'LanguageFiles_Tmp\'+MainForm.ULTInfo[i]+'\main\','*', DirList);
        for j := 0 to DirList.Count-1 do begin
          RenameFile(DirList[j],GetFileNameWOExt(DirList[j]));
          DirList2:=TStringList.Create;
          FindFolders(GetFileNameWOExt(DirList[j]),'*', DirList2);
          ForceDirectories(GetFileNameWOExt(DirList[j])+'\Files');
          for k := 0 to DirList2.Count-1 do begin
            CopyFiles(DirList2[k], GetFileNameWOExt(DirList[j])+'\Files\','*');
            Application.ProcessMessages;
            DelTree(DirList2[k]);
          end;
          DirList2.Free;
        end;
        CopyFiles(MainForm.DataDir+'LanguageFiles_Tmp\'+MainForm.ULTInfo.Strings[i]+'\main\', MainForm.DataDir+'LanguageFiles_Main\', '*');
        DirList.Free;

        DirList:=TStringList.Create;
        FindFolders(MainForm.DataDir+'LanguageFiles_Tmp\'+MainForm.ULTInfo[i]+'\device\','*', DirList);
        for j := 0 to DirList.Count-1 do begin
          DirList3:=TStringList.Create;
          FindFolders(DirList[j],'*', DirList3);
          for z := 0 to DirList3.Count-1 do begin
            RenameFile(DirList3[z],GetFileNameWOExt(DirList3[z]));
            DirList2:=TStringList.Create;
            FindFolders(GetFileNameWOExt(DirList3[z]),'*', DirList2);
            ForceDirectories(GetFileNameWOExt(DirList3[z])+'\Files');
            for k := 0 to DirList2.Count-1 do begin
              CopyFiles(DirList2[k], GetFileNameWOExt(DirList3[z])+'\Files\','*');
              Application.ProcessMessages;
              DelTree(DirList2[k]);
            end;
            DirList2.Free;
          end;
          DirList3.Free;
        end;
        CopyFiles(MainForm.DataDir+'LanguageFiles_Tmp\'+MainForm.ULTInfo.Strings[i]+'\device\', MainForm.DataDir+'LanguageFiles_Device\', '*');
        DirList.Free;
      end;
     end;
   end;

  DelTree(MainForm.DataDir+'LanguageFiles_Tmp');
  CmdLine:=MainForm.aAppsDir+'git.exe clone git://github.com/miuirussia/translate-addons.git "'+MainForm.DataDir+'LanguageFiles_Tmp"';
  MainForm.ExecuteCommand('Checking for new translation from GIT: git://github.com/miuirussia/translate-addons.git', CmdLine, MainForm.aAppsDir, True);
  DelTree(MainForm.DataDir+'LanguageFiles_Tmp\.git');
  CopyFiles(MainForm.DataDir+'LanguageFiles_Tmp\', MainForm.DataDir+'LanguageFiles_Main\', '*');
  DelTree(MainForm.DataDir+'LanguageFiles_Tmp');
  DelTree(MainForm.DataDir+'LanguageFiles_Main\BugReport');
  btnGetLangs.Enabled:=True;
  MainForm.btnNext.Enabled:=True;
end;

procedure TGitHubRepositories.chklstGitHubsGroupClickCheck(Sender: TObject);
var
  i:Integer;
  chk:Boolean;
begin
  chk:=False;
  for i := 0 to chklstGitHubsGroup.Items.Count-1 do begin
     if chklstGitHubsGroup.Checked[i] then chk:=True;
  end;
  btnGetLangs.Enabled:=chk;
end;

procedure TGitHubRepositories.Image1Click(Sender: TObject);
begin
  MainForm.btnNext.Enabled:=True;
end;

end.
