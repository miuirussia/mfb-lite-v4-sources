unit ScriptFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  NativeXml, Winapi.ShellAPI, FWZipReader;

type
  TfrmScripts = class(TFrame)
    ListView1: TListView;
    btnDownScript: TButton;
    btnDelScript: TButton;
    btnCleanScr: TButton;
    btnUpdateScr: TButton;
    procedure init;
    procedure UpdateList;
    procedure btnUpdateScrClick(Sender: TObject);
    procedure btnDownScriptClick(Sender: TObject);
    procedure btnDelScriptClick(Sender: TObject);
    procedure btnCleanScrClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses MainUnit, Winapi.UrlMon;

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

function DownloadFile(SourceFile, DestFile: string): Boolean;
begin
  try
    Result := UrlDownloadToFile(nil, PChar(SourceFile), PChar(DestFile), 0, nil) = 0;
  except
    Result := False;
  end;
end;

procedure TfrmScripts.UpdateList;
var
  i:integer;
  repoXML:TNativeXml;
  repoItems:TsdNodeList;
begin
  repoXML:=TNativeXml.Create(Self);
  DeleteFile(MainForm.DataDir+'repository.xml');
  if DownloadFile('http://auth.romz.bz/repository.xml', MainForm.DataDir+'repository.xml') then begin
  repoXML.LoadFromFile(MainForm.DataDir+'repository.xml');
  DeleteFile(MainForm.DataDir+'repository.xml');
  repoItems:=TsdNodeList.Create;
  repoXML.Root.FindNodes('script',repoItems);
  ListView1.Clear;

  for i := 0 to repoItems.Count-1 do begin
      with ListView1.Items.Add do begin
      Caption:=repoItems[i].AttributeValueByName['name'];
      SubItems.Add(repoItems[i].AttributeValueByName['author']);
      SubItems.Add(repoItems[i].AttributeValueByName['desc']);
      SubItems.Add(repoItems[i].AttributeValueByName['folder']);
      if DirectoryExists(MainForm.DataDir+'Scripts\'+repoItems[i].AttributeValueByName['folder']) then GroupID:=0 else GroupID:=1;
      end;

  end;
  repoItems.Free;
  end;
  repoXML.Free;
end;

procedure TfrmScripts.btnCleanScrClick(Sender: TObject);
begin
  DelTree(MainForm.DataDir+'Scripts');
  UpdateList;
end;

procedure TfrmScripts.btnDelScriptClick(Sender: TObject);
begin
  DelTree(MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]);
  UpdateList;
end;

procedure TfrmScripts.btnDownScriptClick(Sender: TObject);
var
  i:integer;
  repoXML:TNativeXml;
  repoItems:TsdNodeList;
begin

   {ListView1.Clear;




  ForceDirectories(MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]);
  GetInetFile('http://auth.romz.bz/'+ListView1.Selected.SubItems[2]+'/Compile.script', MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]+'\Compile.script');
  GetInetFile('http://auth.romz.bz/'+ListView1.Selected.SubItems[2]+'/PostBuildFirmware.script', MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]+'\PostBuildFirmware.script');
  GetInetFile('http://auth.romz.bz/'+ListView1.Selected.SubItems[2]+'/Precomp.script', MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]+'\Precomp.script');
  GetInetFile('http://auth.romz.bz/'+ListView1.Selected.SubItems[2]+'/WorkWithFirmware.script', MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]+'\WorkWithFirmware.script');
  GetInetFile('http://auth.romz.bz/'+ListView1.Selected.SubItems[2]+'/Files.zip', MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]+'\Files.zip');
  UpdateList;}
    ForceDirectories(MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]);
    repoXML:=TNativeXml.Create(Self);
    DeleteFile(MainForm.DataDir+'repository.xml');
    if DownloadFile('http://auth.romz.bz/repository.xml', MainForm.DataDir+'repository.xml') then begin
    repoXML.LoadFromFile(MainForm.DataDir+'repository.xml');
    DeleteFile(MainForm.DataDir+'repository.xml');
    repoItems:=TsdNodeList.Create;
    if Assigned(repoXML.Root.NodeByAttributeValue('script', 'folder', ListView1.Selected.SubItems[2])) then begin
      repoXML.Root.NodeByAttributeValue('script', 'folder', ListView1.Selected.SubItems[2]).FindNodes('file', repoItems);
      for i := 0 to repoItems.Count-1 do begin
        DownloadFile('http://auth.romz.bz/'+ListView1.Selected.SubItems[2]+'/'+repoItems[i].Value, MainForm.DataDir+'Scripts\'+ListView1.Selected.SubItems[2]+'\'+repoItems[i].Value);
      end;
    end;
    repoItems.Free;
    end;
    repoXML.Free;
    UpdateList;
end;

procedure TfrmScripts.btnUpdateScrClick(Sender: TObject);
begin
  UpdateList;
end;

procedure TfrmScripts.init;
var
  i:Integer;
begin
  for i := 0 to ListView1.Columns.Count-1 do begin
    ListView1.Column[i].Width:=Round(ListView1.Width/(ListView1.Columns.Count))+18;
  end;
  ListView1.Clear;
  UpdateList;
end;

end.
