unit OpenFirmware;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, JvBaseDlg,
  JvBrowseFolder, Vcl.ComCtrls, Vcl.CheckLst, System.Masks, Vcl.Samples.Spin, Winapi.ShellAPI;

type
  TfrmOpenFirmware = class(TFrame)
    edtFirmwareFile: TEdit;
    btnBrowseFirmware: TButton;
    OpenDialog1: TOpenDialog;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    grpGeneralGroup: TGroupBox;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    SpinEdit2: TSpinEdit;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    CheckBox3: TCheckBox;
    Button1: TButton;
    Edit2: TEdit;
    Label3: TLabel;
    Button2: TButton;
    procedure btnBrowseFirmwareClick(Sender: TObject);
    procedure edtFirmwareFileChange(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses MainUnit;

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

procedure TfrmOpenFirmware.btnBrowseFirmwareClick(Sender: TObject);
begin
  if MainForm.MultiCompile then begin if JvBrowseForFolderDialog1.Execute then edtFirmwareFile.Text:=JvBrowseForFolderDialog1.Directory end
  else if OpenDialog1.Execute then edtFirmwareFile.Text:=OpenDialog1.FileName;
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

procedure TfrmOpenFirmware.Button1Click(Sender: TObject);
var
  TempFile:TStringList;
begin
  TempFile:=TStringList.Create;
  DelTree(MainForm.DataDir+'Patches');
  ForceDirectories(MainForm.DataDir+'Patches');
  ForceDirectories(MainForm.DataDir+'Patches\MiuiHome\res\values');
  TempFile.Clear;
  TempFile.Add('<?xml version="1.0" encoding="utf-8"?>');
  TempFile.Add('<resources>');
  TempFile.Add('    <integer name="config_cell_count_x">'+IntToStr(SpinEdit1.Value)+'</integer>');
  TempFile.Add('    <integer name="config_cell_count_y">'+IntToStr(SpinEdit2.Value)+'</integer>');
  TempFile.Add('</resources>');
  TempFile.SaveToFile(MainForm.DataDir+'Patches\MiuiHome\res\values\integers.xml.part');
  TempFile.Clear;
  TempFile.Add('<?xml version="1.0" encoding="utf-8"?>');
  TempFile.Add('<resources>');
  if CheckBox3.Checked then
  TempFile.Add('    <bool name="config_hardwareAccelerated">true</bool>')
  else TempFile.Add('    <bool name="config_hardwareAccelerated">false</bool>');
  TempFile.Add('</resources>');
  TempFile.SaveToFile(MainForm.DataDir+'Patches\MiuiHome\res\values\bools.xml.part');
  TempFile.Clear;
  if CheckBox1.Checked then begin
  ForceDirectories(MainForm.DataDir+'Patches\Updater\res\values');
  TempFile.Add('<?xml version="1.0" encoding="utf-8"?>');
  TempFile.Add('<resources>');
  if CheckBox1.Checked then
  TempFile.Add('    <bool name="has_kernel_upgrade_check">true</bool>')
  else TempFile.Add('    <bool name="has_kernel_upgrade_check">false</bool>');
  TempFile.Add('</resources>');
  TempFile.SaveToFile(MainForm.DataDir+'Patches\Updater\res\values\bools.xml.part');
  TempFile.Clear;
  TempFile.Add('<?xml version="1.0" encoding="utf-8"?>');
  TempFile.Add('<resources>');
  TempFile.Add('    <string name="kernel_upgrade_link">'+Edit2.Text+'</string>');
  TempFile.Add('    <string name="kernel_upgrade_version">'+Edit1.Text+'</string>');
  TempFile.Add('</resources>');
  TempFile.SaveToFile(MainForm.DataDir+'Patches\Updater\res\values\strings.xml.part');
  TempFile.Clear;
  end;
  TempFile.Free;
end;

procedure TfrmOpenFirmware.Button2Click(Sender: TObject);
begin
  DelTree(MainForm.DataDir+'Patches');
end;

procedure TfrmOpenFirmware.CheckBox2Click(Sender: TObject);
begin
  MainForm.MultiCompile:=CheckBox2.Checked;
end;

procedure TfrmOpenFirmware.edtFirmwareFileChange(Sender: TObject);
begin
  if not MainForm.MultiCompile then begin
  if FileExists(edtFirmwareFile.Text) then begin
  MainForm.FirmwareFile:=edtFirmwareFile.Text;
  MainForm.btnNext.Enabled:=True;
  end else MainForm.btnNext.Enabled:=False;
  end else begin
   if DirectoryExists(edtFirmwareFile.Text) then begin
     MainForm.FirmwareFile:=edtFirmwareFile.Text;
     MainForm.btnNext.Enabled:=True;
   end else MainForm.btnNext.Enabled:=False;
  end;
end;

end.
