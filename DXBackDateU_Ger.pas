unit DXBackDateU_Ger; 
{
 // 14-DEC-98 as (Arne Schäpers)
 Setzt die Versionsnummern der DirectX-DLLs auf "DirectX 1"
 zurück und macht damit die Reinstallation älterer DX-Versionen
 möglich.
 Das Programm ändert außer dem Registrierungseintrag für das
 DirectX-Setup *nichts* am System. Es sorgt lediglich dafür,
 daß die installierten DirectX-DLLs, Treiber usw. nach
 dem nächsten Neustart eine gehackte Versionsnummer haben.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, IniFiles, Registry, FileCtrl;
type
  TDXBackForm = class(TForm)
    Label2: TLabel;
    OpenDXInfDialog: TOpenDialog;
    mHelp: TMemo;
    bHelpOK: TButton;
    GroupStep1: TGroupBox;
    bHelp: TButton;
    Label3: TLabel;
    GroupStep2: TGroupBox;
    Label1: TLabel;
    eDirectXINF: TEdit;
    bBrowseDXInf: TButton;
    lWinNTHack: TLabel;
    GroupStep3: TGroupBox;
    mProtocol: TMemo;
    bResetVersionNumbers: TButton;
    cRebootIfDone: TCheckBox;
    procedure bBrowseDXInfClick(Sender: TObject);
    procedure bResetVersionNumbersClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure eDirectXINFChange(Sender: TObject);
    procedure bHelpClick(Sender: TObject);
    procedure bHelpOKClick(Sender: TObject);
  private
    IsWindowsNT: Boolean;
    WinDir, SystemDir, TempDir: String;
    DXFileList, DXCopyList: TStringList;
  protected
    function CreateModifiedCopy(FName: String): Boolean;
    procedure ResetVersionEntryInRegistry;
    function PendingSystemUpdates: Boolean;
  end;

var DXBackForm: TDXBackForm;

implementation
{$R *.DFM}

procedure TDXBackForm.FormCreate(Sender: TObject);
var Buf: Array[0..MAX_PATH] of Char;
  ProcHandle, Token: THandle;  // NT only (für Reboot)
  PvOld, PvNew: TTokenPrivileges; Dummy: DWORD;  // dito

  function BufToDir: String;
  begin
    Result := StrPas(Buf);
    if Result[Length(Result)] <> '\'then Result := Result + '\';
  end;

begin
  DXFileList := TStringList.Create;
  DXCopyList := TStringList.Create;
  IsWindowsNT := Win32Platform and VER_PLATFORM_WIN32_NT <> 0;
  if IsWindowsNT then
  begin  // Reboot-Privileg setzen, fixe Dateiliste als Vorgabe
    ProcHandle := GetCurrentProcess;
    if OpenProcessToken(ProcHandle, TOKEN_ADJUST_PRIVILEGES
         or TOKEN_QUERY, Token)
      and LookupPrivilegeValue('','SeShutdownPrivilege',
          PvNew.Privileges[0].LUID) then
    with PvNew do
    begin
     PrivilegeCount := 1;
     Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
     cRebootIfDone.Enabled := AdjustTokenPrivileges(Token, False,
       PvNew, 4 + 12* PvNew.PrivilegeCount, PvOld, Dummy);
    end;
    eDirectXINF.Text := ExtractFilePath(ParamStr(0))+'DXforNT.INF';
    lWinNTHack.Visible := bResetVersionNumbers.Enabled;
  end else
  begin
    lWinNTHack.Visible := False;
    bResetVersionNumbers.Enabled := False;
  end;
  cRebootIfDone.Checked := cRebootIfDone.Enabled;

  // Windows, System- und TEMP-Verzeichnis ermitteln
  GetWindowsDirectory(Buf,Sizeof(Buf)); WinDir := BufToDir;
  GetSystemDirectory(Buf,SizeOf(Buf)); SystemDir := BufToDir;
  GetTempPath(SizeOf(Buf),Buf); TempDir := BufToDir;
  with mProtocol.Lines do
  begin
    Clear;
    Add('Protocol:'); Add('WinDir = '+WinDir);
    Add('SystemDir = '+SystemDir); Add('TempDir = '+TempDir);
  end;
  if TempDir = AnsiUpperCase(ExtractFilePath(ParamStr(0))) then
  begin
    TempDir := WinDir[1]+':\DXTEMP';
    if not DirectoryExists(TempDir) and
        not CreateDirectory(PChar(TempDir),nil) then
    begin
      ShowMessage('Kein TEMP-Verzeichnis; Fehler beim Anlegen '+
       'von'+TempDir+'. Programm wird abgebrochen (sorry).');
      Halt;
    end else
    begin
      ShowMessage('Kein TEMP-Verzeichnis; Verzeichnis '+TempDir+
        ' angelegt - kann nach Neustart gelöscht werden.');
      TempDir := TempDir + '\';
    end;
  end;
  // Stehen im Moment System-Updates an?
  if PendingSystemUpdates then PostQuitMessage(0);
end;

procedure TDXBackForm.FormDestroy(Sender: TObject);
begin
 DXFileList.Free;
 DXCopyList.Free;
end;

function TDXBackForm.PendingSystemUpdates: Boolean;
const Renames = 'PendingFileRenameOperations';
var Reg: TRegistry; WinInit: TIniFile; RenameList: TStringList;
    SZMultiBuf: String; SZMultiLen, SZType: DWord; x: Integer;
begin
  Result := False;
  RenameList := TStringList.Create;
  // Existiert im Moment eine Liste mit umzubenennenden Dateien?
  if not IsWindowsNT then
  begin
    WinInit := TIniFile.Create(WinDir+'WININIT.INI');
    WinInit.ReadSectionValues('Rename',RenameList);
    if RenameList.Count <> 0 then
    begin
      RenameList.Insert(0,'Ein anderes Setup-Programm hat '+
      'Anweisungen zur Aktualisierung von Systemdateien '+
      'hinterlassen, die noch nicht ausgeführt wurden. '+
      'Bitte starten Sie das System neu.'#13#10+
      'Wenn dieser Fehler weiterhin auftritt, sollten Sie die '+
      'Datei '+WinInit.FileName+' manuell bearbeiten (oder '+
      'komplett löschen).'#13#10+
      'Aktuelle Einträge in '+WinInit.FileName+' sind:');
      ShowMessage(RenameList.Text);
      Result := True;
    end;
    WinInit.Free;
  end;
  // Windows NT und Win98(?)
  Reg := TRegistry.Create;
  with Reg do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey('\SYSTEM\CurrentControlSet\Control\'+
      'Session Manager', False) then
    begin  // REGSZ_MULTI ist anscheinend jenseits von TRegistry
      if RegQueryValueEx(CurrentKey,Renames, nil, @SZType,nil,
         @SZMultiLen) = ERROR_SUCCESS then
      begin
        SetLength(SZMultiBuf,SZMultiLen);
        RegQueryValueEx(CurrentKey,Renames, nil, @SZType,
                     PByte(PChar(SZMultiBuf)), @SZMultiLen);
        for x := SZMultiLen-1 downto 1 do
          if SZMultiBuf[x] = #0 then SZMultiBuf[x] := #13;
        x := Pos(#13'!\??\',SZMultiBuf);
        while x <> 0 do
        begin
          System.Delete(SZMultiBuf,x,5); SZMultiBuf[x] := '=';
          x := Pos(#13'!\??\',SZMultiBuf);
        end;
        RenameList.Text := SZMultiBuf;
        if RenameList.Count <> 0 then
        begin
          RenameList.Insert(0,'Ein anderes Setup-Programm hat '+
          'Anweisungen zur Aktualisierung von Systemdateien '+
          'hinterlassen, die noch nicht ausgeführt wurden. '+
          'Bitte starten Sie das System neu.'#13#10+
          'Wenn dieser Fehler weiterhin auftritt, benutzen Sie '+
          'bitte RegEdt32 (nicht RegEdit!) zum Zurücksetzen '+
          'von HKLM\SYSTEM\CurrentControlSet\Control\'+
          'Session Manager\PendingFileRenameOperations'#13#10+
          'Aktuelle Einträge in diesem Wert sind:');
          ShowMessage(RenameList.Text);
          Result := True;
        end;
      end;
    end;
    Free;
  end;
  RenameList.Free;
end;

procedure TDXBackForm.bResetVersionNumbersClick(Sender: TObject);
var x: Integer; FName: String; WinInit: TIniFile;

   // Kurze Dateinamen für die Einträge in Wininit.ini
   function ShortPath(LongPath: String): String;
   var ShortBuf: Array[0..255] of Char;
   begin
     if GetShortPathName(PChar(LongPath),@ShortBuf,255) = 0
       then RaiseLastWin32Error;
     Result := StrPas(ShortBuf);
   end;

begin
  try
    DXFileList.LoadFromFile(eDirectXInf.Text);
  except
    ShowMessage('Fehler beim Lesen von '+eDirectXInf.Text); Exit;
  end;
  DXCopyList.Clear;
  for x := 0 to DXFileList.Count-1 do
  begin
    FName := AnsiUpperCase(Trim(DXFileList[x]));
    if (FName <> '') and (Pos('[',FName) = 0) then
    begin
      // Format von DirectX.INF:
      // 1,Pfad/Dateiname,... (1. Feld =  Diskettennummer)
      // 1,Pfad/Dateiname,...
      // Format von DXForNT.INF:
      // Dateiname
      // Dateiname
      FName := Copy(FName,Pos(',',FName)+1,255);
      if Pos(',',FName) <> 0
            then FName := Trim(Copy(FName,1,Pos(',',FName)-1));
      FName := ExtractFileName(FName);
      if (Pos(Copy(FName,Pos('.',FName)+1,3),'EXEDLLCPLVXD') <> 0)
        and FileExists(SystemDir+FName) then
      begin
        try
          if CreateModifiedCopy(FName) then DXCopyList.Add(FName);
        except
          on E: Exception do
            if MessageDlg(Format('Anlegen einer Kopie von %s '+
             'mit Fehler "%s" gescheitert. Programm trotzdem '+
             'fortsetzen?', [FName,E.Message]),
             mtError,[mbNo,mbYes], 0) <> mrYes then
            begin
              ShowMessage('Programm abgebrochen'); Close; Exit;
            end;
        end;
      end;
    end;
  end;
  if DXCopyList.Count = 0
    then ShowMessage('Nichts zu tun (falsche INF-Datei)?')
  else
  begin  // Die beim nächsten Systemstart zu kopierenden Dateien
    ResetVersionEntryInRegistry;
    if IsWindowsNT then
    begin  // MoveFileEx -> Registrierungseinträge
      for x := 0 to DXCopyList.Count-1 do
        if not MoveFileEx(PChar(TempDir+DXCopyList[x]),
          PChar(SystemDir+DXCopyList[x]),
        MOVEFILE_DELAY_UNTIL_REBOOT or MOVEFILE_REPLACE_EXISTING)
         then RaiseLastWin32Error;
    end else
    begin  // Windows 9x (Win98 ginge auch mit MoveFileEx)
      WinInit := TIniFile.Create(WinDir+'WININIT.INI');
      for x := 0 to DXCopyList.Count-1 do
        WinInit.WriteString('rename',
          ShortPath(SystemDir+DXCopyList[x]),
          ShortPath(TempDir+DXCopyList[x]));
      WinInit.Free;
    end;

    if cRebootIfDone.Checked then
    begin
      ShowMessage('Erledigt. Das System wird nach einem Klick '+
        'auf OK neu gestartet.');
      ExitWindowsEx(EWX_REBOOT,0);
      Close;
    end else
    begin
      ShowMessage('Erledigt. Systemdateien mit zurückgesetzten '+
      'Versionsnummern werden beim nächsten Hochfahren kopiert.');
      Close;
    end;
  end;
end;

procedure TDXBackForm.ResetVersionEntryInRegistry;
var Reg: TRegistry; DXCurVersion, DXVersion: String; x: Integer;
begin
  Reg := TRegistry.Create;
  with Reg do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey('\Software\Microsoft\DirectX',False) then
    begin
      DXCurVersion := ReadString('Version');
      if DXCurVersion <> '' then
      begin
        DXVersion := DXCurVersion;
        if Win32MajorVersion = 4 then DXVersion[1] := '4';
        for x := Pos('.',DXVersion)+1 to Length(DXVersion) do
          if DXVersion[x] <> '.' then DXVersion[x] := '0';
        WriteString('Version',DXVersion);
        mProtocol.Lines.Add(Format('%s von %s nach %s',
          ['\HKLM\Software\Microsoft\DirectX\Version',
           DXCurVersion, DXVersion]));
      end
        else mProtocol.Lines.Add('Kein Registrierungseintrag '+
         'für die Versionsnummer des DirectX-Setups vorhanden');
    end;
    Free;
  end;
end;

function TDXBackForm.CreateModifiedCopy(FName: String): Boolean;
var Buf: Pointer; FixedFileInfo, PInfo: PVSFixedFileInfo;
    BufSize: Integer; FFInfoLen: UINT;
    FStream: TFileStream; Stream: TMemoryStream;
    x: Integer; FoundInfo: Boolean; InfoCount: Integer;
    Prec3, Prec4: Integer;
    dummy: DWORD;

  procedure ShowError(Msg: String);
  begin
    Result := False;
    mProtocol.Lines.Add(FName+': *** '+Msg);
    raise Exception.Create(FName+': '+Msg);
  end;
begin
  BufSize := GetFileVersionInfoSize(PChar(SystemDir+FName),Dummy);
  if BufSize = 0 then ShowError('keine Versionsinformationen');
  Stream := nil;
  GetMem(Buf,BufSize);
  try
    if not GetFileVersionInfo(PChar(SystemDir+FName),0,BufSize,Buf)
      then ShowError('keine Versionsinformationen');

    if not VerQueryValue(Buf, '\', Pointer(FixedFileInfo), FFInfoLen)
      then ShowError('no fixed file info (shouldn''t happen)');

    try
      FStream := TFileStream.Create(SystemDir+FName,
          fmOpenRead or fmShareDenyNone);
      Stream := TMemoryStream.Create;
      Stream.CopyFrom(FStream,0); FStream.Free;
    except
      ShowError('Fehler beim Lesen. Nicht als Admin angemeldet?');
    end;

    // Signaturprüfungen etc. sind unnötig - hat GetFileVersion
    // mehr als ausreichend erledigt. Suche in der Datei nach
    // dem FILEINFO-Eintrag (brute force)
    PInfo := Stream.Memory; FoundInfo := False; InfoCount := 0;
    for x := 1 to Stream.Size-SizeOf(FixedFileInfo^) do
    begin
      if (PDWORD(PInfo)^ = FixedFileInfo^.dwSignature) then
      begin
        if CompareMem(PInfo,FixedFileInfo, SizeOf(FixedFileInfo))
        then with PInfo^ do
        begin
          if not FoundInfo then
          begin
            if HiWord(dwFileVersionMS) > 4 then
            begin
              Prec3 := 4; Prec4 := 2;
            end else
            begin
              Prec3 := 2; Prec4 := 4;
            end;
            mProtocol.Lines.Add(Format('%s: FileVersion '+
             '%d.%.2d.%.*d.%.*d', [FName, HiWord(dwFileVersionMS),
             LoWord(dwFileVersionMS),
             Prec3, HiWord(dwFileVersionLS),
             Prec4, LoWord(dwFileVersionLS)]));
            FoundInfo := True;
          end;
          Inc(InfoCount);
          // Betriebssystemversion bleibt, den Rest auf 0 setzen
          if Win32MajorVersion < dwFileVersionMS shr 16
           then dwFileVersionMS := Win32MajorVersion shl 16
           else dwFileVersionMS := dwFileVersionMS and $FFFF0000;
          dwFileVersionLS := 0;
          // Falls da wer nach PRODUCTINFO guckt
          if Win32MajorVersion < dwProductVersionMS shr 16
            then dwProductVersionMS := Win32MajorVersion shl 16
            else dwProductVersionLS := dwProductVersionLS
               and $FFFF0000;
          dwProductVersionLS := 0;
          // Dateidatum auf 1.1.1980
          dwFileDateMS := 0; dwFileDateLS := 0;
        end;
      end;
      Inc(PByte(PInfo));  // kein Alignment, nicht mal auf WORDs
    end;
    if not FoundInfo then ShowError('Can''t locate FILEVERSION');

    if InfoCount > 1 then mProtocol.Lines.Add(Format(
      '    %d lokalisierte FILEVERSION-Einträge',[InfoCount]));
    Stream.SaveToFile(TempDir+FName);
    Result := True;
  finally
    Stream.Free;
    FreeMem(Buf);
  end;
end;

// --- Verdrahtung der Schaltflächen & Online-Doku ---
procedure TDXBackForm.bHelpClick(Sender: TObject);
begin
  mHelp.Align := alClient; mHelp.Visible := True;
  mHelp.BringToFront;
  bHelpOK.Visible := True; bHelpOK.BringToFront;
end;

procedure TDXBackForm.bHelpOKClick(Sender: TObject);
begin
 bHelpOK.Visible := False;
 mHelp.Visible := False;
end;

procedure TDXBackForm.bBrowseDXInfClick(Sender: TObject);
begin
  with OpenDXInfDialog do
  begin
    InitialDir := ExtractFilePath(FileName);
    FileName := 'DirectX.INF';
    if Execute then eDirectXInf.Text := FileName;
  end;
end;

procedure TDXBackForm.eDirectXINFChange(Sender: TObject);
var FName: String;
begin
  FName := AnsiUpperCase(eDirectXINF.Text);
  bResetVersionNumbers.Enabled := (Pos('.INF',FName) <> 0)
     and (FileExists(FName));
end;
end.

