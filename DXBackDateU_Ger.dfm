?
 TDXBACKFORM 0\  TPF0TDXBackForm
DXBackFormLeft?Top? BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionDirectX BackdateClientHeight1ClientWidth?Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style OldCreateOrder	PositionpoDesktopCenterOnCreate
FormCreate	OnDestroyFormDestroyPixelsPerInchx
TextHeight TLabelLabel2LeftTopWidth?Height0Caption?   Setzt die Versionsnummern installierter DirectX-DLLs auf "DirectX 1" zurьck und macht damit komplette DirectX-Reinstallationen jeder Art (6 ьber 6, aber auch 5.0 ьber 5.2 oder 6) mцglich.WordWrap	  TMemomHelpLeftTop?WidthyHeight9Lines.StringsKurzanleitung=========== Windows 95 und Windows 980------------------------------------------------;1. Lokalisieren Sie die INF-Datei  des DirectX-Setups, das Sie nach dem 5   Zurьcksetzen der Versionsnummern ausfьhren wollen. Wenn Ihnen nur eine 7   Package (= selbstauspackende EXE-Datei) zur Verfьgung    steht: Fьhren Sie diese 3Package einmal aus (auch wenn das momentan sinnlos ist), lassen Sie die 5   abschlieЯende Meldung "DirectX installiert" auf dem Bildschirm stehen und suchen 5Sie mit dem Explorer das von dieser Package erzeugte TEMP-Unterverzeichnis 5(Standard: C:\Windows\Temp\lxp000.tmp). Kopieren Sie DirectX.INF aus diesem !Verzeichnis an einen anderen Ort. <   2. Klicken Sie auf "Modifikation der Dateien durchfьhren". Das Programm kopiert 9daraufhin alle Systemdateien, die bei der Reinstallation ersetzt werden sollen (lies: :in DirectX.INF verzeichnet sind) in das TEMP-Verzeichnis, 
setzt die 9   Versionsnummern in diesen Kopien auf "DirectX-1" zurьck und weist das System 4   an, die Originale beim nдchsten Neustart durch die modifizierten Kopien zu 	ersetzen. ;3. Starten Sie Windows neu. Das System sollte sich *exakt* so verhalten wie 7   zuvor, weil ausschlieЯlich die (nur vom DirectX-Setup    ьberprьften) &   Versionsnummern geдndert worden sind. ;   4. Starten Sie das gewьnschte DirectX-Setup. Dieses Setup findet nun "DirectX 1" 9   vor und fьhrt deshalb eine komplette Reinstallation aus. 
Windows NT--------------------8   1. Da es fьr Windows NT keine separaten DirectX-Setups gibt, verwendet das 7Programm hier eine fixe Dateiliste (DXForNT.INF). Wenn Sie abenteuerlustig sind, 5   kцnnen Sie es auch mit einer DirectX.INF-Datei fьr Windows 9x probieren #(beispielsweise aus dem 5.2er-SDK.) <   2. Klicken Sie auf "Modifikationn der Dateien durchfьhren"    (wie Schritt 2 fьr *   Windows 9x). Fьr Windows NT 5 werden die Versionsnummern nicht auf /4.00.00.0000, sondern auf 5.00.0000.00 gesetzt. 3. Starten Sie das System neu. :   4. Fьhren Sie das Service Pack 3 oder 4 aus. Wer an NT 5 herumgeschraubt hat, 5ohne die Originaldateien vorher zu sichern, hat eine 
komplette    "Drьberinstallation" 6   von der CD aus vor sich (Eselsmьtze nicht vergessen!)   *   Ich hab's mir anders ьberlegt. Was jetzt? ================================5Kein Problem - solange Sie das System noch nicht neu gestartet haben.-   Win9x: Lцschen Sie die Datei WININIT.INI im Windows-Verzeichnis. 8NT: Starten Sie RegEdt32 (nicht RegEdit) und setzen Sie 	den Wert .HKLM\SYSTEM\CurrentControlSet\Control\Session .Manager\PendingFileRenameOperations auf einen Leerstring (Alles    auswдhlen, dann <Entf>).)   Beide Windows-Versionen: Rдumen Sie das TEMP-Verzeichnis auf (optional) 
ScrollBars
ssVerticalTabOrder Visible  TButtonbHelpOKLeftpTop WidthQHeightCaptionOKTabOrderVisibleOnClickbHelpOKClick  	TGroupBox
GroupStep1LeftTop?Width?Height:Caption
 Schritt 1Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFontTabOrder TLabelLabel3Left? TopWidth? HeightCaption&Jetzt lesen oder hinterher jammern :-)Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFont  TButtonbHelpLeftTopWidth? HeightCaptionWeitere InformationenFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder OnClick
bHelpClick   	TGroupBox
GroupStep2LeftTop? Width?Height? Caption<    Schritt 2: Liste der zu modifizierenden Dateien auswдhlen Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFontTabOrder TLabelLabel1Left
Top"WidthqHeightCaption=   DirectX.INF des DX-Setups, das Sie nachher ausfьhren wollen:Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabel
lWinNTHackLeft
TopYWidth?Height Captionj   Programm lдuft unter NT und verwendet DXforNT.INF (solange Sie nicht explizit eine andere Datei wдhlen).WordWrap	  TEditeDirectXINFLeft
Top6Width?HeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder OnChangeeDirectXINFChange  TButtonbBrowseDXInfLeft?TopWidthQHeightCaption	Suchen...Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrderOnClickbBrowseDXInfClick   	TGroupBox
GroupStep3LeftTopWidth?Height!Caption*    Schritt 3: Versionsnummern zurьcksetzen Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFontTabOrder TMemo	mProtocolLeftTop;Width?Height? Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style Lines.Strings
Protokoll: 
ParentFont
ScrollBars
ssVerticalTabOrder   TButtonbResetVersionNumbersLeftTopWidthHeightCaption%   Modifikation der Dateien durchfьhrenFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrderOnClickbResetVersionNumbersClick  	TCheckBoxcRebootIfDoneLeft&TopWidth? HeightCaptionSystem danach neu startenFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrder   TOpenDialogOpenDXInfDialog
DefaultExtINFFileNameDirectX.INFFilter,Installation Information (*.inf)|DirectX.INFOptionsofHideReadOnlyofPathMustExistofFileMustExist Title<Locate Directory of DirectX Version to be installed later onLeftXTop?    