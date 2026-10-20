; -- TeXmacs-Fixes.iss --
; Inno Setup script of the installer of TeXmacs Fixes (wip_fixes: the stock
; TeXmacs with fixes, Qt and Guile 1.8), made from TeXmacs-Upsilon.iss of
; TeXmacs Upsilon so that it installs beside the stock TeXmacs (and
; Upsilon) without touching it:
; - its own AppId, folder, group and uninstaller;
; - no TEXMACS_PATH in the environment (texmacs.exe uses the tree it is in,
;   .github/fixes/identity.patch);
; - .tm files opened by it only when asked (a task, off by default), with a
;   type of file of its own (TeXmacsFixes.tm), so that its uninstaller leaves
;   the association of the stock TeXmacs alone.
; It shares the home of the stock TeXmacs (%APPDATA%\TeXmacs).
;
;   iscc /DVersion=2.1.5 /DBundle=<TeXmacs-Windows> /DPkg=<src\packages\windows>
;        /DOutputDir=<dir> /DOutputName=texmacs-fixes-x86_64-setup .github\fixes\TeXmacs-Fixes.iss

#ifndef Version
  #define Version "2.1.5"
#endif
#ifndef Bundle
  #define Bundle "..\..\distr\TeXmacs-Windows"
#endif
#ifndef Pkg
  #define Pkg "..\..\src\packages\windows"
#endif
#ifndef OutputDir
  #define OutputDir "..\..\distr\windows"
#endif
#ifndef OutputName
  #define OutputName "texmacs-fixes-x86_64-setup"
#endif

#define AppName "TeXmacs Fixes"
#define AppGuid "{{63152257-FA24-4B9A-B933-C1884F1327C1}"
#define ProgId "TeXmacsFixes.tm"

[Setup]
AppId={#AppGuid}
AppName={#AppName}
AppVerName={#AppName} {#Version}
AppVersion={#Version}
VersionInfoTextVersion={#Version}
AppPublisher=TeXmacs Fixes
AppPublisherURL=https://mgubi.github.io/texmacs/fixes.html
AppSupportURL=https://github.com/mgubi/texmacs/issues
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
LicenseFile={#Bundle}\LICENSE
UninstallDisplayIcon={app}\TeXmacs.ico
UninstallDisplayName={#AppName}
OutputDir={#OutputDir}
OutputBaseFilename={#OutputName}
ChangesAssociations=yes
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=commandline dialog
CloseApplications=yes
ArchitecturesAllowed=x64os
ArchitecturesInstallIn64BitMode=x64os
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
SetupIconFile={#Pkg}\TeXmacs.ico

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "associate"; Description: "Open .tm files with {#AppName}"; GroupDescription: "Files:"; Flags: unchecked

[Registry]
; (only with the task: the type of file is TeXmacs Fixes', the stock one is tmfile)
Root: HKA; Subkey: "Software\Classes\.tm"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}"; Flags: uninsdeletevalue; Tasks: associate
Root: HKA; Subkey: "Software\Classes\.tm\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\{#ProgId}"; ValueType: string; ValueName: ""; ValueData: "TeXmacs document"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\TeXmacs.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\texmacs.exe"" ""%1"""

[Files]
Source: {#Bundle}\*; DestDir: {app}; Flags: recursesubdirs createallsubdirs ignoreversion
Source: {#Pkg}\TeXmacs.ico; DestDir: {app}
Source: {#Pkg}\topbanner.bmp; Flags: dontcopy

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\bin\texmacs.exe"; IconFilename: "{app}\TeXmacs.ico"
Name: "{group}\Uninstall {#AppName}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\bin\texmacs.exe"; IconFilename: "{app}\TeXmacs.ico"; Tasks: desktopicon

[UninstallDelete]
Type: files; Name: "{app}\*"
Type: dirifempty; Name: "{app}"

[Code]
procedure InitializeWizard();
var
  BitmapImage: TBitmapImage;
begin
  ExtractTemporaryFile('topbanner.bmp');
  BitmapImage := TBitmapImage.Create(WizardForm);
  BitmapImage.Parent := WizardForm.MainPanel;
  BitmapImage.Width := WizardForm.MainPanel.Width;
  BitmapImage.Height := WizardForm.MainPanel.Height;
  BitmapImage.Anchors := [akLeft, akTop, akRight, akBottom];
  BitmapImage.Stretch := True;
  BitmapImage.AutoSize := False;
  BitmapImage.Bitmap.LoadFromFile(ExpandConstant('{tmp}\topbanner.bmp'));

  WizardForm.WizardSmallBitmapImage.Visible := False;
  WizardForm.PageDescriptionLabel.Visible := False;
  WizardForm.PageNameLabel.Visible := False;
end;
