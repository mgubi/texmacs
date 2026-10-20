; -- TeXmacs-Upsilon.iss --
; Inno Setup script of the installers of TeXmacs Upsilon (Qt and Vue), made
; from TeXmacs.iss (the installer of the stock TeXmacs) so that Upsilon
; installs beside it without touching it:
; - its own AppId, folder, group and uninstaller, one for each interface,
;   and its own icon (TeXmacs-Upsilon.ico, the flat chip of texmacs-vue-*.png);
; - no TEXMACS_PATH in the environment of the user (texmacs.exe uses the
;   tree it is in, see TeXmacs_init_paths in src/Texmacs/Texmacs/texmacs.cpp);
; - .tm files opened by Upsilon only when asked (a task, off by default),
;   with a type of file of its own (TeXmacsUpsilon.tm), so that its
;   uninstaller leaves the association of the stock TeXmacs alone.
;
;   iscc /DGui=Vue /DVersion=2.1.5 /DBundle=<TeXmacs-Windows> /DOutputDir=<dir>
;        /DOutputName=texmacs-upsilon-vue-x86_64-setup packages\windows\TeXmacs-Upsilon.iss
;
; Gui is Qt or Vue; Bundle is the folder made by make WINDOWS_BUNDLE.

#ifndef Gui
  #define Gui "Qt"
#endif
#ifndef Version
  #define Version "2.1.5"
#endif
#ifndef Bundle
  #define Bundle "..\..\..\distr\TeXmacs-Windows"
#endif
#ifndef OutputDir
  #define OutputDir "..\..\..\distr\windows"
#endif
#ifndef OutputName
  #define OutputName "texmacs-upsilon-setup"
#endif

#if Gui == "Vue"
  #define AppName "GNU TeXmacs Upsilon Vue"
  #define AppGuid "{{163A2D93-F063-409A-87C8-16BD85C9A623}"
#else
  #define AppName "GNU TeXmacs Upsilon"
  #define AppGuid "{{6298E18B-B111-48B7-BA8F-A89B3C450771}"
#endif
; (TeXmacsUpsilon.tm, TeXmacsUpsilonVue.tm: without the GNU of the name)
#define ProgId StringChange(StringChange(AppName, "GNU ", ""), " ", "") + ".tm"

[Setup]
AppId={#AppGuid}
AppName={#AppName}
AppVerName={#AppName} {#Version}
AppVersion={#Version}
VersionInfoTextVersion={#Version}
AppPublisher=TeXmacs Upsilon
AppPublisherURL=https://mgubi.github.io/texmacs/
AppSupportURL=https://github.com/mgubi/texmacs/issues
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
LicenseFile={#Bundle}\LICENSE
UninstallDisplayIcon={app}\TeXmacs-Upsilon.ico
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
SetupIconFile=TeXmacs-Upsilon.ico

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
; (only with the task: the type of file is Upsilon's, the stock one is tmfile)
Root: HKA; Subkey: "Software\Classes\.tm"; ValueType: string; ValueName: ""; ValueData: "{#ProgId}"; Flags: uninsdeletevalue; Tasks: associate
Root: HKA; Subkey: "Software\Classes\.tm\OpenWithProgids"; ValueType: string; ValueName: "{#ProgId}"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\{#ProgId}"; ValueType: string; ValueName: ""; ValueData: "TeXmacs document"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#ProgId}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\TeXmacs-Upsilon.ico"
Root: HKA; Subkey: "Software\Classes\{#ProgId}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\texmacs.exe"" ""%1"""

[Files]
Source: {#Bundle}\*; DestDir: {app}; Flags: recursesubdirs createallsubdirs ignoreversion
Source: TeXmacs-Upsilon.ico; DestDir: {app}
Source: topbanner.bmp; Flags: dontcopy

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\bin\texmacs.exe"; IconFilename: "{app}\TeXmacs-Upsilon.ico"
Name: "{group}\Uninstall {#AppName}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\bin\texmacs.exe"; IconFilename: "{app}\TeXmacs-Upsilon.ico"; Tasks: desktopicon

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
