// --pre-js of the core of Tau for the page (misc/tau/Makefile, "web"): the
// environment of TeXmacs in the file system of the program. The files of
// TeXmacs are brought by misc/wasm/packages.js, under /texmacs.
Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function () {
  ENV['TEXMACS_PATH'] = '/texmacs';
  ENV['HOME'] = '/home/web_user';
  ENV['TEXMACS_HOME_PATH'] = '/home/web_user/.TeXmacs';
  ENV['LANG'] = 'en_US.UTF-8';
  // the system of the user, for the shortcuts of TeXmacs: Cmd+... on a Mac
  // (default_look_and_feel in src/Kernel/Abstractions/basic.cpp)
  var platform = (typeof navigator === 'undefined') ? '' :
    (navigator.userAgentData && navigator.userAgentData.platform) || navigator.platform || '';
  ENV['TEXMACS_WEB_PLATFORM'] = /mac|iphone|ipad/i.test (platform) ? 'macos' :
                                /win/i.test (platform) ? 'windows' : 'other';
});
