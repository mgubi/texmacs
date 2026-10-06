// --pre-js of the node build (tests of the browser build without a
// browser): the environment of node is the environment of TeXmacs, so that
// TEXMACS_PATH and TEXMACS_HOME_PATH can point into the host file system,
// which NODERAWFS exposes as it is
Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function () {
  for (var k in process.env) ENV[k] = process.env[k];
});
