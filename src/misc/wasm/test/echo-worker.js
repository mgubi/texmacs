// A plugin of the browser for the tests of the worker links: a session
// which answers each line with "echo: " and the line (see workers.js)
var B = '\x02', E = '\x05', buf = '', decoder = new TextDecoder ();
postMessage ({ out: B + 'verbatim:Echo worker' + B + 'prompt#Echo] ' + E + E });
onmessage = function (e) {
  if (e.data.interrupt) { postMessage ({ err: B + 'utf8:interrupted' + E }); return; }
  buf += decoder.decode (e.data.input, { stream: true });
  var i;
  while ((i = buf.indexOf ('\n')) >= 0) {
    var line = buf.slice (0, i);
    buf = buf.slice (i + 1);
    postMessage ({ out: B + 'verbatim:echo: ' + line + B + 'prompt#Echo] ' + E + E });
  }
};
