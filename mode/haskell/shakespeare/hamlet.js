// While Hamlet is different from HTML, in practice it seems to work fine with
// the stock HTML mode.
CodeMirror.defineMode("hamlet", function(config) {
  return CodeMirror.shakespeare(config, "text/html");
});
CodeMirror.defineMIME("text/hamlet", "hamlet");

