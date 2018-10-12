const {seashell} = require('./seashell');
const ace = require('brace');
const URLSearchParams = require('url-search-params');
require('brace/mode/ocaml')
require('brace/mode/c_cpp')
require('brace/theme/monokai');

const urlParams = new URLSearchParams(window.location.search);

// input container
const editor = ace.edit("editor");
editor.session.setMode("ace/mode/ocaml");
editor.session.setUseWrapMode("ace/mode/c_cpp");
if (urlParams.has('prog')) {
  editor.setValue(decodeURIComponent(urlParams.get('prog')));
}

// Output container
const result = ace.edit("result");
result.session.setMode("ace/mode/c_cpp");
result.session.setUseWrapMode("ace/mode/c_cpp");
result.setOptions({
  readOnly: true,
  highlightActiveLine: false,
  highlightGutterLine: false
});

function getStaticLink() {
  urlParams.set('prog', encodeURIComponent(editor.getValue()));
  window.location.search = urlParams.toString();
}
window.getStaticLink = getStaticLink;

function compileSS() {
  const prog = editor.getValue();
  let out;
  try {
    out = seashell.compile(prog);
  } catch (e) {
    out = e.toString().split(',').slice(4).join(", ");
  }
  result.setValue(out);
  result.clearSelection();
}
window.compileSS = compileSS;
