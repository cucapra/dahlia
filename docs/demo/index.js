const {seashell} = require('./seashell');
const ace = require('brace');
require('brace/mode/ocaml')
require('brace/mode/c_cpp')
require('brace/theme/monokai');

const editor = ace.edit("editor");
editor.session.setMode("ace/mode/ocaml");
editor.session.setUseWrapMode("ace/mode/c_cpp");

// Output container
const result = ace.edit("result");
result.session.setMode("ace/mode/c_cpp");
result.session.setUseWrapMode("ace/mode/c_cpp");
result.setOptions({
  readOnly: true,
  highlightActiveLine: false,
  highlightGutterLine: false
});

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
