const { Dahlia } = require('../dahlia/js/target/scala-2.13/dahlia-fastopt.js');
const ace = require('brace');
require('brace/mode/ocaml')
require('brace/mode/c_cpp')
require('brace/theme/monokai');

const Range = ace.acequire('ace/range').Range;
console.log(Range)

// input container
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

function compile() {
  const prog = editor.getValue();
  // Returns a tuple with either the result or an error.
  let [res, [pos, err]] = Dahlia.compileString(prog);

  let out;
  // If there is no result, report error message.
  if (res === "") {
    out = err;
    editor.session.addMarker(new Range(pos - 1, 0, pos, 2000), "warning", "fullLine")
  } else {
    out = res;
  }
  result.setValue(out);
  result.clearSelection();
}
window.compile = compile;
