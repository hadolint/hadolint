import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

let widgets = []

const wasi = new WASI({
    stdout: (out) => {
      let checks = JSON.parse(out);
      updateHints(checks);
    }
});

const jsffiExports = {};
const wasm = await WebAssembly.instantiateStreaming(
    fetch('/hadolint/hadolint.wasm'),
    Object.assign(
        { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
        wasi.getImportObject()
    )
);
Object.assign(jsffiExports, wasm.instance.exports);

wasi.initialize(wasm, {
    ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports)
});

wasi.instance.exports.setup();

(function() {
  var delay = setTimeout(lint, 350);;

  window.editor = CodeMirror.fromTextArea(document.getElementById('dockerfile__code'), {
    mode: 'text/x-dockerfile',
    lineNumbers: true,
    viewportMargin: Infinity,
  });

  window.editor.on("change", function() {
    window.editor.save()
    clearTimeout(delay);
    delay = setTimeout(lint, 350);
  });

  document.getElementById('lint').onclick = function () {
    lint();
  }

  document.getElementById('clear').onclick = function () {
    clear();
  }
})();

function lint() {
  wasi.instance.exports.lint();
}

function clear() {
  editor.operation(function () {
    for (var i = 0; i < widgets.length; i++) {
      editor.removeLineWidget(widgets[i]);
    }
  });
}

function updateHints(checks) {
  editor.operation(function () {
    for (var i = 0; i < widgets.length; i++) {
      editor.removeLineWidget(widgets[i]);
    }

    widgets = [];
    for (var i = 0; i < checks.length; i++) {
      var check = checks[i];

      var msg = document.createElement("span");
      msg.className = "lint-message";
      msg.innerText = check.code + " " + check.level + ": " + check.message;

      if (check.code != 'DL1000') {
        var link = 'https://github.com/hadolint/hadolint/wiki/'
        if (check.code.startsWith('SC')) {
          link = 'https://github.com/koalaman/shellcheck/wiki/';
        }

        var inline = document.createElement('a')
        inline.href = link + check.code;
        inline.className = "lint-link";
        inline.append(msg);
      } else {
        var inline = check.level + ": " + check.message;
      }

      var hint = document.createElement('div');
      hint.className = "lint-" + check.level;
      hint.append(inline);

      var linenumber = Math.max(0, check.line - 1);

      widgets.push(editor.addLineWidget(linenumber, hint, {
        coverGutter: false,
        noHScroll: true,
        above: true
      }));
    }
  });
}
