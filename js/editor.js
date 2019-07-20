(function() {
  window.editor = CodeMirror.fromTextArea(document.getElementById('dockerfile__code'), {
    mode: 'text/x-dockerfile',
    lineNumbers: true,
    viewportMargin: Infinity,
  });

  document.getElementById('clear').onclick = function () {
    updateHints([]);
  }
})();

let widgets = []
let lintButton = document.getElementById('lint')

function showLoading() {
  let loading = document.getElementById('loading')
  loading.style.display = 'inline';
}

function hideLoading() {
  let loading = document.getElementById('loading')
  loading.style.display = 'none';
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
      msg.innerText = check.message;

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
        var inline = msg;
      }

      var hint = document.createElement('div');
      hint.className = "lint-error";
      hint.append(inline);

      var linenumber = check.line - 1;
      if (linenumber < 0) {
        linenumber = 0;
      }

      widgets.push(editor.addLineWidget(linenumber, hint, {
        coverGutter: false,
        noHScroll: true,
        above: true
      }));
    }
  });
}
