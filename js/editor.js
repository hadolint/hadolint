$(function() {
    var editor = CodeMirror.fromTextArea(document.getElementById('dockerfile__code'), {
      mode: 'text/x-dockerfile',
      lineNumbers: true,
      viewportMargin: Infinity,
    });
    var widgets = [];

    function updateHints(checks) {
      editor.operation(function() {
        $.each(widgets, function(_, widget) {
          editor.removeLineWidget(widget);
        });

        widgets = [];
        $.each(checks, function(_, check) {
          var inlineText = check.metadata.code + ': ' + check.metadata.message;
          var hint = $('<div class="lint-error"></div>').text(inlineText).get(0);
          var widget = editor.addLineWidget(check.linenumber - 1, hint, {
            coverGutter: false,
            noHScroll: true,
            above: true
          });
          widgets.push(widget);
        });
      });
    }
    $('#clear').click(function() {
        updateHints([]);
    });
    $('#lint').click(function() {
      var src = editor.getDoc().getValue() + '\n';
      $.post('http://api.hadolint.lukasmartinelli.ch/dockerfile', { dockerfile: src }, function(checks) {
          console.log(checks);
          updateHints(checks);
      });
    });
});
