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
          var hint = $('<div class="lint-error"></div>').text(check.message).get(0);
          var linenumber = 0;
          if(check.linenumber) {
            linenumber = check.linenumber - 1;
          }
          var widget = editor.addLineWidget(linenumber, hint, {
            coverGutter: false,
            noHScroll: true,
            above: true
          });
          widgets.push(widget);
        });
      });
    }
    $('#lint').click(function() {
      var src = editor.getDoc().getValue();
      $.post('http://hadolint.apps.lukasmartinelli.ch/dockerfile', { dockerfile: src }, function(checks) {
          console.log(checks);
          updateHints(checks);
      });
    });

    window.setTimeout(function() {
      $('#lint').click();
    }, 1000);
});
