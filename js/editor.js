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
          var msg = $('<span class="lint-message"></span>').text(check.metadata.message);
          var inline = $('<a class="lint-link" href="' + check.link + '">' + check.metadata.code + ' </a>').append(msg);
          var hint = $('<div class="lint-error"></div>').html(inline).get(0);
          var linenumber = check.linenumber - 1;
          if (linenumber < 0) {
                linenumber = 0;
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
