function handleMessage(message) {
    // TODO
}

function pageDidLoad() {
    $('#confirm').bind('click', runNaCl);
    $('#inputbox').bind('keypress', function(event) {
        if (event.keyCode == 13) { // Enter
            runNaCl();
        }
    });
}

function runNaCl() {
    var input = $('#inputbox').val();

    var output = $('#output');
    output.html("");

    // TODO

    var nacl = $('#nacl_module')[0];
    nacl.postMessage(input);
}
