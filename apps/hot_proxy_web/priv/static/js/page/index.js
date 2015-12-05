;(function(){
'use strict';

$(document).ready(function(){
        var connection = new KnotConn({
		url: '/ws/',
		eventHandlers: {
			'#': function(key, content, raw) {
				//console.log([key, content, raw]);
				//$('.debug').text(JSON.stringify(raw, undefined, 2));
				$('.debug').html(syntaxHighlight(raw));
			}
		},
		onOpen: function() {
		}
	});

});

}());

function syntaxHighlight(json) {
    if (typeof json != 'string') {
         json = JSON.stringify(json, undefined, 2);
    }
    json = json.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\n/g, '<br/>');
    return json.replace(/("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)/g, function (match) {
        var cls = 'number';
        if (/^"/.test(match)) {
            if (/:$/.test(match)) {
                cls = 'key';
            } else {
                cls = 'string';
            }
        } else if (/true|false/.test(match)) {
            cls = 'boolean';
        } else if (/null/.test(match)) {
            cls = 'null';
        }
        return '<span class="' + cls + '">' + match + '</span>';
    });
}
