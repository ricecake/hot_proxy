;(function(){
'use strict';

$(document).ready(function(){
        var connection = new KnotConn({
		url: '/ws/',
		eventHandlers: {
			'#': function(key, content, raw) {
				console.log([key, content, raw]);
			}
		},
		onOpen: function() {
		}
	});

});

}());
