
(function() {
    var Dom = YAHOO.util.Dom,
        Event = YAHOO.util.Event,
        status = null;
    
    var handleSuccess = function(o) {
	var json = o.responseText.substring(o.responseText.indexOf('{'), o.responseText.lastIndexOf('}') + 1);
	status.innerHTML = 'Status: Received OK <br> Thank you for your feedback.' + '<br>' + (new Date().toString());
	myEditor.setEditorHTML('Thank you.');
    }
    var handleFailure = function(o) {
	var json = o.responseText.substring(o.responseText.indexOf('{'), o.responseText.lastIndexOf('}') + 1);
	status.innerHTML = 'Status: Failed <br> Sorry a problem occured.  Please try again later.';
    }
    
    var callback = {
	success: handleSuccess,
	failure: handleFailure
    };

    var _button = new YAHOO.widget.Button('submitEditor');
    
    var myConfig = {
        height: '300px',
        width:  '530px',
        animate: true,
        dompath: true
    };
    
    var myEditor = new YAHOO.widget.Editor('editor', myConfig);
    myEditor.render();

    _button.on('click', function(ev) {
        Event.stopEvent(ev);
        myEditor.saveHTML();
        window.setTimeout(function() {
            var sUrl = "/feedback";
	    var author = document.getElementById('feedback-author');
	    var email  = document.getElementById('feedback-email');
            var data = 'feedback=' + encodeURIComponent(myEditor.get('textarea').value)
		+ '&author=' + encodeURIComponent(author.value)
		+ '&email='  + encodeURIComponent(email.value);	    
            var request = YAHOO.util.Connect.asyncRequest('POST', sUrl, callback, data);
        }, 200);
    });


    Event.onDOMReady(function() {
        status = Dom.get('status');
    });
})();

