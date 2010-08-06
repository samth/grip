
YAHOO.util.Event.onContentReady("mainnavmenu", function () {
	
	var oMenuBar = new YAHOO.widget.MenuBar("mainnavmenu", { 
		autosubmenudisplay: true, 
		hidedelay: 750, 
		lazyload: true });
	
	oMenuBar.render();
	
    });

