
YAHOO.namespace("example.calendar");

YAHOO.example.calendar.init = function() {
    YAHOO.example.calendar.cal1 = new YAHOO.widget.CalendarGroup("cal1","cal1Container", {PAGES:2});
    YAHOO.example.calendar.cal1.render();
}
    
YAHOO.util.Event.onDOMReady(YAHOO.example.calendar.init);
