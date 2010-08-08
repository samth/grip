
var BRAVAIS = {};

YAHOO.util.Event.addListener (window, "load", function() {
	new function() {
	    var myColumnDefs = [
				//{key:"asin", label:"ASIN", sortable:false, resizable:true},
				{key:"title", label:"Title", sortable:true, resizeable:true},
				{key:"author", label:"Author", sortable:true, resizeable:true},
				{key:"rank", label:"Rank", formater:YAHOO.widget.DataTable.formatNumber, sortable:true, resizeable:true}
				];
	    this.myDataSource = new YAHOO.util.DataSource(KNOZAMA.results.bookorders);
	    this.myDataSource.responseType = YAHOO.util.DataSource.TYPE_JSARRAY;
	    this.myDataSource.responseSchema = {
		fields: ["title","author","rank"]
	    };
	    this.myDataTable = new YAHOO.widget.DataTable ( "basic",
							    myColumnDefs, 
							    this.myDataSource, 

							    {caption:""});
	    this.myDataTable.sortColumn(this.myDataTable.getColumn("rank"));
	    this.myDataTable.subscribe ('cellClickEvent', function (ev) {
		    var target = YAHOO.util.Event.getTarget(ev);
		    var record = this.getRecord(target);
		    //show(record.getData('id'),record.getData('data'),target);
		});
	};
    });


