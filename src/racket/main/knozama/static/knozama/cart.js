var Cart;

if (!Cart) Cart = {};

Cart = {};

Cart.emptyCartButton = new YAHOO.widget.Button ("push-button-empty-cart");

Cart.div = document.getElementById ('cartmain');

Cart.handleSuccess = function(o) {
    
    if (o.responseText !== undefined) {
	Cart.div.innerHTML = "<p class=\"cartheader\" id=\"cartmain\">Your cart is sadly empty.</p>";
    }
}
    
Cart.handleFailure = function(o) {    
    Cart.div.innerHTML = "<p class=\"cartheader\" id=\"cartmain\">An Error Occured.  Please try again.</p>";
}
	
Cart.callback = {
    success: Cart.handleSuccess,
    failure: Cart.handleFailure
};

Cart.sUrl = "cart";

Cart.makeRequest = function () {
    var request = YAHOO.util.Connect.asyncRequest('DELETE', Cart.sUrl, Cart.callback);
}
    
Cart.emptyCartButtonClick = function (e) {
    this.blur();    
    Cart.makeRequest();
}

Cart.emptyCartButton.addListener("click", Cart.emptyCartButtonClick);

// focus and blur to force alignment.
(function () {
    Cart.emptyCartButton.focus();
    Cart.emptyCartButton.blur();
})();
