function search () {
	if (searchBox.value) {
		corewidgets.hidden = true;
	} else{
		corewidgets.hidden = false;
	};
}

jQuery.expr[':'].Contains = function(a, i, m) {
    return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0;
};

$(function() {
    $('#searchBox').on('keyup', function() {
        var w = $(this).val();
        if (w) {
            $('#corewidgets li').hide();
            $('#corewidgets li:Contains('+w+')').show();
        } else {
            $('#corewidgets li').show();                  
        }
    });
});​