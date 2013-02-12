







jQuery.expr[':'].Contains = function(a, i, m) {
    return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0;
};

$(function() {
    $('#searchBox').on('keyup', function() {
        var w = $(this).val();
        if (w) {
            $('#corewidgets li').hide();
            $('#corewidgets .folder:Contains('+w+')').parent().show().find('li').show();
            $('#corewidgets li:Contains('+w+')').show();
        } else {
            $('#corewidgets li').show();
        }
    });

});​
