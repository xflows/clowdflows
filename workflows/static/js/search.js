jQuery.expr[':'].Contains = function (a, i, m) {
    return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0;
};

$(function () {
    $('#searchBox').on('keyup', function () {
        var w = $(this).val();
        if (w) {
            $('#widgets li').hide();
            $('#widgets .folder:Contains(' + w + ')').parent().show().find('li').show();
            $('#widgets li:Contains(' + w + ')').show();
        } else {
            $('#widgets li').show();
        }
    });
});