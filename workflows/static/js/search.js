jQuery.expr[':'].Contains = function (a, i, m) {
    return jQuery(a).text().toUpperCase().indexOf(m[3].toUpperCase()) >= 0;
};

$(function () {
    $('#searchBox').on('keyup', function () {
        var w = $(this).val();
        if (w) {
            $('#widgetsTree li').hide();
            $('#widgetsTree .folder:Contains(' + w + ')').parent().show().find('li').show();
            $('#widgetsTree li:Contains(' + w + ')').show();

            $('#widgetsTree').find("div[class*=expandable-hitarea]").click();
        } else {
            $('#widgetsTree li').show();
            
            var collapsables = $('#widgetsTree').find("div[class*=collapsable-hitarea]");
            for (var i = collapsables.length - 1; i >= 0; i--) {
                collapsables[i].click();
            };
        }
    });
});