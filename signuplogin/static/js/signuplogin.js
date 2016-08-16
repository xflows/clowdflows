$(document).ajaxSend(function(event, xhr, settings) {
    function getCookie(name) {
        var cookieValue = null;
        if (document.cookie && document.cookie != '') {
            var cookies = document.cookie.split(';');
            for (var i = 0; i < cookies.length; i++) {
                var cookie = jQuery.trim(cookies[i]);
                // Does this cookie string begin with the name we want?
                if (cookie.substring(0, name.length + 1) == (name + '=')) {
                    cookieValue = decodeURIComponent(cookie.substring(name.length + 1));
                    break;
                }
            }
        }
        return cookieValue;
    }
    function sameOrigin(url) {
        // url could be relative or scheme relative or absolute
        var host = document.location.host; // host + port
        var protocol = document.location.protocol;
        var sr_origin = '//' + host;
        var origin = protocol + sr_origin;
        // Allow absolute or scheme relative URLs to same origin
        return (url == origin || url.slice(0, origin.length + 1) == origin + '/') ||
            (url == sr_origin || url.slice(0, sr_origin.length + 1) == sr_origin + '/') ||
            // or any other URL that isn't scheme relative or absolute i.e relative.
            !(/^(\/\/|http:|https:).*/.test(url));
    }
    function safeMethod(method) {
        return (/^(GET|HEAD|OPTIONS|TRACE)$/.test(method));
    }

    if (!safeMethod(settings.type) && sameOrigin(settings.url)) {
        xhr.setRequestHeader("X-CSRFToken", getCookie('csrftoken'));
    }
});

$(function() {
    $(".must_login").fancybox({ href:'/signuplogin/'
                              , padding:0
                              , onComplete: function(){
                                      loginListener();
                              }
                              , onClosed: function() {
                                    if ($.fancybox.logged_in) {
                                        if($.fancybox.proceed != "javascript:;")
                                        {
                                            window.location = $.fancybox.proceed;
                                        }
                                        else
                                        {
                                            if (window.location.href.indexOf("logout")!=-1) {
                                                window.location.href="/";
                                            } else {
                                                window.location.href=window.location.href;
                                            }
                                        }
                                    }
                                }
                              });

    $(".must_login").click(function() {
        // k kliknemo na en link za kerga rabmo bit loginani
        // si u $.fancybox.precoeed zapomnemo kam je user hotu it da ga loh
        // pol tja fuknemo k se logina/signupa
        $.fancybox.proceed = $(this).attr('href');
    });

    $(".logged_in").click(function() {
        window.location=$(this).attr('href');
    })

    function loginListener()
    {
            $("#login_form").submit(function(){
                    valid = true;

                    $.ajax({
                            type: "POST",
                            url: "/signuplogin/",
                            data: $("#login_form").serialize(),
                            success: function(html)
                            {
                                    if($.trim(html) == "OK")
                                    {
                                            $.fancybox.logged_in = true;
                                            $.fancybox.close();
                                    }
                                    else
                                    {
                                            $("#wrong_pass").show();
                                            $("[name=password]").val("");
                                            $.fancybox.resize();
                                    }
                            }
                    });
                    return false;
            });


            $("#register_form").submit(function(){
                    valid = true;

                    uname = $("[name=rusername]").val();
                    pass = $("[name=rpassword]").val();
                    rpass = $("[name=repeat_password]").val();
                    email = $("[name=email]").val();

                    if((uname == "") || (pass == "") || (rpass == "") || (email == ""))
                    {
                            valid = false;
                            $("#missing_error").show();
                            $.fancybox.resize();
                    }

                    if(pass != rpass)
                    {
                            valid = false;
                            $("#mismatch_error").show();
                            $.fancybox.resize();
                    }
                    else
                    {
                        $("#mismatch_error").hide();
                    }

                    if(valid)
                    {
                            $.ajax({
                                    type: "POST",
                                    url: "/signuplogin/",
                                    data: $("#register_form").serialize(),
                                    success: function(html)
                                    {
                                            if($.trim(html) == "OK")
                                            {
                                                    $.fancybox.logged_in = true;
                                                    $.fancybox.close();
                                            }
                                            else if($.trim(html) == "TAKEN")
                                            {
                                                    alert('Username already in use.');
                                                    $("#taken_error").show();
                                                    $.fancybox.resize();
                                            }
                                            else if($.trim(html) == "MAILTAKEN")
                                            {
                                                    alert('This email is already in use.');
                                                    $("#mailtaken_error").show();
                                                    $.fancybox.resize();
                                            }
                                            else
                                            {
                                                    alert("An unknown error has occured.");
                                                    $("#unknown_error").show();
                                                    $.fancybox.resize();
                                            }
                                    }
                            });
                    }
                    return false;
            });
    }
});