$(document)
        .ready(
                function() {
                    setTimeout(
                            function() {
                                $(".advise")
                                        .html(suggestion.join(""))
                                        .css("position", "fixed")
                                        .css("bottom", "20px")
                                        .addClass(
                                                "ui-widget ui-widget-content ui-corner-all")
                                        .prepend(
                                                '<div class="ui-widget-header">See also<button><span class="ui-icon ui-icon-close"></span></button></div>')
                                        .show("slide").tooltip({
                                            track : true
                                        }).find("button").click(function() {
                                            $(this).closest(".advise").hide()
                                        }).css('float', 'right');

                                $('body').css('height', '100%');
                                setTimeout(function() {
                                    $(".advise").hide("blind");
                                }, 10000);
                            }, 2000)
                })

