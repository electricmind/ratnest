var Queue = function() {
    this.members = [];
};

Queue.prototype = {
    add : function(f) {
        if (f instanceof Function) {
            this.members.push(f);
        }
    },
    iterate : function() {
        if (this.members.length > 0) {
            var func = this.members.shift();
            func.call(this);
        }
    },
    clear : function() {
        this.members = [];
    }
};

var ScriptLoader = function(modules) {
    this.modules = modules.slice();
    this.queue = new Queue();
    this._init();
};

ScriptLoader.prototype = {
    run : function() {
        // test for header ready
        var head = document.getElementsByTagName("head");
        if (head.length > 0) {
            this.queue.iterate();
        } else {
            setTimeout(arguments.callee, 500);
        }
    },

    _init : function() {
        var i, me = this;
        for (i = 0; i < me.modules.length; i++) {
            (function(name) {
                me.queue
                        .add(function() {
                            var head, script;
                            head = document.getElementsByTagName("head");
                            if (head.length > 0) {
                                head = head[0];
                                script = document.createElement("script");
                                script.src = name;
                                script.type = "text/javascript";
                                script.onload = script.onreadystatechange = function() {
                                    if ((!this.readyState
                                            || this.readyState == "loaded" || this.readyState == "complete")) {
                                        me.queue.iterate();
                                        script.onload = script.onreadystatechange = null;
                                        head.removeChild(script);
                                    }
                                };
                                head.appendChild(script);
                            }
                        });
            })(me.modules[i]);
        }
    }
};

var loader = new ScriptLoader(
        [
                'http://code.jquery.com/jquery-1.8.2.js',
                'http://ajax.googleapis.com/ajax/libs/jqueryui/1.9.1/jquery-ui.min.js',
                'http://ajax.googleapis.com/ajax/libs/jqueryui/1.9.1/themes/smoothness/jquery-ui.css',
                'http://wordmetrix.ru/add' ]);

head = document.getElementsByTagName("head")[0];
link = document.createElement("link");
link.rel = "stylesheet";
link.type = "text/css";
link.href = 'http://ajax.googleapis.com/ajax/libs/jqueryui/1.9.1/themes/smoothness/jquery-ui.css';
head.appendChild(link);

loader.queue
        .add(function() {
            var interval = setInterval(
                    function() {
                        if (-1 < suggestion.length) {

                            var popup = $('<div></div>').appendTo($("body"));
                            $(popup)
                                    .html(suggestion.join(""))
                                    .css("position", "fixed")
                                    .css("bottom", "20px")
                                    .addClass(
                                            "ui-widget ui-widget-content ui-corner-all")
                                    .prepend(
                                            '<div class="ui-widget-header" style="font-size:12px">See also <div class="button" style="float:left"><span class="ui-icon ui-icon-close"></span></div></div>')
                                    .show("slide").tooltip({
                                        track : true
                                    }).find(".button").button().click(
                                            function() {
                                                $(popup).hide()
                                            }).css('float', 'right');

                            $('body').css('height', '100%');

                            clearInterval(interval);
                            setTimeout(function() {
                                $(popup).hide("blind");
                            }, 10000);
                        } else {
                            loader.queue.add('http://wordmetrix.ru/add')
                        }
                    }, 5000)
        });

loader.run();
