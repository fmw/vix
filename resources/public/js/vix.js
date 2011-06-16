var editor;
var createSlug;
var incrementSlug;
var validateSlug;
var Vix;

Vix = {
    Views: {},
    Controllers: {},
    Collections: {},

    init: function() {
        $(document).ready(function() {
            new Vix.Controllers.Documents();
            Backbone.history.start();
        });
    }
};

incrementSlug = function(slug) {
    var m = slug.match(/(.*?)-([0-9]{1,})$/i);

    if(m) {
        return m[1] + "-" + (parseInt(m[2]) + 1);
    } else {
        return slug + "-2";
    }
};

invalidSlug = function(slug) {
    if(slug.match(/[^/\-a-z0-9]/i)) {
        return "Slugs can only contain '/', '-' and alphanumeric " +
                "characters.";
    } else if(slug.match(/[\-\/]{2,}/)) {
        return "Consecutive '-' or '/' characters are not allowed " +
                "in the slug.";
    } else {
        return false;
    }
};

//FIXME: remove hardcoded /blog/ prefix
createSlug = function(input) {
    if(typeof(input) === "string") {
        return "/blog/" + _.filter(input.split(/[^a-z0-9]/i),
                              function(s) {
                                  return (s.length === 0 ? false : true);
                              }).join("-");
    } else {
        return "";
    }
}

$(document).ready(function() {
    Vix.init();
});
