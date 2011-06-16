Document = Backbone.Model.extend({
    url: function() {
        if(this.isNew()) {
            return "/json/blog/new";
        } else {
            //handle missing slash
            if(_.first(this.id) === "/") {
                return "/json/document" + this.id;
            } else {
                return "/json/document/" + this.id;
            }
        }
    }
});
