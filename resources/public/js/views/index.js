var documentOverviewTemplate = '<h3>Documents:</h3>' +
    '<ul id="document-list">' +
    '<% _.each(documents, function(doc) { %>' +
        '<li><a rel="section" class="edit-link" href="/admin/' + 
        '<%= getFeed(location.href) %>/edit' +
        '<%= doc.slug %>"><%= doc.title %></a> ' +
        '(<a rel="section" href="#" id="<%= doc.slug %>" ' +
        'class="delete-document">delete</a>)</li>' +
    '<% }); %>' +
    '<button id="add-document">New</button>';


$(document).ready(function(){
    Vix.Views.Index = Backbone.View.extend({
        el: $("#main-page"),
        
        events: {        
            "click #add-document": "openNewEditor",
            "click .edit-link": "editDocument",
            "click .delete-document": "deleteDocument"
        },

        initialize: function() {
            this.render();
        },

        openNewEditor: function() {
            Vix.Routes.navigate("admin/blog/new", true);
        },

        editDocument: function(event) {
            event.preventDefault();

            if(event.target.pathname.substring(0, 1) === "/") {
                Vix.Routes.navigate(event.target.pathname.substring(1), true);
            } else {
                Vix.Routes.navigate(event.target.pathname, true);
            }
        },

        deleteDocument: function(event) {
            var el = $(event.target);
            var doc = new Document({id: el.attr("id")});

            event.preventDefault();

            doc.destroy({
                success: function(model, response) {
                    new Vix.Views.Notice({message: "Document deleted"});
                    $(el).parent().remove();
                },
            
                error: function() {
                    new Vix.Views.Error(
                        {message: "Server error: couldn't delete document."});
                }
            });
        },

        render: function() {
            $(this.el).empty();
            $(this.el).html(_.template(documentOverviewTemplate,
                        {documents: this.collection.toJSON()}));
        }
    });
});
