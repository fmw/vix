var documentOverviewTemplate = '<h3>Documents:</h3>' +
    '<ul id="document-list">' +
    '<% _.each(documents, function(doc) { %>' +
        '<li><a rel="section" href="#edit' +
        '<%= doc.slug %>"><%= doc.title %></a> ' +
        '(<a rel="section" href="#" id="<%= doc.slug %>" ' +
        'class="delete-document">delete</a>)</li>' +
    '<% }); %>' +
    '<button id="add-document">New</button>';


$(document).ready(function(){
    Vix.Views.Index = Backbone.View.extend({
        el: $("#documents-overview"),
        
        events: {
            "click .delete-document": "deleteDocument",
            "click #add-document": "openNewEditor"
        },


        initialize: function() {
            this.render();
        },

        openNewEditor: function() {
            window.location.hash = "#new";
        },

        deleteDocument: function(ev) {
            var el = $(ev.target);
            var doc = new Document({id: el.attr("id")});

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
