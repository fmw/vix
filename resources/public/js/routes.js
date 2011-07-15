var editView;
var indexView;

$(document).ready(function(){
    Vix.Routes = Backbone.Router.extend({
        routes: {
            "admin/:feed/edit/*id":      "edit",
            "admin/:feed":               "index",
            "admin/:feed/new":           "newDoc"
        },

        edit: function(feed, id) {
            var doc = new Document({id: id});
            doc.fetch({
                success: function(model, resp) {
                    $("button").unbind();
                    editView = new Vix.Views.Edit({model: doc});
                },
                
                error: function() {
                    new Error({message: "Document not found."});
                }
            });
        },

        index: function() {
            var documents = new Vix.Collections.Documents();
            documents.fetch({
                success: function() {
                    indexView = new Vix.Views.Index({collection: documents});
                },
                error: function() {
                    new Error({message: "Error loading documents"});
                }
            });
        },

        newDoc: function() {
            $("button").unbind();
            editView = new Vix.Views.Edit({model: new Document()});
        }
    });
});
