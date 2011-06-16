$(document).ready(function(){
    Vix.Controllers.Documents = Backbone.Controller.extend({
        routes: {
            "edit/*id": "edit",
            "":         "index",
            "new":      "newDoc"
        },

        edit: function(id) {
            $("#document-form").show();
            $("#documents-overview").hide();

            var doc = new Document({id: id});
            doc.fetch({
                success: function(model, resp) {
                    new Vix.Views.Edit({model: doc});
                },
                
                error: function() {
                    new Error({message: "Document not found."});
                    window.location.hash = "#";
                }
            });
        },

        index: function() {
           $("#document-form").hide();
           $("#documents-overview").show();

           var documents = new Vix.Collections.Documents();
           documents.fetch({
               success: function() {
                   new Vix.Views.Index({collection: documents});
               },
               error: function() {
                   new Error({message: "Error loading documents"});
               }
           });
        },

        newDoc: function() {
            $("#document-form").show();
            $("#documents-overview").hide();

            new Vix.Views.Edit({model: new Document()});
        }
    });
});
