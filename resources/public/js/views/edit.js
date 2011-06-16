var documentFormTemplate = '<p>' +
        '<a href="#">Back to overview</a>' +
    '</p>' +

    '<div class="document-fields-row">' +
        '<label for="title">Title:</label>' +
        '<input type="text" id="title" name="title"' +
            'class="input-field" value="<%= model.get("title") %>"/>' +

        '<input type="checkbox" id="draft" name="draft"' +
            'class="checkbox-field"' +
            //'<%' +
            //    'if(model.get("draft")) {' +
            //        'print("checked");' +
            //    '}' +
            //'%>' +
            '/>' +
        '<label for="draft">Draft</label>' +
    '</div>' +

    '<div class="document-fields-row">' +
        '<label for="slug" id="slug-label">Slug:</label>' +
        '<input type="text" id="slug" name="slug"' +
            'class="input-field" value="<%= model.get("slug") %>" />' +
        
        '<input type="checkbox" id="custom-slug"' +
            'name="custom-slug" class="checkbox-field" />' +
        '<label for="custom-slug" id="custom-slug-label">' +
            'Custom' +
        '</label>' +
    '</div>' +

    '<textarea cols="50" rows="10" name="content"' +
        'id="content"><%= model.get("content") %></textarea>' +
    '<button id="save">Save</button>';

$(document).ready(function(){
    Vix.Views.Edit = Backbone.View.extend({

        el: $("#document-form"),

        events: {
            "change input#title": "updateSlug",
            "change input#custom-slug": "updateSlug",
            "keyup input#slug": "handleSlugErrors",
            "click #save": "save"
        },

        initialize: function() {
            _.bindAll(this, "render");

            //this.model.bind("change", this.render);
            this.render();
        },

        save: function() {
            var msg = this.model.isNew() ? "Successfully created!" : "Saved!";

            this.model.save({
                title: this.$("input#title").val(),
                slug: this.$("input#slug").val(),
                content: this.editor.saveHTML(),
                draft: this.$("input#draft").is(":checked")
            }, {
                success: function(model, resp) {
                    new Vix.Views.Notice({message: msg});
                    window.location.hash = "#edit" + model.get("slug");
                },
                error: function() {
                    new Vix.Views.Error();
                }
            });
        },

        clearSlugError: function() {
            this.$("input#slug").removeClass("error");

            if(this.statusMessage) {
                this.statusMessage.fold(false);
                this.statusMessage = null;
            }
        },


        checkSlug: function(autoUpdate) {
            //autoUpdate: if true, slugs are appended with a numeric
            //postfix; otherwise an error is shown (for custom slugs)

            if(!invalidSlug(this.$("input#slug").val())) {
                var _this = this;
                var doc = new Document({id: this.$("input#slug").val()});

                doc.fetch({
                    success: function(model, resp) {
                        //FIXME: make less noisy
                        //(currently executing 5 times per change)
                        //console.log(autoUpdate);
                        var slugEl = $("input#slug");
                        if(resp) {
                            if(autoUpdate) {
                                slugEl.val(incrementSlug(slugEl.val()));
                                _this.checkSlug(autoUpdate);
                            } else {
                                _this.statusMessage = new Vix.Views.Error(
                                    {message: "This slug is already in use."});
                                slugEl.addClass("error");
                            }
                        } else if(!autoUpdate) {
                            _this.clearSlugError();
                        }
                    }
                });
            }
        },
        
        disableSlug: function() {
            this.$("input#slug").attr("disabled", "disabled");
            this.$("input#slug").addClass("disabled");
            this.$("#slug-label").addClass("disabled");
        },
        
        enableSlug: function() {
            this.$("input#slug").removeAttr("disabled");
            this.$("input#slug").removeClass("disabled");
            this.$("#slug-label").removeClass("disabled");
        },

        updateSlug: function() {
            if(this.model.isNew()) {
                if(this.$("input#custom-slug").is(":checked")) {
                    this.enableSlug();
                }
                else {
                    this.$("input#slug").val(
                        createSlug(this.$("input#title").val()));
                    
                    this.disableSlug();
                    this.clearSlugError();                    
                    this.checkSlug(true);
                }
            }
        },
        
        handleSlugErrors: function(event) {
            var slugError;

            if(event.keyCode !== 16) {
                slugError = invalidSlug(this.$("input#slug").val());
                
                if(slugError) {
                    if(!this.statusMessage ||
                            this.statusMessage.options.message != slugError){
                        this.statusMessage = new Vix.Views.Error(
                                {message: slugError});
                        this.$("input#slug").addClass("error");
                    }
                } else {
                    this.checkSlug(false);
                    this.clearSlugError();                    
                }
            }
        },

        createEditor: function() {
            this.editor = new YAHOO.widget.Editor("content", {
                height: "300px",
                width: "560px",
                dompath: true,
                animate: true,
                toolbar: {
                    titlebar: "Content",
                    buttons: [
                        { group: "textstyle", label: "Font Style",
                            buttons: [
                                { type: "push", label:
                                    "Bold CTRL + SHIFT + B",
                                    value: "bold"},
                                { type: "push", label:
                                    "Italic CTRL + SHIFT + I",
                                    value: "italic"},
                                { type: "push", label:
                                    "Underline CTRL + SHIFT + U",
                                    value: "underline"},
                                //{ type: "push", label: "Font Color",
                                //    value: "forecolor" },
                                //{ type: "push", label: "Background Color",
                                //    value: "backcolor" }
                            ]
                        },
                        { type: "separator" },
                        { group: 'parastyle', label: 'Paragraph Style',
                            buttons: [
                                { type: 'select',
                                    label: 'Normal', value: 'heading',
                                    menu: [
                                        { text: 'Normal',
                                            value: 'none', checked: true },
                                        { text: 'Header 1', value: 'h1' },
                                        { text: 'Header 2', value: 'h2' },
                                        { text: 'Header 3', value: 'h3' },
                                        { text: 'Header 4', value: 'h4' },
                                        { text: 'Header 5', value: 'h5' },
                                        { text: 'Header 6', value: 'h6' }
                                    ]
                                }
                            ]
                        },
                        { type: "separator" },
                        { group: "indentlist", label: "Lists",
                            buttons: [
                                { type: 'push',
                                    label: 'Create an Unordered List',
                                    value: 'insertunorderedlist' }, 
                                { type: 'push',
                                    label: 'Create an Ordered List',
                                    value: 'insertorderedlist' }
                            ]
                        },
                        { type: "separator" },
                        { group: 'insertitem', label: 'Insert Item',
                            buttons: [
                                { type: 'push',
                                    label: 'HTML Link CTRL + SHIFT + L',
                                    value: 'createlink', disabled: true },
                                { type: 'push',
                                    label: 'Insert Image',
                                    value: 'insertimage' } 
                            ]
                        }
                    ]
                }
            });
        },


        render: function() {
            $(this.el).empty();
            $(this.el).html(_.template(documentFormTemplate,
                        {model: this.model}));

            $("iframe#_yuiResizeMonitor").remove();

            if(this.editor) {
                try {
                    this.editor.destroy();
                } catch(e) {
                }
                this.editor = null;
            }

            this.createEditor();
            this.editor.render();

            if(this.model.isNew()) {
                this.$("input#slug").val("/blog");
                this.disableSlug();
            } else {
                // can only edit slugs for new items
                this.$("input#custom-slug").attr("disabled", "disabled");
                this.$("#custom-slug-label").addClass("disabled");
                this.$("input#slug").attr("disabled", "disabled");
                this.$("input#slug").addClass("disabled");
                this.$("#slug-label").addClass("disabled");
            }

            this.delegateEvents();
        }
    });
});
