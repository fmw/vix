$(document).ready(function(){
    Vix.Views.Notice = Backbone.View.extend({
        el: $("#status-message"),

        displayLength: 5000,
        defaultMessage: "",

        initialize: function() {
            _.bindAll(this, "render");
            
            this.message = this.options.message || this.defaultMessage;
            this.render();
        },

        fold: function(delay) {
            var _this = this;

            if(delay) {
                $.doTimeout(this.displayLength, function() {
                    $(_this.el).slideUp();
                    $.doTimeout(2000, function() {
                        $(_this).remove();
                    });
                });
            } else {
                $(this.el).slideUp();
                $(this).remove();
            }
        },

        render: function() {
            $(this.el).text(this.message);
            $(this.el).removeClass("error");

            $(this.el).hide();
            $(this.el).slideDown();

            this.fold(true);
        }
    });

    Vix.Views.Error = Vix.Views.Notice.extend({
        defaultMessage: "Something went wrong. Please try again.",
        
        render: function() {
            $(this.el).text(this.message);
            $(this.el).addClass("error");

            $(this.el).hide();
            $(this.el).slideDown();
        }
    });
});
