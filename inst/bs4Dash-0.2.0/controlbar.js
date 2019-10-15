// Input binding
var controlbarBinding = new Shiny.InputBinding();

$.extend(controlbarBinding, {

  find: function(scope) {
    return $(scope).find(".control-sidebar");
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    var open = $("body").hasClass("control-sidebar-slide-open");
    return open;
  },

  // see updateF7Tabs
  receiveMessage: function(el, data) {
    $("#controlbar-toggle").click();
  },

  subscribe: function(el, callback) {
    $("#controlbar-toggle").on("expanded.lte.controlsidebar collapsed.lte.controlsidebar", function(e) {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off(".controlbarBinding");
  }
});

Shiny.inputBindings.register(controlbarBinding);