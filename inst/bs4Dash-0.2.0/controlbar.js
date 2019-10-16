$(function() {
  

  // This prevent box content from going outside their container 
  // when the control-bar is on push mode
  $("#controlbar-toggle").on("click",
    function() {
      if ($("body").hasClass("control-sidebar-push-slide")) {
        $(window).trigger("resize"); 
      }
  });


  // Input binding
  var controlbarBinding = new Shiny.InputBinding();
  
  $.extend(controlbarBinding, {
  
    find: function(scope) {
      return $(scope).find(".control-sidebar");
    },
  
    // Given the DOM element for the input, return the value
    getValue: function(el) {
      return $("body").hasClass("control-sidebar-slide-open");
    },
  
    // see updatebs4Controlbar
    receiveMessage: function(el, data) {
      $("#controlbar-toggle").click();
    },
  
    subscribe: function(el, callback) {
      $("#controlbar-toggle").on("collapsed.lte.controlsidebar expanded.lte.controlsidebar", function(e) {
        // add a delay so that Shiny get the input value 
        // after the AdminLTE3 animation is finished!
        setTimeout(
          function() {
            callback();
          }, 10);
      });
    },
  
    unsubscribe: function(el) {
      $(el).off(".controlbarBinding");
    }
  });
  
  Shiny.inputBindings.register(controlbarBinding);

});