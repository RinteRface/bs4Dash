$(function() {
  
  // required to show a toast when the controlbar is pinned 
  // for the first time. Show once since it may be annoying ...
  var showToast = true;
  const controlbarToast = () => {
    if (showToast) {
      $(document).Toasts('create', {
        title: 'Controlbar is pinned',
        close: false,
        autohide: true,
        delay: 2000
      });
      showToast = false; 
    }
  };

  // This prevent box content from going outside their container 
  // when the control-bar is on push mode
  $("#controlbar-toggle").on("click",
    function() {
      if ($("body").hasClass("control-sidebar-push-slide")) {
        $(window).trigger("resize"); 
      }
  });
  
  
  // The code below hande the click out of the right control bar
  $(window).click(function(e) { 
    // There is a potential conflict. This function detect any click outside
    // the controlbar and close if if it is not pinned. Yet, if we click on an action       // button controlling the controlbar state (see updatebs4Controlbar), it is also outside the controlbar so the toggle event will be triggered twice. The controlbar will never close as shown in https://github.com/RinteRface/bs4Dash/issues/110. Below we make sure to leave the function as soon as a click on a button holding the class action button. This is not really a fix but a reasonable workaround.
    var isActionButton = $(e.target).hasClass("action-button");
    if (isActionButton) return null;
      
    if($("aside.control-sidebar").find(e.target).length === 0) {
      var pinned = $(".control-sidebar").attr("data-pin");
      if (pinned === "false" || pinned === undefined) {
        $("body").removeClass("control-sidebar-slide-open");  
        // don't forget to refresh the input binding
        $("#controlbar-toggle").trigger('collapsed.lte.controlsidebar');
      }
    }  
  });
  
  // handle the pin button: toggle data-pin state
  $("#controlbarPin").on('click', function() {
    var $pinIcon = $(this).children();
    $pinIcon.toggleClass("fa-rotate-90 fa-lg");
    
    $(".control-sidebar").attr("data-pin",
       $(".control-sidebar").attr("data-pin") == "false" ? "true" : "false");
    // toggle right sidebar control depending on the datapin
    if ($(".control-sidebar").attr("data-pin") === "true") {
      $pinIcon.css("color", "#007bff");
      $("#controlbar-toggle").addClass("disabled");
      controlbarToast();
    } else {
      $("#controlbar-toggle").removeClass("disabled");
      $pinIcon.css("color", "");
    }
  });


var init = true;

  // Input binding
  var controlbarBinding = new Shiny.InputBinding();
  
  $.extend(controlbarBinding, {
  
    find: function(scope) {
      return $(scope).find(".control-sidebar");
    },
  
    // Given the DOM element for the input, return the value
    getValue: function(el) {
      // Handles the pin 
      var controlbarOpen = $("body").hasClass("control-sidebar-slide-open");
      var pinned = $(el).attr("data-pin") === "true";
      if (controlbarOpen && pinned && init) {
        $("#controlbar-toggle").addClass("disabled");
        $("#controlbarPin")
          .children()
          .css("color", "#007bff");
        controlbarToast();
        init = false;
      }
      
      // this handles the case where the controlbar is not collapsed at start
      var controlbarCollapsed = $(el).attr('data-collapsed');
      if (controlbarCollapsed === "false") {
        $("#controlbar-toggle").ControlSidebar('toggle');
        $(el).attr('data-collapsed', "true");
        return true;
      } else {
        return $("body").hasClass("control-sidebar-slide-open");
      }
    },
    // see updatebs4Controlbar
    receiveMessage: function(el, data) {
      $("#controlbar-toggle").ControlSidebar('toggle');
    },
  
    subscribe: function(el, callback) {
      $("#controlbar-toggle").on("collapsed.lte.controlsidebar expanded.lte.controlsidebar", function(e) {
        $(el).trigger('shown');
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
  
  Shiny.inputBindings.register(controlbarBinding, "bs4Dash.controlbarBinding");
  
  // handle controlbar overlay
  var controlbarOverlay = $('.control-sidebar').attr('data-overlay');
  if (controlbarOverlay === "false") {
    $('body').addClass('control-sidebar-push-slide');
  }

});
