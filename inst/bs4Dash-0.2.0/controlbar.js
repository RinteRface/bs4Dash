$(function() {
  
  // required to show a toast when the controlbar is pinned 
  // for the first time
  var showToast = true;
  const controlbarToast = () => {
    $(document).Toasts('create', {
      title: 'Controlbar is pinned',
      close: false,
      autohide: true,
      delay: 2000
    });
    showToast = false;
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
    var isActionButton = $(e.target).hasClass("action-button shiny-bound-input");
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
  
  // if pin is TRUE at start we need to disable the controlbar toggle as soon
  // as it is opened. Only do this if pin data is present.
  $("#controlbar-toggle").one("click", function() {
    var pinned = $(".control-sidebar").attr("data-pin");
    if (pinned !== undefined && pinned !== "false") {
      setTimeout(function() {
        $("#controlbar-toggle").addClass("disabled");
        controlbarToast();
      }, 10); 
    }
  });
  
  // handle the case where the controlbar is already opened at start
  $(document).one("shiny:sessioninitialized", function() {
    var controlbarOpen = $("body").hasClass("control-sidebar-slide-open");
    var pinned = ($(".control-sidebar").attr("data-pin") === "true");
    if (controlbarOpen && pinned) {
      $("#controlbar-toggle").addClass("disabled");
      controlbarToast();
    }
  });
  
  
  // handle the pin button: toggle data-pin state
  $("#controlbarPin").on('click', function() {
    $(".control-sidebar").attr("data-pin",
       $(".control-sidebar").attr("data-pin") == "false" ? "true" : "false");
    // toggle right sidebar control depending on the datapin
    if ($(".control-sidebar").attr("data-pin") === "true") {
      $("#controlbar-toggle").addClass("disabled");
      if (showToast) {
        controlbarToast();
      }
    } else {
      $("#controlbar-toggle").removeClass("disabled");
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
      $(el).trigger("shown");
      return $("body").hasClass("control-sidebar-slide-open");
    },
  
    // see updatebs4Controlbar
    receiveMessage: function(el, data) {
      $("#controlbar-toggle").ControlSidebar('toggle');
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
  
  Shiny.inputBindings.register(controlbarBinding, "bs4Dash.controlbarBinding");
  
  
  // toggle controlbar at start
  var controlbarStartsOpen = $('body').attr('controlbar-start-open');
  if (controlbarStartsOpen === "true") {
    $('body').addClass('control-sidebar-slide-open');
  }

});