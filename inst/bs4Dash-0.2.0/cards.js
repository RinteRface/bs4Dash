// Input binding
var cardBinding = new Shiny.InputBinding();

$.extend(cardBinding, {
  
  find: function(scope) {
    return $(scope).find(".card");
  },
  
  // Given the DOM element for the input, return the value
  getValue: function(el) {
    var collapsed = $(el).hasClass("collapsed-card");
    var maximized = $(el).hasClass("maximized-card");
    var display = $(el).css('display');
    
    var visible;
    if (display === "none") {
      visible = false;
    } else {
      visible = true;
    }
    
    // toggle collapse button when maximized
    if (maximized) {
      $(el).find("[data-card-widget = 'collapse']").hide();
    } else {
      $(el).find("[data-card-widget = 'collapse']").show();
    }
    return {collapsed: collapsed, maximized: maximized, visible: visible};
  },
  
  // see updatebs4Card
  receiveMessage: function(el, data) {
    if (data != "restore") {
      if ($(el).css('display') != 'none') {
        $(el).CardWidget(data);  
      }
    } else {
        $(el).show();
        // this is needed so that the last event handler is considered
        // in the subscribe method. 
        $(el).trigger("shown");
    }
  },
  
  subscribe: function(el, callback) {
    $(el).on('expanded.lte.cardwidget collapsed.lte.cardwidget', function(e) {
      // set a delay so that SHiny get the input value when the collapse animation
      // is finished. 
      setTimeout(
        function() {
          callback();
        }, 500);
    });
    
    $(el).on('maximized.lte.cardwidget minimized.lte.cardwidget', function(e) {
      callback();
    });
    
    $(el).on('removed.lte.cardwidget', function(e) {
      setTimeout(
        function() {
          callback();
        }, 500);
    });
    // we need to split removed and shown event since shown is immediate whereas close
    // takes some time
    $(el).on('shown', function(e) {
      callback();
    });
  },
  
  unsubscribe: function(el) {
    $(el).off(".cardBinding");
  }
});

Shiny.inputBindings.register(cardBinding);



// Card sidebar input binding
var cardSidebarBinding = new Shiny.InputBinding();
$.extend(cardSidebarBinding, {
  
  find: function(scope) {
    return $(scope).find('[data-widget="chat-pane-toggle"]');
  },
  
  // Given the DOM element for the input, return the value
  getValue: function(el) {
    var cardWrapper = $(el).closest(".card");
    return $(cardWrapper).hasClass("direct-chat-contacts-open");
  },
  
  // see updatebs4Card
  receiveMessage: function(el, data) {
    // In theory, adminLTE3 has a builtin function
    // we could use $(el).DirectChat('toggle');
    // However, it does not update the related input.
    // The toggled.lte.directchat event seems to be broken.
    $(el).trigger('click');
    $(el).trigger("shown");
  },
  
  subscribe: function(el, callback) {
    $(el).on('click', function(e) {
      // set a delay so that Shiny get the input value when the collapse animation
      // is finished. 
      setTimeout(
        function() {
          callback();
        }, 10);
    });
  },
  
  unsubscribe: function(el) {
    $(el).off(".cardSidebarBinding");
  }
});

Shiny.inputBindings.register(cardSidebarBinding);
