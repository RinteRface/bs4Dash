$(function () {
  
  // This code makes sure that each time
  // a tabItem is clicked, we set the value
  // of the input binding
  var setInputBindingValue = function() {
    // Trigger event for the tabItemInputBinding
    var $obj = $('.sidebarMenuSelectedTabItem');
    var inputBinding = $obj.data('shiny-input-binding');
    if (typeof inputBinding !== 'undefined') {
      inputBinding.setValue($obj, $(this).attr('data-value'));
      $obj.trigger('change');
    }
  };

  $(document).on('shown.bs.tab', '#mymenu a[data-toggle="tab"]', setInputBindingValue);
  
  
  // When document is ready, if there is a sidebar menu with no activated tabs,
  // activate the one specified by `data-start-selected`, or if that's not
  // present, the first one.
  var ensureActivatedTab = function() {
      // get the selected tabs
    var tabs = $("#mymenu a[data-toggle='tab']");
    var selectedTab = tabs.filter(".active.show");
    
    if (selectedTab.length === 0) {
      // If no tab starts selected, use the first one, if present
      $(tabs[0]).tab('show');
      $('.container-fluid.tab-pane:eq(0)').addClass('active show');
      // This is indirectly setting the value of the Shiny input by setting
      // an attribute on the html element it is bound to. We cannot use the
      // inputBinding's setValue() method here because this is called too
      // early (before Shiny has fully initialized)
      $('.sidebarMenuSelectedTabItem').attr('data-value',
        $(tabs[0]).attr('data-value'));
    
    
    
    } 
  };

  ensureActivatedTab();
  
  // select the first sidebar item by default
  var firsSidebarItem = $("#mymenu").children[0];
  if (firsSidebarItem != "undefined") {
    $(firsSidebarItem).addClass("active"); 
  }
  
  // when click on treeview, collapse all other treeviews
  $(".has-treeview").on('click', function() {
    
    // take care of all other treeviews 
    var navItems = $(this).siblings();
    var res;
    var ul;
    for (i = 0; i < navItems.length; i++) {
      // we go through all siblings
      // but we are only interested in the other
      // treeviews. If found, we remove the menu-open class
      // from the li wrapper and set display none to the ul children.
      // ul is the second children of the li wrapper, hence index 1.
      if ($(navItems[i]).hasClass("has-treeview")) {
        res = navItems[i];
        ul = res.children[1];
        $(res).removeClass("menu-open");
        $(ul).css("display", "none");
      }
    }
  });
  
  // Select treeview link on click
  // the treeview li element cannot be active itself thus
  // we take the related link a.
  $(".has-treeview > a").on("click", function() {
    var treeview = $(".has-treeview > a");
    var activeTreeviews = $(".has-treeview > a.active");
    // set all other nav-links to inactive
    $("#mymenu .nav-link").removeClass("active");
    // set all other treeviews to inactive
    // if there are more than 1 treeview
    if (treeview.length > 1) {
      $(activeTreeviews).removeClass("active");
    }
    $(this).addClass("active");
    
    // when click on a treeview, select its first element
    var treeviewFirstChild = $(this).siblings()[0].children[0].children[0];
    $(treeviewFirstChild).addClass("active");
    // trigger a click so that the corresponding tabPanel is shown
    // By default, setting the active class do not provide any click event
    $(treeviewFirstChild).click();
  });
  
  // when click on a navitem, deselect all other navitems
  $("#mymenu a[data-toggle='tab']").on("click", function() {
    $("#mymenu .nav-link").removeClass("active");
    
    
    // collapse all treeviews elements when click on a link
    // that is not a treeview
    var treeviews =  $(".has-treeview");
    if (!$(this).parents().hasClass("has-treeview")) {
      $(treeviews).removeClass("menu-open");
      for (i = 0; i < treeviews.length; i++) {
        $(treeviews[i].children[1]).css("display", "none");
      }
    }
    
    // if click on an item part of a treeview, select the treeview parent
    // by default
    var treeViewLi = $(this).parents()[2];
    var treeViewLiLink = treeViewLi.children[0];
    $(treeViewLiLink).addClass("active");
  });
  
  
  //---------------------------------------------------------------------
  // tabItemInputBinding
  // ------------------------------------------------------------------
  // Based on Shiny.tabItemInputBinding, but customized for tabItems in
  // bs4Dash, which have a slightly different structure.
  var tabItemInputBinding = new Shiny.InputBinding();
    $.extend(tabItemInputBinding, {
    find: function(scope) {
      return $(scope).find('.sidebarMenuSelectedTabItem');
    },
    getValue: function(el) {
      var value = $(el).attr('data-value');
      if (value === "null") return null;
      return value;
    },
    setValue: function(el, value) {
      var self = this;
      var anchors = $(el).parent('#mymenu').find('li:not(.treeview)').children('a');
      anchors.each(function() { // eslint-disable-line consistent-return
        if (self._getTabName($(this)) === value) {
          //$(this).tab('show');
          $(el).attr('data-value', self._getTabName($(this)));
          return false;
        }
      });
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);
    },
    subscribe: function(el, callback) {
      // This event is triggered by deactivateOtherTabs, which is triggered by
      // shown. The deactivation of other tabs must occur before Shiny gets the
      // input value.
      $(el).on('change.tabItemInputBinding', function() {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.tabItemInputBinding');
    },
    _getTabName: function(anchor) {
      return anchor.attr('data-value');
    }
  });

  Shiny.inputBindings.register(tabItemInputBinding);
  
  
  //---------------------------------------------------------------------
  // sidebarInputBinding
  // ------------------------------------------------------------------
  // similar to controlbarInputBinding
  var sidebarBinding = new Shiny.InputBinding();
  
  $.extend(sidebarBinding, {
  
    find: function(scope) {
      return $(scope).find(".main-sidebar");
    },
  
    // Given the DOM element for the input, return the value
    getValue: function(el) {
      // Warning: we can't look for sidebar-open since this
      // class is only generated on mobile devices
      return !$("body").hasClass("sidebar-collapse");
    },
  
    // see updatebs4Controlbar
    receiveMessage: function(el, data) {
      $("[data-widget='pushmenu']").click();
    },
  
    subscribe: function(el, callback) {
      $("[data-widget='pushmenu']").on("collapsed.lte.pushmenu shown.lte.pushmenu", function(e) {
        callback();
      });
    },
  
    unsubscribe: function(el) {
      $(el).off(".sidebarBinding");
    }
  });
  
  Shiny.inputBindings.register(sidebarBinding);
  
  
  // toggle sidebar at start depending on the body class
  var sidebarStartsOpen = $('body').attr('sidebar-start-open');
  if (sidebarStartsOpen === "false") {
    $('body').addClass('sidebar-collapse');
  }
  
});