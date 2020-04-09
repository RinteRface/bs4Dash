$(function () {
  
  // Below some functions are taken from shinydashboard and modified to 
  // work with the new structure of adminLTE3
  
  
  
  // This function handles a special case in the AdminLTE sidebar: when there
  // is a sidebar-menu with items, and one of those items has sub-items, and
  // they are used for tab navigation. Normally, if one of the items is
  // selected and then a sub-item is clicked, both the item and sub-item will
  // retain the "active" class, so they will both be highlighted. This happens
  // because they're not designed to be used together for tab panels. This
  // code ensures that only one item will have the "active" class.
  var deactivateOtherTabs = function() {
    // Find all tab links under sidebar-menu even if they don't have a
    // tabName (which is why the second selector is necessary)
    var $tablinks = $("#sidebar-menu a[data-toggle='tab']," +
      "#sidebar-menu li.has-treeview > a");

    // If any other items are active, deactivate them
    $tablinks.not($(this)).removeClass("active");
    
    // also manually activate the parent link when the selected item
    // is part of a treeview. For some reason, this is not done by AdminLTE3...
    if ($(this).hasClass('treeview-link')) {
      $(this).parents('.has-treeview').children().eq(0).addClass('active');
    }
    
    // Trigger event for the tabItemInputBinding
    var $obj = $('.sidebarMenuSelectedTabItem');
    var inputBinding = $obj.data('shiny-input-binding');
    if (typeof inputBinding !== 'undefined') {
      inputBinding.setValue($obj, $(this).attr('data-value'));
      $obj.trigger('change');
    }
  };

  $(document).on('shown.bs.tab', '#sidebar-menu a[data-toggle="tab"]', deactivateOtherTabs);
  
  
  // When document is ready, if there is a sidebar menu with no activated tabs,
  // activate the one specified by `data-start-selected`, or if that's not
  // present, the first one.
  var ensureActivatedTab = function() {
      // get the selected tabs
    var $tablinks = $("#sidebar-menu a[data-toggle='tab']");
    
    // If there are no tabs, $startTab.length will be 0.
    var $startTab = $tablinks.filter("[data-start-selected='1']");
    if ($startTab.length === 0) {
      // If no tab starts selected, use the first one, if present
      $startTab = $tablinks.first();
    } 
    
    // If there's a `data-start-selected` attribute and we can find a tab with
    // that name, activate it.
    if ($startTab.length !== 0) {
      $startTab.tab("show");

      // This is indirectly setting the value of the Shiny input by setting
      // an attribute on the html element it is bound to. We cannot use the
      // inputBinding's setValue() method here because this is called too
      // early (before Shiny has fully initialized)
      $(".sidebarMenuSelectedTabItem").attr("data-value",
        $startTab.attr("data-value"));
    }
  };

  ensureActivatedTab();
  
  
  // Whenever we expand a menuItem (to be expandable, it must have children),
  // update the value for the expandedItem's input binding (this is the
  // tabName of the fist subMenuItem inside the menuItem that is currently
  // expanded)
  $(document).on("click", ".has-treeview", function() {
    var $menu = $(this);
    console.log($menu.hasClass('menu-open'));
    // If this menuItem was already open, then clicking on it again,
    // should trigger the "hidden" event, so Shiny doesn't worry about
    // it while it's hidden (and vice versa).
    if ($menu.hasClass("menu-open")) $menu.trigger("collapsed.lte.treeview");
    else if ($menu.hasClass("has-treeview")) $menu.trigger("expanded.lte.treeview");
  
    // need to set timeout to account for the slideUp/slideDown animation
    var $obj = $('sidebar.shiny-bound-input');
    setTimeout(function() { $obj.trigger('change'); }, 600);
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
      var anchors = $(el).parent('#sidebar-menu').find('li:not(.treeview)').children('a');
      anchors.each(function() { // eslint-disable-line consistent-return
        if (self._getTabName($(this)) === value) {
          $(this).tab('show');
          // this make sure that treeview items are open when we
          // use the updatebs4TabItems function on the server side
          if ($(this).hasClass('treeview-link')) {
            if (!$(this).parents('.has-treeview').hasClass('menu-open')) {
              $(this).parents('.has-treeview').children().eq(0).trigger('click');
            }
          }
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

  Shiny.inputBindings.register(tabItemInputBinding, 'bs4Dash.tabItemInput');
  
  
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
  
  Shiny.inputBindings.register(sidebarBinding, 'bs4Dash.sidebarInput');
  
  // does not work
  //$(document).on('collapsed.lte.pushmenu', function(e) {
  //  console.log('plop');
  //  console.log(e);
  //  $(window).trigger("resize");
  //});
  
  // sidebarmenuExpandedInputBinding
  // ------------------------------------------------------------------
  // This keeps tracks of what menuItem (if any) is expanded
  var sidebarmenuExpandedInputBinding = new Shiny.InputBinding();
  $.extend(sidebarmenuExpandedInputBinding, {
    find: function(scope) {
      // This will also have id="sidebarItemExpanded"
      return $(scope).find('.sidebar');
    },
    getValue: function(el) {
      var $open = $(el)
        .find('li')
        .filter('.menu-open')
        .find('ul');
      if ($open.length === 1) return $open.attr('data-expanded');
      else return null;
    },
    setValue: function(el, value) {
      // does not work (nothing is printed)
      var $menuItem = $(el).find("[data-expanded='" + value + "']");
      // This will trigger actions defined by AdminLTE, as well as actions
      // defined in sidebar.js.
      $menuItem.prev().trigger("click");
    },
    subscribe: function(el, callback) {
      $(el).on('change.sidebarmenuExpandedInputBinding', function() {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.sidebarmenuExpandedInputBinding');
    }
  });
  Shiny.inputBindings.register(sidebarmenuExpandedInputBinding,
  'bs4Dash.sidebarmenuExpandedInputBinding');
  
  
  
  
  // toggle sidebar at start depending on the body class
  var sidebarStartsOpen = $('body').attr('sidebar-start-open');
  if (sidebarStartsOpen === "false") {
    $('body').addClass('sidebar-collapse');
  }
  
  // handle fixed sidebar
  if ($(".main-sidebar").attr("data-fixed") === "true") {
    $("body").addClass("layout-fixed");
  }
  
});