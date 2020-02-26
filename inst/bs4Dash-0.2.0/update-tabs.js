$(function() {
  
  // Input binding
  var tabSetPanelBinding = new Shiny.InputBinding();
  $.extend(tabSetPanelBinding, {
    
    initialize: function(el) {
      var activeTab = $(el).find(".active");
      // if no tab is selected, select the first tab by default
      if (activeTab.length === 0) {
        $('#' + $(el).attr('id') + ' ' + 'li:first-child a').tab('show');
      }
    },
  
    find: function(scope) {
      return $(scope).find(".tabsetpanel");
    },
  
    // Given the DOM element for the input, return the value
    getValue: function(el) {
      var activeTab = $(el).find(".active");
      return $(activeTab).html(); 
    },
    
    setValue: function(el, value) {
      // tabs are prefixed by their container id
      // this is to handle multiple tabs and shiny modules
      var parent = $(el).attr('id');
      // clean white space
      value = value.replace(/\s/g, '');
      $('#' + parent + ' ' + 'a[href="#' + parent + '-' + value + '"]').tab('show');
    },
  
    // see updatebs4TabSetPanel
    receiveMessage: function(el, data) {
      // update the active tab
      if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
      }
    },
  
    subscribe: function(el, callback) {
      // important to use shown.bs.tab and not show.bs.tab!
      $(el).on("shown.bs.tab.tabSetPanelBinding", function(e) {
        callback();
      });
    },
  
    unsubscribe: function(el) {
      $(el).off(".tabSetPanelBinding");
    }
  });
  Shiny.inputBindings.register(tabSetPanelBinding);
  
  // recover all tabSet ids in an array
  // The idea is that we will add each respective 
  // id to the Shiny.addCustomMessageHandler function
  // which first argument is the type and should be the id
  // of the targeted tabSet
  var tabIds = [];
  getAllTabSetIds = function() {
    $('.content-wrapper .tabsetpanel').each(function() {
      tabIds.push(this.id);
    });  
  };
  
  // call the function ...
  getAllTabSetIds();
  
  
  // handles the insertTab function
  tabIds.forEach(function(index) {
    var id = "insert_" + index;
    Shiny.addCustomMessageHandler(id, function(message) {
      
      // define div and li targets
      var $divTag = $(message.value.html);
      var $liTag = $(message.link.html);
      
      var tabId = message.ns + "-" + message.target;
      if (message.position === "after") {
        // insert after the targeted tag in the tab-panel div
        $divTag.insertAfter($("#" + tabId));
        // we also need to insert an item in the navigation
        $liTag.insertAfter($('[href ="#' + tabId + '"]').parent());
      } else if (message.position === "before") {
        // insert before the targeted tag in the tab-panel div
        $divTag.insertBefore($("#" + tabId));
        // we also need to insert an item in the navigation
        $liTag.insertBefore($('[href ="#' + tabId + '"]').parent());
      }
      
      // needed to render input/output in newly added tab. It takes the possible
      // deps and add them to the tag. Indeed, if we insert a tab, its deps are not
      // included in the page so it can't render properly
      Shiny.renderContent($liTag[0], {html: $liTag.html(), deps: message.link.deps});
      Shiny.renderContent($divTag[0], {html: $divTag.html(), deps: message.value.deps});
      
      // if the newly inserted tab is active, disable other tabs
      if (message.select === "true") {
        // trigger a click on corresponding the new tab button. 
        $('#' + index + ' a[href="#' +  message.id +'"]').tab('show');
      }
    });
  });
  
  
  // handles the removeTab function
  tabIds.forEach(function(index) {
    var id = "remove_" + index;
    Shiny.addCustomMessageHandler(id, function(message) {
      var tabId = message.ns + "-" + message.target;
      $('#' + index + ' a[href="#' +  tabId +'"]').parent().remove();
      $('#' + tabId).remove();
      
      // if only one element remains, select it by default
      var items = $('#' + index + ' li');
      if (items.length === 1) $(items).find('a').click();
    });
  });
  
  
  // handle the right controlbar 
  var controlbarIds = [];
  getAllControlbarIds = function() {
    $('.control-sidebar ul').each(function() {
      controlbarIds.push(this.id);
    });  
  };
  
  getAllControlbarIds();
  
  
  // As mentioned previously, we create a customMessageHandler
  // for each tabSet. The unique id will allow for multiple
  // update call at the same time.
  controlbarIds.forEach(function(index) {
    Shiny.addCustomMessageHandler(index, function(message) {
      var selectedIdx = message.value;
      var selectedTab = index + '-Tab' + selectedIdx;
      
      // trigger a click on corresponding the tab button. This will enable the body content
      // to be shown. Otherwise, shiny does not know if the element needs to be
      // rendered...
      $('#' + index + ' a[href="#' + selectedTab +'"]').trigger('click');
      $('#' + selectedTab).addClass('active show');
      
      // remove active class from ul menu + the body content (tab content)
      $('#' + index + ' a[href!="#' + selectedTab +'"]').removeClass('active');
      $('div[id="' + selectedTab + '"]').siblings().removeClass('active show');
    });
  });
  
});