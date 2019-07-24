$(function() {
  
  // recover all tabSet ids in an array
  // The idea is that we will add each respective 
  // id to the Shiny.addCustomMessageHandler function
  // which first argument is the type and should be the id
  // of the targeted tabSet
  var tabIds = [];
  getAllTabSetIds = function() {
    $('.content-wrapper ul').each(function() {
      tabIds.push(this.id);
    });  
  };
  
  // call the function ...
  getAllTabSetIds();
  
  // As mentioned previously, we create a customMessageHandler
  // for each tabSet. The unique id will allow for multiple
  // update call at the same time.
  tabIds.forEach(function(index) {
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
  
  
  // handle the left sidebar tabs if needed
  var sidebarIds = [];
  getAllSidebarIds = function() {
    $('.sidebarMenuSelectedTabItem').each(function() {
      sidebarIds.push(this.id);
    });  
  };
  
  // call the function ...
  getAllSidebarIds();
  
  // As mentioned previously, we create a customMessageHandler
  // for the sidebar. We handle the case of multiple menus.
  sidebarIds.forEach(function(index) {
    Shiny.addCustomMessageHandler(index, function(message) {
      var sidebarSiblings = $('#' + index).siblings().find('a');
      var selectedIdx = message.value - 1;
      var selectedTab = $(sidebarSiblings[selectedIdx]).attr('id');
      // trigger a click on the corresponding the tab button. This will enable the body content
      // to be shown. Otherwise, shiny does not know if the element needs to be
      // rendered...
      $('a[href="#shiny-' + selectedTab +'"]').trigger('click');
      $('#shiny-' + selectedTab).addClass('active show');
      
      // remove active class from ul menu + the body content (tab content)
      $('a[href!="#shiny-' + selectedTab +'"]').removeClass('active');
      $('div[id="shiny-' + index + '"]').siblings().removeClass('active show');
    });
  });
});