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
});