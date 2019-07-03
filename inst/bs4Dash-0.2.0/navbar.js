$(function() {
  // hide the right sidebar toggle 
  // if no right sidebar is specified
  noControlbar = ($(".control-sidebar").length === 0);
  if (noControlbar) {
    $(".nav-item > a[data-widget='control-sidebar']").hide();
  }
  
  // hide the right sidebar toggle if the controlbar is disable
  disableControlbar = ($(".control-sidebar").css("display") == "none");
  if (disableControlbar) {
    $(".nav-item > a[data-widget='control-sidebar']").hide();
  }
  
  // when the sidebar is disabled, hide the sidebar toggle
  disableSidebar = ($(".main-sidebar").css("display") == "none");
  if (disableSidebar) {
    $(".nav-item > a[data-widget='pushmenu']").css("visibility", "hidden");
  }
  
});