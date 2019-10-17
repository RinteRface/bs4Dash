$(function() {
  // hide the right sidebar toggle 
  // if no right sidebar is specified
  noControlbar = ($(".control-sidebar").length === 0);
  if (noControlbar) {
    $("#controlbar-toggle").hide();
  }
  
  // hide the right sidebar toggle if the controlbar is disable
  disableControlbar = ($(".control-sidebar").attr("data-show"));
  if (!disableControlbar) {
    $("#controlbar-toggle").hide();
  }
  
  // controlbar slide
  controlbarSlide = ($(".control-sidebar").attr("data-slide"));
  if (controlbarSlide) {
    $("#controlbar-toggle").attr('data-controlsidebar-slide', controlbarSlide);
  }
  
  // when the sidebar is disabled, hide the sidebar toggle
  disableSidebar = ($(".main-sidebar").css("display") == "none");
  if (disableSidebar) {
    $(".nav-item > a[data-widget='pushmenu']").css("visibility", "hidden");
  }
  
});