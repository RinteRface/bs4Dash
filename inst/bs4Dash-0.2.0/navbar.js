$(function() {
  // hide the right sidebar toggle 
  // if no right sidebar is specified
  noControlbar = ($(".control-sidebar").length === 0);
  if (noControlbar) {
    $(".nav-item > a[data-widget='control-sidebar']").hide();
  }
});