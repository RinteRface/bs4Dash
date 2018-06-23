$(function () {

  // function to get the index of an element in a list
  function getIndex(elem) {
    var $t = $(elem);
    return $t.parent().index();
  }
  // select all nav items links that have not the has-treeview class
  //var $tabs = $( " #mymenu li.nav-item:not(.has-treeview) a" );
  var $tabs = $("#mymenu a[data-toggle='tab']");
  var $selectedTab = $tabs.filter(".active.show");
  var $selectedTabLink = $selectedTab.attr("id");
  var $index = getIndex($selectedTab);
  var $indexPane = $index - 1;
  
  if ($selectedTab.length === 0) {
    // If no tab starts selected, use the first one, if present
    $('#mymenu .nav-item:eq(0) a').tab('show');
    $('.tab-pane:eq(0)').addClass('active show');
  } //else {
    //$("#mymenu .nav-item:eq(" + $indexPane + ") a").tab("show");
    //$(".tab-pane:eq(" + $indexPane + ")").addClass('active show');
  //}
});