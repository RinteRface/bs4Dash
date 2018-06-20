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
  var $index = getIndex($selectedTab) - 1;
  $selectedTab.tab("show");

  console.log($tabs);
  console.log($selectedTab);
  console.log($index);
  $("#mymenu .nav-item:eq(" + $index + ") a").tab("show");
  //$("#mymenu .nav-item:eq(1) a").tab("show");
  //$('#mymenu li.nav-item:nth-child(2) a').tab('show');
  console.log($('#mymenu li.nav-item:eq(0) a'));
  //console.log($("#mymenu .nav-item:eq(" + $index + ") a:not([href='#'])"));
});