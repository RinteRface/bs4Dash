$(function () {
  
  // select the first treeview by default
  var firstTriview = $(".has-treeview > a")[0];
  if (firstTriview != "undefined") {
   $(firstTriview).addClass("active"); 
  }
  
  // when click on treeview, collapse all other treeviews
  $(".has-treeview").on('click', function() {
    var navItems = $(this).siblings();
    var res;
    var ul;
    for (i = 0; i < navItems.length; i++) {
      // we go through all siblings
      // but we are only interested in the other
      // treeviews. If found, we remove the menu-open class
      // from the li wrapper and set display none to the ul children.
      // ul is the second children of the li wrapper.
      if ($(navItems[i]).hasClass("has-treeview")) {
        res = navItems[i];
        ul = res.children[1];
        $(res).removeClass("menu-open");
        $(ul).css("display", "none");
      }
    }
    
    //$(".has-treeview").removeClass("menu-open");
    //$(".has-treeview > ul").css("display", "none");
    //$(this).addClass("menu-open");
    //$(this).siblings()[0].children[0].css("display", "block");
  });
  
  // Select treeview link on click
  $(".has-treeview > a").on("click", function() {
    var activeTreeviews = $(".has-treeview > a.active");
    // set all other nav-links to inactive
    $("#mymenu .nav-link").removeClass("active");
    // set all other treeviews to inactive
    $(activeTreeviews).removeClass("active");
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
    var treeViewLi = $(this).parents()[2];
    var treeViewLiLink = treeViewLi.children[0];
    $(treeViewLiLink).addClass("active");
  });
});