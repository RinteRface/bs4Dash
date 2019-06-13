$(function () {
  
  // select the first sidebar item by default
  var firsSidebarItem = $("#mymenu").children[0];
  if (firsSidebarItem != "undefined") {
   $(firsSidebarItem).addClass("active"); 
  }
  
  // when click on treeview, collapse all other treeviews
  $(".has-treeview").on('click', function() {
    
    // take care of all other treeviews 
    var navItems = $(this).siblings();
    var res;
    var ul;
    for (i = 0; i < navItems.length; i++) {
      // we go through all siblings
      // but we are only interested in the other
      // treeviews. If found, we remove the menu-open class
      // from the li wrapper and set display none to the ul children.
      // ul is the second children of the li wrapper, hence index 1.
      if ($(navItems[i]).hasClass("has-treeview")) {
        res = navItems[i];
        ul = res.children[1];
        $(res).removeClass("menu-open");
        $(ul).css("display", "none");
      }
    }
  });
  
  // Select treeview link on click
  // the treeview li element cannot be active itself thus
  // we take the related link a.
  $(".has-treeview > a").on("click", function() {
    var treeview = $(".has-treeview > a");
    var activeTreeviews = $(".has-treeview > a.active");
    // set all other nav-links to inactive
    $("#mymenu .nav-link").removeClass("active");
    // set all other treeviews to inactive
    // if there are more than 1 treeview
    if (treeview.length > 1) {
      $(activeTreeviews).removeClass("active");
    }
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
    
    
    // collapse all treeviews elements when click on a link
    // that is not a treeview
    var treeviews =  $(".has-treeview");
    if (!$(this).parents().hasClass("has-treeview")) {
      $(treeviews).removeClass("menu-open");
      for (i = 0; i < treeviews.length; i++) {
        $(treeviews[i].children[1]).css("display", "none");
      }
    }
    
    // if click on an item part of a treeview, select the treeview parent
    // by default
    var treeViewLi = $(this).parents()[2];
    var treeViewLiLink = treeViewLi.children[0];
    $(treeViewLiLink).addClass("active");
  });
});