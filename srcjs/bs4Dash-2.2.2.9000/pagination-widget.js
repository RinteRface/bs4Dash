$( document ).ready(function() {
  
  function findActivePage() {
    return $('ul.pagination').find('li.active');
  };
  
  function toggleNavigationItems(activeItem) {
    // hide pagination previous if we are at the first item
    if ($(activeItem).prev().find('a').hasClass('pagination-previous')) {
      $('.pagination-previous').parent('li').css('display', 'none');
    } else {
      $('.pagination-previous').parent('li').css('display', 'block');
    }
    
    // hide pagination next if we are at the last item
    if ($(activeItem).next().find('a').hasClass('pagination-next')) {
      $('.pagination-next').parent('li').css('display', 'none');
    } else {
      $('.pagination-next').parent('li').css('display', 'block');
    }
  }
  
  // Must run at start
  toggleNavigationItems(findActivePage());
  
  // Toggle active item button state
  $('ul.pagination a:not(.disabled, .pagination-previous, .pagination-next)')
    .on('click', function() {
      var activeItem = findActivePage();
      $(activeItem).removeClass('active');
      $(this).parent().addClass('active');
      activeItem = $(this).parent(); // store new active item
      // Also run dynamically
      toggleNavigationItems(activeItem);
  });
  
  // Previous click
  $('ul.pagination .pagination-previous').on('click', function() {
    var activeItem = findActivePage();
      
    var previousSibling = $(activeItem).prev();
    // jump back when we find a disabled sibling
    while ($(previousSibling).hasClass('disabled')) {
      previousSibling = $(previousSibling).prev();
    }
    // Only if active item has a previous sibling that is not  
    // the pagination-previous itself.
    if ($(previousSibling).find('a').hasClass('pagination-previous') == false) {
      $(activeItem).removeClass('active');
      $(previousSibling).find('a').click(); 
    }
  });
  
  // Next click
  $('ul.pagination .pagination-next').on('click', function() {
    var activeItem = findActivePage();
      
    var nextSibling = $(activeItem).next();
    // jump back when we find a disabled sibling
    while ($(nextSibling).hasClass('disabled')) {
      nextSibling = $(nextSibling).next();
    }
    // Only if active item has a previous sibling that is not  
    // the pagination-previous itself.
    if ($(nextSibling).find('a').hasClass('pagination-next') == false) {
      $(activeItem).removeClass('active');
      $(nextSibling).find('a').click(); 
    }
  });
  
  var paginationBinding = new Shiny.InputBinding();
  $.extend(paginationBinding, {
    
    initialize: function(el) {
      
    },
    
    find: function (scope) {
      return $(scope).find('ul.pagination');
    },
  
    // Given the DOM element for the input, return the value
    getValue: function (el) {
      return $(el).find('li.active a').attr('data-value');
    },
    
    setValue: function(el, value) {
      
    },
    // internal
    _disableTab: function(el, value) {
      $(el)
        .find('a[data-value="' + value + '"]')
        .parent()
        .removeClass('active')
        .addClass('disabled')
        .attr('tabindex', '-1')
    },
    // see updatePagination
    receiveMessage: function (el, data) {
      // Activate new element
      if (data.hasOwnProperty('selected')) {
        // Disable active element
        $(el)
          .find('.active')
          .removeClass('active');
        // Activate new element
        var selectedItem = $(el)
          .find('a[data-value="' + data.selected + '"]');
        // If the element was disabled before
        if ($(selectedItem).parent('li').hasClass('disabled')) {
          $(selectedItem)
            .parent('li')
            .removeClass('disabled')
            .removeAttr('tabindex');
        }
        $(selectedItem).click();
      }
      
      // Disable elements
      if (data.hasOwnProperty('disabled')) {
        // loop over all elements
        if (typeof data.disabled == 'string') {
          this._disableTab(el, data.disabled)
        } else {
          for (i of data.disabled) {
          // disable element 
            this._disableTab(el, i)
          }
        }
        
        // Activate next or previous not disabled li
        var hasActiveItem = $(el).find('li.active').length
        if (hasActiveItem == 0) {
          // Activate first element found (maybe discussed ...)
          var newActive = $(el)
            .find('a:not(.pagination-previous, .pagination-next)')
            .parent('li:not(.disabled)');
          if (newActive.length > 0) {
            $(newActive[0]).find('a').click();
          }
        }
      }
      
      // Trigger the callback to update the input value 
      // on the server side.
      $(el).trigger('change');
    },
  
    subscribe: function (el, callback) {
      $(el).find('a').on('click', function() {
        callback();
      });
      
      // Necessry for updatePagination
      $(el).on('change', function() {
        callback();
      })
    },
  
    unsubscribe: function (el) {
      $(el).off('.paginationBinding');
    }
  });
 
  Shiny.inputBindings.register(paginationBinding);
});
