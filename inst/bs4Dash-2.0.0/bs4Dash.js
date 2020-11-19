$(function () {
  
  // Modify the shiny tabsetpanel binding to follow BS4 rules
  $(document).on('shiny:connected', function(event) {
    Shiny.unbindAll();
    $.extend(Shiny
      .inputBindings
      .bindingNames['shiny.bootstrapTabInput']
      .binding, {
        // do whathever you want to edit existing methods
        getValue: function(el) {
          var anchor = $(el).find('li:not(.dropdown)').children('a.active');
          if (anchor.length === 1)
            return this._getTabName(anchor);

          return null;
        }
      });
    Shiny.bindAll();
  });
  
  // Make the dashboard widgets sortable Using jquery UI
  $('.connectedSortable').sortable({
    placeholder         : 'sort-highlight',
    connectWith         : '.connectedSortable',
    handle              : '.card-header, .nav-tabs',
    forcePlaceholderSize: true,
    zIndex              : 999999
  });
  $('.connectedSortable .card-header, .connectedSortable .nav-tabs-custom').css('cursor', 'move');
 
  
  // handle shinyapps.io: w need to extract the worker id and
  // paste it in the url so that the apps works correctly
  // get the shiny app.io workerId
  // handles shinyapps.io
  var workerId = $('base').attr('href');
  // ensure that this code does not locally
  if (typeof workerId != "undefined") {
    var pathname = window.location.pathname;
    var newpath = pathname + workerId;
    window.history.replaceState( {} , 'newpath', newpath);
  }
  
  // footer has fixed layout?
  if ($(".main-footer").attr("data-fixed") === "true") {
    $("body").addClass("layout-footer-fixed");
  }
  
  
  // slide to top button
  var $slideToTop = $('<div />');

  $slideToTop.html('<i class="fa fa-chevron-up"></i>');

  $slideToTop.css({
    position          : 'fixed',
    bottom            : '20px',
    right             : '25px',
    width             : '40px',
    height            : '40px',
    color             : '#eee',
    'font-size'       : '',
    'line-height'     : '40px',
    'text-align'      : 'center',
    'background-color': '#222d32',
    cursor            : 'pointer',
    'border-radius'   : '5px',
    'z-index'         : '99999',
    opacity           : '.7',
    'display'         : 'none'
  });

  $slideToTop.on('mouseenter', function () {
    $(this).css('opacity', '1');
  });

  $slideToTop.on('mouseout', function () {
    $(this).css('opacity', '.7');
  });

  $('.wrapper').append($slideToTop);

  $(window).scroll(function () {
    if ($(window).scrollTop() >= 150) {
      if (!$($slideToTop).is(':visible')) {
        $($slideToTop).fadeIn(500);
      }
    } else {
      $($slideToTop).fadeOut(500);
    }
  });

  $($slideToTop).click(function () {
    $('html, body').animate({
      scrollTop: 0
    }, 500);
  });
  
  // dark mode input
  $(document).one('shiny:connected', function() {
    if ($('body').hasClass('dark-mode')) {
      Shiny.setInputValue('dark_mode', true, {priority: 'event'});
    } else {
      Shiny.setInputValue('dark_mode', false, {priority: 'event'});
    }
  });
  
  // automatic global theme switcher
  $navbar = $('.main-header.navbar');
  var $dark_mode_checkbox = $('<input />', {
    type: 'checkbox',
    id: 'customSwitch1',
    checked: $('body').hasClass('dark-mode'),
    class: 'custom-control-input'
  }).on('click', function () {
    if ($(this).is(':checked')) {
      $('body').addClass('dark-mode');
      $navbar
        .removeClass('navbar-light')
        .addClass('navbar-dark');
      
      // sidebar update  
      if ($('.main-sidebar').length > 0) {
        $('.main-sidebar').attr('class', $('.main-sidebar')
          .attr('class')
          .replace('light', 'dark'));
        $('#sidebar-skin').prop( "checked", true );
        
        $('.sidebar-themer-icon')
          .removeClass('fa-sun')
          .addClass('fa-moon');
      }
        
      // controlbar update
      if ($('.control-sidebar').length > 0) {
        $('.control-sidebar').attr('class', $('.control-sidebar')
          .attr('class')
          .replace('light', 'dark'));
        $('#controlbar-skin').prop( "checked", true );
        
        $('.controlbar-themer-icon')
          .removeClass('fa-sun')
          .addClass('fa-moon');
      }
      
      
      $('.dark-theme-icon')
        .removeClass('fa-sun')
        .addClass('fa-moon');
        
      // refresh shiny input value  
      Shiny.setInputValue('dark_mode', true, {priority: 'event'});
        
    } else {
      $('body').removeClass('dark-mode');
      $navbar
        .removeClass('navbar-dark')
        .addClass('navbar-light');
      
      // sidebar update
      if ($('.main-sidebar').length > 0) {
        $('.main-sidebar').attr('class', $('.main-sidebar')
          .attr('class')
          .replace('dark', 'light'));
        $('#sidebar-skin').prop( "checked", false );
        
        $('.sidebar-themer-icon')
          .removeClass('fa-moon')
          .addClass('fa-sun');
      }

      // controlbar update
      if ($('.control-sidebar').length > 0) {
        $('.control-sidebar').attr('class', $('.control-sidebar')
          .attr('class')
          .replace('dark', 'light'));
        $('#controlbar-skin').prop( "checked", false );
        
        $('.controlbar-themer-icon')
          .removeClass('fa-moon')
          .addClass('fa-sun');
      }
      
      $('.dark-theme-icon')
        .removeClass('fa-moon')
        .addClass('fa-sun');
      
      // refresh shiny input value  
      Shiny.setInputValue('dark_mode', false, {priority: 'event'});
    }
  });
  
  var $dark_mode_icon = $('body').hasClass('dark-mode') ? '<i class="dark-theme-icon fa fa-moon"></i>' : '<i class="dark-theme-icon fa fa-sun"></i>';
  var $dark_mode_container = $('<div />', { class: 'custom-control custom-switch' }).append($dark_mode_checkbox).append(`<label class="custom-control-label" for="customSwitch1">${$dark_mode_icon}</label>`);
  $navbar.append($dark_mode_container);
  
  // Themer chips
  
  // Better style on hover
  $('.themer-chip').hover(function () {
    $(this).css({ opacity: 1 }).removeClass('elevation-2').addClass('elevation-4');
  }, function () {
    $(this).css({ opacity: 0.8 }).removeClass('elevation-4').addClass('elevation-2');
  });
  
  // 
  $('.navbar-themer-chip').on('click', function() {
    $(this).css({ 'border-color': 'yellow', 'border-style': 'solid' });
    $('.navbar-themer-chip').not(this).css({ 'border-color': '', 'border-style': '' });
  });
  
  $('.accents-themer-chip').on('click', function() {
    $(this).css({ 'border-color': 'yellow', 'border-style': 'solid' });
    $('.accents-themer-chip').not(this).css({ 'border-color': '', 'border-style': '' });
  });
  
  $('.sidebar-themer-chip').on('click', function() {
    $(this).css({ 'border-color': 'yellow', 'border-style': 'solid' });
    $('.sidebar-themer-chip').not(this).css({ 'border-color': '', 'border-style': '' });
  });
  
  
  // Navbar Themer
  var navbar_dark_skins = [
    'navbar-primary',
    'navbar-secondary',
    'navbar-info',
    'navbar-success',
    'navbar-danger',
    'navbar-indigo',
    'navbar-purple',
    'navbar-pink',
    'navbar-maroon',
    'navbar-fuchsia',
    'navbar-navy',
    'navbar-lightblue',
    'navbar-lime',
    'navbar-teal',
    'navbar-olive',
    'navbar-gray-dark',
    'navbar-gray'
  ];

  var navbar_light_skins = [
    'navbar-light',
    'navbar-warning',
    'navbar-white',
    'navbar-orange'
  ];
  
  var navbar_all_colors = navbar_dark_skins.concat(navbar_light_skins);
  
  /**
  * Update color theme to navbar tag
  *
  * @param String color Color to apply.
  * @returns void
  */
  updateNavbarTheme = function (color) {
    var $main_header = $('.main-header');
    $main_header.removeClass('navbar-dark').removeClass('navbar-light');
    navbar_all_colors.forEach(function (color) {
      $main_header.removeClass(color);
    });

    if (navbar_dark_skins.indexOf(color) > -1) {
      $main_header.addClass('navbar-dark');
    } else {
      $main_header.addClass('navbar-light');
    }

    $main_header.addClass(color);
  };
  
  
  // Sidebar themer
  
  // detect global sidebar theme and select or not the toggle
  if ($('.main-sidebar').length > 0) {
    if ($('.main-sidebar').attr('class').match('dark')) {
      $('#sidebar-skin').prop( "checked", true );
    } 
  }
  
  // clicking on dark/light switch changes:
  // - icon style
  // - sidebar class 
  $('#sidebar-skin').on('click', function () {
    var sidebarCl;
    if ($(this).is(':checked')) {
      sidebarCl = $('.main-sidebar')
        .attr('class')
        .replace('light', 'dark');
      $('.main-sidebar').attr('class', sidebarCl);
        
      $('.sidebar-themer-icon')
        .removeClass('fa-sun')
        .addClass('fa-moon');
    } else {
      sidebarCl = $('.main-sidebar')
        .attr('class')
        .replace('dark', 'light');
      $('.main-sidebar').attr('class', sidebarCl);
      
      $('.sidebar-themer-icon')
        .removeClass('fa-moon')
        .addClass('fa-sun'); 
    }
  });
  
  var sidebar_colors = [
    'bg-primary',
    'bg-secondary',
    'bg-info',
    'bg-success',
    'bg-danger',
    'bg-indigo',
    'bg-purple',
    'bg-pink',
    'bg-maroon',
    'bg-fuchsia',
    'bg-navy',
    'bg-lightblue',
    'bg-lime',
    'bg-teal',
    'bg-olive',
    'bg-gray-dark',
    'bg-gray',
    'bg-light',
    'bg-warning',
    'bg-white',
    'bg-orange'
  ];
  
  
  var sidebar_skins = [
    'sidebar-dark-primary',
    'sidebar-dark-secondary',
    'sidebar-dark-info',
    'sidebar-dark-success',
    'sidebar-dark-danger',
    'sidebar-dark-indigo',
    'sidebar-dark-purple',
    'sidebar-dark-pink',
    'sidebar-dark-maroon',
    'sidebar-dark-fuchsia',
    'sidebar-dark-navy',
    'sidebar-dark-lightblue',
    'sidebar-dark-lime',
    'sidebar-dark-teal',
    'sidebar-dark-olive',
    'sidebar-dark-gray-dark',
    'sidebar-dark-gray',
    'sidebar-dark-light',
    'sidebar-dark-warning',
    'sidebar-dark-white',
    'sidebar-dark-orange',
    'sidebar-light-primary',
    'sidebar-light-secondary',
    'sidebar-light-info',
    'sidebar-light-success',
    'sidebar-light-danger',
    'sidebar-light-indigo',
    'sidebar-light-purple',
    'sidebar-light-pink',
    'sidebar-light-maroon',
    'sidebar-light-fuchsia',
    'sidebar-light-navy',
    'sidebar-light-lightblue',
    'sidebar-light-lime',
    'sidebar-light-teal',
    'sidebar-light-olive',
    'sidebar-light-gray-dark',
    'sidebar-light-gray',
    'sidebar-light-light',
    'sidebar-light-warning',
    'sidebar-light-white',
    'sidebar-light-orange'
  ];
  
  
  updateSidebarTheme = function (color) {
    var sidebarCl;
    if ($('#sidebar-skin').is(':checked')) {
      sidebarCl = 'sidebar-dark-';
    } else {
      sidebarCl = 'sidebar-light-';
    }
    
    var sidebar_class = sidebarCl + color.replace('bg-', '');
    var $sidebar = $('.main-sidebar');
    sidebar_skins.forEach(function (skin) {
      $sidebar.removeClass(skin);
    });

    $sidebar.addClass(sidebar_class);
  };
  
  
  // Accents themer
  var accent_colors = [
    'accent-primary',
    'accent-secondary',
    'accent-info',
    'accent-success',
    'accent-danger',
    'accent-indigo',
    'accent-purple',
    'accent-pink',
    'accent-maroon',
    'accent-fuchsia',
    'accent-navy',
    'accent-lightblue',
    'accent-lime',
    'accent-teal',
    'accent-olive',
    'accent-gray-dark',
    'accent-gray',
    'accent-light',
    'accent-warning',
    'accent-white',
    'accent-orange'
  ];
  
  
  updateAccentsTheme = function (color) {
    var accent_class = color;
    var $body = $('body');
    accent_colors.forEach(function (skin) {
      $body.removeClass(skin);
    });

    $body.addClass(accent_class);
  };
  
  
  // Controlbar themer
  
  // detect global controlbar theme and select or not the toggle
  if ($('.control-sidebar').length > 0) {
    if ($('.control-sidebar').attr('class').match('dark')) {
      $('#controlbar-skin').prop( "checked", true );
    } 
  }
  
  // clicking on dark/light switch changes:
  // - icon style
  // - sidebar class 
  $('#controlbar-skin').on('click', function () {
    var controlbarCl;
    if ($(this).is(':checked')) {
      controlbarCl = $('.control-sidebar')
        .attr('class')
        .replace('light', 'dark');
      $('.control-sidebar').attr('class', controlbarCl);
        
      $('.controlbar-themer-icon')
        .removeClass('fa-sun')
        .addClass('fa-moon');
    } else {
      controlbarCl = $('.control-sidebar')
        .attr('class')
        .replace('dark', 'light');
      $('.control-sidebar').attr('class', controlbarCl);
      
      $('.controlbar-themer-icon')
        .removeClass('fa-moon')
        .addClass('fa-sun'); 
    }
  });
  
});