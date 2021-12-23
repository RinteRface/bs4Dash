$(function () {

  $navbar = $('.main-header.navbar');

  // Modify the shiny tabsetpanel binding to follow BS4 rules
  $(document).on('shiny:connected', function (event) {
    Shiny.unbindAll();
    $.extend(Shiny
      .inputBindings
      .bindingNames['shiny.bootstrapTabInput']
      .binding, {
      // do whathever you want to edit existing methods
      getValue: function (el) {
        var anchor = $(el).find('li:not(.dropdown)').children('a.active');
        if (anchor.length === 1)
          return this._getTabName(anchor);

        return null;
      }
    });
    Shiny.bindAll();
  });

  // footer has fixed layout?
  if ($(".main-footer").attr("data-fixed") === "true") {
    $("body").addClass("layout-footer-fixed");
  }

  // add dropdown-menu-right class to correctly open the dropdown to all 
  // navbar rightUi elements
  var navbarRight = $('.navbar-right').find('.dropdown-menu');
  $(navbarRight).each(function () {
    if (!$(this).hasClass('dropdown-menu-right')) {
      $(this).addClass('dropdown-menu-right');
    }
  });


  // data toggle collapse icon update
  $('.user-block [data-toggle="collapse"]').on('click', function () {
    if ($(this).children('i').hasClass('fa-plus')) {
      $(this).children('i').attr('class', 'fa fa-minus');
    } else {
      $(this).children('i').attr('class', 'fa fa-plus');
    }
  });

  // fullscreen toggle
  if ($('body').attr('data-fullscreen') == 1) {
    var fullScreenToggle = `<li class="nav-item">
      <a class="nav-link" data-widget="fullscreen" href = "#" role = "button" >
        <i class="fas fa-expand-arrows-alt"></i>
      </a>
    </li > `;
    $(fullScreenToggle).insertBefore($('[data-widget="control-sidebar"]').parent());
  }

  // slide to top button
  if ($('body').attr('data-scrollToTop') == 1) {
    var $slideToTop = $('<div />');

    $slideToTop.html('<i class="fa fa-chevron-up"></i>');

    $slideToTop.css({
      position: 'fixed',
      bottom: '20px',
      right: '25px',
      width: '40px',
      height: '40px',
      color: '#eee',
      'font-size': '',
      'line-height': '40px',
      'text-align': 'center',
      'background-color': '#222d32',
      cursor: 'pointer',
      'border-radius': '5px',
      'z-index': '99999',
      opacity: '.7',
      'display': 'none'
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
  }
  
  // nav item click also triggers scroll to top
  $('.main-sidebar .nav-item').on('click', function () {
    $('html, body').animate({
      scrollTop: 0
    }, 0);
  }); 


  // tooltip/popover toggle
  if ($('body').attr('data-help') == 1) {
    var $help_switch_checkbox = $('<input />', {
      type: 'checkbox',
      id: 'help_switch',
      class: 'custom-control-input'
    }).on('click', function () {
      if ($(this).is(':checked')) {
        $('[data-toggle="tooltip"]').tooltip('enable');
        $('[data-toggle="popover"]').popover({
          trigger: 'hover'
        });
        $('[data-toggle="popover"]').popover('enable');
      } else {
        $('[data-toggle="tooltip"]').tooltip('disable');
        $('[data-toggle="popover"]').popover('disable');
      }
    });

    var $help_switch_container = $('<div />', { class: 'custom-control custom-switch mx-2 mt-2' }).append($help_switch_checkbox).append(`<label class="custom-control-label" for="help_switch"><i class="fa fa-question"></i></label>`);
    
    // insert before $('#controlbar-toggle') whenever possible ...
    if ($('.nav-item #controlbar-toggle')) {
      $help_switch_container.insertBefore($('#controlbar-toggle').parent());
    } else {
      $navbar.append($help_switch_container);
    }

    // trigger first click, if necessary
    $help_switch_checkbox.click();
  }

  // dark mode input
  $(document).one('shiny:connected', function () {
    if ($('body').hasClass('dark-mode')) {
      Shiny.setInputValue('dark_mode', true, { priority: 'event' });
    } else {
      Shiny.setInputValue('dark_mode', false, { priority: 'event' });
    }
  });


  // Navbar colors
  getNavbarColor = function () {
    for (let color of navbar_all_colors) {
      if ($('.main-header').attr('class').search(color) > -1) {
        return color;
      }
    }
  };

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
    'navbar-warning',
    'navbar-white',
    'navbar-orange'
  ];

  var navbar_all_colors = navbar_dark_skins.concat(navbar_light_skins);
  // find navbar color
  var navbarColor;

  // automatic global theme switcher
  if ($('body').attr('data-dark') == 2 || 
  $('body').attr('data-dark') == 1) {
    var $dark_mode_checkbox = $('<input />', {
      type: 'checkbox',
      id: 'customSwitch1',
      class: 'custom-control-input'
    }).on('click', function () {

      // get any selected navbar skin in the navbar themer
      var newNavbarColor;
      $('.navbar-themer-chip').filter(function () {
        if ($(this).css('border-style') === 'solid') {
          newNavbarColor = 'navbar-' +
            $(this)
              .attr('class')
              .split('elevation-2')[0]
              .trim()
              .replace('bg-', '');
        }
      });

      if ($(this).is(':checked')) {
        $('body').addClass('dark-mode');

        // use updateNavbarTheme to correctly setup the skin as depending
        // on the required color. If no color is chosen, we use gray-dark for dark mode
        if (newNavbarColor === undefined) {
          newNavbarColor = "navbar-gray-dark";
        }
        updateNavbarTheme(newNavbarColor);

        // sidebar update  
        if ($('.main-sidebar').length > 0) {
          $('.main-sidebar').attr('class', $('.main-sidebar')
            .attr('class')
            .replace('light', 'dark'));
          $('#sidebar-skin').prop("checked", true);

          $('.sidebar-themer-icon')
            .removeClass('fa-sun')
            .addClass('fa-moon');
        }

        // controlbar update
        if ($('.control-sidebar').length > 0) {
          $('.control-sidebar').attr('class', $('.control-sidebar')
            .attr('class')
            .replace('light', 'dark'));
          $('#controlbar-skin').prop("checked", true);

          $('.controlbar-themer-icon')
            .removeClass('fa-sun')
            .addClass('fa-moon');
        }


        $('.dark-theme-icon')
          .removeClass('fa-sun')
          .addClass('fa-moon');

        // refresh shiny input value  
        Shiny.setInputValue('dark_mode', true, { priority: 'event' });

      } else {
        $('body').removeClass('dark-mode');

        // use updateNavbarTheme to correctly setup the skin as depending
        // on the required color. If no color is chosen, we use white for light mode
        if (newNavbarColor === undefined) {
          newNavbarColor = "navbar-white";
        }
        updateNavbarTheme(newNavbarColor);

        // sidebar update
        if ($('.main-sidebar').length > 0) {
          $('.main-sidebar').attr('class', $('.main-sidebar')
            .attr('class')
            .replace('dark', 'light'));
          $('#sidebar-skin').prop("checked", false);

          $('.sidebar-themer-icon')
            .removeClass('fa-moon')
            .addClass('fa-sun');
        }

        // controlbar update
        if ($('.control-sidebar').length > 0) {
          $('.control-sidebar').attr('class', $('.control-sidebar')
            .attr('class')
            .replace('dark', 'light'));
          $('#controlbar-skin').prop("checked", false);

          $('.controlbar-themer-icon')
            .removeClass('fa-moon')
            .addClass('fa-sun');
        }

        $('.dark-theme-icon')
          .removeClass('fa-moon')
          .addClass('fa-sun');

        // refresh shiny input value  
        Shiny.setInputValue('dark_mode', false, { priority: 'event' });
      }
    });

    var $dark_mode_icon = $('body').hasClass('dark-mode') ? '<i class="dark-theme-icon fa fa-moon"></i>' : '<i class="dark-theme-icon fa fa-sun"></i>';
    var $dark_mode_container = $('<div />', { class: 'custom-control custom-switch mx-2 mt-2' }).append($dark_mode_checkbox).append(`<label class="custom-control-label" for="customSwitch1">${$dark_mode_icon}</label>`);
    
    // insert before $('#controlbar-toggle') whenever possible ...
    if ($('.nav-item #controlbar-toggle')) {
      $dark_mode_container.insertBefore($('#controlbar-toggle').parent());
    } else {
      $navbar.append($dark_mode_container);
    }
    
    
    // Trigger dark mode
    if ($('body').attr('data-dark') == 2) {
      $(document).on('shiny:connected', function() {
        $('#customSwitch1').click();
      }); 
    }
  }
  

  // Themer chips

  // Better style on hover
  $('.themer-chip').hover(function () {
    $(this).css({ opacity: 1 }).removeClass('elevation-2').addClass('elevation-4');
  }, function () {
    $(this).css({ opacity: 0.8 }).removeClass('elevation-4').addClass('elevation-2');
  });

  // 
  $('.navbar-themer-chip').on('click', function () {
    $(this).css({ 'border-color': 'yellow', 'border-style': 'solid' });
    $('.navbar-themer-chip').not(this).css({ 'border-color': '', 'border-style': '' });
  });

  $('.accents-themer-chip').on('click', function () {
    $(this).css({ 'border-color': 'yellow', 'border-style': 'solid' });
    $('.accents-themer-chip').not(this).css({ 'border-color': '', 'border-style': '' });
  });

  $('.sidebar-themer-chip').on('click', function () {
    $(this).css({ 'border-color': 'yellow', 'border-style': 'solid' });
    $('.sidebar-themer-chip').not(this).css({ 'border-color': '', 'border-style': '' });
  });


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
      $('#sidebar-skin').prop("checked", true);
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
      $('#controlbar-skin').prop("checked", true);
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
