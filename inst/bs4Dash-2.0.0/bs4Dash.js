$(function () {
  
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
      $('.dark-theme-icon')
        .removeClass('fa-sun')
        .addClass('fa-moon');
    } else {
      $('body').removeClass('dark-mode');
      $navbar
        .removeClass('navbar-dark')
        .addClass('navbar-light');
      $('.dark-theme-icon')
        .removeClass('fa-moon')
        .addClass('fa-sun');
    }
  });
  
  var $dark_mode_icon = $('body').hasClass('dark-mode') ? '<i class="dark-theme-icon fa fa-moon"></i>' : '<i class="dark-theme-icon fa fa-sun"></i>';
  var $dark_mode_container = $('<div />', { class: 'custom-control custom-switch' }).append($dark_mode_checkbox).append(`<label class="custom-control-label" for="customSwitch1">${$dark_mode_icon}</label>`);
  $navbar.append($dark_mode_container);
  
});