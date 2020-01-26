$(function() {
  // handle tooltip from the server side
  Shiny.addCustomMessageHandler('tooltip', function(message) {
    var tooltipTarget = '#' + message.target;
    $(tooltipTarget).attr('data-toggle', 'tooltip');
    $(tooltipTarget).tooltip({
      placement: message.placement,
      title: message.title
    });
  });
  
  // handle popover from the server side
  Shiny.addCustomMessageHandler('popover', function(message) {
    var popoverTarget = '#' + message.target;
    $(popoverTarget).attr('data-toggle', 'popover');
    $(popoverTarget).popover({
      placement: message.placement,
      title: message.title,
      content: message.content,
      // allow to close the popover when click on another element
      trigger: 'focus'
    });
  });
});