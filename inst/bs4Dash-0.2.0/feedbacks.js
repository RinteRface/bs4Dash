$(function() {
  // handle tooltip from the server side
  Shiny.addCustomMessageHandler('tooltip', function(message) {
  var tooltipTarget;
  if (message.id) {
    tooltipTarget = '#' + message.id;
  } else {
    if (message.selector){
      tooltipTarget = message.selector;
    }
  }
  $(tooltipTarget).tooltip(message.options);
 });
 
 // handle popover from the server side
 Shiny.addCustomMessageHandler('popover', function(message) {
  var popoverTarget;
  if (message.id) {
    popoverTarget = '#' + message.id;
  } else {
    if (message.selector){
      popoverTarget = message.selector;
    }
  }
  $(popoverTarget).popover(message.options);
 });
  
  
  // handle builtin toasts
  Shiny.addCustomMessageHandler('toast', function(message) {
    $(document).Toasts('create', message);
  });
});