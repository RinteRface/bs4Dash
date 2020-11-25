$(function() {
  // handle tooltip from the server side
  Shiny.addCustomMessageHandler('create-tooltip', function(message) {
  var tooltipTarget;
  if (message.id) {
    tooltipTarget = '#' + message.id;
  } else {
    if (message.selector){
      tooltipTarget = message.selector;
    }
  }
  $(tooltipTarget)
    .addClass('has-tooltip')
    .tooltip(message.options);
    console.log(`"Tooltip created for ${tooltipTarget}"`);
 });

 Shiny.addCustomMessageHandler('remove-tooltip', function(message) {
  var tooltipTarget = '#' + message;

  // only destroys if popover exists
  if ($(tooltipTarget).hasClass('has-tooltip')) {
    $(tooltipTarget).tooltip('dispose');
    console.log(`"Tooltip destroyed for ${tooltipTarget}"`);
  }
 });
 
 // handle popover from the server side
 Shiny.addCustomMessageHandler('create-popover', function(message) {
  var popoverTarget;
  if (message.id) {
    popoverTarget = '#' + message.id;
  } else {
    if (message.selector){
      popoverTarget = message.selector;
    }
  }
  // indicate target has popover. This is for removePopover to know
  // whether the popover exists
  $(popoverTarget)
    .addClass('has-popover')
    .popover(message.options);
  console.log(`"Popover created for ${popoverTarget}"`);
 });


 Shiny.addCustomMessageHandler('remove-popover', function(message) {
  var popoverTarget = '#' + message;

  // only destroys if popover exists
  if ($(popoverTarget).hasClass('has-popover')) {
    $(popoverTarget).popover('dispose');
    console.log(`"Popover destroyed for ${popoverTarget}"`);
  }
 });
  
  
  // handle builtin toasts
  Shiny.addCustomMessageHandler('toast', function(message) {
    $(document).Toasts('create', message);
  });
  
  // handle alert
  Shiny.addCustomMessageHandler('alert', function(message) {
    // callback -> give ability to perform more actions on the Shiny side
    // once the alert is closed
    $('#' + message).on('closed.bs.alert', function () {
      Shiny.setInputValue(message, true);
    });
    $('#' + message).alert('close');
  });
});