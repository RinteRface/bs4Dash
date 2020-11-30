$(function () {
  // handle tooltip from the server side
  Shiny.addCustomMessageHandler('create-tooltip', function (message) {
    var tooltipTarget;
    if (message.id) {
      tooltipTarget = '#' + message.id;
    } else {
      if (message.selector) {
        tooltipTarget = message.selector;
      }
    }
    $(tooltipTarget)
      .addClass('has-tooltip')
      .tooltip(message.options);
    console.log(`'Tooltip created for ${tooltipTarget}'`);
  });

  Shiny.addCustomMessageHandler('remove-tooltip', function (message) {
    var tooltipTarget = '#' + message;

    // only destroys if popover exists
    if ($(tooltipTarget).hasClass('has-tooltip')) {
      $(tooltipTarget)
        .removeClass('has-tooltip')
        .tooltip('dispose');
      console.log(`'Tooltip destroyed for ${tooltipTarget}'`);
    }
  });

  // handle popover from the server side
  Shiny.addCustomMessageHandler('create-popover', function (message) {
    var popoverTarget;
    if (message.id) {
      popoverTarget = '#' + message.id;
    } else {
      if (message.selector) {
        popoverTarget = message.selector;
      }
    }
    // indicate target has popover. This is for removePopover to know
    // whether the popover exists
    $(popoverTarget)
      .addClass('has-popover')
      .popover(message.options);
    console.log(`'Popover created for ${popoverTarget}'`);
  });


  Shiny.addCustomMessageHandler('remove-popover', function (message) {
    var popoverTarget = '#' + message;

    // only destroys if popover exists
    if ($(popoverTarget).hasClass('has-popover')) {
      $(popoverTarget)
        .removeClass('has-popover')
        .popover('dispose');
      console.log(`'Popover destroyed for ${popoverTarget}'`);
    }
  });


  // handle builtin toasts
  Shiny.addCustomMessageHandler('toast', function (message) {
    $(document).Toasts('create', message);
  });

  // Create an alert
  Shiny.addCustomMessageHandler('create-alert', function (message) {
    // setup target
    var alertTarget;
    if (message.id) {
      alertTarget = `#${message.id}`;
    } else {
      if (message.selector) {
        alertTarget = message.selector;
      }
    }

    // build the tag from options
    var config = message.options, alertCl, alertTag, iconType, closeButton, titleTag, contentTag;
    alertCl = 'alert alert-dismissible';
    if (config.status !== undefined) {
      alertCl = `${alertCl} alert-${config.status}`;
    }
    if (config.elevation !== undefined) {
      alertCl = `${alertCl} elevation-${config.elevation}`;
    }

    switch (config.status) {
      case 'primary': iconType = 'info';
        break;
      case 'danger': iconType = 'ban';
        break;
      case 'info': iconType = 'info';
        break;
      case 'warning': iconType = 'warning';
        break;
      case 'success': iconType = 'check';
        break;
      default: console.warn(`${config.status} does not belong to allowed statuses!`)
    }

    if (config.closable) {
      closeButton = '<button type="button" class="close" data-dismiss="alert" aria-hidden="true">x</button>'
    }

    titleTag = `<h5><i class="icon fa fa-${iconType}"></i></h5>`
    contentTag = config.content;

    alertTag = `<div 
      id="${message.id}-alert" 
      class="${alertCl}">
        ${closeButton}${titleTag}${contentTag}
    </div>`
    if (config.width !== undefined) {
      alertTag = `<div class="col-sm-${config.width}">${alertTag}</div>`
    }

    // add it to the DOM if no existing alert is found in the anchor
    if ($(`#${message.id}-alert`).length === 0) {
      $(alertTarget).append(alertTag);
      Shiny.setInputValue(message.id, true, { priority: 'event' });

      // add events only after element is inserted

      // callback -> give ability to perform more actions on the Shiny side
      // once the alert is closed
      $(`#${message.id}-alert`).on('closed.bs.alert', function () {
        Shiny.setInputValue(message.id, false, { priority: 'event' });
      });
      // Clicking on close button does not trigger any event.
      // Trigger the closed.bs.alert event.
      $('[data-dismiss="alert"]').on('click', function () {
        var alertId = $(this).parent.attr('id');
        $(`#${alertId}.`).trigger('closed.bs.alert');
      });

    } else {
      console.warn(`${alertTarget} already has an alert!`);
    }
  });


  Shiny.addCustomMessageHandler('close-alert', function (message) {
    // only closes if element exists
    if ($(`#${message}-alert`).length > 0) {
      $(`#${message}-alert`).alert('close');
    } else {
      console.warn('Nothing to delete!');
    }
  });
});