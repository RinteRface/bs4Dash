// This code creates an input binding for the accordion component
var accordionBinding = new Shiny.InputBinding();

$.extend(accordionBinding, {
  find: function(scope) {
    return $(scope).find(".accordion");
  },
  // Given the DOM element for the input, return the value
  getValue: function(el) {
    // active is given by the setValue method
    var activeItem = $(el).children(".card.active").index() + 1;
    if (activeItem === 0) return;
    // returns the index of the active item from the R point of view. It is possible
    // that no item is shown at start. In this case,  NULL is returned
    return activeItem;
  },
  
  setValue: function(el, value) {
    // remove active class from all other panels
    $(el).children(".card.active").removeClass("active");
    
    // add active class to current panel
    $(el).children()
     .eq(value - 1)
     .addClass("active");
    
    // click on the header to trigger a collapse
    $(el)
     .children()
     .eq(value - 1)
     .find('[data-toggle="collapse"]')
     .click();
     
     // trigger change to tell Shiny to update the value
     $(el).trigger("change");
  },

  // see updateAccordion
  receiveMessage: function(el, data) {
    this.setValue(el, data);
  },

  subscribe: function(el, callback) {
    // cf setValue
    $(el).on("change", function(e) {
      callback();
    });
    
    // manual click will update
    $(el).find('[data-toggle="collapse"]').on("click", function(e) {
      if (!$(this).closest(".card").hasClass("active")) {
        $(el).children(".card.active").removeClass("active");
      } 
      $(this).closest(".card").addClass("active");
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off(".accordionBinding");
  }
});

Shiny.inputBindings.register(accordionBinding, "accordion-input");
// Buttons valid colors are part of statuses like btn-primary, ...
const validStatuses = [
  "primary",
  "secondary",
  "success",
  "info",
  "warning",
  "danger"
];
// Cards may have few additional statuses like bg-orange
const validStatusesPlus = [
  "dark",
  "white",
  "lightblue",
  "navy",
  "orange",
  "fuchsia",
  "purple",
  "indigo",
  "gray",
  "gray-dark",
  "pink",
  "maroon",
  "teal",
  "lime",
  "olive",
  "green",
  "yellow",
  "red",
  "blue"
];

// Input binding
var cardBinding = new Shiny.InputBinding();

$.extend(cardBinding, {
  find: function(scope) {
    return $(scope).find(".card.bs4Dash");
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    var config = $(el)
      .parent()
      .find("script[data-for='" + el.id + "']");
    config = JSON.parse(config.html());

    var isCollapsed = $(el).hasClass("collapsed-card");
    var display = $(el).css("display");
    var isMaximized = $(el).hasClass("maximized-card");

    var visible;
    if (display === "none") {
      visible = false;
    } else {
      visible = true;
    }

    // toggle collapse button when maximized
    if (isMaximized) {
      $(el)
        .find("[data-card-widget = 'collapse']")
        .hide();
    } else {
      $(el)
        .find("[data-card-widget = 'collapse']")
        .show();
    }

    return {
      collapsible: config.collapsible,
      collapsed: isCollapsed,
      closable: config.closable,
      visible: visible,
      maximizable: config.maximizable,
      maximized: isMaximized,
      status: config.status,
      solidHeader: config.solidHeader,
      background: config.background,
      width: config.width,
      height: config.height
    }; // this will be a list in R
  },
  _updateWidth: function(el, o, n) {
    $(el)
      .parent()
      .toggleClass("col-sm-" + o);
    $(el)
      .parent()
      .addClass("col-sm-" + n);
    // trigger resize so that output resize
    $(el).trigger("resize");
  },

  setValue: function(el, value) {
    var config = $(el)
      .parent()
      .find("script[data-for='" + el.id + "']");
    config = JSON.parse(config.html());

    if (value.action === "update") {
      var isUserCard = $(el).hasClass("user-card");
      var isSocialCard = $(el).hasClass("social-card");

      if (value.options.hasOwnProperty("title")) {
        if (value.options.title !== config.title) {
          var newTitle;
          if (typeof value.options.title !== "string") {
            newTitle = $.parseHTML(value.options.title[0]);
          } else {
            newTitle = $.parseHTML(value.options.title);
          }

          var tools = $(el).find(".card-tools");
          // social box
          if (isSocialCard) {
            $(el)
              .find(".user-block")
              .replaceWith($(newTitle));
          } else if (isUserCard) {
            // handle 2 cards types
            if (typeof value.options.title === "string") {
              // don't take newTitle[1] (contains some text)
              newTitle = [newTitle[0], newTitle[2]];
              // change widget-use class
              $(el)
                .removeClass("widget-user-2")
                .addClass("widget-user");
              // insert header and image after
              $(el)
                .find(".widget-user-header")
                .replaceWith($(newTitle[0]));
              $(newTitle[1]).insertAfter($(el).find(".widget-user-header"));
            } else {
              $(el)
                .removeClass("widget-user")
                .addClass("widget-user-2");
              // remove old user inage if old type was 1
              $(el)
                .find(".widget-user-image")
                .remove();
              $(el)
                .find(".widget-user-header")
                .replaceWith($(newTitle));

              if (value.options.status !== null) {
                if (value.options.gradient) {
                  $(el)
                    .find(".widget-user-header")
                    .addClass("bg-gradient-", status);
                } else {
                  $(el)
                    .find(".widget-user-header")
                    .addClass("bg-", status);
                }
              }
            }
            // add tools as first child of widget-user-header
            $(el)
              .find(".widget-user-header")
              .prepend($(tools));
          } else {
            if (!$(newTitle).hasClass("card-title"))
              $(newTitle).addClass("card-title");
            $(el)
              .find(".card-title")
              .replaceWith($(newTitle));
          }
          config.title = value.options.title;
        }
      }

      if (value.options.hasOwnProperty("collapsible")) {
        if (value.options.collapsible !== config.collapsible) {
          if (!value.options.collapsible) {
            $(el)
              .find('[data-card-widget = "collapse"]')
              .remove();
            config.collapsible = false;
          } else {
            // only add if no collapsible
            if ($(el).find('[data-card-widget = "collapse"]').length === 0) {
              $(el)
                .find(".card-tools.float-right")
                .prepend(
                  $(
                    '<button class="btn btn-tool btn-sm" data-card-widget="collapse"><i class="fa fa-minus"></i></button>'
                  )
                );
              config.collapsible = true;
            }
          }
        }
      }

      if (value.options.hasOwnProperty("closable")) {
        if (value.options.closable !== config.closable) {
          if (!value.options.closable) {
            $(el)
              .find('[data-card-widget = "remove"]')
              .remove();
            config.closable = false;
          } else {
            if ($(el).find('[data-card-widget = "remove"]').length === 0) {
              // Remove goes between collapse and maximize...
              if ($(el).find('[data-card-widget = "maximize"]').length === 0) {
                $(el)
                  .find(".card-tools.float-right")
                  .append(
                    $(
                      '<button class="btn btn-tool btn-sm" data-card-widget="remove"><i class="fa fa-times"></i></button>'
                    )
                  );
              } else {
                $(
                  '<button class="btn btn-tool btn-sm" data-card-widget="remove"><i class="fa fa-times"></i></button>'
                ).insertBefore($(el).find('[data-card-widget = "maximize"]'));
              }
              config.closable = true;
            }
          }
        }
      }

      if (value.options.hasOwnProperty("maximizable")) {
        if (value.options.maximizable !== config.maximizable) {
          if (!value.options.maximizable) {
            $(el)
              .find('[data-card-widget = "maximize"]')
              .remove();
            config.maximizable = false;
          } else {
            if ($(el).find('[data-card-widget = "maximize"]').length === 0) {
              $(el)
                .find(".card-tools.float-right")
                .append(
                  $(
                    '<button class="btn btn-tool btn-sm" data-card-widget="maximize"><i class="fa fa-expand"></i></button>'
                  )
                );
              config.maximizable = true;
            }
          }
        }
      }

      if (value.options.hasOwnProperty("solidHeader")) {
        // only update if config an new value are different
        if (!isSocialCard && !isUserCard) {
          if (
            value.options.solidHeader !== config.solidHeader &&
            $(el).hasClass("card-outline")
          ) {
            $(el).removeClass("card-outline");
            config.solidHeader = true;
          } else {
            if (!$(el).hasClass("card-outline") && !value.options.solidHeader) {
              var cond = config.status || value.options.status;
              // solidheader cannot be removed if status and background exist or if status is null
              if (!(value.options.background && cond)) {
                $(el).addClass("card-outline");
                config.solidHeader = false;
              } else if (
                value.options.background === null &&
                !(config.background && cond)
              ) {
                $(el).addClass("card-outline");
                config.solidHeader = false;
              }
            } else if ($(el).hasClass("card-outline")) {
              var cond = config.status || value.options.status;
              // solidheader cannot be removed if status and background exist or if status is null
              if (value.options.background && cond) {
                $(el).removeClass("card-outline");
                config.solidHeader = true;
              } else if (config.background && cond) {
                $(el).removeClass("card-outline");
                config.solidHeader = false;
              }
            }
          }
        }
      }

      // To remove status explicitly set status = NULL in updateBox. Don't apply
      // to socialBox in AdminLTE2!!!
      if (value.options.hasOwnProperty("status")) {
        if (!isSocialCard) {
          if (value.options.status !== config.status) {
            var oldClass, newClass;
            // If there was a status and the user decide to remove any status
            if (value.options.status === null && config.status !== null) {
              if (!isUserCard) $(el).removeClass("card-" + config.status);
              // add class card-outline for better render (status = NULL)
              // renders with grey border which is not nice
              if ($(el).hasClass("card-outline") && !isUserCard) {
                $(el).addClass("card-outline");
              }

              // Apply new background color to buttons if any
              if (value.options.background) {
                var background = value.options.background;
                if (validStatusesPlus.indexOf(background) > -1) {
                  $(el)
                    .find(".btn-tool")
                    .addClass("bg-" + background);
                } else if (validStatuses.indexOf(background) > -1) {
                  $(el)
                    .find(".btn-tool")
                    .addClass("btn-" + background);
                }
              }

              // in case there is a status and it is not null (indeed we can send null through R)
            } else if (value.options.status) {
              // apply new status
              if (isUserCard) {
                newClass = "bg-";
                if (value.options.gradient) {
                  newClass = newClass + "gradient-";
                }
                newClass = newClass + value.options.status;
                $(el)
                  .find(".widget-user-header")
                  .addClass(newClass);
              } else {
                newClass = "card-" + value.options.status;
                $(el).addClass(newClass);
              }

              // remove old status, if there was one ...
              if (config.status) {
                if (isUserCard) {
                  oldClass = "bg-";
                  if (config.gradient) {
                    oldClass = oldClass + "gradient-";
                  }
                  oldClass = oldClass + config.status;
                  $(el)
                    .find(".widget-user-header")
                    .removeClass(oldClass);
                } else {
                  oldClass = "card-" + config.status;
                  $(el).removeClass(oldClass);
                }
              }

              // Add new color for Buttons. We handle extra statuses in which case
              // the button class changes. Only if solidHeader
              if (!$(el).hasClass("card-outline") || isUserCard) {
                if (validStatusesPlus.indexOf(value.options.status) > -1) {
                  $(el)
                    .find(".btn-tool")
                    .addClass("bg-" + value.options.status);
                } else if (validStatuses.indexOf(value.options.status) > -1) {
                  $(el)
                    .find(".btn-tool")
                    .addClass("btn-" + value.options.status);
                }
              }
            }

            // If there was a status or background, we must cleanup the old button status
            // since status predominate over background. We also handle extra
            // statuses ...
            var status;
            if (config.status || config.background) {
              // status dominates
              if (config.status) {
                status = config.status;
              } else if (config.background) {
                status = config.background;
              }

              if (validStatusesPlus.indexOf(status) > -1) {
                $(el)
                  .find(".btn-tool")
                  .removeClass("bg-" + status);
              } else if (validStatuses.indexOf(status) > -1) {
                $(el)
                  .find(".btn-tool")
                  .removeClass("btn-" + status);
              }
            }
            config.status = value.options.status;
          }
        }
      }

      // To remove background explicitly set background = NULL in updateBox
      if (value.options.hasOwnProperty("background")) {
        if (value.options.background !== config.background) {
          var oldBgClass = "bg-";
          newBgClass = oldBgClass;
          // don't touch if null
          if (config.background) {
            // if gradient, the class has a gradient in between!
            if (config.gradient) {
              oldBgClass = oldBgClass + "gradient-";
            }
            oldBgClass = oldBgClass + config.background;
            // handle userBox
            // for which we also have to toggle the header bg color
            // and the box tools buttons color
            if (isUserCard && !(config.status || value.options.status)) {
              var header = $(el).find(".widget-user-header");
              $(header).removeClass(oldBgClass);
            }

            $(el).removeClass(oldBgClass);
          }
          if (value.options.background) {
            if (config.gradient || value.options.gradient) {
              newBgClass = newBgClass + "gradient-";
            }
            newBgClass = newBgClass + value.options.background;
            if (isUserCard && !(config.status || value.options.status)) {
              var header = $(el).find(".widget-user-header");
              $(header).addClass(newBgClass);
            }
            $(el).addClass(newBgClass);
          }
          if (
            config.gradient !== value.options.gradient &&
            value.options.gradient !== undefined
          ) {
            config.gradient = value.options.gradient;
          }
          config.background = value.options.background;
        }
      }

      if (value.options.hasOwnProperty("width")) {
        if (value.options.width !== config.width) {
          this._updateWidth(el, config.width, value.options.width);
          config.width = value.options.width;
        }
      }

      if (value.options.hasOwnProperty("height")) {
        if (value.options.height !== config.height) {
          if (value.options.height === null) {
            $(el)
              .find(".card-body")
              .css("height", "");
          } else {
            $(el)
              .find(".card-body")
              .css("height", value.options.height);
          }

          config.height = value.options.height;
          // don't need to trigger resize since the output height
          // is not controlled by the box size ...
        }
      }

      // replace the old JSON config by the new one to update the input value
      $(el)
        .parent()
        .find("script[data-for='" + el.id + "']")
        .replaceWith(
          '<script type="application/json" data-for="' +
            el.id +
            '">' +
            JSON.stringify(config) +
            "</script>"
        );
    } else {
      if (value != "restore") {
        if ($(el).css("display") != "none") {
          $(el).CardWidget(value);
        }
      } else {
        $(el).show();
        // this is needed so that the last event handler is considered
        // in the subscribe method.
        $(el).trigger("shown");
      }
    }
  },
  receiveMessage: function(el, data) {
    this.setValue(el, data);
    $(el).trigger("change");
  },

  subscribe: function(el, callback) {
    $(el).on("expanded.lte.cardwidget collapsed.lte.cardwidget", function(e) {
      // set a delay so that SHiny get the input value when the collapse animation
      // is finished.
      setTimeout(function() {
        callback();
      }, 500);
    });

    $(el).on("maximized.lte.cardwidget minimized.lte.cardwidget", function(e) {
      callback();
    });

    $(el).on("removed.lte.cardwidget", function(e) {
      setTimeout(function() {
        callback();
      }, 500);
    });
    // we need to split removed and shown event since shown is immediate whereas close
    // takes some time
    $(el).on("shown.cardBinding", function(e) {
      callback();
    });

    // handle change event triggered in the setValue method
    $(el).on("change.cardBinding", function(event) {
      setTimeout(function() {
        callback();
      }, 500);
    });
  },

  unsubscribe: function(el) {
    $(el).off(".cardBinding");
  }
});

Shiny.inputBindings.register(cardBinding);

// Card sidebar input binding
var cardSidebarBinding = new Shiny.InputBinding();
$.extend(cardSidebarBinding, {
  initialize: function(el) {
    // erase default to avoid seeing moving sidebars on initialization
    $(".direct-chat-contacts, .direct-chat-messages").css({
      transition: "transform .0s ease-in-out"
    });

    var background = $(el).attr("data-background")
      ? $(el).attr("data-background")
      : "#343a40";
    var width = $(el).attr("data-width")
      ? parseInt($(el).attr("data-width"))
      : 100;
    var closeTranslationRate = (100 * 100) / width;
    var contacts = $(el)
      .closest(".direct-chat")
      .find(".direct-chat-contacts");

    // apply width and background
    $(contacts).css({
      background: `${background}`,
      width: `${width}%`
    });

    // If start open, apply openTranslationRate else apply closeTranslationRate ...
    if ($(el).attr("data-start-open") === "true") {
      var openTranslationRate = closeTranslationRate - 100;
      $(contacts).css({ transform: `translate(${openTranslationRate}%, 0)` });
    } else {
      $(contacts).css({ transform: `translate(${closeTranslationRate}%, 0)` });
    }

    // Restore for better transitions
    setTimeout(function() {
      $(".direct-chat-contacts, .direct-chat-messages").css({
        transition: "transform .5s ease-in-out"
      });
    }, 300);
    
    // Easyclose feature
    if ($(el).attr("data-easy-close") === "true") {
      $(document).mouseup(function(e) {
        var container = $(".direct-chat-contacts");
        var openContainer = $(".direct-chat-contacts-open");
        // if the target of the click isn't the container nor a descendant of the container and also not if the filter symbol was clicke  d
        if (!container.is(e.target) && 
            container.has(e.target).length === 0 && 
            $(e.target).parents('.card-tools').length !== 1) {
            openContainer
              .find("[data-widget='chat-pane-toggle']")
              .click();
        }
      }); 
    }
  },

  find: function(scope) {
    return $(scope).find('[data-widget="chat-pane-toggle"]');
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    var cardWrapper = $(el).closest(".card");
    return $(cardWrapper).hasClass("direct-chat-contacts-open");
  },

  // see updatebs4Card
  receiveMessage: function(el, data) {
    // In theory, adminLTE3 has a builtin function
    // we could use $(el).DirectChat('toggle');
    // However, it does not update the related input.
    // The toggled.lte.directchat event seems to be broken.
    $(el).trigger("click");
    $(el).trigger("shown");
  },

  subscribe: function(el, callback) {
    var self = this;
    $(el).on("click", function(e) {
      var width = $(el).attr("data-width")
        ? parseInt($(el).attr("data-width"))
        : 100;
      var closeTranslationRate = (100 * 100) / width;
      var openTranslationRate = closeTranslationRate - 100;
      // set a delay so that Shiny get the input value when the collapse animation
      // is finished.
      var target = e.currentTarget;
      setTimeout(function(e = target) {
        // apply correct translation rate depending on current state
        var contacts = $(e)
          .closest(".direct-chat")
          .find(".direct-chat-contacts");
        if (self.getValue(el)) {
          $(contacts).css({
            transform: `translate(${openTranslationRate}%, 0)`
          });
        } else {
          $(contacts).css({
            transform: `translate(${closeTranslationRate}%, 0)`
          });
        }
        callback();
      }, 10);
    });
  },

  unsubscribe: function(el) {
    $(el).off(".cardSidebarBinding");
  }
});

Shiny.inputBindings.register(cardSidebarBinding);

$(function() {
  
  // required to show a toast when the controlbar is pinned 
  // for the first time. Show once since it may be annoying ...
  var showToast = true;
  const controlbarToast = () => {
    if (showToast) {
      $(document).Toasts('create', {
        title: 'Controlbar is pinned',
        close: false,
        autohide: true,
        delay: 2000
      });
      showToast = false; 
    }
  };

  // This prevent box content from going outside their container 
  // when the control-bar is on push mode
  $("#controlbar-toggle").on("click",
    function() {
      if ($("body").hasClass("control-sidebar-push-slide")) {
        $(window).trigger("resize"); 
      }
  });
  
  
  // The code below hande the click out of the right control bar
  $(window).click(function(e) { 
    // There is a potential conflict. This function detect any click outside
    // the controlbar and close if if it is not pinned. Yet, if we click on an action       // button controlling the controlbar state (see updatebs4Controlbar), it is also outside the controlbar so the toggle event will be triggered twice. The controlbar will never close as shown in https://github.com/RinteRface/bs4Dash/issues/110. Below we make sure to leave the function as soon as a click on a button holding the class action button. This is not really a fix but a reasonable workaround.
    var isActionButton = $(e.target).hasClass("action-button");
    if (isActionButton) return null;
      
    if($("aside.control-sidebar").find(e.target).length === 0) {
      var pinned = $(".control-sidebar").attr("data-pin");
      if (pinned === "false" || pinned === undefined) {
        $("body").removeClass("control-sidebar-slide-open");  
        // don't forget to refresh the input binding
        $("#controlbar-toggle").trigger('collapsed.lte.controlsidebar');
      }
    }  
  });
  
  // handle the pin button: toggle data-pin state
  $("#controlbarPin").on('click', function() {
    var $pinIcon = $(this).children();
    $pinIcon.toggleClass("fa-rotate-90 fa-lg");
    
    $(".control-sidebar").attr("data-pin",
       $(".control-sidebar").attr("data-pin") == "false" ? "true" : "false");
    // toggle right sidebar control depending on the datapin
    if ($(".control-sidebar").attr("data-pin") === "true") {
      $pinIcon.css("color", "#007bff");
      $("#controlbar-toggle").addClass("disabled");
      controlbarToast();
    } else {
      $("#controlbar-toggle").removeClass("disabled");
      $pinIcon.css("color", "");
    }
  });


var init = true;

  // Input binding
  var controlbarBinding = new Shiny.InputBinding();
  
  $.extend(controlbarBinding, {
  
    find: function(scope) {
      return $(scope).find(".control-sidebar");
    },
  
    // Given the DOM element for the input, return the value
    getValue: function(el) {
      // Handles the pin 
      var controlbarOpen = $("body").hasClass("control-sidebar-slide-open");
      var pinned = $(el).attr("data-pin") === "true";
      if (controlbarOpen && pinned && init) {
        $("#controlbar-toggle").addClass("disabled");
        $("#controlbarPin")
          .children()
          .css("color", "#007bff");
        controlbarToast();
        init = false;
      }
      
      // this handles the case where the controlbar is not collapsed at start
      var controlbarCollapsed = $(el).attr('data-collapsed');
      if (controlbarCollapsed === "false") {
        $("#controlbar-toggle").ControlSidebar('toggle');
        $(el).attr('data-collapsed', "true");
        return true;
      } else {
        return $("body").hasClass("control-sidebar-slide-open");
      }
    },
    // see updatebs4Controlbar
    receiveMessage: function(el, data) {
      $("#controlbar-toggle").ControlSidebar('toggle');
    },
  
    subscribe: function(el, callback) {
      $("#controlbar-toggle").on("collapsed.lte.controlsidebar expanded.lte.controlsidebar", function(e) {
        $(el).trigger('shown');
        // add a delay so that Shiny get the input value 
        // after the AdminLTE3 animation is finished!
        setTimeout(
          function() {
            callback();
          }, 10);
      });
    },
  
    unsubscribe: function(el) {
      $(el).off(".controlbarBinding");
    }
  });
  
  Shiny.inputBindings.register(controlbarBinding, "bs4Dash.controlbarBinding");
  
  // handle controlbar overlay
  var controlbarOverlay = $('.control-sidebar').attr('data-overlay');
  if (controlbarOverlay === "false") {
    $('body').addClass('control-sidebar-push-slide');
  }

});
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

    closeButton = '';

    if (config.closable) {
      closeButton = '<button type="button" class="close" data-dismiss="alert" aria-hidden="true">x</button>'
    }

    titleTag = `<h5><i class="icon fa fa-${iconType}"></i>${config.title}</h5>`
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
// When document is ready, if there is a sidebar menu with no activated tabs,
// activate the one specified by `data-start-selected`, or if that's not
// present, the first one.
var ensureActivatedTab = function() {
  // get the selected tabs
  var $tablinks = $(".sidebar-menu a[data-toggle='tab']");

  // If there are no tabs, $startTab.length will be 0.
  var $startTab = $tablinks.filter("[data-start-selected='1']");
  if ($startTab.length === 0) {
    // If no tab starts selected, use the first one, if present
    $startTab = $tablinks.first();
  }

  // If there's a `data-start-selected` attribute and we can find a tab with
  // that name, activate it.
  if ($startTab.length !== 0) {
    // This is just in case the user renders the tabs in a renderUI that does not
    // print immediately in the DOM. We need a bit of a delay before telling which
    // tab to show ...
    if ($(".sidebar-menu").hasClass("bs4Dash-menu-output")) {
      setTimeout(function() {
        // we need to initialize any treeview elements that were not inserted
        // in the DOM when adminlte was first initialized!
        adminlte.Treeview._jQueryInterface.call($('[data-widget="treeview"]'), 'init');

        $startTab.tab("show");
      }, 10);
    } else {
      $startTab.tab("show");
    }
    

    // This is indirectly setting the value of the Shiny input by setting
    // an attribute on the html element it is bound to. We cannot use the
    // inputBinding's setValue() method here because this is called too
    // early (before Shiny has fully initialized)
    $(".sidebarMenuSelectedTabItem").attr(
      "data-value",
      $startTab.attr("data-value")
    );
  }
};

// This function handles a special case in the AdminLTE sidebar: when there
// is a sidebar-menu with items, and one of those items has sub-items, and
// they are used for tab navigation. Normally, if one of the items is
// selected and then a sub-item is clicked, both the item and sub-item will
// retain the "active" class, so they will both be highlighted. This happens
// because they're not designed to be used together for tab panels. This
// code ensures that only one item will have the "active" class.
var deactivateOtherTabs = function() {
  // Find all tab links under sidebar-menu even if they don't have a
  // tabName (which is why the second selector is necessary)
  var $tablinks = $(
    ".sidebar-menu a[data-toggle='tab']," + ".sidebar-menu li.has-treeview > a"
  );

  // If any other items are active, deactivate them
  $tablinks.not($(this)).removeClass("active");

  // also manually activate the parent link when the selected item
  // is part of a treeview. For some reason, this is not done by AdminLTE3...
  if ($(this).hasClass("treeview-link")) {
    $(this)
      .parents(".has-treeview")
      .children()
      .eq(0)
      .addClass("active");
  }

  // Trigger event for the tabItemInputBinding
  var $obj = $(".sidebarMenuSelectedTabItem");
  var inputBinding = $obj.data("shiny-input-binding");
  if (typeof inputBinding !== "undefined") {
    inputBinding.setValue($obj, $(this).attr("data-value"));
    $obj.trigger("change");
  }
};

$(function() {
  // Whenever the sidebar finishes a transition (which it does every time it
  // changes from collapsed to expanded and vice versa), trigger resize,
  // so that all outputs are resized.
  $(".main-sidebar").on(
    "webkitTransitionEnd otransitionend oTransitionEnd msTransitionEnd transitionend",
    function() {
      $(window).trigger("resize");
    }
  );

  $(document).on(
    "shown.bs.tab",
    '.sidebar-menu a[data-toggle="tab"]',
    deactivateOtherTabs
  );

  ensureActivatedTab();

  // Whenever we expand a menuItem (to be expandable, it must have children),
  // update the value for the expandedItem's input binding (this is the
  // tabName of the fist subMenuItem inside the menuItem that is currently
  // expanded)
  $(document).on("click", ".has-treeview", function() {
    var $menu = $(this);
    // If this menuItem was already open, then clicking on it again,
    // should trigger the "hidden" event, so Shiny doesn't worry about
    // it while it's hidden (and vice versa).
    if ($menu.hasClass("menu-open")) $menu.trigger("collapsed.lte.treeview");
    else if ($menu.hasClass(".has-treeview"))
      $menu.trigger("expanded.lte.treeview");

    // need to set timeout to account for the slideUp/slideDown animation
    var $obj = $(".sidebar.shiny-bound-input");
    setTimeout(function() {
      $obj.trigger("change");
    }, 600);
  });

  //---------------------------------------------------------------------
  // tabItemInputBinding
  // ------------------------------------------------------------------
  // Based on Shiny.tabItemInputBinding, but customized for tabItems in
  // bs4Dash, which have a slightly different structure.
  var tabItemInputBinding = new Shiny.InputBinding();
  $.extend(tabItemInputBinding, {
    find: function(scope) {
      return $(scope).find(".sidebarMenuSelectedTabItem");
    },
    getValue: function(el) {
      var value = $(el).attr("data-value");
      if (value === "null") return null;
      return value;
    },
    setValue: function(el, value) {
      var self = this;
      var anchors = $(el)
        .parent(".sidebar-menu")
        .find("li:not(.treeview)")
        .children("a");
      anchors.each(function() {
        // eslint-disable-line consistent-return
        if (self._getTabName($(this)) === value) {
          $(this).tab("show");
          // this make sure that treeview items are open when we
          // use the updatebs4TabItems function on the server side
          if ($(this).hasClass("treeview-link")) {
            if (
              !$(this)
                .parents(".has-treeview")
                .hasClass("menu-open")
            ) {
              $(this)
                .parents(".has-treeview")
                .children()
                .eq(0)
                .trigger("click");
            }
          }
          $(el).attr("data-value", self._getTabName($(this)));
          return false;
        }
      });
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);
    },
    subscribe: function(el, callback) {
      // This event is triggered by deactivateOtherTabs, which is triggered by
      // shown. The deactivation of other tabs must occur before Shiny gets the
      // input value.
      $(el).on("change.tabItemInputBinding", function() {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off(".tabItemInputBinding");
    },
    _getTabName: function(anchor) {
      return anchor.attr("data-value");
    }
  });

  Shiny.inputBindings.register(tabItemInputBinding, "bs4Dash.tabItemInput");

  //---------------------------------------------------------------------
  // sidebarInputBinding
  // ------------------------------------------------------------------
  // similar to controlbarInputBinding
  var sidebarBinding = new Shiny.InputBinding();

  $.extend(sidebarBinding, {
    find: function(scope) {
      return $(scope).find(".main-sidebar");
    },

    // Given the DOM element for the input, return the value
    getValue: function(el) {
      // Warning: we can't look for sidebar-open since this
      // class is only generated on mobile devices
      return !$("body").hasClass("sidebar-collapse");
    },

    // see updatebs4Controlbar
    receiveMessage: function(el, data) {
      $("[data-widget='pushmenu']").PushMenu("toggle");
    },

    subscribe: function(el, callback) {
      $("[data-widget='pushmenu']").on(
        "collapsed.lte.pushmenu.sidebarBinding shown.lte.pushmenu.sidebarBinding",
        function(e) {
          callback();
        }
      );
    },

    unsubscribe: function(el) {
      $(el).off(".sidebarBinding");
    }
  });

  Shiny.inputBindings.register(sidebarBinding, "bs4Dash.sidebarInput");

  // sidebarmenuExpandedInputBinding
  // ------------------------------------------------------------------
  // This keeps tracks of what menuItem (if any) is expanded
  var sidebarmenuExpandedInputBinding = new Shiny.InputBinding();
  $.extend(sidebarmenuExpandedInputBinding, {
    find: function(scope) {
      // This will also have id="sidebarItemExpanded"
      return $(scope).find(".sidebar");
    },
    getValue: function(el) {
      var $open = $(el)
        .find("li")
        .filter(".menu-open")
        .find("ul");
      if ($open.length === 1) return $open.attr("data-expanded");
      else return null;
    },
    setValue: function(el, value) {
      // does not work (nothing is printed)
      var $menuItem = $(el).find("[data-expanded='" + value + "']");
      // This will trigger actions defined by AdminLTE, as well as actions
      // defined in sidebar.js.
      $menuItem.prev().trigger("click");
    },
    subscribe: function(el, callback) {
      $(el).on("change.sidebarmenuExpandedInputBinding", function() {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off(".sidebarmenuExpandedInputBinding");
    }
  });
  Shiny.inputBindings.register(
    sidebarmenuExpandedInputBinding,
    "bs4Dash.sidebarmenuExpandedInputBinding"
  );

  // handle fixed sidebar
  if ($(".main-sidebar").attr("data-fixed") === "true") {
    $("body").addClass("layout-fixed");
    //$('body').Layout('fixLayoutHeight');
  }
});

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
  if ($('body').attr('data-help') == 2 || 
    $('body').attr('data-help') == 1) {
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
      $(document).on('shiny:connected', function() {
        if ($('body').attr('data-help') == 2 || $('body').attr('data-help') == 1) {
          $help_switch_checkbox.click(); 
          // Click again if option is set to FALSE
          if ($('body').attr('data-help') == 1) {
            $help_switch_checkbox.click(); 
          }
        }
      }); 
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
  
  /**
  * Update icon color style based on navbar color.
  *
  * @param String color Current navbar color.
  * @returns void
  */
  updateNavbarIconColor = function(color) {
    var iconThemeColor = navbar_dark_skins.indexOf(color) > -1 ? "white" : "rgba(0,0,0,.5)";
    $(".dark-theme-icon").css("color", iconThemeColor);
    $('[for="help_switch"] i').css("color", iconThemeColor);
  };
  
  // automatic global theme switcher
  if ($('body').attr('data-dark') == 2 || 
  $('body').attr('data-dark') == 1) {
    var $dark_mode_checkbox = $('<input />', {
      type: 'checkbox',
      id: 'theme_switch',
      class: 'custom-control-input'
    }).on('click', function () {

      // get any selected navbar skin in the navbar themer
      var newNavbarColor;
      // If there is not themer, we keep the navbar current color.
      // Otherwise, we replace it by the new color.
      if ($('.navbar-themer-chip').length > 0) {
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
      } else {
        newNavbarColor = getNavbarColor();
      }

      if ($(this).is(':checked')) {
        $('body').addClass('dark-mode');

        // use updateNavbarTheme to correctly setup the skin as depending
        // on the required color. If no color is chosen, we use gray-dark for dark mode
        if (newNavbarColor === undefined || newNavbarColor === 'navbar-white') {
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
        if (newNavbarColor === undefined || newNavbarColor === 'navbar-gray-dark') {
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
      
      // update navbar icon colors
      updateNavbarIconColor(newNavbarColor);
    });

    var $dark_mode_icon = $('body').hasClass('dark-mode') ? '<i class="dark-theme-icon fa fa-moon"></i>' : '<i class="dark-theme-icon fa fa-sun"></i>';
    var $dark_mode_container = $('<div />', { class: 'custom-control custom-switch mx-2 mt-2' }).append($dark_mode_checkbox).append(`<label class="custom-control-label" for="theme_switch">${$dark_mode_icon}</label>`);
    
    // insert before $('#controlbar-toggle') whenever possible ...
    if ($('.nav-item #controlbar-toggle')) {
      $dark_mode_container.insertBefore($('#controlbar-toggle').parent());
    } else {
      $navbar.append($dark_mode_container);
    }
    
    
    // Trigger dark mode
    if ($('body').attr('data-dark') == 2) {
      $(document).on('shiny:connected', function() {
        $dark_mode_checkbox.click();
      }); 
    }
  }
  
  // apply correct navbar class depending on selected color
  if (getNavbarColor() !== undefined) {
    updateNavbarTheme(getNavbarColor());
    updateNavbarIconColor(getNavbarColor());
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
$(function () {
  // hide the right sidebar toggle 
  // if no right sidebar is specified
  noControlbar = ($(".control-sidebar").length === 0);
  if (noControlbar) {
    $("#controlbar-toggle").hide();
  }

  // hide the right sidebar toggle if the controlbar is disable
  disableControlbar = ($(".control-sidebar").attr("data-show"));
  if (!disableControlbar) {
    $("#controlbar-toggle").hide();
  }

  // controlbar slide
  controlbarSlide = ($(".control-sidebar").attr("data-slide"));
  if (controlbarSlide) {
    $("#controlbar-toggle").attr('data-controlsidebar-slide', controlbarSlide);
  }

  // when the sidebar is disabled, hide the sidebar toggle
  disableSidebar = ($(".main-sidebar").length === 0);
  if (disableSidebar) {
    $(".nav-item > a[data-widget='pushmenu']").css("visibility", "hidden");
  }

  // handle fixed navbar
  if ($(".navbar").attr("data-fixed") === "true") {
    $("body").addClass("layout-navbar-fixed");
  }

});
var menuOutputBinding = new Shiny.OutputBinding();
$.extend(menuOutputBinding, {
  find: function (scope) {
    return $(scope).find('.bs4Dash-menu-output');
  },
  onValueError: function (el, err) {
    Shiny.unbindAll(el);
    this.renderError(el, err);
  },
  renderValue: function (el, data) {
    Shiny.unbindAll(el);

    var html;
    var dependencies = [];
    if (data === null) {
      return;
    } else if (typeof (data) === 'string') {
      html = data;
    } else if (typeof (data) === 'object') {
      html = data.html;
      dependencies = data.deps;
    }

    var $html = $($.parseHTML(html));

    // Convert the inner contents to HTML, and pass to renderHtml
    Shiny.renderHtml($html.html(), el, dependencies);

    // Extract class of wrapper, and add them to the wrapper element
    el.className = 'bs4Dash-menu-output shiny-bound-output ' +
      $html.attr('class');
    
    // need this to activate adminLTE3 plugin for treeview  
    $(el)
      .attr("data-widget", "treeview")
      .attr("role", "menu")
      .attr("data-accordion", "true");

    Shiny.initializeInputs(el);
    Shiny.bindAll(el);
    if ($(el).hasClass("sidebar-menu")) ensureActivatedTab(); // eslint-disable-line
  }
});
Shiny.outputBindings.register(menuOutputBinding, "bs4Dash.menuOutputBinding");
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
var bootstrapTabInputBinding = new Shiny.InputBinding();
$.extend(bootstrapTabInputBinding, {
  find: function(scope) {
    return $(scope).find('ul.nav.shiny-tab-input');
  },
  getValue: function(el) {
    // for Bootstrap 4, li element does not hold the active class anymore!
    // We need to look at a.active.
    var anchor = $(el).find('li:not(.dropdown)').children('a.active');
    if (anchor.length === 1)
      return this._getTabName(anchor);

    return null;
  },
  setValue: function(el, value) {
    let self = this;
    let success = false;
    if (value) {
      let anchors = $(el).find('li:not(.dropdown)').children('a');
      anchors.each(function() {
        if (self._getTabName($(this)) === value) {
          $(this).tab('show');
          success = true;
          return false; // Break out of each()
        }
        return true;
      });
    }
    if (!success) {
      // This is to handle the case where nothing is selected, e.g. the last tab
      // was removed using removeTab.
      $(el).trigger("change");
    }
  },
  getState: function(el) {
    return { value: this.getValue(el) };
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);
  },
  subscribe: function(el, callback) {
    $(el).on('change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding', function(event) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.bootstrapTabInputBinding');
  },
  _getTabName: function(anchor) {
    return anchor.attr('data-value') || anchor.text();
  }
});

Shiny.inputBindings.register(bootstrapTabInputBinding, 'shiny.bootstrapTabInput');
// This code creates acustom handler for userMessages
Shiny.addCustomMessageHandler("user-messages", function(message) {
  var id = message.id, action = message.action, content = message.body, index = message.index;
  
  // message text
  // We use Shiny.renderHtml to handle the case where the user pass input/outputs in the updated content that require a new dependency not available in the 
  // page at startup. 
  if (content.hasOwnProperty("text")) {
    var text;
    if (content.text.html === undefined) {
      text = content.text;
    } else {
      text = Shiny.renderHtml(content.text.html, $([]), content.text.dependencies).html;
    } 
  }
  
  // unbind all
  Shiny.unbindAll();
  
  if (action === "remove") {
    $("#" + id).find(".direct-chat-msg").eq(index - 1).remove();
  } else if (action === "add") {
    var author = content.author, date = content.date, image = content.image, type = content.type;
    
    // build the new message 
    var authorWrapper, dateWrapper;
    if (type === "sent") {
      authorWrapper = '<span class="direct-chat-name float-right">' + author + '</span>';
      dateWrapper = '<span class="direct-chat-timestamp float-left">' + date + '</span>';
    } else {
      authorWrapper = '<span class="direct-chat-name float-left">' + author + '</span>';
      dateWrapper = '<span class="direct-chat-timestamp float-right">' + date + '</span>';
    }

    var newMessage = `<div class="direct-chat-infos clearfix">${authorWrapper}${dateWrapper}</div><img class="direct-chat-img" src="${image}"/><div class="direct-chat-text">${text}</div>`;
        
    // build wrapper
    var newMessageWrapper;
    if (type === "sent") {
      newMessageWrapper = '<div class="direct-chat-msg right">' + newMessage + '</div>';
    } else {
      newMessageWrapper = '<div class="direct-chat-msg">' + newMessage + '</div>';
    }
      
    // append message
    $("#" + id).find(".direct-chat-messages").append(newMessageWrapper);
  } else if (action === "update") {
      
    // today's date
    var d = new Date();
    var month = d.getMonth() + 1;
    var day = d.getDate();
    var today = d.getFullYear() + '/' +
      ((''+month).length<2 ? '0' : '') + month + '/' +
      ((''+day).length<2 ? '0' : '') + day;
      
    // we assume only text may be updated. Does not make sense to modify author/date
    
    $("#" + id)
      .find(".direct-chat-text")
      .eq(index - 1)
      .replaceWith('<div class="direct-chat-text"><small class="text-red">(modified: ' + today +')</small><br>' +  text + '</div>');
  }
    
  // Calls .initialize() for all of the input objects in all input bindings,
  // in the given scope (document)
  Shiny.initializeInputs();
  Shiny.bindAll(); // bind all inputs/outputs
});
