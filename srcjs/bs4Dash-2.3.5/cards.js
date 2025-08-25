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

