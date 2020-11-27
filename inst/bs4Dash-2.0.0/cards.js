// Input binding
var cardBinding = new Shiny.InputBinding();

$.extend(cardBinding, {

  find: function (scope) {
    return $(scope).find(".card");
  },

  // Given the DOM element for the input, return the value
  getValue: function (el) {
    var config = $(el).parent().find("script[data-for='" + el.id + "']");
    config = JSON.parse(config.html());

    var isCollapsed = $(el).hasClass('collapsed-card');
    var display = $(el).css('display');
    var isMaximized = $(el).hasClass('maximized-card');

    var visible;
    if (display === "none") {
      visible = false;
    } else {
      visible = true;
    }

    // toggle collapse button when maximized
    if (isMaximized) {
      $(el).find("[data-card-widget = 'collapse']").hide();
    } else {
      $(el).find("[data-card-widget = 'collapse']").show();
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
  _updateWidth: function (el, o, n) {
    $(el).parent().toggleClass("col-sm-" + o);
    $(el).parent().addClass("col-sm-" + n);
    // trigger resize so that output resize
    $(el).trigger('resize');
  },

  setValue: function (el, value) {

    var config = $(el).parent().find("script[data-for='" + el.id + "']");
    config = JSON.parse(config.html());

    if (value.action === "update") {
      var isUserCard = $(el).hasClass('user-card');
      var isSocialCard = $(el).hasClass('social-card');
      // To remove status explicitly set status = NULL in updateBox
      if (value.options.hasOwnProperty("status")) {
        if (value.options.status !== config.status) {
          // don't touch if null
          if (config.status !== null) {
            $(el).toggleClass("card-" + config.status);
          }
          if (value.options.status !== null) {
            $(el).addClass("card-" + value.options.status);
          }
          config.status = value.options.status;
        }
      }
      if (value.options.hasOwnProperty("solidHeader")) {
        // only update if config an new value are different
        if (value.options.solidHeader !== config.solidHeader) {
          $(el).toggleClass("card-outline");
          config.solidHeader = value.options.solidHeader;
        }
      }
      // To remove background explicitly set background = NULL in updateBox
      if (value.options.hasOwnProperty("background")) {
        if (value.options.background !== config.background) {
          var newBoxClass;
          if (config.gradient) {
            newBoxClass = "bg-gradient-";
          } else {
            newBoxClass = "bg-";
          }
          // don't touch if null
          if (config.background !== null) {
            // if gradient, the class has a gradient at the end!
            newBoxClass = newBoxClass + config.background;
            // handle userBox
            // for which we also have to toggle the header bg color
            // and the box tools buttons color
            if (isUserCard) {
              var header = $(el).find('.widget-user-header');
              $(header).toggleClass(newBoxClass);
            }
            $(el).toggleClass(newBoxClass);
            $(el).find('.btn-tool').toggleClass("btn-" + config.background);
          }
          if (value.options.background !== null) {
            newBoxClass = newBoxClass + value.options.background;
            if (isUserCard) {
              var header = $(el).find('.widget-user-header');
              $(header).addClass(newBoxClass);
            }
            $(el).addClass(newBoxClass);
            $(el).find('.btn-tool').toggleClass("btn-" + value.options.background);
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
            $(el).find(".card-body").css("height", '');
          } else {
            $(el).find(".card-body").css("height", value.options.height);
          }

          config.height = value.options.height;
          // don't need to trigger resize since the output height
          // is not controlled by the box size ...
        }
      }
      if (value.options.hasOwnProperty("collapsible")) {
        if (value.options.collapsible !== config.collapsible) {
          if (!value.options.collapsible) {
            $(el).find('[data-card-widget = "collapse"]').remove();
            config.collapsible = false;
          } else {
            // only add if no collapsible
            if ($(el).find('[data-card-widget = "collapse"]').length === 0) {
              $(el)
                .find(".card-tools.pull-right")
                .prepend($('<button class="btn btn-tool" data-card-widget="collapse"><i class="fa fa-minus"></i></button>'));
              config.collapsible = true;
            }
          }
        }
      }
      if (value.options.hasOwnProperty("closable")) {
        if (value.options.closable !== config.closable) {
          if (!value.options.closable) {
            $(el).find('[data-card-widget = "remove"]').remove();
            config.closable = false;
          } else {
            if ($(el).find('[data-card-widget = "remove"]').length === 0) {
              $(el)
                .find(".card-tools.pull-right")
                .append($('<button class="btn btn-tool" data-card-widget="remove"><i class="fa fa-times"></i></button>'));
              config.closable = true;
            }
          }
        }
      }

      if (value.options.hasOwnProperty("maximizable")) {
        if (value.options.maximizable !== config.maximizable) {
          if (!value.options.maximizable) {
            $(el).find('[data-card-widget = "maximize"]').remove();
            config.maximizable = false;
          } else {
            if ($(el).find('[data-card-widget = "maximize"]').length === 0) {
              $(el)
                .find(".card-tools.pull-right")
                .append($('<button class="btn btn-tool" data-card-widget="maximize"><i class="fa fa-expand"></i></button>'));
              config.maximizable = true;
            }
          }
        }
      }

      // handle HTML tags (harder)
      if (value.options.hasOwnProperty("title")) {
        if (value.options.title !== config.title) {
          var newTitle = $.parseHTML(value.options.title);
          // social box
          if (isSocialCard) {
            $(el).find(".user-block").replaceWith($(newTitle));
          } else if (isUserCard) {
            var tools = $(el).find('.card-tools');
            // handle 2 cards types
            if (newTitle.length === 3) {
              // don't take newTitle[1] (contains some text)
              newTitle = [newTitle[0], newTitle[2]];
              // change widget-use class 
              $(el)
                .removeClass('widget-user-2')
                .addClass('widget-user');
              // insert header and image after
              $(el).find('.widget-user-header').replaceWith($(newTitle[0]));
              $(newTitle[1]).insertAfter($(el).find('.widget-user-header'));

            } else {
              $(el)
                .removeClass('widget-user')
                .addClass('widget-user-2');
              $(el).find('.widget-user-header').replaceWith($(newTitle));
              if (value.options.status !== null) {
                if (value.options.gradient) {
                  $(el).find('.widget-user-header').addClass('bg-gradient-', status);
                } else {
                  $(el).find('.widget-user-header').addClass('bg-', status);
                }
              }
            }
            // add tools as first child of widget-user-header
            $(el).find('.widget-user-header').prepend($(tools));
          } else {
            $(newTitle).addClass("card-title");
            $(el).find("h3").replaceWith($(newTitle));
          }
        }
      }

      // replace the old JSON config by the new one to update the input value 
      $(el).parent().find("script[data-for='" + el.id + "']").replaceWith(
        '<script type="application/json" data-for="' + el.id + '">' + JSON.stringify(config) + '</script>'
      );
    } else {
      if (value != "restore") {
        if ($(el).css('display') != 'none') {
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
  receiveMessage: function (el, data) {
    this.setValue(el, data);
    $(el).trigger('change');
  },

  subscribe: function (el, callback) {
    $(el).on('expanded.lte.cardwidget collapsed.lte.cardwidget', function (e) {
      // set a delay so that SHiny get the input value when the collapse animation
      // is finished. 
      setTimeout(
        function () {
          callback();
        }, 500);
    });

    $(el).on('maximized.lte.cardwidget minimized.lte.cardwidget', function (e) {
      callback();
    });

    $(el).on('removed.lte.cardwidget', function (e) {
      setTimeout(
        function () {
          callback();
        }, 500);
    });
    // we need to split removed and shown event since shown is immediate whereas close
    // takes some time
    $(el).on('shown.cardBinding', function (e) {
      callback();
    });

    // handle change event triggered in the setValue method 
    $(el).on('change.cardBinding', function (event) {
      setTimeout(function () {
        callback();
      }, 500);
    });
  },

  unsubscribe: function (el) {
    $(el).off(".cardBinding");
  }
});

Shiny.inputBindings.register(cardBinding);



// Card sidebar input binding
var cardSidebarBinding = new Shiny.InputBinding();
$.extend(cardSidebarBinding, {
  
  initialize: function(el) {
    // erase default to avoid seeing moving sidebars on initialization
  $('.direct-chat-contacts, .direct-chat-messages').css({'transition': 'transform .0s ease-in-out'});
  
  var background = $(el).attr('data-background') ? $(el).attr('data-background') : '#343a40';
    var width = $(el).attr('data-width') ? parseInt($(el).attr('data-width')) : 100;
    var closeTranslationRate =  100 * 100 / width;
    var contacts = $(el).closest('.direct-chat').find('.direct-chat-contacts');
    
    // apply width and background
    $(contacts).css({
      'background': `${background}`,
      'width': `${width}%`
    });
    
    // If start open, apply openTranslationRate else apply closeTranslationRate ...
    if ($(el).attr('data-start-open') === "true") {
      var openTranslationRate = closeTranslationRate - 100;
      $(contacts).css({'transform': `translate(${openTranslationRate}%, 0)`});
    } else {
      $(contacts).css({'transform': `translate(${closeTranslationRate}%, 0)`});
    }
  
    // Restore for better transitions
    setTimeout(function() {
      $('.direct-chat-contacts, .direct-chat-messages').css({'transition': 'transform .5s ease-in-out'});
    }, 300);
  },
  
  find: function (scope) {
    return $(scope).find('[data-widget="chat-pane-toggle"]');
  },

  // Given the DOM element for the input, return the value
  getValue: function (el) {
    var cardWrapper = $(el).closest(".card");
    return $(cardWrapper).hasClass("direct-chat-contacts-open");
  },

  // see updatebs4Card
  receiveMessage: function (el, data) {
    // In theory, adminLTE3 has a builtin function
    // we could use $(el).DirectChat('toggle');
    // However, it does not update the related input.
    // The toggled.lte.directchat event seems to be broken.
    $(el).trigger('click');
    $(el).trigger("shown");
  },

  subscribe: function (el, callback) {
    var self = this;
    $(el).on('click', function (e) {
      var width = $(el).attr('data-width') ? parseInt($(el).attr('data-width')) : 100;
      var closeTranslationRate =  100 * 100 / width;
      var openTranslationRate = closeTranslationRate - 100;
      // set a delay so that Shiny get the input value when the collapse animation
      // is finished. 
      var target = e.currentTarget
      setTimeout(
        function (e = target) {
          // apply correct translation rate depending on current state
          var contacts = $(e).closest('.direct-chat').find('.direct-chat-contacts');
          if (self.getValue(el)) {
            $(contacts).css({'transform': `translate(${openTranslationRate}%, 0)`});
          } else {
            $(contacts).css({'transform': `translate(${closeTranslationRate}%, 0)`});
          }
          callback();
        }, 10);
    });
  },

  unsubscribe: function (el) {
    $(el).off(".cardSidebarBinding");
  }
});

Shiny.inputBindings.register(cardSidebarBinding);
