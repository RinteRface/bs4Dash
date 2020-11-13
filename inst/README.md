# Notes for updating adminLTE

Here are some advises to make adminLTE3 working better with shiny.

## Tweaks to fix dropdown link color issue in dark, info, danger, success and primary modes

For instance, for primary cards, we have:

```css
.card-primary:not(.card-outline) .card-header,		 
.card-primary:not(.card-outline) .card-header a { 
   color: #ffffff;
 }
```

In the bs4Dash.css we have:

```css
.dropdown-item { 
   color: #000 !important; 
}
```

## User post bottom border issue

In the bs4Dash.css we have:

```css
.post {
  border-bottom: none !important;
}
```

This feature does not play well with the collapse option and has been removed.

## Tweaks to support shinyFiles
Since shinyFiles is based on Boostrap 3, one may expect conflicts. Here
is a css tweak to apply to fix modals:

```css
/* Remove for shinyFiles support
.modal.fade .modal-dialog {
  transition: -webkit-transform 0.3s ease-out;
  transition: transform 0.3s ease-out;
  transition: transform 0.3s ease-out, -webkit-transform 0.3s ease-out;
  -webkit-transform: translate(0, -50px);
  transform: translate(0, -50px);
}
*/
```

```css
.modal-header {
  /* Remove for shinyFiles support */
  /*display: -ms-flexbox;
  display: flex;
  -ms-flex-align: start;
  align-items: flex-start;
  -ms-flex-pack: justify;
  justify-content: space-between; */
  padding: 1rem;
  border-bottom: 1px solid #e9ecef;
  border-top-left-radius: 0.3rem;
  border-top-right-radius: 0.3rem;
}
```

## Tell shiny to show the content on expand
When a card start collapsed, we need to tell shiny to show the content
when expanded. Add `_this._parent.trigger("hidden.bs.collapse");` and `_this2._parent.trigger("shown.bs.collapse");` to collapse and expand prototypes,
respectively in adminlte.js:

```javascript
_proto.collapse = function collapse() {
        var _this = this;

        this._parent.children(Selector.CARD_BODY + ", " + Selector.CARD_FOOTER).slideUp(this._settings.animationSpeed, function () {
          _this._parent.addClass(ClassName.COLLAPSED);
          _this._parent.trigger("hidden.bs.collapse");
        });

        this._parent.find(this._settings.collapseTrigger + ' .' + this._settings.collapseIcon).addClass(this._settings.expandIcon).removeClass(this._settings.collapseIcon);

        var collapsed = $.Event(Event.COLLAPSED);

        this._element.trigger(collapsed, this._parent);
      };
```

```javascript
_proto.expand = function expand() {
        var _this2 = this;

        this._parent.children(Selector.CARD_BODY + ", " + Selector.CARD_FOOTER).slideDown(this._settings.animationSpeed, function () {
          _this2._parent.trigger("shown.bs.collapse");
          _this2._parent.removeClass(ClassName.COLLAPSED);
        });

        this._parent.find(this._settings.collapseTrigger + ' .' + this._settings.expandIcon).addClass(this._settings.collapseIcon).removeClass(this._settings.expandIcon);

        var expanded = $.Event(Event.EXPANDED);

        this._element.trigger(expanded, this._parent);
      };
```


## Tell shiny to resize the content of the box when click on maximize
AdminLTE3 introduces a nice feature called "maximize". However, shiny does not
know when the content needs to be resized after toggle maximize.
We need to add the following code `$(this).trigger('shown')` to each prototype in adminlte.js. 
Moreover, so that the input binding works well, we have to remove the delay function for
maximize and minimize events.

```javascript
_proto.maximize = function maximize() {
        this._parent.find(this._settings.maximizeTrigger + ' .' + this._settings.maximizeIcon).addClass(this._settings.minimizeIcon).removeClass(this._settings.maximizeIcon);

        this._parent.css({
          'height': this._parent.height(),
          'width': this._parent.width(),
          'transition': 'all .15s'
        }).delay(150).queue(function () {
          $(this).addClass(ClassName.MAXIMIZED);
          $('html').addClass(ClassName.MAXIMIZED);
          
          // tells shiny to resize the content
          $(this).trigger('shown');

          if ($(this).hasClass(ClassName.COLLAPSED)) {
            $(this).addClass(ClassName.WAS_COLLAPSED);
          }

          $(this).dequeue();
        });

        var maximized = $.Event(Event.MAXIMIZED);

        this._element.trigger(maximized, this._parent);
      };
```

```javascript
_proto.minimize = function minimize() {
        this._parent.find(this._settings.maximizeTrigger + ' .' + this._settings.minimizeIcon).addClass(this._settings.maximizeIcon).removeClass(this._settings.minimizeIcon);

        this._parent.css('cssText', 'height:' + this._parent[0].style.height + ' !important;' + 'width:' + this._parent[0].style.width + ' !important; transition: all .15s;').delay(10).queue(function () {
          $(this).removeClass(ClassName.MAXIMIZED);
          $('html').removeClass(ClassName.MAXIMIZED);
          $(this).css({
            'height': 'inherit',
            'width': 'inherit'
          });
          
          // tells shiny to resize the content
          $(this).trigger('shown');

          if ($(this).hasClass(ClassName.WAS_COLLAPSED)) {
            $(this).removeClass(ClassName.WAS_COLLAPSED);
          }

          $(this).dequeue();
        });

        var MINIMIZED = $.Event(Event.MINIMIZED);

        this._element.trigger(MINIMIZED, this._parent);
      };
```