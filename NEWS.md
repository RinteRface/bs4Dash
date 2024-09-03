# bs4Dash 2.3.4

- Added shinylive to support documentation examples.

## Minor change
- Update jquery-ui to 1.13.2. Thanks @biognosys-so.
- Fix CRAN NOTES.

# bs4Dash 2.3.3

## Breaking change (potential)

- Fix #302: both `dashboardSidebar()` and `dashboardControlbar()` default __skin__ value is NULL.
This allows them to inherit from the parent `dashboardPage()` __dark__ parameter and have either
a full light or full dark skin. Therefore, it won't be possible anymore to apply a light sidebar background with the `dashboardPage()` when the main theme is dark and inversely. If you want to do so, you have to set `dark = NULL`, for instance:

```r
library(shiny)
library(bs4Dash)

shinyApp(
  ui = dashboardPage(
    dark = NULL,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "My dashboard",
        color = "primary",
        href = "https://adminlte.io/themes/v3",
        image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
      )
    ),
    sidebar = dashboardSidebar(skin = "dark"),
    body = dashboardBody(
      box(status = "danger"),
      box(status = "primary"),
      box(status = "orange")
    ),
    controlbar = dashboardControlbar(skin = "dark"),
    title = "DashboardPage"
  ),
  server = function(input, output) { }
)
```

## Bug fixes
- Remove unused `headTitles` parameter from `bs4Table()`.
- Fix #315: alert title is not added to alert body and if Alert is not closable the header contains "undefined" key word. Thanks @MohammedFCIS.

# bs4Dash 2.3.0

## New feature
New navbar navigation menu.
- `navbarTab()`, `navbarMenu()` and `updateNavbarTabs()` to create navbar navigation. 
This will close #108. 

## Minor change
- New `dropdownHeader()` function to display Bootstrap 4 dropdown headers. 
- Adjust `dropdownDivider()` as it was invisible.
- `tabItems()` has __.list__ parameter to pass `tabItem()` elements as list.
- Fix #342: better title alignment in `tabBox()`. Increase padding for card with pills. Thanks @HugoGit39
for reporting. 

## Bug fixes
- Fix #349: allow to pass list of `accordionItem()` with `.list` parameter in `accordion()`. Thanks @vladimirstroganov for reporting.
- Fix #330: allow to use input elements (or any not `menuItem` element) in the sidebar.
- Fix #343: Refine `help` parameter behavior in `dashboardPage()`. If NULL, no icon is shown. If FALSE, icon and toggle are shown but not checked. If TRUE the toggle is checked.

## Internal
- Change dark/light switch CSS class for consistency.

# bs4Dash 2.2.1

## New features
- New `pagination()` and `updatePagination()`. Bootstrap 4 implementation of
[pagination](https://getbootstrap.com/docs/4.0/components/pagination/).

## Minor change
- Fix #323: remove sidebar collapse animation on app startup. JS code moved
back to R. Thanks @lucas-alcantara for reporting.

## Bug fixes
- Fix #325: `menuItem()` applied wrong class to any nested element that is not
a `menuSubItem()`. Thanks @echoplaza for reporting. 
- Fix #322: `notificationItem()` href does not work. 
- Fix #306: Dynamic `menuSubItems()` with apply() adds some extra text. Added __.list__
param to `menuItem()` to programmatically pass `menuSubItem()` as list. 
- Fix #297: `tabsetPanel()` renders below the list of `tabPanels` when vertical = TRUE. 
Changed layout to `fluidRow`. Thanks @lucas-alcantara for reporting. 
- Fix #296: Documentation issue for `valueBox()` and `infoBox()` in the render function section.
Thanks @corderoortiz for reporting.
- Fix #293: Navbar stays keeps white background, even when dark mode is selected/toggled. Related to a previous PR. Thanks to @JJohnson-DA for reporting.
- Fix #290: don't set `data-toggle="tab"` when `href` is not NULL in `tabsetPanel()`. Thanks @veer0318 for reporting.
- Fix #243: `tabsetPanel()` id's not properly generated when inserting tabs to non-empty tabset. Thanks @DarkSideOfTheMat for reporting and giving hints.

# bs4Dash 2.1.0

## Breaking change
- Align with `{shiny}` and remove `position` from `tabsetPanel()`. This was long
time deprecated. 

## Minor changes/fixes
- Remove echarts4r suggest causing #240.
- Fix issue in `bs4DashGallery()` and `tabsetPanel()`. We now use `bs4Dash::tabsetPanel()`.
- Fix issue with navbar theme. #268. Thanks @etiennebacher.
- Allow `type = "hidden"` for `tabsetPanel`. Fixes #248.
- Fix regression introduced in 2.0.3: `renderMenu()` not working. See #234
- Update Github actions.
- Add bs4Dash class for card [binding](https://github.com/RinteRface/bs4Dash/commit/685d180a4e51f973f59ae35d47a8325018abf79d). Thanks @galachad.
- Typo in vignette. Thanks @Duque-de-Sealand.
- Wrong parameter in doc. Thanks @zoezhang106.
- Add new `.list` param to `tabBox()` to allow programmatically generated items, like
in `tabsetPanel()`. Thanks @mtopart


# bs4Dash 2.0.3
This release is a patch to fix an issue preventing the release of Shiny 1.7.0. 

## Minor changes
- Internal change regarding the `{waiter}` package but no user impact. Thanks @JohnCoene.


# bs4Dash 2.0.2

Patch to fix CRAN warning on Mac M1. 

## Minor changes

- Move away from `shiny:::processDeps` in favor of `htmltools::renderTags`. Does not
impact end user. 

# bs4Dash 2.0.1

## Minor changes
- Simple patch to fix wrong .Rbuildignore [rule](https://github.com/DivadNojnarg/outstanding-shiny-ui/issues/52).
- Update internal file structure. 
- Clean NOTE: https://cran.rstudio.com//web/checks/check_results_bs4Dash.html

## Bug Fix
- Fix #220: Disable badgeLabel in SidebarMenuItem when collapsed. Wrong tag structure fixed.
- Fix #219: icon appears after header in bs4Card/box.

# bs4Dash 2.0.0

## Breaking changes
This is to align with shinydashboard and shinydashboardPlus.
### bs4DashPage/dashboardPage
- `navbar` param becomes `header`.
- Remove `old_school`, `sidebar_mini`, `sidebar_collapsed`, `controlbar_collapsed`, 
  `controlbar_overlay`, `enable_preloader`, `loading_duration` and 
  `loading_background`. These parameters move to dashboardSidebar, controlbar ...
- New `freshTheme` parameter to pass `{fresh}` powered themes.
- New `options` parameter to configure AdminLTE3.
- New `preloader` to use `{waiter}` as a preloading engine.


### bs4DashFooter
- `...` becomes `left`.
- `right_text` becomes `right`.

### bs4DashNavbar/dashboardHeader
- `controlbarIcon` accepts `shiny::icon` instead of the icon name.
- `sidebarIcon` accepts `shiny::icon` instead of the icon name.
- add `title`, `titleWidth` and `disable`

### bs4UserMenu
- `src` becomes `image`.


### bs4DashControlbar/dashboardControlbar
- Add `collapsed` and `overlay`.
- Remove `title`.
- Rename `inputId` to `id`.


### updatebs4Controlbar
- `inputId` becomes `id`.
- New alias: `updateControlbar`.
- `session` is not mandatory


### bs4DashSidebar/dashboardSidebar/updateSidebar
- `inputId` moved to the end.
- `inputId` renamed to `id`.
- Added `collapsed` and `minified`.
- Add `width`.
- Remove `opacity`, `title`, `brandColor`, `url` and `src`.

### bs4SidebarMenu
- `child_indent` becomes `childIndent`.
- Add `.list` to pass programmatically generated `bs4SidebarMenuItem`.

### bs4SidebarMenuItem
- Add `href` and `newTab`.
- `icon` expects `shiny::icon`.


### bs4SidebarUserPanel
- `img` and `text` become `image` and `name`, respectively.


### bs4Card
- Change `solidHeader` behavior.
- Remove all sidebar related parameters from `bs4Card()`. This is now part of
 the new `bs4CardSidebar()`.
- Remove dropdownIcon parameter from `bs4Card()`. It is now part of the `dropdownItemList()`.
- Remove labelStatus, labelText and labelTooltip params from `bs4Card()`. This is to reduce the number of parameters of cards.
- By default, `closable` is now FALSE for `bs4Card()`.
- Rename `dropdownItemList` by `cardDropdown`
- Rename `dropdownItemListItem` by `cardDropdownItem`


### bs4CardSidebar
- `inputId` becomes `id`.
- `icon` expects `shiny::icon`.

### bs4TabCard
- Change `solidHeader` behavior. 


### bs4ValueBox
- `status` becomes `color`. 
- `icon` expects `shiny::icon`.

### bs4InfoBox
- `status` becomes color
- Remove `...` for `subtitle`
- Remove `iconStatus`.
- Add `href` and `fill`.
- Rename `gradientColor` to `gradient`.

### bs4UserCard
- `src` becomes `image`.
- `status` becomes `color`.

### bs4SocialBox
- rework component
- add `userBlock()` to pass on title.


### attachmentBlock
- `src` becomes `image`.
- `titleUrl` becomes `href`.

### descriptionBlock
- `icon` expects `shiny::icon`.


### bs4Box 
- This component has been removed


### bs4TabCard
- rework component. Like `tabBox()` from `{shinydashboard}`


### bs4DropdownMenu
- `badge` becomes `badgeStatus`
- Add `type`.
- icon expects `shiny::icon`.
- Replace `labelText` by `headerText`.
- Add `.list` and `href`.
- Remove `align`.

### bs4DropdownMenuItem
- This has been replaced by `notificationItem()` and `messageItem()`


### Others
- Reworked `bs4Table()` API.
- In `bs4Stars()`: `grade` and `maxstar`, respectively become `value` and `grade`.
- In `listGroupItem()`, remove `type` and put it in `listGroup()`. `src` becomes `href`.
- `status` becomes `color` in `blockQuote`.
- Remove `src` from `carouselItem()`
- In `bs4Ribbon()`, `status` becomes `color`. Remove `size`.
- In `bs4Badge()`, `status` becomes `color`.
- In `userMessage()`, `src` becomes `image` and `side` becomes `type`.
- In `timelineItemMedia()`, `src` becomes `image`.
- In `timelineItem()`, `timelineStart()` and `timelineEnd()`, `icon` expects `shiny::icon`.
- `status` becomes `color` in `timelineItem()`, `timelineLabel()`, `timelineStart()` and `timelineEnd()`.
- In `userPost()` and `userPostMedia()`, `src` becomes `image`. Removed `collapseStatus`.
- In `bs4Stars()`, status becomes `color`.
- Remove `bs4ShowTab()`, `bs4HideTab()` and `bs4RemoveTab()`. shiny vanilla function should work well now.
- Remove `bs4TabPanel()`.
- `bs4TabSetPanel()` becomes `bs4TabsetPanel()` and is built on top of `shiny::tabsetPanel`.
- In `cardProfile()`, add `bordered`. `src` becomes `image`. 
- Remove `cardProfileItemList()`.
- `updatebs4TabSetPanel()`: selected takes the name of the tab instead of its index. This is more convenient (same as in shinydashboard)
- value is now mandatory in `bs4InfoBox()`
- change `attachmentBlock()` title_url to titleUrl (to match with a new upcoming package...)
- `descriptionBlock()`: number_color, number_icon, right_border and margin_bottom become
camel case parms (numberColor, numberIcon, rightBorder and marginBottom)
- numberIcon in `descriptionBlock()` only need the name of the icon ('times') instead
- `bs4Jumbotron()`: btn_name becomes btnName
- `userPost()`: collapse_status becomes collapseStatus 

## New features
- Add `easyClose` param to `boxSidebar()` to allow to close sidebar on outside-click. Thanks @predict42-patrick for the suggestion and JS code. 
- New `gradient` parameter to `valuebox()`.
- New help mode in `dashboardPage()` to automatically toggle all tooltips and popovers. Use
with `bs4TooltipUI()` and `bs4PopoverUI()`. 
- New `useAutoColor()` leveraging new Shiny features and {thematic} to automatically
style plots depending on the dashboard background color.
- New bottom area for `dashboardSidebar()`.
- Add fullscreen widget to `dashboardPage()`.
- New `updateAccordion()`.
- `actionButton()` is updated to provide Bootstrap 4 features.
- New `appButton()`.
- New `updateUserMessages()`.
- New `userList()` and `userListItem()`.
- New `productList()` and `productListItem()`.
- New `userDescription()` and `userBlock()`.
- New built-in `skinSelector`.
- New built-in dark/light mode switcher.
- New "go to top" feature.
- New `dashboardUserItem()`, `userOutput()` and `renderUser()`.
- New `bs4DashBrand` for better dashboard titles.
- add `bs4ShowTab()` and `bs4HideTab()`: thanks @fmmattioni for the reminder ;)
- New `bs4CardLayout()`: simplify the way to deal with `bs4Card()`!
- New `bs4CloseAlert()`: programmatically close `bs4Alert()`
- New `bs4Toast()`: include the builtin AdminLTE3 toasts!
- Add options to select `bs4SidebarMenuItem()` and `bs4SidebarMenuSubItem()` at start.
- Now `bs4DropdownMenuItem()` may act as an actionButton
- Whenever a `bs4Card()` is maximized, the collapsible icon is hidden (does not make sense to have it)
- new `bs4CardLabel()` to add text labels in `bs4Card()`
- new `bs4CardSidebar()`: access the status via input$id. Add `updatebs4CardSidebar()` to toggle the card sidebar
- add `bs4PopoverServer()`, `bs4PopoverUI()`, `bs4TooltipUI()` and `bs4TooltipServer()`
- `bs4TabSetPanel()`: you may access the currently selected tab with input$id
- add type parameter to `bs4TabSetPanel()` and `bs4TabCard()` so as to select
between pills or tabs (cosmetic choice). If type is not provided, the behaviour is unchanged (pills are the default)
- add fixed param to `bs4DashFooter()`
- add fixed parameter to `bs4DashSidebar()`. Thanks @mppallante
- re-add fixed parameter to `bs4DashNavbar()`. This is reintroduced by adminlte with a new css class name
- add animated, label parameters to `bs4ProgressBar()`
- add iconStatus parameter to `bs4InfoBox()` to allow icon color
to be independant from the card status (https://adminlte.io/themes/v3/pages/widgets.html). Thanks @rolando-gsw

## Minor change
- `bs4Sortable()` is initialized each time a new sortable element is added. Fixes #198.
- New `taskItem()`.
- Add `indicators` and `.list` to `carousel()`.
- `insertTab()` is using more shiny vanilla elements.
- `bs4UserMenu()` does not close when clicked inside.
- Add glyphicons as an html dependency
- New legacy parameter in `bs4SidebarMenu()`: to use old AdminLTE2 styling for 
item selection
- Now, clicking outside the `bs4DashControlbar()` when it is opened will close it.
To keep it open, see below
- New pinned option for `bs4DashControlbar()`: allow to block the controlbar state
- New expandedName param for `bs4SidebarMenuItem` to align with shinydashboard
- id is NULL in `bs4TabSetPanel()` by default
- side is default to left in `bs4TabSetPanel()`
- change `bs4TabPanel()` param order to align with shiny

## Bug Fix
- Fix #200: simplified `bs4Table()` API. 
- Fix #189: `hidden` type is not available for `bs4Dash::tabsetPanel` that uses an old version of the
`shiny::tabsetPanel`. Thanks @zilch42.
- Upgrade AdminLTE to 3.1.0. This fixes many sidebar issues!!!
- Fix issue #110: due to the fact that the `bs4Controlbar()` now collapse when clicked outside, clicking on an input triggering the `updatebs4Controlbar()` would toggle it twice. Thanks @dwhdai
- Fix issue #112 with `sliderInput` z-index and `bs4DashPage()` preloader. Thanks @analytichealth for the report
- Fix issue in `bs4DashSidebar()` url link. Thanks @pvictor
- Fix #30: programmatically close `bs4Alert()`
- Fix #47: Control bar not showing top elements when collapsed is FALSE
- Fix #99: rework the way sidebar items work. By default, when sidebar items has subitems and is selected, the first item is not selected. We leave the choice to the user. Thanks @analytichealth
- Fix `bs4DropdownMenu()` example
- fix #94: cannot render element with dependencies in `bs4InsertTab()`
- fix #93: set default inputId when it is not provided in `bs4DashControlbar()` so that
`renderUI` works. Thanks @artemklevtsov
- fix #86 : cannot use `updatebs4TabSetPanel()` with `renderUI`. Thanks @jyjek
- add condition argument to `bs4SidebarMenuItem()`. See https://github.com/RinteRface/bs4Dash/issues/35. Thanks @aldomann
- some vignettes examples were not up to date

# bs4Dash 0.5.0

## Breaking Changes
- remove fixed parameters from `bs4DashNavbar()` since it does not exist in the new
AdminLTE version
- rework on `bs4DropdownMenuItem`: change text to message, date to time. Add new params: from, src, status and type.

## Major Changes
- add link back to https://www.glyphicons.com. Thanks @statnmap
- upgrade AdminLTE3 version to rc4
- add `bs4DashSidebar()` input binding to indicate the state (collapse: FALSE, open: TRUE) as well as 
`updatebs4Sidebar()` to programmatically toggle its state
- add `bs4Controlbar()` input binding to indicate the state (collapse: FALSE, open: TRUE) as well as 
`updatebs4Controlbar()` to programmatically toggle its state
- add `bs4Card()` input binding to indicate the state (collapse: FALSE, open: TRUE, maximize, closed, ...) as well as 
`updatebs4Card()` to programmatically toggle its state
- add `bs4InsertTab()` to programmatically insert `bs4TabPanel()` in `bs4TabSetPanel()`
- add `bs4RemoveTab()` to programmatically remove `bs4TabPanel()` in `bs4TabSetPanel()`


## New features
- `bs4DashPage()` has a new sidebar_mini parameter. When TRUE, the sidebar has a minimum
width when collapsed (you still see icons). When false, it behaves like in shinydashboard
- `bs4InfoBox()` can now navigate between tabs if the tabName parameter is specified. It must however correspond
to an existing `bs4TabItem()` in the body!
- `bs4ValueBox()` has a footer argument. It is different from the href parameter. Thanks @stefanfritsch
- add expand_on_hover parameter to `bs4DashSidebar()`
- add compact parameters to `bs4DashNavbar()` to reduce the navbar size
- add `getAdminLTEColors()` to preview all available color themes
- by default, `bs4DashSidebar()` has a fixed layout. This prevent from not seeing sidebar items
if the body contains too many elements
- `bs4DashPage()` has options to change the `bs4Controlbar()` behavior when it expands (see controlbar_overlay)
- `bs4SidebarMenu()` has 3 new parameters: flat (design effect), child_indent and compact
- add sidebar in `bs4Card()`
- add `bs4UserMenu()` for `bs4DashNavbar()`
- add `bs4Ribbon()`, a sort of enhanced label for cards or any content
- add `bs4Quote()`, an improved blockquote tag for Bootstrap 4

## Bug Fix
- fix #66 and #71: sidebar_collapsed was not working. Thanks @analytichealth and @federicomarini
- remove hardcoded style for navbar dropdown icons
- remove hardcoded style for navbar icons (fixed in the last adminLTE3 release). Thanks @federicomarini 
- fix height issue in `bs4Card()`. Thanks @analytichealth
- Rework the `column()` function from Shiny to make it work with Bootstrap 4. Thanks @federicomarini
- fix `bs4DashNavbar()` background color issue. 
- rework `bs4DashBody()` so that when no element is in the sidebar and no tabItems
are in the body, margin between the sidebar, the controlbar is not 0
- when btn_name is NULL in `bs4Jumbotron()`, do not show a button. Thanks @davidlvb
- fix `bs4ProgressBar()`: remove height and width parameters. Add size parameter. This
fix the progress behaviour which was wrong

# bs4Dash 0.4.0

## Major changes
- add `bs4DashControlbarMenu()`, `bs4DashControlbarItem()` and `updatebs4ControlbarMenu()`
- add `updatebs4TabItems()` and `updateTabItems()` to programatically update the selected
sidebar tab and the corresponding body tab
- add `updatebs4TabSetPanel()` and `updateTabsetPanel()` (aliases) to dynamically update `bs4TabSetPanel()` from
the server side
- add maximizable option for `bs4Card()` and `bs4TabCard()`. In other words, allow full screen option
- Fix #8: add input binding to the `bs4SidebarMenu()`. Now by adding an id to the menu,
the user can recover the currently selected tab. Thanks @rpodcast
- add aliases to make bs4Dash closer to shinydashboard (the conversion is easier). 
For instance `bs4DashPage()` can also be `dashboardPage()`
- update adminLTE dependencies
- add vertical mode to `bs4TabSetPanel()`
- add `bs4SidebarMenuSubItem()`
- add `bs4Table()`, `bs4TableItems()` and `bs4TableItem()`

## Breaking changes
- add text arg to `bs4SidebarMenuItem()`
- Now the ... arg is for `bs4SidebarMenuSubItem()`

## Minor Changes
- Fix #41: add disable argument to `bs4DashSidebar()` and `bs4DashControlbar()`. Thanks @jamiebono 
- images are centered in `userPost()`
- add startExpanded arg to `bs4SidebarMenuItem()`
- add `bs4TabSetPanel()` to the gallery

## Bug Fix
- fix overflow-y issue in `bs4TabCard()`
- fix #44: update `bs4InfoBox()` class. Thanks @henry090.
- update adminLTE dependencies (fix sidebar horizontal overflow, ...)
- hide the `bs4DashControlbar()` toggle if no `bs4DashControlbar()` is given in `bs4DashPage()`
- fix wrong url in `attachmentBlock()` image (and replace http by https)
- fix #24: No right border on collapsed sidebar menu items. Thanks @aldomann
- Tooltips z-index is lower than sidebar's z-index, this cause tooltips are not showing up 
(see https://github.com/ColorlibHQ/AdminLTE/pull/2105)
- after Bootstrap 4.3.1 update the table color in the green calendar widget changed to black, caused by bootstrap (see https://github.com/ColorlibHQ/AdminLTE/pull/2105)


# bs4Dash 0.3.0

## Breaking change
- rework `bs4TabSetPanel()` and `bs4TabCard()`. `bs4TabSetPanel()` can now be used independently of `bs4TabCard()`. 
Need to provide id argument to `bs4TabCard()`.

## Major changes
- update adminLTE3 to the latest release
- update to bootstrap 4.3.1
- add preloader options to `bs4DashPage()`
- add collapsible and closable to `bs4TabCard()`. Buttons visible by default. PR by @statnmap
- add preloader options to `bs4DashPage()`
- move to RinteRface
- add `bs4Sortable()` and all necessary javascript dependencies
- add `userPost()`, `userPostMedia()`, `userPostTagItems()` and `userPostTagItem()`
- add `userMessages()` and `userMessage()`
- add `descriptionBlock()`, `attachmentBlock()` and `cardPad()`
- add `bs4SidebarUserPanel()`
- add `cardProfile()`, `cardProfileItemList()` and `cardProfileItem()`
- add `bs4SocialCard()` and `cardComment()`

## Minor changes
- add collapse_status argument to `userPost()`
- add glyphicons (BS3 old dependency) thanks to @ntncmch
- add overflow argument to `bs4Card()` to enable or disable overflow
- hide `bs4Card()` footer when footer is NULL
- patch for `bs4Accordion()`: click on 1 item collapse all other items in the same accordion
add an id argument.
- add licence
- add controlbar_collapsed argument to `bs4DashPage()`
- fix #16: add side argument to `bs4TabCard()`. Thanks @stephLH
- fix #11: add width arg to `bs4DashControlbar()`. Thanks @stephLH
- fix #14: remove `bs4TabCard()` max-height attribute. Thanks @stephLH
- add hex icon
- add menuIcon arg and align arg to `bs4DropdownMenu()`, add id arg to `bs4DashControlbar()` PR by @bjornlind 
- add background and header colours like cards to `bs4TabCard()`. PR by @statnmap

## Bug fix
- Fix icon rendering issue due to a change in shiny 1.2.0 dependencies (font awesome)
- Fix #19: Whenever a `bs4Card()` starts on a collapsed state, the content is displayed when
 uncollapsed is pressed. Thanks @stanmap for the remark. (See [here](https://github.com/rstudio/shinydashboard/commit/73f602736efe496dc0e2b54882c1036eacaef79f) for a similar issue)
- `bs4Card()`: the collapse icon changes to + and - depending on the current collapse status.
- fix various issues in `bs4DropdownMenu()` 
- fix #12: Compatibility with shinyWidgets pickerInput. Thanks @pvictor
- fix #13 in `bs4TabPanel()`: handle tabName with space and with punctuation characters. Thanks @stephLH
- fix `bs4TabCard()`: when the user set 2 or more active tabs at the same time, only 
the first one is selected
- related to #16: modify bs4Dash.js file so that only the first body content tab is selected
 and not that of the `bs4TabCard()`, as they have the same class (tab-pane).

# bs4Dash 0.2.0
## Major changes
- bs4Dash now works on shinyapp.io (previously, there was a problem with tab navigation)
- major update of all dependencies (bootstrap 4, fontawesome, ionicons, ...) with
htmltools

## Minor changes
- close and collapse buttons not shown when the title is NULL in `bs4Card()`
- `bs4ValueBox()` footer is not shown when the external link is NULL
- `bs4ValueBox()` external link opens in a new page
- add sidebar_collapsed argument to `bs4DashPage()` to control the sidebar opening at start
- update `bs4DashGallery()`. Add a "theme" argument
- add `ionicon()` (implementation of ionicons)
- fix vignette titles
- add a "fixed" argument to the `bs4DashNavbar()` (#7, thanks @theRcast)
- update Readme

# bs4Dash 0.1.0
- initial release
