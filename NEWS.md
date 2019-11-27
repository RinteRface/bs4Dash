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