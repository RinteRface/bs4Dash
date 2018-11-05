# bs4Dash 0.3.0.9000
## Major changes
- add `bs4SidebarUserPanel()`
- add `cardProfile()`, `cardProfileItemList()` and `cardProfileItem()`
- add `bs4SocialCard()`

## Minor changes
- fix #16: add side argument to `bs4TabCard()`. Thanks @stephLH
- fix #11: add width arg to `bs4DashControlbar()`. Thanks @stephLH
- fix #14: remove `bs4TabCard()` max-height attribute. Thanks @stephLH
- add hex icon
- add menuIcon arg and align arg to `bs4DropdownMenu()`, add id arg to `bs4DashControlbar()` PR by @bjornlind 

## Bug fix
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