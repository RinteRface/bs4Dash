#' @include dashboardPage.R
#' @include dashboardNavbar.R
#' @include dashboardControlbar.R
#' @include dashboardFooter.R
#' @include dashboardBody.R
#' @include dashboardSidebar.R
#' @include cards.R
#' @include render-functions.R
#' @include update-functions.R
NULL


#' Alias to \link{bs4DashboardPage}
#' See \link{bs4DashboardPage} for full details
#'
#' @rdname bs4DashPage
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(bs4Dash)
#' 
#'  shiny::shinyApp(
#'    ui = dashboardPage(
#'    navbar = dashboardHeader(),
#'    sidebar = dashboardSidebar(),
#'    body = dashboardBody(
#'      fluidRow(
#'        column(width = 4, plotOutput("distPlot")),
#'        column(width = 4, tableOutput("data")),
#'        column(width = 4, textOutput("result"))
#'      )
#'    ),
#'    controlbar = dashboardControlbar(
#'      skin = "light",
#'      tabsetPanel(
#'        type = "tabs",
#'        id = "tabsetpanel",
#'        tabPanel(
#'          title = "Tab 1",
#'          br(),
#'          sliderInput(
#'            "obs",
#'            "Number of observations:",
#'            min = 0,
#'            max = 1000,
#'            value = 500
#'          )
#'        ),
#'        tabPanel(
#'          title = "Tab 2",
#'          br(),
#'          checkboxGroupInput(
#'            "variable",
#'            "Variables to show:",
#'            c("Cylinders" = "cyl",
#'              "Transmission" = "am",
#'              "Gears" = "gear")
#'          )
#'        ),
#'        tabPanel(
#'          title = "Tab 3",
#'          br(),
#'          selectInput(
#'            "state",
#'            "Choose a state:",
#'            list(`East Coast` = list("NY", "NJ", "CT"),
#'                 `West Coast` = list("WA", "OR", "CA"),
#'                 `Midwest` = list("MN", "WI", "IA")
#'            )
#'          )
#'        )
#'      )
#'     )
#'    ),
#'    server = function(input, output) {
#'     output$distPlot <- renderPlot({
#'      hist(rnorm(input$obs))
#'     })
#'    
#'     output$data <- renderTable({
#'       head(mtcars[, c("mpg", input$variable), drop = FALSE])
#'     }, rownames = TRUE)
#'    
#'     output$result <- renderText({
#'       paste("You chose", input$state)
#'     })
#'    }
#'  )
#' }
#'
#' @export
dashboardPage <- bs4DashPage




#' Alias to \link{bs4DashNavbar}
#' See \link{bs4DashNavbar} for full details
#'
#' @rdname bs4DashNavbar
#'
#' @export
dashboardHeader <- bs4DashNavbar





#' Alias to \link{bs4DropdownMenu}
#' See \link{bs4DropdownMenu} for full details
#'
#' @rdname bs4DropdownMenu
#'
#' @export
dropdownMenu <- bs4DropdownMenu





#' Alias to \link{bs4DropdownMenuItem}
#' See \link{bs4DropdownMenuItem} for full details
#'
#' @rdname bs4DropdownMenuItem
#'
#' @export
dropdownMenuItem <- bs4DropdownMenuItem





#' Alias to \link{bs4DashControlbar}
#' See \link{bs4DashControlbar} for full details
#'
#' @rdname bs4DashControlbar
#'
#' @export
dashboardControlbar <- bs4DashControlbar





#' Alias to \link{bs4DashControlbarMenu}
#' See \link{bs4DashControlbarMenu} for full details
#'
#' @inheritParams bs4DashControlbarMenu
#' @rdname bs4DashControlbarMenu
#'
#' @export
controlbarMenu <- bs4DashControlbarMenu




#' Alias to \link{bs4DashControlbarItem}
#' See \link{bs4DashControlbarItem} for full details
#'
#' @inheritParams bs4DashControlbarItem
#' @rdname bs4DashControlbarItem
#'
#' @export
controlbarItem <- bs4DashControlbarItem 





#' Alias to \link{updatebs4ControlbarMenu}
#' See \link{updatebs4ControlbarMenu} for full details
#'
#' @inheritParams updatebs4ControlbarMenu 
#' @rdname updatebs4ControlbarMenu 
#'
#' @export
updateControlbarMenu <- updatebs4ControlbarMenu 





#' Alias to \link{bs4DashFooter}
#' See \link{bs4DashFooter} for full details
#'
#' @rdname bs4DashFooter
#'
#' @export
dashboardFooter <- bs4DashFooter





#' Alias to \link{bs4DashSidebar}
#' See \link{bs4DashSidebar} for full details
#'
#' @rdname bs4DashSidebar
#'
#' @export
dashboardSidebar <- bs4DashSidebar





#' Alias to \link{bs4SidebarHeader}
#' See \link{bs4SidebarHeader} for full details
#'
#' @rdname bs4SidebarHeader
#'
#' @export
sidebarHeader <- bs4SidebarHeader





#' Alias to \link{bs4SidebarMenu}
#' See \link{bs4SidebarMenu} for full details
#'
#' @rdname bs4SidebarMenu
#'
#' @export
sidebarMenu <- bs4SidebarMenu





#' Alias to \link{bs4SidebarUserPanel}
#' See \link{bs4SidebarUserPanel} for full details
#'
#' @rdname bs4SidebarUserPanel
#'
#' @export
sidebarUserPanel <- bs4SidebarUserPanel





#' Alias to \link{bs4SidebarMenuItem}
#' See \link{bs4SidebarMenuItem} for full details
#'
#' @rdname bs4SidebarMenuItem
#'
#' @export
menuItem <- bs4SidebarMenuItem





#' Alias to \link{bs4SidebarMenuSubItem}
#' See \link{bs4SidebarMenuSubItem} for full details
#'
#' @rdname bs4SidebarMenuSubItem
#'
#' @export
menuSubItem <- bs4SidebarMenuSubItem





#' Alias to \link{bs4DashBody}
#' See \link{bs4DashBody} for full details
#'
#' @rdname bs4DashBody
#'
#' @export
dashboardBody <- bs4DashBody



#' Alias to \link{bs4TabItems}
#' See \link{bs4TabItems} for full details
#'
#' @rdname bs4TabItems
#'
#' @export
tabItems <- bs4TabItems




#' Alias to \link{bs4TabItem}
#' See \link{bs4TabItem} for full details
#'
#' @rdname bs4TabItem
#'
#' @export
tabItem <- bs4TabItem






#' Alias to \link{bs4Card}
#' See \link{bs4Card} for full details
#'
#' @rdname bs4Card
#'
#' @export
box <- bs4Card





#' Alias to \link{bs4TabCard}
#' See \link{bs4TabCard} for full details
#'
#' @rdname bs4TabCard
#'
#' @export
tabBox <- bs4TabCard




#' Alias to \link{bs4TabSetPanel}
#' See \link{bs4TabSetPanel} for full details
#'
#' @rdname bs4TabSetPanel
#'
#' @export
tabsetPanel <- bs4TabSetPanel






#' Alias to \link{bs4TabPanel}
#' See \link{bs4TabPanel} for full details
#'
#' @rdname bs4TabPanel
#'
#' @export
tabPanel <- bs4TabPanel






#' Alias to \link{bs4InfoBox}
#' See \link{bs4InfoBox} for full details
#'
#' @rdname bs4InfoBox
#'
#' @export
infoBox <- bs4InfoBox





#' Alias to \link{bs4InfoBoxOutput}
#' See \link{bs4InfoBoxOutput} for full details
#' 
#' @inheritParams bs4InfoBoxOutput
#' @rdname bs4InfoBoxOutput
#'
#' @export
infoBoxOutput <- bs4InfoBoxOutput





#' Alias to \link{renderbs4InfoBox}
#' See \link{renderbs4InfoBox} for full details
#'
#' @inheritParams renderbs4InfoBox
#' @rdname renderbs4InfoBox
#'
#' @export
renderInfoBox <- renderbs4InfoBox





#' Alias to \link{bs4ValueBox}
#' See \link{bs4ValueBox} for full details
#'
#' @rdname bs4ValueBox
#'
#' @export
valueBox <- bs4ValueBox





#' Alias to \link{bs4ValueBoxOutput}
#' See \link{bs4ValueBoxOutput} for full details
#'
#' @rdname bs4ValueBoxOutput
#'
#' @export
valueBoxOutput <- bs4ValueBoxOutput





#' Alias to \link{renderbs4ValueBox}
#' See \link{renderbs4ValueBox} for full details
#'
#' @rdname renderbs4ValueBox
#'
#' @export
renderValueBox <- renderbs4ValueBox





#' Alias to \link{updatebs4TabSetPanel}
#' See \link{updatebs4TabSetPanel} for full details
#'
#' @rdname updatebs4TabSetPanel
#'
#' @export
updateTabsetPanel <- updatebs4TabSetPanel




#' Alias to \link{ updatebs4TabItems}
#' See \link{updatebs4TabItems} for full details
#'
#' @rdname  updatebs4TabItems
#'
#' @export
updateTabItems <- updatebs4TabItems




#' Alias to \link{bs4CardSidebar}
#' See \link{bs4CardSidebar} for full details
#'
#' @rdname bs4CardSidebar
#'
#' @export
cardSidebar <- bs4CardSidebar





#' Alias to \link{updatebs4CardSidebar}
#' See \link{updatebs4CardSidebar} for full details
#'
#' @rdname updatebs4CardSidebar
#'
#' @export
updateCardSidebar <- updatebs4CardSidebar