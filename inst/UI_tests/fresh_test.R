library(bs4Dash)
library(fresh)
# Theme -------------------------------------------------------------------

bs4DashTheme <- create_theme(
  bs4dash_vars(
    navbar_dark_color = "#bec5cb",
    navbar_dark_active_color = "#FFF",
    navbar_dark_hover_color = "#FFF"
  ),
  bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#272c30"),
  bs4dash_layout(main_bg = "#353c42"),
  bs4dash_sidebar_dark(
    bg = "#272c30", color = "#bec5cb", hover_color = "#FFF",
    submenu_bg = "#272c30", submenu_color = "#FFF", submenu_hover_color = "#FFF"
  ),
  bs4dash_status(dark = "#272c30"),
  bs4dash_color(gray_900 = "#FFF", white = "#272c30")
)



# App ---------------------------------------------------------------------


ui <- bs4DashPage(
  title = "bs4Dash Dark Mode",
  # sidebar_collapsed = FALSE,
  navbar = bs4DashNavbar(skin = "dark"),
  controlbar = bs4DashControlbar(
    skin = "dark",
    "This is the control bar"
  ),
  sidebar = bs4DashSidebar(
    title = "bs4Dash Dark Mode",
    skin = "dark",
    bs4SidebarMenu(
      bs4SidebarHeader("Menu:"),
      bs4SidebarMenuItem(
        tabName = "tab1",
        text = "UI components",
        icon = "home"
      )
    )
  ),
  body = bs4DashBody(
    
    use_theme(bs4DashTheme),
    
    bs4TabItems(
      bs4TabItem(
        tabName = "tab1",
        fluidRow(
          bs4TabCard(
            width = 6,
            id = "tabcard",
            title = "Tab box elements not shown at start",
            bs4TabPanel(
              tabName = "Tab 1", 
              active = FALSE,
              "Content 1"
            ),
            bs4TabPanel(
              tabName = "Tab 2", 
              active = TRUE,
              "Content 2"
            ),
            bs4TabPanel(
              tabName = "Tab 3", 
              active = FALSE,
              "Content 3"
            )
          ),
          bs4Card(
            title = "Dropdown menu elements not properly highlighted", 
            closable = TRUE, 
            width = 6,
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            cardLabel = bs4CardLabel(
              text = 1,
              status = "danger",
              tooltip = "Hello!"
            ),
            dropdownMenu = dropdownItemList(
              dropdownItem(url = "http://www.google.com", name = "Link to google"),
              dropdownItem(url = "#", name = "item 2"),
              dropdownDivider(),
              dropdownItem(url = "#", name = "item 3")
            ),
            p("Box Content")
          )
        ),
        fluidRow(
          bs4SocialCard(
            title = "Social Card elements should have dark grey background",
            subtitle = "example-01.05.2018",
            src = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
            "Some text here!",
            comments = tagList(
              lapply(X = 1:10, FUN = function(i) {
                cardComment(
                  src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
                  title = paste("Comment", i),
                  date = "01.05.2018",
                  paste0("The ", i, "-th comment")
                )
              })
            ),
            footer = "The footer here!"
          )
        ),
        fluidRow(
          bs4Card(
            title = "Timeline cards should have dark skin",
            bs4Timeline(
              width = 12,
              reversed = TRUE,
              bs4TimelineEnd(status = "danger"),
              bs4TimelineLabel("10 Feb. 2014", status = "info"),
              bs4TimelineItem(
                elevation = 4, 
                title = "Item 1",
                icon = "gears",
                status = "success",
                time = "now",
                footer = "Here is the footer",
                "This is the body"
              ),
              bs4TimelineItem(
                title = "Item 2",
                border = FALSE
              ),
              bs4TimelineLabel("3 Jan. 2014", status = "primary"),
              bs4TimelineItem(
                elevation = 2,
                title = "Item 3",
                icon = "paint-brush",
                status = "warning",
                bs4TimelineItemMedia(src = "http://placehold.it/150x100"),
                bs4TimelineItemMedia(src = "http://placehold.it/150x100")
              ),
              bs4TimelineStart(status = "danger")
            )
          )
        ),
        fluidRow(
          textInput("caption", "Text input color should be white", "Data Summary"),
          verbatimTextOutput("value"),
          numericInput("obs", "Observations:", 10, min = 1, max = 100),
          verbatimTextOutput("value2"),
          selectInput("state", "Choose a state:",
                      list(`East Coast` = list("NY", "NJ", "CT"),
                           `West Coast` = list("WA", "OR", "CA"),
                           `Midwest` = list("MN", "WI", "IA"))
          ),
          textOutput("result")
        )
      )
    )
  )
)


server <- function(input, output) {
  output$value <- renderText({ input$caption })
  output$value2 <- renderText({ input$obs })
  output$result <- renderText({
    paste("You chose", input$state)
  })
}


shinyApp(ui, server)