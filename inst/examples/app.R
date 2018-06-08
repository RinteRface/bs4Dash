library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(plotly)
# devtools::install_github(
#   "DivadNojnarg/bs4Dash",
#   auth_token = "6ba4d889bd51b7d4225a4cbc1b984b813ad5cb95")

shiny::shinyApp(
  ui = bs4DashPage(
    navbar = bs4DashNavbar(
      status = "primary",
      "I can write text in the navbar!"
    ),
    sidebar = bs4DashSidebar(
      title = "bs4Dash",
      brandColor = "primary",
      url = "http://www.google.fr",
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      elevation = 3,
      opacity = 0.8,
      bs4SidebarMenu(
        bs4SidebarHeader("Main content"),
        bs4SidebarMenuItemList(
          name = "Cards",
          icon = "gears",
          open = TRUE,
          active = FALSE,
          bs4SidebarMenuItem(
            "Basic cards",
            tabName = "cards",
            icon = "sliders",
            active = TRUE
          ),
          bs4SidebarMenuItem(
            "Tab boxes",
            tabName = "tabcards",
            icon = "picture-o",
            active = FALSE
          ),
          bs4SidebarMenuItem(
            "Value boxes",
            tabName = "valueboxes",
            icon = "suitcase",
            active = FALSE
          )
        ),
        bs4SidebarHeader("Bonus"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Other stuff", 
              bs4DashBadge(
                "new", 
                position = "right", 
                status = "danger"
              )
            )
          ),
          tabName = "bonus",
          icon = "paint-brush",
          active = FALSE
        ),
        bs4SidebarMenuItem(
          "Test",
          tabName = "test",
          icon = "map",
          active = FALSE
        )
      )
    ),
    body = bs4DashBody(
      tabItems(
        tabItem(
          tabName = "cards",
          fluidRow(
            bs4Card(
              title = "Closable Box with dropdown", 
              closable = TRUE, 
              width = 6,
              status = "warning", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              labelText = 1,
              labelStatus = "danger",
              labelTooltip = "Hi Bro!",
              dropdownIcon = "wrench",
              dropdownMenu = dropdownItemList(
                dropdownItem(url = "http://www.google.com", name = "Link to google"),
                dropdownItem(url = "#", name = "item 2"),
                dropdownDivider(),
                dropdownItem(url = "#", name = "item 3")
              ),
              plotOutput("plot")
            ),
            bs4Card(
              title = "Closable Box with gradient", 
              closable = TRUE, 
              width = 6,
              status = "warning", 
              solidHeader = FALSE, 
              gradientColor = "success",
              collapsible = TRUE,
              plotOutput("distPlot")
            ),
            bs4Card(
              title = "Closable Box with solidHeader", 
              closable = TRUE, 
              width = 6,
              solidHeader = TRUE, 
              status = "primary",
              collapsible = TRUE,
              plot_ly(z = ~volcano) %>% add_surface()
            )
          )
        ),
        tabItem(
          tabName = "tabcards",
          bs4TabCard(
            title = "A card with tabs",
            bs4TabPanel(
              tabName = "Tab1", 
              active = FALSE,
              "A wonderful serenity has taken possession of my entire soul, 
              like these sweet mornings of spring which I enjoy with my 
              whole heart. I am alone, and feel the charm of existence in 
              this spot, which was created for the bliss of souls like mine. 
              I am so happy, my dear friend, so absorbed in the exquisite sense 
              of mere tranquil existence, that I neglect my talents. I should be 
              incapable of drawing a single stroke at the present moment; and yet 
              I feel that I never was a greater artist than now"
            ),
            bs4TabPanel(
              tabName = "Tab2", 
              active = TRUE,
              "The European languages are members of the same family. 
              Their separate existence is a myth. For science, music, 
              sport, etc, Europe uses the same vocabulary. The languages 
              only differ in their grammar, their pronunciation and their 
              most common words. Everyone realizes why a new common 
              language would be desirable: one could refuse to pay expensive 
              translators. To achieve this, it would be necessary to have 
              uniform grammar, pronunciation and more common words. If several 
              languages coalesce, the grammar of the resulting language is 
              more simple and regular than that of the individual languages."
            ),
            bs4TabPanel(
              tabName = "Tab3", 
              active = FALSE,
              "Lorem Ipsum is simply dummy text of the printing and 
              typesetting industry. Lorem Ipsum has been the industry's 
              standard dummy text ever since the 1500s, when an unknown 
              printer took a galley of type and scrambled it to make a 
              type specimen book. It has survived not only five centuries, 
              but also the leap into electronic typesetting, remaining 
              essentially unchanged. It was popularised in the 1960s with 
              the release of Letraset sheets containing Lorem Ipsum passages, 
              and more recently with desktop publishing software like Aldus 
              PageMaker including versions of Lorem Ipsum."
            )
          )
        ),
        tabItem(
          tabName = "valueboxes",
          fluidRow(
            bs4ValueBox(
              value = 150,
              subtitle = "New orders",
              status = "primary",
              icon = "shopping-cart",
              href = "#"
            ),
            bs4ValueBox(
              value = "53%",
              subtitle = "New orders",
              status = "danger",
              icon = "cogs"
            ),
            bs4ValueBox(
              value = "44",
              subtitle = "User Registrations",
              status = "warning",
              icon = "sliders"
            )
          )
        ),
        tabItem(
          tabName = "bonus"
        ),
        tabItem(
          tabName = "test"
        )
      )
    ),
    controlbar = bs4DashControlbar(
      title = "My right sidebar",
      sliderInput("obs", "Number of observations:",
                  min = 0, max = 1000, value = 500
      ),
      radioButtons(
        "dist", 
        "Distribution type:",
        c("Normal" = "norm",
          "Uniform" = "unif",
          "Log-normal" = "lnorm",
          "Exponential" = "exp")
      )
    ),
    footer = bs4DashFooter(
      copyrights = "@DivadNojnarg",
      right_text = "2018"
    ),
    title = "test"
  ),
  server = function(input, output) {
    
    output$plot <- renderPlot({
      hist(rnorm(input$obs))
    })
    
    output$distPlot <- renderPlot({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     lnorm = rlnorm,
                     exp = rexp,
                     rnorm)
      
      hist(dist(500))
    })
    
  }
)
