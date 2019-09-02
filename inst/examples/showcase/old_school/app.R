source("global.R")

shiny::shinyApp(
  ui = bs4DashPage(
    old_school = TRUE,
    sidebar_collapsed = TRUE,
    navbar = bs4DashNavbar(
      status = "white",
      "I can write text in the navbar!",
      rightUi = tagList(
        bs4DropdownMenu(
          show = FALSE,
          labelText = "!",
          status = "danger",
          src = "https://www.google.fr",
          bs4DropdownMenuItem(
            text = "message 1",
            date = "today"
          ),
          bs4DropdownMenuItem(
            text = "message 2",
            date = "yesterday"
          )
        ),
        bs4UserMenu(
          name = "Divad Nojnarg", 
          status = "primary",
          src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg", 
          title = "bs4Dash",
          subtitle = "Author", 
          footer = p("The footer", class = "text-center"),
          "This is the menu content."
        )
      )
    ),
    sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      title = "bs4Dash",
      brandColor = "primary",
      url = "https://www.google.fr",
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      elevation = 3,
      opacity = 0.8,
      bs4SidebarMenu(
        bs4SidebarHeader("Cards"),
        bs4SidebarMenuItem(
          "Basic cards",
          tabName = "cards",
          icon = "sliders"
        ),
        bs4SidebarMenuItem(
          "Social cards",
          tabName = "socialcards",
          icon = "id-card"
        ),
        bs4SidebarMenuItem(
          "Tab cards",
          tabName = "tabcards",
          icon = "picture-o"
        ),
        bs4SidebarMenuItem(
          "Sortable cards",
          tabName = "sortablecards",
          icon = "object-ungroup"
        ),
        bs4SidebarMenuItem(
          "Stats elements",
          tabName = "statsboxes",
          icon = "bank"
        ),
        bs4SidebarHeader("Boxes"),
        bs4SidebarMenuItem(
          "Basic boxes",
          tabName = "boxes",
          icon = "desktop"
        ),
        bs4SidebarMenuItem(
          "Value/Info boxes",
          tabName = "valueboxes",
          icon = "suitcase"
        ),
        bs4SidebarHeader("BS4 gallery"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Gallery 1", 
              bs4Badge(
                "new", 
                position = "right", 
                status = "danger"
              )
            )
          ),
          tabName = "gallery1",
          icon = "paint-brush"
        ),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Gallery 2", 
              bs4Badge(
                "!", 
                position = "right", 
                status = "success"
              )
            )
          ),
          tabName = "gallery2",
          icon = "map"
        )
      )
    ),
    body = bs4DashBody(
      bs4TabItems(
        basic_cards_tab,
        social_cards_tab,
        tab_cards_tab,
        sortable_cards_tab,
        statsboxes_tab,
        boxes_tab,
        value_boxes_tab,
        gallery_1_tab,
        gallery_2_tab
      )
    ),
    controlbar = bs4DashControlbar(
      skin = "light",
      title = "My right sidebar",
      setSliderColor(sliderId = 1, "black"),
      sliderInput(
        inputId = "obs", 
        label = "Number of observations:",
        min = 0, 
        max = 1000, 
        value = 500
      ),
      column(
        width = 12,
        align = "center",
        radioButtons(
          inputId = "dist", 
          label = "Distribution type:",
          c("Normal" = "norm",
            "Uniform" = "unif",
            "Log-normal" = "lnorm",
            "Exponential" = "exp")
        )
      )
    ),
    footer = bs4DashFooter(
      copyrights = a(
        href = "https://twitter.com/divadnojnarg", 
        target = "_blank", "@DivadNojnarg"
      ),
      right_text = "2018"
    ),
    title = "bs4Dash Showcase"
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
    
    output$plot2 <- renderPlotly({
      p <- plot_ly(df, x = ~x) %>%
        add_lines(y = ~y1, name = "A") %>%
        add_lines(y = ~y2, name = "B", visible = F) %>%
        layout(
          xaxis = list(domain = c(0.1, 1)),
          yaxis = list(title = "y"),
          updatemenus = list(
            list(
              y = 0.8,
              buttons = list(
                
                list(method = "restyle",
                     args = list("line.color", "blue"),
                     label = "Blue"),
                
                list(method = "restyle",
                     args = list("line.color", "red"),
                     label = "Red"))),
            
            list(
              y = 0.7,
              buttons = list(
                list(method = "restyle",
                     args = list("visible", list(TRUE, FALSE)),
                     label = "Sin"),
                
                list(method = "restyle",
                     args = list("visible", list(FALSE, TRUE)),
                     label = "Cos")))
          )
        )
    })
    
    output$plot3 <- renderPlotly({
      s <- subplot(
        plot_ly(x = x, type = "histogram"),
        plotly_empty(),
        plot_ly(x = x, y = y, type = "histogram2dcontour"),
        plot_ly(y = y, type = "histogram"),
        nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
        shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
      )
      p <- layout(s, showlegend = FALSE)
    })
    
  }
)