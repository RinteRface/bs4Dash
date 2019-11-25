source("global.R")

shiny::shinyApp(
  ui = bs4DashPage(
    sidebar_collapsed = TRUE,
    enable_preloader = TRUE,
    loading_duration = 3,
    controlbar_overlay = FALSE,
    navbar = bs4DashNavbar(
      status = "white",
      actionButton(inputId = "controlbarToggle", label = "Toggle Controlbar"),
      rightUi = tagList(
        bs4DropdownMenu(
          show = FALSE,
          labelText = "!",
          status = "danger",
          src = "https://www.google.fr",
          bs4DropdownMenuItem(
            message = "update your system",
            time = "today",
            type = "notification"
          ),
          bs4DropdownMenuItem(
            src = "https://adminlte.io/themes/v3/dist/img/user2-160x160.jpg",
            from = "John Doe",
            message = "Call me whenever you can...",
            time = "4 Minutes Ago",
            type = "message"
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
      expand_on_hover = TRUE,
      skin = "light",
      status = "primary",
      title = "bs4Dash",
      brandColor = "primary",
      url = "https://www.google.fr",
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      elevation = 3,
      opacity = 0.8,
      bs4SidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        child_indent = TRUE,
        bs4SidebarHeader("Cards"),
        bs4SidebarMenuItem(
          "Basic cards",
          tabName = "cards",
          icon = "sliders"
        ),
        bs4SidebarMenuItem(
          "Cards API",
          tabName = "cardsAPI",
          icon = "laptop-code"
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
        
        bs4SidebarHeader("Colors"),
        
        bs4SidebarMenuItem(
          "Colors",
          tabName = "colors",
          icon = "tint"
        ),
        
        bs4SidebarHeader("BS4 gallery"),
        bs4SidebarMenuItem(
          text = "Galleries",
          icon = "cubes",
          startExpanded = FALSE,
          bs4SidebarMenuSubItem(
            text = HTML(
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
            icon = "circle-thin"
          ),
          bs4SidebarMenuSubItem(
            text = HTML(
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
            icon = "circle-thin"
          )
        )
      )
    ),
    body = bs4DashBody(
      bs4TabItems(
        basic_cards_tab,
        cards_api_tab,
        social_cards_tab,
        tab_cards_tab,
        sortable_cards_tab,
        statsboxes_tab,
        boxes_tab,
        value_boxes_tab,
        colors_tab,
        gallery_1_tab,
        gallery_2_tab
      )
    ),
    controlbar = bs4DashControlbar(
      inputId = "controlbar",
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
  server = function(input, output, session) {
    
    output$bigPlot <- renderPlot({
      hist(rnorm(input$bigObs))
    })
    
    output$plot <- renderPlot({
      hist(rnorm(input$obs))
    })
    
    # this is not reactive but just for fixing the plot size on the client side.
    output$riverPlot <- renderEcharts4r({
      river %>% 
        e_charts(dates) %>% 
        e_river(apples) %>% 
        e_river(bananas) %>% 
        e_river(pears) %>% 
        e_tooltip(trigger = "axis") %>% 
        e_title("River charts", "(Streamgraphs)") %>%
        e_theme("shine")
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
    
    
    observeEvent(input$current_tab, {
      if (input$current_tab == "cards") {
        showModal(modalDialog(
          title = "This event only triggers for the first tab!",
          "You clicked me! This event is the result of
          an input bound to the menu. By adding an id to the
          bs4SidebarMenu, input$id will give the currently selected
          tab. This is useful to trigger some events.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    
    output$cardAPIPlot <- renderPlot({
      if (input$mycard$maximized) {
        hist(rnorm(input$obsAPI)) 
      }
    })
    
    observeEvent(input$triggerCard, {
      updatebs4Card(inputId = "mycard", session = session, action = input$cardAction)
    })
    
    observe({
      print(
        list(
          collapsed = input$mycard$collapsed,
          maximized = input$mycard$maximized,
          visible = input$mycard$visible
        )
      )
    })
    
    observeEvent(input$controlbar, {
      if (input$controlbar) {
        showModal(modalDialog(
          title = "Alert",
          "The controlbar is opened.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    observeEvent(input$controlbarToggle, {
      updatebs4Controlbar(inputId = "controlbar", session = session)
    })
    
    observe({
      print(input$controlbar)
    })
    
  }
)