source("global.R")

shinyApp(
  ui = dashboardPage(
    preloader = list(
      waiter = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
      duration = 5
    ),
    header = dashboardHeader(
      title = dashboardBrand(
        title = "bs4Dash",
        color = "primary",
        href = "https://divadnojnarg.github.io/outstanding-shiny-ui/",
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
        opacity = 0.8
      ),
      fixed = TRUE,
      fullscreen = TRUE,
      actionButton(inputId = "controlbarToggle", label = "Toggle Controlbar"),
      rightUi = tagList(
        dropdownMenu(
          badgeStatus = "danger",
          type = "messages",
          messageItem(
            inputId = "triggerAction1",
            message = "message 1",
            from = "Divad Nojnarg",
            image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
            time = "today",
            color = "lime"
          )
        ),
        userOutput("user")
      ),
      leftUi = tagList(
        dropdownMenu(
          badgeStatus = "info",
          type = "notifications",
          notificationItem(
            inputId = "triggerAction2",
            text = "Error!",
            status = "danger"
          )
        ),
        dropdownMenu(
          badgeStatus = "info",
          type = "tasks",
          taskItem(
            inputId = "triggerAction3",
            text = "My progress",
            color = "orange",
            value = 10
          )
        )
      )
    ),
    sidebar = dashboardSidebar(
      fixed = TRUE,
      skin = "light",
      status = "primary",
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        sidebarHeader("Cards"),
        menuItem(
          "Basic cards",
          tabName = "cards",
          icon = icon("sliders")
        ),
        menuItem(
          "Cards API",
          tabName = "cardsAPI",
          icon = icon("laptop-code")
        ),
        menuItem(
          "Social cards",
          tabName = "socialcards",
          icon = icon("id-card")
        ),
        menuItem(
          "Tab cards",
          tabName = "tabcards",
          icon = icon("picture-o")
        ),
        menuItem(
          "Sortable cards",
          tabName = "sortablecards",
          icon = icon("object-ungroup")
        ),
        menuItem(
          "Stats elements",
          tabName = "statsboxes",
          icon = icon("bank")
        ),
        sidebarHeader("Other boxes"),
        menuItem(
          "Value/Info boxes",
          tabName = "valueboxes",
          icon = icon("suitcase")
        ),
        
        sidebarHeader("Colors"),
        
        menuItem(
          "Colors",
          tabName = "colors",
          icon = icon("tint")
        ),
        
        sidebarHeader("BS4 gallery"),
        menuItem(
          text = "Galleries",
          icon = icon("cubes"),
          startExpanded = FALSE,
          menuSubItem(
            text = HTML(
              paste(
                "Gallery 1", 
                dashboardBadge(
                  "new", 
                  position = "right", 
                  color = "danger"
                )
              )
            ),
            tabName = "gallery1",
            icon = icon("circle-thin")
          ),
          menuSubItem(
            text = HTML(
              paste(
                "Gallery 2", 
                dashboardBadge(
                  "!", 
                  position = "right", 
                  color = "success"
                )
              )
            ),
            tabName = "gallery2",
            icon = icon("circle-thin")
          )
        )
      )
    ),
    body = bs4DashBody(
      tags$head(
        tags$script(
          "$(function() {
            $(document).on('shiny:connected', function() {
              $('#customSwitch1').click();
            });
          });
          "
        )
      ),
      e_theme_register(echarts_dark_theme$options, name = echarts_dark_theme$name),
      bs4TabItems(
        basic_cards_tab,
        cards_api_tab,
        social_cards_tab,
        tab_cards_tab,
        sortable_cards_tab,
        statsboxes_tab,
        value_boxes_tab,
        colors_tab,
        gallery_1_tab,
        gallery_2_tab
      )
    ),
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        type = "tabs",
        controlbarItem(
          "Inputs",
          setSliderColor(sliderId = 1, "black"),
          radioGroupButtons(
            inputId = "somevalue3",
            label = "With icons:",
            choices = names(mtcars)[1:3],
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square-o")
            )
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
        controlbarItem(
          "Skin",
          skinSelector()
        )
      )
    ),
    footer = dashboardFooter(
      fixed = FALSE,
      left = a(
        href = "https://twitter.com/divadnojnarg", 
        target = "_blank", "@DivadNojnarg"
      ),
      right = "2020"
    ),
    title = "bs4Dash Showcase"
  ),
  server = function(input, output, session) {
    
    useAutoColor()
    
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
    
    output$rosetype <- renderEcharts4r({
      
      plot <- mtcars %>% 
        head() %>% 
        dplyr::mutate(model = row.names(.)) %>% 
        e_charts(model) %>% 
        e_pie(hp, roseType = "radius")
      if (input$dark_mode) plot <- plot %>% e_theme(echarts_dark_theme$name)
      plot
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
    

    # card API ----------------------------------------------------------------
    
    output$cardAPIPlot <- renderPlot({
      if (input$mycard$maximized) {
        hist(rnorm(input$obsAPI)) 
      }
    })
    
    observeEvent(input$triggerCard, {
      updateBox(id = "mycard", action = input$cardAction)
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
    
    
    # card sidebar API --------------------------------------------------------
    
    observeEvent(input$toggle_card_sidebar, {
      updateBoxSidebar("mycardsidebar")
    })

    # controlbar input --------------------------------------------------------
    
    observeEvent(input$controlbar, {
      toastOpts <- list(
        autohide = TRUE,
        icon = "fas fa-home",
        close = FALSE,
        position = "bottomRight"
      )
      toastOpts$class <- if (input$controlbar) "bg-success" else "bg-danger"
      bs4Toast(
        title = "Controlbar opened!", 
        options = toastOpts
      )
    })
    
    observeEvent(input$controlbarToggle, {
      updateControlbar(id = "controlbar")
    })
    
    observe({
      print(input$controlbar)
    })
    
    
    observeEvent(input$dropdown_item2, {
      bs4Toast(
        title = "I am a toast!", 
        options = list(
          autohide = TRUE,
          icon = "fas fa-home",
          close = FALSE,
          position = "topLeft",
          class = "bg-orange"
        )
      )
    })
    
    
    # user messages -----------------------------------------------------------
    
    observeEvent(input$remove_message, {
      updateUserMessages("message", action = "remove", index = input$index)
    })
    observeEvent(input$add_message, {
      updateUserMessages(
        "message", 
        action = "add", 
        content = list(
          author = "David",
          date = "Now",
          image = "https://i.pinimg.com/originals/f1/15/df/f115dfc9cab063597b1221d015996b39.jpg",
          type = "received",
          text = "Message content"
        )
      )
    })
    
    observeEvent(input$update_message, {
      updateUserMessages(
        "message", 
        action = "update", 
        index = input$index_message,
        content = list(
          text = tagList(
            appButton(
              inputId = "reload",
              label = "Click me!", 
              icon = icon("sync"), 
              dashboardBadge(1, color = "primary")
            )
          )
        )
      )
    })
    
    observeEvent(input$reload, {
      showNotification("Yeah!", duration = 1, type = "default")
    })
    
    
    
    # user menu ---------------------------------------------------------------
    
    output$user <- renderUser({
      dashboardUser(
        name = "Divad Nojnarg", 
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg", 
        title = "shinydashboardPlus",
        subtitle = "Author", 
        footer = p("The footer", class = "text-center"),
        fluidRow(
          dashboardUserItem(
            width = 6,
            "Item 1"
          ),
          dashboardUserItem(
            width = 6,
            "Item 2"
          )
        )
      )
    })
    
  }
)