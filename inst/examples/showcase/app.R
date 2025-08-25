# Load packages
library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)

thematic_shiny()

# toast options
toastOpts <- list(
  autohide = TRUE,
  icon = "fas fa-home",
  close = FALSE,
  position = "bottomRight"
)

# color statuses
statusColors <- c(
  "gray-dark",
  "gray",
  "secondary",
  "navy",
  "indigo",
  "purple",
  "primary",
  "lightblue",
  "info",
  "success",
  "olive",
  "teal",
  "lime",
  "warning",
  "orange",
  "danger",
  "fuchsia",
  "maroon",
  "pink",
  "white"
)

#' basic_cards_tab ----
basic_cards_tab <- tabItem(
  tabName = "cards",
  fluidRow(
    box(
      title = "Box with all widgets",
      closable = TRUE,
      width = 6,
      status = "warning",
      solidHeader = FALSE,
      collapsible = TRUE,
      label = boxLabel(
        text = 1,
        status = "danger"
      ),
      dropdownMenu = boxDropdown(
        boxDropdownItem("Link to google", href = "https://www.google.com"),
        boxDropdownItem("Item with inputId", id = "dropdown_item2"),
        dropdownDivider(),
        boxDropdownItem("item 3", href = "#", icon = icon("table-cells"))
      ),
      sidebar = boxSidebar(
        startOpen = FALSE,
        id = "mycardsidebar",
        background = "#7f7f7f",
        sliderInput(
          "obs",
          "Number of observations:",
          min = 0,
          max = 1000,
          value = 500
        )
      ),
      actionButton("toggle_card_sidebar", "Toggle card sidebar"),
      plotOutput("plot")
    ),
    box(
      ribbon(
        text = "New",
        color = "orange"
      ),
      title = "Closable card with gradient",
      width = 6,
      gradient = TRUE,
      background = "teal",
      status = "teal",
      solidHeader = TRUE,
      collapsible = FALSE,
      "Empty card"
    )
  ),
  fluidRow(
    box(
      title = "Card with solidHeader and elevation",
      elevation = 4,
      closable = TRUE,
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      "Empty card"
    ),
    box(
      id = "card4",
      title = "Maximizable Card",
      width = 6,
      status = "danger",
      closable = FALSE,
      maximizable = TRUE,
      collapsible = TRUE,
      sliderInput(
        "bigObs",
        "Number of observations:",
        min = 0,
        max = 1000,
        value = 500
      ),
      plotOutput("bigPlot")
    )
  )
)

#' card API
cards_api_tab <- tabItem(
  tabName = "cardsAPI",
  actionButton(inputId = "triggerCard", label = "Trigger Card Action"),
  actionButton("update_box", "Update box"),
  selectInput(
    inputId = "cardAction",
    label = "Card action",
    choices = c(
      "remove",
      "toggle",
      "toggleMaximize",
      "restore"
    )
  ),

  box(
    id = "mycard",
    title = "The plot is visible when you maximize the card",
    closable = TRUE,
    maximizable = TRUE,
    width = 12,
    status = "warning",
    solidHeader = FALSE,
    collapsible = TRUE,
    sliderInput(
      "obsAPI",
      "Number of observations:",
      min = 0,
      max = 1000,
      value = 500
    ),
    plotOutput("cardAPIPlot")
  )
)


#' social_cards_tab ----
social_cards_tab <- tabItem(
  tabName = "socialcards",
  fluidRow(
    userBox(
      title = userDescription(
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
        title = "User card type 1",
        subtitle = "a subtitle here"
      ),
      collapsible = FALSE,
      ribbon(
        text = "New user",
        color = "fuchsia"
      ),
      status = "purple",
      elevation = 4,
      "Any content here"
    ),
    userBox(
      title = userDescription(
        type = 2,
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
        title = "User card type 2",
        subtitle = "a subtitle here",
        imageElevation = 4
      ),
      status = "teal",
      background = "teal",
      maximizable = TRUE,
      gradient = TRUE,
      progressBar(
        value = 5,
        striped = FALSE,
        status = "info"
      ),
      progressBar(
        value = 20,
        striped = TRUE,
        status = "warning"
      )
    )
  ),
  fluidRow(
    socialBox(
      title = userBlock(
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
        title = "Social Box",
        subtitle = "example-01.05.2018"
      ),
      "Some text here!",
      attachmentBlock(
        image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg",
        title = "Test",
        href = "https://google.com",
        "This is the content"
      ),
      lapply(X = 1:10, FUN = function(i) {
        boxComment(
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
          title = paste("Comment", i),
          date = "01.05.2018",
          paste0("The ", i, "-th comment")
        )
      }),
      footer = "The footer here!"
    ),
    box(
      title = "Box with user comment",
      status = "primary",
      userPost(
        id = 1,
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
        author = "Jonathan Burke Jr.",
        description = "Shared publicly - 7:30 PM today",
        "Lorem ipsum represents a long-held tradition for designers, 
       typographers and the like. Some people hate it and argue for 
       its demise, but others ignore the hate as they create awesome 
       tools to help create filler text for everyone from bacon 
       lovers to Charlie Sheen fans.",
        userPostTagItems(
          userPostTagItem(dashboardBadge("item 1", color = "warning")),
          userPostTagItem(dashboardBadge("item 2", color = "danger"))
        )
      ),
      userPost(
        id = 2,
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
        author = "Adam Jones",
        description = "Shared publicly - 5 days ago",
        userPostMedia(
          image = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"
        ),
        userPostTagItems(
          userPostTagItem(dashboardBadge("item 1", color = "info")),
          userPostTagItem(dashboardBadge("item 2", color = "danger"))
        )
      )
    )
  ),
  fluidRow(
    box(
      status = "primary",
      width = 3,
      solidHeader = TRUE,
      boxProfile(
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
        title = "Nina Mcintire",
        subtitle = "Software Engineer",
        bordered = TRUE,
        boxProfileItem(
          title = "Followers",
          description = 1322
        ),
        boxProfileItem(
          title = "Following",
          description = 543
        ),
        boxProfileItem(
          title = "Friends",
          description = 13287
        )
      )
    ),
    box(
      title = "Card with messages",
      width = 9,
      footer = tagList(
        actionButton("remove_message", "Remove"),
        actionButton("add_message", "Add"),
        actionButton("update_message", "Update"),
        numericInput("index_message", "Message index:", 1, min = 1, max = 3)
      ),
      userMessages(
        width = 6,
        status = "danger",
        id = "message",
        userMessage(
          author = "Alexander Pierce",
          date = "20 Jan 2:00 pm",
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
          type = "received",
          "Is this template really for free? That's unbelievable!"
        ),
        userMessage(
          author = "Sarah Bullock",
          date = "23 Jan 2:05 pm",
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
          type = "sent",
          "You better believe it!"
        )
      )
    )
  )
)

# tab_cards_tab ----
tab_cards_tab <- tabItem(
  tabName = "tabcards",
  fluidRow(
    column(
      width = 6,
      tabBox(
        ribbon(
          text = "Tabs",
          color = "pink"
        ),
        title = "A card with tabs",
        elevation = 2,
        id = "tabcard1",
        width = 12,
        collapsible = FALSE,
        closable = FALSE,
        type = "tabs",
        status = "primary",
        solidHeader = TRUE,
        selected = "Tab 2",
        tabPanel(
          "Tab 1",
          "A wonderful serenity has taken possession of my entire soul,
          like these sweet mornings of spring which I enjoy with my
          whole heart. I am alone, and feel the charm of existence in
          this spot, which was created for the bliss of souls like mine.
          I am so happy, my dear friend, so absorbed in the exquisite sense
          of mere tranquil existence, that I neglect my talents. I should be
          incapable of drawing a single stroke at the present moment; and yet
          I feel that I never was a greater artist than now"
        ),
        tabPanel(
          "Tab 2",
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
        tabPanel(
          "Tab 3",
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
    column(
      width = 6,
      actionButton("update_tabBox2", "Toggle maximize tabBox", class = "my-2"),
      tabBox(
        title = "Tabs on right!",
        side = "right",
        id = "tabcard2",
        type = "tabs",
        elevation = 2,
        width = 12,
        status = "warning",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        selected = "Tab 6",
        tabPanel(
          "Tab 4",
          "A wonderful serenity has taken possession of my entire soul,
          like these sweet mornings of spring which I enjoy with my
          whole heart. I am alone, and feel the charm of existence in
          this spot, which was created for the bliss of souls like mine.
          I am so happy, my dear friend, so absorbed in the exquisite sense
          of mere tranquil existence, that I neglect my talents. I should be
          incapable of drawing a single stroke at the present moment; and yet
          I feel that I never was a greater artist than now"
        ),
        tabPanel(
          "Tab 5",
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
        tabPanel(
          "Tab 6",
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
    )
  ),
  br(),
  br(),
  fluidRow(
    # manually inserted panels
    column(
      width = 6,
      bs4Dash::tabsetPanel(
        id = "tabsetpanel1",
        selected = "Tab 2",
        tabPanel(
          "Tab 1",
          "Content 1"
        ),
        tabPanel(
          "Tab 2",
          "Content 2"
        ),
        tabPanel(
          "Tab 3",
          "Content 3"
        )
      )
    ),

    # programmatically inserted panels
    column(
      width = 6,
      bs4Dash::tabsetPanel(
        id = "tabsetpanel2",
        type = "pills",
        .list = lapply(1:3, function(i) {
          tabPanel(
            paste0("Tab", i),
            paste("Content", i)
          )
        })
      )
    )
  ),
  br(),
  br(),
  # Vertical panels: TO DO
  tabsetPanel(
    id = "tabsetpanel3",
    selected = "Tab 2",
    vertical = TRUE,
    tabPanel(
      "Tab 1",
      "Content 1"
    ),
    tabPanel(
      "Tab 2",
      "Content 2"
    ),
    tabPanel(
      "Tab 3",
      "Content 3"
    )
  )
)


# sortable_cards_tab ----
sortable_cards_tab <- tabItem(
  tabName = "sortablecards",
  fluidRow(
    lapply(1:3, FUN = function(i) {
      sortable(
        width = 4,
        p(class = "text-center", paste("Column", i)),
        lapply(1:2, FUN = function(j) {
          box(
            title = paste0("I am the ", j, "-th card of the ", i, "-th column"),
            width = 12,
            "Click on my header"
          )
        })
      )
    })
  )
)


# statsboxes_tab ----
statsboxes_tab <- tabItem(
  tabName = "statsboxes",
  fluidRow(
    box(
      solidHeader = FALSE,
      title = "Card with descriptionBlock",
      background = NULL,
      width = 6,
      status = "danger",
      footer = fluidRow(
        column(
          width = 6,
          descriptionBlock(
            number = "17%",
            numberColor = "success",
            numberIcon = icon("caret-up"),
            header = "$35,210.43",
            text = "TOTAL REVENUE",
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 6,
          descriptionBlock(
            number = "18%",
            numberColor = "danger",
            numberIcon = icon("caret-down"),
            header = "1200",
            text = "GOAL COMPLETION",
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      )
    ),
    box(
      title = "Box with right pad",
      status = "warning",
      fluidRow(
        column(width = 6),
        column(
          width = 6,
          boxPad(
            color = "info",
            descriptionBlock(
              header = "8390",
              text = "VISITS",
              rightBorder = FALSE,
              marginBottom = TRUE
            ),
            descriptionBlock(
              header = "30%",
              text = "REFERRALS",
              rightBorder = FALSE,
              marginBottom = TRUE
            ),
            descriptionBlock(
              header = "70%",
              text = "ORGANIC",
              rightBorder = FALSE,
              marginBottom = FALSE
            )
          )
        )
      )
    )
  )
)


# value_boxes_tab ----
value_boxes_tab <- tabItem(
  tabName = "valueboxes",
  h4("Value Boxes"),
  fluidRow(
    valueBox(
      value = 150,
      subtitle = "New orders",
      color = "primary",
      icon = icon("cart-shopping"),
      href = "#"
    ),
    valueBox(
      elevation = 4,
      value = "53%",
      subtitle = "New orders",
      color = "danger",
      icon = icon("gears")
    ),
    valueBox(
      value = "44",
      subtitle = "User Registrations",
      color = "warning",
      icon = icon("sliders")
    ),
    valueBox(
      value = "53%",
      subtitle = "Bounce rate",
      color = "success",
      icon = icon("database")
    )
  ),
  h4("Info Boxes"),
  fluidRow(
    infoBox(
      tabName = "cardsAPI",
      title = "Navigate to Cards API section",
      value = 1410,
      color = "indigo",
      icon = icon("laptop-code")
    ),
    infoBox(
      tabName = "colors",
      title = "Navigate to colors section",
      color = "info",
      value = 240,
      icon = icon("droplet"),
      elevation = 4
    ),
    infoBox(
      title = "Comments",
      subtitle = "A subtitle",
      color = "indigo",
      gradient = TRUE,
      fill = TRUE,
      value = 41410,
      icon = icon("comments"),
      href = "https://www.google.com"
    )
  )
)


# gallery_1_tab ----
gallery_1_tab <- tabItem(
  tabName = "gallery1",
  fluidRow(
    box(
      title = "Accordions",
      footer = tagList(
        h4("There is an accordion in the footer!"),
        accordion(
          id = "accordion1",
          accordionItem(
            title = "Item 1",
            status = "danger",
            "Anim pariatur cliche reprehenderit, enim 
            eiusmod high life accusamus terry richardson ad 
            squid. 3 wolf moon officia aute, non cupidatat 
            skateboard dolor brunch. Food truck quinoa nesciunt 
            laborum eiusmod. Brunch 3 wolf moon tempor, sunt 
            aliqua put a bird on it squid single-origin coffee 
            nulla assumenda shoreditch et. Nihil anim keffiyeh 
            helvetica, craft beer labore wes anderson cred 
            nesciunt sapiente ea proident. Ad vegan excepteur 
            butcher vice lomo. Leggings occaecat craft beer farm-to-table, 
            raw denim aesthetic synth nesciunt you probably haven't 
            heard of them accusamus labore sustainable VHS"
          ),
          accordionItem(
            title = "Item 2",
            status = "warning",
            "Anim pariatur cliche reprehenderit, enim 
            eiusmod high life accusamus terry richardson ad 
            squid. 3 wolf moon officia aute, non cupidatat 
            skateboard dolor brunch. Food truck quinoa nesciunt 
            laborum eiusmod. Brunch 3 wolf moon tempor, sunt 
            aliqua put a bird on it squid single-origin coffee 
            nulla assumenda shoreditch et. Nihil anim keffiyeh 
            helvetica, craft beer labore wes anderson cred 
            nesciunt sapiente ea proident. Ad vegan excepteur 
            butcher vice lomo. Leggings occaecat craft beer farm-to-table, 
            raw denim aesthetic synth nesciunt you probably haven't 
            heard of them accusamus labore sustainable VHS"
          )
        )
      )
    ),
    box(
      title = "Carousel",
      carousel(
        id = "mycarousel",
        width = 12,
        carouselItem(
          tags$img(src = "placeholders/500x500.png")
        ),
        carouselItem(
          tags$img(src = "placeholders/500x500.png")
        ),
        carouselItem(
          tags$img(src = "placeholders/500x500.png")
        )
      )
    )
  ),
  fluidRow(
    box(
      title = "bs4Quote",
      fluidRow(
        blockQuote("Blablabla", color = "indigo"),
        blockQuote("Blablabla", color = "danger"),
        blockQuote("Blablabla", color = "teal"),
        blockQuote("Blablabla", color = "orange"),
        blockQuote("Blablabla", color = "warning"),
        blockQuote("Blablabla", color = "fuchsia")
      )
    )
  ),
  fluidRow(
    box(
      title = "Progress bars",
      footer = tagList(
        progressBar(
          value = 5,
          striped = FALSE,
          status = "info"
        ),
        progressBar(
          value = 10,
          striped = TRUE,
          status = "maroon"
        )
      ),
      progressBar(
        value = 80,
        vertical = TRUE,
        status = "success"
      ),
      progressBar(
        value = 100,
        vertical = TRUE,
        striped = TRUE,
        status = "danger"
      )
    ),
    box(
      title = "Alerts",
      elevation = 4,
      actionButton("show_alert", "Show"),
      actionButton("hide_alert", "Hide"),
      div(id = "alert_anchor")
    )
  ),
  fluidRow(
    box(
      title = "Callouts",
      callout(
        title = "I am a danger callout!",
        elevation = 4,
        status = "danger",
        width = 12,
        "There is a problem that we need to fix. 
        A wonderful serenity has taken possession of 
        my entire soul, like these sweet mornings of 
        spring which I enjoy with my whole heart."
      )
    )
  ),
  fluidRow(
    box(
      title = "Timeline",
      timelineBlock(
        width = 12,
        reversed = TRUE,
        timelineEnd(color = "danger"),
        timelineLabel("10 Feb. 2014", color = "info"),
        timelineItem(
          title = "Item 1",
          icon = icon("gears"),
          color = "success",
          time = "now",
          footer = "Here is the footer",
          "This is the body"
        ),
        timelineItem(
          title = "Item 2",
          border = FALSE
        ),
        timelineLabel("3 Jan. 2014", color = "primary"),
        timelineItem(
          title = "Item 3",
          icon = icon("paint-brush"),
          color = "warning",
          timelineItemMedia(image = "placeholders/150x150.png"),
          timelineItemMedia(image = "placeholders/150x150.png")
        ),
        timelineStart(color = "danger")
      )
    ),
    timelineBlock(
      width = 6,
      timelineEnd(color = "danger"),
      timelineLabel("10 Feb. 2014", color = "info"),
      timelineItem(
        title = "Item 1",
        icon = icon("gears"),
        color = "success",
        time = "now",
        footer = "Here is the footer",
        "This is the body"
      ),
      timelineItem(
        title = "Item 2",
        border = FALSE
      ),
      timelineLabel("3 Jan. 2014", color = "primary"),
      timelineItem(
        title = "Item 3",
        icon = icon("paint-brush"),
        color = "warning",
        timelineItemMedia(image = "placeholders/150x150.png"),
        timelineItemMedia(image = "placeholders/150x150.png")
      ),
      timelineStart(color = "danger")
    )
  ),
  br(),
  fluidRow(
    box(
      title = "Stars",
      starBlock(5),
      starBlock(5, color = "success"),
      starBlock(1, color = "danger"),
      starBlock(3, color = "info")
    ),
    box(
      title = "Attachment example",
      attachmentBlock(
        image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg",
        title = "Test",
        href = "https://google.com",
        "This is the content"
      )
    )
  ),
  h4("bs4Table"),
  fluidRow(
    bs4Table(
      cardWrap = TRUE,
      bordered = TRUE,
      striped = TRUE,
      list(
        list(
          income = "$2,500 USD",
          status = dashboardBadge(
            "Pending",
            position = "right",
            color = "danger",
            rounded = TRUE
          ),
          progress = progressBar(value = 50, status = "pink", size = "xxs"),
          text = "test",
          confirm = actionButton(
            "go",
            "Go"
          )
        ),
        list("$2,500 USD", "NA", "NA", "test", "NA")
      )
    )
  )
)


# gallery_2_tab ----
gallery_2_tab <- tabItem(
  tabName = "gallery2",
  jumbotron(
    title = "I am a Jumbotron!",
    lead = "This is a simple hero unit, a simple jumbotron-style 
                    component for calling extra attention to featured 
                    content or information.",
    "It uses utility classes for typography and spacing 
            to space content out within the larger container.",
    status = "primary",
    href = "https://www.google.fr"
  ),

  br(),

  fluidRow(
    box(
      title = "Badges",
      dashboardBadge(color = "secondary", "blabla", rounded = TRUE),
      dashboardBadge(color = "info", "blabla", rounded = TRUE)
    )
  ),

  br(),

  h4("BS4 list group"),
  fluidRow(
    listGroup(
      type = "basic",
      listGroupItem("Cras justo odio"),
      listGroupItem("Dapibus ac facilisis in"),
      listGroupItem("Morbi leo risus")
    ),
    listGroup(
      type = "action",
      listGroupItem(
        "Cras justo odio",
        active = TRUE,
        disabled = FALSE,
        href = "https://www.google.com"
      ),
      listGroupItem(
        active = FALSE,
        disabled = FALSE,
        "Dapibus ac facilisis in",
        href = "https://www.google.com"
      ),
      listGroupItem(
        "Morbi leo risus",
        active = FALSE,
        disabled = TRUE,
        href = "https://www.google.com"
      )
    ),
    listGroup(
      type = "heading",
      listGroupItem(
        "Donec id elit non mi porta gravida at eget metus. 
         Maecenas sed diam eget risus varius blandit.",
        active = TRUE,
        disabled = FALSE,
        title = "List group item heading",
        subtitle = "3 days ago",
        footer = "Donec id elit non mi porta."
      ),
      listGroupItem(
        "Donec id elit non mi porta gravida at eget metus. 
         Maecenas sed diam eget risus varius blandit.",
        active = FALSE,
        disabled = FALSE,
        title = "List group item heading",
        subtitle = "3 days ago",
        footer = "Donec id elit non mi porta."
      )
    )
  )
)

# color_tab ----
colors_tab <- tabItem(
  tabName = "colors",
  lapply(seq_along(statusColors), function(i) {
    fluidRow(
      box(
        status = statusColors[i],
        title = paste(statusColors[i], "card"),
        solidHeader = TRUE,
        width = 12,
        closable = FALSE,
        collapsible = FALSE
      )
    )
  })
)

shinyApp(
  ui = dashboardPage(
    preloader = list(
      html = tagList(spin_1(), "Loading ..."),
      color = "#343a40"
    ),
    dark = TRUE,
    help = FALSE,
    fullscreen = TRUE,
    scrollToTop = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = sprintf(
          "bs4Dash v%s",
          as.character(utils::packageVersion("bs4Dash"))
        ),
        color = "primary",
        href = "https://divadnojnarg.github.io/outstanding-shiny-ui/",
        image = "https://adminlte.io/themes/v3/dist/img/user2-160x160.jpg",
        opacity = 0.8
      ),
      fixed = TRUE,
      tooltip(
        title = "This toggles the right sidebar",
        placement = "bottom",
        actionButton(
          inputId = "controlbarToggle",
          label = "Toggle Controlbar",
          class = "mx-2"
        )
      ),
      popover(
        title = "Toggle button",
        content = "This toggle the left sidebar",
        placement = "bottom",
        # `data-trigger` = "hover",
        actionButton(
          inputId = "sidebarToggle",
          label = "Toggle left sidebar",
          class = "mx-2"
        )
      ),
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
      id = "sidebar",
      customArea = fluidRow(
        actionButton(
          inputId = "myAppButton",
          label = NULL,
          icon = icon("users"),
          width = NULL,
          status = "primary",
          style = "margin: auto",
          dashboardBadge(textOutput("btnVal"), color = "danger")
        )
      ),
      sidebarUserPanel(
        image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png",
        name = "Welcome Onboard!"
      ),
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        sidebarHeader("Cards"),
        menuItem(
          "Basic cards",
          tabName = "cards",
          icon = icon("sliders"),
          checkboxInput("myCheck", "Checkbox")
        ),
        menuItem(
          "Cards API",
          badgeLabel = "New",
          badgeColor = "success",
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
          icon = icon("layer-group")
        ),
        menuItem(
          "Sortable cards",
          tabName = "sortablecards",
          icon = icon("object-ungroup")
        ),
        menuItem(
          "Stats elements",
          tabName = "statsboxes",
          icon = icon("chart-area")
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
          icon = icon("droplet")
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
            icon = icon("circle")
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
            tabName = "gallery2"
          )
        )
      )
    ),
    body = dashboardBody(
      tabItems(
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
        type = "pills",
        controlbarItem(
          "Inputs",
          column(
            width = 12,
            align = "center",
            radioButtons(
              inputId = "dist",
              label = "Distribution type:",
              c(
                "Normal" = "norm",
                "Uniform" = "unif",
                "Log-normal" = "lnorm",
                "Exponential" = "exp"
              )
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
        target = "_blank",
        "@DivadNojnarg"
      ),
      right = "2022"
    ),
    title = "bs4Dash Showcase"
  ),
  server = function(input, output, session) {
    useAutoColor()

    # app button --------------------------------------------------------------
    output$btnVal <- renderText(input$myAppButton)
    observeEvent(input$myAppButton, {
      showModal(modalDialog("Thanks for clicking me!", easyClose = TRUE))
    })

    # alerts ------------------------------------------------------------------

    observeEvent(input$show_alert, {
      print("created")
      createAlert(
        id = "alert_anchor",
        options = list(
          title = "Be Careful!",
          status = "danger",
          closable = TRUE,
          width = 12,
          content = "Danger alert preview. This alert is dismissable. 
          A wonderful serenity has taken possession of my entire soul, 
          like these sweet mornings of spring which 
          I enjoy with my whole heart."
        )
      )
    })

    observeEvent(input$hide_alert, {
      print("deleted")
      closeAlert(id = "alert_anchor")
    })

    # alert callback event
    observeEvent(input$alert_anchor, {
      alertStatus <- if (input$alert_anchor) "opened" else "closed"
      toastColor <- if (input$alert_anchor) "bg-lime" else "bg-fuchsia"
      toast(
        title = sprintf("Alert succesfully %s!", alertStatus),
        options = list(
          class = toastColor,
          autohide = TRUE,
          position = "bottomRight"
        )
      )
    })

    # tooltips, popovers ------------------------------------------------------

    # observe({
    #  addTooltip(
    #    id = "controlbarToggle",
    #    options = list(
    #      title = "This toggles the right sidebar",
    #      placement = "bottom"
    #    )
    #  )
    # })

    # plots -------------------------------------------------------------------

    output$bigPlot <- renderPlot({
      hist(rnorm(input$bigObs))
    })

    output$plot <- renderPlot({
      hist(rnorm(input$obs))
    })

    observeEvent(input$current_tab, {
      if (input$current_tab == "cards") {
        showModal(modalDialog(
          title = "This event only triggers for the first tab!",
          "You clicked me! This event is the result of
          an input bound to the menu. By adding an id to the
          sidebarMenu, input$id will give the currently selected
          tab. This is useful to trigger some events.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })

    # current theme info ---------------------------------------------------------

    observeEvent(input$dark_mode, {
      toast(
        title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
        options = list(
          position = "topRight",
          class = "bg-warning",
          autohide = TRUE
        )
      )
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

    observeEvent(input$update_box, {
      updateBox(
        "mycard",
        action = "update",
        options = list(
          title = h3(
            class = "card-title",
            "hello",
            dashboardBadge(1, color = "primary")
          ),
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          background = NULL,
          height = "900px",
          closable = FALSE
        )
      )
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

    observeEvent(input$sidebar, {
      toastOpts$class <- if (input$sidebar) "bg-success" else "bg-danger"
      toast(
        title = if (input$sidebar) "Sidebar opened!" else "Sidebar is closed!",
        options = toastOpts
      )
    })

    # tabBox API  -------------------------------------------------------------

    observeEvent(input$update_tabBox2, {
      updateBox("tabcard2_box", action = "toggleMaximize")
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
      toast(
        title = if (input$controlbar) {
          "Controlbar opened!"
        } else {
          "Controlbar closed!"
        },
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
      toast(
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

    # update sidebar ----------------------------------------------------------

    observeEvent(input$sidebarToggle, {
      updateSidebar(id = "sidebar")
    })

    # user messages -----------------------------------------------------------

    observeEvent(input$remove_message, {
      updateUserMessages(
        "message",
        action = "remove",
        index = input$index_message
      )
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
              icon = icon("arrows-rotate"),
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
