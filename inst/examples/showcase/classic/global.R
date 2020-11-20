# Load packages
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(echarts4r)
library(thematic)
library(waiter)

thematic_shiny()

# echarts4r theme #3d444c
echarts_dark_theme <- list(
  options = '{
    "color":["#6610f2", "#ffc107", "#e83e8c", "#ff851b", "#17a2b8", "#3d9970"], 
    "backgroundColor": "#343a40", 
    "textStyle": {
        color: "#fff"
    }
  }',
  name = "dark_theme"
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
  "light"
)

# river charts 
dates <- seq.Date(Sys.Date() - 30, Sys.Date(), by = "day")

river <- data.frame(
  dates = dates,
  apples = runif(length(dates)),
  bananas = runif(length(dates)),
  pears = runif(length(dates))
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
        boxDropdownItem("Link to google", href = "http://www.google.com"),
        boxDropdownItem("Item with inputId", id = "dropdown_item2"),
        dropdownDivider(),
        boxDropdownItem("item 3", href = "#", icon = icon("th"))
      ),
      sidebar = boxSidebar(
        startOpen = TRUE,
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
      title = "Closable card with gradient", 
      closable = TRUE, 
      width = 6,
      gradient = TRUE,
      background = "lightblue",
      status = "lightblue", 
      solidHeader = TRUE, 
      collapsible = TRUE,
      echarts4rOutput("riverPlot")
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
      echarts4rOutput("rosetype")
    ),
    box(
      id = "card4",
      title = "Maximizable Card", 
      width = 6,
      status = "danger", 
      closable = FALSE,
      maximizable = TRUE, 
      collapsible = TRUE,
      sliderInput("bigObs", "Number of observations:",
                  min = 0, max = 1000, value = 500
      ),
      plotOutput("bigPlot")
    )
  )
)

#' card API
cards_api_tab <- tabItem(
  tabName = "cardsAPI",
  actionButton(inputId = "triggerCard", label = "Trigger Card Action"),
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
    sliderInput("obsAPI", "Number of observations:",
                min = 0, max = 1000, value = 500
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
      status = "purple",
      elevation = 4,
      maximizable = TRUE,
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
        image = "https://adminlte.io/themes/AdminLTE/dist/img/photo1.png",
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
        userPostMedia(image = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
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
      footer =  tagList(
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
  br(), br(),
  fluidRow(
    # manually inserted panels
    column(
      width = 6,
      tabsetPanel(
        id = "tabcard",
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
      tabsetPanel(
        id = "tabset",
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
  br(), br()
  # Vertical panels: TO DO
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
            title = paste0("I am the ", j,"-th card of the ", i, "-th column"), 
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
      icon = icon("shopping-cart"),
      href = "#"
    ),
    valueBox(
      elevation = 4,
      value = "53%",
      subtitle = "New orders",
      color = "danger",
      icon = icon("cogs")
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
      icon = icon("tint"),
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
          tags$img(src = "https://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap")
        ),
        carouselItem(
          tags$img(src = "https://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap")
        ),
        carouselItem(
          tags$img(src = "https://placehold.it/900x500/f39c12/ffffff&text=I+Love+Bootstrap")
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
      bs4Alert(
        title = "Be Careful!",
        status = "danger",
        closable = TRUE,
        width = 12,
        "Danger alert preview. This alert is dismissable. 
                A wonderful serenity has taken possession of my entire soul, 
                like these sweet mornings of spring which 
                I enjoy with my whole heart."
      )
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
          timelineItemMedia(image = "https://placehold.it/150x100"),
          timelineItemMedia(image = "https://placehold.it/150x100")
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
        timelineItemMedia(image = "https://placehold.it/150x100"),
        timelineItemMedia(image = "https://placehold.it/150x100")
      ),
      timelineStart(color = "danger")
    )
  ),
  br(),
  fluidRow(
    box(
      title = "Stars",
      starBlock(grade = 5),
      starBlock(grade = 5, color = "success"),
      starBlock(grade = 1, color = "danger"),
      starBlock(grade = 3, color = "info")
    ),
    box(
      title = "Attachment example",
      attachmentBlock(
        image = "https://adminlte.io/themes/dev/AdminLTE/dist/img/photo1.png",
        title = "Test",
        href = "http://google.com",
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
      headTitles = c(
        "PROJECT",
        "BUDGET",
        "STATUS",
        "USERS",
        "COMPLETION",
        ""
      ),
      bs4TableItems(
        bs4TableItem("bs4 Design System"),
        bs4TableItem(dataCell = TRUE, "$2,500 USD"),
        bs4TableItem(
          dataCell = TRUE, 
          dashboardBadge(
            "Pending",
            position = "right",
            color = "danger",
            rounded = TRUE
          )
        ),
        bs4TableItem(
          progressBar(value = 50, status = "pink", size = "xxs")
        ),
        bs4TableItem(
          dataCell = TRUE, 
          "test"
        ),
        bs4TableItem(
          actionButton(
            "go",
            "Go"
          )
        )
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
        href = "http://www.google.fr"
      ),
      listGroupItem(
        active = FALSE, 
        disabled = FALSE, 
        "Dapibus ac facilisis in",
        href = "http://www.google.fr"
      ),
      listGroupItem(
        "Morbi leo risus",
        active = FALSE, 
        disabled = TRUE, 
        href = "http://www.google.fr"
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