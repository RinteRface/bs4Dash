# Load packages
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(plotly)

# plot 2
x <- seq(-2 * pi, 2 * pi, length.out = 1000)
df <- data.frame(x, y1 = sin(x), y2 = cos(x))

# plot 3
x <- rnorm(200)
y <- rnorm(200)


basic_cards_tab <- bs4TabItem(
  tabName = "cards",
  fluidRow(
    bs4Card(
      title = "Closable card with dropdown", 
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
        dropdownItem(url = "https://www.google.com", name = "Link to google"),
        dropdownItem(url = "#", name = "item 2"),
        dropdownDivider(),
        dropdownItem(url = "#", name = "item 3")
      ),
      plotOutput("plot")
    ),
    bs4Card(
      title = "Closable card with gradient", 
      closable = TRUE, 
      width = 6,
      status = "warning", 
      solidHeader = FALSE, 
      gradientColor = "success",
      collapsible = TRUE,
      plotOutput("distPlot")
    ),
    bs4Card(
      title = "Card with solidHeader and elevation", 
      elevation = 4,
      closable = TRUE, 
      width = 6,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
      plot_ly(z = ~volcano) %>% add_surface()
    )
  )
)


social_cards_tab <- bs4TabItem(
  tabName = "socialcards",
  fluidRow(
    bs4UserCard(
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
      status = "info",
      title = "User card type 1",
      subtitle = "a subtitle here",
      elevation = 4,
      "Any content here"
    ),
    bs4UserCard(
      type = 2,
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
      status = "success",
      imageElevation = 4,
      title = "User card type 2",
      subtitle = "a subtitle here",
      bs4ProgressBar(
        value = 5,
        striped = FALSE,
        status = "info"
      ),
      bs4ProgressBar(
        value = 20,
        striped = TRUE,
        status = "warning"
      )
    )
  ),
  fluidRow(
    bs4SocialCard(
      title = "Social Card",
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
    ),
    bs4Card(
      title = "Box with user comment",
      status = "primary",
      userPost(
        id = 1,
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
        author = "Jonathan Burke Jr.",
        description = "Shared publicly - 7:30 PM today",
        "Lorem ipsum represents a long-held tradition for designers, 
       typographers and the like. Some people hate it and argue for 
       its demise, but others ignore the hate as they create awesome 
       tools to help create filler text for everyone from bacon 
       lovers to Charlie Sheen fans.",
        userPostTagItems(
          userPostTagItem(bs4Badge("item 1", status = "warning")),
          userPostTagItem(bs4Badge("item 2", status = "danger"))
        )
      ),
      userPost(
        id = 2,
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
        author = "Adam Jones",
        description = "Shared publicly - 5 days ago",
        userPostMedia(src = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
        userPostTagItems(
          userPostTagItem(bs4Badge("item 1", status = "info")),
          userPostTagItem(bs4Badge("item 2", status = "danger"))
        )
      )
    )
  ),
  fluidRow(
    bs4Card(
      status = "primary",
      width = 3,
      solidHeader = TRUE,
      cardProfile(
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
        title = "Nina Mcintire",
        subtitle = "Software Engineer",
        cardProfileItemList(
          bordered = TRUE,
          cardProfileItem(
            title = "Followers",
            description = 1322
          ),
          cardProfileItem(
            title = "Following",
            description = 543
          ),
          cardProfileItem(
            title = "Friends",
            description = 13287
          )
        )
      )
    ),
    bs4Card(
      title = "Card with messages",
      width = 9,
      userMessages(
        width = 12,
        status = "success",
        userMessage(
          author = "Alexander Pierce",
          date = "20 Jan 2:00 pm",
          src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
          side = NULL,
          "Is this template really for free? That's unbelievable!"
        ),
        userMessage(
          author = "Dana Pierce",
          date = "21 Jan 4:00 pm",
          src = "https://adminlte.io/themes/AdminLTE/dist/img/user5-128x128.jpg",
          side = "right",
          "Indeed, that's unbelievable!"
        )
      )
    )
  )
)


tab_cards_tab <- bs4TabItem(
  tabName = "tabcards",
  fluidRow(
    column(
      width = 6,
      bs4TabCard(
        title = "A card with tabs",
        elevation = 2,
        id = "tabcard1",
        width = 12,
        bs4TabPanel(
          tabName = "Tab 1",
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
          tabName = "Tab 2",
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
          tabName = "Tab 3",
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
    column(
      width = 6,
      bs4TabCard(
        title = "A card with tabs",
        side = "right",
        elevation = 2,
        id = "tabcard2",
        width = 12,
        bs4TabPanel(
          tabName = "Tab 4",
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
          tabName = "Tab 5",
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
          tabName = "Tab 6",
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
    )
  ),
  fluidRow(
    # manually inserted panels
    column(
      width = 6,
      bs4TabSetPanel(
        id = "tabcard",
        side = "left",
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
      )
    ),
    
    # programmatically inserted panels
    column(
      width = 6,
      bs4TabSetPanel(
        id = "tabset",
        side = "left",
        tabStatus = "warning",
        .list = lapply(1:3, function(i) {
          bs4TabPanel(
            tabName = paste0("Tab", i), 
            active = FALSE,
            paste("Content", i)
          )
        })
      )
    )
  )
)



sortable_cards_tab <- bs4TabItem(
  tabName = "sortablecards",
  fluidRow(
    lapply(1:3, FUN = function(i) {
      bs4Sortable(
        width = 4,
        p(class = "text-center", paste("Column", i)),
        lapply(1:2, FUN = function(j) {
          bs4Card(
            title = paste0("I am the ", j,"-th card of the ", i, "-th column"), 
            width = 12,
            "Click on my header"
          )
        })
      )
    })
  )
)



statsboxes_tab <- bs4TabItem(
  tabName = "statsboxes",
  fluidRow(
    bs4Card(
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
            number_color = "success", 
            number_icon = "fa fa-caret-up",
            header = "$35,210.43", 
            text = "TOTAL REVENUE", 
            right_border = TRUE,
            margin_bottom = FALSE
          )
        ),
        column(
          width = 6,
          descriptionBlock(
            number = "18%", 
            number_color = "danger", 
            number_icon = "fa fa-caret-down",
            header = "1200", 
            text = "GOAL COMPLETION", 
            right_border = FALSE,
            margin_bottom = FALSE
          )
        )
      )
    ),
    bs4Card(
      title = "Box with right pad",
      status = "warning",
      fluidRow(
        column(width = 6),
        column(
          width = 6,
          cardPad(
            color = "info",
            descriptionBlock(
              header = "8390", 
              text = "VISITS", 
              right_border = FALSE,
              margin_bottom = TRUE
            ),
            descriptionBlock(
              header = "30%", 
              text = "REFERRALS", 
              right_border = FALSE,
              margin_bottom = TRUE
            ),
            descriptionBlock(
              header = "70%", 
              text = "ORGANIC", 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          )
        )
      )
    )
  )
)



boxes_tab <- bs4TabItem(
  tabName = "boxes",
  fluidRow(
    bs4Box(
      height = "600px",
      title = "Box 1",
      bs4Ribbon(
        text = "Plot 1",
        status = "success"
      ),
      plotlyOutput("plot2")
    ),
    bs4Box(
      height = "600px",
      title = "Box 2",
      bs4Ribbon(
        text = "Plot 2",
        status = "danger",
        size = "xl"
      ),
      plotlyOutput("plot3")
    )
  )
)




value_boxes_tab <- bs4TabItem(
  tabName = "valueboxes",
  h4("Value Boxes"),
  fluidRow(
    bs4ValueBox(
      value = 150,
      subtitle = "New orders",
      status = "primary",
      icon = "shopping-cart",
      href = "#"
    ),
    bs4ValueBox(
      elevation = 4,
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
    ),
    bs4ValueBox(
      value = "53%",
      subtitle = "Bounce rate",
      status = "success",
      icon = "database"
    )
  ),
  h4("Info Boxes"),
  fluidRow(
    bs4InfoBox(
      title = "Messages",
      value = 1410,
      icon = "envelope"
    ),
    bs4InfoBox(
      title = "Bookmarks",
      status = "info",
      value = 240,
      icon = "bookmark"
    ),
    bs4InfoBox(
      title = "Comments",
      gradientColor = "danger",
      value = 41410,
      icon = "comments"
    )
  )
)



gallery_1_tab <- bs4TabItem(
  tabName = "gallery1",
  fluidRow(
    bs4Card(
      title = "Accordions",
      footer = tagList(
        h4("There is an accordion in the footer!"),
        bs4Accordion(
          id = "accordion1",
          bs4AccordionItem(
            id = "item1",
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
          bs4AccordionItem(
            id = "item2",
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
    bs4Card(
      title = "Carousel",
      bs4Carousel(
        id = "mycarousel",
        width = 12,
        bs4CarouselItem(
          active = TRUE,
          src = "https://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "https://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "https://placehold.it/900x500/f39c12/ffffff&text=I+Love+Bootstrap"
        )
      )
    )
  ),
  fluidRow(
    bs4Card(
      title = "bs4Quote",
      fluidRow(
        bs4Quote("Blablabla", status = "indigo"),
        bs4Quote("Blablabla", status = "danger"),
        bs4Quote("Blablabla", status = "teal"),
        bs4Quote("Blablabla", status = "orange"),
        bs4Quote("Blablabla", status = "warning"),
        bs4Quote("Blablabla", status = "fuchsia")
      )
    )
  ),
  fluidRow(
    bs4Card(
      title = "Progress bars",
      footer = tagList(
        bs4ProgressBar(
          value = 5,
          striped = FALSE,
          status = "info"
        ),
        bs4ProgressBar(
          value = 20,
          striped = TRUE,
          status = "warning"
        )
      ),
      bs4ProgressBar(
        value = 80,
        vertical = TRUE,
        status = "success"
      ),
      bs4ProgressBar(
        value = 100,
        vertical = TRUE,
        striped = TRUE,
        status = "danger"
      )
    ),
    bs4Card(
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
    bs4Card(
      title = "Callouts",
      bs4Callout(
        title = "I am a danger callout!",
        elevation = 4,
        status = "danger",
        width = 12,
        "There is a problem that we need to fix. 
                A wonderful serenity has taken possession of 
                my entire soul, like these sweet mornings of 
                spring which I enjoy with my whole heart."
      )
    ),
    bs4Card(
      title = "Loading State",
      bs4Loading()
    )
  ),
  fluidRow(
    bs4Card(
      title = "Timeline",
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
          bs4TimelineItemMedia(src = "https://placehold.it/150x100"),
          bs4TimelineItemMedia(src = "https://placehold.it/150x100")
        ),
        bs4TimelineStart(status = "danger")
      )
    ),
    bs4Timeline(
      width = 6,
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
        bs4TimelineItemMedia(src = "https://placehold.it/150x100"),
        bs4TimelineItemMedia(src = "https://placehold.it/150x100")
      ),
      bs4TimelineStart(status = "danger")
    )
  ),
  br(),
  fluidRow(
    bs4Card(
      title = "Stars",
      bs4Stars(grade = 5),
      bs4Stars(grade = 5, status = "success"),
      bs4Stars(grade = 1, status = "danger"),
      bs4Stars(grade = 3, status = "info")
    ),
    bs4Card(
      title = "Attachment example",
      attachmentBlock(
        src = "https://adminlte.io/themes/dev/AdminLTE/dist/img/photo1.png",
        title = "Test",
        title_url = "http://google.com",
        "This is the content"
      )
    )
  )
)



gallery_2_tab <- bs4TabItem(
  tabName = "gallery2",
  bs4Jumbotron(
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
    bs4Card(
      title = "Badges",
      bs4Badge(status = "secondary", "blabla", rounded = TRUE),
      bs4Badge(status = "info", "blabla", rounded = TRUE)
    )
  ),
  
  br(),
  
  h4("BS4 list group"),
  fluidRow(
    bs4ListGroup(
      bs4ListGroupItem(
        type = "basic",
        "Cras justo odio"
      ),
      bs4ListGroupItem(
        type = "basic",
        "Dapibus ac facilisis in"
      ),
      bs4ListGroupItem(
        type = "basic",
        "Morbi leo risus"
      )
    ),
    bs4ListGroup(
      bs4ListGroupItem(
        "Cras justo odio",
        active = TRUE, 
        disabled = FALSE, 
        type = "action",
        src = "https://www.google.fr"
      ),
      bs4ListGroupItem(
        active = FALSE, 
        disabled = FALSE, 
        type = "action",
        "Dapibus ac facilisis in",
        src = "https://www.google.fr"
      ),
      bs4ListGroupItem(
        "Morbi leo risus",
        active = FALSE, 
        disabled = TRUE, 
        type = "action",
        src = "https://www.google.fr"
      )
    ),
    bs4ListGroup(
      bs4ListGroupItem(
        "Donec id elit non mi porta gravida at eget metus. 
                Maecenas sed diam eget risus varius blandit.",
        active = TRUE, 
        disabled = FALSE, 
        type = "heading",
        title = "List group item heading", 
        subtitle = "3 days ago", 
        footer = "Donec id elit non mi porta."
      ),
      bs4ListGroupItem(
        "Donec id elit non mi porta gravida at eget metus. 
                Maecenas sed diam eget risus varius blandit.",
        active = FALSE, 
        disabled = FALSE, 
        type = "heading",
        title = "List group item heading", 
        subtitle = "3 days ago", 
        footer = "Donec id elit non mi porta."
      )
    )
  )
)