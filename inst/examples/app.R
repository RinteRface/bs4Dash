library(shiny)
library(fontawesome)
library(shinyWidgets)
library(bs4Dash)
library(plotly)



shiny::shinyApp(
  ui = bs4DashPage(
    old_school = TRUE,
    navbar = bs4DashNavbar(
      status = "white",
      "I can write text in the navbar!",
      rightUi = bs4DropdownMenu(
        show = TRUE,
        labelText = "!",
        status = "danger",
        src = "http://www.google.fr",
        bs4DropdownMenuItem(
          text = "message 1",
          date = "today"
        ),
        bs4DropdownMenuItem(
          text = "message 2",
          date = "yesterday"
        )
      )
    ),
    sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      title = "bs4Dash",
      brandColor = "primary",
      url = "http://www.google.fr",
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      elevation = 3,
      opacity = 0.8,
      bs4SidebarMenu(
        bs4SidebarHeader("Main content"),
        bs4SidebarMenuItem(
          "Basic cards",
          tabName = "cards",
          icon = "sliders",
          active = TRUE
        ),
        # bs4SidebarMenuItemList(
        #   name = "Cards",
        #   icon = "gears",
        #   open = TRUE,
        #   active = FALSE,
        #   bs4SidebarMenuItem(
        #     "Basic cards",
        #     tabName = "cards",
        #     icon = "sliders",
        #     active = TRUE
        #   ),
        #   bs4SidebarMenuItem(
        #     "User cards",
        #     tabName = "usercards",
        #     icon = "object-ungroup",
        #     active = FALSE
        #   ),
        #   bs4SidebarMenuItem(
        #     "Tab cards",
        #     tabName = "tabcards",
        #     icon = "picture-o",
        #     active = FALSE
        #   ),
        #   bs4SidebarMenuItem(
        #     "Value/Info boxes",
        #     tabName = "valueboxes",
        #     icon = "suitcase",
        #     active = FALSE
        #   )
        # ),
        bs4SidebarHeader("Bonus"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Other stuff", 
              bs4Badge(
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
          "BS4 extras",
          tabName = "test",
          icon = "map",
          active = FALSE
        )
      )
    ),
    body = bs4DashBody(
      bs4TabItems(
        bs4TabItem(
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
                dropdownItem(url = "http://www.google.com", name = "Link to google"),
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
        ),
        bs4TabItem(
          tabName = "usercards",
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
                value = 5,
                striped = TRUE,
                status = "warning",
                width = "20%"
              )
            )
          )
        ),
        bs4TabItem(
          tabName = "tabcards",
          bs4TabCard(
            title = "A card with tabs",
            elevation = 2,
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
        bs4TabItem(
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
        ),
        bs4TabItem(
          tabName = "bonus",
          fluidRow(
            bs4Card(
              title = "Accordions",
              footer = tagList(
                h4("There is an accordion in the footer!"),
                bs4Accordion(
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
                  src = "http://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap"
                ),
                bs4CarouselItem(
                  active = FALSE,
                  src = "http://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap"
                ),
                bs4CarouselItem(
                  active = FALSE,
                  src = "http://placehold.it/900x500/f39c12/ffffff&text=I+Love+Bootstrap"
                )
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
                  value = 5,
                  striped = TRUE,
                  status = "warning",
                  width = "20%"
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
                status = "danger",
                height = "80%"
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
                  bs4TimelineItemMedia(src = "http://placehold.it/150x100"),
                  bs4TimelineItemMedia(src = "http://placehold.it/150x100")
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
                bs4TimelineItemMedia(src = "http://placehold.it/150x100"),
                bs4TimelineItemMedia(src = "http://placehold.it/150x100")
              ),
              bs4TimelineStart(status = "danger")
            )
          ),
          fluidRow(
            bs4Card(
              title = "Stars",
              bs4Stars(grade = 5),
              bs4Stars(grade = 5, status = "success"),
              bs4Stars(grade = 1, status = "danger"),
              bs4Stars(grade = 3, status = "info")
            )
          )
        ),
        bs4TabItem(
          tabName = "test",
          bs4Jumbotron(
            title = "I am a Jumbotron!",
            lead = "This is a simple hero unit, a simple jumbotron-style 
                    component for calling extra attention to featured 
                    content or information.",
            "It uses utility classes for typography and spacing 
            to space content out within the larger container.",
            status = "primary",
            href = "http://www.google.fr"
          ),
          
          br(),
          
          h4("Rounded Badges"),
          fluidRow(
            bs4Badge(status = "secondary", "blabla", rounded = TRUE),
            bs4Badge(status = "dark", "blabla", rounded = TRUE)
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
                src = "http://www.google.fr"
              ),
              bs4ListGroupItem(
                active = FALSE, 
                disabled = FALSE, 
                type = "action",
                "Dapibus ac facilisis in",
                src = "http://www.google.fr"
              ),
              bs4ListGroupItem(
                "Morbi leo risus",
                active = FALSE, 
                disabled = TRUE, 
                type = "action",
                src = "http://www.google.fr"
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
      )
    ),
    controlbar = bs4DashControlbar(
      skin = "light",
      title = "My right sidebar",
      setSliderColor(sliderId = 1, "black"),
      sliderInput("obs", "Number of observations:",
                  min = 0, max = 1000, value = 500
      ),
      column(
        width = 12,
        align = "center",
        radioButtons(
          "dist", 
          "Distribution type:",
          c("Normal" = "norm",
            "Uniform" = "unif",
            "Log-normal" = "lnorm",
            "Exponential" = "exp")
        )
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