library(shiny)
library(tidyverse)
library(leaflet)
library(sf)

bechirot_wide_places <- st_read("bechirot_wide_places.sqlite")

bechirot_long_places <- bechirot_wide_places %>% pivot_longer(-c(semel_yishuv, shem_yishuv, shem_yishuv_english, GEOMETRY)) %>% filter(!is.na(value)) %>% separate(name, into = c("name", "elections"), sep = "_") %>% mutate(percent = 100*value) %>% mutate(logpercent = log(1 + percent))

electionslist <- unique(bechirot_long_places$elections) %>% sort(decreasing = TRUE)

ui <- fluidPage(
    titlePanel("Knesset (Israeli parliament) Election results / תוצאות הבחירות לכנסת"),
    leafletOutput("bechirotmap", height = "66vh"),
    hr(),
    fluidRow(
        column(6,
               selectInput(
                   "election",
                   "Knesset / כנסת",
                   electionslist,
                   "25",
                   selectize=FALSE
               )
               ),
        column(6,
               selectInput(
                   "party",
                   "Party / מפלגה",
                   c("didntvote"),
                   "didntvote",
                   selectize=FALSE
               )
               )
    ),
    fluidRow(
        column(12, tags$div(id="cite",
        'Created by ', tags$a('Guy Freeman', href='https://www.linkedin.com/in/guyfreemanstat'), ' from ', tags$a('public data', href='https://publicdatamarket.com/israeldata/bechirot'))
      )
    )
)

server <- function(input, output) {
    observe({
        election <- input$election
        current_data <- bechirot_long_places %>% filter(elections == input$election)
        updateSelectInput(
            inputId = "party",
            choices = unique(current_data$name) %>% sort(),
            selected = "didntvote"
        )
    })

    output$bechirotmap <- renderLeaflet({
        current_data <- bechirot_long_places %>% filter(name == input$party) %>% filter(elections == input$election)
        current_city <- input$map_shape_click$properties
        vote_colours <- colorNumeric("Blues", current_data$logpercent)
        leaflet(data = current_data) %>%
            setView(34.8788452148438, 32.1384086967725, 8) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~vote_colours(logpercent),
                weight = 1,
                opacity = 0.9,
                fillOpacity = 0.9,
                layerId = ~semel_yishuv,
                highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#000",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                )
            ) %>%
            addLegend(
                title = "%",
                pal = vote_colours,
                values = ~logpercent,
                opacity = 1,
                labFormat = labelFormat(
                    digits = 0,
                    transform = function(x) exp(x - 1)
                )
            )
    })

    show_city_popup <- function(semel, lat, lng) {
        current_data <- bechirot_long_places %>% filter(elections == input$election) %>% filter(semel_yishuv == semel) %>% as_tibble
        current_votes <- current_data %>% select(party = name, percent) %>% mutate(percentround = round(percent, 1)) %>% filter(percentround > 0) %>% arrange(desc(percent)) %>% transmute(partytext = str_c(party, ": ", percentround, "%")) %>% unlist %>% paste(collapse = "<br/>")
        place_hebrew <- current_data %>% pull(shem_yishuv) %>% unique
        place_english <- current_data %>% pull(shem_yishuv_english) %>% unique
        content <- str_glue("<strong>{place_english} {place_hebrew}</strong><br/>{current_votes}")
        leafletProxy("bechirotmap") %>% addPopups(lng, lat, content, layerId = semel)
    }

    observe({
        leafletProxy("bechirotmap") %>% clearPopups()
        event <- input$bechirotmap_shape_click
        if (is.null(event))
            return()
        isolate({
            show_city_popup(event$id, event$lat, event$lng)
        })
    })

}

shinyApp(ui, server)
