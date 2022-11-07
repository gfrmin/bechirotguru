library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)


ui <- fluidPage(
    titlePanel("Knesset (Israeli parliament) Election results / תוצאות הבחירות לכנסת"),
    hr(),
    fluidRow(
        column(4,
               selectInput(
                   "election",
                   "Knesset / כנסת",
                   25:15,
                   "25",
                   selectize=FALSE
               )
               ),
        column(4,
               selectInput(
                   "party",
                   "Party / מפלגה",
                   "ALL",
                   "ALL",
                   selectize=FALSE
               )
               ),
        column(4,
               selectizeInput(
                   "place",
                   "Place / ישוב",
                   "כל הישובים",
                   "כל הישובים"
               )
               ),
        ),
    tabsetPanel(type = "tabs",
                tabPanel("Map", leafletOutput("bechirotmap", height = "66vh")),
                tabPanel("Results history / תוצאות לאורך זמן", plotlyOutput("placegraph")),
                tabPanel("Top votes / התוצאות המובילות", tableOutput("partytable")),
                ),
    fluidRow(
        column(12, tags$div(id="cite",
        'Created by ', tags$a('Guy Freeman', href='https://www.linkedin.com/in/guyfreemanstat'), ' from ', tags$a('public data', href='https://publicdatamarket.com/israeldata/bechirot'), '. ', tags$a('Source', href='https://github.com/gfrmin/bechirotguru'))
      )
    )
)

server <- function(input, output) {
    bechirot_wide_places <- st_read("bechirot_wide_places.sqlite")
    bechirot_long_places <- bechirot_wide_places %>% pivot_longer(-c(semel_yishuv, shem_yishuv, shem_yishuv_english, GEOMETRY)) %>% filter(!is.na(value)) %>% separate(name, into = c("name", "elections"), sep = "_") %>% mutate(percent = 100*value) %>% mutate(logpercent = log(1 + percent)) %>% arrange(desc(value))

    electionslist <- unique(bechirot_long_places$elections) %>% sort(decreasing = TRUE)
    placeslist <- bechirot_wide_places %>% select(semel_yishuv, shem_yishuv, shem_yishuv_english) %>% unique

    observe({
        election <- input$election
        current_data <- bechirot_long_places %>% filter(elections == input$election)
        updateSelectInput(
            inputId = "party",
            choices = c("ALL", unique(current_data$name) %>% sort()),
            selected = "ALL"
        )
    })

    output$placegraph <- renderPlotly({
        place_votes <- bechirot_long_places
        if (input$place != "כל הישובים" & input$party == "ALL") {
            place_votes <- place_votes %>% filter(!name %in% c("cancelled", "didntvote")) %>% filter(shem_yishuv == input$place)
            main_parties <- place_votes %>% filter(elections >= 24) %>% filter(percent >= 3) %>% pull(name) %>% unique
            place_votes <- place_votes %>% filter(name %in% main_parties)
        } else if (input$place != "כל הישובים" & input$party != "ALL") {
            place_votes <- place_votes %>% filter(shem_yishuv == input$place) %>% filter(name == input$party)
        }
        if (input$place != "כל הישובים") {
            ggplotly(ggplot(place_votes) + geom_line(aes(elections, percent, colour = name, group = name, text = paste0(name, ": ", round(percent, 1))), size = 1) + xlab("Election") + ylab("%") + labs(colour = "Party") + theme_bw(base_size = 18), tooltip = "text")
        }
    })

    output$partytable <- renderTable({
        party_votes <- bechirot_long_places %>% as_tibble %>% filter(elections == input$election) %>% select(shem_yishuv_english, shem_yishuv, name, percent)
        if (input$place == "כל הישובים" & input$party == "ALL") {
            party_votes <- party_votes %>% filter(!name %in% c("cancelled", "didntvote"))
        }
        else if (input$place == "כל הישובים" & input$party != "ALL") {
            party_votes <- party_votes %>% filter(name == input$party)
        } else if (input$place != "כל הישובים" & input$party == "ALL") {
            party_votes <- party_votes %>% filter(shem_yishuv == input$place)
        } else {
            party_votes <- party_votes %>% filter(shem_yishuv == input$place) %>% filter(name == input$party)
        }
        party_votes %>% rename(`ישוב` = shem_yishuv, `Place` = shem_yishuv_english, `Party / מפלגה` = name, `%` = percent) %>% head(10) %>% mutate(`%` = round(`%`, 1))
    })

    output$bechirotmap <- renderLeaflet({
        leaflet() %>%
            setView(34.8788452148438, 32.1384086967725, 8) %>%
            addTiles()
    })

    observe({
        event <- input$bechirotmap_shape_click
        if (is.null(event)) {
            updateSelectizeInput(
                inputId = "place",
                selected = "כל הישובים",
                choices = c("כל הישובים", placeslist %>% pull(shem_yishuv) %>% sort),
                server = TRUE,
                options = list(maxOptions = 2000)
            )
        } else {
            updateSelectizeInput(
                inputId = "place",
                selected = placeslist %>% filter(semel_yishuv == event$id) %>% pull(shem_yishuv),
                choices = c("כל הישובים", placeslist %>% pull(shem_yishuv) %>% sort),
                server = TRUE
            )
        }
    })

    show_city_popup <- function(semel, lat, lng) {
        current_data <- bechirot_long_places %>% filter(elections == input$election) %>% filter(semel_yishuv == semel) %>% as_tibble
        current_votes <- current_data %>% select(party = name, percent) %>% mutate(percentround = round(percent, 1)) %>% filter(percentround > 0) %>% transmute(partytext = str_c(party, ": ", percentround, "%")) %>% unlist %>% paste(collapse = "<br/>")
        place_hebrew <- current_data %>% pull(shem_yishuv) %>% unique
        place_english <- current_data %>% pull(shem_yishuv_english) %>% unique
        content <- str_glue("<strong>{place_english} {place_hebrew}</strong><br/>{current_votes}")
        leafletProxy("bechirotmap") %>% addPopups(lng, lat, content, layerId = semel)
    }

    observe({
        shem_yishuv_chosen <- input$place
        leafletProxy("bechirotmap") %>% clearPopups()
        placechosen <- placeslist %>% filter(shem_yishuv == shem_yishuv_chosen)
        if (nrow(placechosen) > 0) {
            isolate({
                placechosen_semel <- placechosen %>% pull(semel_yishuv)
                placecentre <- placechosen %>% st_geometry %>% st_centroid %>% st_coordinates
                show_city_popup(placechosen_semel, placecentre[2], placecentre[1])
            })
        }
    })

    observe({
        party_chosen <- input$party
        leafletProxy("bechirotmap") %>% clearShapes() %>% clearControls()
        if (party_chosen == "ALL") {
            bechirot_winners <- bechirot_long_places %>% group_by(semel_yishuv, elections) %>% filter(value == max(value)) %>% ungroup
            current_winners <- bechirot_winners %>% filter(elections == input$election)
            winning_parties <- unique(current_winners$name)
            winning_parties_number <- length(winning_parties)
            winnerpal <- colorFactor(hcl.colors(winning_parties_number, palette = "YlGnBu"), winning_parties)
            leafletProxy("bechirotmap", data = current_winners) %>%
                addPolygons(
                    fillColor = ~winnerpal(name),
                    weight = 1,
                    opacity = 0.9,
                    fillOpacity = 0.9,
                    color = "white",
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
                    pal = winnerpal,
                    values = ~name,
                    opacity = 1,
                )
        } else {
            current_data <- bechirot_long_places %>% filter(name == party_chosen) %>% filter(elections == input$election)
            vote_colours <- colorNumeric("Blues", current_data$logpercent)
            leafletProxy("bechirotmap", data = current_data) %>%
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
        }
    })
}

shinyApp(ui, server)
