# This is a Shiny web application for the developer test at Appsilon.

# load libraries
library(shiny)
library(shiny.semantic)
library(shinyjs)
library(leaflet)
library(readr)
library(geodist)
library(tidyverse)

# load all data at once
#data <- read_csv("data/ships_04112020.zip")
# calculate distance in meters between observations
#ship_data <- data %>%
#    group_by(SHIP_ID) %>%
#    arrange(DATETIME, .by_group = T) %>%
#    mutate(obs_dist = geodist(cbind(LON,LAT), sequential = T, pad = T, measure = "geodesic"))

ship_data <- read_csv("data/ship_data_distance.zip")

# setting grid template layout
myGridTemplate <- grid_template(
    default = list(
        areas = rbind(
            c("title", "map"),
            c("menus", "map"),
            c("info", "map")
        ),
        cols_width = c("300px", "1fr"),
        rows_height = c("60px", "150px", "auto")
    ),
    mobile = list(
        areas = rbind(
            "title",
            "menus",
            "info",
            "map"
        ),
        rows_height = c("50px", "100px", "200px", "auto"),
        cols_width = c("100%")
    )
)

# modules
# vessel type dropdown module
# ui module
dropdown_uimodule <- function(id, ship_data = ship_data) {
    ns <- NS(id)
    tagList(
        dropdown_input(
            input_id = ns("vessel_type"),
            default_text = "Vessel Type",
            value = NULL,
            type = "selection fluid",
            choices = unique(ship_data$ship_type)
        ),
        dropdown_input(
            input_id = ns("vessel"),
            default_text = "Vessel",
            value = NULL,
            type = "selection fluid",
            choices = ""
        )
    )
}
# server module
dropdown_servermodule <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            observeEvent(input$vessel_type, {
                vessel_names = ship_data$SHIPNAME[ship_data$ship_type %in% input$vessel_type]
                update_dropdown_input(session, "vessel", choices = unique(vessel_names))
            })
            return(reactive({input$vessel}))
        }
    )
}

# Define UI for application
ui <- semanticPage(
    theme = "superhero",
    margin = "0px",
    grid(
        myGridTemplate,

        # Page title
        title = h1("Marine Data"),

        menus = div(
            dropdown_uimodule("dropdown_input", ship_data)
        ),

        map = leafletOutput("marine_map", height = "100%"),

        info = div(
            textOutput("dist_sailed"), br(),
            textOutput("destination"), br(),
            textOutput("speed")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    # map output
    output$marine_map <- renderLeaflet({
        leaflet(ship_data) %>%
            addTiles(group = "OSM", options = tileOptions(minZoom = 1, maxZoom = 17)) %>%
            fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    })

    # functions
    ship_filter <- reactive({
        sel_ship <- ship_data %>%
            filter(SHIPNAME %in% vessel_val()) %>%
            arrange(DATETIME)

        end_position <- sel_ship %>%
            filter(obs_dist == max(obs_dist, na.rm = T)) %>%
            arrange(desc(DATETIME)) %>%
            filter(row_number() == 1) %>%
            mutate(popup = "Terminal")

        start_position <- sel_ship[which(sel_ship$DATETIME == end_position$DATETIME) - 1,]
        start_position$popup <- "Initial"
        return(rbind(start_position,end_position))
    })

    map_render <- reactive({
        leafletProxy("marine_map", data = ship_filter()) %>%
            clearMarkers() %>%
            addMarkers(lng = ~LON, lat = ~LAT, label = ~as.character(DATETIME)) %>%
            fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    })

    info_render <- reactive({
        info_txt <- data.frame(
            dist_sailed = paste0("Distance sailed in meters: ",round(ship_filter()[2,"obs_dist"],2)),
            destination = paste0("Destination: ",ship_filter()[2,"DESTINATION"]),
            speed = paste0("Speed in knots: ",ship_filter()[2,"SPEED"])
        )
        return(info_txt)
    })

    # vessel values
    # update vessel based on type selected
    vessel_val <- dropdown_servermodule("dropdown_input")

    # filter the ships by selected and get most recent longest distance
    observeEvent(vessel_val(),{
        req(vessel_val())
        #filter by ship
        ship_filter()
        #re-render map
        map_render()
        #change information
        output$dist_sailed <- renderText({info_render()$dist_sailed})
        output$destination <- renderText({info_render()$destination})
        output$speed <- renderText({info_render()$speed})
    })
}

# Run the application
shinyApp(ui = ui, server = server)
