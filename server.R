library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)
library(leaflet.extras)
library(DT)
library(echarts4r)
library(glue)
library(sf)
library(reactable)
library(shinymanager)


# Securing the application and commenting it out for now

# credentials <- data.frame(
#   user = c("shiny", "shinymanager"),
#   password = c("azerty", "12345"),
#   stringsAsFactors = FALSE
# )


# Loading the whole data
species <- read.csv("data/eDNA_Detections_1.csv")

# Just picking up the selected columns to show it on the map as a different layer
all_sites <- species[species$Data_Source == "eDNA", c("Species", "Sites", "Latitude", "Longitude", "Class", "Data_Source")]

# Marked areas of Protected Areas site. Source: Canadian Wildlife website
marked_areas <- st_read("data/CPCAD2020_ATL_NWAsMBSFedRealPro.gpkg")

# Adding bat detections source in histogram

bats <- read.csv("data/bats.csv")

# Adding bats count for day

bats_detections_day <- read.csv("data/fine_scale_day.csv")

# Adding bats count for week
bats_detections_week <- read.csv("data/fine_scale_week.csv")


server <- function(input, output, session) {

  # Wrapping the credentials before the app is executed
  # Commenting it out for now

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  # Changing into regular function
  # creating a reactive function to populate user's input as per their desired  ----
  filter_species <- function(Site, class, taxon)(
    # Using if-else statement because we want site data when only site is selected and
    # when both checkmark and site is selected then it should only show the filtered data
    # !is.null will check that input$site is not NULL, if it is not then will return TRUE and function will start executing it

    if (!is.null(Site) && !is.null(class) && !is.null(taxon)) {
      species_data <- dplyr::filter(
        species,
        Sites %in% Site,
        Class %in% class,
        Taxon %in% taxon
      )
    } else if (!is.null(Site) && !is.null(class)) {
      # Adding one more filter of Class where user selects any checkmark then it will show the output
      species_data <- dplyr::filter(
        species,
        Sites %in% Site,
        Class %in% class
      )
    } else if (!is.null(Site) && !is.null(taxon)) {
      species_data <- dplyr::filter(
        species,
        Sites %in% Site,
        Taxon %in% taxon
      )
    }

    # Only when site is selected
    else if (!is.null(Site)) {
      species_data <- dplyr::filter(
        species,
        Sites %in% Site
      )
    }

    # if no site is selected and only class then this one will triggered
    else if (!is.null(class)) {
      species_data <- dplyr::filter(
        species,
        Class %in% class
      )
    } else if (!is.null(taxon)) {
      species_data <- dplyr::filter(
        species,
        Taxon %in% taxon
      )
    }

    # If nothing is selected then it will print whole data
    else {
      species_data <- species
    }

  )

  # Specifying the reactive function for Site, class and taxon

  species_filter <- reactive({
    filter_species(input$Site, input$class, input$taxon)
  })






  # matching the user's input to the common species name in the table ----

  filtered_species <- reactive({
    # Calling filtered data from the table output and then using that data to show on the map !!!Important
    filtered_species_map <- species_filter()

    # In this line of code means that from species_data dataframe it will only include rows where value of common name is equal to input of species name
    # I changed the code to %in% instead of == because "equals to" will need the same object length while %in% will be okay and it will just check the names
    filtered_species_map[filtered_species_map$Species %in% input$Species_Name, ]
  })




  # Once the matching species are found then we have matched them to their latitudes and longitudes ----

  lat <- reactive({
    filtered_species()$Latitude
  })

  lon <- reactive({
    filtered_species()$Longitude
  })


  # Getting the filtered class name according to the basis of Site. If Site does not have mammals then there will be no mammals in the dropdown menu of Class ----

  observeEvent(c(input$Site), {
    updateSelectInput(session, "class", "Select class(es)", sort(unique(species_filter()$Class)))
  })

  # Taxon dropdown ----

  observeEvent(c(input$Site, input$class), {
    # VERY IMPORTANT using the session first because updateselect works on current user session and updates the input control associated with the session
    # It updates in the current session of the app which can also saves time for the app
    updateSelectInput(session, "taxon", "Select Taxon:", sort(unique(species_filter()$Taxon)))
  })


  # Using this input in server in order to change the species name in the dropdown. Unique way to show this output in UI ----

  output$species_dropdown <- renderUI({
    selectInput("Species_Name", "Select any species name from the list:", choices = sort(unique(species_filter()$Species)), multiple = TRUE)
  })


  # showing the data in a tabular form for the species
  # stop the search option at the end of the data table output  ----
  output$speciestable <- renderReactable(

    # Changing into reactable format to make the dashboard faster when large datasets come in

    reactable(species_filter()[, 1:5], searchable = TRUE, paginationType = "jump",
              language = reactableLang(
                noData = "No entries found"
              ),
              theme = reactableTheme(
                backgroundColor = "#e6f2ff",
                borderColor = "#d9d9d9"
              ),
              class = "tableoutput"
              )
)


  # showing on the map with the help of leaflet  ----

  output$Specieswithnames <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
      # Using Topo map and using their options

      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      leaflet.extras::addResetMapButton() %>% # Adding reset button on map
      setView(lng = -63.5, lat = 45.32, zoom = 7) %>%
      # Adding a minimap to see the zoomed data
      addMiniMap(
        position = "bottomright",
        tiles = providers$Esri.WorldTopoMap,
        toggleDisplay = T,
        minimized = F
      ) %>%
      # Adding cosmetics
      addCircleMarkers(
        data = filtered_species(),
        lat = lat(),
        lng = lon(),
        color = "black",
        weight = 2,
        fillColor = "orange",
        fillOpacity = 1,
        popup = paste(
          "<br><b>Species Name:</b> ", filtered_species()$Species,
          "<br><b> Relative Abundance:</b> ", filtered_species()$Relative_Abundance,
          "<br><b> Island Name:</b>", filtered_species()$Sites
        ),
        clusterOptions = markerClusterOptions(),
        group = "Filtered-Species"
      ) %>%
      addPolygons(data = marked_areas, group = "Wildlife Areas") %>%
      # Adding this layer inside the leaflet and distinguish them with the help of groups and then added into addLayersControl ----
      addCircleMarkers(
        data = all_sites,
        lat = all_sites$Latitude,
        lng = all_sites$Longitude,
        color = "black",
        weight = 2,
        fillColor = "#A1A9D5",
        fillOpacity = 1,
        popup = paste(
          "<br><b> Sites:</b> ", all_sites$Sites,
          "<br><b>Species Name:</b> ", all_sites$Species,
          "<br><b> Class:</b> ", all_sites$Class
        ),
        clusterOptions = markerClusterOptions(), # if multiple markers then it will cluster the circles
        group = "eDNA Detections"
      ) %>%
      # Adding a user interface to control switch layers
      addLayersControl(
        overlayGroups = c("Filtered-Species", "Wildlife Areas"),
        baseGroups = c("Filtered-Species", "eDNA Detections", "Wildlife Areas"), # User will get chance to select what data they want to choose

        # Choose to permanently display or collapse layers control switch
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      # Adding the measurement tool if user wants to measure the distance between two island or for any other reference ----

      addMeasure(
        position = "bottomright",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters"
      ) %>%
      # Add scale bar
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(imperial = FALSE)
      )
  })

  # Using the observe Event in order to display the custom_bar_click for the back button and show the output in console too ----
  # In this way we can check if our code is emitting any wrong things or not


  observeEvent(input$custom_bar_click1, {
    # print(input$custom_bar_click1)
  })


  # Rendering the data for histogram ----

  output$specieshist <- renderEcharts4r({
    # print(input$custom_bar_click)

    # Getting the selected radio button option
    radio_option <- input$radio_option

    # Very important to understand the code over here because this is the code for specifying "back" button in the chart so that drill down comes back to orginal state
    # Source: https://appsilon.com/echarts4r-tutorial-drill-down-bar-chart-in-shiny/
    # e_on is the observer function to make thing clickable using javascript


    # Adding the esthetics part over here so it takes place for both level 1 and level 2 instead of assigning it individually

    plot_data <- function(data, chart_title, chart_color, chart_drill_to, chart_back_drill_to, filtered_place) {
      species_chart <- data %>%
        e_chart(x = site) %>% # Using X = site because defining site below for columns Sites and Species
        # e_bar(Relative_Abundance, color = chart_color) %>%
        e_bar(eDNA_Detections, stack = "total", name = paste0("eDNA source by ", filtered_place)) %>% # Using eDNA to show the filtered data according to eDNA and concatenating with island name
        e_bar(PAM_Detections, stack = "total", name = paste0("PAM Detections by ", filtered_place)) %>% # Using eDNA to show the filtered data according to eDNA and concatenating with island name
        e_title("Species Count by Site Name", left = "center") %>% # Adding title of the graph
        e_legend(show = TRUE, Position = "bottom", orient = "horizontal", top = "5%") %>% # Having legends below the graph
        e_theme("walden") %>% # Adding the same theme as Richness


        # Using this function to allow the user to download the histogram
        e_toolbox(
          show = TRUE,
          feature = list(
            saveAsImage = list(
              show = TRUE,
              title = "Save",
              name = "species_detections_histogram",
              type = "png",
              backgroundColor = "#ffffff",
              pixelRatio = 2
            )
          )
        ) %>%
        # showing the line and bar chart as per user's request
        e_toolbox_feature(
          feature = "magicType",
          type = list("line", "bar")
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_x_axis(
          type = "category",
          axisLabel = list(rotate = 26, fontsize = 5)
        ) %>% # Showing the site names in a categorical way
        e_grid(left = 100, right = 60, top = 70, bottom = 80) # Setting the grid of the histogram so that names on X-axis fit in one screen


      # Adding the click observer only when drill_to is passed
      if (!is.null(chart_drill_to)) {
        species_chart <- species_chart %>%
          e_on(
            query = "series.bar",
            # Set input values
            handler = glue::glue(
              "function(params){
             Shiny.setInputValue(
              'custom_bar_click',
              {clicked_level: 'level2', drilled_place: params.name}, {priority: 'event'}
             );
           }",
              .open = "<<", .close = ">>"
            ),
            event = "click"
          )
      }
      if (!is.null(chart_back_drill_to)) {
        if (is.null(filtered_place)) {
          observe_handler <- glue::glue(
            "function(params){
        Shiny.setInputValue(
          'custom_bar_click',
          {clicked_level: 'level1'},
          {priority: 'event'}
        );
      }",
            .open = "<<", .close = ">>"
          )
        } else {
          observe_handler <- glue::glue(
            "function(params){
          Shiny.setInputValue(
            'custom_bar_click',
            {clicked_level: 'level1', drilled_place: '<>'},
            {priority: 'event'}
          );
        }",
            .open = "<<", .close = ">>"
          )
        }

        species_chart <- species_chart %>%
          e_title("Back", triggerEvent = TRUE) %>%
          e_on(
            query = "title",
            # Set input values
            handler = observe_handler,
            event = "click"
          )
      }
      return(species_chart)
    }

    # Using the if-else statement because to make our radio buttons work and once the level 1 is clicked then it goes to level 2
    # and defining plot data for each "data" object

    if (radio_option == "Detections") {
      if (is.null(input$custom_bar_click$clicked_level) || input$custom_bar_click$clicked_level == "level1") {
        # !important Main reason for using eDNA and PAM differently because the output was having 4 values for the sites which has eDNA and PAM together
        # 4 values include eDNA source by 0, eDNA source by 120000, PAM source by 0, PAM source by 20000

        # Filtering the values individually for eDNA and grouping by Sites and then summarizing with counts

        data_eDNA <- species_filter() %>%
          filter(Data_Source == "eDNA") %>%
          group_by(site = Sites) %>%
          summarize(eDNA_Detections = sum(Relative_Abundance))


        # Filtering the values individually for PAM and grouping by Sites and then summarizing with counts

        data_PAM <- species_filter() %>%
          filter(Data_Source == "PAM") %>%
          group_by(site = Sites) %>%
          summarize(PAM_Detections = sum(Relative_Abundance))

        # Joining both the values eDNA and PAM by Sites and using coalesce which means that it will handle all the missing values
        # and then using distinct which will display unique values

        data <- left_join(data_eDNA, data_PAM, by = "site") %>%
          mutate(Relative_Abundance = coalesce(eDNA_Detections, 0) + coalesce(PAM_Detections, 0)) %>%
          distinct(site, Relative_Abundance, eDNA_Detections, PAM_Detections)

        # print("Hello")
        # Create chart
        plot_data(
          data = data,
          # chart_title = "Species Detections",
          chart_color = "#5470C6",
          chart_drill_to = "level2",
          chart_back_drill_to = NULL,
          filtered_place = NULL
        )


        # As soon as the bar is clicked then it goes to Level 2
        # Using sum in front of if-else statement because to provide unique values of eDNA and PAM otherwise it was giving 4 values with two having correct values
      } else if (input$custom_bar_click$clicked_level == "level2") {
        data <- species_filter() %>%
          filter(Sites == input$custom_bar_click$drilled_place) %>%
          group_by(site = Species) %>%
          summarize(
            Relative_Abundance = sum(Relative_Abundance),
            eDNA_Detections = sum(ifelse(Data_Source == "eDNA", Relative_Abundance, 0)), # !important used unique because to get the same number as relative abundance or else number will keep incrementing as the site changes
            PAM_Detections = sum(ifelse(Data_Source == "PAM", Relative_Abundance, 0)), .groups = "drop"
          ) %>% # same for PAM detections. Using .groups because it will drop the Sites and flattened output
          distinct()


        # print("Hello done")

        plot_data(
          data = data,
          # chart_title = glue::glue(
          #   "Species Detections (Filtered for {input$custom_bar_click$drilled_place})"
          # ),
          chart_color = "#91CC75",
          chart_drill_to = "level1",
          chart_back_drill_to = "level1",
          filtered_place = input$custom_bar_click$drilled_place
        )
      }
    }


    # Using the elseif statement if user select the option Richness

    else if (radio_option == "Richness") {
      # Filtering the values individually for PAM and grouping by Sites and then summarizing with number of species

      data_eDNA <- species_filter() %>%
        filter(Data_Source == "eDNA") %>%
        group_by(site = Sites) %>%
        summarize(eDNA_Detections = n_distinct(Species))


      # Filtering the values individually for PAM and grouping by Sites and then summarizing with number of species

      data_PAM <- species_filter() %>%
        filter(Data_Source == "PAM") %>%
        group_by(site = Sites) %>%
        summarize(PAM_Detections = n_distinct(Species))

      # Added the source Sonobat to visualize the data of species BATS

      data_BAT <- species_filter() %>%
        filter(Data_Source == "Sonobat") %>%
        group_by(site = Sites) %>%
        summarize(Sonobat = n_distinct(Species))



      # Joining both the values eDNA and PAM by Sites and using coalesce which means that it will handle all the missing values
      # and then using distinct which will display unique values

      unique_data <- left_join(data_eDNA, data_PAM, by = "site") %>%
        full_join(data_BAT, by = "site") %>%
        mutate(Total_Count = coalesce(eDNA_Detections, 0) + coalesce(PAM_Detections, 0) + coalesce(Sonobat, 0)) %>%
        distinct()

      # print(unique_data)

      e_charts(unique_data, site) %>%
        # e_bar(unique_species, stack = "total", name = "No. of Species") %>%             # creating bar chart and totalling by the object- unique_species provided by us
        e_bar(eDNA_Detections, stack = "total", name = "eDNA Source") %>% # adding into the bar chart to show only eDNA detections
        e_y_axis(type = "value") %>% # Assigning y-axis variable in value format
        e_bar(PAM_Detections, stack = "total", name = "PAM Detections") %>% # adding into the bar chart to show only PAM detections
        e_y_axis(type = "value") %>% # Assigning y-axis in value format because when user selects PAM Detection then scale should get change accordingly
        e_bar(Sonobat, stack = "total", name = "Sonobat") %>% # Adding a new bar of Sonobat
        e_y_axis(type = "value") %>%
        e_tooltip(trigger = "axis") %>% # hovering over mouse and showing the data
        e_legend(show = TRUE, Position = "bottom", orient = "horizontal", top = "5%") %>% # creating legend

        e_title("Species Diversity by Site Name", left = "center") %>% # title name


        e_x_axis(type = "category", axisLabel = list(interval = 0, rotate = 30, fontsize = 3)) %>% # showing site names on X-axis
        e_grid(left = 100, right = 60, top = 70, bottom = 80) %>% # Setting the grid of the histogram so that names on X-axis fit in one screen

        e_theme("walden") %>%
        e_animation() %>% # Adding animation to the chart

        # Using this function to allow the user to download the histogram
        e_toolbox(
          show = TRUE,
          feature = list(
            saveAsImage = list(
              show = TRUE,
              title = "Save",
              name = "species_count_histogram",
              type = "png",
              backgroundColor = "#ffffff",
              pixelRatio = 2
            )
          )
        ) %>%
        # showing the line and bar chart as per user's request
        e_toolbox_feature(
          feature = "magicType",
          type = list("line", "bar")
        )
    }
  })

  output$batsdetections <- renderEcharts4r({
    if (input$view == "Day") {
      # Getting the distinct values of species and grouping them to get unique value
      bat_day <- bats_detections_day %>%
        group_by(sciName) %>%
        distinct()
      


      # Reading the fine scale data of bats and showing weekly time-series chart for species

      bat_chart <- bat_day %>%
        e_charts(x = day, option = list(xAxis = list(type = "category", tick = list(lineStyle = list(type = "dashed"))))) %>%
        e_datazoom(
          type = "slider",
          toolbox = FALSE,
          bottom = 7
        ) %>% # Using the time slider at the bottom of the chart allowing users to change the time
        e_tooltip(trigger = "axis") %>%
        e_title("Species Detections by Day", left = "center") %>%
        e_x_axis(day, axisPointer = list(show = TRUE)) %>% # axispointer is used because not to show popup for each and every point. Axis will be shows to avoid clutter
        e_line(detections) %>%
        e_legend(show = TRUE, Position = "bottom", orient = "horizontal", top = "5%") %>%
        e_y_axis(type = "value", min = 0, interval = 100) %>%
        e_axis_labels(y = "Detections") %>%
        # Using this function to allow the user to download the line chart
        e_toolbox(
          show = TRUE,
          feature = list(
            saveAsImage = list(
              show = TRUE,
              title = "Save",
              name = "species_occurences_histogram",
              type = "png",
              backgroundColor = "#ffffff",
              pixelRatio = 2
            )
          )
        )
    } else {
      # Getting the distinct values of species and grouping them to get unique value
      bat_week <- bats_detections_week %>%
        group_by(sciName) %>%
        distinct()


      # Reading the fine scale data of bats and showing weekly time-series chart for species

      bat_chart <- bat_week %>%
        e_charts(x = week) %>%
        e_datazoom(
          type = "slider",
          toolbox = FALSE,
          bottom = 7
        ) %>% # Using the time slider at the bottom of the chart allowing users to change the time
        e_tooltip(trigger = "axis") %>%
        e_title("Species Detections by Week", left = "center") %>%
        e_x_axis(week, axisPointer = list(show = TRUE)) %>% # axispointer is used because not to show popup for each and every point. Axis will be shows to avoid clutter
        e_line(detections) %>%
        e_legend(show = TRUE, Position = "bottom", orient = "horizontal", top = "5%") %>%
        e_y_axis(type = "value", min = 0, interval = 100) %>%
        e_axis_labels(y = "Detections") %>%
        # Using this function to allow the user to download the line chart
        e_toolbox(
          show = TRUE,
          feature = list(
            saveAsImage = list(
              show = TRUE,
              title = "Save",
              name = "species_occurences_histogram",
              type = "png",
              backgroundColor = "#ffffff",
              pixelRatio = 2
            )
          )
        )
    }
  })
}
