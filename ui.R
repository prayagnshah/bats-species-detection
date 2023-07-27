library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(scico)
library(echarts4r)
library(glue)
library(reactable)
library(shinymanager)



ui <- fluidPage(

  # css file to apply the effects
  includeCSS("data/table_style.css"),

  # setting the background color

  setBackgroundColor(
    color = "#e6f2ff",
    direction = "top",
    shinydashboard = FALSE
  ),

  # providing the theme to the app
  theme = bslib::bs_theme(bootswatch = "united", version = 5),


  # Title of the application

  div(
    style = "background-color: #78D4F2; padding: 12px;font-size: 30px;text-align: left;",
    h1("Inventory data–CWS Atlantic Region Protected Areas ")
  ),
  div(
    style = "color: black; font-size: 17px;",
    HTML("<i>Filter occurrences by site, class, or by lower level taxonomy (taxon); view corresponding mapped occurrences.</i>")
  ),
  
  


  fluidRow(
    column(3, selectInput("Site", "Select site(s):", choices = sort(unique(species$Sites)), multiple = TRUE))
    # column(8, uiOutput("species_dropdown"))
  ),
  fluidRow(
    column(1, selectInput("class", "Select class(es)", choices = sort(unique(species$Class)), multiple = TRUE)),
    column(2, selectInput("taxon", "Select taxon:", choices = sort(unique(species$Taxon)), multiple = TRUE))
    # column(4,radioButtons("radio_option", label = NULL, choices = c("Detections", "Richness"), inline = TRUE, selected = "Detections"))
  ),
  
  fluidRow(
    column(8, offset = 6, radioButtons("radio_option", label = NULL, choices = c("Detections", "Richness"), inline = TRUE, selected = "Detections"))
    ),

  # using fluidrow differently in order to adjust the width of the table 
  # Using reactable to make the dashboard load faster
  fluidRow(
    column(6, reactableOutput("speciestable", height = "auto")),
    # column(4, leafletOutput(outputId = "Specieswithnames", height = "560px")),

    column(
      6,
      # radioButtons("radio_option", label = NULL, choices = c("Detections", "Richness"), inline = TRUE, selected = "Detections"),
      echarts4rOutput("specieshist", height = "520px")
    )
  ),
  
  # Using fluid row differently for all the outputs to make it more interactive
  br(),
  fluidRow(
    column(6, offset = 1, uiOutput("species_dropdown"))
  ),
  
  # using fluidrow differently in order to adjust the width of the table
  
  fluidRow(
    column(10, offset = 1, leafletOutput(outputId = "Specieswithnames", height = "600px"))
  ),
  br(),
  
  fluidRow(
    column(10, offset = 1, selectInput("view", "Select view:", choices = c("Week", "Day")))
  ),
  
  fluidRow(
    column(12, offset = 0.6,
           # selectInput("view", "Select view:", choices = c("Week", "Day")),
           echarts4rOutput("batsdetections", height = "600px")) 
  )
)

# Securing the ui and commenting it out for now
# ui <- secure_app(ui, choose_language = TRUE)
