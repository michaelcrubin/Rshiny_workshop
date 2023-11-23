#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# renv::init()
# Shiny Libraries
library(here)
library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

# Tidyverse stuff
#library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(magrittr)
library(readr)
library(tibble)
library(tidyr)
library(stringr)

# Geo Stuff
library(leaflet)
library(raster)
library(leafem)
library(sp)
library(sf)
sf_use_s2(FALSE) # This is important
library(leaflet.extras)
library(DT)
library(ggplot2)

# Loading Modules

source(here("MODULES","SSA_Mapview.R"), local = F)
source(here("MODULES","helpers.R"), local = F)

# read parameters
# user <- readRDS(here("data", "user.rds"))
## UI PART: APP UI FUNCTION ---------------------------------------------
ui <- shinydashboardPlus::dashboardPage(
  # APP HEADER
  shinydashboardPlus::dashboardHeader(
    # Title and second logo
    title = tagList(span(class = "logo-lg", span(img(src = "img/ssa_logo_free.png"))), img(src = "img/ssa_icon_free.png", width = 40))
  ),
  
  # APP SIDEBAR
  shinydashboardPlus::dashboardSidebar(
    collapsed = TRUE, minified = TRUE,
    sidebarMenu(id="pages",
                menuItem("Mapview", tabName = "map", icon = icon("map-marked-alt"))
    )
  ),
  
  # APP BODY
  dashboardBody(
    ##  Style and JS stuff // Don't touch ---------
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.odapes.app/css/stylesheet_SSA.css"),
              tags$link(rel = "stylesheet", type = "text/css", href = "css/entertainment.css"), # temporary // merge with CSS
              tags$style(HTML(".popover.popover-lg {width: 500px; max-width: 500px;}"))
    ),
    
    # BODY TABS
    tabItems(
      # map tab
      tabItem(tabName = "map",
              SSA_Map_UI("mappage_id")
      )

    )
  )
)

## SERVER PART: APP SERVER FUNCTION ---------------------------------------------
server <- function(input, output, session) {
  
  ##  APERO------------
  # entertainment window
  entertain_modal()
  
  ##  LOAD REACTIVE VALS ------------
  # reactive user data
  r_user <- get_r_user()
  # reactive data
  r_data <- get_r_data()
  # triggers
  r_control <- get_r_control()
  # reactive admin data

  # 
  ## CALL LAYER 2 MODULES -------------------------------------------------------
  # Map Panes
  SSA_Map_SERVER("mappage_id", r_data, r_user, r_control,  params)
  
  ## TABPAGE SELECTOR ----------
  
  # Selects and switches to another tab (page) selected by a ui input inside a module
  observeEvent(r_control$goto_tab,{
    req(r_control$goto_tab)
    updateTabItems(session, inputId="pages", selected = r_control$goto_tab)
    r_control$goto_tab <- NULL
  })
  
}

shinyApp(ui, server)
