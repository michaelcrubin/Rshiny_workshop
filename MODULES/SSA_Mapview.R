#### THIS IS THE MICROSERVICE WHICH CREATES THE BACKGROUND MAP AND RENDERS ALL ELEMENTS AND LAYERS

#rasterOptions(progress = "text")  # show the progress of running commands
rasterOptions(maxmemory = 1e+09)  # increase memory allowance
rasterOptions(tmpdir = "temp_files")  # folder for temporary storage of large objects

## FUNCTIONS: ENTERTAINMENT WINDOW  --------------------
# showing load symbol
entertain_modal <- function(){
  
  removeModal()
  
  # create content
  title <- "SwissShinyApp"
  ttl <- "Who is SwissShinyApp"
  txt <- "SwissShinyApp is a software service company specialized on developing on-demand solutions for enterprise- and production-grade Shiny Apps in both, R and Python. We make your Data science process simpler, faster and better."
  txt2 <- "Our core services focus on an efficient Infrastructure for deploying Shiny Apps, integrating Shiny Apps into external services, such as Data bases and AWS Lambda and, of course, developing and improving the touch, feel and functionality of the proper R-Shiny code."
  lnk <- "https://swissshinyapp.com/"
  img <- "https://cdn.odapes.app/img/mountain.jpeg"

  # img <- "img/ssa_logo_free.png"
  go_txt1 <- "Your app is loading..."
  go_txt2 <- "All right. You can go ahead!"
  
  showModal(
    tags$div(id="entertain_modal",
             modalDialog(
               
               title = div(class = "ui header", HTML('<i class="fa fa-refresh fa-spin"></i>'), div(class = "content", title, style="font-size:30px;")),
               
               #body Taglist
               tagList(
                 column(12, fluidRow(h3(ttl, style="color: #DC1A04"))),
                 fluidRow(
                   column(7, offset = 0,
                          tags$br(),
                          fluidRow(p(txt)),
                          # fluidRow(p(txt2)),
                          fluidRow(a(href = lnk, "Learn more", target="_blank")),
                   ),
                   column(5, offset=0,
                          img(src = img,style="width: 100%;height: 100%; object-fit: contain; max-height:200px")
                   )
                 )
               ),
               
               tags$br(),
               
               #footer taglist
               footer = tagList(
                 fluidRow(
                   column(9, offset=0,
                          fluidRow(id = "mappage_id-entertain_wait_txt", h6(go_txt1, style = "text-align: left;margin-top: 10px;")),
                          fluidRow(id = "mappage_id-entertain_go_txt", h6(go_txt2, style = "text-align: left;margin-top: 10px"))%>% shinyjs::hidden()
                   ),
                   column(3,
                          shinyjs::disabled(actionButton("mappage_id-ok_entertain","Go ahead", icon("check-circle"), width="120px", class = "ui positive button"))
                   )
                 )
               )
             ),
             easyClose = F,
             fade = T
    )
  )
}

## FUNCTIONS: BASE MAP  --------------------

# calculates the delay to zoom in depending on long, lat, zoom
calculate_zm_delay <- function(X){
  zeroPoint = c(0,0,2)
  weights = c(25,25,400)
  X %>% dplyr::select(matches("lng"), matches("lat"), matches("zoom")) %>% as.matrix() %>% as.vector()%>%
    magrittr::subtract(zeroPoint) %>% magrittr::multiply_by(weights) %>% magrittr::raise_to_power(2) %>% sum() %>% sqrt()
}

create_base_map <- function(basemap = "simple") {
  switch(
    tolower(basemap),
    "satellite" = {
      leaflet::leaflet() %>%
        leaflet::addTiles(
          urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
          attribution = 'Map data ©2023 Google  Imagery ©2023 NASA, TerraMetrics',
          options = providerTileOptions(minZoom = 2, maxZoom = 20, maxNativeZoom = 20))
    },
    
    "normal" = {leaflet::leaflet() %>% leaflet::addTiles()},
    
    "simple" = {leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)},
    
    "abc" = {leaflet::leaflet() %>%leaflet::addTiles()},
    
    { leaflet() %>% addTiles()}
  )
}

add_control <- function(map, position = "bottomleft") {
  map %>%
    leafem::addMouseCoordinates() %>%
    leaflet::addMeasure(
      position = position,
      primaryLengthUnit = "meters",
      primaryAreaUnit = "hectares"
    )
}

# Creating initial map and loading all elements and layers // MAIN FUNCTION
create_backgnd_map<-function(user_preference){
  # adding the Background Tile
  
  background_map<-create_base_map("satellite") %>%
    setView(lng = 0, lat = 0, zoom = 2) %>%
    add_control()
# background_map
  return(background_map)
}

##FUNCTIONS: MENU LIST --------------------

element_menu_cell <- function( ns, element_id, element_name, value, ... ){
  bx <- list(
    inputId = ns(element_id),
    label = element_name,
    value = value,
    status = "success"
    ) %>%
    do.call(what = shinyWidgets::prettyCheckbox, args = .) %>% htmltools::tags$div(class = "check_mini")
  fluidRow(class = "flymenu_row",
           shiny::column(width = 1, style = "padding-right: 0; padding-left: 0;", shiny::actionButton(inputId = ns(paste0("zoom-", element_id)), label = NULL, icon = icon("map-marker"), class = "btn_mini")),
           shiny::column(width = 11, style = "padding-right: 10px; padding-left: 10px;", bx),
           )
}

# CAN BE TRANSFERED AS SOON AS I HAVE CLEAR HOW THE SHOWS ARE DEFINED.
element_menu_list <- function(X, ns){
  menu <- X %>% mutate(value = TRUE) %>% purrr::pmap(., .f = element_menu_cell, ns = ns)
}

geo_menu_cell <- function( ns, geo_id, geo_name, ... ){
  fluidRow(class = "flymenu_row",
           shiny::column(width = 1, style = "padding-right: 0; padding-left: 0;", shiny::actionButton(inputId = ns(geo_id), label = NULL, icon = icon("map-marker"), class = "btn_mini")),
           shiny::column(width = 11, style = "padding-right: 10px; padding-left: 10px;", tags$p(geo_name, class = "flymenu_txt"))  )
}

# CAN BE TRANSFERED AS SOON AS I HAVE CLEAR HOW THE SHOWS ARE DEFINED.
geo_menu_list <- function(X, ns){
  menu <- X %>% purrr::pmap(., .f = geo_menu_cell, ns = ns)
}

## FUNCTIONS: ELEMENT RENDER --------------------

# makes icon depending on type
leaflet_icon <- function(element_type, ...){
  leaflet::makeIcon(iconUrl =  paste0("icon/", element_type, ".png"), iconWidth= 50, iconHeight=50)
}

# Adds new elements OR UPDATES visibility to PROXY map
# Problem: I cannot show/hide when clustered. Solution: need to clear/remove markets and to
# store the labels permanently so i just rerendering them when show/hide. The DF_chache lable
# creation is separate process. cann maybe also clearmarkersfromcluster
render_proxy_element <- function(element, pxy_map){
  tryCatch({
    if (nrow(element) > 0){
      pxy_map %>%
        clearMarkerClusters %>%
        addMarkers(data = element, lng = ~element_lng, lat = ~element_lat,
                   icon = ~leaflet_icon(element_type),
                   clusterOptions = markerClusterOptions(),
                   layerId = element$element_id
        )
      return(element$element_id)
    } else{
      pxy_map %>%
        clearMarkerClusters
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

## FUNCTIONS: MAP RENDERER ----------------------------------------------------

# Proxy function show/hide maps
show_hide_layer <- function(to_show, all, pxy_map, ...){

  # this part only hides or shows layers already rendered
  if (length(to_show) > 0){
    pxy_map %>%
      hideGroup(group = all) %>%
      showGroup(group = to_show)
  } else {
    pxy_map %>%
      hideGroup(group = all)
  }
}

crs_meter <- function(x_sf, region = "BR"){
  switch(region,
         
         "BR" = {sf::st_transform(x_sf, crs = 29101)},
         
         "MX" = {sf::st_transform(x_sf, crs = 7801)}
  )
}

# transforms sf back to original crs + projection (after meter transform)
backtransform_crs <- function(x_sf, orig_sf){
  if (is.na(sf::st_crs(orig_sf))) {
    sf::st_transform(x_sf, crs = 4326)
  }
  else {
    sf::st_transform(x_sf, crs = sf::st_crs(orig_sf))
  }
}

# note that polygonize is much too slow, hence I take the extent
raster_to_border <- function(ras, ...){
  if (!requireNamespace("lwgeom", quietly = TRUE)) {
    install.packages("lwgeom")
    library(lwgeom)
  }
  x_poly <- ras %>% extent() %>% as('SpatialPolygons') %>% st_as_sf()
  st_crs(x_poly) <- crs(ras)
  d <- sf::st_area(x_poly) %>% as.numeric() %>% sqrt() %>% magrittr::multiply_by(0.02)
  z <- x_poly %>% crs_meter() %>% st_buffer(dist = -d) %>% backtransform_crs(x_poly)
}

# rescale the raster type
rescale_raster<-function(img, max_rows){
  ##option scale to n res
  #min_pix<-params$sys_params$scaling$min_pix
  #pix<-min(xres(img), yres(img))
  nrow <- max(nrow(img), ncol(img))
  #getting scaler based on rows
  if (nrow > max_rows){
    scaler_rows <- nrow/max_rows
  }else{
    return(img)
  }

  scaler <- scaler_rows
  # rounding to next higher scalar
  scaler <- ceiling(scaler)
  #rescaling
  img<-raster::aggregate(img, fact = scaler)
  return(img)
}

read_raster_stack <- function(file_path, max_rows = 100){
  img_stack <- raster::stack(file_path)
  # rescaling when uploading to keep performance
  img_stack <- rescale_raster(img = img_stack, max_rows = max_rows)
  # reproject the CRS
  img_stack <- raster::projectRaster(img_stack, crs="+proj=longlat +datum=WGS84")
  # getting always the first layer
  img_layer <- img_stack[[1]]
  return(img_layer)
}

alert_message <- function(){
  shiny::showNotification(
    "Map with unexpected format. Parsing not implemented. Please contact SSA.",
    duration = 6000,
    type = "warning"
  )
  return(FALSE)
}

kmz_importer<-function(path){
  ## Note many user maps come in wierd formats. SSA has a API service to parse and return that.
  alert_message()
  # upload_to_s3(path)
  # kml_path <- convert_kmz_to_kml(path)
  # kml<-kml_importer(path = kml_path)
  #return(kml)
}

kml_importer<-function(path){
  layers <- st_layers(path)
  lynm<-layers$name[1]
  kml<-st_read(dsn = path, drivers = "KML")
  colnames(kml)[1]<-lynm
  return(kml)
}

read_vector_file <- function(file_path){
  format <- sub(".*\\.", "", basename(file_path))
  import <- switch(format,
         "shp" = {sf::st_read(dsn = file_path)},
         "kml" = {kml_importer(file_path)},
         "kmz" = {kmz_importer(file_path)},
         {alert_message()})
  if (isFALSE(import)){return(import)}
  sf_obj <- import %>%  sf::st_transform("+proj=longlat +datum=WGS84") %>% # getting the right CRS
    sf::st_zm(drop = TRUE, what = "ZM")  # taking out a z nd m dimension
  # getting always the first layer
  sf_obj <- sf_obj[,1]
  colnames(sf_obj) <- c("value", "geometry")
  return(sf_obj)
}

check_geo_type <- function(memory_file, geo_type, ...){
  if (all(class(memory_file) %in% c("RasterStack", "RasterLayer", "RasterBrick"))) {
    return(as.character(class(memory_file)))
  } else if (all(class(memory_file) == c("sf", "data.frame"))){
    type <- sf::st_geometry_type(memory_file, by_geometry = F) %>% as.character()
    return(type)
  }
  else {
    alert_message()
  }
}

# imports the map on the fly // usually this calls a GeoAPI
import_map_onfly <- function(file_path, geo_type){
  switch(geo_type,
         "raster" = {
           read_raster_stack(here("maps", file_path), max_rows = 100)
         },
         "vector" = {
           read_vector_file(here("maps", file_path))
        }
  )
}

# Ads a clickable transparent polygon over raster or points
add_click_layer <- function(pxy_map, data, name, type, ...){
  switch(type,
         
         "RasterLayer" = {pxy_map %>% addPolygons(data = raster_to_border(data), group = name,
                                                  stroke = 0,  weight = 0, dashArray = 0, color = "transparent",
                                                  highlightOptions = highlightOptions(weight = 0, fillColor = "#ffffff", dashArray = 0,  fillOpacity = 0.2, bringToFront = TRUE))
         },
         
         "POINT" = {pxy_map %>% addPolygons(data = make_convex_hull(data, buf = 0.04), group = name,
                                            weight = 0, dashArray = "",  color = "transparent", opacity = 0, fillOpacity = 0,
                                            highlightOptions = highlightOptions(weight = 2, color = "#ffffff",  fillColor = "#ffffff", dashArray = "2",  opacity = 1, fillOpacity = 0.1, bringToFront = TRUE)
         )},
         
         {NULL})
  
}

get_buffer <- function(x_sf, buf){
  sqrt(2* sf::st_area(x_sf)) * buf
}

make_convex_hull <- function(x_sf, buf = 0.2){
  orig_crs <- sf::st_crs(x_sf)
  x_sf %>% crs_meter() %>% sf::st_union() %>% sf::st_convex_hull() %>%
    sf::st_buffer(dist = get_buffer(., buf)) %>% sf::st_transform(crs = orig_crs)
}

render_to_leaflet <- function(geo_id, geo_type, map, pxy_map){
  # define the type of the map
  plot_type <- check_geo_type(map, geo_type)
  switch(plot_type, 
         
         "RasterBrick"= {
           pxy_map %>%
             addRasterRGB(x = map, group = geo_id,
                          r = 1, g = 2,b = 3, opacity = 0.6)
         },
         
         "RasterStack"= {
           pxy_map %>%
             addRasterRGB(x = map, group = geo_id,
                          r = 1, g = 2,b = 3, opacity = 0.6)
         },
         
         "RasterLayer"= {
           pxy_map %>%
             addRasterImage(x = map, group = geo_id, layerId = geo_id,
                            colors = "viridis", opacity = 0.6) %>% 
             add_click_layer(map, geo_id, plot_type)
         },

         "POINT"= {
           color_pal <- colorNumeric(palette = "Spectral", domain = c(min(map$value, na.rm = T), max(map$value, na.rm = T)))
           pxy_map %>%
             addCircleMarkers(data = map, group = geo_id,
                              color = ~color_pal(value), stroke = TRUE,  weight = 2, radius=5, fillOpacity = 0.7) %>% 
             add_click_layer(map, geo_id, plot_type)
         },

         "MULTIPOLYGON" = {
           pxy_map %>%
             addPolygons(data = map, group = geo_id,
                         fillColor = "#3460bf", smoothFactor = 0.2, weight = 0, color = "transparent", dashArray = "", opacity = 0, fillOpacity = 0.5,
                         highlightOptions = highlightOptions(weight = 3, color = "#ffffff", dashArray = "3", opacity = 1,  fillOpacity = 0.7, bringToFront = TRUE))
         }
  )
  return(geo_id)
}

get_return_data <- function(geo_id, geo_type, map){
  switch (geo_type,
            "raster" = {
              bbx <- extent(map)
              vals <- raster::values(map) %>% na.omit()
              val_type <- class(vals)
              df <- data.frame(
                geo_id = geo_id,
                xmin = bbx[1],
                xmax = bbx[2],
                ymin = bbx[3],
                ymax = bbx[4],
                val_type = val_type,
                min_val = ifelse(val_type == "numeric", min(vals), as.numeric(NA)) ,
                max_val = ifelse(val_type == "numeric", max(vals), as.numeric(NA)) ,
                mean_val = ifelse(val_type == "numeric", mean(vals), as.numeric(NA))
              ) %>%  
                mutate(values = list(values = vals) )
              },
            
            "vector" = {
              bbx <- st_bbox(map)
              val_type <- class(map$value)
              data.frame(
                geo_id = geo_id,
                xmin = bbx$xmin,
                xmax = bbx$xmax,
                ymin = bbx$ymin,
                ymax = bbx$ymax,
                val_type = val_type,
                min_val = ifelse(val_type == "numeric", min(map$value), as.numeric(NA)),
                max_val = ifelse(val_type == "numeric", max(map$value), as.numeric(NA)),
                mean_val = ifelse(val_type == "numeric", mean(map$value), as.numeric(NA))
              ) %>% mutate(values = list(values = map$value) )
              }
    )
}

# IMPORTS THE MAP ON THE FLY AND RENDERS IT TO LEAFLET
render_proxy_GEO <- function(geo_id, geo_type, file_path, pxy_map, ...){

  # 1. importing the map for any type
  map <- tryCatch({
    import_map_onfly(file_path, geo_type)
  }, error = function(e) {
    return(FALSE)
  })
  # abort if unable to parse map
  if (isFALSE(map)){return(NULL)}

  # 2. rendering
  ret <- tryCatch({
    render_to_leaflet(geo_id, geo_type, map, pxy_map)
    get_return_data(geo_id, geo_type, map)
    }, error = function(e) {
      list(geo_id = NULL)
  })

  rm(map)
  return(ret)
}


## FUNCTIONS: POPUP / modals-----------------

make_ggplot <- function(data){
  # Unnest the list column
  data_unnested <- unnest(data, values)
  bnwd <- (data$max_val - data$min_val) / 25
  # Create a histogram
  p<- ggplot(data_unnested, aes(x = values)) +
    geom_histogram(binwidth = bnwd, fill = "#3460bf70", color = "#3460bf", alpha = 0.7) +
    labs(title = paste("Values of", data$name), x = "Values", y = "Frequency") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Center the title
  
  return(p)
}

click_modal <- function(ns, data){
  
  removeModal()
  # create content
  if (data$type %in% c("raster", "vector")){
    title <- "Map Details"
  } else {
    title <- "Element Details"
  }
  
  ttl <- paste(data$name)
  txt <- paste("ID:", data$id)
  tp <- paste("Type:", data$type)
  
  
  if (data$val_type %in% c("numeric")){
    content <- plotOutput(ns("ggplot"))
    av <- paste("Mean:", round(data$min_val), digits=2)
    mn <- paste("Min:", round(data$min_val, digits=2))
    mx <- paste("Max:", round(data$max_val, digits=2))
    ret <- TRUE
  } else {
    content <- img(src = "img/ssa_logo_free.png",style="width: 100%;height: 50%;object-fit: contain;padding-top: 50px; max-height: 180px;")
    av <- ""
    mn <- ""
    mx <- ""
    ret <- FALSE
  }
  showModal(
    tags$div(id="click_modal",
             modalDialog(
               title = title,

               # title = div(class = "ui header", HTML('<i class="fa-solid fa-chart-line"></i>'), div(class = "content", title, style="font-size:30px; color = #FFFFFF")),
               
               #body Taglist
               tagList(
                 fluidRow(
                   column(4, offset = 0,
                          
                          fluidRow(h3(ttl, style="color: #DC1A04")),
                          tags$br(),
                          fluidRow(p(txt)),
                          fluidRow(p(tp)),
                          fluidRow(p(av)),
                          fluidRow(p(mn)),
                          fluidRow(p(mx)),
                   ),
                   
                   column(8, offset=0, content)
                   # img(src = content,style="width: 100%;height: 100%;object-fit: contain;     padding-top: 50px;max-height: 180px;")
                 )
               ),
               tags$br(),
               footer = tagList(
                 fluidRow(
                   column(9, offset=0),
                   column(3, actionButton(ns("ok_detail"),"OK", icon("check-circle"), width="120px", class = "ui positive button")
                   )
                 )
               )
             ),
             easyClose = T,
             fade = T
    )
  )
  
  return(ret)
  
}

get_popup_data <- function(click_id, r_data, r_user){

  if (click_id %in% r_data$element$element_id){
    r_data$element %>% dplyr::filter(element_id %in% click_id) %>%
      mutate(
        id = element_id, 
        name = element_name, 
        type = element_type,
        val_type = "element"
      ) %>%
      dplyr::select(any_of(c("id", "name" ,"type", "val_type")))
  }
  else if (click_id %in% r_data$georef$geo_id){
    
   r_data$georef %>% dplyr::filter(geo_id %in% click_id) %>%
     mutate(
       id = geo_id, 
       name = geo_name, 
       type = geo_type,
       ) %>%
     dplyr::select(any_of(c("id", "name" ,"type" ,"val_type", "min_val", "max_val", "mean_val", "values")))
  }
  else {
    NA
  }
}


## FUNCTIONS: UI FUNCTIONS --------------------

# function creates button family in blue frame
menu_btn_group <- function(ns){
  btn_lst <- tagList(
    shiny::actionButton(inputId = ns("button_1"), label = NULL, icon = icon("eye"), class = "btn_menu"),
    shiny::actionButton(inputId = ns("button_2"), label = NULL, icon = icon("home"), class = "btn_menu"),
    shiny::actionButton(inputId = ns("button_3"), label = NULL, icon = icon("file"), class = "btn_menu")
  )
  column(width = 12, align="left", class = "blue-btn-frame", btn_lst)
}

# Flymenu tab which holds maps
make_flymenu_maps <- function(ns){
  elem <- shiny::tabPanel(
    value = ("map_tab"),
    title = h4("Maps"),
    tags$br(),
    #Top  button family
    menu_btn_group(ns),
    column(width = 12, align="left", class = "blue-menu-frame",
           h5("Hide/Show"),
           tags$hr(style="margin:10px;"),
           shiny::uiOutput(ns("map_tab")),
           tags$br()
    ),
    column(width = 12, align="left") #dummy to make space
  ) # end tab elements
  return(elem)
}

# Flymenu tab which holds elements
make_flymenu_elements <- function(ns){
  elem <- shiny::tabPanel(
    value = ("element_tab"),
    title = h4("Elements"),
    tags$br(),
    #Top  button family
   menu_btn_group(ns),
   column(width = 12, align="left", class = "blue-menu-frame",
          h5("Hide/Show"),
          tags$hr(style="margin:10px;"),
          shiny::uiOutput(ns("element_tab")),
          tags$br()
          ),
    column(width = 12, align="left") #dummy to make space
  ) # end tab elements
  return(elem)
}

## similar to make_tab_list go to odstable
make_flymenu <- function( ns){
  shiny::tabsetPanel(
    id = ns("flymenu_tabpanel"),
    make_flymenu_elements(ns),
    make_flymenu_maps(ns)
  )
}

# similar to make_DT_tab go to odstable
leaflet_home_button <- function( ns, ...){
  # make regex for class
  tags$div(id = ns("panel_up_left") , class = "flying-btn-left", 
           shiny::actionButton(inputId = ns("zoom_home"), label = NULL, icon = icon("home"), class = "btn_leaflet")
           )
}

# similar to make_DT_tab go to odstable
leaflet_flymenu_button <- function( ns, ...){
  # make regex for class
  tags$div(id = ns("panel_up_right") , class = "flying-btn-right", 
           shiny::actionButton(inputId = ns("show_flymenu"), label = NULL, icon = icon("bars"), class = "btn_leaflet")
  )
}

## MODULES: MAP UI FUNCTION --------------------------
SSA_Map_UI <- function(id) {
  ns <- NS(id)
  tagList(

    # MAP ELEMENT
    tags$div(class = 'outer',
             leafletOutput(ns("background_map"),height = "100%", width = '100%'),
             shiny::uiOutput(ns("popup_place")) # This is the popup renderui
             ),

    # Home Button and set home button
    leaflet_home_button(ns),
    leaflet_flymenu_button(ns),
    
    # MAP LOGO
    tags$div(class = "ods-logo-map", tags$img(src = "img/ssa_logo_freeSW.png", width = "100%", height = "100%") ),


    # Flying Menu Over the Map----------
    absolutePanel(id = ns("panel_flymenu"),
                  class = "flying-menu",
                  top = 70, left = "auto", right = 20, bottom = "auto",
                  width = 290, height = "auto",
                  draggable = TRUE,
                  fixed = TRUE,
                  style = "opacity: 0.75",
                  #Close Button
                  shiny::actionButton(inputId = ns("close_flymenu"), label = NULL, icon = icon("times"), class =  "btn_closemenu"),
                  # menu itself
                  wellPanel(id = "wellpanel_menu", style = "overflow-y:scroll; max-height: 75vh", make_flymenu(ns)
                    )
      )
  )# CLOSE TAGLIST
}

## MODULES: MAP SERVER FUNCTION --------------------------
SSA_Map_SERVER <- function(id, r_data, r_user, r_control, params) {
  moduleServer(id,function(input, output, session) {

    observeEvent(input$button_1, {
      browser()
      # do anything
    })
    
    # APERO------------------------------------------------------------------------------------
    ns <- session$ns
    # creating new Temp Var for transfer inside module
    temp_vals<-reactiveValues()
    
    # HANDLE MAP ELEMENTS ------------------------------------------------------
    
    observeEvent(input$close_flymenu, {
      shinyjs::show("panel_up_right")
      shinyjs::hide("panel_flymenu")
    })
    
    observeEvent(input$show_flymenu, {
      shinyjs::hide("panel_up_right")
      shinyjs::show("panel_flymenu")
    })
    
    observeEvent(input$zoom_home, {
      r_control$rerender_map = as.numeric(Sys.time())*10
    })
    

    # CREATING INITIAL MAP-------------------------------------------------------

    # sets max memory size for raster data and helper vars
    options(shiny.maxRequestSize = 30*1024^2)

    # Rendering the Initial map and adding all layers -- this can be done faster//better
    output$background_map <- renderLeaflet({
      print("1. RENDER LEAFLET TILE")
      r_control$rerender_map
      r_control$procedure_started <- NULL
      r_control$rendered_element <- NULL
      r_control$rendered_georef <- NULL
      r_control$MS_check_id <- NULL
      background_map <- create_backgnd_map(r_user$user_preference)

      # if come from home button must make directly r_control$start_procedure
      if (!is.null(r_control$rerender_map)){
        r_control$start_procedure <- as.numeric(Sys.time())*10
      }
      # show/hide/activate entertainment modal
      shinyjs::enable("ok_entertain")
      shinyjs::hide("entertain_wait_txt")
      shinyjs::show("entertain_go_txt")
      return(background_map)
    })

    # THIS IS THE DELAY AND ENTERTAIN PROCESS // BYPASS THIS FOR DEV
    observeEvent(input$ok_entertain,{
      req(input$ok_entertain)
      removeModal()
      r_control$start_procedure <- as.numeric(Sys.time())*10
    })


    ## zooms home // make a better startup procedure + delay
    observeEvent(r_control$start_procedure, {
      req(r_control$start_procedure)
      print("2. ZOOM HOME")
      leaflet::leafletProxy("background_map") %>% 
        leaflet::flyTo(lng = r_user$user_preference$user_lng,
                       lat = r_user$user_preference$user_lat,
                       zoom = r_user$user_preference$user_zoom
                       )
      r_control$delay_procedure <- as.numeric(Sys.time())*10
    })

    # Startup Procedure with Delay
    observeEvent(r_control$delay_procedure, {
      req(r_control$delay_procedure)
      print("3.1. START DELAY")
      # hide all
      shinyjs::hide("panel_flymenu")
      shinyjs::hide("panel_up_right")

      tm <-  calculate_zm_delay(r_user$user_preference)
      delay(0,{
        print("3.2 DELAY OVER")
        r_control$procedure_started <- as.numeric(Sys.time())*10
        shinyjs::show("panel_flymenu")
      })
    })

    # AFTER DELAY: Provokes the rendering of the Element and Georef menu
    observeEvent(r_control$procedure_started, {
      req(r_control$procedure_started)
      print("5.1.1 PROVOKE RENDER OF MENUS")
      # This changes to each tab of the flymenu to force the rendering, which will show the right elements
        purrr::walk(c("element_tab", "map_tab", "element_tab"),~ updateTabsetPanel(session, inputId="flymenu_tabpanel", selected = .x))
    })

    # AFTER DELAY: Shows again first map
    observeEvent(r_control$procedure_started, {
      req(r_control$procedure_started)
      print("5.1.2 SHOW MAP")
      # show the map (first in pile)
      show_hide_layer(to_show = r_control$rendered_georef[1], all = c(r_control$rendered_georef , "element"), pxy_map = leafletProxy("background_map"))
    })
    

    # ELEMENT HANDLING ------------------------------------------------------
    # Observes r_data$element and triggers delete or add of new elements
    observe({
      req(r_data$element)
      req(r_control$procedure_started)
      print("4.1 GET ELEMENTS TO RENDER")
      r_control$to_render_element <- r_data$element %>%
        dplyr::filter(!(element_id %in% isolate(r_control$rendered_element))) %>% 
        mutate(show = TRUE) %>% df_to_lst(nms = "element_id", vals = "show")
    })
    
    ## Render elements (assing only to rendered_element var because for elements I render/rerender when show/hide)
    observeEvent(r_control$to_render_element, {
      req(length(r_control$to_render_element) > 0)
      print("4.2 RENDER ELEMENT")
      vec <- r_control$to_render_element %>% purrr::keep(.p = isTRUE) %>% names()
      r_control$rendered_element <- r_data$element %>% 
        dplyr::filter(element_id %in% vec) %>%
        render_proxy_element(pxy_map = leaflet::leafletProxy("background_map"))
    })

    # Listens to element checkboxes to show/hide them
    observe({
      req(nrow(r_data$element)>0)
      r_control$to_render_element <- r_data$element$element_id %>% purrr::map(~ input[[.x]]) %>% 
        setNames(r_data$element$element_id) #%>% purrr::keep(.p = isTRUE) #%>% names()
    })
    
    # Listens to element zoom button
    observe({
      req(nrow(r_data$element)>0)
      new_zoom_vec <- r_data$element$element_id %>% purrr::map(~ input[[paste0("zoom-", .x)]])%>% setNames(r_data$element$element_id) %>%
        Filter(function(x) x != 0, .)
      
      zoom_to <- which_changed(new = new_zoom_vec, old = isolate(r_control$zoom_vec))
      r_control$zoom_vec <- new_zoom_vec
      if (length(zoom_to) > 0){
        zoom_to_df <- r_data$element %>% dplyr::filter(element_id %in% zoom_to) %>% slice(1)
        leaflet::leafletProxy("background_map") %>% 
          leaflet::flyTo(lng = zoom_to_df$element_lng,
                         lat = zoom_to_df$element_lat,
                         zoom = 18,
          )
      }                           
    })
    
    #  AFTER DELAY: Render / Rerendering the ELEMENT menu
    output$element_tab <- shiny::renderUI({
      req(r_control$procedure_started)
      print("5.2 CREATE + RENDER EL MENU")
      menu <- (r_data$element) %>% element_menu_list(ns)
      return(menu)
    })


    # GEO HANDLING ------------------------------------------------------

    # Rendering the GEO on leaflet
    observeEvent(r_control$procedure_started, {
      
      req(r_control$procedure_started)
      req(nrow(r_data$georef ) > 0)
      print("4.2 RENDER GEO")
      r_data$georef  <- r_data$georef %>% dplyr::select("geo_id", "geo_name", "geo_type", "file_path")
      ret_df <- r_data$georef  %>% purrr::pmap_dfr(., .f = render_proxy_GEO,  pxy_map = leafletProxy("background_map")) 
      r_data$georef  <- r_data$georef  %>% left_join(ret_df, by = join_by(geo_id))
     
      r_control$rendered_georef <- ret_df$geo_id
      # temporary hide all layers to avoid zoom into map
      show_hide_layer(to_show = NULL, all = r_control$rendered_georef,  pxy_map = leafletProxy("background_map"))
      r_control$to_show_georef <- NULL
    })
    
    # # Listens to element checkboxes to show/hide them
    observe({
      req(nrow(r_data$georef)>0)
      new_geo_vec <- r_data$georef$geo_id %>% purrr::map(~ input[[.x]])%>% setNames(r_data$georef$geo_id) %>% Filter(function(x) x != 0, .)
      selected <- which_changed(new = new_geo_vec, old = isolate(r_control$geo_vec))
      r_control$geo_vec <- new_geo_vec
      r_control$to_show_georef <- selected
    })
    
    
    # Shows & Zooms to clicked layer
    observeEvent(r_control$to_show_georef, {
      req(r_control$rendered_georef)
      # show/hide
      show_hide_layer(to_show = r_control$to_show_georef, 
                      all = r_control$rendered_georef, 
                      pxy_map = leafletProxy("background_map")
                      )

      # zooming
      zoom_to_df <- r_data$georef %>% dplyr::filter(geo_id %in% r_control$to_show_georef) %>% slice(1)
      leaflet::leafletProxy("background_map") %>% 
        leaflet::fitBounds(
          lng1 = zoom_to_df$xmin,
          lat1 = zoom_to_df$ymin,
          lng2 = zoom_to_df$xmax,
          lat = zoom_to_df$ymax
          )
    })

    
    # AFTER DELAY: Render / Rerendering the GEO menu
    output$map_tab <- shiny::renderUI({
      req(r_control$rendered_georef)
      print("5.2 CREATE + RENDER GEO MENU")
      menu <- (r_data$georef) %>% dplyr::filter(geo_id %in% r_control$rendered_georef) %>% geo_menu_list(ns)
      return(menu)
    })
    

    # POPUP  REACTIVE ----------------------------------------------------------------------
    
    observeEvent(input$ok_detail, {
      removeModal()
    })
    
    # close popup by clicking outside
    observeEvent(input$background_map_click, {
      req(input$background_map_click$lat != temp_vals$click$lat )
      req( input$background_map_click$lat != temp_vals$click$lat)
      temp_vals$close_popup <- as.numeric(Sys.time())*10
    })

    # Closing the popup and showing the menu
    observeEvent(input$close_popup, {
      temp_vals$close_popup <- as.numeric(Sys.time())*10
    })

    # Closing the popup and showing the menu
    observeEvent(temp_vals$close_popup, {
      print("close popup")
      browser()
      shinyjs::show("panel_flymenu")
      shinyjs::hide("button_flymenu")
      delete_proxy_map(leafletProxy("background_map"), c("temp_raster", "temp_poly"))
      temp_vals$actual_popup_data<- NULL
      temp_vals$id_vector <- NULL
      temp_vals$click <- NULL
      output$popup_place <- shiny::renderUI({NULL})
    })

    # clicks on marker
    observeEvent(input$background_map_marker_click, {
      temp_vals$click <- list(
        id  = input$background_map_marker_click$id,
        lng = input$background_map_marker_click$lng,
        lat = input$background_map_marker_click$lat,
        bounds = input$background_map_bounds,
        zoom = input$background_map_zoom,
        center = input$background_map_center
      )
    })

    # Clicks on shape
    observeEvent(input$background_map_shape_click, {
      temp_vals$click <- list(
        id  = input$background_map_shape_click$group,
        lng = input$background_map_shape_click$lng,
        lat = input$background_map_shape_click$lat,
        bounds = input$background_map_bounds,
        zoom = input$background_map_zoom,
        center = input$background_map_center
      )
    })

    # POPUP STRUCTURE ----------------------------------------------------------------------
    
    # ON SHAPE CLICK
    observeEvent(temp_vals$click, {

      req(temp_vals$click$id)
      st <- Sys.time()
       # get the data
      temp_vals$actual_popup_data <- get_popup_data(temp_vals$click$id, r_data, r_user)
      temp_vals$renderggplot <- click_modal(ns, temp_vals$actual_popup_data)
      isTRUE(temp_vals$renderggplot)
      if (isTRUE(temp_vals$renderggplot)){
        output$ggplot <- renderPlot({make_ggplot( temp_vals$actual_popup_data)})
      }
    })

  })
}

