#### HELPER FUNCTIONS

which_changed <- function(new, old){
  if (length(new) == 0) {return(NULL)}
  if (length(old) == 0) {return(names(new))}
  if (identical(old, new)){return(NULL)}
  map_lgl(names(new), ~!identical(new[.x], old[.x])) %>% names(new)[.]
}

df_to_lst <- function(df,  vals, nms, params = NULL){
  setNames(as.list(df[[vals]]), df[[nms]])
  
}

find_column <- function(X, what = "id", n = 1, mode = "end"){
  y <- switch(mode,
              
              "end" = {X %>% dplyr::select(ends_with(what)) %>% colnames()},
              
              "start" = {X %>% dplyr::select(starts_with(what)) %>% colnames()},
              
              "all" = {X %>% dplyr::select(matches(paste0(what, "_", "|", "_", what))) %>% colnames()},
              
              "regex" = {X %>% dplyr::select(matches(what)) %>% colnames()}
  )
  
  if (length(y) <= n){
    y %>% magrittr::extract(n)
  } else {
    y %>% magrittr::extract(1)
  }
}

get_r_data <- function(){
  
  element <- readRDS(here("data", "element.rds")) 
  georef<- readRDS(here("data", "georef.rds"))
  
  r_data <- shiny::reactiveValues(
    element = element,
    georef = georef
  )
  return(r_data)
}

get_r_control <- function(){
  
  r_control<-shiny::reactiveValues(
    actual_layers = NULL,
    deletedmaps= NULL,
    
    # general control elements
    trigger_linetool=0,
    
    # Drawing stuff
    trigger_drawing_tool = "remove",
    open_fieldmanager = 0,
    new_elem_trigger = 0,
    new_field_trigger = 0,
    drawtype = "",
    
    # Start up process
    rerender_map = NULL,
    map_started = FALSE,
    to_render_georef = NULL,
    map_rendered = FALSE,
    MS_check_id = NULL,
    zoom_vec = NULL,
    geo_vec = NULL,
    start_1 = TRUE,
    start_2 = FALSE,
    start_3 = FALSE,
    start_4 = FALSE,
    start_5 = FALSE,
    started = FALSE,
    trigger_treeupdate = 0,
    logout_trigger = FALSE,
    trigger_rerender = 0, # needed???
    close_map_menu = 0,
    open_map_menu = 0,
    

    # General App Control
    trigger_refresh=0,
    goto_tab = NULL
    
  )
  return(r_control)
}


get_r_user <- function(){

  user <- readRDS(here("data", "user.rds"))
  user_preference <- readRDS(here("data", "user_preference.rds"))
  
  react_list = shiny::reactiveValues(
    user = user,
    user_preference = user_preference
    )
  
  return(react_list)
}
