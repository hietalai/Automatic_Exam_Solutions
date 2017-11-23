### Function that subsets the table based on Pokemon name and timespan

subset.pokemon <- function(database,          # Full database
                           pokemon,           # Name(s) of Pokemon wanting to subset
                           time = "all",      # Timespan, c("today", "all", "..number.."). 
                           time_start = NULL, # Specifies if the timespan should start at other time than current
                           era = "all",       # Specifies if specific era is wanted
                           map_coord = NULL
                           ){
  if(era != "all"){
    database <- subset(database, database$pokemon_go_era == era)
  }
  
  # Subsets the specified timespan
  # If time_start is not null time must be numeric and acts as interval length
  if (!is.null(time_start) & suppressWarnings(is.na(as.numeric(time)))){
    
    stop("You have specified a start time other than the current. Time acts as interval length and must now be numeric.")
    
#   } else if (!is.null(time_start)){
#     
#     stop("Start time should be given in a '%H:%M:%S'-format.")
#     
  } else if (!is.null(time_start) & suppressWarnings(!is.na(as.numeric(time)))){
    
    start <- lubridate::hms(time_start)
    nr_hours <- as.numeric(time)
    time_index <- which(database$hidden_hms >= start & 
                          database$hidden_hms < start + lubridate::hours(nr_hours))
    
  } else if(time == "today"){
    # Pokemon spotted today (00:00 and forward)
    time_index <- which(database$date_key == as.numeric(stringr::str_replace_all(Sys.Date(), "-", "")))
    
  } else if (suppressWarnings(!is.na(as.numeric(time)))) {
    # Pokemon spotted the last x hours
    nr_hours <- as.numeric(time)
    time_index <- which(database$hidden_time_utc >= Sys.time() - lubridate::hours(nr_hours))
    
  } else {
    
    time_index <- 1:nrow(database)
    
  }
  
  # Note it assumes that a column with Pokemon name is given in the table
  # Subsets the specified Pokemon
  name_index <- which(database$name %in% pokemon)
  
  # Joins the two indices
  index <- time_index[which(time_index %in% name_index)]
  
  checks <- 0
  # If no Pokemon have spawned in the given timespan, stop.
  while(length(index)==0){
    if(checks < 2){
      suggestions <- agrep(pattern = pokemon, x = unique(database$name), ignore.case = TRUE, value = TRUE)
      print(suggestions)
      
      pokemon <- readline(prompt = "WARNING! Please provide the correct spelling of your Pokemon: ")
      
      # Note it assumes that a column with Pokemon name is given in the table
      # Subsets the specified Pokemon
      name_index <- which(database$name %in% pokemon)
      
      # Joins the two indices
      index <- time_index[which(time_index %in% name_index)]
      
      checks <- checks + 1
    } else {
      stop(paste("No", R.utils::capitalize(pokemon), "was found in the given timespan. Try increasing the time."))
    }
  }
  
  if (!is.null(map_coord)){
    # Creates a map_index of the specified map
    map_index <- which(database$latitude > map_coord$ll.lat & database$longitude > map_coord$ll.lon & 
                         database$latitude < map_coord$ur.lat & database$longitude < map_coord$ur.lon)
    # Joins the two indices
    index <- index[which(index %in% map_index)]
    
  }
  
  # Subsets the data
  data <- database[index,]
  
  return(data)
}