map.title <- function(time,
                      time_start = NULL){
  if(is.null(time_start) & suppressWarnings(!is.na(as.numeric(time)))){
    title <- paste(paste("Past", R.utils::capitalize(time), "hours"), "Pokémon, Nr. of Spawns", "", sep = "\n")  
  } else if (!is.null(time_start)){
    
    start <- time_start
    end <- lubridate::hms(time_start) + lubridate::hours(time)
    title <- paste(paste(start, "-", format(lubridate::make_datetime(hour = lubridate::hour(end), 
                                                                     min = lubridate::minute(end),
                                                                     sec = lubridate::second(end)),
                                            "%H:%M:%S")),
                   "Pokémon, Nr. of Spawns", "", sep = "\n")
  } else {
    title <- paste(R.utils::capitalize(time), "Pokémon, Nr. of Spawns", "", sep = "\n")  
  }
  
  return(title)
}