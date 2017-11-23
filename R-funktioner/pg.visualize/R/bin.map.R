bin.map <- function(database,      # Full database of spawns
                    pokemon,       # Specified Pokemon to visualize (max 1)
                    time = "all",  # Specified timespan of spawns to visualize
                    map,           # Map to visualize on
                    bins = 30,    # Decides the number of squares covering the map
                    era = "all",
                    alpha = 0.6,   # If alpha is "freq" then table of frequencies in each coordinate is summarized in a table
                    ...
                    ){
  # Checks limit of number of Pokemon to visualize
  if(length(pokemon) > 1){
    stop("You have specified too many Pokemon! Only one (1) can be specified in this mapmode.")
  }
  
  if(pokemon != "all"){
    sub_data <- subset.pokemon(database = database, pokemon = tolower(pokemon), 
                             time = time, era = era, map_coord = attr(map, "bb"), ...)
    
    spawn_freqs <- as.data.frame(table(sub_data$name))
  
    label <- R.utils::capitalize(sort(paste(spawn_freqs$Var1, paste(spawn_freqs$Freq, "spawns"), sep = "\n")))
  
  } else {
    sub_data <- database
    
    freq <- nrow(database)
    
    label <- R.utils::capitalize(sort(paste("All", paste(freq, "spawns"), sep = "\n")))
  }
  
  
  title <- map.title(time, ...)
  
  if(alpha == "freq"){
    if(attr(map, which = "zoom") == 13){
      sub_data$longitude <- round(sub_data$longitude/0.005)*0.005
      sub_data$latitude <- round(sub_data$latitude/0.0025)*0.0025
    } else if (attr(map, which = "zoom") == 12){
      sub_data$longitude <- round(sub_data$longitude, 2)
      sub_data$latitude <- round(sub_data$latitude/0.005)*0.005
    }
    
    lon_range <- extendrange(pokemon_db_local$longitude)
    lat_range <- extendrange(pokemon_db_local$latitude)

    sub_data_table <- as.data.frame(table(sub_data[, c("longitude", "latitude")]))
    sub_data_table <- sub_data_table[sub_data_table$Freq > 0,]
    sub_data_table <- as.data.frame(apply(sub_data_table, FUN = as.numeric, MARGIN = 2))
    names(sub_data_table) <- c("longitude", "latitude", "freq")
    
    ggmap::ggmap(map, extent = "device") + 
      ggplot2::geom_tile(ggplot2::aes(x = longitude,
                                       y = latitude,
                                       alpha = freq),
                         fill = "firebrick",
                         data = sub_data_table) + 
      ggplot2::scale_alpha_continuous(name = label, range = c(0.1, 0.7), 
                                      guide = ggplot2::guide_legend(reverse = TRUE)) + 
      ggplot2::theme(legend.title=ggplot2::element_text(size=20), 
                     legend.text=ggplot2::element_text(size=16))
  } else {
    ggmap::ggmap(map, extent = "device") + 
      ggplot2::geom_bin2d(ggplot2::aes(x = longitude,
                                       y = latitude),
                          alpha = alpha,
                          data = sub_data,
                          bins = bins) + 
      ggplot2::scale_fill_gradient(paste(title, label, "", sep = "\n"), 
                                   low = "dodgerblue", 
                                   high = "dodgerblue4")   
  }
}