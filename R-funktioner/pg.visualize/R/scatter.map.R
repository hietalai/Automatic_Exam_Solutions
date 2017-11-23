### Function that visualizes the individual spawns as points on a map
scatter.map <- function(database,       # Full database of spawns
                        pokemon,        # Specified Pokemon to visualize (max 4)
                        time = "all",   # Specified timespan of spawns to visualize
                        map,            # Map to visualize on
                        cluster = NULL, # Specified clusters
                        era = "all",
                        ...
                        ){
  # Checks limit of number of Pokemon to visualize
  if(length(pokemon) > 4){
    stop("You have specified too many Pokemon! Needs to be less than four.")
  }
  
  # All spawns can be provided and will produce a different map
  test <- FALSE
  if(length(pokemon) == 1){
    if (pokemon == "all") test <- TRUE
  }
  
  title <- map.title(time, ...)
  
  if(test){
    sub_data <- subset.pokemon(database = database, pokemon = R.utils::capitalize(unique(database$name)), 
                               time = time, era = era, map_coord = attr(map, "bb"), ...)
    
    ggmap::ggmap(map, extent = "device") + 
      ggplot2::geom_point(data = sub_data, 
                          ggplot2::aes(x = longitude, y = latitude, col = "black"),
                          alpha = 0.3,
                          size = 3) +
      ggplot2::scale_color_manual(title, 
                                 values = c("black"),
                                 labels = paste("All Pokémon", paste(nrow(sub_data), "spawns"), sep = ", "))
  } else {
    sub_data <- subset.pokemon(database = database, pokemon = R.utils::capitalize(pokemon), time = time, era = era, ...)
    
    spawn_freqs <- as.data.frame(table(sub_data$name))
    
    labels <- R.utils::capitalize(sort(paste(spawn_freqs$Var1, paste(spawn_freqs$Freq, "spawns"), sep = ", ")))
    
    if(!is.null(cluster)){
      title <- paste(title, labels, sep = "\n")
      
      labels <- as.character(sort(unique(cluster)))
      
      labels[labels != "0"] <- paste("Cluster", labels[labels != "0"])
      labels[labels == "0"] <- "Noise"
      
      
      sub_data$name <- as.factor(cluster)
    }
    
    ggmap::ggmap(map, extent = "device") + 
      ggplot2::geom_point(data = sub_data, 
                          ggplot2::aes(x = longitude, y = latitude, col = name),
                          alpha = 0.3,
                          size = 3) +
      ggplot2::scale_color_manual(title, 
                                  values = RColorBrewer::brewer.pal(name = "Set1", n = max(3, length(labels))),
                                  labels = labels)
  }
}