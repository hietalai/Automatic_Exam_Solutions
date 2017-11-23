density.map <- function(database,      # Full database of spawns
                        pokemon,       # Specified Pokemon to visualize (max 1)
                        time = "all",  # Specified timespan of spawns to visualize
                        map,           # Map to visualize on
                        scales = 10,    # scale control the heat temperature scale
                        bins = 4,
                        n = 250,       # n are settings for density computation
                        era = "all",
                        ...
                        ){
  # Checks limit of number of Pokemon to visualize
  if(length(pokemon) > 1){
    stop("You have specified too many Pokemon! Only one (1) can be specified in this mapmode.")
  }
  
  sub_data <- subset.pokemon(database = database, pokemon = tolower(pokemon), time = time, era = era, map_coord = attr(map, "bb"), ...)
  
  if(nrow(sub_data)==0){
    ggmap::ggmap(map, extent = "device")
    
  } else {
    spawn_freqs <- as.data.frame(table(sub_data$name))
    
    label <- R.utils::capitalize(sort(paste(spawn_freqs$Var1, paste(spawn_freqs$Freq, "spawns"), sep = ", ")))
    title <- map.title(time, ...)
    
    base::attach(sub_data, warn.conflicts = FALSE)
    densities <- MASS::kde2d(x = longitude, y = latitude, n = n)
    base::detach(sub_data)
    
    
    max <- max(densities$z) + 0.05* diff(range(densities$z))
    if(max > 1 & max < 10){
      digits <- -1
    } else if (max > 0.1 & max < 1){
      digits <- 2
    } else if (max > 0.01 & max < 0.1){
      digits <- 3
    } else if (max > 0.0001 & max < 0.01){
      digits <- 6
    } else {
      digits <- -2
    }
    max <- round(max, digits = digits)
    
    ggmap::ggmap(map, extent = "device") + 
      ggplot2::stat_density_2d(ggplot2::aes(x = longitude,
                                            y = latitude,
                                            fill = ..level..),
                               alpha = 0.6,
                               data = sub_data,
                               bins = bins,
                               n = n,
                               geom = "polygon") + 
      ggplot2::scale_fill_gradient(paste(title, label, "\n", sep = "\n"), 
                                   low = "black", 
                                   high = "red",
                                   limits = c(0, max),
                                   breaks = seq(0, max, by = max/(scales - 1)),
                                   labels = c("Low",rep("", times = scales - 2), "High"))
  }
  
 
}