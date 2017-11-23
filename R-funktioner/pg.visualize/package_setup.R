# # Visualization packages
# devtools::use_package("ggplot2")
# devtools::use_package("ggmap")
# devtools::use_package("gridExtra")
# devtools::use_package("RColorBrewer")
# devtools::use_package("animation")
# # DB connection
# devtools::use_package("RPostgreSQL")
# devtools::use_package("DBI")
# # Molding data
# devtools::use_package("reshape2")
# # Data processing
# devtools::use_package("R.utils")
# devtools::use_package("stringr")
# devtools::use_package("lubridate")
# # Density estimation
# devtools::use_package("MASS")
# # Clustering
# devtools::use_package("dbscan")
# # Use of data
# devtools::use_data(pokemon_db, overwrite = TRUE)


### Testing
setwd("F:/OneDrive/OneDrive - Linköpings universitet/Jobb/LiU/Kurser/0. R-funktioner/pg.visualize")
devtools::load_all()
# devtools::use_testthat()

{
pw <- "D#IOYP"






























































}
pokemon_db <- load.db(dbname = "pokemon_go", host = "192.168.1.10", user = "pokemon_go_role", pw = pw)
rm(pw)

# Loading maps
{
map13 <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 13, scale = 2, maptype = "terrain", color = "bw")
map12 <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 12, scale = 2, maptype = "terrain", color = "bw")
map_garden <- ggmap::get_googlemap(c(15.620266, 58.405196), zoom = 16, scale = 2, maptype = "terrain", color = "bw")
}


sub_pokemon <- subset.pokemon(database = pokemon_db, pokemon = "Pinsir", time = "all")
sub_pokemon <- subset.pokemon(database = pokemon_db, pokemon = "Pidgey", time = "all")

windows()
scatter.map(database = pokemon_db, pokemon = c("Jynx"), time = "all", map = africa, era = "all")

bin.map(database = pokemon_db, pokemon = "snorlax", time = "all", map = map12)

plot1 <- scatter.map(database = pokemon_db, pokemon = "dratini", time = "all", map = map12)
plot2 <- density.map(database = pokemon_db, pokemon = "dratini", time = "all", map = map13)

gridExtra::grid.arrange(plot1, plot2, ncol = 1)


bin.map(database = pokemon_db, pokemon = "Pidgey", time = "all", map = europe, bins = 100)


cluster.map(pokemon = "tentacool", eps = 0.002, time = "24", database = pokemon_db)

pokemon <- "vulpix"
animation::saveGIF({
print(cluster.map(pokemon = pokemon, eps = 0.002, time = "all", database = pokemon_db, era = "1") + 
  ggplot2::ggtitle("Era 1"))

print(cluster.map(pokemon = pokemon, eps = 0.002, time = "all", database = pokemon_db, era = "2") + 
  ggplot2::ggtitle("Era 2"))
} ,
movie.name = "C:/Users/Canadice/Downloads/eras.gif", 
ani.width = 1024, 
ani.height = 1024, 
interval = 2, 
ani.type = "png")


time <- "4"
time_start <- "00:00:00"
pokemon <- "onix"

animation::saveGIF({
  for(i in 1:(24/as.numeric(time))){
    
    print(density.map(database = pokemon_db, pokemon = pokemon, bins = 5,
                  time = time, map = map12, time_start = time_start))
    
    
    end <- lubridate::hms(time_start) + lubridate::hours(time)
    time_start <- format(lubridate::make_datetime(hour = lubridate::hour(end), 
                                                  min = lubridate::minute(end),
                                                  sec = lubridate::second(end)),
                         "%H:%M:%S")
  }
},
  movie.name = "C:/Users/Canadice/Downloads/test.gif", 
  ani.width = 1024, 
  ani.height = 1024, 
  interval = 2, 
  ani.type = "png", 
  autobrowse = FALSE)
