### Testing
# dir <- "C:/Users/isahi12/OD/Jobb/LiU/Kurser/0. R-funktioner/pg.visualize"
dir <- "F:/OneDrive/OneDrive - Linköpings universitet/Jobb/LiU/Kurser/0. R-funktioner/pg.visualize"
setwd(dir)
devtools::load_all()

# World Spawns
{
pw <- "D#IOYP"






























































}
pokemon_db_global <- load_db_dump(dbname = "pokemon_go", host = "192.168.1.10", user = "pokemon_go_role", pw = pw)
rm(pw)

# Local Spawns
{
  pw <- "D#IOYP"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
pokemon_db_local <- load.db(dbname = "pokemon_go", host = "192.168.1.10", user = "pokemon_go_role", pw = pw)
rm(pw)

## Getting maps
{
europe <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 3, scale = 2, maptype = "terrain", color = "bw")
north_america <- ggmap::get_googlemap(t(ggmap::geocode("Texas")), zoom = 3, scale = 2, maptype = "terrain", color = "bw")
australia <- ggmap::get_googlemap(t(ggmap::geocode("Sydney")), zoom = 3, scale = 2, maptype = "terrain", color = "bw")
south_america <- ggmap::get_googlemap(t(ggmap::geocode("Brazil")), zoom = 3, scale = 2, maptype = "terrain", color = "bw")
asia <- ggmap::get_googlemap(t(ggmap::geocode("Delhi")), zoom = 3, scale = 2, maptype = "terrain", color = "bw")
africa <- ggmap::get_googlemap(t(ggmap::geocode("Brazzaville")), zoom = 3, scale = 2, maptype = "terrain", color = "bw")
map13 <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 13, scale = 2, maptype = "terrain", color = "bw")
map13 <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 13, scale = 2, maptype = "terrain", color = "bw")
map13_col <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 13, scale = 2, maptype = "terrain", color = "color")
map13_sat <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 13, scale = 2, maptype = "satellite", color = "bw")
map13_hyb <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 13, scale = 2, maptype = "hybrid", color = "color")
map12 <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 12, scale = 2, maptype = "terrain", color = "bw")
map12_col <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 12, scale = 2, maptype = "terrain", color = "color")
map12_sat <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 12, scale = 2, maptype = "satellite", color = "bw")
map12_hyb <- ggmap::get_googlemap(c(15.60, 58.405), zoom = 12, scale = 2, maptype = "hybrid", color = "color")
}


windows()
bins <- 100
plot_europe <- bin.map(database = pokemon_db, pokemon = "Drowzee", time = "all", map = europe, bins = bins)
plot_north_america <- bin.map(database = pokemon_db, pokemon = "Pidgey", time = "all", map = north_america, bins = bins)
plot_asia <- bin.map(database = pokemon_db, pokemon = "Pidgey", time = "all", map = asia, bins = bins)
plot_australia <- bin.map(database = pokemon_db, pokemon = "Pidgey", time = "all", map = australia, bins = bins)

density.map(database = pokemon_db_local, pokemon = "pidgey", time = "all", map = map13)

bin.map(database = pokemon_db_local, pokemon = "all", time = "all", map = map13_col, alpha = "freq")

density.map(database = pokemon_db_local, pokemon = "pidgey", time = "all", map = north_america)

gridExtra::grid.arrange(plot_europe, plot_north_america, plot_asia, plot_australia, ncol = 2)














