cluster.map <- function(database, 
                        pokemon, 
                        time = "all",
                        map,
                        eps = 0.002, 
                        minPts = 4,
                        era = "all",
                        ...
                        ){
  
  sub_pokemon <- subset.pokemon(database = database, pokemon = pokemon, time = time, era = era, ...)
  
  coordinates <- sub_pokemon[, c("longitude", "latitude")]
  distances <- dist(x = coordinates)
  
  nr_clusters <- 10
  while(nr_clusters > 9){
    clusters <- dbscan::dbscan(x = distances, eps = eps, minPts = minPts)
    
    nr_clusters <- length(unique(clusters$cluster))
    
    eps <- eps + 0.001
  }
  
  scatter.map(database = sub_pokemon, time = time, pokemon = pokemon, map = map12, cluster = clusters$cluster)
}
