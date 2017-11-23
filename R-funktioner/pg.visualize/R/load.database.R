### Function for loading the database from PostgreSQL to R
load.db <- function(dbname,       # Name of the database
                    host,         # Hostname or IP
                    port = 5432,  # Port of the PostgreSQL-database, standard 5432,
                    user,         # Username with access to the database
                    pw            # Password to the database
                    ){
  # Loads the PostgreSQL driver
  drv <- DBI::dbDriver("PostgreSQL")
  # Creates a connection to the Postgres database
  # Note that "con" will be used later in each connection to the database
  con <- DBI::dbConnect(drv, dbname = dbname,
                                         host = host, port = port,
                                         user = user, password = pw)
  
  # Checks the connection
  if(!DBI::dbExistsTable(con, "spotted_pokemon")){
    stop("Table 'spotted_pokemon' does not exist.")
  } else {
    # Extracts the spotted_pokemon from the database
    spot_pokemon <- suppressWarnings(DBI::dbGetQuery(con, paste("SELECT * from spotted_pokemon")))
    pokemon_info <- suppressWarnings(DBI::dbGetQuery(con, paste("SELECT * from pokemon_info")))
    
    DBI::dbDisconnect(con)
    
    # Removes spotted Pokemon with negative hiding time
    spot_pokemon <- subset(spot_pokemon, subset = spot_pokemon$time_until_hidden_ms > 0)
    
    # Changes hiding time from UTC to GMT+2
    spot_pokemon$hidden_time_utc <- spot_pokemon$hidden_time_utc + lubridate::hours(2)
    
    spot_pokemon$hidden_hms <- lubridate::hms(format(spot_pokemon$hidden_time_utc, "%H:%M:%S"))
  
    pokemon_db <- base::merge(x = spot_pokemon, y = pokemon_info, all.x = TRUE, 
                              by.x = "pokemon_id", by.y = "pokemon_id", sort = FALSE)
    
    
    pokemon_db$name <- tolower(pokemon_db$name)
    
    return(pokemon_db)
  }
}