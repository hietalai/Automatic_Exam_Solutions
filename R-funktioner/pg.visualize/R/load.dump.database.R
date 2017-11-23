load_db_dump <- function(dbname,       # Name of the database
                         host,         # Hostname or IP
                         port = 5432,  # Port of the PostgreSQL-database, standard 5432,
                         user,         # Username with access to the database
                         pw            # Password to the database
){
  drv <- DBI::dbDriver("PostgreSQL")
  # Creates a connection to the Postgres database
  # Note that "con" will be used later in each connection to the database
  con <- DBI::dbConnect(drv, dbname = dbname,
                        host = host, port = port,
                        user = user, password = pw)
  
  # Checks the connection
  if(!DBI::dbExistsTable(con, "pokemon")){
    stop("Table 'pokemon' does not exist.")
  } else {
    # Extracts the spotted_pokemon from the database
    spot_pokemon <- suppressWarnings(DBI::dbGetQuery(con, paste("SELECT * from pokemon")))
    pokemon_info <- suppressWarnings(DBI::dbGetQuery(con, paste("SELECT * from pokemon_info")))
    
    DBI::dbDisconnect(con)
    
    pokemon_db <- base::merge(x = spot_pokemon, y = pokemon_info, all.x = TRUE, 
                              by.x = "pokemon_id", by.y = "pokemon_id", sort = FALSE)
    
    pokemon_db$name <- tolower(pokemon_db$name)
    
    return(pokemon_db)
  }
}