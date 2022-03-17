# author: Daniel Câmara
# date: March 3rd, 2022

egh_connection <- function(auto_connect = TRUE,
                           use_env = TRUE,
                           path_env = "../.env",
                           user_db,
                           password_db){
  
  if (auto_connect == TRUE){
    system2("ssh", c("epigraph@epigraphhub.org -L 5432:localhost:5432", "-NC"), wait = FALSE)
    Sys.sleep(3)
    
    if (use_env == TRUE){
      # load .env file in the root folder (using the example in the EpiGraphHub)
      library(dotenv)
      load_dot_env(path_env)
      
      # loading the library
      library(RPostgres)
      # making the connection to the database
      con <- RPostgres::dbConnect(
        # loading the driver, we use the Postgres in the RPostgres library.
        drv = RPostgres::Postgres(),
        # loading the info from the .env file
        host = Sys.getenv("POSTGRES_HOST"),
        port = Sys.getenv("POSTGRES_PORT"),
        user = Sys.getenv("POSTGRES_USER"),
        password = Sys.getenv("POSTGRES_PASSWORD"),
        dbname = Sys.getenv("POSTGRES_DB")
      )
    } else if (use_env == FALSE){
      
      # loading the library
      library(RPostgres)
      # making the connection to the database using the user and password
      con <- RPostgres::dbConnect(
        RPostgres::Postgres(),
        host = "localhost",
        port = "5432",
        user = user_db,
        password = password_db,
        dbname = "epigraphhub"
      )
    }
  } else {
    stop("Please connect via the terminal: 'ssh -f epigraph@epigraphhub.org -L 5432:localhost:5432 -NC'")
  }
  
  print(con)
  
}

# End