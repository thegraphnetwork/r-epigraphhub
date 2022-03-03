# author: Daniel Câmara
# date: March 3rd, 2022

system2("ssh", c("epigraph@epigraphhub.org -L 5432:localhost:5432", "-NC"), wait = FALSE)

choice_env <- menu(title = "Would you like to load your .env file?", 
                   choices = c("Yes", "No"))

if(choice_env == 1){
  # load .env file in the root folder (using the example in the EpiGraphHub)
  library(dotenv)
  load_dot_env(file = "../.env")
  
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
} else if(choice_env == 2){
  # manually entering values to be passed to the connection
  db_user <- readline("Please, enter the user:")
  db_password <- readline("Please, enter the password:")
  
  # loading the library
  library(RPostgres)
  # making the connection to the database using the user and password
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    port = "5432",
    user = db_user,
    password = db_password,
    dbname = "epigraphhub"
  )
}

# End