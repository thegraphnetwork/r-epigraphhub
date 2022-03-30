#' Connect to the EpiGraphHub database via SSH tunneling
#'
#' \code{egh_connection} connects to the EpiGraphHub database using a .env local file or by manually providing information.
#'
#' This function connects to the EpiGraphHub (https://epigraphhub.org) database via SSH tunneling. User is required to have a SSH key. Default connection parameter can be used or provided by the user. The user can load a local .env file or provide the parameters inside the \code{egh_connection}.
#'
#' @param auto_connect logical. Should the function use a default connection? Automatically calls the session terminal and makes the connection.
#' @param use_env logical. Should the function use the parameters configured in a local .env file?
#' @param path_env character. If use_env = TRUE, the user should provide a path to the local .env file.
#' @param user,password if use_env = FALSE, please provide access parameters to the EpiGraphHub database.
#'
#' @section Warning:
#' A Internet connection is needed to use this function.
#'
#' Watch out for hardcoded login and password.
#'
#' @examples \dontrun{
#' egh_connection(auto_connect = TRUE, use_env = TRUE, path_env = "../.env")
#'
#' egh_connection(auto_connect = TRUE, use_env = FALSE, user = "user", password = "password")
#' }
#' @import dotenv
#' @import RPostgres
#' @export

egh_connection <- function(auto_connect = TRUE,
                           use_env = TRUE,
                           path_env = "../.env",
                           user,
                           password){

  if (auto_connect == TRUE){
    system2("ssh", c("epigraph@epigraphhub.org -L 5432:localhost:5432", "-NC"), wait = FALSE)
    Sys.sleep(3)

    if (use_env == TRUE){
      # load .env file in the root folder (using the example in the EpiGraphHub)
      load_dot_env(path_env)

      # making the connection to the database
      con <- RPostgres::dbConnect(
        drv = RPostgres::Postgres(),
        # loading the info from the .env file
        host = Sys.getenv("POSTGRES_HOST"),
        port = Sys.getenv("POSTGRES_PORT"),
        user = Sys.getenv("POSTGRES_USER"),
        password = Sys.getenv("POSTGRES_PASSWORD"),
        dbname = Sys.getenv("POSTGRES_DB")
      )
    } else if (use_env == FALSE){

      # making the connection to the database using the user and password
      con <- RPostgres::dbConnect(
        RPostgres::Postgres(),
        host = "localhost",
        port = "5432",
        user = user,
        password = password,
        dbname = "epigraphhub"
      )
    }
  } else {
    stop("Please connect via the terminal. Example: 'ssh -f epigraph@epigraphhub.org -L 5432:localhost:5432 -NC'")
  }

  print(con)

}

# End
