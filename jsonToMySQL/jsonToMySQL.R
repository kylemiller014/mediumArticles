# From Weather API to MySQL Database Function
#### Initialize required libraries ####
require(dplyr)
require(tidyr)
require(jsonlite)
require(RMySQL)
require(DBI)
require(expss)
require(httr)
#### FUNCTION ####
parseThatJson <- function(df # The data frame of the JSON you want to parse 
                            )
{
  
}
weatherJsonParse <- function(dbHost, # IP address of MySQL database
                             dbPort, # 4 digit port number of MySQL database
                             dbUser, # Username to access MySQL database
                             dbSchema, # Schema of interest within MySQL database
                             dbTable, # Table of interest within MySQL database
                             opf, # Output file directory
                             liveOrLog = 0, # 0 = Log, 1 = Live
                             uri # endpoint of interest
                                )
{
  #### Check if the user's environment has the required packages installed and available ####
  # dplyr
  if(!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Install the dplyr package - install.packages('dplyr')", call. = FALSE)
  }
  
  # tidyr
  if(!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Install the tidyr package - install.packages('tidyr')", call. = FALSE)
  }
  
  # jsonlite
  if(!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install the jsonlite package - install.packages('jsonlite')", call. = FALSE)
  }
  
  # RMySQL
  if(!requireNamespace("RMySQL", quietly = TRUE)) {
    stop("Install the RMySQL package - install.packages('RMySQL')", call. = FALSE)
  }
  
  # DBI
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("Install the DBI package - install.packages('DBI')", call. = FALSE)
  }
  # expss
  if(!requireNamespace("expss", quietly = TRUE)) {
    stop("Install the expss package - install.packages('expss')", call. = FALSE)
  }
  # httr
  if(!requireNamespace("httr", quietly = TRUE)) {
    stop("Install the httr package - install.packages('httr')", call. = FALSE)
  }
  
  #### Define MySQL database password from user input ####
  if(exists("dbPwd") == FALSE){
    dbPwd <- .rs.askForPassword(paste0("Please provide MySQL database password associated to ",
                                       dbUser, ": "))
  }
  
  #### Create generic MySQL variable naming conventions ####
  # Define schema.table naming convention
  schemaTable <- paste0(dbSchema,".",dbTable)
  
  # Create primary key column name query
  getPrimaryKeyColName <- paste0("SHOW columns FROM ", schemaTable, " where `Key` = 'PRI';")
  
  # Create all table column names query
  getAllOtherColNames <- paste0("SHOW columns FROM ", schemaTable, " where `Key` != 'PRI' & Extra != 'auto_increment';")
  
  #### Connect to MySQL database and get data ####
  # Initial connection
  conn <- dbConnect(MySQL(), host=dbHost, port=dbPort, username=dbUser, password=dbPwd)
  
  # Get primary key name
  queryPkResult <- dbSendQuery(conn, getPrimaryKeyColName)
  primaryKeyDf <- dbFetch(queryPkResult, -1)
  primaryKeyName <- primaryKeyDf[1,1]
  
  # Get all other column names
  queryAllColResult <- dbSendQuery(conn, getAllOtherColNames)
  allOtherColDf <- dbFetch(queryAllColResult, -1)
  columnNames <- paste(allOtherColDf$Field, collapse = ", ")
  
  #### Determine if static JSON files or live API call ####
  if(liveOrLog == 1){
    #### Make API call ####
    # Get raw JSON data from API
    res <- GET(paste0(uri))
    
    # Write JSON data to file directory of choice
    datetimeNow <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    write_json(res, paste0(opf, "/", "rawJson_", datetimeNow))
  } else {
    # Get list of valid file types in directory
    files <- list.files(opf, pattern = '.json')
  }
  #### Loop over all files in directory ####
  # Loop over files
  for(i in 1:length(files)){
    # Read in i JSON file
    rawJson <- paste0(c(opf, '/', files[i]), collapse = '')
    
    # Create data frame from JSON file
    df <- data.frame(fromJSON(rawJson, flatten = TRUE))
    
    # Parse the JSON into intended format for MySQL
    parsedJson <- parseThatJson(df)
    
    # Define end row value
    endRow <- nrow(df) - 1
    
    # Loop over each row in data frame from JSON file i
    for(j in 1:endRow){
      # Define an empty list for appending
      forFancyQuotes <- list()
      
      # Loop over each column in MySQL database
      for(k in 1:nrow(allOtherColDf)){
        # Append data of interest to list
        forFancyQuotes <- append(forFancyQuotes,
                                 eval(parse(text = paste0("sQuote(df[j,", k,"], options(useFancyQuotes = FALSE))"))))
      }
      
      # After looping over all the columns - add values to list of database query
      forQuery <- paste(forFancyQuotes, collapse = " ,")
      
      # Concatenate all lists into single MySQL query
      query <- paste0("INSERT INTO ", schemaTable, "(",
                      paste0(columnNames, collapse = ","), ") VALUES (",
                      paste(c(forQuery), collapse = " "), ");")
      
      # Send query to database
      dbSendQuery(conn, query)
    }
    # Close out database connection
    dbDisconnect(conn)
  }
}
# Testing….
weatherJsonParse("127.0.0.1", 
                 3306,
                 "kyle_dba",
                 "/Users/kylemiller/Medium Articles/genericJsonParse",
                 1,
                 "http://api.weatherapi.com/v1/current.json?key=19bab67245074f7a839171248253012&q=London")