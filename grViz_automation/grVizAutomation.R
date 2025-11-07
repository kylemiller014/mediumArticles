# Diagrammer101
# Graph Visulaizations (grViz)
# Load required packages
require(DiagrammeR)
require(htmlwidgets)
require(DiagrammeRsvg)
require(rsvg)

#### Background/Introduction ####
# Simple example of grViz functionality

# Read in .csv file and start testing column manipulation required

#### Create function #####
csvToGrViz <- function(inputFile, # .CSV file required
                       fileDir, # File path for output dir
                       fileName, # Output file name
                       outputType = "HTML", # Should be .png, .pdf, or HTML [Default = HTML]
                       graphDir = "LR", # one of four options - 'LR', 'RL', 'TB', 'BT'"
                       graphType = "directed", # Either directed or undirected 
                       nodeSep = 0.25, # Define the separation between each node [0.02, 5.0]
                       rankSep = 0.5 # Defines the separation between each rank [0.02, 5.0]
                       ) 
  {
  #### Check if the user's environment has the required packages installed and available ####
  # DiagrammeR
  if(!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Install the Diagrammer package - install.packages('Diagrammer')", call. = FALSE)
  }
  # htmlwidgets
  if(!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Install the htmlwidgets package - install.packages('htmlwidgets')", call. = FALSE)
  }
  # DiagrammeR
  if(!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("Install the DiagrammeRsvg package - install.packages('DiagrammeRsvg')", call. = FALSE)
  }
  # rsvg
  if(!requireNamespace("rsvg", quietly = TRUE)) {
    stop("Install the rsvg package - install.packages('rsvg')", call. = FALSE)
  }
  #### Initial data validation check for user inputs ####
  # Check the "inputFile" path provided by the user
  # 1) Was a value provided?
  # 2) Does the file path include a .csv file extension
  # 3) Ensure no non-standard characters are provided within the file path that would impact reading a file
    if(missing(inputFile) || file_ext(as.character(inputFile)) != "csv" || !grepl("^[A-Za-z0-9._\\-/\\\\]+$", as.character(inputFile))){
      stop("File path provided does not point to a .csv file or includes non-standard characters - check the path provided", call. = FALSE)
    }
  
  # Check the "fileDir" path provided by the user
  # Confirm the path is found and can be reached from this enviornment
  if(missing(fileDir) || !file.exists(fileDir)){
    stop("Output file directory missing or not found - check the path provided", call. = FALSE)
  }
  
  # Check the "fileName" field provided by the user
  # 1) Was a value provided?
  # 2) IS the value a string value and longer than a single character
  # 3) Are there any non-standard characters in the string that could impact writing to a file
  if(missing(fileName) || as.character(fileName) < 1 || !grepl("^[A-Za-z0-9._\\-/\\\\]+$", as.character(fileName))){
    stop("File name missing, not longer than one character, or includes non-standard characters - check the path provided", call. = FALSE)
  }
  
  # Check the "outputType" field provided by the user
  if(missing(outputType) || upper(outputType) != "HTML" || upper(outputType)  != "PDF" || upper(outputType)  != "PNG"){
    # Set outputType to default value - "HTML"
    outputType <- "HTML"
    print("Output type value either missing or not one of three standard options - 'HTML', 'PDF', or 'PNG' ")
    print("Setting outputType to default value 'HTML' ")
  } else{
    # Ensure value is all caps for later use
    outputType <- upper(outputType)
  }
  
  # Check the "graphDir" field provided by the user
  if(missing(graphDir) || upper(as.character(graphDir) != "LR" || upper(as.character(graphDir))!= "RL" 
                       || upper(as.character(graphDir)) != "TB" || upper(as.character(graphDir)) != "BT")){
    graphDir <- "LR"
    print("Graph direction value either missing or not one of four options - 'LR', 'RL', 'TB', 'BT'")
    print("Setting graphDir value to default value - 'LR'")
  } else{
    # Ensure value is all caps for later use
    graphDir <- upper(as.character(graphDir))
  }
  # Check the "graphType" field provided by the user
  if(missing(graphType) || lower(as.character(graphType)) != "directed" || lower(as.character(graphType)) != "undirected"){
    graphType <- "directed"
    print("Graph type value either missing or not one of two options - directed or undirected")
    print("Setting graphType to default value - 'directed'")
  }
  
  # Check the "nodeSep" field provided by the user
  if(missing(nodeSep) || !is.numeric(nodeSep) || as.double(nodeSep) < 0.02 || as.double(nodeSep > 5.0)){
    nodeSep <- 0.25
    print("Node seperation value either missing or is not a numeric value between 0.02 and 5.0")
    print("Setting nodeSep to the default value - 0.25")
  }
  
  # Check the "rankSep" field provided by the user
  if(missing(rankSep) || !is.numeric(rankSep) || as.double(rankSep) < 0.02 || as.double(rankSep > 5.0)){
    rankSep <- 0.5
    print("Rank seperation value either missing or is not a numeric value between 0.02 and 5.0")
    print("Setting rankSep to the default value - 0.5")
  }
}