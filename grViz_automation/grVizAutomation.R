# Diagrammer101
# Graph Visulaizations (grViz)
# Load required packages
require(DiagrammeR)
require(htmlwidgets)
require(DiagrammeRsvg)
require(rsvg)
require(stringr)
require(tools)
require(dplyr)

#### Background/Introduction ####
# Simple example of grViz functionality
grViz("digraph {graph [rankdir = LR]
tab1[label ='duck', shape = circle, style = filled, fillcolor = gold]
tab2[label ='duck', shape = circle, style = filled, fillcolor = gold]
tab3[label ='duck', shape = circle, style = filled, fillcolor = gold]
tab4[label ='duck', shape = circle, style = filled, fillcolor = gold]
tab5[label ='duck', shape = circle, style = filled, fillcolor = gold]
tab6[label ='duck', shape = circle, style = filled, fillcolor = gold]
tab7[label ='GOOSE', shape = circle, style = filled, fillcolor = gray90]

tab1->tab2
tab2->tab3
tab3->tab4
tab4->tab5
tab5->tab6
tab6->tab1
}
")

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
  # stringr
  if(!requireNamespace("stringr", quietly = TRUE)) {
    stop("Install the stringr package - install.packages('stringr')", call. = FALSE)
  }
  # tools
  if(!requireNamespace("tools", quietly = TRUE)) {
    stop("Install the tools package - install.packages('tools')", call. = FALSE)
  }
  # dplyr
  if(!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Install the dplyr package - install.packages('dplyr')", call. = FALSE)
  }
  #### Initial data validation check for user inputs ####
  # Check the "inputFile" path provided by the user
  # 1) Was a value provided?
  # 2) Does the file path include a .csv file extension
  # 3) Ensure no non-standard characters are provided within the file path that would impact reading a file
  # Regex allows:
  # allows A–Z, a–z, 0–9
  # allows spaces
  # allows . _ - / \
  # puts - at the end to avoid invalid ranges
  # handles R's string-escaping correctly
    if(missing(inputFile) || file_ext(as.character(inputFile)) != "csv" || !grepl("^[A-Za-z0-9._ /\\\\-]+$", as.character(inputFile))){
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
  if(missing(fileName) || as.character(fileName) < 1 || !grepl("^[A-Za-z0-9._ /\\\\-]+$", as.character(fileName))){
    stop("File name missing, not longer than one character, or includes non-standard characters - check the path provided", call. = FALSE)
  }
  
  # Check the "outputType" field provided by the user
  if(missing(outputType) || (str_to_upper(outputType) != "HTML" && str_to_upper(outputType)  != "PDF" && str_to_upper(outputType)  != "PNG")){
    # Set outputType to default value - "HTML"
    outputType <- "HTML"
    print("Output type value either missing or not one of three standard options - 'HTML', 'PDF', or 'PNG' ")
    print("Setting outputType to default value 'HTML' ")
  } else{
    # Ensure value is all caps for later use
    outputType <- str_to_upper(outputType)
  }
  
  # Check the "graphDir" field provided by the user
  if(missing(graphDir) || (str_to_upper(as.character(graphDir)) != "LR" && str_to_upper(as.character(graphDir))!= "RL" 
     && str_to_upper(as.character(graphDir)) != "TB" && str_to_upper(as.character(graphDir)) != "BT")){
    graphDir <- "LR"
    print("Graph direction value either missing or not one of four options - 'LR', 'RL', 'TB', 'BT'")
    print("Setting graphDir value to default value - 'LR'")
  } else{
    # Ensure value is all caps for later use
    graphDir <- str_to_upper(as.character(graphDir))
  }
  # Check the "graphType" field provided by the user
  if(missing(graphType) || (str_to_lower(as.character(graphType)) != "directed" && str_to_lower(as.character(graphType)) != "undirected")){
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
  
  # Following input validation checks - read in .CSV file
  # Read in .csv 
  df <- read.csv(paste0(inputFile))
  
  # Remove any columns that were read in as all "NA" on import
  df <- df %>%
    select(where(~ !all(is.na(.x))))
  
  # Get a list of column data types to ensure only string values provided
  colDataTypes <- list(sapply(df, class))
  
  # Check column count
  if(ncol(df) != 6){
    stop("CSV file did not have the correct number of columns... 
         Check file provided and try again", call. = FALSE)
  
    # Determine if any columns contain non-string values
  } else if(length(unique(colDataTypes)) != 1){
    stop("CSV file contained columns with data types other than string... 
         Check file provided and try again", call. = FALSE)
    
    # Standardized column naming conventions
    # Print the df to the console for user / debugging purposes...
  } else {
    print("Standardizing column names...")
    colnames(df) <- c("Name", "Description", "Color", "NodeShape", "From", "To")
    print(df)
  }
  
  # Specific column handling
  # Name - remove spaces or any weird characters
  df$Name <- str_replace_all(df$Name, " ", "")
  df$Name <- str_replace_all(df$Name, "[^[:alnum:]]", "")
  
  # Get a list of unique node names
  uniqueNodeNames <- unique(df$Name)
  print(paste0("Number of user defined nodes: ",length(uniqueNodeNames)))
  
  # Color - Determine the unique colors provided by the user
  # Check if any of the colors do not exist / are not valid
  # If yes - replace with standard color and warn user
  # List of all available colors for use
  validColors <- colors()
  userProvidedColors <- df$Colors
  
  # Determine if user provided colors are valid
  colorCheck <- str_to_lower(userProvidedColors) %in% str_to_lower(validColors)
  
  if(all(colorCheck)){
    print("All user provided node colors are valid...")
  } else{
    # Replace nonstandard colors with "gray90" and warn user
    userProvidedColors[!colorCheck] <- "gray90"
    print("One or more invalid colors provided in 'Color' column...
          Replacing invalid colors with 'gray90'...")
  }
  
  # NodeShape - limit the shapes available to the following:
  validNodeShapes <- c(
    "box","polygon","ellipse","oval","circle","point","egg","triangle",
    "plaintext","plain","diamond","trapezium","parallelogram","house",
    "pentagon","hexagon","septagon","octagon","doublecircle",
    "doubleoctagon","tripleoctagon","invtriangle","invtrapezium",
    "invhouse","Mdiamond","Msquare","Mcircle","rect","rectangle","square",
    "star","none","underline","cylinder","note","tab","folder","box3d",
    "component","promoter","cds","terminator","utr","primersite",
    "restrictionsite","fivepoverhang","threepoverhang","noverhang",
    "assembly","signature","insulator","ribosite","rnastab","proteasesite",
    "proteinase","proteinstab","rpromoter","rarrow","larrow","record",
    "Mrecord"
  )
  userProvidedShapes <- df$NodeShape
  
  # Determine if user provided node shapes are valid
  nodeShapeCheck <- str_to_lower(userProvidedShapes) %in% str_to_lower(validNodeShapes)
  
  if(all(nodeShapeCheck)){
    print("All user provided node shapes are valid...")
  } else{
    # Replace nonstandard shapes with "box" and warn user
    userProvidedShapes[!nodeShapeCheck] <- "box"
    print("One or more invalid node shapes provided in 'NodeShape' column...
          Replacing invalid shapes with 'box'...")
  }
  # From - check if node names provided are also in name column
  # Perform same normalization applied to 'Name' column
  df$From <- str_replace_all(df$From, " ", "")
  df$From <- str_replace_all(df$From, "[^[:alnum:]]", "")
  
  # Get rid of any blanks
  fromClean <- df$From[df$From != ""]
  uniqueFromNodes <- unique(fromClean)
  fromNodeCheck <- uniqueFromNodes %in% uniqueNodeNames
  if(!all(fromNodeCheck)){
    stop("Node names in 'From' column not found in 'Name' column 
         Check file provided and try again", call. = FALSE)
  }
  
  # To - check if node names provided are also in name column
  # Perform same normalization applied to 'Name' column
  df$To <- str_replace_all(df$To, " ", "")
  df$To <- str_replace_all(df$To, "[^[:alnum:]]", "")
  
  # Get rid of any blanks
  toClean <- df$To[df$To != ""]
  uniqueToNodes <- unique(toClean)
  toNodeCheck <- uniqueToNodes %in% uniqueNodeNames
  if(!all(toNodeCheck)){
    stop("Node names in 'To' column not found in 'Name' column 
         Check file provided and try again", call. = FALSE)
  }
  
  # Create new columns to set the index of each node name and description
  # Name Index
  df$NameId <- paste0("@@", seq_len(nrow(df)))
  
  # Description Index
  df$DescId <- paste0("@@", seq_len(nrow(df)))
  
  # Create edge connection descriptions column
  # FROM -> TO
  # Determine the graph type provided by the user (or default)
  if(graphType == "directed"){
    arrowStyle <- "->"
  } else{
    arrowStyle <- "-"
  }
  
  # Create new column - EdgeConnections
  df$EdgeConnections <- paste0(df$From,arrowStyle,df$To)
  
  # Edge connector df to get rid of undeeded nulls
  finalEdgeConnections <- df %>%
    filter(nchar(EdgeConnections) > 4)
  
  # Create new column - NodeBlock
  df$NodeBlock <- paste0(df$Name,"[label ='", df$DescId, "', shape = ", df$NodeShape, ", style = filled, fillcolor = ", df$Color, "]")
  
  # Create new column - attribute block
  df$Description <- str_replace_all(df$Description, "'", "")
  
  # df$AttributeBlock <- paste0("[", seq_len(nrow(df)),"]: df[",seq_len(nrow(df)),", 2];" )
  df$AttributeBlock <- paste0("[", seq_len(nrow(df)),"]: '",df$Description,"';" )
  
  # Concat a massive string that includes all information needed
  # Initial graph layout
  initialBlock <- paste0("digraph {",
                         "graph [rankdir = ", graphDir, "]")
  writeLines(initialBlock, paste0(fileDir,fileName,".txt"))
  
  # Node definiton
  nodeBlock <- paste(df$NodeBlock)
  write(nodeBlock, paste0(fileDir,fileName,".txt"), append = TRUE)

  # Edge Connections
  edgeBlock <- paste(finalEdgeConnections$EdgeConnections)
  write(edgeBlock, paste0(fileDir,fileName,".txt"), append = TRUE)
  write("}", paste0(fileDir,fileName,".txt"), append = TRUE)
  
  # Create attribute defintion block
  attributeBlock <- paste(df$AttributeBlock)
  write(attributeBlock, paste0(fileDir,fileName,".txt"), append = TRUE)

  # Read the text file back into R and feed it to grViz
  grVizStr <- readLines(paste0(fileDir,fileName,".txt"))
  staticGraph <- grViz(grVizStr)
  
  # Write graphic to whichever output option selected by user
  if(outputType == "HTML"){
    # HTML
    saveWidget(staticGraph, paste0(fileDir,fileName,".html"), selfcontained = TRUE)
  } else {
    svgCode <- export_svg(staticGraph)
    # PNG
    if(outputType == "PNG"){
      rsvg_png(charToRaw(svgCode), file = paste0(fileDir,fileName,".png"), width = 800, height = 600)
    }else{
      # PDF
      rsvg_pdf(charToRaw(svgCode), file = paste0(fileDir,fileName,".pdf"), width = 800, height = 600)
    }
  }
  # Return datas frame to user for debug or tweaks
  return(df)
}

wow <- csvToGrViz(inputFile = "/Users/kylemiller/Medium Articles/grViz automation/csvToGrViz.csv",
                      fileDir = "/Users/kylemiller/Medium Articles/grViz automation/", 
                      fileName = "sickGraphicDude", 
                      outputType = "HTML", 
                      graphDir = "LR", 
                      graphType = "directed", 
                      nodeSep = 0.25, 
                      rankSep = 0.5) 