#Checks to make sure a file is below the bufferSizeLimit.
checkFileSize <- function(fileName, bufferSizeLimit)
{
  #Scalar for megabyte
  mb <- 10^6
  if(file.size(fileName) < bufferSizeLimit * mb)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}

#Makes sure that the maximum number of rows from a CSV (within the bufferSizeLimit) are read.
checkRowSize <- function(start, rowCount)
{
  if(rowCount - start < rowIntakeLimit)
  {
    return(rowCount - start)
  }
  else if(start == rowCount)
  {
    return(0)
  }
  else
  {
    return(rowIntakeLimit)
  }
}

# Creates a new directory if the current one does not exist.
checkDirectory <- function(directoryName)
{
  if (!dir.exists(directoryName)) 
  {
    dir.create(directoryName, recursive = TRUE)
  }
}

#Creates a new CSV if the current one does not exist.
#Header is defined globally in another function
checkCSV<-function(fileName)
{
  if (file.exists(fileName)==FALSE) 
  {
    fwrite(header,fileName)
  }
}

#This function either creates a new file if one that follows the patternOut does not exist.
#Or, if there is a match for patternOut, returns the match with the highest suffix.
#Regardless, its output should be patternOut + _highestsuffix
checkFile <-function(filesInDir, year,currentDirectory,header)
{  
    filteredFilesInDir <- filesInDir[grep(patternOut, filesInDir)]
    if(identical(filteredFilesInDir, character(0)))
    {
      checkDirectory(currentDirectory)
      currentFilename <- file.path(currentDirectory, paste0(patternOut, year,"_0", ".csv"))
      checkCSV(currentFilename)
    }
    else
    {
      currentFilename<-sortCSVFiles(filteredFilesInDir,year)
    }
    return(currentFilename)
}

#This function is responsible for returning the CSV file with the largest index (I.E.)
# ["table_1.csv", "table_2.csv", "table_11.csv"] -> "table_11.csv"
sortCSVFiles <-function(filesInDir,year)
{
    result <- character()
    maxValue<- -1
    for(item in filesInDir){
      searchTerm <- paste0(year, "_")
      start <-str_locate(item,as.character(searchTerm))[2]
      end <-str_locate(item,".csv")[1]
      curValue <- as.numeric(substr(item, start+1, end))
      if(curValue>maxValue){
        result<-item
        maxValue<-curValue
      }
    }
    return(result)
}

#Returns the last row in CSV
getRowCount <- function(fileName)
{
  comm <- sprintf("cat %s |wc -l ", fileName)
  rowCount<- as.numeric(system(comm, intern = TRUE)) - 1 
  return(rowCount)
}

#Returns the header from a CSV file
getHeader <- function(fileName)
{
  header <- fread(fileName, nrows = 0)
  return(header)
}

#Updaters#
#This function increments the last filepath in a directory by one. 
#I.E. "malcolm_2.csv" -> "malcolm_3.csv"
#Used primarily once the buffer size limit is hit.
updateFilepath <- function(currentFilepath, currentDirectory)
{
  rootFilename <-substr(basename(currentFilepath),0,nchar(basename(currentFilepath))-nchar(".csv"))
  toIteration <- nchar(patternOut)+5
  iteration <- as.numeric(substr(rootFilename, toIteration+1, nchar(rootFilename))) + 1
  updatedFilename <- paste0(substr(rootFilename,0,toIteration),as.character.numeric_version(iteration), ".csv")
  result <- file.path(currentDirectory, updatedFilename)
  return(result)
}

#Creates a directory with a year!
updateDirectory<- function(year, process)
{
    currentDirectory <- paste0(DATA_DIRNAME, year, "/")
    checkDirectory(currentDirectory)
    return(currentDirectory)
}



# This function batches multiple files into a list of lists.
# Each list contains file paths that total at or less than the buffer size limit.
batchFiles <- function(filePaths)
{ result <- list()
  batch <- list()
  sum <-0
  count <- 1
  batchCount<-1
  for(filePath in filePaths)
  {
    currentSize <- file.size(filePath)
    if (currentSize + sum < (bufferSizeLimit * 10^6))
    {
      sum <- sum + currentSize
      batch[batchCount] <- filePath
      batchCount <- batchCount + 1
    }
    else
    {
      if(length(batch)>0)
      {
      result[[count]] <- batch
      batch <- list()
      count <- count +1
      sum <- currentSize
      batch[1] <- filePath
      batchCount <- batchCount +1
      }
    }
  }
  result[[count]] <- batch
  return(result)
}

#This function determines how many rows can be read from a csv file while
#staying under the buffer size limit.
calculateRowIntakeLimit <- function(fileSize, rowCount)
{
    avgRowSize <- fileSize/rowCount
    rowIntakeLimit <- as.integer(bufferSizeLimit * 10^6 / avgRowSize)
    #Sets row intake limit globally
    assign("rowIntakeLimit", rowIntakeLimit, envir = .GlobalEnv)
}

#This function acts as a switch statement to set global variables for each
#table reading process
globalSwitcher <- function(process)
{
  if(process == "sortFact")
  {
    header <- cbind(SOURCE_FILENAME= NA, getHeader(factTableFPS[1]))
    patternOut <- "standard_nmr_feed_fact_table_"
    filePaths <- factTableFPS
    colClassesUsed <- c("character","character","character","character","character", "character","character",
    "character", "character", "character", "character", "character", "character",
    "character","character","character","character","character",
    "character", "character")
    filterFunction <- function(DFS, currDF){}
  }
  if(process == "sortStatic")
  {
    header <- cbind(SOURCE_FILENAME= NA, getHeader(staticTableFPS[1]))
    patternOut <- "standard_nmr_feed_static_table_"
    filePaths <- staticTableFPS
    colClassesUsed <- c("character", "character", "integer", "numeric", "numeric")
    filterFunction <- function(DFS,currDF){return(filter(DFS, DFS$USER_ID %in% currDF$USER_ID))}
  }
  if(process == "sortItem")
  {
    header <- cbind(SOURCE_FILENAME= NA, getHeader(itemTableFPS[1]))
    patternOut <- "standard_nmr_feed_item_table_"
    filePaths <- itemTableFPS
    colClassesUsed <- c("integer64", "integer64", "integer64", "character", "character","character","character","character","character","character",
    "character","character","character","character","character","character","character","character","character","character","character","character",
    "character")
    filterFunction <- function(DFS,currDF){return(filter(DFS, DFS$ITEM_ID %in% currDF$ITEM_ID))}
  }
  if(process == "sortBanner")
  {
    header <- cbind(SOURCE_FILENAME= NA, getHeader(bannerTableFPS[1]))
    patternOut <- "standard_nmr_feed_banner_table_"
    filePaths <- bannerTableFPS
    colClassesUsed <- c("character","character","character","character","character")
    filterFunction <- function(DFS,currDF){return(filter(DFS, DFS$BANNER_ID %in% currDF$BANNER_ID))}
  }
  if(process == "sortPeopleAttributes")
  {
    header <- cbind(SOURCE_FILENAME= NA, getHeader(peopleAttributesTableFPS[1]))
    patternOut <- "standard_nmr_feed_people_attributes_table_"
    filePaths <- peopleAttributesTableFPS
    colClassesUsed <- c("integer", "character", "character", "character", "character","character",
    "character","character","character","character","character",
    "character","character","character","character","character","character",
    "character","character","character","character","integer", "character")
    filterFunction <- function(DFS,currDF){return(filter(DFS, DFS$USER_ID %in% currDF$USER_ID))}
  }
  if(process == "sortPeople")
  {
    header <- cbind(SOURCE_FILENAME= NA, getHeader(peopleTableFPS[1]))
    patternOut <- "standard_nmr_feed_people_table_"
    filePaths <- peopleTableFPS
    colClassesUsed <- c("integer", "character", "character", "character", "character","character",
    "character","character","character","character","character",
    "character","character","character","character","character","character",
    "character","character","character","character","integer", "character")
    filterFunction <- function(DFS,currDF){return(filter(DFS, DFS$USER_ID %in% currDF$USER_ID))}
  }
    assign("header", header, envir = .GlobalEnv)
    assign("patternOut", patternOut, envir = .GlobalEnv)
    assign("filePaths", filePaths, envir = .GlobalEnv)
    assign("colClassesUsed", colClassesUsed, envir = .GlobalEnv)
    assign("filterFunction", filterFunction, envir = .GlobalEnv)
}


#This function proccesses one Fact Table at a time in chunks to avoid memory issues.
processAllTable <- function(filePath, process)
{
    rowCount <- getRowCount(filePath)
    calculateRowIntakeLimit(file.size(filePath), rowCount)
    start <- 0
    while(start < rowCount)
    {
        rowsToRead <- checkRowSize(start, rowCount)
        sendToProcessor(filePath, start, rowsToRead, process)
        start <- start + rowsToRead
    }
}
#This function loads the Fact Table and then routes it to the appropriate filter and data table 
#for filtering.
sendToProcessor <- function(filePath, start, rowsToRead, process)
{
    globalSwitcher("sortFact")
    currDF <- fread(filePath,  sep = '|', skip = start, nrows = rowsToRead, header=FALSE)
    currDF <- addFilepathColumn(currDF,filePath)
    years <- unique(substring(currDF[["TRANSACTION_DATE"]], 1, 4))
    if(process == "sortFact")
    {
      processTable(years, currDF, process)
    }
    else
    {
        globalSwitcher(process)
        batchedPaths <- batchFiles(filePaths)
        batchProcessTable(years,batchedPaths, currDF, process)
    }
}

#If there's only one file per batch -- meaning it was the same size or larger
#than the buffer size limit -- this incrementally loads the large file.
#If, however, multiple input files were smaller collectively than the buffer
#size limit this loads them all at once.
batchProcessTable <- function(years,batchedPaths,currDF, process)
{
  for(paths in batchedPaths)
    { 
      if(length(unlist(paths))> 1)
      {
        DFS <- batchLoadFilepaths(paths)
        processTable(years, currDF, process, DFS)
      }
      else
      {
        filePath<-unlist(paths[1])
        start <- 1                
        rowCount <- getRowCount(filePath)
        while(start < rowCount)
        {
          rowsToRead <- checkRowSize(start, rowCount)
          start <- partialLoad(years,filePath,currDF, start, rowsToRead, process)
        }
      }
    }
}

# Adds a column to the Data Table that has the filePath on it.
addFilepathColumn <- function(currDF, filePath)
{
    currDF$source_filepath <- basename(filePath)
    currDF <- currDF %>% relocate(source_filepath)
    colnames(currDF) <- colnames(header)
    return(currDF)
}

#Loads and combines multiple filepaths
batchLoadFilepaths <- function(filePaths)
{
    resultList <- list()
    result <- lapply(unlist(filePaths), fread)
    for(item in 1:length(result))
    {
      currDF <- as.data.table(result[item])
      currDF <- addFilepathColumn(currDF, unlist(filePaths)[item])
      resultList <- c(resultList, list(currDF))
    }
    result <- rbindlist(resultList, use.names = TRUE)
    return(result) 
}

# Loads and filters a subset of a larger file
partialLoad <- function(years, filePath, currDF, start, rowsToRead, process)
{
    DFS <- fread(filePath,  sep = '|', skip = start, nrows = rowsToRead, header=FALSE, colClasses = colClassesUsed)
    DFS <- addFilepathColumn(DFS,filePath)
    processTable(years, currDF, process, DFS)
    start <- start + rowsToRead
    return(start)
}

#Processes a table by years and writes a csv
processTable <- function(years, currDF, process, DFS)
{ 
    for(year in years) 
        {
        currentYear<- currDF[substring(currDF[["TRANSACTION_DATE"]], 1, 4) == year]
        if(missing(DFS)==FALSE)
        {
          currentYear <-filterFunction(DFS, currentYear)  
        }
        currentDirectory<- updateDirectory(year, process)
        writeCSV(currentYear, currentDirectory,year, process)
        }
}

#Self evident -- it writes the final csvs.
writeCSV <- function(currentYear, currentDirectory, year, process)
{
  filesInDir <- list.files(currentDirectory, patternOut, full.names = TRUE)
  currentFilename <- checkFile(filesInDir, year, currentDirectory, header)
  if(checkFileSize(currentFilename, 200*bufferSizeLimit))
  {
    fwrite(currentYear, currentFilename, append = TRUE)
  }
  else
  { 
    currentFilename <- updateFilepath(currentFilename, year, currentDirectory, process)
    checkCSV(currentFilename)
    fwrite(currentYear, currentFilename, append=TRUE)
  }
}
