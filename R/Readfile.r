#' Counts the Unique Values in a Column of a Dataframe
# counts unique values in a column
value_counts <- function(column) {
  values <- unique(column)
  k <- 0
  valuelist <- vector()
  for (i in values) {
    k <- k + 1
    valuelist <- append(valuelist, sum(column == i))
  }
  return(data.frame("name" = values, "number" = valuelist))
}




#' Manipulates the data in various ways
#' @Param wmean performs william's mean on data Default: TRUE
#' @Param startle Removes startle responce Default: TRUE
#' @Param fulldays Removes incomplete days Default: TRUE
#' @export

data_proc <- function(wmean=TRUE, startle=TRUE, fulldays=TRUE, path=FALSE) {

  # Imports all text files from the current directory and
  # excludes those already processed
  if (file != FALSE){
    all_files <- list.files(path=path, pattern = ".*.txt")
  } else{
  all_files <- list.files(pattern = ".*.txt")
  }
  strdeterm <- "proc"
  filecondition <- grepl(strdeterm, all_files)
  files <- subset (data.frame(all_files, filecondition), filecondition == FALSE)
  files <- files[, "all_files"]

  # loops for every file not already processed
  for (i in files) {
    # imports text file as dataframe
    readdata <- read.table(i, header = FALSE, sep = "\t")

    # Gets rid of incomplete days
    if (fulldays == TRUE) {
      values <- value_counts(readdata[, "V2"])
      cvalues <- subset(values, number != 1440)[, "name"]
      cdata <- readdata
      for (k in cvalues) {
        cdata <- subset(cdata, V2 != k)
        }
      readdata <- cdata
      }

    # gets rid of startle times
    if (startle == TRUE) {
      # creates time interval, may eventually rewrite to
      # allow user input.
      times <- paste(append(append(paste("18:0", 00:9, sep = ""), "18:10"), append(paste("06:0", 0:9, sep = ""), "06:10")), "00", sep = ":")
      tdata <- readdata
      for (k in times) {
        tdata <- subset(tdata, V3 != k)
        }
      readdata <- tdata
      }
    # applies William's mean with natural log
    if (wmean == TRUE) {
      fmean <- function(x) log(x+1)
      wmdata <- data.frame(readdata[1:10], fmean(readdata[11:42]))
      readdata <- wmdata
      }
      if (file != FALSE){
        filename <- paste(path,"/", strdeterm, "_", i)
      }
      filename <- paste(strdeterm, i, sep = "_")
      write.table(readdata, filename, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  }
