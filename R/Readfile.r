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
converttoseconds <- function(time) {
    sum <- strtoi(substr(time, 1, 1)) * 60 * 60 * 10 +
                strtoi(substr(time, 2, 2)) * 60 * 60 +
                strtoi(substr(time, 4, 4)) * 60 * 10 +
                strtoi(substr(time, 5, 5)) * 60 +
                strtoi(substr(time, 7, 7)) * 10 +
                strtoi(substr(time, 8, 8))
    return(sum)
}
converttotime <- function(seconds) {
    time <- "00:00:00"
    substr(time, 1, 1) <- toString(as.integer(seconds / (60 * 60 * 10)))
    seconds <- seconds - as.integer(seconds / (60 * 60 * 10)) * 60 * 60 * 10
    substr(time, 2, 2) <- toString(as.integer(seconds / (60 * 60)))
    seconds <- seconds - as.integer(seconds / (60 * 60)) * 60 * 60
    substr(time, 4, 4) <- toString(as.integer(seconds / (60 * 10)))
    seconds <- seconds - as.integer(seconds / (60 * 10)) * 60 * 10
    substr(time, 5, 5) <- toString(as.integer(seconds / 60))
    seconds <- seconds - as.integer(seconds / 60) * 60
    substr(time, 7, 7) <- toString(as.integer(seconds / 10))
    seconds <- seconds - as.integer(seconds / 10) * 10
    substr(time, 8, 8) <- toString(as.integer(seconds))
    return(time)
}

timerange <- function(itime, ftime, by=60) {
  iseconds <- converttoseconds(itime)
  fseconds <- converttoseconds(ftime)
  t <- iseconds
  tlist <- list()
  while (t <= fseconds) {
    tlist <- c(tlist, converttotime(t))
    t <- t + by
  }
  return(tlist)
}

timegenerator <- function(listoftimes, by=60) {
  totaltimes = list()
  for(i in seq(1, length(listoftimes), 2)){
    totaltimes <- c(totaltimes, timerange(listoftimes[i], listoftimes[i+1], by))
  }
  return(totaltimes)
}


#' Manipulates the data in various ways
#' @Param wmean performs william's mean on data Default: TRUE
#' @Param startle Removes startle responce Default: TRUE
#' @Param fulldays Removes incomplete days Default: TRUE
#' @export

data_proc <- function(wmean=TRUE, startle=TRUE, fulldays=TRUE, path=FALSE, by=60, startlelist) {

  # Imports all text files from the current directory and
  # excludes those already processed
  if (path != FALSE){
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
    a <- i
    if (path != FALSE){
      i <- paste(path, "/", i, sep = "")

    }
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
      times <- timegenerator(startlelist, by = by)
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
      if (path != FALSE){
        filename <- paste(path,"/", strdeterm, "_", a, sep="")

      }else{
      filename <- paste(strdeterm, a, sep = "_")
      }
      write.table(readdata, filename, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  }
