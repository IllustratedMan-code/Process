#' converts time to seconds
#' @export
convert_time <- function(time) {
  tenhours <- strtoi(substr(time, 1, 1))
  hours <- strtoi(substr(time, 2, 2))
  hours <- strtoi(tenhours * 10 + hours)
  tenminutes <- strtoi(substr(time, 4, 4))
  minutes <- strtoi(substr(time, 5, 5))
  minutes <- tenminutes * 10 + minutes
  tenseconds <- strtoi(substr(time, 7, 7))
  seconds <- strtoi(substr(time, 8, 8))
  seconds <- tenseconds * 10 + seconds
  return(hours * 60 * 60 + minutes * 60 + seconds)
}

#' links the data to the metadata
#' @export
mlink <- function(metafile, proc = TRUE){

  meta <- file(metafile)
  metadata <- read.csv(metafile, header=TRUE)
  filenames <- unique(metadata[1])
  lines <- readLines(meta)
  close(meta)
  for (i in filenames){
    if (proc == TRUE){
        file <- paste("proc", i, sep = "_")
    }


    combine <- list()
    curfile <- read.table(toString(file), header = FALSE, sep = "\t")
    numbrows <- nrow(curfile)
    it <- 0
    datalist <- list()

    for (a in seq(2, length(lines))) {

      line <- unlist(strsplit(lines[a], split = ","))
      if (line[1] == toString(i)){

        for (l in seq(1, numbrows)){
          combine[[l]] <- line

          }

          m <- matrix(unlist(combine), nrow=numbrows, byrow = TRUE)

          df <- data.frame(m, curfile[2], curfile[3], curfile[metadata[a-1, 4] + 10])
          colnames(df)[ncol(df)] <- "V11"
          datalist[[a]] <- df

        }


      }
      finaldata <-do.call("rbind", datalist)
      colnames(finaldata) <- c(colnames(metadata), "date", "time", "data")
      finaldata['time'] <- sapply(finaldata['time'],  convert_time)
      return(finaldata)

  }


}
