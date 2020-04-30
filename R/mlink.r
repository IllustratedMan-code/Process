setwd("C:/Users/dalew/OneDrive/Documents/BioResearch/DataProcessing")


mlink <- function(metafile, proc = TRUE){
  meta <- file(metafile)
  metadata <- read.csv(metafile, header=TRUE)
  filenames <- unique(metadata[1])
  for (i in filenames){
    lines <- readLines(meta)

    combine <- list()
    curfile <- read.table(toString(i), header = FALSE, sep = "\t")
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

          df <- data.frame(m, curfile[metadata[a-1, 4] + 10])
          colnames(df)[ncol(df)] <- "V11"
          datalist[[a]] <- df

        }


      }
      finaldata <-do.call("rbind", datalist)
      colnames(finaldata) <- c(colnames(metadata), "data")
      return(finaldata)

  }


}


a <- mlink("metadata_lonestartick.csv")
a
