setwd("C:/Users/dalew/OneDrive/Documents/BioResearch/DataProcessing")
#' links the metadata to the experiment data (CAUTION: VERY SLOW)
mlink <- function (metadata, processed = TRUE, bound = TRUE) {
  # imports metadata
  metatable <- read.csv(metadata)
  filenames <- unique(metatable[1])

  
  # loops for every unique file in the metadata
  for (i in filenames) {

    if (processed == TRUE) {
      file <- paste("proc", i, sep = "_")

    } else {
      file <- i
    }
    filemeta <- subset(metatable, metatable[1] == toString(i))
    imfile <- read.table(file, header = FALSE, sep = "\t")
    columnnumb <- filemeta[1, 4] + 10
    fdata <- data.frame(do.call("rbind", replicate(nrow(imfile), filemeta[1, ], simplify = FALSE)), imfile[1:10], imfile[columnnumb])
    for (i in seq(2, nrow(filemeta))) {
      columnnumb <- filemeta[i, 4] + 10
      ndata <- data.frame(do.call("rbind", replicate(nrow(imfile), filemeta[i, ], simplify = FALSE)), imfile[1:10], imfile[columnnumb])
      ndata
      colnames(ndata)[ncol(ndata)] <- "V11"
      fdata <- rbind(fdata, ndata)
    }
    return(fdata)



  }

}
a <- mlink("metadata_lonestartick.csv")
a
