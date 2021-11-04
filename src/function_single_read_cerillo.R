# Function to read a single measurement from the Cerillo plate reader
library(tidyr)

read.cerillo.single <- function(file, blank_wells=NULL) {
  line_nb <- system(paste("wc -l", file), intern=T) %>%
    gsub(pattern = " .*", replacement = "")
  if(line_nb != "7") stop("The file does not contain the right number of lines
                          Check that it is a single measurement file.")
    
  run_info <- system(paste("head -n 5", file),intern = T) %>%
    gsub(pattern = ",\r", replacement = "")
  d <- system(paste("tail -n 2", file), intern = T) %>%
    strsplit(",")
  
  run_info <- c(run_info, 
                paste(d[[1]][1], d[[2]][1]), # stores the UNIX Time stamp
                paste("Time of measurment:", as.POSIXct(as.numeric(d[[2]][1]), origin="1970-01-01")),
                paste(d[[1]][2], d[[2]][2])) # stores the temperature 
  res <- data.frame(
    "well"=d[[1]][-c(1,2,99)],
    "raw_OD"=as.numeric(d[[2]][-c(1,2,99)])
  )
  well_order <- paste0(LETTERS[1:8], rep(1:12,each=8))
  res <- res[match(well_order, res$well),]
  if(!is.null(blank_wells)) {
    mean_OD_blank <- 
      blank_wells %>%
      match(res$well) %>%
      res[.,"raw_OD"] %>%
      mean
    
    res$adjusted_OD <-
      res$raw_OD - mean_OD_blank 
  }
  
  res$col <- cut(res$raw_OD, breaks=9) %>%
    as.numeric %>%
    RColorBrewer::brewer.pal(9,"YlOrBr")[.]
  l <- LETTERS[1:8]
  #pdf("plate_map.pdf", 7,5)
  par(bty="n", xpd=T, las=1, mar=c(2,2,2,2),fg="gray30", col.axis="gray30")
  plot(rep(1:12, each=8), rep(8:1, 12), axes=F,
       xlab="", ylab="",
       pch=21, cex=5, col="darkgray", bg=res$col)
  text(rep(1:12, each=8), rep(8:1, 12), 
       round(res$raw_OD, digits = 2))
  
  mtext(rev(l), side = 2, at = 1:8, line = 1)
  mtext(1:12, side = 3, at = 1:12, line = 1)
  mtext(run_info[grep("Time of measurment", run_info)], side=1, line=1)
  #dev.off()
  
  print(run_info)
  return(list(
    "run_info"= run_info,
    "results"= res
  ))
}
