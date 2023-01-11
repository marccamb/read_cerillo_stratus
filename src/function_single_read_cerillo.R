# Function to read a single measurement from the Cerillo plate reader
library(tidyr)

read.cerillo.single <- function(file, nb_wells=96, blank_wells=NULL, 
                                plot=T, print_runinfo=T) {
  line_nb <- system(paste("wc -l", file), intern=T) %>%
    gsub(pattern = " .*", replacement = "")
  if(line_nb != "7") stop("The file does not contain the right number of lines
                          Check that it is a single measurement file.")
  if(!nb_wells %in% c(12,96)) stop("Only 96- and 12-wells plates are available for now.")  
  
  run_info <- system(paste("head -n 5", file),intern = T) %>%
    gsub(pattern = ",\r", replacement = "") %>%
    gsub(pattern = ",", replacement = " ")
  
  
  d <- system(paste("tail -n 2", file), intern = T) %>%
    strsplit(",")
  res <- data.frame(
    "well"=d[[1]][-c(1,2,99)],
    "raw_OD"=as.numeric(d[[2]][-c(1,2,99)])
  )
  
  run_info <- c(run_info, 
                paste(d[[1]][1], d[[2]][1]), # stores the UNIX Time stamp
                paste("Time of measurment:", as.POSIXct(as.numeric(d[[2]][1]), origin="1970-01-01")),
                paste(d[[1]][2], d[[2]][2])) # stores the temperature 
  
  if(nb_wells==96) {
    well_order <- paste0(LETTERS[1:8], rep(1:12,each=8))
    names(well_order) <- well_order
  }
  if(nb_wells==12) well_order <- 
    c("A1"="B2",	"A2"="B5",	"A3"="B8",	"A4"="B11",
      "B1"="E2", "B2"="E5",	"B3"="E8",	"B4"="E11",
      "C1"="H2",	"C2"="H5",	"C3"="H8",	"C4"="H11")[paste0(LETTERS[1:3], rep(1:4,each=3))]
  # if(nb_wells==12) well_order <- 
  #   c("A1"="A2",	"A2"="A5",	"A3"="A8",	"A4"="A11",
  #     "B1"="D2", "B2"="D5",	"B3"="D8",	"B4"="D11",
  #     "C1"="G2",	"C2"="G5",	"C3"="G8",	"C4"="G11")[paste0(LETTERS[1:3], rep(1:4,each=3))]
  
  res <- res[match(well_order, res$well),]
  res$well <- names(well_order)
  
  if(!is.null(blank_wells)) {
    mean_OD_blank <- 
      blank_wells %>%
      match(res$well) %>%
      res[.,"raw_OD"] %>%
      mean
    
    res$adjusted_OD <-
      res$raw_OD - mean_OD_blank 
  }
  
  cuts <- seq(-0.1,1.5,length.out=9)
  od_palette <- RColorBrewer::brewer.pal(9,"YlOrBr")
  names(od_palette) <- cut(seq(0,1.5,length.out=9),breaks = cuts) 
  
  res$col <- cut(res$raw_OD, breaks=cuts) %>%
    as.numeric %>%
    od_palette[.]
  
  l <- unique(substr(res$well, 1,1))
  if(nb_wells==12) {
    x <- rep(1:4, each=3)
    y <- rep(3:1, 4)
  } else {
    x <- rep(1:12, each=8)
    y <- rep(8:1, 12)
  }
  
  
  if(plot) {
    par(bty="n", xpd=T, las=1, mar=c(2,2,2,2),fg="gray30", col.axis="gray30")
    plot(x, y, axes=F,
         xlab="", ylab="",
         pch=21, cex=ifelse(nb_wells==96,5,7), col="darkgray", bg=res$col)
    text(x, y, 
         round(res$raw_OD, digits = 2))
    
    mtext(rev(l), side = 2, at = 1:ifelse(nb_wells==96,8,3), line = 1)
    mtext(1:ifelse(nb_wells==96,12,4), side = 3, at = 1:ifelse(nb_wells==96,12,4), line = 1)
    mtext(run_info[grep("Time of measurment", run_info)], side=1, line=1)
  }
  
  if(print_runinfo) print(run_info)
  return(list(
    "run_info"= run_info,
    "results"= res
  ))
}
