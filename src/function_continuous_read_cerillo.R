# Function to read a continuous measurements from the Cerillo plate reader
library(tidyr)

read.cerillo.continuous <- function(platemap, file, nb_wells=96
                                    ) {
  # Check the file number of lines
  line_nb <- system(paste("wc -l", file), intern=T) %>%
    gsub(pattern = " .*", replacement = "") %>%
    as.numeric
  if(line_nb <= 7) stop("The file does not contain enough lines.
                          Check that it is a continuous measurement file.")
  
  # store run info
  run_info <- system(paste("head -n 5", file),intern = T) %>%
    gsub(pattern = ",\r", replacement = "")
  
  keep_lines <- line_nb - 5
  d <- system(paste("tail -n", keep_lines, file), intern = T) %>%
    strsplit(",") %>%
    do.call(rbind, .)
  colnames(d) <- d[1,]
  d <- d[-1,-99]
  
  run_info <- c(run_info, 
                paste("Time of first measurment:", 
                      as.POSIXct(as.numeric(d[1,1]), origin="1970-01-01"))
  )
  
  # only select non empy wells
  wells <- platemap$well %>%
    .[!is.na(platemap$sample_id) & platemap$sample_id != ""] %>%
    as.character()
  if(nb_wells==12) wells <- c("A1"="A2",	"A2"="A5",	"A3"="A8",	"A4"="A11",
                                   "B1"="D2", "B2"="D5",	"B3"="D8",	"B4"="D11",
                                   "C1"="G2",	"C2"="G5",	"C3"="G8",	"C4"="G11")[wells]
  d <- d[,c(1,2,match(wells,colnames(d)))]
  d <- apply(d,2,as.numeric) %>%
    data.frame
  if(nb_wells==12) names(d)[-c(1,2)] <- names(wells)
  
  time <- as.POSIXct(d$UNIX.Timestamp, origin="1970-01-01") - as.POSIXct(d$UNIX.Timestamp, origin="1970-01-01")[1]
  time <- time/3600
  d <- cbind("hpi"=time, d)
  return(d)
}

# wells <- paste0(LETTERS[1:8], rep(1:12, each=8))
# file <- "data/211123_continuous_read.CSV"
