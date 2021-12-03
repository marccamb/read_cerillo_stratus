# Function to read a continuous measurements from the Cerillo plate reader
read.cerillo.continuous <- function(platemap, file) {
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
    .[!is.na(platemap$sample_id) & platemap$sample_id != ""]
  d <- d[,c(1,2,match(wells,colnames(d)))]
  d <- apply(d,2,as.numeric) %>%
    data.frame
  
  # this step is specific to my way of naming cultures. 
  # I need to change it to make it more generic
  
  # OD <- platemap$sample_id %>% gsub("_.$", "", .) %>% unique %>%
  #   purrr::set_names() %>%
  #   purrr::map(function(x) {
  #     w <- platemap$well[grep(x, platemap$sample_id)]
  #     print(w)
  #     list(
  #       "mean" = apply(d[,colnames(d) %in% w], 1, mean),
  #       "sd" = apply(d[,colnames(d) %in% w], 1, sd)
  #     )
  #   })
  time <- as.POSIXct(d$UNIX.Timestamp, origin="1970-01-01") - as.POSIXct(d$UNIX.Timestamp, origin="1970-01-01")[1]
  time <- time/3600
  d$hpi <- time  
  return(d)
}
