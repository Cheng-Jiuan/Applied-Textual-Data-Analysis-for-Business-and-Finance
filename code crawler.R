
# Iterate over all quarters
for(quarter in 1:4){
  
  # define web.url
  web.url <- paste(
    "https://www.sec.gov/Archives/edgar/full-index/", 
    2008, 
    "/QTR", quarter, 
    "/master.idx", sep ="")
  
  # Download
  download.file(web.url, 
                destfile = paste0("temp directory/EdgarIndexFileYear", y, "QTR", quarter, ".txt"), 
                headers = c("User-Agent"="YOUR_MAIL_ADRESSE@nhh.no"))
  
  # Read
  edgar.index <- read_delim(paste0("temp directory/EdgarIndexFileYear", y, "QTR", quarter, ".txt"),
                            col_names = F,
                            delim = "|",
                            skip = 11) 
  colnames(edgar.index) <- strsplit(readLines(paste0("temp directory/EdgarIndexFileYear", y, "QTR", q, ".txt"), n = 10)[10], "\\|")[[1]]
  
  # Limit
  edgar.index %>% 
    filter(CIK == "320193", 
           `Form Type` == "10-Q") -> edgar.index
  
  # Make sure that the crawler iterates further if there is no filing
  if(nrow(edgar.index) == 0){next} 
  
  # Loop over all identified filings
  for(i in 1:nrow(edgar.index)){
    # download
    download.file(paste0( "https://www.sec.gov/Archives/", edgar.index$Filename[i]), 
                  destfile = paste0("temp directory/10Q_" , edgar.index$`Date Filed`[i],".txt"), 
                  headers = c("User-Agent"="YOUR_MAIL_ADRESSE@nhh.no"))
  }
}
