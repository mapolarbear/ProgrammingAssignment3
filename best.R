best <- function (state, outcome){
  ## The function reads OUTCOME-OF-CARE-MESURES.CSV file
  ## and returns a character vector with the name of hospital
  ## that has the best (lowest) 30-day mortalityfor the 
  ## specified outcome in than state
  
  ## 1. Initialize, create reference data
  
  ocolid       <- as.numeric()
  st_list      <- state.abb
  outcome_list <- c("heart attack","heart failure","pneumonia")
  oframe       <- data.frame(outcome=c("heart attack","heart failure","pneumonia"),
                             colid=c(11,17,23))
  ## 2. Validate parameters
  
  if(missing(state) | !is.element(state,st_list) )
    stop("invalid state")
  if(missing(outcome) | !is.element(outcome,outcome_list))
    stop("invalid outcome")
  
  ## 3. Main loop - read, validate and extract data
  
  for (i in 1:nrow(oframe)){
    if(oframe$outcome[i] == outcome) {
      ocolid  <- oframe$colid[i]
      temp_ds <- read.csv("week4/outcome-of-care-measures.csv", colClasses="character")
      
      ## apply criteria
      ext_ds  <- temp_ds[which(
                               temp_ds$State == state
                               & !is.na(temp_ds[,ocolid]) 
                               & temp_ds[,ocolid] != "Not Available"
                               ),]
      
      ## rename 30-day mortality column
      names(ext_ds)[ocolid] <- "outcome"
    }
  }
  
  ## 4. Return hospital name with LOWEST outcome value 
  sub <- which.min(ext_ds$outcome)
  ext_ds[sub,"Hospital.Name"]
}
  