rankhospital <- function (state, outcome, num){
  ## The function reads OUTCOME-OF-CARE-MESURES.CSV file
  ## and returns a character vector with the name of hospital
  ## that has the best (lowest) or worst (highest) or 
  ## based on hospital ranc (num parameter ) 30-day mortalityfor the 
  ## specified outcome in than state
 
  ## 1. Before validating parameters, initialize, create reference data
  
  ocolid       <- as.numeric()
  st_list      <- state.abb
  num_list     <- c("best","worst")
  outcome_list <- c("heart attack","heart failure","pneumonia")
  oframe       <- data.frame(outcome=c("heart attack","heart failure","pneumonia"),
                             colid=c(11,17,23))
  
  ## 2. Validate parameters
  
  if(missing(state) | !is.element(state,st_list) )
    stop("invalid state")
  if(missing(outcome) | !is.element(outcome,outcome_list))
    stop("invalid outcome")
  if(missing(num)) 
    stop("invalid num")
  if(is.character(num) & !is.element(num,num_list))
    stop("invalid num")
  
  
  ## 3. Main loop - read, validate and extract/subset data
  ## read CSV file first
  temp_ds <- read.csv("week4/outcome-of-care-measures.csv", colClasses="character")
  
  for (i in 1:nrow(oframe)){
    if(oframe$outcome[i] == outcome) {
      ocolid  <- oframe$colid[i]
      ## rename 30-day mortality column to simplify reference
      names(temp_ds)[ocolid] <- "outcome"
      
      
      ## subset (tried another aproach then in best.r) state, apply criteria
      ## select only what I need
      ext_ds  <- subset(temp_ds,State==state
                                & !is.na(outcome) 
                                & outcome != "Not Available"
                        ,select=c("State","Hospital.Name","outcome")
                        )
      
      ## sort data frame by outcome & Hospital.Name
      ext_ds_srtd <- ext_ds[order(as.numeric(ext_ds[["outcome"]])
                                            ,ext_ds[["Hospital.Name"]]
                                            ,decreasing=FALSE,na.last=NA)
                            , ]
      ## add sequence number to rows
      ext_ds_srtd$rank <- seq.int(nrow(ext_ds_srtd))
    }
  }

  
  ## 4. Return hospital name with LOWEST outcome value 
  #if(num =="best")    ix <- 1 
  #if(num =="worst")   ix <- nrow(ext_ds_srtd)
  #if(num =="best")    ix <- which.min(ext_ds_srtd$outcome)
  #if(num =="worst")   ix <- which.max(ext_ds_srtd$outcome)
  if(num =="best")    ix <- which.min(ext_ds_srtd$rank)
  if(num =="worst")   ix <- which.max(ext_ds_srtd$rank)
  if(is.numeric(num)) ix <- as.numeric(num)
  
  ext_ds_srtd[ix, "Hospital.Name"]
}
