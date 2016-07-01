rankall <- function (outcome, num="best"){
  ## The function reads OUTCOME-OF-CARE-MESURES.CSV file
  ## and returns a 2-column data frame containing the hospital
  ## in each state that has the ranking specigied in num.
  ## num can accept "best", "worst and integer  
  ## outcome - 30-day mortalityfor the 
  ## specified outcome in than state
  
  ## 1. Before validating parameters, initialize, create reference data
  
  ocolid       <- as.numeric()
  st_list      <- sort(state.abb,decreasing=FALSE,na.last=NA)
  num_list     <- c("best","worst")
  outcome_list <- c("heart attack","heart failure","pneumonia")
  oframe       <- data.frame(outcome=c("heart attack","heart failure","pneumonia"),
                             colid=c(11,17,23))
  out_dframe   <- data.frame(hospital= character(), state=character())
  
  ## 2. Validate parameters
  
  if(missing(outcome) | !is.element(outcome,outcome_list))
    stop("invalid outcome")
  if(is.character(num) & !is.element(num,num_list))
    stop("invalid num")
 
  ## read CSV file first
  temp_ds <- read.csv("week4/outcome-of-care-measures.csv", colClasses="character")
  
  ## rename column 11, or 17, or 23 to  outcome
  ## to  simplify coding
  for(i in 1:nrow(oframe)){
    if(oframe$outcome[i] == outcome) {
      ocolid  <- oframe$colid[i]
      names(temp_ds)[ocolid] <- "outcome"
    }
  }
 
  ## 3. Main loop - match, validate and extract/subset data 
  
  ## match state, subset state data, sort, pick up by rank
  ## add row to out_dframe
  for (i in 1:length(st_list)) {
      ext_ds  <- subset(temp_ds,State==st_list[i]
                        & !is.na(outcome) 
                        & outcome != "Not Available"
                        ,select=c("State","Hospital.Name","outcome")
      )
      
      ## sort data frame by outcome & Hospital.Name
      ext_ds_srtd <- ext_ds[order(as.numeric(ext_ds[["outcome"]])
                                  ,ext_ds[["Hospital.Name"]]
                                  ,decreasing=FALSE,na.last=NA)
                            , ]
      ## add sequence number to rows (new RANK column created)
      ext_ds_srtd$rank <- seq.int(nrow(ext_ds_srtd))
      
      ## 4. Return hospital name with LOWEST/HIGHEST outcome value 

      if(num =="best")    ix <- which.min(ext_ds_srtd$rank)
      if(num =="worst")   ix <- which.max(ext_ds_srtd$rank)
      if(is.numeric(num)) ix <- as.numeric(num)
      
      h_value <- as.character(ext_ds_srtd[ix, "Hospital.Name"])
      s_value <- as.character(ext_ds_srtd[ix, "State"]) 
      out_dframe <- rbind(out_dframe, data.frame(hospital=h_value, state=st_list[i]))
  } ## end of state loop
  out_dframe
}
