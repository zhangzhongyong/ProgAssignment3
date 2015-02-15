best<- function(state,outcome){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!(state %in% data$State)){
    stop("invalid state")
  }
  
  if (outcome=="heart attack"){
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  else if(outcome=="heart failure"){
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
  }
  else if(outcome=="pneumonia"){
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  }
  else {
    stop("invalid outcome")
  }
  data<-split(data,data$State)
  data_state<-data[[state]]
  #print(str((data_state[outcome])))
  value_vector<-as.numeric(data_state[[outcome]])
  index<-which(value_vector==min(value_vector,na.rm=T))
  hosp_v<-sort(data_state[["Hospital.Name"]][index])
  #print(hosp_v)
  hosp_v[1]  
}

  
  