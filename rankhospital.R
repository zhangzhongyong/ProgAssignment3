rankhospital<-function(state,outcome,rank="best"){
  
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
  value_vector<-as.numeric(data_state[[outcome]])
  index1<-(!is.na(value_vector))
  index<-order(value_vector,data_state[["Hospital.Name"]])
  
  index1<-index1[index]
  name_vector<-data_state[["Hospital.Name"]][index]
  name_vector<-name_vector[index1]
  #print(name_vector)
  if(rank=="worst"){
    return(name_vector[length(name_vector)])
  }
  else if(rank=="best"){
    return(name_vector[1])
  }
  else if(rank>length(index)){
    return("NA")
  } 
  else if(rank<=length(index)){
    return(name_vector[rank])
  }
  else{
    stop("invalid rank")
  }
}
