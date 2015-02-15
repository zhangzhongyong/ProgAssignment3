rankall<-function(outcome,num="best"){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbre
  
 
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
  
  state_fc<-factor(data$State)
  le<-levels(state_fc)
  data_list<-split(data,data$State)
  h<-character(0)
  for(i in le){
    new_data<-data_list[[i]]
    new_data[[outcome]]<as.numeric(new_data[[outcome]])
    index<-order(new_data[[outcome]],new_data[["Hospital.Name"]])
    index1<-is.na((new_data[[outcome]]))
    index1<-index1[index]
    name<-new_data[["Hospital.Name"]][index]
    #name<-name[!index1]
    if(num=="best"&& length(name)>0){
      h<-append(h,name[1])
    }
    else if (num=="worst" && length(name)>0){
      h<-append(h,name[[length(name)]])
    }
    else if (num<=length(name)){
     h<-append(h,name[num])
    }
    else {
     h<-append(h,"NA")
    }
  }
  #print(length(h))
 
  return(data.frame(hospital=h,state=le,row.names=le))
}