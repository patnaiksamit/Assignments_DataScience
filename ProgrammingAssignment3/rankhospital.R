rankhospital <- function(state, outcome, rank = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Read the hospitalname,state ,heart attack, heart failure and pneumonia data and form a data frame
  Hospital_Rank_Data <-  as.data.frame(cbind(data[, 2],   # hospital
                                                  data[, 7],   # state
                                                  data[, 11],  # heart attack
                                                  data[, 17],  # heart failure
                                                  data[, 23]), # pneumonia
                                            stringsAsFactors = FALSE)
  #Assign Column names
  colnames(Hospital_Rank_Data)<- c("Hospital Name", "state", "heart attack", "heart failure", "pneumonia")
    if(!state %in% Hospital_Rank_Data[,"state"]){ #Check if state name is valid
    stop('Invalide State')
  } else if (!outcome%in% c("heart attack", "heart failure", "pneumonia")){  
    stop('Invalid Outcome')  #Check if outcome passed is valid
  } else if (is.numeric(rank)){ #Check if rank is numeric
    
    state1 <- which(Hospital_Rank_Data[, "state"] == state) #returns a vector of indices indicating 
                                                            #all rows with the states 
    data_State1 <- Hospital_Rank_Data[state1, ]     #extracting all the row data for the  state
    data_State1[,eval(outcome)] <- as.numeric(data_State1[, eval(outcome)]) #extract the outcome data for the state
    #Order by outcome data and hospital name on the data extracted
    data_State1 <- data_State1[order(data_State1[,eval(outcome)],data_State1[,"Hospital Name"]),]
    output  <- data_State1[,"Hospital Name"][rank]
  }else if(!is.numeric(rank)){
    if(rank=="best"){
      output <- best(state,outcome)
    }else if (rank=="worst"){
      state1 <- which(Hospital_Rank_Data[, "state"] == state) #returns a vector of indices indicating 
      #all rows with the states 
      data_State1 <- Hospital_Rank_Data[state1, ]     #extracting all the row data for the  state
      data_State1[,eval(outcome)] <- as.numeric(data_State1[, eval(outcome)]) #extract the outcome data for the state
      #Order by outcome data and hospital name on the data extracted
      data_State1 <- data_State1[order(data_State1[,eval(outcome)],data_State1[,"Hospital Name"],decreasing = TRUE),]
      output  <- data_State1[,"Hospital Name"][1]
    }
  }
  else{
    stop('Invalid Rank')
  }
  return(output)
}
