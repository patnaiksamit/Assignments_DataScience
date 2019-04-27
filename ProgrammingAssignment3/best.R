best <- function(state,outcome){
  #This function will determine the best hospital in a state
  #Read the data from the csv file into 
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Read the hospitalname , state ,heart attack, heart failure and pneumonia data and form a data frame
  Hospital_Mortality_Data <-  as.data.frame(cbind(data[, 2],   # hospital
                                                  data[, 7],   # state
                                                  data[, 11],  # heart attack
                                                  data[, 17],  # heart failure
                                                  data[, 23]), # pneumonia
                                            stringsAsFactors = FALSE)
  #Assign Column names
  colnames(Hospital_Mortality_Data)<- c("Hospital Name", "state", "heart attack", "heart failure", "pneumonia")
  #check the validity of state name and outcome
  if(!state %in% Hospital_Mortality_Data[,"state"]){
    stop('Invalide State')
  } else if (!outcome%in% c("heart attack", "heart failure", "pneumonia")){
    stop('Invalid Outcome')
  } else {
    state1 <- which(Hospital_Mortality_Data[, "state"] == state) #returns a vector of numbers indicating all rows with the states 
    data_State1 <- Hospital_Mortality_Data[state1, ]     #extracting all the rows data for the  state
    outcome1 <- as.numeric(data_State1[, eval(outcome)]) #extract the outcome data for the state
    min_val <- min(outcome1, na.rm = TRUE)
    result  <- data_State1[, "Hospital Name"][which(outcome1 == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}