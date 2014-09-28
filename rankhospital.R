rankhospital <- function(state, myoutcome, num = "best") { 
    source("prog3-common.R")
    
    ## Read outcome data
    outcomes <<- getOutcomeData("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    if (!validState(state)) stop("invalid state")
    if (!validOutcome(myoutcome)) stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank ## 30-day death rate
    if (myoutcome == "heart attack") {
        rankHA(state, num)
    }else if (myoutcome == "heart failure") {
        rankHF(state, num)
    }else if (myoutcome == "pneumonia") {
        rankPN(state, num)
    }else stop("invalid outcome")
    
}