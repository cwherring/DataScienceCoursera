rankall <- function(myoutcome, num = "best") { 
    source("prog3-common.R")
    
    ## Read outcome data
    outcome <<- getOutcomeData("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    if (!validOutcome(myoutcome)) stop("invalid outcome")


    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the ## (abbreviated) state name
    states <<- unique(unlist(outcome[,7], use.names = FALSE))
    states <<- states[order(states)]8
    N=length(states)
    df = NULL
    df = data.frame(hospital=rep("",N), state=rep("",N), stringsAsFactors=FALSE)
    i = 0
    if (myoutcome == "heart attack") {
        for(state in states){
            i = i + 1
            df[i,1] <- rankHA(state, num)[1]
            df[i,2] <- state
        }
    }else if (myoutcome == "heart failure") {
        for(state in states){
            i = i + 1
            df[i,1] <- rankHF(state, num)[1]
            df[i,2] <- state
        }
    }else if (myoutcome == "pneumonia") {
        for(state in states){
            i = i + 1
            df[i,1] <- rankPN(state, num)[1]
            df[i,2] <- state
        }
    }else stop("invalid outcome")
    df <- df[order(df[,2]),]
    df
}
