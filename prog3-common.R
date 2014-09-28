

rankHA <- function(state, rank) {
    rankoutcome <- subset(outcome, outcome$State == state, select=c(2,7,11))
    rank.s <- rankoutcome[order(rankoutcome[,3], rankoutcome[,1]),]
    rank.s <- rank.s[complete.cases(rank.s),]
    names(rank.s) <- c("Hospital.Name", "state", "rate")
    
    if(rank=="worst"){
        as.data.frame(tail(rank.s, n=1)[,])
    } else if (rank=="best") {
        as.data.frame(rank.s[1,])
    } else {
        as.data.frame(rank.s[rank,])
    }
}

rankHF <- function(state, rank) {
    rankoutcome <- subset(outcome, outcome$State == state, select=c(2,7,17))
    rank.s <- rankoutcome[order(rankoutcome[,3], rankoutcome[,1]),]
    rank.s <- rank.s[complete.cases(rank.s),]
    names(rank.s) <- c("Hospital.Name", "state", "rate")
    
    if(rank=="worst"){
        as.data.frame(tail(rank.s, n=1)[,])
    } else if (rank=="best") {
        as.data.frame(rank.s[1,])
    } else {
        as.data.frame(rank.s[rank,])
    }
}

rankPN <- function(state, rank) {
    rankoutcome <- subset(outcome, outcome$State == state, select=c(2,7,23))
    rank.s <- rankoutcome[order(rankoutcome[,3], rankoutcome[,1]),]
    rank.s <- rank.s[complete.cases(rank.s),]
    names(rank.s) <- c("Hospital.Name", "state", "rate")

    if(rank=="worst"){
        as.data.frame(tail(rank.s, n=1)[,])
    } else if (rank=="best") {
        as.data.frame(rank.s[1,])
    } else {
        as.data.frame(rank.s[rank,])
    }
}

bestHA <- function(state) {
    bestoutcome <-  subset(outcome, outcome$State == state)
    as.character(subset(bestoutcome, bestoutcome[,11]==min(bestoutcome[,11], na.rm=TRUE), select=2))
}

bestHF <- function(state) {
    bestoutcome <-  subset(outcome, outcome$State == state)
    as.character(subset(bestoutcome, bestoutcome[,17]==min(bestoutcome[,17], na.rm=TRUE), select=2), all.names=FALSE)
    
}

bestPN <- function(state) {
    bestoutcome <-  subset(outcome, outcome$State == state)
    as.character(subset(bestoutcome, bestoutcome[,23]==min(bestoutcome[,23], na.rm=TRUE), select=2))
    
}

validState <- function(state) {
    states <<- unique(unlist(outcome[,7], use.names = FALSE))
    states <<- states[order(states)]
    if (state %in% states) TRUE
    else FALSE
}

validOutcome <- function(myoutcome) {
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (myoutcome %in% outcomes) TRUE
    else FALSE
}

getOutcomeData <- function(filepath) {
    outcome <- read.csv(filepath, colClasses = "character")
    outcome[,11] <- as.numeric(outcome[,11]) #heart attack
    outcome[,17] <- as.numeric(outcome[,17]) #heart failure
    outcome[,23] <- as.numeric(outcome[,23]) #pneumonia
    outcome
}