## best.R
##

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
        fdata <- factor(data[, 7])
        validstates <- levels(fdata)
        validoutcomes <- c("heart attack", "heart failure", "pneumonia")
        
        
        ## Test validity of state input
        if (sum(state == validstates) != 1) {
                stop("invalid state")
        }
        
        ## Test validity of outcome input
        if (sum(outcome == validoutcomes) != 1) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the lowest
        ## 30-day death rate
        
        statesplit <- split(data, fdata)
        statefilter <- statesplit$state
        if (outcome == "heart attack") {
                x <- order(as.numeric(statefilter[, 11]), statefilter[, 2])
                x[1, c(2, 11)]                
        }
        databystate$state[]
        
}