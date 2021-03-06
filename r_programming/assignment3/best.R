## best.R
##

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character", stringsAsFactors=FALSE,
                         na.strings=c("Not Available"))
        
        fdata <- factor(data[, 7])
        validstates <- levels(fdata)
        validoutcomes <- c("heart attack", "heart failure", "pneumonia")
        index <- NULL
        
        
        ## Test validity of state input
        if (sum(state == validstates) != 1) {
                stop("invalid state")
        } 
        else {
                statedata <- data[data$State == state, ]
        }
        
        ## Test validity of outcome input
        if (sum(outcome == validoutcomes) != 1) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the lowest
        ## 30-day death rate        
        
        if (outcome == "heart attack") {
                statedata[, 11] <- as.numeric(statedata[, 11])
                index <- order(statedata[[11]], statedata[[2]])
        }
        
        if (outcome == "heart failure") {
                statedata[, 17] <- as.numeric(statedata[, 17])
                index <- order(statedata[[17]], statedata[[2]])
        }
        
        if (outcome == "pneumonia") {
                statedata[, 23] <- as.numeric(statedata[, 23])
                index <- order(statedata[[23]], statedata[[2]])
        }
        
        hospital <- statedata$Hospital.Name[index[1]]
        hospital
                
}