## rankhospital.R
##

rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Returns hospital name in that state with the lowest
        ## 30-day death rate by creating an index of ranks.       
        
        if (outcome == "heart attack") {
                statedata[, 11] <- as.numeric(statedata[, 11])
                index <- order(statedata[[11]], statedata[[2]], na.last = NA)
        }
        
        if (outcome == "heart failure") {
                statedata[, 17] <- as.numeric(statedata[, 17])
                index <- order(statedata[[17]], statedata[[2]], na.last = NA)
        }
        
        if (outcome == "pneumonia") {
                statedata[, 23] <- as.numeric(statedata[, 23])
                index <- order(statedata[[23]], statedata[[2]], na.last = NA)
        }
        
        if (num == "best") {
                rank <- as.integer(1)
        } else if (num == "worst") {
                rank <- as.integer(length(index))                
        } else  rank <- as.integer(num)                
        
        hospital <- statedata$Hospital.Name[index[rank]]
        hospital
        
}