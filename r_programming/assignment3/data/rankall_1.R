rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character", stringsAsFactors=FALSE,
                         na.strings=c("Not Available"))
        
        fdata <- factor(data[, 7])
        validstates <- levels(fdata)
        validoutcomes <- c("heart attack", "heart failure", "pneumonia")
        
        
        
        ## Test validity of outcome input
        if (sum(outcome == validoutcomes) != 1) {
                stop("invalid outcome")
        }
        
        ## Returns hospital name in that state with the lowest
        ## 30-day death rate by creating an index of ranks.       
        
        if (outcome == "heart attack") {
                data[, 11] <- as.numeric(data[, 11])
        }
        
        if (outcome == "heart failure") {
                data[, 17] <- as.numeric(data[, 17])
        }
        
        if (outcome == "pneumonia") {
                data[, 23] <- as.numeric(data[, 23])
        }
        
        z <- split(data, data[, 7])
        
        
        if (num == "best") {
                num <- as.integer(1)
                x <- sapply(z, function(x) {
                        x[order(x[11], x[2], na.last = NA), 2][num]
                })
                y <- sapply(z, function(y) {
                        y[order(y[11], y[2], na.last = NA), 7][num]
                })

        } else if (num == "worst") {
                x <- sapply(z, function(x) {
                        x[order(x[11], x[2], na.last = NA,
                                decreasing = TRUE), 2][1]
                })
                y <- sapply(z, function(y) {
                        y[order(y[11], y[2], na.last = NA,
                                decreasing = TRUE), 7][1]
                })
        } else  x <- sapply(z, function(x) {
                x[order(x[11], x[2], na.last = NA), 2][num]
        })
                y <- sapply(z, function(y) {
                        y[order(y[11], y[2], na.last = NA), 7][num]
                })
        
result <- data.frame("hospital" = x, "state" = y)
        
             
        
}