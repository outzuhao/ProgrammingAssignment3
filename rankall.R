rankall <- function(outcome, num = "best") {
    raw_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- subset(raw_data, select = c(Hospital.Name, State, 
                                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    colnames(data) <- c("hospital", "state", "heart attack", 
                        "heart failure", "pneumonia")
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    sel_data <- subset(data, select = c("hospital", "state", outcome))
    sel_data_comp <- sel_data[(sel_data[, 3] != "Not Available"), ]
    sel_data_comp[, 3] <- as.numeric(sel_data_comp[, 3])
    ord_data <- sel_data_comp[order(sel_data_comp[, 2], sel_data_comp[, 3], 
                                    sel_data_comp[, 1]), ]
    spl_data <- split(ord_data, ord_data[, 2])
    
    output <- data.frame()
    if (num == "best") {
        for (i in 1:length(spl_data)) {
            temp_df <- spl_data[[i]]
            output <- rbind(output, temp_df[1, c(1, 2)])
        }
    } else if (num == "worst") {
        for (i in 1:length(spl_data)) {
            temp_df <- spl_data[[i]]
            output <- rbind(output, temp_df[nrow(temp_df), c(1, 2)])
        }
    } else {
        for (i in 1:length(spl_data)) {
            temp_df <- spl_data[[i]]
            if (num <= nrow(temp_df)) {
                output <- rbind(output, temp_df[num, c(1, 2)])
            } else {
                output <- rbind(output, data.frame(hospital = NA, state = temp_df[1, 2]))
            }
        }
    }
    output
}