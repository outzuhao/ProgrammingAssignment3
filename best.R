best <- function(state, outcome) {
    raw_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- subset(raw_data, select = c(Hospital.Name, State, 
                                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    colnames(data) <- c("hospital", "State", "heart attack", 
                        "heart failure", "pneumonia")
    if (!(is.element(state, data$State))) {
        stop("invalid state")
    } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    sel_data <- subset(data, State == state, c("hospital", outcome))
    sel_data_comp <- sel_data[(sel_data[, 2] != "Not Available"), ]
    min_idx <- which.min(sel_data_comp[, 2])
    tie <- numeric()
    if (length(min_idx) == 1) {
        sel_data_comp[min_idx, 1]    
    } else {
        tie <- sel_data_comp[min_idx, 1]
        sort(tie)
        tie[1]
    }
}