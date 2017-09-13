rankhospital <- function(state, outcome, num = "best") {
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
    ord_data <- sel_data_comp[order(as.numeric(sel_data_comp[, 2]), sel_data_comp[, 1]), ]
    if (num == "best") {
        ord_data[1, 1]
    } else if (num == "worst") {
        ord_data[nrow(ord_data), 1]
    } else if (num <= nrow(ord_data)) {
        ord_data[num, 1]
    } else {
        NA
    }
}