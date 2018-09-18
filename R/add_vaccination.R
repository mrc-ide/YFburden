#' add vaccination coverage to change immunity profile
#'
#' @param coverage vaccination campaign coverage
#' @param age_first youngest age group to be vaccinated
#' @param age_last oldest age group to be vaccinated
#' @param immunity current immunity in one year
#' @param skew skew of vaccination. Defaults to 0 where vaccination is random
#' @return The immunity profile by age in one year
#' add_vaccination()

add_vaccination = function(coverage, age_first, age_last, immunity, skew = 0) {
    ## year is the year of vaccination of the new birth cohort.
    if (is.na(skew)) {
        skew = 0
    }
    coverage = pmin(coverage, 1)  #check that it is at most 1

    ## immunity is the age distribution of immunity at the time point in question.  this adds vaccination as if the skew = 0
    if (skew == 0) {
        if (age_first != age_last) {
            immunity[paste0(age_first):paste0(age_last)] = 1 - (1 - coverage) * (1 - immunity[paste0(age_first):paste0(age_last)])
        } else {
            immunity[paste0(age_first)] = 1 - (1 - coverage) * (1 - immunity[paste0(age_first)])
        }
    } else if (skew == -1) {
        if (age_first != age_last) {
            immunity[paste0(age_first):paste0(age_last)] = pmin(1, coverage + immunity[paste0(age_first):paste0(age_last)])
        } else {
            immunity[paste0(age_first)] = pmin(1, coverage + immunity[paste0(age_first)])
        }
    }
    return(immunity)
}
