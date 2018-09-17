#' calculate the number of infections in a year for all age groups in R0 model
#' 
#' @param R0 R0 in country
#' @param pop population by age in one year
#' @param immunity current immunity profile by age in one year
#' @return The number of new infections in one year and the immunity profile by age in one year
#' generate_infections_R0()

generate_infections_R0 = function(R0, pop, immunity) {
    
    
    
    herd_immunity = 1 - (1/R0)
    sus = pop * (1 - immunity)
    prop_sus = sum(sus)/sum(pop)
    prop_to_infect = herd_immunity - (1 - prop_sus)  ## proportion of the population
    if (prop_to_infect < 0) {
        ## don't do any infections.
        new_infect = rep(0, length(pop))
        names(new_infect) = names(pop)
        new_immunity = immunity
    } else {
        ## put in infections as appropriate, by newly infecting prop_to_infect of each susceptible age category:
        prop_sus_to_infect = prop_to_infect/prop_sus
        new_infect = sus * prop_sus_to_infect
        new_immunity = (pop - sus + new_infect)/pop
        new_immunity[is.na(new_immunity)] = 0
        
        if (abs(sum(new_immunity * pop)/sum(pop) - herd_immunity) > .Machine$double.eps * 2) {
            stop("generate_infections_R0: new immunity different from herd immunity.\n")
        }
    }
    
    return(list(new_infections = new_infect, immunity = new_immunity))
}
