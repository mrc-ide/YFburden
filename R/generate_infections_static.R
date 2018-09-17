#' calculate the number of infections in a year for all age groups in Foi model
#' 
#' @param foi Foi in country
#' @param pop population by age in one year
#' @param immunity current immunity profile by age in one year
#' @return The immunity profile by age in one year and the number of infections for that year
#' generate_infections_static()


generate_infections_static = function(foi, pop, immunity) {
    ## foi is a scalar
    
    ## pop and immunity are vectors of length age.max+1, hopefully with ages as names, with pop giving absolute population size, and immunity the proportion immune
    ## in each age category.
    
    ## returns vectors new.infections and immunity of the same format as pop and immunity.
    
    
    sus = pop * (1 - immunity)
    
    prop_sus_to_infect = foi
    new_infect = sus * prop_sus_to_infect
    
    new_immunity = (pop - sus + new_infect)/pop
    
    new_immunity[is.na(new_immunity)] = 0
    
    rownames(new_immunity) = rownames(immunity)
    colnames(new_immunity) = colnames(immunity)
    
    return(list(new_infections = new_infect, immunity = new_immunity))
}
