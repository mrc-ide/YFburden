#' move on immunity profile through ageing
#' 
#' @param immunity current immunity profile
#' @return New immunity profile by age
#' update_immunity()

update_immunity = function(immunity) {
    ## takes the proportion of immune people in each age class should be a vector, of length n.ages.
    
    immunity_next = c(0, immunity[-length(immunity)])
    names(immunity_next) = names(immunity)
    
    return(immunity_next)
}
