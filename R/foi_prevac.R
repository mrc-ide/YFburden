#' calculate foi before vaccination starts for R0 model
#' 
#' @param adm admin indeices
#' @param R0 R0 in admins
#' @param pop_moments population moments
#' @param polydeg degree of polynomial. Defaults to 6
#' @return The force of infection before vaccination in the admin or admins specified
#' foi_prevac()

foi_prevac = function(adm, R0, pop_moments, polydeg = 6) {
    # returns the foi of the pre_vacc period
    
    
    lambda = NULL
    if (polydeg > 0) {
        for (deg in 1:polydeg) {
            if (is.na(max(adm)) == 0) {
                PM = as.numeric(pop_moments[, deg + 1])
            } else {
                PM = pop_moments[deg]
            }  #INDEX IS DIFFERENT AS INCLUDES NAMES
            lambda = cbind(lambda, (-1)^(deg + 1) * PM/factorial(deg - 1))  # 
        }
    }
    lambda[, 1] = lambda[, 1] - 1/R0  # we have to put the equation =0, so the term of order 0 (first column) should integrate -1/R0
    
    out = sapply(1:nrow(lambda), function(j) polyroot(lambda[j, ]))
    
    out[abs(Arg(out)) <= 1e-10] = Re(out)[abs(Arg(out)) <= 1e-10]
    out[abs(Arg(out)) > 1e-10] = NA  # here we have a resolution problem : in case of survey=5 and R0=5 (for instance), we have no solution in Real
    dt = dim(out)
    out = as.numeric(out)
    dim(out) = dt
    if (polydeg > 2) {
        out = apply(out, 2, min, na.rm = T)
    }
    if (is.na(max(adm)) == 0) 
        {
            names(out) = paste("FOI", pop_moments[1], sep = "_")
        }  #NAME IS ATTACHED TO POP_MOMENTS FOR WHOLE
    return(out)
}
