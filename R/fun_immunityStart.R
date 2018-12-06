#' calculate foi before vaccination starts for R0 model
#'
#' @param adm admin indices
#' @param R0 R0 in admins
#' @param pop_moments population moments
#' @param polydeg degree of polynomial. Defaults to 6
#' @return The force of infection before vaccination in the admin or admins specified
#' @export

foi_prevac = function(adm,
                      R0,
                      pop_moments,
                      polydeg = 6) {

  lambda = NULL
  if (polydeg > 0) {
    for (deg in 1:polydeg) {
      if (!anyNA(adm)) {
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
  if (!anyNA(adm)){
    names(out) = paste("FOI", pop_moments[1], sep = "_")
  }  #NAME IS ATTACHED TO POP_MOMENTS FOR WHOLE
  return(out)
}





#' calculate immunity profile at start
#'
#' @param model_type R0 or Foi. defaults to Foi
#' @param transmission_param transmission intensity in country
#' @param age_max maximum age gorup
#' @param pop population in country by year and age
#' @param old_coverage vaccination coverage by year and age
#'
#' @return immunity profile by age in the first year of interest
#' @export

fun_immunityStart = function(model_type = "Foi",
                             transmission_param,
                             age_max,
                             pop,
                             old_coverage) {

    ages = c(0:age_max)
    year_start = 1939 #before vaccination started

    # for R0 need the force of infection before vaccination
    if (model_type == "R0") {

        R0 = transmission_param

        ### population moments
        pop_start = pop[pop[, 1] == year_start, ]
        pop_prop = pop_start[2:length(pop_start)]/
                          sum(pop_start[2:length(pop_start)], na.rm = TRUE)

        pop_mom = rep(NA, 6)
        for(i in 1:6){
          pop_mom[i] = sum(pop_prop * ages^(i-1), na.rm =TRUE)
        }

        foi = foi_prevac(adm = NA, R0 = R0, pop_moments = pop_mom, polydeg = 6)


    }

    if (model_type == "Foi") {
        foi = transmission_param
    }

    immunityStart = 1 - (exp(-foi * ages))

    return(immunityStart)
}
