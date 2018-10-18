#' Calculate the number of infections
#'
#' @param param_samples transmission intensity in each admin 1 unit or country
#' @param coverage_df vaccination coverage
#' @param pop_all population in all countries by year and age
#' @param years years of interest
#' @param age_max maximum age in output so age range is [0, age_max]
#' @param model_type whether "Foi" or "R0". Defaults to "Foi".
#'
#' @return The number of infections in each country in each year of interest AND
#'         cohort size in each country in each year of interest
#' @export

calc_infections = function(param_samples,
                           coverage_df,
                           pop_all,
                           years,
                           age_max,
                           model_type = "Foi"){

  #get the number of countries
  countries = unique(pop_all$country_code)
  n_countries = length(countries)
  ages = 0:age_max

  #declare outputs
  infections = rep(NA, length(years)*length(ages)*n_countries)
  dim(infections) = c(length(years), length(ages), n_countries)

  cohort_size = rep(NA, length(years)*length(ages)*n_countries)
  dim(cohort_size) = c( length(years), length(ages), n_countries)

  for (country_ind in 1 : n_countries){

    #get coverage for that country
    coverage_country = dplyr::filter(coverage_df, country_code == countries[country_ind])

    #population for that country
    pop_country = dplyr::filter(pop_all, country_code == countries[country_ind])

    ### want to reshape into previous format ###
    pop_new = NULL
    for (y in max(pop_country$year) : min(pop_country$year)){
      tmp = c(y, dplyr::filter(pop_country, year == y)$value)

      if(length(tmp)< 102) {tmp = c(tmp, rep(NA, 102-length(tmp)))}
      pop_new = rbind(pop_new, tmp)
    }
    colnames(pop_new) = c("year", ages)
    pop_new = pop_new[order(pop_new[,1]),]
    ############################################


    #collect transmission param for that country
    param_country = param_samples[ grep(countries[country_ind], names(param_samples)) ]

    #average over country
    param_country_ave = mean( as.numeric(param_country) )

    #calculate start conditions
    immunity_start = fun_immunityStart(model_type,
                                       transmission_param = param_country_ave,
                                       age_max = age_max,
                                       pop = pop_new,
                                       old_coverage = coverage_country,
                                       years[1])

    #calculate burden in year of interest
    out = run_infections_unit(model_type,
                              transmission_param = param_country_ave,
                              years = years,
                              age_max = age_max,
                              pop = pop_new,
                              coverage = coverage_country,
                              immunityStart = immunity_start)


    infections[ , , country_ind] = out$new_infections

    cohort_size[ , , country_ind] = pop_new[ which( pop_new[,1] %in% years), -1]

  }

  dimnames(infections)[[1]] = dimnames(cohort_size)[[1]] = as.list(years)
  dimnames(infections)[[2]] = dimnames(cohort_size)[[2]] = as.list(ages)
  dimnames(infections)[[3]] = dimnames(cohort_size)[[3]] = as.list(countries)


  return(list(infections = infections, cohort_size = cohort_size))

}
