#' Calculate the number of infections
#'
#' @param param_samples transmission intensity in each admin 1 unit or country
#' @param vac_eff vaccine efficacy, defaults to 1
#' @param coverage_df vaccination coverage
#' @param pop_all population in all countries by year and age
#' @param years years of interest
#' @param age_max maximum age in output so age range is [0, age_max]
#' @param model_type whether "Foi" or "R0". Defaults to "Foi".
#'
#' @return The number of infections in each country in each year of interest AND
#'         cohort size in each country in each year of interest AND immunity profiles.
#' @export

calc_infections = function(param_samples,
                           vac_eff = 1,
                           coverage_df,
                           pop_all,
                           years,
                           age_max,
                           model_type = "Foi"){

  #get the number of countries
  countries = unique(pop_all$country_code)
  n_countries = length(countries)
  n_years = length(years)
  ages = 0:age_max
  n_ages = length(ages)

  #declare outputs
  infections = rep(NA, n_years*n_ages*n_countries)
  dim(infections) = c(n_years, n_ages, n_countries)

  cohort_size = rep(NA, n_years*n_ages*n_countries)
  dim(cohort_size) = c( n_years, n_ages, n_countries)

  immunity_out = rep(NA, n_years*n_ages*n_countries)
  dim(immunity_out) = c( n_years, n_ages, n_countries)

  for (country_ind in 1 : n_countries){

    #get coverage for that country
    coverage_country = coverage_df %>% filter( country_code == countries[country_ind])

    #population for that country
    pop_country = pop_all %>% filter( country_code == countries[country_ind])

    ### want to reshape into previous format ###
    pop_new = NULL
    for (y in max(pop_country$year) : min(pop_country$year)){
      tmp = c(y, filter(pop_country, year == y)$value)

      if(length(tmp)< 102) {tmp = c(tmp, rep(NA, 102-length(tmp)))}
      pop_new = rbind(pop_new, tmp)
    }
    colnames(pop_new) = c("year", ages)
    pop_new = pop_new[order(pop_new[,1]),]

    ############################################

    ##### FOR CAMPAIGN USE DOSES RATHER THAN COVERAGE #####
    # UPDATE: THIS IS ALL SPECIFIED AT NATIONAL LEVEL NOW #

    if (!is.null(length(coverage_country[coverage_country$activity_type == "campaign" &
                                         coverage_country$target > 0, "coverage"]))) {

      coverage_country[coverage_country$activity_type == "campaign" &
                         coverage_country$target > 0 &
                         !is.na(coverage_country$target),
                       "coverage"] =
        as.numeric(coverage_country[coverage_country$activity_type == "campaign" &
                                      coverage_country$target > 0 & !is.na(coverage_country$target),
                                    "target"]) *
        as.numeric(coverage_country[coverage_country$activity_type == "campaign" &
                                      coverage_country$target > 0 & !is.na(coverage_country$target),
                                    "coverage"])/
        rowSums(pop_new[match(coverage_country[coverage_country$activity_type == "campaign" &
                                                 coverage_country$target > 0 & !is.na(coverage_country$target),
                                               "year"], pop_new[,1]), ,
                        drop = FALSE], na.rm = TRUE)

    }

    #collect transmission param for that country
    param_country = param_samples[ grep(countries[country_ind], names(param_samples)) ]

    #average over country
    param_country_ave = mean( as.numeric(param_country) )

    #calculate start conditions before vaccination started in 1939
    immunity_start = fun_immunityStart(model_type = model_type,
                                       transmission_param = param_country_ave,
                                       age_max = age_max,
                                       pop = pop_new,
                                       old_coverage = coverage_country)

    #calculate burden in year of interest
    out = run_infections_unit(model_type = model_type,
                              transmission_param = param_country_ave,
                              vac_eff = vac_eff,
                              years_in = years,
                              age_max = age_max,
                              pop = pop_new,
                              coverage = coverage_country,
                              immunityStart = immunity_start)


    infections[ , , country_ind] = out$new_infections

    cohort_size[ , , country_ind] = pop_new[ which( pop_new[,1] %in% years), -1]

    immunity_out[ , , country_ind] = out$immunity

  }

  dimnames(infections)[[1]] = dimnames(cohort_size)[[1]] = dimnames(immunity_out)[[1]] = as.list(years)
  dimnames(infections)[[2]] = dimnames(cohort_size)[[2]] = dimnames(immunity_out)[[2]] = as.list(ages)
  dimnames(infections)[[3]] = dimnames(cohort_size)[[3]] = dimnames(immunity_out)[[3]] = as.list(countries)


  return(list(infections = infections, cohort_size = cohort_size, immunity = immunity_out))

}
