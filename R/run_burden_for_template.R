#' run burden steps and put into a template
#'
#' @param historic_dat = Historic vaccination coverages
#' @param GAVI_preventive GAVI preventive campaign info - this includes all vaccination activities
#' @param GAVI_switch which vaccination activity to compile
#' @param param_samples transmission intensity in each admin 1 unit or country
#' @param pop_all population in all countries by year and age
#' @param P_severe probability of severe infection
#' @param P_severeDeath probability of dying if severe
#' @param life_exp_GAVI life expectancies from Montagu
#' @param template output template
#' @param round whether to round the output to save memory
#' @param run_id run_id for stochastic runs. Defaults to NA
#'
run_burden_for_template = function(historic_dat,
                                   GAVI_preventive,
                                   GAVI_switch,
                                   param_samples,
                                   pop_all,
                                   P_severe,
                                   P_severeDeath,
                                   life_exp_GAVI,
                                   template,
                                   round = TRUE,
                                   run_id = NA){
  ## get info from template ##
  years = unique(template$year)
  ages = unique(template$age)
  countries = unique(template$country)
  country_names = unique(template$country_name)

  ## check the input data only applies for these years, ages and countries ##
  pop_all = dplyr::filter(pop_all, country_code %in% countries)
  life_exp_GAVI = dplyr::filter(life_exp_GAVI, country_code %in% countries)

  #--------------------------------------------------------------------------------------------------#
  ### step 1: coverage ### ---------------------------------------------------------------------------
  coverage_df =  compile_vaccination(historic_dat, GAVI_preventive, WUENIC = NA, GAVI_switch)

  ### step 2: infections ### -------------------------------------------------------------------------
  out1 = calc_infections(param_samples, coverage_df, pop_all, years, age_max = max(ages) )

  infections = out1$infections
  cohort_size = out1$cohort_size

  ### step 3: cases and deaths ### -------------------------------------------------------------------
  out2 = calc_cases_and_deaths(infections, P_severe, P_severeDeath)

  cases = out2$cases
  deaths = out2$deaths

  ### step 4: DALYS ### ------------------------------------------------------------------------------
  DALYs = calc_DALYs(cases[, , 1:length(countries)], deaths[, , 1:length(countries)], life_exp_GAVI, P_severe)

  ### step 5: template ### ---------------------------------------------------------------------------
  output_df = NULL

  for(countryIndex in 1:length(countries)){
    for (ageIndex in 1:length(ages)) {


      output_df = rbind( output_df, data.frame(disease = rep("YF", length(years)) ,
                                               run_id =  rep(run_id, length(years)),
                                               year = years,
                                               age = rep(ages[ageIndex], length(years)),
                                               country = rep(countries[countryIndex],
                                                             length(years)),
                                               country_name = rep(country_names[countryIndex],
                                                                  length(years)),
                                               cohort_size = cohort_size[, paste0(ages[ageIndex]),
                                                                         paste0(countries[countryIndex])],
                                               deaths = deaths[, paste0(ages[ageIndex]),
                                                               paste0(countries[countryIndex])],
                                               cases = cases[, paste0(ages[ageIndex]),
                                                             paste0(countries[countryIndex])],
                                               dalys = DALYs[, paste0(ages[ageIndex]),
                                                             paste0(countries[countryIndex]) ]) )
    }
  }

  output_df = output_df[, names(template)]


  # rounding #
  if(round){
    output_df[,c("deaths","cases","dalys")] = round(output_df [,c("deaths","cases","dalys")])
  }

  return(output_df)
}
