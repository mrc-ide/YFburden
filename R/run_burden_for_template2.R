#' run burden steps and put into a template - new version with faster data output
#'
#' @param historic_dat = Historic vaccination coverages
#' @param GAVI_preventive GAVI preventive campaign info - this includes all vaccination activities
#' @param GAVI_switch which vaccination activity to compile
#' @param param_samples transmission intensity in each admin 1 unit or country
#' @param vac_eff vaccine efficacy
#' @param pop_all population in all countries by year and age
#' @param P_severe probability of severe infection
#' @param P_severeDeath probability of dying if severe
#' @param life_exp_GAVI life expectancies from Montagu
#' @param template output template
#' @param round whether to round the output to save memory
#' @param run_id run_id for stochastic runs. Defaults to NA
#'
#' @return output_df output in template format
#' @export
run_burden_for_template2 = function(historic_dat,
                                   GAVI_preventive,
                                   GAVI_switch,
                                   param_samples,
                                   vac_eff,
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
  pop_all = pop_all %>% filter( country_code %in% countries)
  life_exp_GAVI = life_exp_GAVI %>% filter( country_code %in% countries)

  #--------------------------------------------------------------------------------------------------#
  ### step 1: coverage ### ---------------------------------------------------------------------------
  coverage_df =  compile_vaccination(historic_dat, GAVI_preventive, WUENIC = NA, GAVI_switch)

  ### step 2: infections ### -------------------------------------------------------------------------
  out1 = calc_infections(param_samples, vac_eff, coverage_df, pop_all, years, age_max = max(ages) )

  infections = out1$infections
  cohort_size = out1$cohort_size

  ### step 3: cases and deaths ### -------------------------------------------------------------------
  out2 = calc_cases_and_deaths(infections, P_severe, P_severeDeath)

  cases = out2$cases
  deaths = out2$deaths

  ### step 4: DALYS ### ------------------------------------------------------------------------------
  DALYs = calc_DALYs(cases[, , 1:length(countries)], deaths[, , 1:length(countries)], life_exp_GAVI, P_severe)

  ### step 5: template ### ---------------------------------------------------------------------------
  n_countries=length(countries)
  n_ages=length(ages)
  n_years=length(years)
  n_cats=n_years*n_ages
  n_lines=n_cats*n_countries

  null_list=rep(NA,n_lines)
  age_list=null_list
  country_list=null_list
  country_name_list=null_list
  values1=c(1:(n_cats))
  values2=c(1:n_years)
  for(countryIndex in 1:n_countries){
    country_list[values1]=countries[countryIndex]
    country_name_list[values1]=country_names[countryIndex]
    values1=values1+n_cats
    for (ageIndex in 1:n_ages) {
      age_list[values2]=rep(ages[ageIndex],n_years)
      values2=values2+n_ages
    }
  }
  cohort_size_list=as.vector(cohort_size)
  dalys_list=as.vector(DALYs)
  cases_list=as.vector(cases)
  deaths_list=as.vector(deaths)

  output_df <- data.frame(disease = rep("YF", n_lines) ,
                          run_id =  rep(run_id, n_lines),
                          year = rep(years,n_countries*n_ages),
                          age = age_list,
                          country = country_list,
                          country_name = country_name_list,
                          cohort_size = cohort_size_list,
                          dalys = dalys_list,
                          cases = cases_list,
                          deaths = deaths_list)

  output_df = output_df[, names(template)]

  # rounding #
  if(round){
    output_df[,c("deaths","cases","dalys")] = round(output_df [,c("deaths","cases","dalys")])
  }

  return(output_df)
}
