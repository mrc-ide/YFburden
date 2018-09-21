#' get life expectancies in array form
#'
#' @param life_exp_GAVI life expectanices from Montagu
#' @param years years on interest
#' @param ages ages of interest
#'
#' @return life expectancies in form years x ages x countries
#'
reformat_life_expectancies = function(life_exp_GAVI,
                                      years,
                                      ages){

  years_le = min(life_exp_GAVI$year) : (max(life_exp_GAVI$year) + 5)

  ages_le = min(life_exp_GAVI$age_from) : max(life_exp_GAVI$age_to)

  countries_le = unique(life_exp_GAVI$country_code)

  life_exp = rep(NA, length(years_le)*length(ages_le)*length(countries_le))
  dim(life_exp) = c(length(years_le), length(ages_le), length(countries_le))

  for(y_ind in 1:length(unique(life_exp_GAVI$year))){
    for(a_ind in 1:length(unique(life_exp_GAVI$age_to))){

      life_exp[((y_ind-1)*5): (y_ind*5),
               unique(life_exp_GAVI$age_from)[a_ind] : unique(life_exp_GAVI$age_to)[a_ind], ] =
        dplyr::filter(life_exp_GAVI,
                      year == unique(life_exp_GAVI$year)[y_ind] &
                      age_from == unique(life_exp_GAVI$age_from)[a_ind])$value

    }
  }

  dimnames(life_exp)[[1]] = years_le
  dimnames(life_exp)[[2]] = ages_le
  dimnames(life_exp)[[3]] = countries_le

  #filter by year and ages
  life_exp = life_exp[which(years_le %in% years), which(ages_le %in% ages), ]

  return(life_exp)
}



#' Calculate the number of DALYS
#'
#' @param cases number of cases per year, age and country
#' @param deaths number of deaths per year, age and country
#' @param life_exp_GAVI life expectancies from Montagu
#' @param P_severe probability of severe infection
#' @param d_acute days of acute infection per year. Default = 17.8 / 365.
#' @param dw_acute disability weight for acute. Default = 0.172.
#' @param d_conv days convalescent per year. Default = 28 / 365.
#' @param dw_conv disability weight if convalescent. Default = 0.024.
#'
#' @return DALYs by year, age and countries
#'
calc_DALYs = function(cases,
                      deaths,
                      life_exp_GAVI,
                      P_severe,
                      d_acute = 17.8 / 365,
                      dw_acute = 0.172,
                      d_conv = 28 / 365,
                      dw_conv = 0.024){

  ## convert life expectancy format
  life_exp = reformat_life_expectancies(life_exp_GAVI,
                                        years = as.numeric(dimnames(cases)[[1]]),
                                        ages = as.numeric(dimnames(cases)[[2]]) )

  ## YLL ##
  YLL = deaths * life_exp

  ## YLD ##
  YLD = cases * (d_acute * dw_acute + (1 - P_severe) * d_conv * dw_conv)

  DALYs = YLL + YLD

  return(DALYs)
}
