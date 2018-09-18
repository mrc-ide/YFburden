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
