#' calculates the number of deaths and cases
#'
#' @param new_infections number of infections
#' @param P_severe probability of severe infection
#' @param P_severeDeath probability of dying if severe
#'
#' @return the numbers of cases and deaths
#' @export
calc_cases_and_deaths = function(new_infections, P_severe, P_severeDeath){

  # calculate proportion of infections that are severe
  severe_infections = P_severe * new_infections

  # of those calculate the proportion that die
  dead_infections = P_severeDeath * severe_infections

  return(list(cases = severe_infections, deaths = dead_infections))
}
