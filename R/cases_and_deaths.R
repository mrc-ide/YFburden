#' calculates the number of deaths and DALYS
#'
#' @param new_infections number of infections
#' @param P_severe probability of severe infection
#' @param P_severeDeath probability od dying if severe
#'
#' @return the numbers of cases and deaths
#'
cases_and_deaths = function(new_infections, P_severe, P_severeDeath){

  # calculate proportion of infections that are severe
  severe_infections = P_severe * new.infections

  # of those calculate the proportion that die
  dead_infections = P_severeDeath * severe.infections

  return(list(cases = severe_infections, deaths = dead_infections))
}
