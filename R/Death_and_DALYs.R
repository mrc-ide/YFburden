#' calculates the number of deaths and DALYS
#'
#' @param new_infections number of infections
#' @param P_severe probability of severe infection
#' @param P_severeDeath probability od dying if severe
#' @param d_acute probability of acute infection?
#' @param dw_acute disability weight for acute
#' @param d_conv probability convalescent
#' @param dw_conv disability weight if convalescent
#' @param life_exp life expectancies
#' @return the numbers of deaths and dalys in each country and year IN DEVELOPMENT

Death_and_DALYs = function(new_infections, P_severe, P_severeDeath, d_acute, dw_acute, d_conv, dw_conv,  life_exp) {

    # calculate proportion of infections that are severe
    severe_infections = P_severe * new.infections
    # of those calculate the proportion that die
    dead_infections = P_severeDeath * severe.infections

    ### DALYS ### YLL ##
    YLL = dead_infections * life_exp[, 3:103]

    ## YLD ##
    YLD = severe_infections * (d_acute * dw_acute + (1 - P_severe) * d_conv * dw_conv)

    DALY = YLL + YLD

    rownames(DALY) = rownames(new_infections)
    colnames(DALY) = colnames(new_infections)

    rownames(new.infections) = rownames(new_infections)
    colnames(new.infections) = colnames(new_infections)

    return(list(dead_infections = dead_infections, severe_infections = severe_infections, DALY = DALY, YLL = YLL, YLD = YLD))
}
