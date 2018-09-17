#' YFburden: A package for calculating the burden of Yellow Fever across
#'           the African endemic region and GAVI eligible countries.
#'
#' The package takes estimates of the force of infection in each country,
#' projected vaccination coverage, historic vaccination coverage, and
#' projected population sizes to calculate the number of infections. It
#' also takes estimates of disability weights and probability of severe
#' infections to calculate the number of severe infections, deaths and
#' DALYS.
#'
#' @section YFburden functions:
#' add_vaccination: alters the immunity profile for the following year
#' compile_vaccination: takes historic and projected vaccination coverages
#'                      and puts them into one file in the same format
#' Death_and_DALYS: calculates number of severe infections, deaths and DALYS
#'                  given the number of infections
#' foi_prevac: calculates the force of infection before vaccination (ONLY USED
#'             IN R0 MODEL)
#' fun_calcInfections: main function for calculating the infections in each country
#'                     and year
#' fun_immunityStart: Calculates the immunity profile at the satrt of the simulation
#' generate_infections_R0: calculates R0 model projections for a year in a country
#' generate_infections_static: calculates Foi model projections for a year in a
#'                             country
#' run_infections_unit: calculates infections in one country over the years
#' update_immunity: update immunity profile by ageing
#'
#' @docType package
#' @name YFburden
NULL
