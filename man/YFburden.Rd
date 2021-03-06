% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YFburden.R
\docType{package}
\name{YFburden}
\alias{YFburden}
\title{YFburden: A package for calculating the burden of Yellow Fever across
          the African endemic region and GAVI eligible countries.}
\description{
The package takes estimates of the force of infection in each country,
projected vaccination coverage, historic vaccination coverage, and
projected population sizes to calculate the number of infections. It
also takes estimates of disability weights and probability of severe
infections to calculate the number of severe infections, deaths and
DALYS.
}
\section{YFburden functions}{


add_vaccination: alters the immunity profile for the following year

compile_vaccination: takes historic and projected vaccination coverages
                     and puts them into one file in the same format

calc_DALYS: calculates number of DALYS given the number of infections

foi_prevac: calculates the force of infection before vaccination (ONLY USED
            IN R0 MODEL)

calc_infections: main function for calculating the infections in each country
                    and year

calc_cases_and_deaths: calculates cases and deaths given the number of infections

fun_immunityStart: Calculates the immunity profile at the satrt of the simulation

generate_infections_R0: calculates R0 model projections for a year in a country

generate_infections_static: calculates Foi model projections for a year in a
                            country

reformat_life_expectancies: rearranges Montagu life expectancies into the same
                           format as the infections: an array (years x ages x countries)

run_infections_unit: calculates infections in one country over the years

update_immunity: update immunity profile by ageing

run_burden_for_template: runs all functions and puts into Montagu template
}

