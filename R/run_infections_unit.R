#' move on immunity profile through ageing
#'
#' @param immunity current immunity profile
#' @return New immunity profile by age
#' @export

update_immunity = function(immunity) {
  ## takes the proportion of immune people in each age class should be a vector, of length n.ages.

  immunity_next = c(0, immunity[-length(immunity)])
  names(immunity_next) = names(immunity)

  return(immunity_next)
}




#' add vaccination coverage to change immunity profile
#'
#' @param coverage vaccination campaign coverage
#' @param age_first youngest age group to be vaccinated
#' @param age_last oldest age group to be vaccinated
#' @param immunity current immunity in one year
#' @param skew skew of vaccination. Defaults to 0 where vaccination is random
#'
#' @return The immunity profile by age in one year
#'  @export

add_vaccination = function(coverage, age_first, age_last, immunity, skew = 0) {
  ## year is the year of vaccination of the new birth cohort.
  if (is.na(skew)) {
    skew = 0
  }
  coverage = pmin(coverage, 1)  #check that it is at most 1

  ## immunity is the age distribution of immunity at the time point in question.
  if (skew == 0) {
    if (age_first != age_last) {
      immunity[paste0(age_first):paste0(age_last)] = 1 - (1 - coverage) *
        (1 - immunity[paste0(age_first):paste0(age_last)])
    } else {
      immunity[paste0(age_first)] = 1 - (1 - coverage) * (1 - immunity[paste0(age_first)])
    }
  } else if (skew == -1) {
    if (age_first != age_last) {
      immunity[paste0(age_first:age_last)] = pmin(1, coverage +
                                                            immunity[paste0(age_first:age_last)])
    } else {
      immunity[paste0(age_first)] = pmin(1, coverage + immunity[paste0(age_first)])
    }
  }
  return(immunity)
}




#' calculate the number of infections in a year for all age groups in R0 model
#'
#' @param R0 R0 in country
#' @param pop population by age in one year
#' @param immunity current immunity profile by age in one year
#' @return The number of new infections in one year and the immunity profile by age in one year
#' @export

generate_infections_R0 = function(R0, pop, immunity) {

  herd_immunity = 1 - (1/R0)
  sus = pop * (1 - immunity)
  prop_sus = sum(sus)/sum(pop)
  prop_to_infect = herd_immunity - (1 - prop_sus)  ## proportion of the population

  if (prop_to_infect < 0) {
    ## no infections.
    new_infect = rep(0, length(pop))
    names(new_infect) = names(pop)
    new_immunity = immunity
  } else {
    ## put in infections as appropriate, :
    prop_sus_to_infect = prop_to_infect/prop_sus
    new_infect = sus * prop_sus_to_infect
    new_immunity = (pop - sus + new_infect)/pop
    new_immunity[is.na(new_immunity)] = 0

    if (abs(sum(new_immunity * pop)/sum(pop) - herd_immunity) > .Machine$double.eps * 2) {
      stop("generate_infections_R0: new immunity different from herd immunity.\n")
    }
  }

  return(list(new_infections = new_infect, immunity = new_immunity))
}




#' calculate the number of infections in a year for all age groups in Foi model
#'
#' @param foi Foi in country
#' @param pop population by age in one year
#' @param immunity current immunity profile by age in one year
#' @return The immunity profile by age in one year and the number of infections for that year
#' @export


generate_infections_static = function(foi, pop, immunity) {

  sus = pop * (1 - immunity)

  prop_sus_to_infect = foi
  new_infect = sus * prop_sus_to_infect

  new_immunity = (pop - sus + new_infect)/pop

  new_immunity[is.na(new_immunity)] = 0

  rownames(new_immunity) = rownames(immunity)
  colnames(new_immunity) = colnames(immunity)

  return(list(new_infections = new_infect, immunity = new_immunity))
}





#' calculate the number of infections in a country for the years specified
#'
#' @param model_type whether R0 or Foi. Defaults to Foi
#' @param transmission_param transmission intensity in that country
#' @param years_in years of interest
#' @param age_max maximum age group
#' @param pop population in countryby year and age
#' @param coverage vaccination coverage by campaign in country
#' @param immunityStart immunity profile at the beginning of simulation
#'
#' @return The immunity profile by age and number of infections in one year
#' @export

run_infections_unit = function(model_type = "Foi",
                               transmission_param,
                               years_in,
                               age_max,
                               pop,
                               coverage,
                               immunityStart) {
  ### simulate from 1940
  years = 1940:max(years_in)
  ages = c(0:age_max)
  n_years = length(years)

  ### get rid of NA in pop ###
  pop[is.na(pop)] = 0
  ####


  ### declare set up
  immunity = new_infections = matrix(NA, nrow = n_years, ncol = age_max + 1)

  if (anyNA(immunityStart)) {
    immunity[1, ] = rep(0, ncol(immunity))
    new_infections[1, ] = rep(0, ncol(new_infections))
  } else {
    immunity[1, ] = immunityStart  # immunity profile taking into account previous vaccination and infection
  }

  # iterate over the years
  for (yearIndex in 1:(n_years)) {

    if (yearIndex > 1) {
      ## update immunity:
      immunity[yearIndex, ] = update_immunity(immunity[yearIndex - 1, ])
    }

    ## generate new infections

    tmp = switch(model_type,
                 "Foi" = generate_infections_static(transmission_param,
                                                    pop[which( pop[,1] %in% years[yearIndex]), 2:102],
                                                    immunity[yearIndex, ]),
                 "R0" = generate_infections_R0(transmission_param,
                                               pop[which( pop[,1] %in% years[yearIndex]), 2:102],
                                               immunity[yearIndex, ]))

    new_infections[yearIndex, ] = tmp$new_infections
    immunity[yearIndex, ] = tmp$immunity

    rownames(new_infections) = rownames(immunity) = years
    colnames(new_infections) = colnames(immunity) = 0:age_max


    ## add vaccination at the end of the year: coverage_df: year, age, coverage.
    if (nrow(coverage) > 0)
      for (y in 1:nrow(coverage)) {
        if (coverage$year[y] == years[yearIndex]) {

          immunity[yearIndex, ] = add_vaccination(coverage$coverage[y],
                                                  age_first = coverage$age_first[y],
                                                  age_last = coverage$age_last[y],
                                                  immunity[yearIndex,],
                                                  skew = 0)#coverage$skew[y])
        }
      }

  }

  ### only want immunity for years_in
  immunity = immunity[which(years %in% years_in), ]
  new_infections = new_infections[which(years %in% years_in), ]

  return(list(immunity = immunity, new_infections = new_infections))
}
