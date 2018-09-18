#' calculate the number of infections in a country for the years specified
#'
#' @param model_type whether R0 or Foi. Defaults to Foi
#' @param transmission_param transmission intensity in that country
#' @param years years of interest
#' @param age_max maximum age group
#' @param pop population in countryby year and age
#' @param coverage vaccination coverage by campaign in country
#' @param immunityStart immunity profile at the beginning of simulation
#' @return The immunity profile by age and number of infections in one year
#'

run_infections_unit = function(model_type = "Foi",
                               transmission_param,
                               years,
                               age_max,
                               pop,
                               coverage,
                               immunityStart) {
  ### get rid of NA in pop ###
  pop[is.na(pop)] = 0
  ####

  ages = c(0:age_max)
  n_years = length(years)

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

    tmp = switch(model_type, Foi = generate_infections_static(transmission_param,
                                                              pop[yearIndex, 2:102],
                                                              immunity[yearIndex, ]),
                 R0 = generate_infections_R0(transmission_param,
                                             pop[yearIndex, 2:102], immunity[yearIndex, ]))

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
                                                  immunity[yearIndex,
                                                           ], skew = coverage$skew[y])
        }
      }

  }
  return(list(immunity = immunity, new_infections = new_infections))
}
