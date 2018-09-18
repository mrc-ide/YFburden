#' get life expectancies in array form
#'
#' @param life_exp_GAVI life expectanices from Montagu
#'
#' @return life expectancies in form years x ages x countries
#'
reformat_life_expectancies = function(life_exp_GAVI, years, ages){

  years_le = min(life_exp_GAVI$year) : (max(life_exp_GAVI$year) + 5)

  ages_le = min(life_exp_GAVI$age_from) : max(life_exp_GAVI$age_to)

  countries_le = unique(life_exp_GAVI$country_code)

  life_exp = rep(NA, length(years_le)*length(ages_le)*length(countries_le))
  dim(life_exp) = c(length(years_le), length(ages_le), length(countries_le))

  for(y_ind in 1:length(unique(life_exp_GAVI$year))){
    for(a_ind in 1:length(unique(life_exp_GAVI$age_to))){

      life_exp[((y_ind-1)*5): (y_ind*5),
               unique(life_exp_GAVI$age_from)[a_ind] : unique(life_exp_GAVI$age_to)[a_ind], ] =
        dplyr::filter(life_exp_GAVI, year == unique(life_exp_GAVI$year)[y_ind] &
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
