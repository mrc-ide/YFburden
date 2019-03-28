#' Compile vaccination coverages into one df
#'
#' @param historic_dat Historic vaccination coverages
#' @param GAVI_preventive GAVI preventive campaign info - this includes all vaccination activities
#' @param WUENIC WUENIC data for non-Gavi countries
#' @param GAVI_switch which vaccination activity to compile
#'
#' @return Vaccination activities for one GAVI switch scenario from 1940 to 2100
#' @export


compile_vaccination = function(historic_dat,
                               GAVI_preventive,
                               WUENIC,
                               GAVI_switch) {

  if(!is.null(historic_dat)){
    # get rid of NA entries
    historic_dat = historic_dat %>% filter( !is.na(coverage))
  }

  year_cut = switch(GAVI_switch,
                    "no-vaccination" = 2000,
                    "routine" = 2100,
                    "preventive" = 2100)

  if (GAVI_switch == "routine") {
    GAVI_preventive = GAVI_preventive %>% filter( activity_type == GAVI_switch)
  }

  # limit to only entries where coverage>0 to save time
  GAVI_preventive = GAVI_preventive %>% filter( coverage > 0)

  #####################
  if(nrow(GAVI_preventive)>0){
    ### ADD GAVI TO coverage df ###
    for (i in 1 : nrow(GAVI_preventive)) {

      if (GAVI_preventive$year[i] < year_cut) {

        historic_dat = rbind(historic_dat,
                             cbind(GAVI_preventive[i, ],
                                   skew = switch(GAVI_preventive$activity_type[i],
                                                 "routine" = 0,
                                                 "campaign" = -1)))

      }
    }
  }
  #########################
  if(!anyNA(WUENIC)){
    ### ADD WUENIC FOR GAB AND GNQ ###
    missing_countries = c("GAB", "GNQ")
    WUENIC = dplyr::filter(WUENIC, ISO_code %in% missing_countries)


    for (c in missing_countries) {
      WUENIC_subset = dplyr::filter(WUENIC, ISO_code == c)
      for (y in 1980:2016) {
        if (!is.na(WUENIC_subset[paste0("X", y)])) {
          tmp = data.frame(scenario = "yf-WUENIC",
                           set_name = "YF, with, WUENIC",
                           vaccine = "YF",
                           gavi_support = "none",
                           activity_type = "routine",
                           country_code = c,
                           country = WUENIC_subset$Cname,
                           year = y,
                           age_first = 0,
                           age_last = 0,
                           age_range_verbatim = "<NA>",
                           target = NA,
                           coverage = as.numeric(WUENIC_subset[paste0("X", y)])/100, skew = NA)

          historic_dat = rbind(historic_dat, tmp)
        }
      }
    }
  }

  vacc_dat = historic_dat[order(historic_dat$year), ]

  return(vacc_dat)
}



