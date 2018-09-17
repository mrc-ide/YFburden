#' Compile vaccination coverages into one df
#'
#' @param historic_dat Historic vaccination coverages
#' @param GAVI_preventive GAVI preventive campaign info - this includes all vaccination activities
#' @param WUENIC WUENIC data for non-Gavi countries
#' @param GAVI_switch which vaccination activity to compile
#'
#' @return Vaccination activities for one GAVI switch scenario from 1940 to 2100
#' compile_vaccination()


compile_vaccination = function(historic_dat, GAVI_preventive, WUENIC, GAVI_switch) {
    
    # get rid of NA entries
    historic_dat = dplyr::filter(historic_dat, !is.na(coverage))
    
    year_cut = switch(GAVI_switch, `no-vaccination` = 2000, routine = 2100, preventive = 2100)
    
    if (GAVI_switch == "routine") {
        GAVI_preventive = dplyr::filter(GAVI_preventive, activity_type == "routine")
    }
    
    # limit to only entries where coverage>0 to save time
    GAVI_preventive = dplyr::filter(GAVI_preventive, coverage > 0)
    
    ##################### 
    
    ### ADD GAVI TO coverage df ###
    for (i in 1:nrow(GAVI_preventive)) {
        
        if (GAVI_preventive$year[i] < year_cut) {
            
            historic_dat = rbind(historic_dat, cbind(GAVI_preventive[i, ], skew = switch(GAVI_preventive$activity_type[i], routine = 0, campaign = -1)))
            
        }
    }
    
    ######################### 
    
    ### ADD WUENIC FOR GAB AND GNQ ###
    missing_countries = c("GAB", "GNQ")
    WUENIC = dplyr::filter(WUENIC, ISO_code %in% missing_countries)
    
    
    for (c in missing_countries) {
        WUENIC_subset = dplyr::filter(WUENIC, ISO_code == c)
        for (y in 1980:2016) {
            if (!is.na(WUENIC_subset[paste0("X", y)])) {
                tmp = data.frame(scenario = "yf-WUENIC", set_name = "YF, with, WUENIC", vaccine = "YF", gavi_support = "none", activity_type = "routine", country_code = c, 
                  country = WUENIC_subset$Cname, year = y, age_first = 0, age_last = 0, age_range_verbatim = "<NA>", target = NA, coverage = as.numeric(WUENIC_subset[paste0("X", 
                    y)])/100, skew = NA)
                
                historic_dat = rbind(historic_dat, tmp)
            }
        }
    }
    
    vacc_dat = historic_dat[order(historic_dat$year), ]
    
    return(vacc_dat)
}



# ################################################################################# #### SCRIPT TO put doses from GAVI into csv with historic campaigns ######
# ################################################################################# # DATA # # Import historic and WHO campaigns #### historic_dat =
# read.csv(paste0('Outputs/', 'historic_and_WHO_vaccinationCampaigns_in_montagu_format.csv'), stringsAsFactors = FALSE) # Import Gavi scenario #
# latest_GAVI_path =paste0('Inputs/coverage_201710/coverage_201710gavi-2_yf-preventive-gavi.csv') GAVI_preventive = read.csv(latest_GAVI_path,
# stringsAsFactors=FALSE) # Import WUENIC for GAB and GNQ # WUENIC = read.csv('Inputs/WUENIC_2018.csv', stringsAsFactors = FALSE) ################### ### TIDY
# INPUTS ### # get rid of NA entries historic_dat = dplyr::filter(historic_dat, !is.na(coverage)) ################### ### GAVI scenario ### GAVI_switch =
# 'no-vaccination' #'preventive' #'routine' # year_cut = switch(GAVI_switch, 'no-vaccination' = 2000, 'routine' = 2100, 'preventive' = 2100) if(GAVI_switch ==
# 'routine'){ GAVI_preventive = dplyr::filter(GAVI_preventive, activity_type == 'routine') } # limit to only entries where coverage>0 to save time
# GAVI_preventive = dplyr::filter(GAVI_preventive, coverage>0) ##################### ### ADD GAVI TO coverage df ### for( i in 1:nrow(GAVI_preventive) ){
# if(GAVI_preventive$year[i]<year_cut){ historic_dat = rbind(historic_dat, cbind( GAVI_preventive[i,] , skew = switch(GAVI_preventive$activity_type[i],
# 'routine'=0, 'campaign' = -1)) ) } } ######################### ### ADD WUENIC FOR GAB AND GNQ ### missing_countries = c('GAB', 'GNQ') WUENIC =
# dplyr::filter(WUENIC, ISO_code %in% missing_countries) for(c in missing_countries){ WUENIC_subset = dplyr::filter(WUENIC, ISO_code == c) for(y in 1980:2016){
# if(!is.na(WUENIC_subset[paste0('X',y)])){ tmp = data.frame(scenario = 'yf-WUENIC', set_name = 'YF, with, WUENIC', vaccine = 'YF', gavi_support = 'none',
# activity_type = 'routine', country_code = c, country = WUENIC_subset$Cname, year = y, age_first = 0, age_last = 0, age_range_verbatim = '<NA>', target = NA,
# coverage = as.numeric(WUENIC_subset[paste0('X',y)])/100, skew = NA) historic_dat = rbind(historic_dat, tmp) } } } ### SAVE ### historic_dat =
# historic_dat[order(historic_dat$year),] write.csv(historic_dat, paste0('Outputs/', 'vaccination_coverage_country_year_', GAVI_switch, '.csv'), row.names =
# FALSE)
