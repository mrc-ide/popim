#' Function to apply a single vaccination activity to the population
#'
#' @param pop_df a population dataframe object such as created by
#'     function setup_population
#' @param vacc_act details of the vaccination activity to be
#'     implemented as a named list containing the elements "year",
#'     "age_first", "age_last", "country", "coverage".
#' @return The original pop_df dataframe with column "immunity"
#'     updated to reflect the vaccination activity applied
#' @export
#' 

apply_vacc <- function(pop_df, vacc_act) {
    ## TO DO: tests on the input parameters
    stopifnot(is_population(pop_df))
    
    cohorts <- vacc_act$year - (vacc_act$age_last:vacc_act$age_first)
    
    for(j in cohorts) {
        ## year-1 as vaccination occurs at the end of the year
        df <- df %>% 
            dplyr::rowwise() %>% 
            dplyr::mutate(immunity = ifelse(cohort == j & 
                                     (year-1)>=vaccination$year[i] & 
                                     calc_new_coverage(immunity, 
                                                       vaccination$coverage[i], 
                                                       skew = 1),
                                     immunity))
    }
    return(df)
}
