#' Function to combine two coverages for the same cohort
#'
#' This function to calculate the resulting coverage if coverages of
#' two separate vaccination activities are combined, and returns this
#' single value.
#'
#' @param cov1 first coverage value as a proportion - needs to be
#'     between 0 and 1
#' @param cov2 second coverage value as a proportion - needs to be
#'     between 0 and 1
#' @param skew integer to determine the assumption used to combine
#'     coverages. Valid values are 1, 0 and -1. For skew = 0 (the
#'     default), allocation of vaccine is random within the
#'     population, therefore the resulting coverage will be smaller
#'     than the sum of the coverages. For skew = 1, there is a 100%
#'     correlation between who will get the vaccine in either
#'     vaccination activitiy, the resulting coverage is simply the
#'     larger of the two inputs. For skew = -1, doses are targeted at
#'     unvaccinated people, resulting in the sum of both coverages,
#'     though capped by 1 (full coverage).
#' @return combined coverage
#' @export
#'
calc_new_coverage <- function(cov1,cov2,skew=0) {

    if(cov1 < 0 | cov1 > 1) stop("invalid coverage value cov1 supplied")
    if(cov2 < 0 | cov2 > 1) stop("invalid coverage value cov2 supplied")
    if(!(skew %in% c(-1,0,1))) stop("invalid value for parameter skew")

  if(skew == 0){
    return(cov1 + cov2 - cov1*cov2) # allocation is random
  } 
  
  if(skew == 1){
    return(max(c(cov1, cov2), na.rm = TRUE)) # 100% correlation
  } 
  
  if(skew == -1){
    return(min(c(1, cov1 + cov2), na.rm = TRUE)) # doses are targeted at unvaccinated
  }
}
