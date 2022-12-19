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
