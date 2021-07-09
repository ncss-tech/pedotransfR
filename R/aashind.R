# 15 AASHTO Group Index 
# @purpose: Encode AASHTO Group Index Number (GIN) NASIS calculation into 
#           R pedotransfer function. 
# @author: Cathy Seybold
# @contributor: Andrew G. Brown
# @last_update: 11/02/2019
# @nasis_last_update: 03/15/2018
# @inputs: 
# - % Passing #200 Sieve (sieveno200)
# - Liquid Limit (ll)
# - Plasticity Index (pi)
# - [optional] AASHTO Class (to identify A-2-6, A-2-7 & A-8)

#' @title AASHTO Group Index
#' @description R pedotransfer function derived from AASHTO Group Index NASIS calculation.
#' @param sieveno200 A numeric vector containing \% Passing #200 Sieve.
#' @param ll A numeric vector containing Liquid Limit.
#' @param pi A numeric vector containing Plasticity Index.
#' @param aashtocl optional: A character vector containing AASHTO Class 
#'  (to identify A-2-6, A-2-7 & A-8). default: NULL;
#' @return A numeric vector containing calculated AASHTO Group Index.
#' @author Andrew G. Brown.
#' @examples 
#' ptf_aashind(sieveno200 = 60, ll = 44, pi = 26, aashtocl = "A-7-6")
#' @rdname ptf_aashind
#' @export ptf_aashind
ptf_aashind <- function(sieveno200,  ll, pi, aashtocl = NULL) {
  
  # if AASHTO class is not specified, buffer it with NA
  if(length(aashtocl) == 0) {
    aashtocl <- rep(NA, length(sieveno200))
  }
   
  # calculate aashto group index number -- 
  # base calc uses #200 Sieve and Plasticity Index
  aashind <- 0.01 * (sieveno200 - 15) * (pi - 10)
  
  # identify records that need a correction based on LL term
  skipll <- (!is.na(aashtocl) & 
               grepl(aashtocl, pattern="[Aa]-2-[67]")) | 
                (sieveno200 < 35 & pi >= 10)

  # # using liquid limit model to update GIN for records identified above (skipll) 
  aashind[!skipll] <- (sieveno200[!skipll] - 35) *
                        (.2 + .005*(ll[!skipll] - 40)) +
                          aashind[!skipll]

  # any records with plasticty index of zero, 
  #  also, any aashto index less than zero are assigned 0
  low.plasticity <- which(pi == 0 | aashind < 0)
  aashind[low.plasticity] <- 0
  
  # organic soils get null value
  aashind[grepl(aashtocl, pattern = "[Aa]-8")] <- NA
  
  return(aashind)
}

#' @title AASHTO Group Index for NASIS/SSURGO components
#' @description Runs AASHTO Group Index (\code{ptf_aashind}) calculations on all horizons in a set of NASIS/SSURGO components (SoilProfileCollection).
#' @param components A \code{SoilProfileCollection} Object containing low, 
#'  RV, and high values of the standard inputs and SSURGO/NASIS-naming
#'  convention to \code{ptf_aashind}. E.g. \code{sieveno200_l}, \code{ll_r},
#'  \code{pi_h}, \code{aashtocl}, etc.
#' @param FUN A function to apply to the end result. Usually \code{floor}.
#' @param ... Additional arguments to \code{FUN}.
#' @return A \code{data.frame} containing L, RV and H calculated AASHTO Group Index ('calc_aashind_l','calc_aashind_r', 'calc_aashind_h')
#' @author Andrew G. Brown
#' @importFrom aqp horizons profileApply idname hzidname
#' @importFrom soilDB fetchNASIS fetchSDA
#' @importFrom stats complete.cases sd weighted.mean 
#' @importFrom utils read.table 
component_aashind <- function(components, FUN, ...) {
  res <- do.call('rbind', profileApply(components, FUN = function(p) {
      hz <- horizons(p)
      
      if(!'aashtocl' %in% names(hz))
        hz$aashtocl <- NA
      
      # make sure _required_ elements are present for calculation
      hz.ok_h <- complete.cases(hz[,c('sieveno200_h', 'll_h', 'pi_h')])
      hz.ok_r <- complete.cases(hz[,c('sieveno200_r', 'll_r', 'pi_r')])
      hz.ok_l <- complete.cases(hz[,c('sieveno200_l', 'll_l', 'pi_l')])
      
      # operate on complete sets of L, R, H independently...
      # but also, leave records as place holders <NA> where incomplete data occur
      hz[,c('calc_aashind_l','calc_aashind_r', 'calc_aashind_h')] <- NA
      
      hhi <- hz[hz.ok_h, ]
      hr <- hz[hz.ok_r, ]
      hlo <- hz[hz.ok_l, ]
      
      hz[hz.ok_h, ]$calc_aashind_h <- ptf_aashind(hhi$sieveno200_h, hhi$ll_h, hhi$pi_h, hhi$aashtocl)
      hz[hz.ok_r, ]$calc_aashind_r <- ptf_aashind(hr$sieveno200_h, hr$ll_r, hr$pi_r, hr$aashtocl)
      hz[hz.ok_l, ]$calc_aashind_l <- ptf_aashind(hlo$sieveno200_l, hlo$ll_l, hlo$pi_l, hlo$aashtocl)
      
      # TODO: No reordering L-R-H that may be mismatched
      # how often is this needed?
      
      # allow for custom post-processing function
      # e.g. floor v.s. round -- or sorting
      if(!is.null(FUN)) {
        idx <- match(c('calc_aashind_l','calc_aashind_r', 'calc_aashind_h'), colnames(hz))
        hz[,idx] <- FUN(hz[,idx], ...)
      }
      
      # format for easy merging back into parent spc
      return(hz[ , c(idname(p), hzidname(p), 'calc_aashind_l','calc_aashind_r', 'calc_aashind_h')])
    }, simplify = FALSE))
  rownames(res) <- NULL
  return(res)
}
