# 15 AASHTO Group Index 
# @purpose: Encode AASHTO Group Index Number (GIN) NASIS calculation into R pedotransfer function. 
#           Also, provides wrapper functions for dealing with L-RV-H and NASIS component SoilProfileCollections

# @inputs: 
# - % Passing #200 Sieve (sieveno200)
# - Liquid Limit (ll)
# - Plasticity Index (pi)

# @author: Cathy Seybold
# @contributor: Andrew G. Brown
# @last_update: 11/01/2019
# @nasis_last_update: 03/15/2018

# pedotransfer function for aashto
ptf_aashind <- function(.sieveno200, .ll, .pi, .aashtocl) {
  
  # calculate aashto group index number -- base calc uses #200 sieve and Plasticity Index
  aashind <- 0.01 * (.sieveno200 - 15) * (.pi - 10)
  
  # identify records that need a correction based on model involving liquid limit 
  # (note: since aashto class is calculated from GIN... these would be a priori values)
  skipll <- (!is.na(.aashtocl) & .aashtocl %in% c("a-2-6", "a-2-7")) |
            (.sieveno200 < 35 & .pi <= 10)
  
  # using liquid limit model to update GIN for records identified above (ll.idx) 
  aashind[!skipll] <- (.sieveno200[!skipll] - 35)*(.2 + .005*(.ll[!skipll] - 40)) + aashind[!skipll]
  
  # any records with plasticty index of zero, or aashto index less than zero get AASHTO GIN of zero
  low.plasticity <- which(.pi == 0 | aashind < 0)
  aashind[low.plasticity] <- 0
  return(aashind)
}

# helper function for L-RV-H data typical of SSURGO/STATSGO component horizon records
AASHTO_GIN <- function(.sieveno200_l, .sieveno200_r, .sieveno200_h, .ll_l, .ll_r, .ll_h, .pi_l, .pi_r, .pi_h, .aashtocl) {
  dat.l <- data.frame(.sieveno200_l, .ll_l, .pi_l, .aashtocl)
  dat.r <- data.frame(.sieveno200_r, .ll_r, .pi_r, .aashtocl)
  dat.h <- data.frame(.sieveno200_h, .ll_h, .pi_h, .aashtocl)
  gin.l <- ptf_aashind(dat.l[,1], dat.l[,2], dat.l[,3], dat.l[,4])
  gin.r <- ptf_aashind(dat.r[,1], dat.r[,2], dat.r[,3], dat.r[,4])
  gin.h <- ptf_aashind(dat.h[,1], dat.h[,2], dat.h[,3], dat.h[,4])
  gin <- c(gin.l, gin.r, gin.h)
  
  # reorder the values -- TODO: how often is this necessary??? NASIS calc does reordering so low, RV, high come out "in order"
  gin <- gin[order(gin)]
  
  res <- data.frame(calc_aashind_l=gin[1], calc_aashind_r=gin[2], calc_aashind_h=gin[3])
  return(res)
}

# helper function that emulates running calculations on all horizons in a set of NASIS components (SoilProfileCollection)
component_AASHTO_GIN <- function(components, round_gin = TRUE, n.digit = 0) {
  profileApply(components, FUN = function(p) {
      hz <- horizons(p)
      if(!'aashtocl' %in% names(hz))
        hz$aashtocl <- NA
      buf <- data.frame(calc_aashind_l = numeric(0), calc_aashind_r = numeric(0), calc_aashind_h = numeric(0))
      for(h in 1:nrow(hz)) {
        buf <- rbind(buf, AASHTO_GIN(hz[h, 'sieveno200_l'], hz[h, 'sieveno200_r'], hz[h, 'sieveno200_h'],
                                     hz[h, 'll_l'], hz[h, 'll_r'], hz[h, 'll_h'],
                                     hz[h, 'pi_l'], hz[h, 'pi_r'], hz[h, 'pi_h'], 
                                     hz[h, 'aashtocl']))
      }
      if(round_gin)
        buf <- round(buf, digits =  n.digit)
      return(buf)
    }, simplify = FALSE)
}

