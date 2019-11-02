# 15 AASHTO Group Index 
# @purpose: Encode AASHTO Group Index Number (GIN) NASIS calculation into R pedotransfer function. 
#           Also, provides wrapper functions for dealing with L-RV-H and NASIS component SoilProfileCollections

# @inputs: 
# - % Passing #200 Sieve (sieveno200)
# - Liquid Limit (ll)
# - Plasticity Index (pi)
# - [optional] AASHTO Class (to identify A-2-6, A-2-7 & A-8)

# @author: Cathy Seybold
# @contributor: Andrew G. Brown
# @last_update: 11/02/2019
# @nasis_last_update: 03/15/2018

# pedotransfer function for aashto group index
ptf_aashind <- function(.sieveno200,  .ll, .pi, .aashtocl) {
  # calculate aashto group index number -- base calc uses #200 sieve and Plasticity Index
  aashind <- 0.01 * (.sieveno200 - 15) * (.pi - 10)
  
  # identify records that need a correction based on LL term
  # (note: since aashto class is calculated from GIN... these would be a priori values or based on data)
  skipll <- (!is.na(.aashtocl) & 
               grepl(.aashtocl, pattern="[Aa]-2-[67]")) | 
                (.sieveno200 < 35 & .pi >= 10)

  # # using liquid limit model to update GIN for records identified above (skipll) 
  aashind[!skipll] <- (.sieveno200[!skipll] - 35) *
                        (.2 + .005*(.ll[!skipll] - 40)) +
                          aashind[!skipll]

  # any records with plasticty index of zero, or aashto index less than zero get AASHTO GIN of zero
  low.plasticity <- which(.pi == 0 | aashind < 0)
  aashind[low.plasticity] <- 0
  
  # organic soils get null value
  aashind[grepl(.aashtocl, pattern = "[Aa]-8")] <- NA
  
  return(aashind)
}

# helper function for L-RV-H data as individual arguments
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

# emulates running calculations on all horizons in a set of NASIS components (SoilProfileCollection)
component_AASHTO_GIN <- function(components, FUN, ...) {
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
      hz[hz.ok_h,] <- within(hz[hz.ok_h,], {
        calc_aashind_h <- ptf_aashind(sieveno200_h, ll_h, pi_h, aashtocl)
      })
      
      hz[hz.ok_r,] <- within(hz[hz.ok_r,], {
        calc_aashind_r <- ptf_aashind(sieveno200_r, ll_r, pi_r, aashtocl)
      })
      
      hz[hz.ok_l,] <- within(hz[hz.ok_l,], {
        calc_aashind_l <- ptf_aashind(sieveno200_l, ll_l, pi_l, aashtocl)
      })
       
      # TODO: No reordering L-R-H that may be mismatched
      # how often is this needed?
      
      # allow for custom post-processing function
      # e.g. floor v.s. round -- or sorting
      if(!is.null(FUN)) {
        idx <- match(c('calc_aashind_l','calc_aashind_r', 'calc_aashind_h'), colnames(hz))
        hz[,idx] <- FUN(hz[,idx], ...)
      }
      
      # format for easy merging back into parent spc
      return(hz[ ,c(idname(p), 
                    hzidname(p),
                    'calc_aashind_l','calc_aashind_r', 'calc_aashind_h')])
    }, simplify = FALSE))
  rownames(res) <- NULL
  return(res)
}
