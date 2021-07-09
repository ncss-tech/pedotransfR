# # 06 Bulk Density, 0.33 bar
# 
# @purpose: Calculates bulk density at 0.33 bars (low, high, and RV) for a horizon. 
# 
# @inputs:
# - total sand (l, rv, h)
# - total silt (l, rv, h)
# - total clay (l, rv, h)
# - organic matter (l, rv, h)
# - gypsum (l, rv, h)
# - vcos (rv), cos (rv), ms (rv), fs (rv), vfs (rv) only used for sandy soils
# - texture modifier (used to identify ashy, medial, and hydrous textures)
# - horizon name (used to identify Ap, A, Bt, Bw, B, E, C, x, d, r, hs, s, o, h, v, m, and q layers)
# - restriction kind (used to identify fragipan, cemented horizon, duripan, plinthite, densic material, densic bedrock, paralithic bedrock, ortstein, petrocalcic, petroferric, petrogypsic)
# - restriction top depth (rv)
# - taxonomic suborder (used to identify saprists, hemists, fibrists, histels)
# - taxonomic great group (used to identify folistels)

# @author: Cathy Seybold
# @contributor: Andrew G. Brown
# @last_update: 11/01/2019
# @nasis_last_update: 6/12/2019

ptf_db13bar <- function(taxorder, taxsuborder, taxgreatgroup, taxsubgrp, 
                        hzdept, hzdepb, hzname, 
                        sandvc, sandco, sandmed, sandfine, sandvf, 
                        sandtotal, silttotal, claytotal,
                        om, gypsum, ksat, texture) {
  
  oc <- om / 1.72
  # ASSIGNS A, B, & C hznames when "H" is present.
  hzname[grepl(hzname, pattern='^H1$')] <- "A"
  hzname[grepl(hzname, pattern='^H[23]$')] <- "B"
  hzname[grepl(hzname, pattern='^H[^1-3]$')] <- "C"
  
  # gets the 1st two characters of the horizon name.
  hzn2 <- lapply(hzname, substr, 0, 2)
  
  # gets the 1st three characters of the horizon name.
  hzn3 <- lapply(hzname, substr, 0, 3)
  
  texmod <- rep("none", length(texture))
  texmod[grepl(texture, pattern="ashy", ignore.case = TRUE)] <- "Ash"
  texmod[grepl(texture, pattern="medial", ignore.case = TRUE)] <- "Med"
  texmod[grepl(texture, pattern="hydrous", ignore.case = TRUE)] <- "Hyd"
  
  # Read table of bulk densities for horizons with substitute classes, apply simple Nearest Neighbor approach
  
  # andic soil properties -- uses organic carbon content (relative to lookup table) as distance metric
  wt_BD_and <- do_nn_lookup(filename="../data/bd_andic", target=texmod, dist.var=oc,
               lut.names=c('texture_fam', 'hzn_top', 'hzn_bot', 'hzn_desgn', 'hzn_master', 
                                        'sand', 'silt', 'clay', 'carbon', 'w3cld', 'w15l2', 'db_13b'),
               groups="texture_fam", lut.dist.var = "carbon", property="db_13b") 
  
  # gypsum (>40%) -- uses rv gypsum content (relative to lookup table) as distance metric
  wt_BD_gyp40 <- do_nn_lookup(filename="../data/bd_gypsum", target=NA, dist.var=gypsum,
                            lut.names=c('ghzn_top' , 'ghzn_bot' , 'ghzn_desgn' , 'ghzn_master' , 'gsand' , 'gsilt' , 
                           'gclay' , 'gcarbon' , 'g_db_13b' , 'c_gypl2' , 'gw3cld' , 'gw15l2'),
                            groups=NA, lut.dist.var = "c_gypl2", property="g_db_13b") 
  wt_BD_gyp40[gypsum < 40] <- NA
  
  # gypsum (<=40%)
  wt_BD_gyp <- do_normalized_nn_lookup(filename="../data/bd_gypsum", target=NA, dist.var=gypsum,
                              lut.names=c('ghzn_top' , 'ghzn_bot' , 'ghzn_desgn' , 'ghzn_master' , 'gsand' , 'gsilt' , 
                                          'gclay' , 'gcarbon' , 'g_db_13b' , 'c_gypl2' , 'gw3cld' , 'gw15l2'),
                              normalize.names=list('sandtotal_r' = 'gsand' , 
                                                   'silttotal_r' = 'gsilt' , 
                                                   'claytotal_r' = 'gclay' , 
                                                   'oc' = 'gcarbon', 
                                                   'gypsum_r' = 'c_gypl2'),
                              groups=NA, lut.dist.var = "c_gypl2", property="g_db_13b")  
  
  
}

do_normalized_nn_lookup <- function(filename, lut.names, normalize.names, groups, target, dist.var, lut.dist.var, property) {
  lut <- read.table(filename, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(lut) <- lut.names
  
  if(is.na(groups) & all(is.na(target))) {
    # no grouping var
    lut$group <- 'all'
    groups <- 'group'
    target <- rep('all', length(dist.var))
  }
  
  lut.sp <- split(lut, f = lut[,groups])
  
  res <- lapply(1:length(target), function(i) {
    cur <- target[i]
    lut2 <- lut.sp[[cur]]
    
    lut2.mean <- apply(lut2[,normalize.names], 2, mean)
    lut2.stdev <- apply(lut2[,normalize.names], 2, sd)
    lut2.scale <- apply(lut2[,normalize.names], 2, scale)
    
    target.scale <- #TODO: continue here
    
    if(!is.null(lut)) {
      distance7 <- sqrt((dist.var[i] - lut2[,lut.dist.var]) ^ 2)
      
      # get 10 nearest neighbors index positions in lookup table
      minn <- match(distance7[order(distance7)][1:10], distance7)
      
      # Sets the minimum distance to 1 when equal to zero; avoids dividing by zero in the following equations.
      minn[minn == 0] <- 1
      sum_dist <- sum(minn)
      
      # Calculates the relative distance of each of the 10 nearest neighbors. 
      # A power term of 1 is used, which assumes a simple inverse relationship between distance and bulk density
      rel_dist <- (sum_dist / minn) ^ 1
      sum_rel_dist <- sum(rel_dist)
      
      # Calculate weights for 10 nearest neighbors
      wt <- rel_dist / sum_rel_dist
      
      # Extract bulk densitities from neighbors
      prop <- lut2[minn, property]
      
      # calculate distance-weighted average bulk density
      wt_prop <- weighted.mean(prop, wt)
      return(wt_prop)
    }
  }) 
  return(res)
}

do_nn_lookup <- function(filename, lut.names, groups, target, dist.var, lut.dist.var, property) {
  # Read table of bulk densities for soils with andic soil properties 
  lut <- read.table(filename, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(lut) <- lut.names
  if(is.na(groups) & all(is.na(target))) {
    # no grouping var
    lut$group <- 'all'
    groups <- 'group'
    target <- rep('all', length(dist.var))
  }
  lut.sp <- split(lut, f = lut[,groups])
  
  res <- lapply(1:length(target), function(i) {
    cur <- target[i]
    lut2 <- lut.sp[[cur]]
    if(!is.null(lut)) {
      distance7 <- sqrt((dist.var[i] - lut2[,lut.dist.var]) ^ 2)
      
      # get 10 nearest neighbors index positions in lookup table
      minn <- match(distance7[order(distance7)][1:10], distance7)
      
      # Sets the minimum distance to 1 when equal to zero; avoids dividing by zero in the following equations.
      minn[minn == 0] <- 1
      sum_dist <- sum(minn)
      
      # Calculates the relative distance of each of the 10 nearest neighbors. 
      # A power term of 1 is used, which assumes a simple inverse relationship between distance and bulk density
      rel_dist <- (sum_dist / minn) ^ 1
      sum_rel_dist <- sum(rel_dist)
      
      # Calculate weights for 10 nearest neighbors
      wt <- rel_dist / sum_rel_dist
      
      # Extract bulk densitities from neighbors
      prop <- lut2[minn, property]
      
      # calculate distance-weighted average bulk density
      wt_prop <- weighted.mean(prop, wt)
      return(wt_prop)
    }
  }) 
  return(res)
}
