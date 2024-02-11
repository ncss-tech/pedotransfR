# pedotransfR

## Installation of R package

Get the latest development version from GitHub. This will require the latest version of `remotes` -- install that first from CRAN if you need it.

```r
remotes::install_github("ncss-tech/pedotransfR")
```

## Example: AASHTO Group Index for component data retrieved from Soil Data Access (SDA)

```r
library(aqp)
library(soilDB)
library(pedotransfR)

# use soilDB fetch function to get some soils information from SDA 
#   these are soils with varying amounts of ASP -- wide range in aashto gin
f <- fetchSDA(WHERE = "compname IN ('Mantree','Redapple',
                        'Devilsnose','Lilygap')")

# optional: subset with e.g. subsetProfiles()
f.sub <- f

# construct custom SDA queries to get more information about the above components
comp.q <- paste0("SELECT * FROM component 
                  WHERE cokey IN ", 
                    format_SQL_in_statement(site(f)$cokey), ";")
comp <- SDA_query(comp.q)

# get all horizon data corresponding to above components
hz.q <- paste0("SELECT * FROM chorizon 
                WHERE cokey IN ", 
                  format_SQL_in_statement(comp$cokey), 
                  " ORDER BY hzdept_r;")
hz <- SDA_query(hz.q) 

# get all aashto data corresponding to above horizons
aashto.q <- paste0("SELECT * FROM chaashto 
                     WHERE chkey IN ", 
                      format_SQL_in_statement(hz$chkey), 
                      "AND rvindicator = 'Yes';")
aashto <- SDA_query(aashto.q)

if(length(aashto)) {
  hz <- merge(hz, aashto[,c('aashtocl','chkey')], by="chkey")
} else {
# if there is no aashto data, fill in the aashtocl column so it is present (NA)
  hz$aashtocl <- NA  
}

# optional method for catching organic horizons that have GIN = NULL
# hz$aashtocl[grepl(hz$hzname, pattern="O")] <- "A-8"

# construct an SPC with the full SSURGO data from SDA
newspc <- hz
depths(newspc) <- cokey ~ hzdept_r + hzdepb_r

# calculate aashto index, take floor() for comparison with integer value in nasis
newspc$calc_aashind_r <- floor(ptf_aashind(
  newspc$sieveno200_r,
  newspc$ll_r,
  newspc$pi_r,
  newspc$aashtocl
))

# fit linear model to stored versus calculated
m0 <- lm(newspc$calc_aashind_r ~ newspc$aashind_r)
statz <- m0[[1]]

# ensure model intercept is 0 and slope is 1
all.eq <- all.equal.numeric(as.numeric(m0[[1]]), c(0,1))

# make a plot -- visually check for (lack of) fit
par(mar=c(5.5, 5.5, 5.5, 5.5))
plot(newspc$calc_aashind_r ~ newspc$aashind_r,
     xlab = "AASHTO Group Index Number\nStored (NASIS/SSURGO)",
     ylab = "AASHTO Group Index Number\nCalculated (R package)",
     xlim = c(0, max(newspc$aashind_r, na.rm = T)), 
     ylim = c(0, max(newspc$calc_aashind_r, na.rm = T)))
text(15,35, paste("m = ", m0$coefficients[2], "\n",
                  "b = ", m0$coefficients[1], "\n",
                  "1:1", all.eq))
legend("bottomright", legend = c("Model", "1:1"), 
       lwd = 2, lty = c(1,2), col = c("red","blue"))
abline(m0, col = "red", lwd = 2)
abline(0, 1, col = "blue", lwd = 2, lty = 2)
```
