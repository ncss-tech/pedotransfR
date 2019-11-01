library(RODBC)

# temporary until we get the package built
source('getAndCacheCalculation.R')


## save local copies of calculation code

getAndCacheCalculation(
  calcName = '15 AASHTO Group Index', 
  basePath = 'NASIS_calculations/'
)


getAndCacheCalculation(
  calcName = '14 Liquid Limit and PI', 
  basePath = 'NASIS_calculations/'
)

getAndCacheCalculation(
  calcName = 'HSG', 
  basePath = 'NASIS_calculations/'
)

