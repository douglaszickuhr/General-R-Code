library(plumber)
# rnorm.R

#* Return a random array
#* @param n - Number of records to generate
#* @param mean - Mean value
#* @get /rnorm
function(n, mean = 1) {
  rnorm(n,as.numeric(mean))
}

