library(plumber)


#* @get /echo
function(msg = "default"){
  paste("You said",msg)
}