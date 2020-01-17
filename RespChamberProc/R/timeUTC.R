as.POSIXctUTC <- function(
  ### construct a time in timezone UTC and set tzone attribute to UTC for correct printing
  x    ##<< An object to be converted.
){
  structure(as.POSIXct(x, tz="UTC"), tzone="UTC")
}
attr(as.POSIXctUTC,"ex") <- function(){
  #tmp1 <- as.POSIXct(c("2014-06-23 00:00:01","2014-06-23 05:00:01"))
  #plot(tmp1)    
  tmp <- as.POSIXctUTC(c("2014-06-23 00:00:01","2014-06-23 05:00:01"))
  print(tmp)
  plot(tmp)    # note that it is not converted to local time e.g. CET, but ggplot respects timezone attribute
}
