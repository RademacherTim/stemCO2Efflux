calcVPD <- function (
  ### calculate the water Vapour Pressure Deficit                   
  Ts        ##<< Surface temperature (deg Centrigrade)
  , Pa      ##<< Atmospheric pressure (Pa)
  , H2O     ##<< water Vapour Pressure (ppt)
){
  
  es <- 0.61078*exp(( 17.269*Ts)/(237.3+ Ts))*1000 # saturated water pressure [Pa]
  ea <- H2O/1000*Pa # Pa is atmospheric pressure [Pa]
                                     
  VPD <- (es - ea)/1000 
  VPD
  ### water Vapour Pressure Deficit in kPa
  
}
attr(calcVPD,"ex") <- function(){
  Ts <- 10
  Pa <- 970*100 #hPa -> Pa
  H2O <- 8 #ppt
  calcVPD( Ts, Pa, H2O )
}
