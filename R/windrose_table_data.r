#' @name windrose_table_data
#' @title Extract data from windrose objec
#' @description Extract data from windrose objec
#' @usage windrose_table_data(data_rose)
#' @param windroseobj (required) a windrose object
#  @export


windrose_table_data=function(windroseobj) {

aa=na.omit(windroseobj$data)

res=list()

if ( windroseobj$dirres==22.5) {

  bb=rbind(tapply(aa$spd,aa$dir_binned,FUN = max),
         tapply(aa$spd,aa$dir_binned,FUN = mean),
         tapply(aa$spd,aa$dir_binned,FUN = median),
         tapply(aa$spd,aa$dir_binned,FUN = function(x) quantile(x,probs=c(0.95),na.rm=T)))
bb=as.data.frame(bb)
names(bb)=c("N","NNE","NE","ENE", "E","ESE", "SE","SSE", 
            "S","SSW", "SW","WSW", "W","WNW","NW","NNW")
res$table=bb
}  

if ( windroseobj$dirres==45) {
  
  bb=rbind(tapply(aa$spd,aa$dir_binned,FUN = max),
           tapply(aa$spd,aa$dir_binned,FUN = mean),
           tapply(aa$spd,aa$dir_binned,FUN = median),
           tapply(aa$spd,aa$dir_binned,FUN = function(x) quantile(x,probs=c(0.95),na.rm=T)))
  bb=as.data.frame(bb)
  names(bb)==c("N","NE", "E","SE","S","SW","W","NW")
  res$table=bb
  
  }              

bb=rbind(tapply(aa$spd,aa$dir_binned,FUN = max),
         tapply(aa$spd,aa$dir_binned,FUN = mean),
         tapply(aa$spd,aa$dir_binned,FUN = median),
         tapply(aa$spd,aa$dir_binned,FUN = function(x) quantile(x,probs=c(0.95),na.rm=T)))
bb=as.data.frame(bb)
res$table=bb
res$calm=data.frame(N_data=nrow(windroseobj$data),
                    Nmissing_data=windroseobj$missing_data,
                    frew_calms=windroseobj$calm_freq)

return(res)  

}
