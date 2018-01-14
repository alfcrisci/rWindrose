#' @name windrose_table_data
#' @title Extract data from windrose objec
#' @description Extract data from windrose objec
#' @usage windrose_table_data(data_rose)
#' @param windroseobj (required) a windrose object
#  @export


windrose_table_data=function(windroseobj) {
aa=na.omit(windroseobj$data)
res=list()
b=rbind(tapply(aa$spd,aa$dir_binned,FUN = max),
        tapply(aa$spd,aa$dir_binned,FUN = mean),
        tapply(aa$spd,aa$dir_binned,FUN = median),
        tapply(aa$spd,aa$dir_binned,FUN = function(x) quantile(x,probs=c(0.95),na.rm=T)))
bb=as.data.frame(bb)
res$table=bb
bb$ALL=rbind(max(aa$spd,na.rm=T),
              mean(aa$spd,na.rm=T),
              median(aa$spd,na.rm=T),
              quantile(aa$spd,probs=c(0.95),na.rm=T))

if ( windroseobj$dirres==22.5) {

names(res$table)=c("N","NNE","NE","ENE", "E","ESE", "SE","SSE", 
            "S","SSW", "SW","WSW", "W","WNW","NW","NNW","ALL")
}  

if ( windroseobj$dirres==45) {
  

  names(res$table)==c("N","NE", "E","SE","S","SW","W","NW","ALL")
  
  }              

rownames(res$table)=c("Max","Mean","Median","Q95")

res$calm=data.frame(N_data=nrow(windroseobj$data),
                    Nmissing_data=windroseobj$missing_data,
                    frew_calms=windroseobj$calm_freq)

return(res)  

}
