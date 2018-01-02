#' @name plot
#' @title Plots a waverose or windrose object using ggplot2
#' @param data  data object of class windrose
#' @param palette RColorBrewer palettes
#' @param plot_theme character String specifying a ggplot theme, e.g. "theme_minimal". Also supports ggthemes and xkcd.
#' @param frequency_relative logical If the relative frequency of wind velocity classes is computed. Default is TRUE.
#' @param blanked logical If windrose annotations are eliminated. Default is FALSE.
#' @param no_legend logical  If legend is eliminated. Default is FALSE.
#' @param t_legend character Text of legend.
#' @return returns a ggplot2 graph object
#' @import ggthemes
#' @import RColorBrewer
#' @import grid
#' @export
#' @seealso \code{ggplot2}, \code{ggtheme} and \code{brewer.pal}.

plot.windrose <-
                function(data,
                         palette = "YlGnBu", 
                         plot_theme = "theme_minimal", 
                         t_legend="Wind Speed (m/s)",
                         frequency_relative=T,
                         blanked=F,
                          ...) {
 
   if(is.null(plot_theme)) plot_theme <- "theme_minimal"
  
    
    n_spd_seq <- length(data$spd_colors)
    
    if ("gray50" %in% data$spd_colors) {
      n_spd_seq <- n_spd_seq - 1
      add_gray <- TRUE
    } else
    { add_gray <- FALSE
      n_colors_in_range <- n_spd_seq
     }
 
    ##########################################
     # create the color map
    spd_colors <- colorRampPalette(brewer.pal(min(max(3,
                                                      n_colors_in_range),
                                                  min(9,
                                                      n_colors_in_range)),                                               
                                              palette))(n_colors_in_range)
    
    if(packageVersion("ggplot2") > "2.2") {    
    data$data$spd_binned =factor(data$data$spd_binned, levels = rev(levels(data$data$spd_binned)))
    data$spd_colors = rev(data$spd_colors) 
    }
    
   if (add_gray) {
                 spd_colors <- c(spd_colors, "gray50")
                 data$spd_colors <- spd_colors
                 rm(add_gray, n_spd_seq, n_colors_in_range, spd_colors)
   }
  
  if ( data$dirres == 45)    { label_x=c("N","NE", "E","SE","S","SW","W","NW") }
  if ( data$dirres == 22.5)  { label_x=c("N","NNE","NE","ENE", "E","ESE", "SE","SSE", 
                                         "S","SSW", "SW","WSW", "W","WNW","NW","NNW")}
   if (frequency_relative==F) {basis=ggplot(data = na.omit(data$data),
                                                   aes(x = dir_binned,
                                                       fill = spd_binned)
                                           )
                              }
   if (frequency_relative==T) {basis=ggplot(data = na.omit(data$data),
                                                   aes(x = dir_binned,
                                                       fill = spd_binned,
                                                       y = (..count..)/sum(..count..)
                                                      )
                                           )
                              }
  

   p_windrose <- basis +
                 geom_bar() + 
                 scale_x_discrete(drop = FALSE,
                                  labels = label_x) +
    coord_polar(start = -((data$dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = paste(as.character(t_legend)), 
                      values = data$spd_colors,
                      drop = FALSE)

   
  if ( data$dirres != 22.5 & data$dirres != 45 ) {
    blanked=T;
    
    p_windrose <-basis +
                 geom_bar() + 
                 scale_x_discrete(drop = FALSE,
                                  labels = waiver()) +
    coord_polar(start = -((data$dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = paste(as.character(t_legend)), 
                      values = data$spd_colors,
                      drop = FALSE)
 
  }
  
   
  # adjust axes if required
  if (!is.na(data$countmax)) {
                              p_windrose <- p_windrose + scale_y_reverse(limits = 0,data$countmax)+
                                                       ylab("Frequency")+
                                                       xlab("Sectors of wind provenance")
                              }
  if (frequency_relative==T) {
                              p_windrose <-p_windrose +scale_y_continuous(labels =  function(x){ paste0(100*x, "%")}) +
                                                       ylab("Relative frequency")+
                                                       xlab("Sectors of wind provenance")
                              }
  
  switch(EXPR = plot_theme,
         theme_gray = p_windrose <- p_windrose + theme_gray(),
         theme_bw = p_windrose <- p_windrose + theme_bw(),
         theme_linedraw = p_windrose <- p_windrose + theme_linedraw(),
         theme_light = p_windrose <- p_windrose + theme_light(),
         theme_minimal = p_windrose <- p_windrose + theme_minimal(),
         theme_classic = p_windrose <- p_windrose + theme_classic(),
         theme_xkcd = {if (xkcd_loaded) p_windrose <- p_windrose + ggthemes::theme_xkcd() else p_windrose <- p_windrose + theme_gray()},
         theme_economist = {if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_economist() else p_windrose <- p_windrose + theme_gray()},
         theme_excel = {if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_excel() else p_windrose <- p_windrose + theme_gray()},
         theme_few = {if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_few() else p_windrose <- p_windrose + theme_gray()},
         theme_solarized = {if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_solarized() else p_windrose <- p_windrose + theme_gray()},
         theme_tufte = {if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_tufte() else p_windrose <- p_windrose + theme_gray()},
         theme_wsj = {if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_wsj() else p_windrose <- p_windrose + theme_gray()},
         theme_gray())
  
  if ( blanked==T) {p_windrose <- p_windrose +theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) }
  if ( blanked==T) {p_windrose <-p_windrose + guides(fill=FALSE)}
  return(p_windrose)
}
