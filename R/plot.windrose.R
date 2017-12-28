#' @name plot
#' @title Plots a waverose or windrose object using ggplot2
#' @usage plot(data_rose)
#' @param data a data object of class windrose
#' @param palette (optional) one of the RColorBrewer ColorBrewer palettes
#' @param plot_theme (optional) a character string specifying a ggplot theme, e.g. "theme_minimal". Also supports ggthemes and xkcd.
#' @param t_legend (optional) a custom character string specifying a ggplot theme, e.g. "theme_minimal". Also supports ggthemes and xkcd.
#' @return returns a ggplot2 graph object
#' @import scales
#' @export
#' @seealso \code{ggplot2}, \code{ggtheme} and \code{brewer.pal}.

plot.windrose <-
function(data, x = NULL, y = NULL, 
                          palette = NULL, 
                          plot_theme = "theme_minimal", 
                          t_legend="Wind Speed / m s  ^ ~-1", 
                          ...) {
  
  # dirres
  # countmax
  # spd_colors
  
  if(plot_theme == "theme_grey") plot_theme <- "theme_gray"
  if(is.null(plot_theme)) plot_theme <- "theme_minimal"
  
  if(!is.null(palette)) {
    n_spd_seq <- length(data$spd_colors)
    if ("gray50" %in% data$spd_colors) {
      n_spd_seq <- n_spd_seq - 1
      add_gray <- TRUE
    } else
      add_gray <- FALSE
    n_colors_in_range <- n_spd_seq
    
    labels_plot=waiver();

    if ( data$dirres == 22.5) {labels_plot= c("N","NNE","NE","ENE", "E", 
                                "ESE", "SE","SSE", 
                                "S","SSW", "SW","WSW", "W", 
                                "WNW","NW","NNW")};

   if ( data$dirres == 8)    {labels_plot= c("N",,"NE", "E", 
                                    "SE","S","SW","W", 
                                    "NW")};

  
    # create the color map
    spd_colors <- colorRampPalette(brewer.pal(min(max(3,
                                                      n_colors_in_range),
                                                  min(9,
                                                      n_colors_in_range)),                                               
                                              palette))(n_colors_in_range)
    
    if(packageVersion("ggplot2") > "2.2"){    
    spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
   if (add_gray)
      spd_colors <- c(spd_colors, "gray50")
    
    data$spd_colors <- spd_colors
    rm(add_gray, n_spd_seq, n_colors_in_range, spd_colors)
  }
  
  
  p_windrose <- ggplot(data = na.omit(data$data),
                       aes(x = dir_binned,
                           fill = spd_binned)) +
    geom_bar() + 
    #    geom_bar(aes(y = border, width = 1), position = "stack",
    #             stat = "identity", fill = NA, colour = "white") +
    scale_x_discrete(drop = FALSE,
                     labels = label_plot) +
    coord_polar(start = -((data$dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = paste(as.character(t_legend)), 
                      values = data$spd_colors,
                      drop = FALSE)+
    scale_y_continuous(labels = scales::percent) +
    ylab("Frequency")
  
  
  # adjust axes if required
  if (!is.na(data$countmax)){
    p_windrose <- p_windrose +
      ylim(c(0,data$countmax))
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
  
  p_windrose <- p_windrose +
    theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
  # return the handle to the wind rose
  return(p_windrose)
}
