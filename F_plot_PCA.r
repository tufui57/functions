############################################################################################################
##   Plot environmental space
############################################################################################################


PCAplot <- function(background, # background or PCA score to plot
                    scores, # PCA score to plot
                    extent_x, extent_y, # extent to plot
                    col # Colour of points
                    ){
  
  
  pMain <- ggplot() +
    # plot all NZ data points
    geom_point(data = background, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of each sp
    geom_point(data = scores, aes(PC1, PC2), alpha = 0.1, color = col) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha = 0.7))) +
    # legend position inside plot
    theme(legend.justification = c(1, 1), legend.position = c(1, 1),
          panel.background = element_rect(fill = 'gray96')
    )
  
  return(pMain)
}

PCA_withHist <- function(scores, 
                         spname, # character string for title
                         histColour, # colour of bars
                         pMain, # result object of PCAplot()
                         save = TRUE
                         ){
  pTop <- ggplot(scores, aes(x = PC1)) +
    geom_histogram(data = scores, fill = histColour, alpha = 0.35) +
    xlim(extent_x) +
    xlab(expression(hot %<->% cold)) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(""),
          axis.ticks.x = element_blank()
    ) +
    ggtitle(paste("Environmental space of", spname)) +
    theme(panel.background = element_rect(fill = 'gray96')) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  pRight <- ggplot(scores, aes(x = PC2)) +
    geom_histogram(data = scores, fill = histColour, alpha = 0.35) +
    xlim(extent_y) +
    xlab(expression(dry %<->% wet)) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 270),
          axis.title.y = element_text(angle = 270),
          axis.ticks.y = element_blank()
    ) +
    coord_flip() +
    theme(panel.background = element_rect(fill = 'gray96')) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  pEmpty <- ggplot(scores, aes(PC1, PC2)) +
    geom_blank() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          line = element_blank(),
          panel.background = element_blank())
  
  if (save == TRUE) {
    png(filename = paste("Y:\\", spname, "_EnvSpace.png"), width = 900, height = 630)
    # change font size
    theme_set(theme_gray(base_size = 18))
    # Plot in multiple panels
    grid.arrange(pTop, pEmpty, pMain, pRight,
                 ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
    dev.off()
  } else {
    res <- list(pMain, pTop, pRight, pEmpty)
    return(res)
  }
}

