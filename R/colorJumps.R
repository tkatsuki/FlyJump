colorJumps <- function(dir, obj, x, y, bg, color= 'white', shape = 1, size=0.1){
  require(RImageBook)
  require(ggplot2)
  require(grid)
  w <- dim(bg)[1]
  h <- dim(bg)[2]
  df <- data.frame(obj = as.factor(obj), x = x, y = y)
  p1 <- ggplot(df, aes(x, y)) + 
    geom_point(colour = color, shape=shape, size=size) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(limits=c(0, w), expand=c(0,0)) +
    scale_y_reverse(limits=c(h, 0), expand=c(0,0)) +
    theme(line = element_blank(),
          text = element_blank(),
          title = element_blank(),
          legend.position="none",
          rect= element_blank(),
          plot.margin=unit(c(0,0,-1,-1),"lines"))
  filename <- paste0(dir, "tempsp.png")
  ggsave(plot=p1, filename = filename, width = w/300, height = h/300, bg = "black") 
  tracksp <- readImage(filename)
  trackspbl <- tracksp[,,1]>0|tracksp[,,2]>0|tracksp[,,3]>0
  tracksp <- Image(sweep(bg, 1:2, (1-trackspbl), "*")) + tracksp
  colorMode(tracksp) <- "Color"
  unlink(filename)
  tracksp
}