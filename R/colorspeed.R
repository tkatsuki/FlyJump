#' Color trajectory of an object by speed
#'
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' colorspeed()
#'

colorspeed <- function(dir, obj, x, y, z, bg, min=0, max=350, f="", i=0, linetype=1, lwd=0.1){
  w <- dim(bg)[1]
  h <- dim(bg)[2]
  df <- data.frame(obj = as.factor(obj), x = x, y = y, z = z)
  p1 <- ggplot(df, aes(x, y, colour=z, group=obj)) +
    annotate("text", x = w/2, y = h - 25, label = f, size= 1, colour = "yellow") +
    geom_path(linetype=linetype, lwd = lwd) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(limits=c(0, w), expand=c(0,0)) +
    scale_y_reverse(limits=c(h, 0), expand=c(0,0)) +
    scale_colour_gradientn(limits=c(min, max), colours = colorRampPalette(c("blue", "green", "yellow", "red"))(20)) +
    theme(line = element_blank(),
          text = element_blank(),
          title = element_blank(),
          legend.position="none",
          rect= element_blank(),
          plot.margin=unit(c(0,0,-1,-1),"lines"))
  filename <- paste0(dir, "tempsp", i, ".png")
  ggsave(plot=p1, filename = filename, width = w/300, height = h/300, bg = "black")
  tracksp <- readImage(filename)
  trackspbl <- tracksp[,,1]>0|tracksp[,,2]>0|tracksp[,,3]>0
  tracksp <- Image(sweep(bg, 1:2, (1-trackspbl), "*")) + tracksp
  colorMode(tracksp) <- "Color"
  unlink(filename)
  tracksp
}
