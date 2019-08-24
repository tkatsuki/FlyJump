#' Create a movie of an object
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' colorobjects()
#'

colorobjects <- function(dir, obj, x, y, bg, f, i=0){
  w <- dim(bg)[1]
  h <- dim(bg)[2]
  df <- data.frame(obj = obj, x = x, y = y, f = f)
  p1 <- ggplot(df, aes(x, y, colour=obj+1, group= as.factor(obj))) +
    annotate("text", x = w/2, y = h - 25, label = f, size= 1, colour = "yellow") +
    geom_path(linetype=1, lwd = 0.1) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(limits=c(0, w), expand=c(0,0)) +
    scale_y_reverse(limits=c(h, 0), expand=c(0,0)) +
    scale_colour_identity() +
    theme(line = element_blank(),
          text = element_blank(),
          title = element_blank(),
          legend.position="none",
          rect= element_blank(),
          plot.margin=unit(c(0,0,0,0),"lines"))
  filename <- paste0(dir, "tempsp", i, ".png")
  ggsave(filename = filename, width = w/300, height = h/300, bg = "black")
  tracksp <- readImage(filename)[,,1:3] # Remove alpha channel
  trackspbl <- tracksp[,,1]>0|tracksp[,,2]>0|tracksp[,,3]>0
  tracksp <- Image(sweep(bg, 1:2, (1-trackspbl), "*")) + tracksp
  colorMode(tracksp) <- "Color"
  unlink(filename)
  tracksp
}
