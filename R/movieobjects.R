#' Create a movie of an object
#'
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' movieobjects()
#'

movieobjects <- function(dir, file, start, end, trackres, skip=10, tail=100){
  filename <- paste0(dir, "/", file)
  intdir <- paste0(dir, "/", file, "_dir/")
  nfr <- end - start + 1
  nobj <- max(unique(trackres[[2]][,'obj']))
  ext <- file_ext(filename)

  for(i in 1:(nfr/skip - skip)){
    fr <- start + skip*(i-1) + tail
    rg <- rep(c(rep(FALSE, skip*(i-1)), rep(TRUE, tail), rep(FALSE, nfr - tail - skip*(i-1))), nobj)
    if(ext=="fmf"|ext=="FMF") bgimg <- readFMF(filename, fr, fr)[,,1]/255
    if(ext=="avi"|ext=="AVI") bgimg <- readAVI(filename, fr, fr, silent=T)[,,1]/255
    bgimg <- rgbImage(bgimg, bgimg, bgimg)
    subres <- trackres[[2]][rg,]
    obj <- subres[!is.na(subres[,'x']), "obj"]
    if(length(obj)==0) next
    x <- subres[!is.na(subres[,'x']), "x"]
    y <- subres[!is.na(subres[,'x']), "y"]
    flysp <- colorobjects(intdir, obj, x, y, bgimg, fr, i=i)
    writeImage(flysp, paste0(intdir, "tmpimgs/", formatC(i,width=4,flag="0"), ".png"))
  }
}
