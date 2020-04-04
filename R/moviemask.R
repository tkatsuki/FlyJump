#' Analyze escape response of flies
#'
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' moviemask()
#'

moviemask <- function(dir, file, mask, from, to, skip=1){
  filename <- paste0(dir, "/", file)
  intdir <- paste0(dir, "/", file, "_dir/")
  nfr <- dim(mask)[3]
  if(to - from < skip) skip=1
  for(i in 1:(nfr%/%skip)){
    writeImage(mask[,,i*skip], paste0(intdir, "tmpimgs/", formatC(i,width=4,flag="0"), ".png"))
  }
  cmd <- paste("ffmpeg -i ", intdir, "tmpimgs/%04d.png -q:v 1 -r 10 -pix_fmt yuv444p -y ", intdir, file, "_", from, "-", to, "_mask.mp4", sep="")
  system(cmd, ignore.stderr= T, show.output.on.console=F)
  unlink(paste0(intdir, "tmpimgs/*"))
}

