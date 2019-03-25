#' Color trajectory of an object that jumped
#'
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' mp4toAVIBatch()
#'
#'

mp4toAVIBatch <- function(dir){
  filelist <- list.files(path=dir, full.names=T, pattern=".mp4$")
  for(i in 1:length(filelist)){
    filename <- substr(filelist[i], 1, nchar(filelist[i])-4)
    com <- paste0("ffmpeg -i ", filelist[i], " -pix_fmt gray -vcodec rawvideo -y ", filename, ".avi")
    system.time(system(com))
  }
}
