dir <- "/Users/takeokatsuki/Desktop/Male/"
filelist <- list.files(path=dir, full.names=T, pattern=".mp4$")
for(i in length(filelist)){
  filename <- substr(filelist[i], 1, nchar(filelist[i])-4)
  com <- paste0("ffmpeg -i ", filelist[i], " -pix_fmt gray -vcodec rawvideo -y ", filename, ".avi")
  system.time(system(com))
}
