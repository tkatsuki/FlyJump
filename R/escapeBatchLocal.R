rdir <<- "C:/Users/GreenspanLab/Dropbox/R/"
setwd("E:/presentation/")
rdir <<- "C:/Users/GreenspanLab/Dropbox/R/"
source("C:/Users/GreenspanLab/Dropbox/R/escapeAnalysis2015-04-09.R")
source("C:/Users/GreenspanLab/Dropbox/R/readAVI8.R")
library(methods)
bgstart <- 1
bgend <- 0
bgskip <- 10
start <- 1
end <- 0
large <- 3000
interval <- 1000
maxdist <- 100
size <- 100
unit <- 0.0875
fps <- 160
maskmovie <- F
speedmovie <- T
objectmovie <- F
moviejp <- T
maskmoviejp <- F
DLO <- T
DLOonly <- T
ram <- 0
gender <- "N"
spthresh <- 3000
thresh <- 65
useres <- T  
dir <- "."
filelist <- list.files(dir, pattern=".avi$")
i <- filelist[12]
for(i in filelist[27]){
  ptm <- proc.time() 
  filename <- substr(i, 1, nchar(i)-4)
  com <- paste0("ffmpeg -i ", dir, "/", i, " -pix_fmt gray -vcodec rawvideo -y ", dir, "/", filename, ".avi")
  system(com)
  avifile <- paste0(filename, ".avi")
  file <- avifile
  escapeAnalysis(dir, avifile, bgstart=bgstart, bgend=bgend, start=start, end=end, interval=interval, maxdist=maxdist, 
                 size=size, unit=unit, fps=fps, large=large, maskmovie=maskmovie, speedmovie=speedmovie, objectmovie=objectmovie,
                 moviejp=moviejp, maskmoviejp=maskmoviejp, DLO=DLO, DLOonly=DLOonly, ram=ram, gender=gender, spthresh=spthresh, thresh=thresh, useres=useres)
  unlink(paste0(dir, "/", avifile))
  print(proc.time() - ptm)
  detach("package:ggplot2", unload=TRUE)
  unloadNamespace("data.table")
  unloadNamespace("reshape2")
  unloadNamespace("scales")
  unloadNamespace("plyr")
}