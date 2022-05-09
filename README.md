# FlyJump


Fly jump detection

If you are using Windows install Rtools following the instructions described here: https://cran.r-project.org/bin/windows/Rtools/

You also need ffmpeg installed on your system. Follow https://www.wikihow.com/Install-FFmpeg-on-Windows

```
install.packages(c("devtools"))
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")
library(devtools)
devtools::install_github("tkatsuki/dipr")
devtools::install_github("tkatsuki/FlyJump")
library(dipr)
library(EBImage)
library(FlyJump)
```

`dir <- "/Users/specify/directory/containing/mp4files/"` # change this. Don't forget / at the end.

`bgfile="/Users/takeokatsuki/bg.png"` # change this. Use this if the fly does not move much in the video. To create a bg.png file first run the following code with a video in which 
flies move so that median filter creates a background. Then look for bg.png file in the output folder.

```
filelist <- list.files(path=dir, full.names=F, pattern=".mp4$")
for(i in 1:length(filelist)){
filename <- substr(filelist[i], 1, nchar(filelist[i])-4)
com <- paste0("ffmpeg -i ", dir, filelist[i], " -pix_fmt gray -vcodec rawvideo -y ", dir, filename, ".avi")
system.time(system(com))
file <- paste0(filename, ".avi")
try(escapeAnalysis(dir = dir, file = file, bgfile = NA, bgstart=1, bgend=0, bgskip=100, start=1, end=0, interval=600, large=600, maxdist=200, size=200, unit=0.1, fps=200, maskmovie=T, speedmovie=T, objectmovie=T, moviejp=T, DLO=T, DLOonly=F, stimROI = c(700, 720, 700, 720), ram=0, gender= "N", spthresh=50, thresh=0, useres=F, timestamp=T))
unlink(paste0(dir, file))
}
```
