moviemask <- function(dir, file, mask, skip=1){
  filename <- paste0(dir, "/", file)
  intdir <- paste0(dir, "/", file, "_dir/")
  nfr <- dim(mask)[3]
  
  for(i in 1:(nfr%/%skip)){
    writeImage(mask[,,i*skip], paste0(intdir, "tmpimgs/", formatC(i,width=4,flag="0"), ".png"))
  }
}
