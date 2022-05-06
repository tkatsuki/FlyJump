#' Analyze escape response of flies
#'
#'
#' @param obj A target image of Image object or an array.
#' @param ref A reference image of Image object or an array.
#' @export
#' @examples
#' escapeAnalysis()
#'
#'

escapeAnalysis <- function(dir, file, bgfile=NA, bgstart=1, bgend=0, bgskip=100,
                           start=1, end=0, interval=0, large=300, maxdist=200, size=100, unit=1, fps=160,
                           maskmovie=T, speedmovie=T, objectmovie=T, moviejp=T, DLO=T, DLOonly=F, stimROI=c(220,240,220,240), ram=0,
                           gender=c("N", "FM", "MF", "S", "MM", "FF", "M", "F"), spthresh=50, thresh=0, useres=F,
                           timestamp=T){

  ## To do
  # fix data.table
  #

  intdir <- paste0(dir, "/", file, "_dir/")
  dir.create(paste0(dir, "/", file, "_dir"))
  dir.create(paste0(intdir, "tmpimgs"))
  gender <- match.arg(gender)
  ftrfiles <- list()
  ext <- substr(file, nchar(file)-2, nchar(file))

  # Welcome message
  ms1 <- paste0("Processing ", dir, "/", file, ".\n")
  cat(ms1)
  cat(ms1, file=paste0(intdir, file, "_messages.txt"))

  # Analyze only near the digital looming object?
  if(DLOonly==T | DLO==T){
    ms2 <- paste0("Looking for DLOs.\n")
    cat(ms2)
    cat(ms2, file=paste0(intdir, file, "_messages.txt"), append=T)
    samplesq <- readAVI(paste0(dir, "/", file), start, end, crop=stimROI)
    intprofileall <- apply(samplesq, 3, mean)
    rm(samplesq)
    intdiffall <- diff(intprofileall, lag=3)
    firstpeak <- which(intdiffall > 5)[1] + 3
    if(is.na(firstpeak)){
      ms5 <- "DLO was not found! Exiting."
      cat(ms5, sep="\n")
      cat(ms5, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
      png(file=paste0(intdir, file, "_intdiffall.png"))
      par(mar = c(5,4,4,5))
      plot(intdiffall, type="l", ylim=c(-2, 10), xlab="frame", ylab="Intensity change")
      dev.off()
      return(0)
    }
    intdiffmaxall <- max(intdiffall[(firstpeak-5):(firstpeak+5)])
    png(file=paste0(intdir, file, "_intdiffall.png"))
    par(mar = c(5,4,4,5))
    plot(intdiffall, type="l", ylim=c(-2, 10), xlab="frame", ylab="Intensity change")
    segments((firstpeak-5):(firstpeak+5), intdiffall[(firstpeak-5):(firstpeak+5)],
             (firstpeak-4):(firstpeak+6), intdiffall[(firstpeak-4):(firstpeak+6)],
             col = c("red"))
    dev.off()
    if(DLOonly==T){
      DLOlastfrall <- which(intdiffall==intdiffmaxall)
      ms3 <- paste0("Fist DLO was given at the ", DLOlastfrall, "th frame!")
      start <- DLOlastfrall - 500
      end <- DLOlastfrall + 500
      ms4 <- paste0("Will analyze ", start, " - ", end, ".")
      cat(c(ms3, ms4), sep="\n")
      cat(c(ms3, ms4), file=paste0(intdir, file, "_messages.txt"), sep="\n", append=T)
    }
  }

  # Create background image
  if(!is.na(bgfile)){
    ms6_1 <- "Loading a background image."
    cat(ms6_1, sep="\n")
    cat(ms6_1, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
    bg <- EBImage::readImage(bgfile)*2^8
    EBImage::writeImage(bg, file=paste0(intdir, file, "_bg.png"))
    if(ext=="avi"|ext=="AVI") {
      fn <- readAVI(paste0(dir, "/", file), getFrames=T)
    }
  } else {
  if(ext=="fmf"|ext=="FMF") {
    bg <- readFMF(paste0(dir, "/", file), bgstart, bgend, bgskip)
  }
  if(ext=="avi"|ext=="AVI") {
    fn <- readAVI(paste0(dir, "/", file), getFrames=T)
    if(bgend==0) bgend <- fn
    bgs <- readAVI(paste0(dir, "/", file), bgstart, bgstart)
    bge <- readAVI(paste0(dir, "/", file), bgend, bgend)
  }
  bgmax <- pmax(bgs, bge)
  bgbind <- array(NA, dim=c(dim(bgs)[1], dim(bgs)[2], 3))
  bgbind[,,1] <- bgs
  bgbind[,,2] <- bge
  bgbind[,,3] <- bgmax
  bg <- dipr::medianPrj(bgbind)
  EBImage::writeImage(bg/255, file=paste0(intdir, file, "_bg.png"))
  }

  # Automatic interval setting
  if(interval==0){
    if(.Platform$OS.type=="windows" & ram==0){
      ram <- memory.limit()
    } else if(.Platform$OS.type=="unix" & ram==0){
      ram <- system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)/1000
    }
    interval <- round(ram*0.1/(dim(bg)[1]*dim(bg)[2]/1000000), 0)
  }

  # Reset start and end
  if(end > fn|end <= 0) end <- fn
  if(start <= 0) start <- 1
  blocks <- (end - start)%/% interval
  lastblock <- (end - start)%% interval
  firstfr <- readAVI(paste0(dir, "/", file), start, start)[,,1]/255
  w <- dim(firstfr)[1]
  h <- dim(firstfr)[2]
  # Create a mask for the arena
  arenamask <- drawCircle(firstfr*0, h/2, w/2-5, 400, col=1, fill=T)

  # Skip image analysis if previous result should be used
  if(useres==F){
    # Process images one block at a time
    for(bn in 0:blocks){
      if(bn==blocks){
        from <- start+bn*interval
        to <- start+bn*interval+lastblock
      } else {
        from <- start+bn*interval
        to <- start+(bn+1)*interval-1
      }

      if(ext=="fmf") fly <- readFMF(paste0(dir, "/", file), from, to)
      if(ext=="avi") fly <- readAVI(paste0(dir, "/", file), from, to)

      ms7 <- "Subtracting background."
      cat(ms7, sep="\n")
      cat(ms7, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

      nobg <- -ssweep(fly, bg, "-")
      rm(fly)

      # Extract arena only
      nobg <- ssweep(nobg, arenamask, "*")

      # Remove embedded timestamp
      if(timestamp==T){
        nobg[1:10,1,] <- 0
      }

      # Automatic thresholding
      if(bn==0){
        sf <- seq(from=0, by=dim(nobg)[3]%/%20, length.out=21)[-1]
        writeImage(nobg[,,sf]/255, file=paste0(intdir, file, "_", start, "-", end, "_threshimg.tiff"))
        densitydata <- density(nobg[,,sf], bw=3)
        troughs <- which(diff(sign(diff(densitydata$y)))==+2)+1
        troughint <- densitydata$x[troughs[densitydata$y[troughs]<2e-04 & densitydata$y[troughs]>1e-06]]

        ms8 <- paste0("Calculating a threshold for binarization, intensity troughs are ",
                      paste(round(troughint, 1), collapse=", "))
        cat(ms8, sep="\n")
        cat(ms8, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

        threshbody <- round(tail(troughint, 1), 2)

        png(file=paste0(intdir, file, "_", start, "-", end, "_thresh.png"))
        par(mar = c(5,4,4,5))
        plot(density(nobg[,,sf], bw=3), ylim=c(0,0.0003))
        abline(v=troughint, col="blue")
        abline(v=threshbody, col="green")
        dev.off()
      }
      if(thresh!=0){
        threshbody <- thresh
      }
      ms9 <- paste0("Binarize with threshold = ", threshbody)
      cat(ms9, sep="\n")
      cat(ms9, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

      # Segmentation by global thresholding
      mask <- nobg > threshbody
      kern3 <- makeBrush(size=3, shape="diamond")
      mask <- opening(mask, kern3)
      mask <- bwlabel(mask)
      ftrs <- sfeatures(mask)
      dat <- unlist(lapply(ftrs, function(x) x[,'m.pxs']))
      if(length(which(!is.na(dat)))==0) {
        ms9_2 <- "No object detected"
        cat(ms9_2, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
        return("No object detected")
      }
      png(file=paste0(intdir, file, "_", from, "-", to, "_sizeprofile.png"))
      par(mar = c(5,4,4,5))
      plot(dat, ylim=c(0, max(dat)))
      dev.off()
      smallobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] < 40))
      largeobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] > large))
      largeobjfr <- which(sapply(largeobj, length)!=0)
      mask <- rmObjects(mask, smallobj)
      write(largeobjfr, file=paste0(intdir, file, "_", from, "-", to, "_largeobj.txt"))

      # Try to segment large objects by local thresholding
      if((length(largeobjfr)==1 && max(mask[,,largeobjfr]) < 2)
         || (length(largeobjfr)>1 && min(apply(mask[,,largeobjfr], 3, max)) < 2)){
        ms10 <- paste0("Objects larger than ", large, "px were detected. Applying local thresholding.")
        cat(ms10, sep="\n")
        cat(ms10, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

        locmask <- EBImage::thresh(normalize(nobg[,,largeobjfr]), 20, 20, 0.4)
        locmask <- bwlabel(locmask)
        ftrs <- sfeatures(locmask)
        smallobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] < 40))
        mask[,,largeobjfr] <- rmObjects(locmask, smallobj)
        largeobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] > large))
        largeobjfr <- largeobjfr[which(sapply(largeobj, length)!=0)]
        write(largeobjfr, file=paste0(intdir, file, "_", from, "-", to, "_largeobj2.txt"))
      }

      # If fused objects persist, use watershed segmentation
      if((length(largeobjfr)==1 && max(mask[,,largeobjfr]) < 2)
         || (length(largeobjfr)>1 && min(apply(mask[,,largeobjfr], 3, max)) < 2)){
        ms10_2 <- paste0("Objects larger than ", large, "px were detected. Applying watershed segmentation.")
        cat(ms10_2, sep="\n")
        cat(ms10_2, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

        distmapmask <- distmap(mask[,,largeobjfr])
        watershedmask <- watershed(distmapmask, ext = 7)
        ftrs <- sfeatures(watershedmask) # Needs 1> frames? Issue with video 101
        smallobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] < 40))
        mask[,,largeobjfr] <- rmObjects(watershedmask, smallobj)
        largeobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] > large))
        largeobjfr <- largeobjfr[which(sapply(largeobj, length)!=0)]
        write(largeobjfr, file=paste0(intdir, file, "_", from, "-", to, "_largeobj3.txt"))
      }

      # If fused objects persist, use voronoi-based segmentation
      if((length(largeobjfr)==1 && max(mask[,,largeobjfr]) < 2)
         || (length(largeobjfr)>1 && min(apply(mask[,,largeobjfr], 3, max)) < 2)){
        ms11 <- paste0("Objects larger than ", large, "px persisted. Applying voronoi segmentation.")
        cat(ms11, sep="\n")
        cat(ms11, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

        for (j in largeobjfr){
          if(j==1) break
          seedfr <- j - 1
          ftrs <- sfeatures(mask[,,seedfr])
          seedimg <- mask[,,seedfr]*0
          for(l in 1:nrow(ftrs[[1]])){
            seedimg[ftrs[[1]][l, 'm.x'], ftrs[[1]][l, 'm.y']] <- 1
          }
          seedimg <- bwlabel(seedimg)
          mask[,,j] <- propagate(mask[,,j], seedimg, mask[,,j])
        }
        ftrs <- sfeatures(mask)
        largeobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] > large))
        largeobjfr <- largeobjfr[which(sapply(largeobj, length)!=0)]
      }

      rm(nobg)

      # To remove small object
      ftrs <- sfeatures(mask)

      write(largeobjfr, file=paste0(intdir, file, "_", from, "-", to, "_largeobjFin.txt"))
      dat <- unlist(lapply(ftrs, function(x) x[,'m.pxs']))
      if(length(which(!is.na(dat)))==0) {
        ms12_2 <- "No object detected"
        cat(ms12_2, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
        return("No object detected")
      }
      png(file=paste0(intdir, file, "_", from, "-", to, "_sizeprofilefin.png"))
      par(mar = c(5,4,4,5))
      plot(dat, ylim=c(0, max(dat)))
      dev.off()
      saveRDS(ftrs, file=paste0(intdir, file, "_", from, "-", to, ".rds"))
      ftrfiles[bn+1] <- paste0(intdir, file, "_", from, "-", to, ".rds")

      # Generate an mp4 animation for the mask
      if(maskmovie==T){
        ms13 <- paste0("Creating a movie of the mask.")
        cat(ms13, sep="\n")
        cat(ms13, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
        moviemask(dir, file, mask, from=from, to=to, 5)
      }
      rm(mask)
    }

    # Concatenate all the feature list
    ftrslist <- list()
    for(i in 1:length(ftrfiles)){
      if(is.null(ftrfiles[[i]])) next
      ftrslist[[i]] <- readRDS(ftrfiles[[i]])
    }
    ftrs <- unlist(ftrslist, recursive=F)
    saveRDS(ftrs, file=paste0(intdir, file, "_", start, "-", end, "_ftrs.rds"))
    #ftrs <- readRDS(file=paste0(intdir, file, "_", start, "-", end, "_ftrs.rds"))

    # Run tracking function
    ms12 <- paste0("Running tracking function.")
    cat(ms12, sep="\n")
    cat(ms12, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
    res <- tracking(w=w, h=h, maxdist=maxdist, size=size, ftrs=ftrs, unit=unit, interval=1/fps)
    writeImage(res[[1]], file=paste0(intdir, file, "_", start, "-", end, ".png"))

    # Save results of tracking for future analysis
    saveRDS(res, file=paste0(intdir, file, "_", start, "-", end, ".rds"))

  } else {
    res <- readRDS(file=paste0(intdir, file, "_", start, "-", end, ".rds"))
  }

  # Overlay trajectories onto the first frame
  flycol <- rgbImage(firstfr, firstfr, firstfr)
  flyresbl <- res[[1]][,,1]>0|res[[1]][,,2]>0|res[[1]][,,3]>0
  flyres <- Image(sweep(flycol, 1:2, (1-flyresbl), "*")) + res[[1]][,,1:3] # Ignore alpha channel of the png
  colorMode(flyres) <- "Color"
  writeImage(flyres, file=paste0(intdir, file, "_", start, "-", end, "_overlay", ".png"))

  # Detect discontinuous trajectories
  objnum <- 1:max((res[[2]][,"obj"]))
  if (length(objnum) > 1){
    disconframe <- sapply(objnum, function(x) res[[2]][max(which(res[[2]][,"obj"]==x & !is.na(res[[2]][,"x"]))),"frame"])

    for(d in disconframe){
      if(d-50 <1){
        dstart <- 1
      }else {
          dstart <- d-50
        }
      if(d+50 > fn){
        dend <- fn
      }else{
        dend <- d+50
      }
      fly <- readAVI(paste0(dir, "/", file), dstart, dend)
      nobg <- -ssweep(fly, bg, "-")
      rm(fly)
      nobg <- ssweep(nobg, arenamask, "*")
      mask <- nobg > threshbody
      kern3 <- makeBrush(size=3, shape="diamond")
      mask <- opening(mask, kern3)
      mask <- bwlabel(mask)
      ftrs <- sfeatures(mask)
      png(file=paste0(intdir, file, "_", dstart, "-", dend, "_sizeprofile.png"))
      par(mar = c(5,4,4,5))
      dat <- unlist(lapply(ftrs, function(x) x[,'m.pxs']))
      plot(dat, ylim=c(0, max(dat)))
      dev.off()
      smallobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] < 40))
      largeobj <- lapply(ftrs, function(x) which(x[, 'm.pxs'] > large))
      largeobjfr <- which(sapply(largeobj, length)!=0)
      mask <- rmObjects(mask, smallobj)
      writeImage(mask, file=paste0(intdir, file, "_", dstart, "-", dend, "_mask.tiff"))
    }
  }

  # Detect jumps
  speedmat <- matrix(nrow=(end - start + 1), res[[2]][,'speed'])
  speeddiff <- diff(speedmat, lag=3)*fps/3
  highspeed <- which(speeddiff > spthresh*fps, arr.ind = T)
  if(length(highspeed)!=0){
    speedpeaks <- 1:nrow(highspeed)
    for(hsp in 1:nrow(highspeed)){
      speedpeaks[hsp] <- max(speeddiff[ifelse((highspeed[hsp,1]-80) < 1, 1, (highspeed[hsp,1]-80)):
                                         ifelse((highspeed[hsp,1]+80)>nrow(speeddiff), nrow(speeddiff), (highspeed[hsp,1]+80)),
                                       highspeed[hsp,2]], na.rm=T)
    }
    speedpeakpos <- which(matrix(speeddiff%in%unique(speedpeaks), dim(speeddiff)[1], dim(speeddiff)[2]), arr.ind=T)[,1]
    if(length(speedpeakpos)>1){
      for(p in 1:(length(speedpeakpos)-1)){
        if((speedpeakpos[p+1]-speedpeakpos[p])==1){
          speedpeakpos[p] <- speedpeakpos[p+1]
        }
      }
    }
    speedpeakpos <- unique(speedpeakpos)
  } else {
    speedpeakpos <- NULL
    ms19 <- paste0("No jumps were detected.")
    cat(ms19, sep="\n")
    cat(ms19, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
  }


  # Output intensity profile, motion speed, and jumps
  if(DLO==T){
    ## Plot
    png(file=paste0(intdir, file, "_", start, "-", end, "_intjumpprofile.png"), width=900, height=900)
    par(mar = c(5,4,4,5))
    matplot(-intdiffall[start:end], type="l", col="red", ylim=c(-10, 10), xlab="frame", ylab="Intensity change")
    par(new=T)
    matplot(speedmat, type="l", axes = F, xlab = NA, ylab = NA, ylim=c(0, 200))
    axis(side = 4)
    mtext("mm/sec", side=4, line=2)
    if(!is.null(speedpeakpos)) mtext(1:length(speedpeakpos), side=3, at=speedpeakpos)
    dev.off()
  } else {
    png(file=paste0(intdir, file, "_", start, "-", end, "_jumpprofile.png"), width=900, height=900)
    par(mar = c(5,4,4,5))
    matplot(speedmat, type="l", lty=1, ylim=c(0, 100), xlab="frame", ylab="speed (mm/sec)")
    if(!is.null(speedpeakpos)) mtext(1:length(speedpeakpos), side=3, at=speedpeakpos)
    dev.off()
    png(file=paste0(intdir, file, "_", start, "-", end, "_speeddiff.png"), width=900, height=900)
    par(mar = c(5,4,4,5))
    matplot(speeddiff, type="l", lty=1, ylim=c(-5000, 5000), xlab="frame", ylab="acceleration (mm/sec2)")
    if(!is.null(speedpeakpos)) mtext(1:length(speedpeakpos), side=3, at=speedpeakpos)
    dev.off()
  }

  # Detect jump regions
  if(is.null(speedpeakpos)){
    jumpfr <- NA
    jumps <- NA
  } else {
    jumpfr <- as.vector(sapply(speedpeakpos, function(x) seq(from=x, by=1, length.out=6)))
    jumps <- res[[2]][which(res[[2]][,c('frame')]%in%jumpfr), c('obj', 'x', 'y', 'speed', 'frame')]
  }

  # Detect single digital looming object
  if(DLOonly==T){
    samplesqDLO <- readAVI(paste0(dir, "/", file), start, end, crop=stimROI)
    intprofileDLO <- apply(samplesqDLO, 3, mean)
    rm(samplesqDLO)
    intdiffDLO <- diff(intprofileDLO, lag=3)
    intdiffDLOmax <- max(intdiffDLO)
    if(intdiffDLOmax > 2){
      DLOlastfr <- which(intdiffDLO==intdiffDLOmax)
      print(paste0("DLO was given at the ", DLOlastfr, "th frame from ", start, "!"))
      DLOframes <- which(res[[2]][,"frame"]%in%c((DLOlastfr-90):DLOlastfr))
      DLOflies <- res[[2]][DLOframes[!is.na(res[[2]][DLOframes,"speed"])], c("frame", "obj", "x", "y", "speed", "size")]
    } else {
      DLOlastfr <- NULL
      print(paste0("DLO was not detected."))
      DLOframes <- which(res[[2]][,"frame"]%in%c(start:end))
      DLOflies <- res[[2]][DLOframes[!is.na(res[[2]][DLOframes,"speed"])], c("frame", "obj", "x", "y", "speed", "size")]
    }
  } else {
    gender <- "N"
    DLOlastfr <- NULL
    print(paste0("DLO detection skipped."))
    DLOframes <- which(res[[2]][,"frame"]%in%c(start:end))
    DLOflies <- res[[2]][DLOframes[!is.na(res[[2]][DLOframes,"speed"])], c("frame", "obj", "x", "y", "speed", "size")]
  }

  # Gender detection
  # this is not going to work for copulation
  if(gender!="N"){
    ms14 <- paste0("Detecting gender.")
    cat(ms14, sep="\n")
    cat(ms14, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

    dt <- data.frame(res[[2]][DLOframes[!is.na(res[[2]][DLOframes,"speed"])], c("obj", "size")])
    DLOflies <- cbind(DLOflies, gender=NA)
    gen <- dt[, list(median=median(size)), by=obj]
    if(gender=="MF"|gender=="FM"){
      for(k in 1:length(gen)){
        if(gen[k]$median < 170) {
          DLOflies[DLOflies$obj==gen[k]$obj, "gender"] <- "M"
        } else {
          DLOflies[DLOflies$obj==gen[k]$obj, "gender"] <- "F"
        }
      }
    }
    if(gender=="MM"|gender=="FF"|gender=="S"){
      for(k in 1:length(gen)){
        if(gen[k]$median < 227) {
          DLOflies[DLOflies$obj==gen[k]$obj, "gender"] <- "M"
        } else {
          DLOflies[DLOflies$obj==gen[k]$obj, "gender"] <- "F"
        }
      }
    }
  }else {
    ms15 <- "Skipped gender detection."
    cat(ms15, sep="\n")
    cat(ms15, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")
    gen <- NULL
  }

  # Color-code speed
  obj <- res[[2]][!is.na(res[[2]][,'x']), "obj"]
  x <- res[[2]][!is.na(res[[2]][,'x']), "x"]
  y <- res[[2]][!is.na(res[[2]][,'x']), "y"]
  z <- res[[2]][!is.na(res[[2]][,'x']), "speed"]
  flysp <- colorspeed(intdir, obj, x, y, z, flycol, max=max(z, na.rm=T))
  writeImage(flysp, file=paste0(intdir, file, "_", start, "-", end, "_speed", ".png"))

  # Overlay DLO timing, fly speed, and jumps
  flyspDLO <- colorspeed(intdir, DLOflies[,2], DLOflies[,3], DLOflies[,4], 350, flysp, linetype=3, lwd=0.4)
  writeImage(flyspDLO, file=paste0(intdir, file, "_", start, "-", end, "_speedDLO", ".png"))
  if(length(jumps)!=0){
    flyspDLOjp <- colorJumps(intdir, jumps[,'obj'], jumps[,'x'], jumps[,'y'], flyspDLO, shape=1, size=0.2, color='white')
    writeImage(flyspDLOjp, file=paste0(intdir, file, "_", start, "-", end, "_speedDLOjp", ".png"))
  }

  # Generate an mp4 animation about motion speed
  if(speedmovie==T){
    ms16 <- paste0("Creating a movie of motion speed.")
    cat(ms16, sep="\n")
    cat(ms16, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

    moviespeed(dir, file, start, end, res, skip=10, tail=100)
    cmd <- paste0("ffmpeg -i ", intdir, "tmpimgs/%04d.png -q:v 1 -r 10 -pix_fmt yuv444p -y ", intdir, file, "_", start, "-", end, "_speed.mp4")
    system(cmd, ignore.stderr= T, show.output.on.console=F)
    unlink(paste0(intdir, "tmpimgs/*"))
  }

  # Generate an mp4 animation about object number
  if(objectmovie==T){
    ms17 <- paste0("Creating a movie of object number.")
    cat(ms17, sep="\n")
    cat(ms17, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

    movieobjects(dir, file, start, end, res, skip=10, tail=100)
    cmd <- paste0("ffmpeg -i ", intdir, "tmpimgs/%04d.png -q:v 1 -r 10 -pix_fmt yuv444p -y ", intdir, file, "_", start, "-", end, "_obj.mp4")
    system(cmd, ignore.stderr= T, show.output.on.console=F)
    unlink(paste0(intdir, "tmpimgs/*"))
  }

  # Generate an mp4 animation of each jump +- 160 frames
  if(moviejp==T){
    if(!is.null(speedpeakpos)){
      ms18 <- paste0("Creating a movie of each jump.")
      cat(ms18, sep="\n")
      cat(ms18, file=paste0(intdir, file, "_messages.txt"), append=T, sep="\n")

      for(jp in speedpeakpos){
        jpstart <- jp - 160
        jpend <- jp + 160
        if(jpend > fn|jpend <= 0) jpend <- fn
        if(jpstart <= 0) jpstart <- 1
        tmpfly <- readAVI(paste0(dir, "/", file), jpstart, jpend)/255
        moviemask(dir, file, tmpfly, jpstart, jpend, 1)
      }
    }
  }

  rm(res)
  if(DLOonly==F) DLOflies <- NULL
  reslist <- list(args=c(paste0("dir='", dir, "', file='", file, "', bgfile='", bgfile, "', bgstart=", bgstart, ", bgend=",
                                bgend, ", bgskip=", bgskip, ", start=", start, ", end=", end, ", interval=", interval,
                                ", large=", large, ", maxdist=", maxdist, ", size=", size, ", unit=", unit, ", fps=",
                                fps, ", maskmovie=", maskmovie, ", speedmovie=", speedmovie, ", objectmovie=",
                                objectmovie, ", moviejp=", moviejp, ", DLO=", DLO, ", DLOonly=", DLOonly, ", stimROI=", stimROI, "ram=", ram,
                                ", gender=", gender, ", spthresh=", spthresh, ", thresh=", thresh, ", useres=", useres, ", timestamp=", timestamp)),
                  DLO=DLOlastfr, DLOflies=DLOflies, jumps=jumps, jumpfr=speedpeakpos, jumpnum=length(speedpeakpos),
                  threshbody=threshbody, gen=gen)
  saveRDS(reslist, file=paste0(intdir, file, "_reslist", ".rds"))
  sink(file=paste0(intdir, file, "_reslist", ".txt"), type="output")
  print(reslist)
  sink()
}
