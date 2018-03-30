tracking3 <- function (rdir, mask = NULL, w = 0, h = 0, maxdist = 20, bin = 3, interval = 0.1, unit = 1, size = NULL, ftrs = NULL) {
  require(RImageBook)
  require(Rcpp)
  
  if (is.null(ftrs) & is.null(mask)) stop("Either a mask or ftrs need to be provided.")
  
  if (is.null(ftrs)){
    nf <- dim(mask)[3]
    w <- dim(mask)[1]
    h <- dim(mask)[2]
    ftrs <- sfeatures(rdir, mask) 
    rm(mask)
  } else if(is.null(mask)){
    if(w == 0 | h ==0) stop("Provide image size information")
    nf <- length(ftrs)
  } else {
    nf <- length(ftrs)
    w <- dim(mask)[1]
    h <- dim(mask)[2]
    rm(mask)
  }
  
  # Identify frames with objects
  objfr <- which(sapply(ftrs, length) != 0)
  
  ftrs[[objfr[1]]] <- cbind(ftrs[[objfr[1]]], Index = 1:nrow(ftrs[[objfr[1]]]), Frame = objfr[1])
  maxid <- 1

  if (is.null(size) == T) {
    for (i in 1:(length(objfr)-1)) {
      distance <- distmat(ftrs[[objfr[i]]][, "m.x"], ftrs[[objfr[i]]][,"m.y"], 
                          ftrs[[objfr[i + 1]]][, "m.x"], ftrs[[objfr[i + 1]]][,"m.y"])
      if(maxid < max(ftrs[[objfr[i]]][, "Index"])) maxid <- max(ftrs[[objfr[i]]][, "Index"])
      ftrs[[objfr[i + 1]]] <- cbind(ftrs[[objfr[i + 1]]], Index = (maxid + 1):(maxid + nrow(ftrs[[objfr[i + 1]]])))
      nindex <- which(distance == apply(distance, 1, min) & distance < maxdist, arr.ind = TRUE)
      ftrs[[objfr[i + 1]]][nindex[, "col"], "Index"] <- ftrs[[objfr[i]]][nindex[,"row"], "Index"]
      ftrs[[objfr[i + 1]]] <- cbind(ftrs[[objfr[i + 1]]], Frame = objfr[i + 1])
    }
  } else {
    for (i in 1:(length(objfr)-1)) {
      distance <- distmat(ftrs[[objfr[i]]][, "m.x"], ftrs[[objfr[i]]][,"m.y"], 
                          ftrs[[objfr[i + 1]]][, "m.x"], ftrs[[objfr[i + 1]]][,"m.y"])
      sizediff <- outer(ftrs[[objfr[i]]][, "m.pxs"], ftrs[[objfr[i + 1]]][, "m.pxs"], "-")
      if(maxid < max(ftrs[[objfr[i]]][, "Index"])) maxid <- max(ftrs[[objfr[i]]][, "Index"])
      ftrs[[objfr[i + 1]]] <- cbind(ftrs[[objfr[i + 1]]], Index = (maxid + 1):(maxid + nrow(ftrs[[objfr[i + 1]]])))
      nindex <- which(distance == apply(distance, 1, min) & 
                        distance < maxdist & abs(sizediff) < size, arr.ind = TRUE)
      ftrs[[objfr[i + 1]]][nindex[, "col"], "Index"] <- ftrs[[objfr[i]]][nindex[,"row"], "Index"]
      ftrs[[objfr[i + 1]]] <- cbind(ftrs[[objfr[i + 1]]], Frame = objfr[i + 1])
    }
  }
  ftrsm <- do.call(rbind, ftrs)
  xy <- ftrsm[sort.list(ftrsm[, "Index"]), ]
  xy <- cbind(xy[, "Frame"], xy[, "Index"], xy[, "m.x"], xy[,"m.y"], xy[,"m.pxs"])
  idb <- xy[, 2]
  from <- unique(idb)
  to <- seq_along(from)
  ida <- to[match(idb, from)]
  xy[, 2] <- ida
  ids <- nf * (xy[, 2] - 1) + xy[, 1]
  xmat <- matrix(NA, nf, max(ida))
  xmat[ids] <- xy[, 3]
  ymat <- matrix(NA, nf, max(ida))
  ymat[ids] <- xy[, 4]
  smat <- matrix(NA, nf, max(ida))
  smat[ids] <- xy[, 5]
  fmat <- matrix(NA, nf, max(ida))
  fmat[ids] <- xy[, 1]
  xy <- cbind(rep(unique(ida), each = nf), as.vector(xmat), 
              as.vector(ymat), as.vector(smat), as.vector(fmat))
  png(filename = "trackResult.png", width = w, height = h, bg = "black")
  par(plt = c(0, 1, 0, 1), xaxs = "i", yaxs = "i")
  matplot(xmat, ymat, type = "l", lty = 1, col = rainbow(16), 
          axes = F, xlim = c(0, w), ylim = c(0, h))
  dev.off()
  resultplot <- readImage("trackResult.png")
  resultplot <- flip(resultplot)
  unlink("trackResult.png")
  objid <- unique(xy[, 1])
  dist <- sapply(objid, function(x) c(trackDistance(xy[which(xy[, 1] == x), c(2, 3)]), NA))
  dispx <- sapply(objid, function(x) c(diff(xy[which(xy[, 1] == x), ][, 2], lag = bin), rep(NA, bin)))
  dispy <- sapply(objid, function(x) c(diff(xy[which(xy[, 1] == x), ][, 3], lag = bin), rep(NA, bin)))
  vx <- dispx/interval
  vy <- dispy/interval
  temp <- cbind(xy, as.vector(dist * unit), as.vector(dispx * unit), as.vector(dispy * unit), as.vector(dist * unit)/interval,as.vector(vx * unit), as.vector(vy * unit))
  ax <- sapply(objid, function(x) c(diff(temp[which(temp[,1] == x), ][, 8], lag = bin), rep(NA, bin)))
  ay <- sapply(objid, function(x) c(diff(temp[which(temp[,1] == x), ][, 9], lag = bin), rep(NA, bin)))
  as <- sapply(objid, function(x) c(angularVelocity(xy[which(xy[, 1] == x), c(2, 3)], bin = bin, int = interval), rep(NA, 2 * bin)))
  res <- cbind(temp, as.vector(ax/interval), as.vector(ay/interval), as.vector(as))
  res <- data.frame(obj = res[, 1], x = res[, 2], y = res[,3], distance = res[, 6], d.x = res[, 7], d.y = res[, 8],  speed = res[, 9], v.x = res[, 10], v.y = res[, 11], acc.x = res[, 12], acc.y = res[, 13], v.ang = res[, 14], size = res[, 4], frame = res[, 5])
  result <- list(resultplot, res)
  result
}