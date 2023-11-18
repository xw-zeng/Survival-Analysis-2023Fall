library(splines)
library(ggplot2)

pp = function(x, resid=TRUE, se=TRUE, df=4, nsmo=40, 
              var, xlab="Time", ylab="", lty=1:2, col=1, lwd=1,
              hr = FALSE, ...) {
  xx <- x$x
  yy <- x$y
  df <- max(df)     # in case df is a vector
  nvar <- ncol(yy)
  pred.x <- seq(from=min(xx), to=max(xx), length=nsmo)
  temp <- c(pred.x, xx)
  lmat <- ns(temp, df=df, intercept=TRUE)
  pmat <- lmat[1:nsmo,]       # for prediction
  xmat <- lmat[-(1:nsmo),]
  if (!is.logical(hr)) stop("hr parameter must be TRUE/FALSE")
  
  if (missing(ylab)) {
    if (hr)  ylab <- paste("HR(t) for", dimnames(yy)[[2]])
    else ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
  }       
  if (missing(var)) var <- 1:nvar
  else {
    if (is.character(var)) var <- match(var, dimnames(yy)[[2]])
    if  (any(is.na(var)) || max(var)>nvar || min(var) <1)
      stop("Invalid variable requested")
  }
  
  #
  # Figure out a 'good' set of x-axis labels.  Find 8 equally spaced
  #    values on the 'transformed' axis.  Then adjust until they correspond
  #    to rounded 'true time' values.  Avoid the edges of the x axis, or
  #    approx() may give a missing value
  if (x$transform == 'log') {
    xx <- exp(xx)
    pred.x <- exp(pred.x)
  }
  else if (x$transform != 'identity') {
    xtime <- x$time
    indx <- !duplicated(xx)  #avoid a warning message in R
    apr1  <- approx(xx[indx], xtime[indx], 
                    seq(min(xx), max(xx), length=17)[2*(1:8)])
    temp <- signif(apr1$y,2)
    apr2  <- approx(xtime[indx], xx[indx], temp)
    xaxisval <- apr2$y
    xaxislab <- rep("",8)
    for (i in 1:8) xaxislab[i] <- format(temp[i])
  }
  col <- rep(col, length=2)
  lwd <- rep(lwd, length=2)
  lty <- rep(lty, length=2)
  
  # Now, finally do the work
  for (i in var) {
    #   Since release 3.1-6, yy can have missing values.  If a covariate is
    # constant within a stratum then it's Shoenfeld residual is identially
    # zero for all observations in that stratum.  These "structural zeros"
    # are marked with an NA.  They contain no information and should not
    # by plotted.  Thus we need to do the spline fit one stratum at a time.
    y <- yy[,i]
    keep <- !is.na(y)
    if (!all(keep)) y <- y[keep]
    
    qmat <- qr(xmat[keep,])
    if (qmat$rank < df) {
      warning("spline fit is singular, variable ", i, " skipped")
      next
    } 
    
    yhat <- pmat %*% qr.coef(qmat, y)
    if (resid) yr <-range(yhat, y)
    else       yr <-range(yhat)
    
    if (se) {
      bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
      xtx <- bk %*% t(bk)
      seval <- ((pmat%*% xtx) *pmat) %*% rep(1, df)
      temp <- qnorm(0.95) * sqrt(x$var[i,i]*seval)
      yup <- yhat + temp
      ylow<- yhat - temp
    }
  }
  return(list(yr=yr, yhat=yhat, yup=yup, ylow=ylow))
}

plot_zph = function (fit, resid = TRUE, se = TRUE, df = 4, nsmo = 40, var, 
                point.col = "red", point.size = 1.2, point.shape = 19, point.alpha = 1, 
                line.col = "black", caption = NULL, ggtheme = theme_survminer(), ...) 
{
  x <- fit
  if (!methods::is(x, "cox.zph")) 
    stop("Can't handle an object of class ", class(x))
  xx <- x$x
  yy <- x$y
  d <- nrow(yy)
  df <- max(df)
  nvar <- ncol(yy)
  pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
  temp <- c(pred.x, xx)
  lmat <- splines::ns(temp, df = df, intercept = TRUE)
  pmat <- lmat[1:nsmo, ]
  xmat <- lmat[-(1:nsmo), ]
  qmat <- qr(xmat)
  if (qmat$rank < df) 
    stop("Spline fit is singular, try a smaller degrees of freedom")
  if (se) {
    bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
    xtx <- bk %*% t(bk)
    seval <- d * ((pmat %*% xtx) * pmat) %*% rep(1, df)
  }
  ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
  if (missing(var)) 
    var <- 1:nvar
  else {
    if (is.character(var)) 
      var <- match(var, dimnames(yy)[[2]])
    if (any(is.na(var)) || max(var) > nvar || min(var) < 
        1) 
      stop("Invalid variable requested")
  }
  if (x$transform == "log") {
    xx <- exp(xx)
    pred.x <- exp(pred.x)
  }
  else if (x$transform != "identity") {
    xtime <- as.numeric(dimnames(yy)[[1]])
    indx <- !duplicated(xx)
    apr1 <- approx(xx[indx], xtime[indx], seq(min(xx), max(xx), 
                                              length = 17)[2 * (1:8)])
    temp <- signif(apr1$y, 2)
    apr2 <- approx(xtime[indx], xx[indx], temp)
    xaxisval <- apr2$y
    xaxislab <- rep("", 8)
    for (i in 1:8) xaxislab[i] <- format(temp[i])
  }
  plots <- list()
  plots <- lapply(var, function(i) {
    invisible(pval <- round(x$table[i, 3], 4))
    gplot <- ggplot() + labs(subtitle = paste0("Schoenfeld Individual Test p: ", 
                                            pval)) + ggtheme
    y <- yy[, i]
    pppp <- pp(fit[i])
    yr <- pppp$yr
    yup <- pppp$yup
    ylow <- pppp$ylow
    yhat <- pppp$yhat
    if (se) {
      yr <- range(yr, yup, ylow)
    }
    if (x$transform == "identity") {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + ylim(yr)
    }
    else if (x$transform == "log") {
      gplot <- gplot + geom_line(aes(x = log(pred.x), y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + ylim(yr)
    }
    else {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yhat), col = line.col) + 
        xlab("Time") + ylab(ylab[i]) + scale_x_continuous(breaks = xaxisval, 
                                                          labels = xaxislab) + ylim(yr)
    }
    if (resid) 
      gplot <- gplot + geom_point(aes(x = xx, y = y), col = point.col, 
                                  shape = point.shape, na.rm=T, 
                                  size = point.size, alpha = point.alpha)
    if (se) {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yup),
                                 lty = "dashed", col = line.col) + 
        geom_line(aes(x = pred.x, y = ylow),
                  lty = "dashed", col = line.col)
    }
    ggpubr::ggpar(gplot, ...)
  })
  names(plots) <- var
  class(plots) <- c("ggcoxzph", "ggsurv", "list")
  if ("GLOBAL" %in% rownames(x$table)) 
    global_p <- x$table["GLOBAL", 3]
  else global_p <- NULL
  attr(plots, "global_pval") <- global_p
  attr(plots, "caption") <- caption
  
  return(plots)
}
