
model5P <- function(x, y, T0=NA, Ctrl=NA, useProp=FALSE, sDev=NULL, LPweight=0.25,
                    npars="all", method=c("sdw", "res", "Y2", "pw", "gw"),...){

#   T0=NA; Ctrl=NA; useProp=FALSE; sDev=NULL; LPweight=0.25;
#   npars="all"; method="sdw"
  
  if(is.numeric(npars) & (npars<2 | npars>5))
    stop("\nThe number of parameters (npars) has to be in [2, 5]!\n
         Choose 'all' to test both.\n")
  
  if(any(is.na(x) | is.na(y))){
    NAs <- union(which(is.na(x)), which(is.na(y)))
    x <- x[-NAs]
    y <- y[-NAs]
  }
  object <- cellResp(x=x, y=y, LPweight=LPweight)
  if(useProp) object@yProp <- .survProp(y, T0, Ctrl)
  else object@yProp <- y
  
  weights <- rep(1, length(y))
  method <- match.arg(method)
  .sce <- .chooseSCE(method)
#   switch(method,
#          sdw = {.sce <- .sdWeight},
#          res = {.sce <- .wsqRes},
#          Y2 = {.sce <- .Y2},
#          pw = {.sce <- .poissonWeight},
#          gw = {.sce <- .generalWeight}
#   )
  
#  test <- nlm(f=.sdWeight, p=.initPars(x, y, 2), x=x, yobs=y, Weights=weights, wcoef=LPweight, .PL2)
  
  if(npars=="all"){
#    cat("Testing pars\n")
#     err <- sapply(1, function(k){
#       test2 <- try(nlm(f=.sce, p=.initPars(x, y, 2), x=x, yobs=y, Weights=weights, wcoef=LPweight, .PL2), silent=TRUE)
#       test3 <- try(nlm(f=.sce, p=.initPars(x, y, 3), x=x, yobs=y, Weights=weights, wcoef=LPweight, .PL3), silent=TRUE)
#       test4 <- try(nlm(f=.sce, p=.initPars(x, y, 4), x=x, yobs=y, Weights=weights, wcoef=LPweight, .PL4), silent=TRUE)
#       test5 <- try(nlm(f=.sce, p=.initPars(x, y, 5), x=x, yobs=y, Weights=weights, wcoef=LPweight, .PL5), silent=TRUE)
#       scores <- sapply(list(test2, test3, test4, test5), function(t){
#         if(class(t)!="try-error") return(t$minimum)
#         else return(Inf) 
#       })
#       return(scores)
#     })
    npars <- .testAll(.sce, x, y, weights, LPweight)
    cat(sprintf("%s-Parameters model seems to have better performances.\n", npars))
  }
  
  PL <- .chooseModel(npars)
  best <- nlm(f=.sce, p=.initPars(x, y, npars), x=x, yobs=y, Weights=weights, wcoef=LPweight, PL)
  
  # Best estimates
  bottom <- best$estimate[1]
  top <- best$estimate[2]
  xmid<-best$estimate[3]
  scal <- best$estimate[4]
  s <- best$estimate[5]
  
  # Estimation des valeurs
  newX <- seq(min(x), max(x), length=200)
  newY <- PL(bottom, top, xmid, scal, s, newX)
  yFit <- PL(bottom, top, xmid, scal, s, x)
  perf <- .getPerf(y, yFit)
  
#   # coordinates of inflexion point
#   Xflex = xmid + (1/scal)*log10(s)
#   Yflex = bottom + (top - bottom)*(s/(s+1))^s
#   
#   # coordinates of rep = 0.5
#   Y50 = 0.5*(top + bottom)
#   X50 = xmid - 1/scal*log10(((top - bottom)/(Y50 - bottom))^(1/s)-1)
#   
#   # Slope at the inflexion point
#   B = (top - bottom)*log(10)*(scal)*(s/(s+1))^(s+1)  # + bottom   						# 5P	 	pour d=1, (d/(d+1))^(d+1) = 1/4
#   A = Yflex  - B*(Xflex)																					# 5P		pour d=1, (d/(d+1))^d = 1/2
  
  # Compute simulations to estimate the IC50 conf. interval
  pars <- cbind.data.frame(bottom=bottom, top=top, xmid=xmid, scal=scal, s=s)
  targets <- seq(.1, .9, by = .1)
  estimates <- lapply(targets, function(target){.estimateRange(target, perf$stdErr, pars, 1e4)})
  estimates <- cbind.data.frame(Resp = targets, do.call(rbind, estimates))
  colnames(estimates) <- c('Surv', 'Dmin', 'D', 'Dmax')
  
  object@npars <- npars
  object@pars <- pars
  object@yFit <- yFit
  object@xCurve <- newX
  object@yCurve <- newY
  object@goodness <- perf$goodness
  object@stdErr <- perf$stdErr
  object@estimates <- estimates
  object@AUC <- data.frame(trapezoide = .AUC(newX, newY), Simpson = .Simpson(newX, newY))
  object@PL <- PL
  object@SCE <- .sce

  return(object)
#   # Visualization
#   if(Plot){
#     plot(y ~ x, type = 'n',...)
#     pas = (max(x) - min(x))/20
#     if(!is.null(sDev))
#       for(s in 1:length(sDev)){
#         segments(x0 = x[s], x1 = x[s], y0 = y[s]-sDev[s], y1 = y[s]+sDev[s], lty = 3)
#         segments(x0 = x[s]-pas, x1 = x[s]+pas, y0 = y[s]-sDev[s], y1 = y[s]-sDev[s], lty = 3)
#         segments(x0 = x[s]-pas, x1 = x[s]+pas, y0 = y[s]+sDev[s], y1 = y[s]+sDev[s], lty = 3)
#       }
#     if(addPoints) {points(x, y, pch=19, col = pCol,...); points(x, y, cex=1.2)}
#     if(addLine) lines(newY ~ newX, col = lCol, lwd=3,...)
#     if(addXinf) {abline(A, B, col = 'blue', lwd = 2); points(-A/B, 0, pch = 19, col = 'purple')}
# {
#   if(wcoef==0) Sub <- sprintf("Non weighted %sP logistic regr.", npars)
#   else Sub <- sprintf("Weighted %sP logistic regr.", npars)
# }
#     title (sub = Sub)
#   }
#   return(list(optimErr=best$minimum, pars=best$estimate, goodness=perf$goodness, err=perf$err, p=perf$p,
#               fit=yfit, newX=newX, newY=newY,
#               xflex=Xflex, yflex=Yflex, x50=X50, y50=Y50,
#               slope=B, yIntercept=A, xInfl=-A/B))
}
