# # Define class
setClass('cellResp', representation(x='vector',
                                    y='vector',
                                    isLog='logical',
                                    yProp='vector',
                                    npars='numeric',
                                    LPweight='numeric',
                                    yFit='vector',
                                    xCurve='vector',
                                    yCurve='vector',
                                    goodness='numeric',
                                    stdErr='numeric',
                                    pars='data.frame',
                                    estimates='data.frame',
                                    AUC='data.frame',
                                    PL='ANY',
                                    SCE='ANY'))

# # Constructor
cellResp = function(x=x, y=y, isLog=TRUE, yProp=NA, npars=0, LPweight=0, yFit=NA,
                    xCurve=NA, yCurve=NA, goodness=0, stdErr=0, pars=data.frame(),
                    estimates=data.frame(), AUC=data.frame(), PL=NULL, SCE=NULL){
  new('cellResp', x=x, y=y, isLog=isLog, yProp=yProp, npars=npars, LPweight=LPweight,
      yFit=yFit, xCurve=xCurve, yCurve=yCurve, goodness=goodness, stdErr=stdErr,
      pars=pars, estimates=estimates, AUC = AUC, PL=PL, SCE=SCE)
}

# # setGenerics
setGeneric("getX", function(object) standardGeneric("getX"))
setGeneric("getY", function(object) standardGeneric("getY"))
setGeneric("getYProp", function(object) standardGeneric("getYProp"))
setGeneric("getFitValues", function(object) standardGeneric("getFitValues"))
setGeneric("getXcurve", function(object) standardGeneric("getXcurve"))
setGeneric("getYcurve", function(object) standardGeneric("getYcurve"))
setGeneric("getPar", function(object) standardGeneric("getPar"))
setGeneric("getGoodness", function(object) standardGeneric("getGoodness"))
setGeneric("getStdErr", function(object) standardGeneric("getStdErr"))
setGeneric("getEstimates", function(object) standardGeneric("getEstimates"))
setGeneric("getAUC", function(object) standardGeneric("getAUC"))
#setGeneric("show", valueClass="cellResp", function(object) standardGeneric("show"))

# # Methods
setMethod("getX", "cellResp", function(object) return(object@x))
setMethod("getY", "cellResp", function(object) return(object@y))
setMethod("getYProp", "cellResp", function(object) return(object@yProp))
setMethod("getFitValues", "cellResp", function(object) return(object@yFit))
setMethod("getXcurve", "cellResp", function(object) return(object@xCurve))
setMethod("getYcurve", "cellResp", function(object) return(object@yCurve))
setMethod("getPar", "cellResp", function(object){return(list(npar=object@npars, params=object@pars))})
setMethod('getGoodness', 'cellResp', function(object) return(object@goodness))
setMethod('getStdErr', 'cellResp', function(object) return(object@stdErr))
setMethod('getEstimates', 'cellResp', function(object){
  estim <- object@estimates
  return(estim[order(estim$Surv, decreasing = TRUE),])
  })
setMethod("getAUC", "cellResp", function(object) return(object@AUC))
setMethod("predict", "cellResp", function(object, target){
  if(target<0 | target>1)
    stop("The target value has to be between 0 and 1 (fraction of y)")
  pars <- getPar(object)
  estim <- .estimateRange(target, getStdErr(object), pars$params, 1e4, object@isLog)
  estim <- as.data.frame(t(estim))
  colnames(estim) <- c('xmin', 'x', 'xmax')
  return(estim)
})
setMethod("plot", signature = "cellResp",
          function(object, x=NA, y=NA, pcol="aquamarine1", lcol="red3", cex=1.5,
                   showTarget=.5, showIC=TRUE, B=1e4, unit='',
                   Title=NA, xlab='Log10(Drug[c])', ylab='Survival',...){
            op <- par(no.readonly = TRUE)
            par(las = 1, cex.axis = 1.5, cex.lab = 1.75, mar = c(6.5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
            x <- getX(object)
            y <- getY(object)
            newx <- getXcurve(object)
            newy <- getYcurve(object)
            my <- as.numeric(by(y, x, mean, na.rm=TRUE))
            mx <- unique(x)
            r2adj <- round(getGoodness(object), 3)
            plot(x, y, col=pcol, cex=cex, pch=19, #ylim=range(min(newy, 0)-.05, max(newy, 1)+.05)*1.2,
                 xlab=xlab, ylab=ylab,...)
            points(x, y, pch = 1, cex = cex)
            legend(ifelse(newy[length(newy)]<newx[length(newx)], 'topright', 'bottomright'),
                   legend = paste('Goodness of fit:', r2adj), bty = 'n', cex = 1.5)
            
            if(!is.na(showTarget)){
              stdErr <- getStdErr(object)
              estim <- .estimateRange(showTarget, stdErr, getPar(object)$params, B, object@isLog)
              legend1 <- sprintf("IC%d : %s%s", showTarget*100, format(estim[2], scientific=TRUE), unit)
              legend2 <- sprintf("[%s, %s]", format(estim[1], scientific=TRUE), format(estim[3], scientific=TRUE))
              legend(ifelse(newy[length(newy)]<newx[length(newx)], 'bottomleft', 'topleft'),
                     legend = c(legend1, legend2), cex = 1.5, text.col = 'steelblue4', bty = 'n')
            }
            
            if(showIC){
              bounds <- .IClm(getStdErr(object), getY(object), getFitValues(object), newy)
              xx <- c(newx, rev(newx))
              yy <- c(bounds$lo, rev(bounds$hi))
              polygon(xx, yy, border = NA, col = rgb(.8,.8,.8,.4))
              }
            
            lines(newy ~ newx, col=lcol, lwd=4)#,...)
            if(object@LPweight != 0){
              Sub = sprintf("Weighted %s-P logistic regr. (DoseResp package, version v.1)", object@npars)
            } else{ 
              Sub = sprintf("Non-weighted %s-P logistic regr. (DoseResp package, version v.1)", object@npars)
            }
            title (main = Title, sub = Sub, cex.sub = .75)
            par(op)
          }
)

setMethod('show', 'cellResp',
          function(object){
            cat("Instance of class cellResp\n")
            cat("\n")
            cat(sprintf("%s-P logistic model\n", object@npars))
            cat("Goodness of fit:", getGoodness(object), "\n")
            cat("Standard error:", getStdErr(object), "\n")
            cat("\n")
            cat("Estimated values:\n")
            show(getEstimates(object))
          }
)
