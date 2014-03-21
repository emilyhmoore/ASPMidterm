#' plot.regcombo Function
#'
#' Plots regcombo objects as returned by fitBMA.
#' Plots the density of the coefficient values.
#' The blue line represents the expected value of the coefficient
#' The red line represents an expected value of 0. 
#' If no blue line is visible, the expected value of that coefficient is very close to 0.
#' If there is a red and blue line visible, it show how far the expected value of the
#' coefficient is from 0.
#' Sometimes this falls outside of the density range.
#' It will print the probability that a coefficient is non-zero to the console
#' All of the plots will display, but you may need to go through them.
#' For example, in R studio, I have to press the left arrow to see the other plots.
#'
#' @author Emily Moore
#' @examples
#' 
#' data<-matrix(rnorm(1000), ncol=10)
#' colnames(data)<-c(paste("x", 1:10, sep=""))
#' datay<-5*data[,2]+3*data[,3]+rnorm(100)
#' trial<-(fitBMA(x=data, y=datay, g=3, parallel=FALSE)) 
#' plot(trial)
#' @rdname plot.regcombo
#' @export
#' 

setMethod(f="plot", signature="regcombo",
          definition=function(x,y=NULL,...){
            y=NULL
            thecoefs2<-t(x@thecoefs)
            plot.the.coefs<-function(i){
              lowlim<-min(c(0, thecoefs2[,i], x@exp.vals[i])) ##set low limit
              highlim<-max(c(0, thecoefs2[,i], x@exp.vals[i])) ##set high limit
              ##plot
              varnames<-(colnames(thecoefs2))
              plot(density(thecoefs2[,i]),
                   xlim=c(lowlim, highlim),
                   main=paste("Density Plot of Coef Values for Variable",
                              varnames[i]))
              abline(v=c(x@exp.vals[i],0), col=c("blue", "red"))
            }
            library(plyr)
            l_ply(1:ncol(thecoefs2), plot.the.coefs)
            print(x@exp.vals)
            print(x@coefprobs)
          })

