#' summary.regcombo Function
#'
#' Summarizes regcombo objects as returned by fitBMA
#'
#'
#' @return Prints summaries of coefficients and posterior model odds and returns list 
#'  \item{exp.vals}{A vector of expected coefficient values}
#'  \item{coefprobs}{A vector of probabilities that the coefficient is non-zero}
#' @author Emily Moore
#' @examples
#' 
#' x1<-rnorm(500)
#' x2<-rnorm(500,3,15)
#' dep<-(x1+2*x2)+rnorm(500,4,100)
#' covars<-cbind(x1,x2) 
#' trial<-fitBMA(x=covars, y=dep)
#' summary(trial)
#' @rdname summary.regcombo
#' @export
#' 

setMethod(f="summary", signature="regcombo",
          definition=function(object){
            thecoefstrans<-t(object@thecoefs)
            
            cat("Coefficient Value Summary", "\n")
            print(summary(thecoefstrans))
            cat("\n")
            
            cat("Summary of Posterior Model Odds:", "\n")
            print(summary(object@bmk))
            cat("\n")
            
            cat("Expected Value of Coef and Posterior Probability it is Non-zero:", "\n")
            return(list(exp.vals=object@exp.vals, coef.probs=object@coefprobs))
          })

