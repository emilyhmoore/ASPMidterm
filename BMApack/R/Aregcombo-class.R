#' A regression combination class
#' 
#' Object of class \code{regcombo} as created by the \code{comboreg} functions
#'
#' 
#' An object of the class `regcombo' has the following slots:
#' \itemize{
#' \item \code{combo.coef} A list of coefficients for each possible regression
#' \item \code{combo.fit} A vector of R-Squared values
#' \item \code{x} A matrix input of covariates
#' \item \code{y} The dependent variable input  
#' }
#'
#' @author Emily Moore: \email{emily.moore@@wustl.edu}
#' @export
setClass(Class="regcombo",
         representation = representation(
           combo.coef = "list",
           combo.fit="numeric",
           bmk="numeric",
           exp.vals="numeric",
           thecoefs="matrix",
           coefprobs="numeric",
           x="matrix",
           y="numeric"
         ),
         prototype = prototype(
           combo.coef=list(),
           combo.fit=numeric(),
           bmk=numeric(),
           exp.vals=numeric(),
           thecoefs=matrix(),
           coefprobs=numeric(),
           x = matrix(),
           y = numeric()
         )
)

#' @export
setMethod("initialize", "regcombo",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export
setMethod("show", "regcombo",
          function(object){
            cat("Regression Fits:", "\n")
            print(object@combo.fit)
            cat("\n")
            cat("Regression Coefficients:", "\n")
            print(object@combo.coef)
            cat("\n")
            cat("Posterior Model Odds:", "\n")
            print(object@bmk)
            cat("\n")
            cat("Posterior Expected Value of Coefficients:", "\n")
            print(object@exp.vals)
            cat("\n")
            cat("Posterior Probability Coefficient is Non-zero:", "\n")
            print(object@coefprobs)
          }
)





