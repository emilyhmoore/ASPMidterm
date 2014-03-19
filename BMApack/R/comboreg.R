#' Reg Combo Function
#'
#' Runs regression on all possible combos of covariates
#'
#' @param x A numeric matrix of covariates
#' @param y A numeric vector of the same length as the number of rows in x.
#'
#' @return A list with the elements
#'  \item{combo.coef}{A list of coefficients from each regression}
#'  \item{combo.fit}{Vector of R-squared Values} 
#' @author Emily Moore
#' @examples
#' 
#' x1<-rnorm(500)
#' x2<-rnorm(500,3,15)
#' dep<-(x1+2*x2)+rnorm(500,4,100)
#' covars<-cbind(x1,x2) 
#' comboreg(x=covars, y=dep)
#' @rdname comboreg
#' @export

setMethod(f="comboreg",
          definition=function(x, y){
  library(HapEstXXR) ##Needed for powerset function
  library(plyr) ##Will need for later for parallel stuff
  set<-powerset(1:ncol(x)) ##create a list of all possible combos of variables
  
  list1<-list(NULL) ##empty list
  
  ##This for() loop creates a list item. Each item is a regression based on 
  ##the covariate matrix. The powerset deal allows an index if possible values
  for (i in 1:length(set)){
  list1[i]<-list(lm(y~-1+scale(x[,set[[i]]]))) ##all combinations
  }
 
  coefs<-llply(list1, coef) ##extract coefs from the regressions in list

  ##Sets names of coefs to the appropriate column name
  for (i in 1:length(coefs)){
    names(coefs[[i]])<-colnames(x)[set[[i]]]
  }
  
  ##This function extracts the r.squared values
  r.sq<-function(x){
  summary(x)$r.squared
  }
  
  ##This gets the r.squared values and puts them in a list
  fits<-llply(list1, r.sq)
  
  ##Since lapply makes a list, we unlist to make a vector
  fits<-unlist(fits)
            
  return(new("regcombo", combo.coef=coefs, 
                    combo.fit=fits))
          }
)
comboreg(covars, dep)
