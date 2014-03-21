#' fitBMA Function
#'
#' Runs regression on all possible combos of covariates and returns coefs, R2, and BMA stats
#'
#' @param x: A numeric matrix of covariates
#' @param y: A numeric vector of the same length as the number of rows in x.
#' @param g: A value for g. 
#' @param parallel: runs in parallel if TRUE
#'
#' @return An S4 class object with following slots: 
#'  \item{combo.coef}{A list of coefficients from each regression}
#'  \item{combo.fit}{Vector of R-squared Values} 
#'  \item{bmk}{Vector of posterior probability odds}
#'  \item{exp.vals}{A vector of expected coefficient values}
#'  \item{coefprobs}{A vector of probabilities that the coefficient is non-zero}
#' @author Emily Moore
#' @examples
#' 
#' x1<-rnorm(500)
#' x2<-rnorm(500,3,15)
#' dep<-(x1+2*x2)+rnorm(500,4,100)
#' covars<-cbind(x1,x2) 
#' fitBMA(x=covars, y=dep)
#' @rdname fitBMA
#' @export

setGeneric(name="fitBMA",
           def=function(x, y, g=3, parallel=FALSE, ...)
           {standardGeneric("fitBMA")}
)

setMethod(f="fitBMA",
          definition=function(x, y, g=3, parallel=FALSE){
  library(HapEstXXR) ##Needed for powerset function
  library(plyr) ##Will need for later for parallel stuff
  
  ##Error thrown if non-unque column names.
  if(length(unique(colnames(x)))<ncol(x)){stop("Must have unique names for each column")}
  
  set<-powerset(1:ncol(x)) ##create a list of all possible combos of variables
            
  list1<-list(NULL) ##empty list
            
  ##This for() loop creates a list item. Each item is a regression based on 
  ##the covariate matrix. The powerset deal allows an index of possible values
  for (i in 1:length(set)){
  list1[i]<-list(lm(scale(y)~-1+scale(x[,set[[i]]]))) ##all combinations
  }
  
  coefs<-llply(list1, coef, .parallel=parallel) ##extract coefs from the regressions in list
            
  ##Sets names of coefs to the appropriate column name
  for (i in 1:length(coefs)){
    names(coefs[[i]])<-colnames(x)[set[[i]]]
  }
         
  ##This function extracts the r.squared values
  r.sq<-function(x){
    summary(x)$r.squared
  }
            
  ##This gets the r.squared values and puts them in a list
  fits<-llply(list1, r.sq, .parallel=parallel)
            
  ##Since lapply makes a list, we unlist to make a vector
  fits<-unlist(fits)

  ##Create a matrix of the values needed to calculate b|mk:m0| for each model
  gs<-rep(g, length(set)) ##make a vector of the g value
  ns<-rep(length(y), length(set)) ##make a vector of the n value
  pks<-numeric() ##make an empty vector
  for(i in 1:length(set)){pks[i]<-length(set[[i]])} ##fill in pk values
  r2s<-fits##r2 values
  
  values<-cbind(gs, ns, pks, r2s)##make matrix of these
  
  ##function is intended to be used to aaply over the rows of the matrix above
  bmk<-function(x){
    bmk<-((1+x[1])^((x[2]-x[3])/2))*((1+x[1]*(1-x[4]))^(-(x[2]-1)/2))
    names(bmk)<-c("bmk.val")
    return(bmk)
  }
  
  ##vector of bmk values for each model
  bmk.vec<-aaply(.data=values,.margins=1,.fun=bmk, .parallel=parallel)
  
  ##Sum of bmk for each model
  sum.bmk<-sum(bmk.vec)
  
  ##Fill in odds for bmk
  odds.bmk<-NULL
  for(i in 1:length(bmk.vec)){odds.bmk[i]<-bmk.vec[i]/sum.bmk}
  
  ##Function which returns x in y since I couldn't find what I was looking for
  xiny<-function(y,x){x %in% y}
  
  ##Function which determines which sets in the set of models include each variable
  applier<-function(i){
    index2<-laply(set, xiny, x=i, .parallel=parallel) ##is it included in this one? True/false
    index2<-which(index2==TRUE) ##Which ones are true?
    return(index2)
  }

  ##Function which returns the odds of each model including the relevant variable
  theodds<-function(i){
    index3<-laply(1:ncol(x), applier, .parallel=parallel)
    odds.bmk[index3[i,]]
  }

  ##Get the probability values of the mods in question and put them in a matrix
  themods<-laply(1:ncol(x), theodds, .parallel=parallel)
  
  ##Get the relevant coefs
  coefnamer<-function(i){
    coefvec<-unlist(coefs) ##turn list of coefs into a vector.
    coefname<-which(names(coefvec)==colnames(x)[i]) ##Which coefs have a matching name
    coef1<-coefvec[coefname]
    return(coef1)
  }

##Apply coefnamer function over the columns of x
  thecoefs<-llply(1:ncol(x), coefnamer, .parallel=parallel) 
  thecoefs<-unlist(thecoefs)
  thecoefs<-matrix(thecoefs, nrow=ncol(x), byrow=TRUE)
  rownames(thecoefs)<-colnames(x)

  ptimese<-themods*thecoefs ##Utilize R's practice of element-wise multiplication of matrices

  exp.val1<-aaply(ptimese, 1, sum, .parallel=parallel) ##Sum across the rows

  exp.val<-exp.val1*(g/(g+1)) ##Multiply by g/g+1
  names(exp.val)<-colnames(x)
  
  coefprob<-aaply(themods, 1, sum, .parallel=parallel)
  names(coefprob)<-colnames(x)
  
  return(new("regcombo", x=x, y=y, thecoefs=thecoefs, combo.coef=coefs, 
             combo.fit=fits,bmk=odds.bmk, exp.vals=exp.val, coefprobs=coefprob))
          }#close function definition
) ##Close method


##takes about 10-12 seconds to run regressions with 10 variables 
##and 1000 observations on my computer without parallel.
##I can't make parallel work with my computer, so hopefully it works.


