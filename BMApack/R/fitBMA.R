#' fitBMA Function
#'
#' Runs regression on all possible combos of covariates and returns coefs, R2, and BMA stats
#'
#' @param x A numeric matrix of covariates
#' @param y A numeric vector of the same length as the number of rows in x.
#' @param g A value for g. 
#'
#' @return A list with the elements
#'  \item{combo.coef}{A list of coefficients from each regression}
#'  \item{combo.fit}{Vector of R-squared Values} 
#'  \item{bmk}{Vector of posterior probability odds}
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

fitBMA<-function(x, y, g=3){
  library(HapEstXXR) ##Needed for powerset function
  library(plyr) ##Will need for later for parallel stuff
  set<-powerset(1:ncol(x)) ##create a list of all possible combos of variables
            
  list1<-list(NULL) ##empty list
            
  ##This for() loop creates a list item. Each item is a regression based on 
  ##the covariate matrix. The powerset deal allows an index of possible values
  
  for (i in 1:length(set)){
  list1[i]<-list(lm(scale(y)~-1+scale(x[,set[[i]]]))) ##all combinations
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
  bmk.vec<-aaply(.data=values,.margins=1,.fun=bmk)
  
  ##Sum of bmk for each model
  sum.bmk<-sum(bmk.vec)
  
  ##Fill in odds for bmk
  odds.bmk<-NULL
  for(i in 1:length(bmk.vec)){odds.bmk[i]<-bmk.vec[i]/sum.bmk}
  
  return(list(combo.coef=coefs, 
  combo.fit=fits, bmk=odds.bmk))
} ##Close function


thing<-fitBMA(cbind(covars, x3=covars[1]+rnorm(500), x4=covars[2]+rnorm(500)), dep, g=3)

##play code for figuring out next part
pval<-c(thing$bmk[1], thing$bmk[3])
bval<-c(thing$combo.coef[[1]], thing$combo.coef[[3]][2])
exp.b1<-((pval[1]*bval[1])+(pval[2]*bval[2]))*(g/g+1)
exp.b1
##trying to find out which items in my list contain the first x variable
trial<-unlist(thing$combo.coef)
index<-which(names(trial)=="x1") ##which coefs are for var x1
trial[index] ##the coef values for each mod containing x1

##Trying to figure out what models in my list contain the first x variable
trial2<-powerset(1:4) 
xiny<-function(y,x){x %in% y}

index2<-laply(trial2, .fun=xiny, x=1 ) ##find models contain x1
index2<-which(index2==TRUE) ##which ones

thing$bmk[index2] ##odds values for models containing x1

