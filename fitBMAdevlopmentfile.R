fitBMA<-function(x, y, g){
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
            
  return(list(combo.coef=coefs, 
  combo.fit=fits))
} ##Close function



comboreg(covars, dep)