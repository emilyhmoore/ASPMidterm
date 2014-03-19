library(devtools)
library(roxygen2)
find_rtools() ##For using devtools with windows. 
setwd("~/GitHub/ASPMidterm")
create(path="./BMApack", check=FALSE)

current.code <- as.package("BMAPack")
load_all(current.code)
document(current.code)

check(current.code)

install(pkg=current.code, local=TRUE)

##Example data

help(fitBMA)
example(fitBMA)

