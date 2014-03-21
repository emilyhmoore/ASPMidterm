library(devtools)
library(roxygen2)
find_rtools() ##For using devtools with windows. 
setwd("~/GitHub/ASPMidterm")
create(path="./BMApack", check=FALSE)

current.code <- as.package("BMAPack")
load_all(current.code)
document(current.code)

install(pkg=current.code, local=TRUE)

check(current.code)

##Example data

help(fitBMA)
help(summary.regcombo)
help.search('plot,regcombo-method')
example(fitBMA)
example(summary.regcombo)
example('plot,regcombo-method') ##may take a sec

