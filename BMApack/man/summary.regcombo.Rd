\docType{methods}
\name{summary,regcombo-method}
\alias{summary,regcombo-method}
\title{summary.regcombo Function}
\usage{
\S4method{summary}{regcombo}(object)
}
\arguments{
  \item{object}{an object for which a summary is desired.}
}
\value{
Prints summaries of coefficients and posterior model odds
and returns list \item{exp.vals}{A vector of expected
coefficient values} \item{coefprobs}{A vector of
probabilities that the coefficient is non-zero}
}
\description{
Summarizes regcombo objects as returned by fitBMA
}
\examples{
x1<-rnorm(500)
x2<-rnorm(500,3,15)
dep<-(x1+2*x2)+rnorm(500,4,100)
covars<-cbind(x1,x2)
trial<-fitBMA(x=covars, y=dep)
summary(trial)
}
\author{
Emily Moore
}

