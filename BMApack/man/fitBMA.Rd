\name{fitBMA}
\alias{fitBMA}
\title{fitBMA Function}
\usage{
fitBMA(x, y, g = 3, parallel = FALSE, ...)
}
\arguments{
  \item{x:}{A numeric matrix of covariates}

  \item{y:}{A numeric vector of the same length as the
  number of rows in x.}

  \item{g:}{A value for g.}

  \item{parallel:}{runs in parallel if TRUE}
}
\value{
An S4 class object with following slots:
\item{combo.coef}{A list of coefficients from each
regression} \item{combo.fit}{Vector of R-squared Values}
\item{bmk}{Vector of posterior probability odds}
\item{exp.vals}{A vector of expected coefficient values}
\item{coefprobs}{A vector of probabilities that the
coefficient is non-zero}
}
\description{
Runs regression on all possible combos of covariates and
returns coefs, R2, and BMA stats
}
\examples{
x1<-rnorm(500)
x2<-rnorm(500,3,15)
dep<-(x1+2*x2)+rnorm(500,4,100)
covars<-cbind(x1,x2)
fitBMA(x=covars, y=dep)
}
\author{
Emily Moore
}

