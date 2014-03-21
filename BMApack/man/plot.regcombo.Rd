\docType{methods}
\name{plot,regcombo-method}
\alias{plot,regcombo-method}
\title{plot.regcombo Function}
\usage{
\S4method{plot}{regcombo}(x, y = NULL, ...)
}
\arguments{
  \item{x}{the coordinates of points in the plot.
  Alternatively, a single plotting structure, function or
  \emph{any \R object with a \code{plot} method} can be
  provided.}

  \item{y}{the y coordinates of points in the plot,
  \emph{optional} if \code{x} is an appropriate structure.}

  \item{...}{Arguments to be passed to methods, such as
  \link{graphical parameters} (see \code{\link{par}}).
  Many methods will accept the following arguments:
  \describe{ \item{\code{type}}{what type of plot should be
  drawn.  Possible types are \itemize{ \item \code{"p"} for
  \bold{p}oints, \item \code{"l"} for \bold{l}ines, \item
  \code{"b"} for \bold{b}oth, \item \code{"c"} for the
  lines part alone of \code{"b"}, \item \code{"o"} for both
  \sQuote{\bold{o}verplotted}, \item \code{"h"} for
  \sQuote{\bold{h}istogram} like (or \sQuote{high-density})
  vertical lines, \item \code{"s"} for stair \bold{s}teps,
  \item \code{"S"} for other \bold{s}teps, see
  \sQuote{Details} below, \item \code{"n"} for no plotting.
  } All other \code{type}s give a warning or an error;
  using, e.g., \code{type = "punkte"} being equivalent to
  \code{type = "p"} for S compatibility.  Note that some
  methods, e.g. \code{\link{plot.factor}}, do not accept
  this.  } \item{\code{main}}{an overall title for the
  plot: see \code{\link{title}}.} \item{\code{sub}}{a sub
  title for the plot: see \code{\link{title}}.}
  \item{\code{xlab}}{a title for the x axis: see
  \code{\link{title}}.} \item{\code{ylab}}{a title for the
  y axis: see \code{\link{title}}.} \item{\code{asp}}{the
  \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.} }
  }
}
\description{
Plots regcombo objects as returned by fitBMA. Plots the
density of the coefficient values. The blue line represents
the expected value of the coefficient The red line
represents an expected value of 0. If no blue line is
visible, the expected value of that coefficient is very
close to 0. If there is a red and blue line visible, it
show how far the expected value of the coefficient is from
0. Sometimes this falls outside of the density range. It
will print the probability that a coefficient is non-zero
to the console All of the plots will display, but you may
need to go through them. For example, in R studio, I have
to press the left arrow to see the other plots.
}
\examples{
data<-matrix(rnorm(1000), ncol=10)
colnames(data)<-c(paste("x", 1:10, sep=""))
datay<-5*data[,2]+3*data[,3]+rnorm(100)
trial<-(fitBMA(x=data, y=datay, g=3, parallel=FALSE))
plot(trial)
}
\author{
Emily Moore
}

