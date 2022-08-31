\name{probplotparam.exp}
\alias{probplotparam.exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exponential Probability Plot Parameters
}
\description{
Computes least-squares parameter estimates for exponential probability distribution for preparation for exponential probability plotting.
}
\usage{
probplotparam.exp(xi, F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xi}{
  A vector consisting of the failure or primary event data in a given set.  Data must consist of real and finite numbers.
  }
  \item{F}{
  A vector of non-parametric estimates for failure probability \eqn{F} at data \code{xi}.
  }
}
\details{
  Exponential distribution is positively bounded (\eqn{x >= 0}) consisting of a exponential rate parameter \eqn{\lambda}.

  PDF:
  \deqn{f(x) = \lambda exp(-\lambda x)}

  CDF:

  \deqn{F(x) = 1 - exp(-\lambda x)}
}
\value{
Returns several parameters for a single probability plot from a set of data \code{xi}.

\item{x_p at (0.1\% and 99.9\%)}{
  Returns the \eqn{x_p at 0.1}\% and \eqn{x_p at 99.9}\% based on a exponential distribution.  \code{probplot.exp} uses this output to plot the best-fit line on the exponential probability plots.
}
\item{fcB}{
Returns the transformed y-axis parameter for probabilities \eqn{0.1}\% and \eqn{99.9}\%.  For the exponential plot the y-axis is \eqn{- ln(R)}.  \code{probplot.exp} uses this output to plot the best-fit line on the exponential probability plots.
}
\item{expresults}{
  The least squares estimates for the exponential rate parameter \eqn{\lambda}.
}
\item{R2}{
  Coeefficient of determination \eqn{R^2}.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr. Reuel Smith (2021-2022)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Example 1
Xi1 <- c(70,71,75,78,89)
Rc1 <- c(80,80,84)
x_R <- plotposit.blom(i = rankcalc(xi = Xi1, rc = Rc1), xi = Xi1, rc = Rc1)
probplotparam.exp(x_R[,1], x_R[,3])

## The function is currently defined as
function (xi, F)
{
    fcB <- -log(1 - c(0.001, 0.999))
    if (F[length(F)] == 1) {
        yfit <- -log(1 - F[1:length(F) - 1])
        xfit <- xi[1:length(F) - 1]
    }
    else {
        yfit <- -log(1 - F)
        xfit <- xi
    }
    pb2 <- lm(yfit ~ xfit + 0)
    lambda <- summary(pb2)$coefficients[, 1]
    R2 <- summary(pb2)$r.squared
    expresults <- matrix(c(lambda), nrow = 1, ncol = 1, byrow = TRUE,
        dimnames = list(c("Exponential Parameter"), c("lambda")))
    ttfc <- c(-log(1 - 0.001)/lambda, -log(1 - 0.999)/lambda)
    return(list(ttfc, fcB, expresults, R2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
