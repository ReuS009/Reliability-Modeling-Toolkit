\name{probplotparam.wbl3P}
\alias{probplotparam.wbl3P}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Three Parameter Weibull Probability Plot Parameters
}
\description{
Computes least-squares parameter estimates for three-parameter Weibull probability distribution for preparation for three-parameter Weibull probability plotting.
}
\usage{
probplotparam.wbl3P(xi, R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xi}{
  A vector consisting of the failure or primary event data in a given set.  Data must consist of real and finite numbers.
  }
  \item{R}{
  A vector of non-parametric estimates for reliability \eqn{R} at data \code{xi}.
  }
}
\details{
  Three-parameter Weibull distribution is positively bounded (\eqn{x >= 0}) consisting of a Weibull scale parameter \eqn{\alpha}, a Weibull shape parameter \eqn{\beta}, and a Weibull location parameter \eqn{\gamma}.

  PDF:
  \deqn{f(x) = (\beta/\alpha) ((x - \gamma)/\alpha)^(\beta - 1) exp[-((x - \gamma)/\alpha)^\beta]}

  CDF:

  \deqn{F(x) = 1 - exp[-((x - \gamma)/\alpha)^\beta]}
}
\value{
Returns several parameters for a single probability plot from a set of data \code{xi}.

\item{x_p at (0.1\% and 99.9\%)}{
  Returns the \eqn{x_p at 0.1}\% and \eqn{x_p at 99.9}\% based on a Weibull distribution.  \code{probplot.wbl} uses this output to plot the best-fit line on the Weibull probability plots.
}
\item{fcB}{
Returns the transformed y-axis parameter for probabilities \eqn{0.1}\% and \eqn{99.9}\%.  For the Weibull plot the y-axis is \eqn{ln[ln(1/R)]}.  \code{probplot.wbl} uses this output to plot the best-fit line on the Weibull probability plots.
}
\item{wblresults}{
  The least squares estimates for the Weibull scale parameter \eqn{\alpha}, the Weibull shape parameter \eqn{\beta}, and the Weibull location parameter \eqn{\gamma}.
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
probplotparam.wbl3P(x_R[,1], x_R[,3])

## The function is currently defined as
function (xi, R)
{
    fcB <- log(log(1/(1 - c(0.001, 0.999))))
    gamest <- seq(from = 0.001, to = 0.999, by = 0.01)
    R2up <- 0
    for (i in 1:(length(gamest) - 1)) {
        if (R[length(R)] == 0) {
            yfit <- log(log(1/R[1:length(R) - 1]))
            xfit <- log(xi[1:length(R) - 1] - gamest[i])
        }
        else {
            yfit <- log(log(1/R))
            xfit <- log(xi - gamest[i])
        }
        wblparm <- lm(yfit ~ poly(xfit, 1, raw = TRUE))
        R2 <- summary(wblparm)$r.squared
        if (R2 >= R2up) {
            R2up <- R2
            wblparmup <- wblparm
            gamparam <- gamest[i]
        }
    }
    beta <- summary(wblparmup)$coefficients[2, 1]
    alpha <- exp(-summary(wblparmup)$coefficients[1, 1]/beta)
    gammaparam <- gamparam
    R2 <- summary(wblparmup)$r.squared
    wblresults <- matrix(c(alpha, beta, gammaparam), nrow = 1,
        ncol = 3, byrow = TRUE, dimnames = list(c("3P Wbl Parameters"),
            c("alpha", "beta", "Gamma")))
    intercept <- summary(wblparmup)$coefficients[1, 1]
    ttfc <- c(exp((log(log(1/(1 - 0.001))) - intercept)/beta) +
        gammaparam, exp((log(log(1/(1 - 0.999))) - intercept)/beta) +
        gammaparam)
    return(list(ttfc, fcB, wblresults, R2))
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
