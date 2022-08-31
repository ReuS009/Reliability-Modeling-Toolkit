\name{probplotparam.nor}
\alias{probplotparam.nor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normal Probability Plot Parameters
}
\description{
Computes least-squares parameter estimates for normal probability distribution for preparation for normal probability plotting.
}
\usage{
probplotparam.nor(xi, F)
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
Normal distribution is unbounded (\eqn{-\Inf < x < \Inf}) consisting of a normal mean parameter \eqn{\mu} and a normal standard deviation parameter \eqn{\sigma}.

  PDF:
  \deqn{f(x) = (1 / \sigma \sqrt(2 \pi)) exp[-(1 / 2) ((x - \mu)/\sigma)^2]}

  CDF:

  \deqn{F(x) = \Phi[((x - \mu)/\sigma)]
  }
}
\value{
Returns several parameters for a single probability plot from a set of data \code{xi}.

\item{x_p at (0.1\% and 99.9\%)}{
  Returns the \eqn{x_p at 0.1}\% and \eqn{x_p at 99.9}\% based on a normal distribution.  \code{probplot.nor} uses this output to plot the best-fit line on the normal probability plots.
}
\item{fcB}{
Returns the transformed y-axis parameter for probabilities \eqn{0.1}\% and \eqn{99.9}\%.  For the normal plot the y-axis is \eqn{\Phi^-1 (p)}.  \code{probplot.nor} uses this output to plot the best-fit line on the normal probability plots.
}
\item{normresults}{
  The least squares estimates for the normal mean parameter \eqn{\mu} and the normal standard deviation parameter \eqn{\sigma}.
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
probplotparam.nor(x_R[,1], x_R[,3])

## The function is currently defined as
function (xi, F)
{
    fcB <- qnorm(c(0.001, 0.999), mean = 0, sd = 1)
    if (F[length(F)] == 1) {
        yfit <- qnorm(F[1:length(F) - 1], mean = 0, sd = 1)
        xfit <- xi[1:length(F) - 1]
    }
    else {
        yfit <- qnorm(F, mean = 0, sd = 1)
        xfit <- xi
    }
    pb2 <- lm(yfit ~ poly(xfit, 1, raw = TRUE))
    intercept <- summary(pb2)$coefficients[1, 1]
    slope <- summary(pb2)$coefficients[2, 1]
    sigmay <- 1/slope
    meany <- -intercept * sigmay
    R2 <- summary(pb2)$r.squared
    normresults <- matrix(c(meany, sigmay), nrow = 1, ncol = 2,
        byrow = TRUE, dimnames = list(c("Normal Parameters"),
            c("mean_t", "sigma_t")))
    xfit2 <- c(meany, meany + sigmay)
    yfit2 <- c(qnorm(0.5, mean = 0, sd = 1), qnorm(0.84, mean = 0,
        sd = 1))
    pb2_B <- lm(yfit2 ~ poly(xfit2, 1, raw = TRUE))
    min_mu_sig <- summary(pb2_B)$coefficients[2, 1]
    siginv <- summary(pb2_B)$coefficients[1, 1]
    ttfc <- c((qnorm(0.001, mean = 0, sd = 1) - siginv)/min_mu_sig,
        (qnorm(0.999, mean = 0, sd = 1) - siginv)/min_mu_sig)
    return(list(ttfc, fcB, normresults, R2))
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
