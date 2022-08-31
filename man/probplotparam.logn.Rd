\name{probplotparam.logn}
\alias{probplotparam.logn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lognormal Probability Plot Parameters
}
\description{
Computes least-squares parameter estimates for lognormal probability distribution for preparation for lognormal probability plotting.
}
\usage{
probplotparam.logn(xi, F)
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
Lognormal distribution is positively bounded (\eqn{x >= 0}) consisting of a lognormal mean parameter \eqn{\mu_x} and a lognormal standard deviation parameter \eqn{\sigma_x}.

  PDF:
  \deqn{f(x) = (1 / x \sigma_x \sqrt(2 \pi)) exp[-(1 / 2) ((ln x - \mu_x)/\sigma_x)^2]}

  CDF:

  \deqn{F(x) = \Phi[((ln x - \mu_x)/\sigma_x)]
  }
}
\value{
Returns several parameters for a single probability plot from a set of data \code{xi}.

\item{x_p at (0.1\% and 99.9\%)}{
  Returns the \eqn{x_p at 0.1}\% and \eqn{x_p at 99.9}\% based on a lognormal distribution.  \code{probplot.logn} uses this output to plot the best-fit line on the lognormal probability plots.
}
\item{fcB}{
Returns the transformed y-axis parameter for probabilities \eqn{0.1}\% and \eqn{99.9}\%.  For the lognormal plot the y-axis is \eqn{\Phi^-1 (p)}.  \code{probplot.logn} uses this output to plot the best-fit line on the lognormal probability plots.
}
\item{lognresults}{
  The least squares estimates for the lognormal mean parameter \eqn{\mu_x} and the lognormal standard deviation parameter \eqn{\sigma_x}.
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
probplotparam.logn(x_R[,1], x_R[,3])

## The function is currently defined as
function (xi, F)
{
    fcB <- qnorm(c(0.001, 0.999), mean = 0, sd = 1)
    if (F[length(F)] == 1) {
        yfit <- qnorm(F[1:length(F) - 1], mean = 0, sd = 1)
        xfit <- log(xi[1:length(F) - 1])
    }
    else {
        yfit <- qnorm(F, mean = 0, sd = 1)
        xfit <- log(xi)
    }
    pb2 <- lm(yfit ~ poly(xfit, 1, raw = TRUE))
    intercept <- summary(pb2)$coefficients[1, 1]
    slope <- summary(pb2)$coefficients[2, 1]
    sigmat <- 1/slope
    meant <- -intercept * sigmat
    R2 <- summary(pb2)$r.squared
    lognresults <- matrix(c(meant, sigmat), nrow = 1, ncol = 2,
        byrow = TRUE, dimnames = list(c("Lognormal Parameters"),
            c("mean_t", "sigma_t")))
    xfit2 <- c(meant, meant + sigmat)
    yfit2 <- c(qnorm(0.5, mean = 0, sd = 1), qnorm(0.84, mean = 0,
        sd = 1))
    pb2_B <- lm(yfit2 ~ poly(xfit2, 1, raw = TRUE))
    min_mu_sig <- summary(pb2_B)$coefficients[2, 1]
    siginv <- summary(pb2_B)$coefficients[1, 1]
    ttfc <- c(exp((qnorm(0.001, mean = 0, sd = 1) - siginv)/min_mu_sig),
        exp((qnorm(0.999, mean = 0, sd = 1) - siginv)/min_mu_sig))
    return(list(ttfc, fcB, lognresults, R2))
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
