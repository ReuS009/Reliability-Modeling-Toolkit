\name{plotposit.kaplanmeier}
\alias{plotposit.kaplanmeier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kaplan-Meier Non-Parametric Output Tabulation
}
\description{
Tabulation of nonparametric computations based on Kaplan-Meier Plotting Position (failure probability or unreliability, reliability, hazard, cumulative hazard).
}
\usage{
plotposit.kaplanmeier(xi, rc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xi}{
  A vector consisting of the failure or primary event data in a given set.  Data must consist of real and finite numbers.
  }
  \item{rc}{
  A vector consisting of the right censored data of a given set of data.  Data must consist of real and finite numbers.
  }
}
\details{
Definition of \emph{Kaplan-Meier} plotting position equation for failure probability (or unreliability) \eqn{F(x)}:
\deqn{F_i(x_i) = 1 - \Pi_(x_j <= x) (1 - d/j)} (\emph{NOTE}: \eqn{d} is the number of failed units and \eqn{j} is the reverse rank)
}
\value{
Returns a matrix of nonparameter estimates for each failure data xi: Column 2 is the \emph{failure probability}, Column 3 is the \emph{reliability}, Column 4 is the \emph{hazard rate}, and Column 5 is the \emph{cumulative hazard}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr. Reuel Smith (2020-2022)
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
plotposit.kaplanmeier(xi = Xi1, rc = Rc1)
plotposit.kaplanmeier(Xi1, Rc1)

## Example 2
Xi1 <- c(70,71,75,78,89)
plotposit.kaplanmeier(xi = Xi1)
plotposit.kaplanmeier(Xi1, NULL)
plotposit.kaplanmeier(Xi1)

## The function is currently defined as
function (xi, rc)
{
    nx <- nxi.count(xi, rc)
    if (missing(rc)) {
        faicen <- matrix.failcen(xi, nx = nx[[1]])
    }
    else {
        faicen <- matrix.failcen(xi, rc, nx = nx[[1]])
    }
    fail_and_cen <- count.failcen(faicen, nx[[1]])
    di <- fail_and_cen[, 2][which(fail_and_cen[, 2] > 0)]
    ni <- fail_and_cen[, 4][which(fail_and_cen[, 2] > 0)]
    R <- cumprod((ni - di)/ni)
    F <- 1 - R
    H <- -log(R)
    h <- c(H[1], diff(H))
    matFR <- matrix(c(nx[[2]], F, R, h, H), nrow = length(nx[[2]]),
        ncol = 5)
    return(matFR)
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
