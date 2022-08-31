\name{plotposit.nelsonaalen}
\alias{plotposit.nelsonaalen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nelson-Aalen Non-Parametric Output Tabulation
}
\description{
Tabulation of nonparametric computations based on Nelson-Aalen Plotting Position (failure probability or unreliability, reliability, hazard, cumulative hazard).
}
\usage{
plotposit.nelsonaalen(xi, rc)
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Definition of \emph{Nelson-Aalen} plotting position equation for failure probability (or unreliability) \eqn{F(x)}:
\deqn{F_i(x_i) = 1 - exp[-\Sigma_(x_i <= x) d_i/(n_(i - 1) - d_(i - 1) - c_(i - 1))]} (\emph{NOTE}: \eqn{d} is the number of failed units and \eqn{c} is the number of censored.)
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
plotposit.nelsonaalen(xi = Xi1, rc = Rc1)
plotposit.nelsonaalen(Xi1, Rc1)

## Example 2
Xi1 <- c(70,71,75,78,89)
plotposit.nelsonaalen(xi = Xi1)
plotposit.nelsonaalen(Xi1, NULL)
plotposit.nelsonaalen(Xi1)

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
    F <- 1 - exp(-cumsum(di/ni))
    R <- 1 - F
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
