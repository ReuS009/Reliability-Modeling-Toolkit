\name{probplot.exp2P}
\alias{probplot.exp2P}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-Parameter Exponential Probability Plot
}
\description{
Plots the two-parameter exponential probability plot for singular and multiple stress levels based on an input data set (a matrix) and a given plotting position.
}
\usage{
probplot.exp2P(data, pp, xlabel1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A matrix that represents a data set series which may consist of the following:
\itemize{
  \item{
  Column 1 consists of the failure (or primary event) data and the right censored data in a given set.  Data must consist of real and finite numbers.
  }
  \item{
  Column 2 gives the censored status of the data in column 1.  Demark failed data as \eqn{1} and right-censored data as \eqn{0}.
  }
  \item{
  Column(s) 3 (and on for multi-type stress) states the stress values for each data in column 1.  List one stress type per column (i.e. Column 3 - Temperature and Column 4 - Humidity).  Column 3 is not necessary if all data is from the same stress level however.
  }
}
}
  \item{pp}{
Name of the plotting position to select for computation.  See \code{\link{plotposit.select}} for plotting position definitions.
}
  \item{xlabel1}{
Label for the x-axis for the plot.  If left blank, the default label is "X".
}
}
\details{
  Two-parameter exponential distribution is positively bounded (\eqn{x >= 0}) consisting of a two-parameter exponential rate parameter \eqn{\sigma} and a two-parameter exponential location parameter \eqn{\theta}.

  PDF:
  \deqn{f(x) = (1/\sigma) exp[- (x - \theta)/\sigma]}

  CDF:

  \deqn{F(x) = 1 - exp[- (x - \theta)/\sigma]}
}
\value{
The output, in addition to the Weibull probability plot, includes the following for each stress group outlined in the \code{data} input:

\item{Stress Group}{
Lists the stress level based on the grouped data.  These will always be the first of each group of three on the output list (i.e. [[1]], [[4]], [[7]], etc.).
  }
  \item{exp2Presults}{
  The least squares estimates for the two-parameter exponential rate parameter \eqn{\sigma} and the two-parameter exponential location parameter \eqn{\theta}.  These will always be the second of each group of three on the output list (i.e. [[2]], [[5]], [[8]], etc.).
}
\item{R2}{
  Coeefficient of determination \eqn{R^2}.  These will always be the third of each group of three on the output list (i.e. [[3]], [[6]], [[9]], etc.).
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
## Example 1: Single data set (no stress distinction).  NOTE: Data must include a stress column because the group stress definition is one of the output.
mat0 <- matrix.failcen(xi = c(70,71,75,78,89), rc = c(80,80,84), nx = 8)
temp <- rep(373,length(mat0[,1]))
mat1 <- matrix(c(mat0[,1], mat0[,2], temp), nrow = length(mat0[,1]), ncol = 3, byrow = FALSE)
probplot.exp2P(mat1, "Blom", "Time (hours)")

## Example 2: Single Stress value (Temperature in Kelvin)
time <- c(52, 80, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 20, 28, 35, 50, 50, 50, 50, 50, 50, 50, 4, 11, 12, 18, 25, 25, 25)
cens <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0)
temp <- c(175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 250, 250, 250, 250, 250, 250, 250) + 273.15
mat2 <- matrix(c(time, cens, temp), nrow = length(time), ncol = 3, byrow = FALSE)
probplot.exp2P(mat2, "Blom", "Time (hours)")

## Example 3: Dual Stress values (Temperature in Kelvin and Relative Humidity (RH))
time <- c(102, 115, 151, 196, 210, 184, 193, 244, 265, 281, 298, 311, 337, 347, 405)
cens <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
temp <- c(393, 393, 393, 393, 393, 353, 353, 353, 353, 353, 353, 353, 353, 353, 353)
RH <- c(0.6, 0.6, 0.6, 0.6, 0.6, 0.85, 0.85, 0.85, 0.85, 0.85, 0.6, 0.6, 0.6, 0.6, 0.6)
mat3 <- matrix(c(time, cens, temp, RH), nrow = length(time), ncol = 4, byrow = FALSE)
probplot.exp2P(mat3, "KaplanMeier", "Time (hours)")

## The function is currently defined as
function (data, pp, xlabel1)
{
    if (missing(xlabel1)) {
        xlabel1 <- "X"
    }
    dcount <- checkdatacount(checkstress(data))
    if (sum(dcount[[1]]) == 0) {
        stop("Please check that there are at least two failure data that occur in the same stress level.")
    }
    if (identical(dcount[[1]], dcount[[2]])) {
        databystress <- checkstress(data)
        singledat <- NULL
    }
    else {
        databystress <- checkstress(data)[which(dcount[[1]] ==
            1 & dcount[[2]] == 1)]
        singledat <- checkstress(data)[which(!dcount[[1]] ==
            1 & dcount[[2]] == 1)]
    }
    Pticks1 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
        c(1:10), 10 * c(2:9), 95, 99, 99.9)
    Pticks <- -log(1 - Pticks1 * 0.01)
    Pticks1label <- c(0.1, 0.2, 0.3, "", 0.5, "", "", "", "",
        1, 2, 3, "", 5, "", "", "", "", 10 * c(1:9), 95, 99,
        99.9)
    fcB <- -log(1 - c(0.001, 0.999))
    if (!is.null(dim(databystress))) {
        ixirc <- sort.xircdata(data)
        xiRFblock <- plotposit.select(ixirc[[2]], ixirc[[3]],
            pp)
        XB <- xiRFblock[, 1]
        FB <- -log(xiRFblock[, 3])
        ttfc <- probplotparam.exp2P(xiRFblock[, 1], xiRFblock[,
            2])
        ttfcrange <- ttfc[[1]]
        params <- ttfc[[3]]
        outputpp <- list(ttfc[[3]], ttfc[[4]])
    }
    else {
        ixirc_list <- vector(mode = "list", length = length(databystress))
        xiRFblock_list <- vector(mode = "list", length = length(databystress))
        XB_list <- vector(mode = "list", length = length(databystress))
        FB_list <- vector(mode = "list", length = length(databystress))
        ttfc_list <- vector(mode = "list", length = length(databystress))
        outputpp <- vector(mode = "list", length = 3 * length(databystress) +
            1)
        ttfcrange <- c(1)
        XBfull <- c(1)
        FBfull <- c(1)
        for (i in 1:length(databystress)) {
            ixirc_list[[i]] <- sort.xircdata(databystress[[i]])
            xiRFblock_list[[i]] <- plotposit.select(ixirc_list[[i]][[2]],
                ixirc_list[[i]][[3]], pp)
            XB_list[[i]] <- xiRFblock_list[[i]][, 1]
            FB_list[[i]] <- -log(xiRFblock_list[[i]][, 3])
            ttfc_list[[i]] <- probplotparam.exp2P(xiRFblock_list[[i]][,
                1], xiRFblock_list[[i]][, 2])
            outputpp[[i * 3 - 2]] <- databystress[[i]][1, 3:length(databystress[[i]][1,
                ])]
            outputpp[[i * 3 - 1]] <- ttfc_list[[i]][[3]]
            outputpp[[i * 3]] <- ttfc_list[[i]][[4]]
            XBfull <- c(XBfull, XB_list[[i]])
            FBfull <- c(FBfull, FB_list[[i]])
            ttfcrange <- c(ttfcrange, ttfc_list[[i]][[1]])
        }
        outputpp[[3 * length(databystress) + 1]] <- singledat
        ttfcrange <- ttfcrange[2:length(ttfcrange)]
        XBfull <- XBfull[2:length(XBfull)]
        FBfull <- FBfull[2:length(FBfull)]
    }
    if (!is.null(dim(databystress))) {
        plot(XB, FB, col = "blue", xlab = xlabel1, ylab = "Percent Failure",
            pch = 16, xlim = c(min(ttfcrange), max(ttfcrange)),
            ylim = c(min(fcB), max(fcB)), axes = FALSE)
        lines(ttfc[[1]], ttfc[[2]], col = "blue")
    }
    else {
        plot(XBfull, FBfull, col = "blue", xlab = xlabel1, ylab = "Percent Failure",
            pch = 16, xlim = c(min(ttfcrange), max(ttfcrange)),
            ylim = c(min(fcB), max(fcB)), axes = FALSE)
        for (i in 1:length(databystress)) {
            lines(ttfc_list[[i]][[1]], ttfc_list[[i]][[2]], col = "blue")
        }
    }
    axis(1, las = 2, cex.axis = 0.7)
    axis(2, at = Pticks, labels = Pticks1label, las = 2, cex.axis = 0.7)
    abline(h = Pticks, lty = 2, col = "grey")
    abline(v = axis(1, las = 2, cex.axis = 0.7), lty = 2, col = "grey")
    legend(min(ttfcrange), -log(1 - 0.99, ), legend = c("Data",
        "2P Exponential best fit line"), col = c("blue", "blue"),
        pch = c(16, -1), lty = c(0, 1), cex = 0.8, text.font = 4,
        bg = "white")
    return(outputpp)
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
