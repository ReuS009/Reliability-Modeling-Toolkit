\name{SN.diagram}
\alias{SN.diagram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S-N Diagram
}
\description{
Generates the S-N diagram based on various stress and fatigue life input data.
}
\usage{
SN.diagram(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input_type}{
Numeric definition for the input type of the data and its options: 1 - Fully reversed (R = -1) S-N data points, 2 - cyclic stress range , 3 - multi-axial stress
}
\item{data}{
Data is entered as a list of fully reversed stress and cycle data.  List may include runoff stress and cycle data. (Order of list: (1) alternating stress, (2) cycles to failure, (3) runoff stress, (4) runoff cycles)
}
\item{stressunits}{
Defines the units for the stress amplitude: \code{1} for \emph{'MPa'} and \code{2} for \emph{'ksi'}.  \emph{'MPa'} will be used as default if this heading is left empty.
}
\item{options}{
Optional input vector determined by the definition of its input type \code{input_type}.  May enter the ultimate strength \eqn{Su} and/or the Brinnell Hardness Number (BHN).  Leaving this option blank will result in an estimate of \eqn{Su} and endurance limit \eqn{Se} based on the given data.
}
}
\details{
This code is desgned to create a general S-N diagram (the basis for the life-stress approach) based on available fully reversible data points in the form of alternating or applied stress and number of cycles to failure.  Additional data may be applied if known in the \code{options} input in the form of the ultimate strength \eqn{Su} and/or the Brinnell Hardness Number (BHN).
}
\value{
The code will produce the S-N diagram with a 95\% confidence bound about the mean S-N curve.  The command window will print the estimate for the mean strength at 1000 fatigue cycles, the estimated (or known) ultimate strength \eqn{Su}, and the estimated endurance limit \eqn{Se}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Reuel Smith (2022)
}
\note{
Options for \code{input_type} = 2 and 3 ar currently unavailable for v1.0.  They will be activated in a later build.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Example 1: List amplitude stress (in MPa) and cycles to failure
datSN1 <- list(c(340,300,290,275,260,255,250,235,230,220,215,210),c(15e3,24e3,36e3,80e3,177e3,162e3,301e3,290e3,361e3,881e3,1.3e6,2.5e6),c(210,210,205,205,205),c(1e7,1e7,1e7,1e7,1e7))
SN.diagram(1,datSN1,1)

# Example 2a: List amplitude stress (in ksi) and cycles to failure.  Also includes known ultimate strength and BHN.
datSN_MatA <- list(c(32.3,30.3,27.9,25.9,25.4,24.4),c(4.5e4,2.4e5,8e5,1.5e6,2.7e6,7.8e6),c(24.4,24,23.6,23.5),c(1e7,2.6e7,1.2e7,2.2e7))
Su_MatA <- 42
BHN_MatA <- 69
SN.diagram(1,datSN_MatA,2,list(Su = Su_MatA, BHN = BHN_MatA))

# Example 2b: List amplitude stress (in ksi) and cycles to failure.  Also includes known ultimate strength and BHN.
datSN_MatB <- list(c(81.4,74.7,71.6,64.7,62.1,59.6,58.8,58.7),c(4.4e4,8.5e4,1.4e5,6.3e5,1.9e6,2.9e6,6.4e5,1.4e6),c(57.2,56.2),c(1e8,9e7))
Su_MatB <- 102.6
BHN_MatB <- 209
SN.diagram(1,datSN_MatB,2,list(Su = Su_MatB, BHN = BHN_MatB))

# Example 2c: List amplitude stress (in ksi) and cycles to failure.  Also includes known ultimate strength and BHN.
datSN_MatC <- list(c(110,105,100,98,95),c(2.4e4,3.1e4,4.5e4,8.7e4,1.5e5),c(92,90),c(1e8,1e8))
Su_MatC <- 180
BHN_MatC <- 370
SN.diagram(1,datSN_MatC,2,list(Su = Su_MatC, BHN = BHN_MatC))
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
