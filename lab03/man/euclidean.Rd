\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidean(value1, value2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value1}{
%%     ~~Describe \code{value1} here~~
}
  \item{value2}{
%%     ~~Describe \code{value2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (value1, value2) 
{
    stopifnot(is.numeric(value1) && length(value1) == 1 && value1 > 
        0)
    stopifnot(is.numeric(value2) && length(value2) == 1 && value2 > 
        0)
    while (value1 != value2) {
        if (value1 > value2) {
            value1 <- value1 - value2
        }
        else {
            value2 <- value2 - value1
        }
    }
    return(value1)
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
