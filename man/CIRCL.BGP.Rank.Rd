\name{CIRCL.BGP.Rank}
\alias{CIRCL.BGP.Rank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CIRCL.BGP.Rank
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CIRCL.BGP.Rank(asn, circl.base.url = "http://bgpranking.circl.lu/csv/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{asn}{
%%     ~~Describe \code{asn} here~~
}
  \item{circl.base.url}{
%%     ~~Describe \code{circl.base.url} here~~
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
function (asn, circl.base.url = "http://bgpranking.circl.lu/csv/") 
{
    return(read.csv(sprintf("\%s\%s", circl.base.url, asn)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
