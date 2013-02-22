\name{CIRCL.BGP.Rank}
\alias{CIRCL.BGP.Rank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CIRCL.BGP.Rank
}
\description{
Retrieves CIRCL aggregated, historical/current BGP rank data
}
\usage{
CIRCL.BGP.Rank(asn, circl.base.url = "http://bgpranking.circl.lu/csv/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{asn}{
ASN list to lookup
}
  \item{circl.base.url}{
lookup URL (optional parameter settable here in the event it changes)
}
}
\details{
Background on CIRCL project (+ source): https://github.com/CIRCL/bgp-ranking


}
\value{
Data frame of CIRCL rank data
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bob Rudis
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
CIRCL BGP site: http://bgpranking.circl.lu/
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
