\name{BulkPeer}
\alias{BulkPeer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BulkPeer
}
\description{
Retrieves BGP Peer ASN info for a list of IP addresses
}
\usage{
BulkPeer(ip.list, host = "v4-peer.whois.cymru.com", port = 43)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ip.list}{
Character vector of IP addresses (2 or more)
}
  \item{host}{
Server to hit for lookup (defaults to Team Cymru's server)
}
  \item{port}{
TCP port to use (defaults to 43)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame of BGP Peer ASN lookup results
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bob Rudis
}
\note{
IPv4 version

The Team Cymru's service is NOT a GeoIP service! Do not use this function for that as your results will notbe accurate.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
