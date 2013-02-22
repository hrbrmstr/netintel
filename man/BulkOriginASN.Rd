\name{BulkOriginASN}
\alias{BulkOriginASN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BulkOriginASN
}
\description{
Retrieves BGP Origin ASN info for a list of ASN ids

It also seems to fail if you only search for one ASN address
}
\usage{
BulkOriginASN(asn.list, host = "v4.whois.cymru.com", port = 43)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{asn.list}{
character vector of ASNs (2 or more); each prefixed with "AS"
}
  \item{host}{
which server to use for the lookup (defaults to Team Cymru's)
}
  \item{port}{
port to use for connection (defaults to 43)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame of BGP Origin ASN lookup results
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bob Rudis
}
\note{
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
