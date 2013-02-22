\name{SANS.ASN.Detail}
\alias{SANS.ASN.Detail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SANS.ASN.Detail
}
\description{
Return SANS ASN currently tracked IP detail 
}
\usage{
SANS.ASN.Detail(asn, sans.base.url = "http://isc.sans.edu/asdetailsascii.html?as=")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{asn}{
ASN to lookup
}
  \item{sans.base.url}{
Lookup URL (optional parameter in the event it changes)
}
}
\details{

}

\value{
Data frame of SANS ASN IP data
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bob Rudis
}
\note{
If you use the "https" URL access, you'll need to change the source on some systems that can't read https directly
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  # https://isc.sans.edu/asdetailsascii.html?as=21844
  # 
  # created: Fri, 08 Feb 2013 14:17:01 +0000#
  # Source IP is 0 padded so each byte is three digits long
  # Reports: number of packets received
  # Targets: number of target IPs that reported packets from this source.
  # First Seen: First time we saw a packet from this source
  # Last Seen: Last time we saw a packet from this source
  # Updated: Last time the record was updated.
  #
  # IPs are removed if they have not been seen in 30 days.
  #
  # source IP <tab> Reports <tab> Targets <tab> First Seen <tab> Last Seen <tab> Updated <CR>
  # 118.102.187.188  4495  3909  2010-05-12  2013-02-02	2013-02-02 10:42:01
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
