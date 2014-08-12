# short function to trim leading/trailing whitespace
trim <- function (x) ifelse(is.na(x), NA, gsub("^\\s+|\\s+$", "", x))

# short function to trim leading/trailing whitespace from all character columns
trimdf <- function(df, stringsAsFactors=FALSE) {
  data.frame(lapply(df, function (v) {
    if (is.character(v)) {
      trim(v)
    } else {
      v
    }
  }), stringsAsFactors=stringsAsFactors)
}

#' @title Retrieves BGP Origin ASN info for a list of IPv4 addresses
#' @description Returns a list (slots are named by the input IPv4 addresses) 
#'              with lookup results per slot
#' @param ip.list vector of IPv4 address (character - dotted-decimal)
#' @param host which server to perform the lookup (chr) - 
#'        defaults to \code{v4.whois.cymru.com}
#' @param port TCP port to use to connect to \code{host} (int) - 
#'        defaults to port \code{43}
#' @return data frame of BGP Origin ASN lookup results
#'   \itemize{
#'     \item \code{AS} - AS #
#'     \item \code{IP} - IPv4 (passed in)
#'     \item \code{BGP.Prefix} - BGP CIDR
#'     \item \code{CC} - Country code
#'     \item \code{Registry} - Registry it falls under
#'     \item \code{Allocated} - date it was allocated
#'     \item \code{AS.Name} - AS name
#'   }
#' @note The Team Cymru's service is NOT a GeoIP service! Do not use this 
#'       function for that as your results will not be accurate.
#' @seealso \url{http://www.team-cymru.org/Services/}
#' @export
#'
BulkOrigin <- function(ip.list, host="v4.whois.cymru.com", port=43) {
   
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n", cmd, ips)
  
  # setup connection and post query
  con <- socketConnection(host=host, port=port, blocking=TRUE, open="r+")  
  cat(cmd, file=con)
  response <- readLines(con)
  close(con)

  # trim header, split fields and convert results
  response <- trimdf(read.csv(textConnection(response[2:length(response)]), 
                              stringsAsFactors=FALSE, sep="|", header=FALSE))
  names(response) <- c("AS", "IP", "BGP.Prefix", "CC", 
                       "Registry", "Allocated", "AS.Name")
  response[response=="NA"] <- NA
  
  return(response)
  
}

#' @title Retrieves BGP Peer ASN info for a list of IPv4 addresses
#' @description Retrieves BGP Peer ASN info for a list of IPv4 addresses
#' @param ip.list vector of IPv4 address (character - dotted-decimal)
#' @param host which server to perform the lookup (chr) - 
#'        defaults to \code{v4.whois.cymru.com}
#' @param port TCP port to use to connect to \code{host} (int) - 
#'        defaults to \code{43}
#' @return data frame of BGP Peer ASN lookup results
#'   \itemize{
#'     \item \code{Peer.AS} - peer AS #
#'     \item \code{IP} - IPv4 (passsed in)
#'     \item \code{BGP.Prefix} - BGP CIDR block
#'     \item \code{CC} - Country code
#'     \item \code{Registry} - Registry it falls under
#'     \item \code{Allocated} - date allocated
#'     \item \code{Peer.AS.Name} - peer name
#'   }
#' @note The Team Cymru's service is NOT a GeoIP service! Do not use this 
#'       function for that as your results will not be accurate.
#' @seealso \url{http://www.team-cymru.org/Services/}
#' @export
#'
BulkPeer <- function(ip.list, host="v4-peer.whois.cymru.com", port=43) {
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n", cmd, ips)
  
  # setup connection and post query
  con <- socketConnection(host=host, port=port, blocking=TRUE, open="r+")  
  cat(cmd, file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- trimdf(read.csv(textConnection(response[2:length(response)]), 
                              stringsAsFactors=FALSE, sep="|", header=FALSE))
  names(response) <- c("Peer.AS", "IP", "BGP.Prefix", "CC", 
                       "Registry", "Allocated", "Peer.AS.Name")
  response[response=="NA"] <- NA
  
  return(response)
  
}

#' @title Retrieves BGP Origin ASN info for a list of ASN ids
#' @description Retrieves BGP Origin ASN info for a list of ASN ids
#' @param asn.list character vector of ASN ids (character)
#' @param host which server to perform the lookup (chr) - 
#'        defaults to \code{v4.whois.cymru.com}
#' @param port TCP port to use to connect to \code{host} (int) -
#'        defaults to \code{43}
#' @return data frame of BGP Origin ASN lookup results
#'   \itemize{
#'     \item \code{AS} - AS #
#'     \item \code{CC} - Country code
#'     \item \code{Registry} - registry it falls under
#'     \item \code{Allocated} - when it was allocated
#'     \item \code{AS.Name} - name associated with the allocation
#'   }
#' @note The Team Cymru's service is NOT a GeoIP service! Do not use this 
#'       function for that as your results will not be accurate.
#' @seealso \url{http://www.team-cymru.org/Services/}
#' @export
#'
BulkOriginASN <- function(asn.list, host="v4.whois.cymru.com", port=43) {
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ifelse(grepl("^AS", asn.list), asn.list, 
                             sprintf("AS%s", asn.list))), collapse="\n")
  cmd <- sprintf("%s%s\nend\n", cmd, ips)
  
  # setup connection and post query
  con <- socketConnection(host=host, port=port, blocking=TRUE, open="r+")  
  cat(cmd, file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- trimdf(read.csv(textConnection(response[2:length(response)]), 
                              stringsAsFactors=FALSE, sep="|", header=FALSE))
  names(response) <- c("AS", "CC", "Registry", "Allocated", "AS.Name")
  response[response=="NA"] <- NA
  
  return(response)
  
}

#' @title Retrieves CIRCL aggregated, historical/current BGP rank data
#' @description Retrieves CIRCL aggregated, historical/current BGP rank data
#' @param asn.list character vector of ASN ids (character)
#' @param circl.base.url CIRCL server base URL (chr) - 
#'        defaults to \code{http://bgpranking.circl.lu/csv/}
#' @return data frame of CIRCL rank data
#'   \itemize{
#'     \item \code{asn} asn # 
#'     \item \code{day} date
#'     \item \code{rank} current rank that day
#'   }
#' @seealso
#'   \itemize{
#'     \item Background on CIRCL Project (+source) \url{https://github.com/CIRCL/bgp-ranking}
#'     \item CIRCL BGP site \url{http://bgpranking.circl.lu/}
#'   }
#' @export
#' @examples
#' CIRCL.BGP.Rank(57954)
#'
CIRCL.BGP.Rank <- function(asn.list, 
                           circl.base.url="http://bgpranking.circl.lu/csv/") {
    
  ranks <- ldply(lapply( ifelse(grepl("^AS", asn.list), 
                                gsub("^AS", "", asn.list), asn.list), 
                         function(asn) {
                           cbind(asn, read.csv(sprintf("%s%s", 
                                                       circl.base.url, asn),
                                               stringsAsFactors=FALSE))  
  }), rbind)

  return(ranks)
  
}

#' @title Retrieves SANS ASN intel currently tracked IP detail 
#' @description Retrieves SANS ASN intel currently tracked IP detail 
#' @param asn ASN to lookup (character) - no \code{AS} prefix
#' @param sans.base.url SANS server base URL (chr) - defaults to
#'        \code{http://isc.sans.edu/asdetailsascii.html?as=}
#' @return data frame of SANS ASN IP data 
#'   \itemize{
#'     \item \code{Source.IP} is 0 padded so each byte is three digits long
#'     \item \code{Reports.Count} number of packets received
#'     \item \code{Targets.Count} number of target IPs that reported packets from this source
#'     \item \code{First.Seen} First time we saw a packet from this source
#'     \item \code{Last.Seen} Last time we saw a packet from this source
#'     \item \code{Updated.Date.Time} Last date+time the record was updated
#'   }
#' @note IPs are removed if they have not been seen in 30 days.
#' @seealso \url{https://isc.sans.edu/as.html}
#' @export
#'
SANS.ASN.Detail <- function(asn, sans.base.url="http://isc.sans.edu/asdetailsascii.html?as=") {
  

  asn <- gsub("^AS", "", asn)
  src <- GET(sprintf("%s%s", sans.base.url, asn))
  asn.df <- read.table(textConnection(content(src, as="text")), header=FALSE, sep="\t")
  names(asn.df) <- c("Source.IP", "Reports.Count", "Targets.Count", 
                     "First.Seen", "Last.Seen", "Updated.Date.Time")
  
  return(asn.df)
  
}


#' @title Retrieves Alien Vault's IP reputation database
#' @description Retrieves Alien Vault's IP reputation database.
#' @details   
#' AlienValut refreshes every hour, but the onus is on the caller to force a 
#' refresh. First-time call will setup a cache directory & file in the user's 
#' home directory, download & generate the data frame then write the data frame 
#' out as an R object. Future calls will just re-read this data frame unless 
#' \code{refresh == TRUE} should the function refresh the database.
#'    
#' Please be kind to the AlienValut servers & only refresh if you really need to.
#' @param refresh refresh the database? (bool)
#' @param alien.vault.reputation.url URL of the AlienVault data (chr) - 
#'        defaults to \code{http://reputation.alienvault.com/reputation.data}
#' @return data.table with IP & Reputation information.
#'   \itemize{
#'     \item \code{IP} - IPv4 address
#'     \item \code{Risk} - how risky is the target (1-10)
#'     \item \code{Reliability} - how reliable is the rating (1-10)
#'     \item \code{Activity} - what type of host is it
#'     \item \code{Country} - what is the IPv4 country of origin
#'     \item \code{City} - what is the IPv4 city of origin
#'     \item \code{Latitude} - geolocated latitude of the IPv4
#'     \item \code{Longitude} - geolocated longitude of the IPv4
#'   }
#' @seealso
#'   \itemize{
#'     \item Background on AlienValut's IP rep db: \url{http://labs.alienvault.com/labs/index.php/projects/open-source-ip-reputation-portal/download-ip-reputation-database/}
#'     \item More info on AlienVault's database: \url{http://www.slideshare.net/alienvault/building-an-ip-reputation-engine-tracking-the-miscreants}
#'   }
#' @export
#'
Alien.Vault.Reputation <- function(refresh=FALSE,  alien.vault.reputation.url="http://reputation.alienvault.com/reputation.data") {
  
  # TODO: What is field 8?
  # TODO: Need to split out the ";" separated factors?
  
  av.dir <- file.path(path.expand("~"), ".ipcache")
  av.file <-  file.path(av.dir, "alienvaultrep.rda")
  av.data.file <-  file.path(av.dir, "reputation.data")
  
  dir.create(av.dir, showWarnings=FALSE)

  if (refresh || file.access(av.file, 4)!=0) {
    
    suppressWarnings(av.dt <- fread(alien.vault.reputation.url, sep="#", 
                                    stringsAsFactors=FALSE))
    setnames(av.dt, colnames(av.dt), c("IP", "Risk", "Reliability", "Activity",
                                       "Country", "City", "LatLon", "x"))

    av.dt[, Latitude:=unlist(strsplit(LatLon, split=","))[[1]], by=LatLon]
    av.dt[, Longitude:=unlist(strsplit(LatLon, split=","))[[2]], by=LatLon]
    av.dt$LatLon <- NULL
    av.dt$x <- NULL
    
    setkey(av.dt, IP)

    av.dt$Risk <- factor(av.dt$Risk)
    av.dt$Reliability <- factor(av.dt$Reliability)
    av.dt$Country <- factor(av.dt$Country)
    av.dt$City <- factor(av.dt$City)
    
    save(av.dt, file=av.file, compress=TRUE)
    
  } else {
    av.df = load(av.file)
  }
  
  return(av.dt)
  
}

# http://www.nothink.org/blacklist/blacklist_malware_dns.txt
# Malware DNS network traffic blacklist (FQDN)

# http://www.nothink.org/blacklist/blacklist_malware_http.txt
# Malware HTTP network traffic blacklist (IP address)

# Malware IRC network traffic blacklist (IP address)
# http://www.nothink.org/blacklist/blacklist_malware_irc.txt
