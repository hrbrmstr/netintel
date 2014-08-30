---
title: "README"
author: "Bob Rudis"
date: "August 30, 2014"
output:
  md_document:
    variant: markdown_github
---

Version 1.2 adds the [Zeus](https://zeustracker.abuse.ch/blocklist.php) and [Nothink](http://www.nothink.org/) blocklists

Version 1.1 brings a significant update to the core components of the `netinel` package. Every function has been re-written to be as fast as possible without resorting to `Rcpp` functions. The intent of the package is to provide as many IP & ASN intelligence routines to those using R for Security Data Science and security intel/ops/IR work.

It relies on `httr`, `plyr` & `data.table`.

Current function list:

- `Alien.Vault.Reputation` - Retrieves Alien Vault's IP reputation database
- `BulkOrigin` - Retrieves BGP Origin ASN info for a list of IPv4 addresses
- `BulkOriginASN` - Retrieves BGP Origin ASN info for a list of ASN ids
- `BulkPeer` - Retrieves BGP Peer ASN info for a list of IPv4 addresses
- `CIRCL.BGP.Rank` - Retrieves CIRCL aggregated, historical/current BGP rank data
- `SANS.ASN.Detail` - Retrieves SANS ASN intel currently tracked IP detail
- `Zeus.Blocklist` - Retrieves Zeus Blocklist (IP/FQDN/URL)
- `Nothink.Blocklist` - Retrieves Nothink Malware DNS network traffic blacklist (IP/FQDN)

### Installation


```r
devtools::install_github("hrbrmstr/netintel")
library(netintel)
```

### Usage


```r
library(netintel)

# current verison
packageVersion("netintel")
```

```
## [1] '1.1.0'
```

```r
# Bulk stuff
BulkOrigin("162.243.111.4")
```

```
##      AS            IP     BGP.Prefix CC Registry  Allocated
## 1 62567 162.243.111.4 162.243.0.0/17 US     arin 2013-09-06
##                                         AS.Name
## 1 DIGITALOCEAN-ASN-NY2 - Digital Ocean, Inc.,US
```

```r
BulkOriginASN(62567)
```

```
##      AS CC Registry  Allocated
## 1 62567 US     arin 2013-07-11
##                                         AS.Name
## 1 DIGITALOCEAN-ASN-NY2 - Digital Ocean, Inc.,US
```

```r
BulkPeer("162.243.111.4")
```

```
##   Peer.AS            IP     BGP.Prefix CC Registry  Allocated
## 1     174 162.243.111.4 162.243.0.0/17 US     arin 2013-09-06
## 2     286 162.243.111.4 162.243.0.0/17 US     arin 2013-09-06
## 3    3257 162.243.111.4 162.243.0.0/17 US     arin 2013-09-06
## 4    3356 162.243.111.4 162.243.0.0/17 US     arin 2013-09-06
## 5    4565 162.243.111.4 162.243.0.0/17 US     arin 2013-09-06
## 6   22822 162.243.111.4 162.243.0.0/17 US     arin 2013-09-06
##                               Peer.AS.Name
## 1    COGENT-174 - Cogent Communications,US
## 2 KPN KPN International / KPN Eurorings,NL
## 3              TINET-BACKBONE Tinet SpA,DE
## 4 LEVEL3 - Level 3 Communications, Inc.,US
## 5 MEGAPATH2-US - MegaPath Networks Inc.,US
## 6       LLNW - Limelight Networks, Inc.,US
```

```r
# CIRCL

head(CIRCL.BGP.Rank(62567))
```

```
##     asn        day  rank
## 1 62567 2014-06-30 1.001
## 2 62567 2013-07-27 1.000
## 3 62567 2014-03-28 1.003
## 4 62567 2011-04-30 1.000
## 5 62567 2013-09-09 1.004
## 6 62567 2013-09-08 1.004
```

```r
# SANS was flaky so no example

# SANS.ASN.Detail(62567)

# AlienVault

head(Alien.Vault.Reputation())
```

```
##               IP Risk Reliability       Activity Country      City
## 1:   1.0.131.184    1           2 Malicious Host      TH          
## 2: 1.121.142.154    4           2 Malicious Host      AU          
## 3: 1.121.164.195    4           2 Malicious Host      AU Coorparoo
## 4:    1.123.40.5    4           2 Malicious Host      AU  Adelaide
## 5: 1.133.228.176    4           2 Malicious Host      AU          
## 6:  1.159.48.252    4           2 Malicious Host      AU          
##          Latitude     Longitude
## 1:           15.0         100.0
## 2:          -27.0         133.0
## 3:          -27.5 153.050003052
## 4: -34.9286994934 138.598602295
## 5:          -27.0         133.0
## 6:          -27.0         133.0
```

```r
# Zeus

str(Zeus.Blocklist())
```

```
## List of 3
##  $ domains:'data.frame':	856 obs. of  1 variable:
##   ..$ domain: chr [1:856] "039b1ee.netsolhost.com" "03a6b7a.netsolhost.com" "03a6f57.netsolhost.com" "1day.su" ...
##  $ ips    :'data.frame':	213 obs. of  1 variable:
##   ..$ IP: chr [1:213] "103.241.0.100" "103.4.52.150" "103.7.59.135" "107.181.174.84" ...
##  $ urls   :'data.frame':	673 obs. of  1 variable:
##   ..$ URL: chr [1:673] "190.104.217.181/~ssiprueb/wp-includes/css/b.exe" "190.104.217.181/~ssiprueb/wp-includes/css/cfg.bin" "190.104.217.181/~ssiprueb/wp-includes/css/login.php" "210.37.11.238/jm32/includes/site/bot.exe" ...
```

```r
# Nothink

str(Nothink.Blocklist)
```

```
## function (refresh = FALSE, nothink_url = "http://www.nothink.org/blacklist/blacklist_malware_dns.txt")
```

### Test Results


