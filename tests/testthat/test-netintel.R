context("Team CYMRU")

test_that("we can perform Team CYMRU functions", {
  
  # success
  expect_that(BulkOrigin("162.243.111.4"), equals(structure(list(AS = 62567, IP = "162.243.111.4", BGP.Prefix = "162.243.0.0/17", 
                                                                 CC = "US", Registry = "arin", Allocated = "2013-09-06", AS.Name = "DIGITALOCEAN-ASN-NY2 - Digital Ocean, Inc.,US"), .Names = c("AS", 
                                                                                                                                                                                                "IP", "BGP.Prefix", "CC", "Registry", "Allocated", "AS.Name"), row.names = c(NA, 
                                                                                                                                                                                                                                                                             -1L), class = "data.frame")))
  expect_that(BulkOriginASN(62567), equals(structure(list(AS = 62567, CC = "US", Registry = "arin", Allocated = "2013-07-11", 
                                                   AS.Name = "DIGITALOCEAN-ASN-NY2 - Digital Ocean, Inc.,US"), .Names = c("AS", 
                                                                                                                          "CC", "Registry", "Allocated", "AS.Name"), row.names = c(NA, 
                                                                                                                                                                                   -1L), class = "data.frame")))

  
  expect_that(BulkPeer("162.243.111.4"), equals(structure(list(Peer.AS = c(174, 286, 3257, 3356, 4565, 22822), 
                                                        IP = c("162.243.111.4", "162.243.111.4", "162.243.111.4", 
                                                               "162.243.111.4", "162.243.111.4", "162.243.111.4"), BGP.Prefix = c("162.243.0.0/17", 
                                                                                                                                  "162.243.0.0/17", "162.243.0.0/17", "162.243.0.0/17", "162.243.0.0/17", 
                                                                                                                                  "162.243.0.0/17"), CC = c("US", "US", "US", "US", "US", "US"
                                                                                                                                  ), Registry = c("arin", "arin", "arin", "arin", "arin", "arin"
                                                                                                                                  ), Allocated = c("2013-09-06", "2013-09-06", "2013-09-06", 
                                                                                                                                                   "2013-09-06", "2013-09-06", "2013-09-06"), Peer.AS.Name = c("COGENT-174 - Cogent Communications,US", 
                                                                                                                                                                                                               "KPN KPN International / KPN Eurorings,NL", "TINET-BACKBONE Tinet SpA,DE", 
                                                                                                                                                                                                               "LEVEL3 - Level 3 Communications, Inc.,US", "MEGAPATH2-US - MegaPath Networks Inc.,US", 
                                                                                                                                                                                                               "LLNW - Limelight Networks, Inc.,US")), .Names = c("Peer.AS", 
                                                                                                                                                                                                                                                                  "IP", "BGP.Prefix", "CC", "Registry", "Allocated", "Peer.AS.Name"
                                                                                                                                                                                                               ), row.names = c(NA, -6L), class = "data.frame")))  
})

context("CIRCL")

test_that("we can perform CIRCL functions", {

  expect_that(tail(CIRCL.BGP.Rank(62567))[1,1], equals(62567))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  
}) 

# context("SANS")
# 
# test_that("we can perform SANS functions", {
#   
#   expect_that(SANS.ASN.Detail(62567), is_a("data.frame"))
#   #expect_that(gethostbyname("f0011"), equals(character(0)))
#   #expect_that(gethostbyname("f0011"), equals(character(0)))
# 
# })

context("AlienVault")

test_that("we can perform AlienVault function", {
  
  expect_that(Alien.Vault.Reputation(), is_a("data.table"))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  
})

context("Zeus")

test_that("we can perform Zeus function", {
  
  expect_that(Zeus.Blocklist(), is_a("list"))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  
})


context("Nothink")

test_that("we can perform Nothink function", {
  
  expect_that(Nothink.Blocklist(), is_a("list"))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  #expect_that(gethostbyname("f0011"), equals(character(0)))
  
})