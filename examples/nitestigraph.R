library(igraph)
library(netintel)
library(plyr)

#
# test script for a few of the netintel libraries
#

# load AlienValut data
z = Alien.Vault.Reputation()

# populate some IPs
ips = c("100.43.81.11","100.43.81.7","107.20.39.216","108.166.87.63","109.152.4.217","109.73.79.58","119.235.237.17","128.12.248.13","128.221.197.57","128.221.197.60","128.221.224.57","129.241.249.6","134.226.56.7","137.157.8.253","137.69.117.58","142.56.86.35","146.255.96.169","150.203.4.24","152.62.109.57","152.62.109.62","160.83.30.185","160.83.30.202","160.83.72.205","161.69.220.1","168.159.192.57","168.244.164.254","173.165.182.190","173.57.120.151","175.41.236.5","176.34.78.244","178.85.44.139","184.172.0.214","184.72.187.192","193.164.138.35","194.203.96.184","198.22.122.158","199.181.136.59","204.191.88.251","204.4.182.15","205.185.121.149","206.112.95.181","206.47.249.246","207.189.121.46","207.54.134.4","209.221.90.250","212.36.53.166","216.119.144.209","216.43.0.10","23.20.117.241","23.20.204.157","23.20.9.81","23.22.63.190","24.207.64.10","24.64.233.203","37.59.16.223","49.212.154.200","50.16.130.169","50.16.179.34","50.16.29.33","50.17.13.221","50.17.43.219","50.18.234.67","63.71.9.108","64.102.249.7","64.31.190.1","65.210.5.50","65.52.1.12","65.60.80.199","66.152.247.114","66.193.16.162","66.249.71.143","66.249.71.47","66.249.72.76","66.41.34.181","69.164.221.186","69.171.229.245","69.28.149.29","70.164.152.31","71.127.49.50","71.41.139.254","71.87.20.2","74.112.131.127","74.114.47.11","74.121.22.10","74.125.178.81","74.125.178.82","74.125.178.88","74.125.178.94","74.176.163.56","76.118.2.138","76.126.174.105","76.14.60.62","76.168.198.238","76.22.130.45","77.79.6.37","81.137.59.193","82.132.239.186","82.132.239.97","8.28.16.254","83.111.54.154","83.251.15.145","84.61.15.10","85.90.76.149","88.211.53.36","89.204.182.67","93.186.30.114","96.27.136.169","97.107.138.192","98.158.20.231","98.158.20.237")

# let's only look at C&C servers
ips = z[grep("C&C",z$Activity,fixed=TRUE),1]

# get BGP origin & peers
origin = BulkOrigin(ips)
peers = BulkPeer(ips)

# start graphing
g = graph.empty()

# Make IP vertices; IP endpoints are red
g = g + vertices(ips,size=1,color="red",group=1)

# Make BGP vertices ; BGP nodes are light blue
g = g + vertices(unique(c(peers$Peer.AS, origin$AS)),size=1.5,color="orange",group=2)

# no labels
V(g)$label = ""

# Make IP/BGP edges
ip.edges = lapply(ips,function(x) {
  iAS = origin[origin$IP==x,]$AS
  lapply(iAS,function(y){
    c(x,y)
  })
})

# Make BGP/peer edges
bgp.edges = lapply(unique(origin$BGP.Prefix),function(x) {
  startAS = unique(origin[origin$BGP.Prefix==x,]$AS)
  lapply(startAS,function(z) {
    pAS = peers[peers$BGP.Prefix==x,]$Peer.AS
    lapply(pAS,function(y) {
      c(z,y)
    })
  })
})

# get total graph node count
node.count = table(c(unlist(ip.edges),unlist(bgp.edges)))

# add edges 
g = g + edges(unlist(ip.edges))
g = g + edges(unlist(bgp.edges))

# base edge weight == 1
E(g)$weight = 1

# size nodes according to connectivity
#V(g)$size = 2

# simplify the graph
g = simplify(g, edge.attr.comb=list(weight="sum"))

# no arrows
E(g)$arrow.size = 0

# best layout for this
L = layout.fruchterman.reingold(g)

# plot the graph
plot(g,margin=0)
