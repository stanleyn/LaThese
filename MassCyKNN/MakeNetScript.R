#Creating a K nearest neighbor network from single cell mass cytometry data
##I am using data from CytofKit
#Following the tutotial here, i performed a subset selection of 500 nodes
#https://github.com/JinmiaoChenLab/cytofkit/blob/master/vignettes/cytofkit_example.Rmd
#February 1, 2018




library('FastKNN')

setwd('~/Dropbox/Dissertation/MassCyKNN')

Dat=readRDS('massCyData')

##build KNN greaph
# k=5
# dist_mat <- as.matrix(dist(Dat, method = "euclidean", upper = TRUE, diag=TRUE))
# nrst <- lapply(1:nrow(dist_mat), function(i) k.nearest.neighbors(i, dist_mat, k = k))
# w <- matrix(nrow = dim(dist_mat), ncol=dim(dist_mat)) ## all NA right now
# w[is.na(w)] <- 0 ## populate with 0
# for(i in 1:length(nrst)) for(j in nrst[[i]]) w[i,j] = 1

# ##plot the network
# Net=graph.adjacency(w,mode='undirected')
# #quartz()

lay=layout_nicely(Net)

# postscript('MassCytometryData.eps')
# plot(Net,vertex.label=NA,vertex.size=3,edge.color='gray88',vertex.color='palevioletred')
# dev.off()


###let's compute all network statistics

###degree
# deg.distr<-degree.distribution(Net,cumulative=T,mode="all")
# #quartz()

# par(mfrow=c(1,2))

# plot(deg.distr,log="xy",
# ylim=c(.01,10),
# col="black",pch=8,
# xlab="Degree",cex=1.2,
# ylab="Cumulative Frequency")

# #quartz()
# hist(degree(Net),col='darkturquoise',xlab='Degree',ylab='Fequency',main='')

###centrality

#degree centrality
# SortDegList=order(degree(Net),decreasing=TRUE)[1]
# SortBtwList=order(betweenness(Net,directed=FALSE),decreasing=TRUE)[1]
# SortEigen=order(eigen_centrality(Net,directed=FALSE)$vector,decreasing=TRUE)[1]

# #plot second order neighborhoods of these nodes
# quartz()
# par(mfrow=c(1,3))

# EgoDeg=make_ego_graph(Net,order=2,nodes=SortDegList)[[1]]
# V(EgoDeg)$color='darkorchid2'
# plot(EgoDeg,vertex.size=7,vertex.label=NA,main='degree centrality')

# EgoBtw=make_ego_graph(Net,order=2,nodes=SortBtwList)[[1]]
# V(EgoBtw)$color='darkorchid2'

# plot(EgoBtw,vertex.size=7,vertex.label=NA,main='betweennness centrality')

# EgoEigen=make_ego_graph(Net,order=2,nodes=SortEigen)[[1]]
# V(EgoEigen)$color='darkorchid2'

# plot(EgoEigen,vertex.size=7,vertex.label=NA,main='eigenvector centrality')

###community assignments
#tsnemodel=Rtsne(Dat,dims=2)
#tsnemodel2=as.matrix(tsnemodel$Y)

###cluster with louvain and plot
louvRes=membership(cluster_louvain(Net))
KmeanRes=kmeans(Dat,centers=10)$cluster
#quartz()

postscript('clustering.eps',width=7,height=5)
par(mfrow=c(1,2))
plot(tsnemodel2,col=as.factor(KmeanRes),pch=19,cex=1.2,xlab='tSNE 1',ylab='tSNE 2',main='kmeans')
plot(tsnemodel2,col=as.factor(louvRes),pch=19,cex=1.2,xlab='tSNE 1',ylab='tSNE 2',main='Louvain')
dev.off()
