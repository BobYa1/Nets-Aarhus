if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(version = "3.16")

BiocManager::install()

#Step 1. In case you don't have it yet, download R from http://www.r-project.org/. An excellent online introduction to R by Rob Cabacoff is available from http://www.statmethods.net/index.html.

#Step 2. Install and load required packages and functions for correlations and partial correlations graphs by running the code below.
install.packages("qgraph") #see http://www.jstatsoft.org/v48/i04/ for details 
library(qgraph) #load qgraph

#Step 3. Read in NCS-R data (http://www.hcp.med.harvard.edu/ncs/). In these data, that are missing by design due to skip questions have been recoded to zero (indicating absence of symptom) and duration criteria have been removed.
ncsdata=read.table(file="DepressionAnxiety.txt") #read in data
colnames(ncsdata)=c("depr", "inte", "weig", "mSle", "moto", "mFat", "repr", "conc", "suic", "anxi", "even", "ctrl", "edge", "gFat", "irri", "gCon", "musc", "gSle") #Define variable names.

#Step 4. Produce Figure 2a
n=15 #number of symptoms: 6 MD, 3 bridge symptoms and 6 GAD
     # Networks for symptoms of major depression (MD) and generalized anxiety disorder (GAD)
labels=c("depr","inte","weig","moto","repr","suic","slee","conc","fati","anxi","even","ctrl","irri","musc","edge")
groups=list(MD=1:6,Bridge=7:9,GAD=10:15)
data=matrix(c( #adjacency matrix
0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
1,0,1,1,1,1,1,1,1,0,0,0,0,0,0,
1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,
1,1,1,0,1,1,1,1,1,0,0,0,0,0,0,
1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,
1,1,1,1,1,0,1,1,1,0,0,0,0,0,0,
1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,
0,0,0,0,0,0,1,1,1,0,1,1,1,1,1,
0,0,0,0,0,0,1,1,1,1,0,1,1,1,1,
0,0,0,0,0,0,1,1,1,1,1,0,1,1,1,
0,0,0,0,0,0,1,1,1,1,1,1,0,1,1,
0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,
0,0,0,0,0,0,1,1,1,1,1,1,1,1,0
),n,n)
qgraph(data,filename="Figure 2A",filetype="pdf",groups=groups,legend=T,labels=labels,color=c("red","green","lightblue"))

#Step 5. Produce the correlation network of Figure 2b.
ncscorrelationgraph=qgraph(cor(ncsdata),layout="spring",labels=colnames(ncsdata))

#Step 6. Produce the partial correlation graph of Figure 5. 
n=5 #number of symptoms of specific phobia
data=read.table("SpecificPhobia.txt",header=F) #read specific phobia data
colnames(data)=c("fear","expo","recg","avoi","dist") #define variable names
install.packages("ppcor") #install ppcor package for computing partial correlations
library(ppcor)
Adj=round(matrix(pcor(data)$estimate,n,n),2) #create adjacency matrix with partial correlations as weights
Adj=ifelse((abs(Adj)<.10),0,Adj)#set all partial correlations with absolute value <0.10 to 0
qgraph(Adj,layout="spring",labels=colnames(data),edge.labels=T)

#Step 7. Install and load the pcalg necessary to run the PC algorithm. Depending on your setup this installation may not be entirely smooth yet. A guide is available from Cosma Shalizi's blog: http://cscs.umich.edu/~crshalizi/weblog/914.html
# install.packages("pcalg")


# #check if packageN is installed
# system.file(package='packageN')


# source("http://bioconductor.org/biocLite.R")
# biocLite("RBGL")

if (!require("graph", quietly = TRUE))   # http://www.bioconductor.org/packages/release/bioc/html/graph.html
    # install.packages("BiocManager")
  BiocManager::install("graph")

library(graph)


if (!require("RBGL", quietly = TRUE))
    # install.packages("BiocManager")
  BiocManager::install("RBGL")

library(RBGL)

if (!require("pcalg", quietly = TRUE))
    # install.packages("BiocManager")
  BiocManager::install("pcalg")

library(pcalg)


# install.packages("pcalg")
# install.packages("pcalg", type="binary")
# library(pcalg)

##Step 8. Produce the directed network of Figure 7. Note that this takes a while due to the many conditional independence relations that the program needs to assess.
ncsdata <- data.matrix(ncsdata) # transform from factor to numeric
colnames(ncsdata)=c("depr", "inte", "weig", "mSle", "moto", "mFat",
"repr", "conc", "suic", "anxi", "even", "ctrl", "edge", "gFat",
"irri", "gCon", "musc", "gSle") #Define variable names.
p=ncol(ncsdata)
## define independence test to apply to binary items
indepTest=binCItest
## define sufficient statistics
suffStat=list(dm = ncsdata, adaptDF = FALSE)
## define significance level used to test conditional independencies
alpha=0.05
## determine directed graph
# apcncsdata=pc(suffStat, indepTest, p, alpha=alpha, verbose = TRUE)
pcncsdata=pc(suffStat, indepTest, alpha=alpha, labels=colnames(ncsdata), p=p, verbose = TRUE)

##plot directed graph
ncsdirectedgraph=qgraph(pcncsdata, esize=1, edge.color="black", directed=T, labels=colnames(ncsdata))

##Step 9. Produce the fictitious networks of Bob and Alice in Figure 8
n=15 #number of symptoms: 6 MD, 3 bridge symptoms and 6 GAD
library(qgraph) #load qgraph
labels=c("depr","inte","weig","moto","repr","suic","slee","conc","fati","anxi","even","ctrl","irri","musc","edge") #define variable names
groups=list(MD=1:6,Bridge=7:9,GAD=10:15) #make three groups: MD, Bridge and GAD
Bob=matrix(c( #Bob's adjacency matrix
0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,1,0,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,1,0.1,1,0.1,0.1,0.1,0.1,0
),n,n)
qgraph(Bob,filename="Figure 8 Bob",filetype="pdf",groups=groups,layout="circular",legend=T,labels=labels,color=c("red","green","lightblue"))

Alice=matrix(c( #Alice's adjacency matrix
0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
1,0.1,0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,0.1,1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,1,0,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,1,0.1,0.1,0.1,0.1,0,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0),n,n)
qgraph(Alice,filename="Figure 8 Alice",filetype="pdf",groups=groups,layout="circular",legend=T,labels=labels,color=c("red","green","lightblue"))











