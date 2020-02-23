#Now that we know which nodes have high betweeness centrality, we can look at 
#Eigen Vector Centrality which looks at the importance of connected nodes being 
#connected to other highly connected people. 

#Eigen Vector Centrality - this gives us a manipulation matrix. 
ZNeigen<- eigen_centrality(ZN)

#Now we make a dataframe to analyze the values of each node 
ZNeigen<-as.data.frame(ZNeigen)

#Look at dataframe to see the variation between eigen values. We can see that 
#there is quite a bit of variation between the values. 

# We can create a column that has the node ID next to each value
ZNeigen$Nodelist<-(1:27)

#Now we repeat with the Songbirds data 
#First calculated Betweeness
SNeigen<-eigen_centrality(SN)

#Remake into a dataframe for analysis
SNeigen<-as.data.frame(SNeigen)

# We can create a column that has their node ID next to each value
SNeigen$Nodelist<-(1:117)

#Now we can write these into csvs for future reference

#Zebra network
write.csv(ZNeigen,file=paste(t.path,"Zebra Eigen Vetor Values.csv",sep="/"))

#Songbird network
write.csv(SNeigen,file=paste(t.path,"Songbird Eigen Vetor Values.csv",sep="/"))

#Now we can graph the eigen vector centrality to see who is most connected to 
#other very connected people. 

# Now that we know which players have the highest Betweeness Centrality, we can 
# regraph our networks with them highlighted in a different color. 

############################## Final Graphs ####################################

# Graphs of 1 player

#Zebra
#First we find our top 1 player as measured for Eigen Vector centrallity 
top1ZNeigen<-ZNeigen%>%top_n(1,ZNeigen$vector)

#An important note is that it is our second column we want
#next we highlight those players

LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)

#node options
V(ZN)$color<-"grey"
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"

#Now we can plot it!
plot(ZN)

#lets save this as a pdf for future reference
pdf(file=paste(t.path,"Zebra top 1 Eigen.pdf",sep="/"))
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"
plot(ZN)
dev.off()

# Songbird
#First we find our top player
top1SNeigen<-ZNeigen%>%top_n(1,SNeigen$vector)

#An important note is that it is our second column we want
#next we highlight those players

LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)

#node options
V(SN)$color<-"grey"
V(SN)[top1ZNeigen[,23]]$color<-"yellow"

#Now we can plot it!
plot(SN)


#save as a pdf
pdf(file=paste(t.path,"Songbird top 1 Eigen.pdf",sep="/"))
LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top1SNeigen[,23]]$color<-"yellow"
plot(SN)
dev.off()

# Graphs of 3% (same as 1 player for Zebra)

#Zebra
#First we find our top 3% of players
#total nodes in Zebra are 27, 3% comes out to.81, 
#we round up to 1 player (same as last time)
top1ZNeigen<-ZNeigenn%>%top_n(1,ZNeigen$vector)

#Again note our second column is the nodelist
#next we highlight those players
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"
plot(ZN)

#save as a pdf
pdf(file=paste(t.path,"Zebra top 3 Eigen.pdf",sep="/"))
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"
plot(ZN)
dev.off()


#Songbird
#First we find our top 3% of players
#total nodes in Songbirds are 117 3% comes 
#out to 3.5 we round up to 4 players

top4SNeigen<-SNeigen%>%top_n(4,SNeigen$vector)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SNeigen[,23]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 3.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SN[,23]]$color<-"yellow"
plot(SN)
dev.off()


# Graphs of 10%

#Zebra
#First we find our top 10% of players
#total nodes in Zebra are 27, 10% comes out to 2.7, 
#we round up to 3 players
top3ZNeigen<-ZNeigen%>%top_n(3,ZNeigen$vector)


#Again note our second column is the nodelist
#next we highlight those players
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZNeigen[,23]]$color<-"yellow"
plot(ZN)

#Here we have 4 nodes that are teh top 10 percent because there is a tie between 
#2 nodes and therefore there are not 3, but 4

#Save as a pdf
pdf(file=paste(t.path,"Zebra top 10 Eigen.pdf",sep="/"))
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZNeigen[,23]]$color<-"yellow"
plot(ZN)
dev.off()

#Songbird
#First we find our top 10% of players
#total nodes in Songbirds are 117 10% comes 
#out to 11.7 we round up to 12 players

top12SNeigen<-SNeigen%>%top_n(12,SNeigen$vector)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SNeigen[,23]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 10 Eigen.pdf"))
LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SNeigen[,23]]$color<-"yellow"
plot(SN)
dev.off()


#Now that we know which nodes have high node- betweeness centrality we can look 
#at which edges have high betweeness. This is called Girvan-Newman
#Edge-Betweeness: Girvan-Newman - this looks at the edge that connects the most 
#amount of people and then it removes those edges and looks at the nodes that are 
#left. You continue till you have no more edges and from that tou can understand 
#what are the groups of people that have more connections to themselves then other
#people in the network. 
GNC <- cluster_edge_betweenness(DN_graph2, weights = NULL)
V(DN_graph2)$color <-membership(GNC) #This sets a specific colour for the nodes 
#by different community
DN_graph2$palette <- diverging_pal(length(GNC)) #This sets a specific color pallette 
plot(DN_graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)

#First calculate Girvan- Newman
ZN_GN<-cluster_edge_betweenness(ZN, weights= NULL)

#Now we make a dataframe to analyze the values of each node 
ZN_GN<-as.data.frame(ZN_GN)

# We can create a column that has the node ID next to each value
ZN_GN$Nodelist<-(1:27)

str(ZN_GN)
head

#HOW TO MAKE A DATAFRAME WITH THIS???



# lastly we can arrange them in descending order, so we know to only highlight
# the top IDs in the final graphing portion
arrange(ZNbetween,-ZNbetween)

#Now again with Songbirds
#First calculated Betweeness
SNbetween<-betweenness(SN)

#Remake into a dataframe for analysis
SNbetween<-as.data.frame(SNbetween)

# We can create a column that has their node ID next to each value
SNbetween$Nodelist<-(1:117)

#Next we can arrange them in descending order
arrange(SNbetween,-SNbetween)

#Now we can write these into csvs for future reference

#Zebra network
write.csv(ZNbetween,file=paste(t.path,"Zebra Betweeness Values.csv",sep="/"))

#Songbird network
write.csv(SNbetween,file=paste(t.path,"Songbird Betweeness Values.csv",sep="/"))

# Below we graph who we would immunize with specific immunization criteria

