
################################################################################

#                              TESTING TECHNIQUES                           #       

################################################################################

############################# CREATING NETWORK GRAPHS ##########################
# In our Main document, we read in our SN and ZN networks, which are Songbird 
# and Zebra respectively.

# We can check that they are still loaded and ready to go by doing a simple plot

plot(ZN)
plot(SN)


#To make this a reproducible graph so we are looking at the same thing, we can 
# set a seed and make a layout.

LayoutZN<-layout.auto(ZN)
set.seed(2222020)
plot(ZN)

# And again with our songbird network

LayoutSN<-layout.auto(SN)
set.seed(2222020)
plot(SN)

# We can write these graphs into PDFs for Future Reference

pdf(file=paste(t.path,"Zebra Network Initial.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
plot(ZN)
dev.off()

#and again for Songbirds
pdf(file=paste(t.path,"Songbird Network Initial.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
plot(SN)
dev.off()


############################## PERFORMING ANALYSIS #######################################################


# Next we want to think about what analysis we need to run to answer our question
# Our question is, which players should be immunized to slow the spread of a 
# communicable diseases in these networks? This question can be answered in many ways
# we aim to answer it assuming we can immunize:
# 1 player
# 3% of players
# and 10% of players

# The metric that seems like it is most likely to shed light on this is Betweeness
# Centrality. This metric gives us a value for each node of how often that node is
# used as a bridge between other nodes. We have a formula in our document.

# Now that we know what measure we want to use, we can begin our analysis

#First calculated Betweeness
ZNbetween<-betweenness(ZN)

#Now we make a dataframe to analyze the values of each node 
ZNbetween<-as.data.frame(ZNbetween)

# We can create a column that has the node ID next to each value
ZNbetween$Nodelist<-(1:27)

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


############################## Final Graphs #######################################################################

# Now that we know which players have the highest Betweeness Centrality, we can 
# regraph our networks with them highlighted in a different color. 


# Graphs of 1 player

#Zebra
#First we find our top 1 player as measured in Betweenness
top1ZN<-ZNbetween%>%top_n(1,ZNbetween)

#An important note is that it is our second column we want
#next we highlight those players

LayoutZN<-layout.auto(ZN)
set.seed(2222020)

#node options
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"

#Now we can plot it!
plot(ZN)

#lets save this as a pdf for future reference
pdf(file=paste(t.path,"Zebra top 1.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"
plot(ZN)
dev.off()

# Songbird
#First we find our top player
top1SN<-SNbetween%>%top_n(1,SNbetween)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top1SN[,2]]$color<-"yellow"
plot(SN)


#save as a pdf
pdf(file=paste(t.path,"Songbird top 1.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top1SN[,2]]$color<-"yellow"
plot(SN)
dev.off()
# Graphs of 3% (same as 1 player for Zebra)

#Zebra
#First we find our top 3% of players
#total nodes in Zebra are 27, 3% comes out to.81, 
#we round up to 1 player (same as last time)
top1ZN<-ZNbetween%>%top_n(1,ZNbetween)

#Again note our second column is the nodelist
#next we highlight those players
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"
plot(ZN)

#save as a pdf
pdf(file=paste(t.path,"Zebra top 3.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"
plot(ZN)
dev.off()



#Songbird
#First we find our top 3% of players
#total nodes in Songbirds are 117 3% comes 
#out to 3.5 we round up to 4 players

top4SN<-SNbetween%>%top_n(4,SNbetween)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SN[,2]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 3.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SN[,2]]$color<-"yellow"
plot(SN)
dev.off()


# Graphs of 10%

#Zebra
#First we find our top 10% of players
#total nodes in Zebra are 27, 10% comes out to 2.7, 
#we round up to 3 players
top3ZN<-ZNbetween%>%top_n(3,ZNbetween)


#Again note our second column is the nodelist
#next we highlight those players
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZN[,2]]$color<-"yellow"
plot(ZN)


#Save as a pdf
pdf(file=paste(t.path,"Zebra top 10.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZN[,2]]$color<-"yellow"
plot(ZN)
dev.off()

#Songbird
#First we find our top 10% of players
#total nodes in Songbirds are 117 10% comes 
#out to 11.7 we round up to 12 players

top12SN<-SNbetween%>%top_n(12,SNbetween)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SN[,2]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 10.pdf"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SN[,2]]$color<-"yellow"
plot(SN)
dev.off()

################################# Additional Graphs ###############################
# Additionally we can graph this with the size of the node correlated to the betweenness value
# This could be useful if you are trying to decide how much of the population you should immunize,
# but that would require more information about the disease and how it transfers and also requires
# judgement calls on acceptable losses within the network

#Zebra betweenness by size
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"yellow"
V(ZN)$size=betweenness(ZN)/2# this "/2" is so some nodes dont 
#swallow the whole screen when plotted
plot(ZN)

#save as pdf
pdf(file=paste(t.path,"Zebra betweenness by size.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"yellow"
V(ZN)$size=betweenness(ZN)/2
plot(ZN)
dev.off()



#Songbird betweenness by size
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"yellow"
V(SN)$size=betweenness(SN)/45 # this "/45" is so some nodes dont 
#swallow the whole screen when plotted
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird betweenness by size.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"yellow"
V(SN)$size=betweenness(SN)/45  
plot(SN)
dev.off()


################  attempts to do this with for loops and functions #####################
#This was simpler in R than we thought it might be, however we used a lot 
#of farmer coding. Out of curiosity I started making functions to see
#if we can do this more simply.

#We started by making a function that returns the relevant nodelist
give.nodelists<-function(test,network1,lengthnet1){
  layout.t1<-layout.auto(network1)
  temp.dataframe1<-test(network1)
  temp.dataframe1<-as.data.frame(temp.dataframe1)
  temp.dataframe1$NodeID<-(1:lengthnet1)
  temp.dataframe1%>%arrange(desc(temp.dataframe1))
  return(temp.dataframe1)
  
}
#It works!
ZND<-give.nodelists(betweenness,ZN,27)
SND<-give.nodelists(betweenness,SN,117)


#then we made a function that extracts the relevant values from that list
top.node<-function(relevant.nodelist,n){
  top1.t.1<-relevant.nodelist%>%top_n(n,relevant.nodelist[,1])
}
#it works!
highlight<-top.node(SND,1)

#and the big question
#Can I make a function that does it all??
identify.targets<-function(network,test,node.ID.column.number,
                           length,number.of.targets,seed){
  long.targets<-give.nodelists(test,network,length)
  final.targets<-top.node(long.targets,number.of.targets)
  layout.t2<-layout.auto(network)
  set.seed(seed)
  V(network)$color="grey"
  V(network)[final.targets[,node.ID.column.number]]$color<-"yellow"
  plot(network)
}

#YES I CAN!!!
identify.targets(SN,betweenness,2,117,25,34554)
