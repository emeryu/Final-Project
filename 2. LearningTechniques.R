################################################################################

#                             LEARNING TECHNIQUES                              #       

################################################################################

#-------- FORMATTING DATA FOR GRAPH CREATION ---------

#We have our data depicting our colleaugue networks loaded on our mainscript. 
#We now want to create a data frame with this data to create our graphs. 
CN_graph= graph.data.frame(CN_edgelist, directed =TRUE)

#Now do the same and create a data frame from the discussion networks data. 
DN_graph= graph.data.frame(DN_edgelist, directed =TRUE)

#---------- CREATING NETWORK GRAPHS -----------

#In this we learn how to set the seed, and change the properties of the network 
#graphs

#Colleaugue Networks 1st try: Plotting with colour 

#Layout options
#Set seed 
set.seed(3952)
layout1<- layout.fruchterman.reingold(CN_graph) #using a spring network reflects
#distances within network 

#Node options: colour 
V(CN_graph)$color<- "grey"
V(CN_graph)[degree(CN_graph, mode = "in")>8]$color <- "yellow"

#Edge options: colour
E(CN_graph)$color <- "grey"

#Plot the network graph  
plot(CN_graph)

#Colleaugue networks 2nd try: We can alter the size of the nodes to best fit the 
#graph 

#Layout options
#Set seed 
set.seed(3952)
layout1<- layout.fruchterman.reingold(CN_graph) #using a spring network reflects
#distances within network 

#Node colour
V(CN_graph)$color<- "grey"
V(CN_graph)[degree(CN_graph, mode = "in")>8]$color <- "yellow"

#Edge colour 
E(CN_graph)$color <- "grey"

#Node options: size 
V(CN_graph)$size= degree(CN_graph, mode= "in")/5

#Now plot the graph to see with the nodes being a different size
plot(CN_graph)

#Colleaugue networks 3rd try: We still have larger arrows that are distracting 
# So we can fix the size of the arrows

#Set seed 
set.seed(3952)
layout1<- layout.fruchterman.reingold(CN_graph) #using a spring network reflects
#distances within network 

#Node options: colour 
V(CN_graph)$color<- "grey"
V(CN_graph)[degree(CN_graph, mode = "in")>8]$color <- "yellow"

#Edge options: colour
E(CN_graph)$color <- "grey"

#Node options: size 
V(CN_graph)$size= degree(CN_graph, mode= "in")/5

#Fixing arrow size 
plot(CN_graph, edge.arrow.size=0.25, edge.arrow.mode= "-")

#Colleaugue networks try 4: In this dataset there are loops which are where a
# node is able to "nominate" themselves. In this case we don't need these loops. 

#Getting rid of loops
CN_graph2 <- simplify(CN_graph, remove.multiple = TRUE, remove.loops = TRUE)

#layout options
set.seed(3952)
layout1 <- layout.fruchterman.reingold(CN_graph2)

#Node size 
V(CN_graph2)$size= degree(CN_graph2, mode= "in")/5

#Node colour
V(CN_graph2)$color<- "grey"
V(CN_graph2)[degree(CN_graph2, mode = "in")>8]$color <- "yellow"

#edge colour 
E(CN_graph2)$color <- "grey"

#Plotting graph without loops 
plot(CN_graph2, edge.arrow.size=0.25, edge.arrow.mode= "-")

#Colleague Network 5th Try: Working with the Colleaugues Network Attributes
#this allows us to look at graduate students and researchers. 

#Layout options 
set.seed(3256)
layout1 <- layout.fruchterman.reingold(CN_graph2, niter=500)

#Node colour...
V(CN_graph2)$color <- "grey"
V(CN_graph2)[degree(CN_graph, mode="in")>8]$color <- "yellow"  
V(CN_graph2)$size=degree(CN_graph, mode = "in")/5 

V(CN_graph2)$color <- ifelse(CNA[V(CN_graph2), 2] == "Researcher", "blue", "red")

#edge colour
E(CN_graph2)$color <- "grey"

plot(CN_graph2, edge.arrow.size= 0.25, edge.arrow.mode= "-", vertex.label= NA)

#DISCUSSION NETWORK

#Discussion network 1st try:

#Create a dataframe with the data. 
DN_graph2 <- simplify(DN_graph, remove.multiple = TRUE, remove.loops = TRUE)

#Now we are going to use the discussion networks attributes 

#Layout options
set.seed(3256)
layout1 <- layout.fruchterman.reingold(DN_graph2, niter=500)

#nodes size and colour
V(DN_graph2)$size=degree(DN_graph, mode = "in")/5 
V(DN_graph2)$color <- ifelse(DNA[V(DN_graph2), 2] == "Researcher", "blue", "red")

#Edge colour 
E(DN_graph2)$color <- "grey"

#Plot this first graph 
plot(DN_graph2,edge.arrow.size= 0.25, edge.arrow.mode= "-", vertex.label= NA)

#Discussion network 2nd try: Try with a different layout 

#Layout options
set.seed(3256)
layout1 <- layout_with_kk(DN_graph2)

#nodes size and colour
V(DN_graph2)$size=degree(DN_graph, mode = "in")/5 #emphasizes distances 
V(DN_graph2)$color <- ifelse(DNA[V(DN_graph2), 2] == "Researcher", "blue", "red")

#Edge colour 
E(DN_graph2)$color <- "grey"

#Now plot with this layout to see the difference. 
plot(DN_graph2,edge.arrow.size= 0.25, edge.arrow.mode= "-", vertex.label= NA)

#---------CONNECTIVIY AND POSITION--------

#Density - how connected you are vs how connected you could be 
graph.density(DN_graph2, loop=FALSE)
# we get a value of 0.04 which means that only about 4% of possible connections 
# exist. This is a fairly low density as density ranges from 0-1, with 1 being 
# all possible connections exist. 
graph.density(CN_graph2, loop= FALSE)
# we get a value of 0.06 meaning that only about 6% of possible connections exist. 
# this is slightly more connected than the discussion network graph. 
#from this we can hypothesize that peopke are more likely to work with people that
# you dont talk to then talk to people that you dont work with. 

#Average path length - how quickly can something (ex. communicable disease) move 
#through the network
#larger path distance potentially can mean less dense network 
mean_distance(DN_graph2)
# We have an average path distance of 2.7 which means that on average something can 
# get from random node A to random node B in 2.7 jumps. This means that this network 
# is quite connected. 

mean_distance(CN_graph2)
# For this one, there is an average path distance of 2.54 which means that this 
# network is also quite connected which was graphically represented earlier. 

#Degree distribution - Plotting the probability that any certain node has that number 
# of connections (Degree) - how connected you are to 1 person vs more people 
degree_distribution(DN_graph2)
DDD <- degree.distribution(DN_graph2)

DDD2 <- as.data.frame(DDD)

#plot the probability as a histogram 
qplot (DDD, data=DDD2, geom = "histogram", binwidth= 0.001, 
       ylab= "Number of Connections", 
       xlab= "Probability of Number of Connections")

#Clustering coefficients can be broken down into global custering coefficient (which 
# is known as transitivity). Transitivity is calculated by dividing the number of 
# exsisting number of closed triads by all possible triads within the structure. 
# This tells us how "cliquey" the network is. 
transitivity (DN_graph2)
# we have a transitivity measure of 0.29 meaning that of all possible triads within
# the network, only about 29% of these are closed. 
transitivity (CN_graph2)
# we have a transitivity measure of 0.33 meaning that of all possible triads within
# the network, only about 33% of these are closed. This shows us that this network 
# is slightly more "cliquey". 

#Create an object to use later
DNT<- transitivity (DN_graph2)
CNT<- transitivity (CN_graph2)

#postitional features 

#Degree: in, out, all centrality 
#Degree centrality is the number of edges (in, out or all) of any specific node
#In R this gives us a vector of nodes with each degree centrality attached. 
DOD<- degree(DN_graph2, mode= "out")
DOD <- as.data.frame(DOD)

DID<- degree(DN_graph2, mode= "in")
DID <- as.data.frame(DID)

#visuale to understand positionality with "in"
set.seed(3256)
layout1 <- layout.fruchterman.reingold(DN_graph2, niter= 500)

#nodes size and colour
V(DN_graph2)$size=degree(DN_graph, mode = "in")/5 #emphasizes distances 
V(DN_graph2)$color <- ifelse(DNA[V(DN_graph2), 2] == "Researcher", "blue", "red")

#Edge colour 
E(DN_graph2)$color <- "grey"

plot(DN_graph2,edge.arrow.size= 0.25, edge.arrow.mode= "-", vertex.label= NA)

#visualize with "out"
set.seed(3256)
layout1 <- layout.fruchterman.reingold(DN_graph2, niter= 500)

#nodes size and colour
V(DN_graph2)$size=degree(DN_graph, mode = "out")/5 #emphasizes distances 
V(DN_graph2)$color <- ifelse(DNA[V(DN_graph2), 2] == "Researcher", "blue", "red")

#Edge colour 
E(DN_graph2)$color <- "grey"

plot(DN_graph2,edge.arrow.size= 0.25, edge.arrow.mode= "-", vertex.label= NA)

#visualize with "all"

set.seed(3256)
layout1 <- layout.fruchterman.reingold(DN_graph2, niter= 500)

#nodes size and colour
V(DN_graph2)$size=degree(DN_graph, mode = "all")/5 #emphasizes distances 
V(DN_graph2)$color <- ifelse(DNA[V(DN_graph2), 2] == "Researcher", "blue", "red")

#Edge colour 
E(DN_graph2)$color <- "grey"

plot(DN_graph2,edge.arrow.size= 0.25, edge.arrow.mode= "-", vertex.label= NA)


############################### END LEARNING TECHNIQUES ######################
