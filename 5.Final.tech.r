#----------------------FINAL TECHNIQUES-------------------------------
#In this document, we will create a function that can be used to identify a specificed
#number of targets that should be immunized if a communicable disease were to enter the 
#specified network.
#We also will point you to a source with many networks that you can test

immunize.targets<-function(network,number.of.targets){
  VN<-vcount(network)
  long.targets<- give.nodelists(betweenness,network,VN) 
  final.targets<- top.node(long.targets,number.of.targets)
  layout.t2<-layout.auto(network)                               
  set.seed(1234)                                               
  V(network)$color="grey"                                      
  V(network)[final.targets[,2]]$color<-"yellow"
  V(network)$size=15
  plot(network)
}

immunize.targets(AN,12)

plot(AN)



give.nodelists<-function(test,network,lengthnet1){ 
  temp.dataframe1<-test(network)                   
  temp.dataframe1<-as.data.frame(temp.dataframe1)   
  temp.dataframe1$NodeID<-(1:lengthnet1)            
  temp.dataframe1%>%arrange(desc(temp.dataframe1))  
  return(temp.dataframe1)
}
top.node<-function(relevant.nodelist,n){
  top1.t.1<-relevant.nodelist%>%top_n(n,relevant.nodelist[,1])
}
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
