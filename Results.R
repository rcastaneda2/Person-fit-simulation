
results<-function(data2){
  
  data<- data2[ which(data2[,6]==1 & data2[,7] == 1 &  data2[,8] == 1  &  data2[,9] == 1  ), ]
  print(length(data[,1]))
  res<-matrix(0,ncol=5,nrow=1)
  res[1,1]<-mean(data[,1])
  res[1,2]<-mean(data[,2])
  res[1,3]<-mean(data[,3])
  res[1,4]<-mean(data[,4])
  res[1,5]<-mean(data[,5])
  
  return(res)

}

cell3<-rbind(cell3.1,cell3.2,cell3.3,cell3.4,cell3.5,cell3.6,cell3.7,cell3.8,cell3.9,cell3.10)
cell6<-rbind(cell6.1,cell6.2,cell6.3,cell6.4,cell6.5,cell6.6,cell6.7,cell6.8)
cell9<-rbind(cell9.1,cell9.2,cell9.3,cell9.4,cell9.5,cell9.6,cell9.7,cell9.8,cell9.9,cell9.10)
cell12<-rbind(cell12.1,cell12.2,cell12.3,cell12.4,cell12.5,cell12.6,cell12.7,cell12.8)

results(cell3)
results(cell6)
results(cell9)
results(cell12)
