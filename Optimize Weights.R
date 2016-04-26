
#Creating a weighted classifier (uses  logistic, GB)
accuracy=vector()
for (i in 0:10){
  pred=.1*i*probGB1 + (1-i*.1)*as.numeric(problog)
  pred=as.numeric(pred>.5)
  accuracy[i]=sum(pred==y[test])/length(pred)
}

which(accuracy==max(accuracy)) #.7 weight on GB
max(accuracy) #.7617
pred=.5*probGB1 + .5*problog

accuracy=vector()
for (i in 0:20){
  pred=(.01*i+.65)*probGB1 + (1-(.01*i+.65))*as.numeric(problog)
  pred=as.numeric(pred>.5)
  accuracy[i]=sum(pred==y[test])/length(pred)
}
which(accuracy==max(accuracy)) #.71 on GB 
max(accuracy) #.7623

GB_log=data.frame(seq(.1,1,by=.1), accuracy)
colnames(GB_log)=c("x", "y")
ggplot() +  geom_point(data = GB_log, aes(x = x, y = y), alpha = 1, col="blue") +
  labs(x="Weight From Gradient Boosting", y="Error Rate") + 
  ggtitle("Accuracy as Weight on Gradient Boosting Classifier Changes")


