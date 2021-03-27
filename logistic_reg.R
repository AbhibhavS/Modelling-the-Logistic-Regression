#--------------Logistic Regression --------------#

setwd("C:/Users/Dell/Desktop")
set.seed(111)

logistic.train<- function(train_data, method, lr, verbose){
  
  b0<-rep(1, nrow(train_data))
  x<-as.matrix(cbind(b0, train_data[,1:(ncol(train_data)-1)]))
  y<- train_data[, ncol(train_data)]
  
  beta<- as.matrix(runif(ncol(x), 0,1)) #initiate
  beta_all<-NULL
  beta_at<-c(1,10,50,100,110,150,180,200,300,500,600,800,1000,
             1500,2000,4000,5000,6000,10000)
  
  cost<-1; epoch<-0; old_cost<-1;
  
  
  #-----------------------------Gradient Descent------------#
  if(method=="Gradient"){
    while( (cost > 0.001) & (epoch<=1e5)){
      
      logit<-x%*%beta
      p <- 1/( 1+ exp(-(logit))) #probability
      
      # Likelihood: L(x|beta) = P(Y=1|x,beta)*P(Y=0|x,beta)
      likeh<-0.01
      for(i in 1:length(p)){
        likeh <- likeh*(ifelse( y[i]==1, p[i], (1-p[i])))
      }
      
      #first derivative of the likelihood finction
      first_d<-  t(x) %*% (y-p)
      k<- lr*first_d #step toward minimization
      
      beta <- beta + k #update
      
      old_cost<-cost
      cost<- sum(((p-y)^2))/2 #updated cost #updated cost
      
      if(verbose==T){
        ifelse(epoch%%200==0, 
               print(paste0(epoch, "th Epoch", 
                            "---------cost=", round(cost,4),
                            "---------likelihood=", round(likeh,4),
                            collapse = "")), NA)}
      
      if(epoch %in% beta_at){beta_all<-cbind(beta_all, beta)}
      
      epoch<- epoch+1
    }
  }
  
  #--------------Newtown second order diff method-------------#
  
  else if(method=="Newton"){
    while((cost > 0.001) & (epoch<=1e5)){
      
      logit<-x%*%beta
      p <- 1/( 1+ exp(-(logit))) #probability
      
      # Likelihood: L(x|beta) = P(Y=1|x,beta)*P(Y=0|x,beta)
      likeh<-1
      for(i in 1:length(p)){
        likeh <- likeh*(ifelse( y[i]==1, p[i], (1-p[i])))
      }
      
      #first derivative of the likelihood finction
      first_d<-  t(x) %*% (y-p)
      
      w<-matrix(0, ncol= nrow(x), nrow = nrow(x))
      diag(w)<-p
      hessian<- t(x) %*% w %*% x #hessian matrix
      k<- solve(hessian) %*% (t(x) %*% (y-p)) #newton method
      
      beta <- beta + k #update
      
      old_cost<-cost
      cost<- sum(((p-y)^2))/2 # #update
      
      
      if(verbose==T){
        ifelse(epoch%%200==0, 
               print(paste0(epoch, "th Epoch", 
                            "---------cost=", round(cost,4),
                            "---------likelihood=", round(likeh,4),
                            collapse = "")), NA)}
      
      if(epoch %in% beta_at){beta_all<-cbind(beta_all, beta)}
      
      epoch<- epoch+1
    }
  }
  
  else(break) 
  
  beta_all<-cbind(beta_all, beta)
  colnames(beta_all)<-c(beta_at[1:(ncol(beta_all)-1)], epoch-1)
  
  mylist<-list(as.matrix(beta), likeh, beta_all)
  names(mylist)<- c("Beta", "likeh", "Beta_all")
  return(mylist)
} # Fitting of logistic model

logistic.pred<-function(model, test_data){
  
  test_new<- cbind( rep(1, nrow(test_data)), test_data[,-ncol(test_data)])
  
  beta<-as.matrix(model$Beta)
  beta_all<-model$Beta_all
  ll<- model$likeh
  
  log_odd<-cbind(as.matrix(test_new)) %*% beta
  
  probability<- 1/(1+ exp(-log_odd))
  predicted_label<- ifelse(probability >= 0.5, 1, 0)
  
  k<-cbind(test_data[,ncol(test_data)], predicted_label)
  colnames(k)<- c("Actual", "Predicted")
  k<- as.data.frame(k)
  
  tp<-length(which(k$Actual==1 & k$Predicted==1))
  tn<-length(which(k$Actual==0 & k$Predicted==0))
  fp<-length(which(k$Actual==0 & k$Predicted==1))
  fn<-length(which(k$Actual==1 & k$Predicted==0))
  
  cf<-matrix(c(tp, fn, fp, tn), 2, 2, byrow = F)
  rownames(cf)<- c("1", "0")
  colnames(cf)<- c("1", "0")
  
  p_list<-list(k, cf, beta, ll, beta_all)
  names(p_list)<- c("predticted", "confusion matrix","beta", "liklihood", "Beta_all")
  return(p_list)
  
} # to make prediction from the trained model


#importing data
data<-read.csv("fire.csv", header = T)
data$Classes<-as.numeric(ifelse(1:nrow(data)%in%grep("not",data$Classes), 0, 1))
data<-rbind(data[which(data$Classes==0),],
            data[sample(size=length(which(data$Classes==0)),which(data$Classes==1)),])
data<-data[sample(1:nrow(data)),]
data<-as.data.frame(data)
data<-lapply(data, as.numeric) 
data<-as.data.frame(data)
data<-data[-unique(which(is.na(data), arr.ind = T)[,1]),]

partition<-sample(c(0,1), size=nrow(data), prob = c(0.8,0.2), replace = T)

train<-data[which(partition==0),]
test<-data[which(partition==1),]





mymodel_newton<- logistic.train(train, "Newton", 0.01, verbose=T) # Fitting the model using Newton method
mymodel_gradient<- logistic.train(train, "Gradient", 0.01, verbose=T) # Fitting the model using Gradient method

myresult1<-logistic.pred( mymodel_newton, test_data = test) #prediction using Newton trained model
myresult2<-logistic.pred( mymodel_gradient, test_data = test) #prediction using Gradient trained model

myresult1$`confusion matrix`
myresult2$`confusion matrix`


sigmoid<-function(x){
  sig<-1/(1+exp(-x))
}

par(bg="black")
for(i in 1:ncol(myresult1$Beta_all)){
  
  colo<-ifelse(train$Classes==0,"red", "blue")
  a<-sigmoid(as.matrix(cbind(rep(1,nrow(train)),train[,-ncol(train)]))%*%as.matrix(myresult1$Beta_all[,i]))
  colo<-colo[order(a, decreasing = F)]
  a<-a[order(a, decreasing = F)]
  
  plot(a, lwd=2, type = "n", col.axis="white", xaxt="n",
       ylab="Probability", xlab="Examples", col.lab="white",
       main=paste("Newton Method  Epoch No:.", 
                  colnames(myresult1$Beta_all)[i], collapse = ""),
       ylim=c(0,1),
       col.main="white", cex.lab=1.5, font=2, font.lab=9)
  abline(h=seq(0,1,0.05), col="darkgreen")
  lines(a, col="green", lwd=3)
  points(a,col=colo, pch=16, cex=2, lwd=2)
  points(a,col="black", pch=1, cex=2, lwd=1)
  box(col="white", lwd=2)
  
  legend("bottomright",
         legend = c("Forest Fire","No Forest Fire"),
         col=c("blue","red"),
         pch = 16,
         cex=1.3,
         bty="n",
         text.col=c("blue","red"),
         text.font = 9)
}

par(bg="black")
for(i in 1:ncol(myresult2$Beta_all)){
  
  colo<-ifelse(train$Classes==0,"red", "blue")
  a<-sigmoid(as.matrix(cbind(rep(1,nrow(train)),
                             train[,-ncol(train)]))%*%as.matrix(myresult2$Beta_all[,i]))
  colo<-colo[order(a, decreasing = F)]
  a<-a[order(a, decreasing = F)]
  
  plot(a, lwd=2, type = "n", col.axis="white", xaxt="n",
       ylab="Probability", xlab="Examples", col.lab="white",
       main=paste("Gradient Desc Method  Epoch No:.", 
                  colnames(myresult2$Beta_all)[i], collapse = ""),
       ylim=c(0,1),
       col.main="white", cex.lab=1.5, font=2, font.lab=9)
  abline(h=seq(0,1,0.05), col="darkgreen")
  lines(a, col="green", lwd=3)
  points(a,col=colo, pch=16, cex=2, lwd=2)
  points(a,col="black", pch=1, cex=2, lwd=1)
  box(col="white", lwd=2)
  
  legend("bottomright",
         legend = c("Forest Fire","No Forest Fire"),
         col=c("blue","red"),
         pch = 16,
         cex=1.3,
         bty="n",
         text.col=c("blue","red"),
         text.font = 9)
}





