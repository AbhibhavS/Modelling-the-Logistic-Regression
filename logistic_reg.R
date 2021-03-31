#--------------Logistic Regression --------------#

setwd("C:/Users/Dell/Desktop")
set.seed(111) #to generate the same results as mine

logistic.train<- function(train_data, method, lr, verbose){
  
  b0<-rep(1, nrow(train_data))
  x<-as.matrix(cbind(b0, train_data[,1:(ncol(train_data)-1)]))
  y<- train_data[, ncol(train_data)]
  
  beta<- as.matrix(rep(0.5,ncol(x))); likelihood<-0; epoch<-0 #initiate
  
  beta_all<-NULL
  beta_at<-c(1,10,50,100,110,150,180,200,300,500,600,800,1000,
             1500,2000,4000,5000,6000,10000) #checkpoints (the epochs at which the betas are recorded)
  
  #-----------------------------Gradient Descent---------------------#
  if(method=="Gradient"){
    while( (likelihood < 0.95) & (epoch<=35000)){
      
      logit<-x%*%beta #Calculate logit(p) = x?????
      
      p <- 1/( 1+ exp(-(logit))) #Calculate P=logistic(X?????)= 1/(1+exp(-X?????))
      
      # Likelihood: L(x|beta) = P(Y=1|x,beta)*P(Y=0|x,beta)
      likelihood<-1
      for(i in 1:length(p)){
        likelihood <- likelihood*(ifelse( y[i]==1, p[i], (1-p[i]))) #product of all the probability
      }
      
      first_d<-  t(x) %*% (y-p)#first derivative of the likelihood function
      
      beta <- beta + lr*first_d #updating the parameters for a step toward maximization
      
      #to see inside the steps of learning
      if(verbose==T){
        ifelse(epoch%%200==0, 
               print(paste0(epoch, "th Epoch", 
                            "---------Likelihood=", round(likelihood,4),
                            "---------log-likelihood=", round(log(likelihood),4),
                            collapse = "")), NA)}
      
      if(epoch %in% beta_at){beta_all<-cbind(beta_all, beta)}
      
      epoch<- epoch+1
    }
  }
  
  #--------------Newton second order diff method-------------#
  
  else if(method=="Newton"){
    while((likelihood < 0.95) & (epoch<=35000)){
      
      logit<-x%*%beta #Calculate logit(p) = x?????
      p <- 1/( 1+ exp(-(logit))) #Calculate P=logistic(X?????)= 1/(1+exp(-X?????))
      
      # Likelihood: L(x|beta) = P(Y=1|x,beta)*P(Y=0|x,beta)
      likelihood<-1
      for(i in 1:length(p)){
        likelihood <- likelihood*(ifelse( y[i]==1, p[i], (1-p[i])))
      }
      
      first_d<-  t(x) %*% (y-p)#first derivative of the likelihood function
      
      w<-matrix(0, ncol= nrow(x), nrow = nrow(x)) #initializing p(1-p) diagonal matrix
      diag(w)<-p*(1-p)
      hessian<- -t(x) %*% w %*% x #hessian matrix
      
      hessian<- diag(ncol(x))-hessian #Levenberg-Marquardt method: Add a scaled identity matrix to avoid singularity issues
      
      k<- solve(hessian) %*% (t(x) %*% (y-p)) #the gradient for newton method
      
      beta <- beta + k #updating the parameters for a step toward maximization
      
      if(verbose==T){
        ifelse(epoch%%200==0, 
               print(paste0(epoch, "th Epoch", 
                            "---------Likelihood=", round(likelihood,4),
                            "---------log-likelihood=", round(log(likelihood),4),
                            collapse = "")), NA)}
      
      if(epoch %in% beta_at){beta_all<-cbind(beta_all, beta)} #just to inside the learning
      epoch<- epoch+1
    }
  }
  
  else(break) 
  
  beta_all<-cbind(beta_all, beta)
  colnames(beta_all)<-c(beta_at[1:(ncol(beta_all)-1)], epoch-1)
  
  mylist<-list(as.matrix(beta), likelihood, beta_all)
  names(mylist)<- c("Beta", "likelihood", "Beta_all")
  return(mylist)
} # Fitting of logistic model


logistic.pred<-function(model, test_data){
  
  test_new<- cbind( rep(1, nrow(test_data)), test_data[,-ncol(test_data)]) #adding 1 to fit the intercept
  
  beta<-as.matrix(model$Beta) #extract the best suiting beta (the beta at final epoch)
  beta_all<-model$Beta_all #extract all the betas at different checkpoints
  ll<- model$likelihood #extract the highest likelihood obtained
  
  log_odd<-cbind(as.matrix(test_new)) %*% beta #logit(p)
  
  probability<- 1/(1+ exp(-log_odd)) # p=logistic(logit(p))
  predicted_label<- ifelse(probability >= 0.5, 1, 0) #discrimination rule
  
  k<-cbind(test_data[,ncol(test_data)], predicted_label) # actual label vs predicted label
  colnames(k)<- c("Actual", "Predicted")
  k<- as.data.frame(k)
  
  tp<-length(which(k$Actual==1 & k$Predicted==1)) #true positive
  tn<-length(which(k$Actual==0 & k$Predicted==0)) #true negative
  fp<-length(which(k$Actual==0 & k$Predicted==1)) #false positive
  fn<-length(which(k$Actual==1 & k$Predicted==0)) #false negative
  
  cf<-matrix(c(tp, fn, fp, tn), 2, 2, byrow = F) #confusion matrix
  rownames(cf)<- c("1", "0")
  colnames(cf)<- c("1", "0")
  
  p_list<-list(k, cf, beta, ll, beta_all)
  names(p_list)<- c("predticted", "confusion matrix","beta", "liklihood", "Beta_all")
  return(p_list)
  
} # to make prediction from the trained model


#----------------------------DATASET-------------------------------#
#importing data
data<-read.csv("fire.csv", header = T) #import
data$Classes<-as.numeric(ifelse(1:nrow(data)%in%grep("not",data$Classes), 0, 1)) # one hot encoding ; numeric conversion from label to 1 or 0
data<-rbind(data[which(data$Classes==0),],
            data[sample(size=length(which(data$Classes==0)),which(data$Classes==1)),]) #balancing the classes
data<-data[sample(1:nrow(data)),] #shuffling
data<-as.data.frame(data) #data to data frame
data<-lapply(data, as.numeric) 
data<-as.data.frame(data)


#missing data handling
if(!is.null(is.na(data))){
  data<-data[-unique(which(is.na(data), arr.ind = T)[,1]),]
}

#test train partition
partition<-sample(c(0,1), size=nrow(data), prob = c(0.8,0.2), replace = T) 
train<-data[which(partition==0),]
test<-data[which(partition==1),]


#-------------------------TRAINING---------------------------------#

mymodel_newton<- logistic.train(train, "Newton", 0.01, verbose=T) # Fitting the model using Newton method
mymodel_gradient<- logistic.train(train, "Gradient", 0.01, verbose=T) # Fitting the model using Gradient method

#------------------------TESTING-------------------------------------#
myresult1<-logistic.pred( mymodel_newton, test_data = test) #prediction using Newton trained model
myresult2<-logistic.pred( mymodel_gradient, test_data = test) #prediction using Gradient trained model


#------------------------Results----------------------------------#
Newton_confusion_matrix<-myresult1$`confusion matrix`
Gradient_confusion_matrix<-myresult2$`confusion matrix`

accuracy_obtained_by_newton_method<-round(c(myresult1$`confusion matrix`[1,1]
        +myresult1$`confusion matrix`[2,2])/sum(myresult1$`confusion matrix`),3)
accuracy_obtained_by_gradient_method<-round(c(myresult2$`confusion matrix`[1,1]
              +myresult2$`confusion matrix`[2,2])/sum(myresult2$`confusion matrix`),3)


Newton_confusion_matrix
accuracy_obtained_by_gradient_method
                                   
#-----------------------------------------------------------------#


Gradient_confusion_matrix
accuracy_obtained_by_newton_method
