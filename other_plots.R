

data(iris)
data<-iris
data$Species<-as.numeric(data$Species)
data<-data[which(data$Species%in%c(1,2)),]
data$Species<-ifelse(data$Species==1,0,1)
colo<-ifelse(data$Species==1,"red","blue")


myline<-function(x){
  y<--2.5 + x*0.6
  return(y)
}

par(bg="white")
data("iris")
iris<-iris[sample(1:nrow(iris)),]
iris$Species<-as.numeric(iris$Species)
iris$Species<-ifelse(iris$Species==1,0,ifelse(iris$Species==2,1,3))
iris<-iris[-which(iris$Species==3),]
a<-cbind(iris$Sepal.Length, iris$Species)
colnames(a)<-c("Variable", "Class")
a[1:20,]



par(mfrow=c(1,2))

plot(x=a[,1], y=a[,2], xlab="Variable", cex.lab=1.5, pch=16,
     ylab="Class", yaxt="n",font=9,font.lab=9, cex=1.5)
axis(2, at=c(0,1), label=c(0,1), cex.axis=2, font=2, las=1)
box(lwd=2)


plot(x=a[,1], y=a[,2], xlab="Variable", cex.lab=1.5, pch=16,
     ylab="Class", yaxt="n",font=9,font.lab=9, cex=1.5)
axis(2, at=c(0,1), label=c(0,1), cex.axis=2, font=2, las=1)
box(lwd=2)
abline(lm(a[,2]~a[,1]), lwd=4, col="red")

par(new=T)
plot((1/(1+exp(-(seq(-10,10,length.out = 100))))),
     type="l", lwd=4, col="blue", xaxt="n", yaxt="n",
     xlab="", ylab="")




par(mfrow=c(1,1))
logit<- function(p){
  logi<- log(p/(1-p))
}
par(bg="lightblue")
a<-logit(seq(0.001,0.999, length.out = 200))
plot(x=seq(0,1, length.out = length(a)),a,
     type="n", lwd=4, col="orange",
     xlab="probability", cex.lab=1.5, pch=16,
     ylab="log(p/1-p) :: Logit", yaxt="n",font=9,font.lab=2, cex=1.5)
axis(2, at=c(-7,seq(-5,5,1),7),
     label=c("-inf",seq(-5,5,1),"+inf"), cex.axis=1.4, font=2, las=1)
abline(h=seq(-7,7,1), lwd=1,lty=3, col="grey20")
abline(v=seq(0.0,1.0,0.05), lwd=1,lty=3, col="grey20")
lines(x=seq(0,1, length.out = length(a)),a,lwd=4, col="orange")
box(lwd=2)




logit<- function(p){
  logi<- log(p/(1-p))
}
par(bg="white")
data("iris")
iris<-iris[sample(1:nrow(iris)),]
iris$Species<-as.numeric(iris$Species)
iris$Species<-ifelse(iris$Species==1,0,ifelse(iris$Species==2,1,3))
iris<-iris[-which(iris$Species==3),]
a<-cbind(iris$Sepal.Length, iris$Species)
colnames(a)<-c("Variable", "Class")
a[1:20,]

par(mfrow=c(1,3))
a[,2]<-ifelse(a[,2]==0, 0.001, 0.999)
a[,2]<-logit(a[,2])
plot(x=a[,1],y=a[,2], main="Fig C (I)",
     type="n", lwd=4, col="orange",
     xlab="variable (v)", cex.lab=1.5, pch=16,
     ylab="log(p/1-p) :: Logit", yaxt="n",font=9,font.lab=2, cex=1.5)
axis(2, at=c(-7,seq(-5,5,1),7),
     label=c("-inf",seq(-5,5,1),"+inf"), cex.axis=1.4, font=2, las=1)
points(x=a[,1],y=a[,2], col="black", pch=16, cex=2)
segments(4,-5,8,10, col="red",lwd=5)
text(6.25,1,label="log(p/1-p) = ?????+ ?????*v", font=2,cex=1.2,col="blue")
text(6.25,-1,label=" ?????=-20, ??1=3.75", font=2,cex=1.2,col="blue")
box(lwd=2)


plot(x=a[,1],y=a[,2],
     type="n", lwd=4, col="orange",main="Fig C (II)",
     xlab="variable (v)", cex.lab=1.5, pch=16,
     ylab="log(p/1-p) :: Logit", yaxt="n",font=9,font.lab=2, cex=1.5)
axis(2, at=c(-7,seq(-5,5,1),7),
     label=c("-inf",seq(-5,5,1),"+inf"), cex.axis=1.4, font=2, las=1)
points(x=a[,1],y=a[,2], col="black", pch=16, cex=2)
segments(4,-5,8,10, col="red",lwd=5)
points_on_red<--20+15/4*a[,1]
a_down<-a[,1][c(which(a[,2]<0))]
a_down_pol<-points_on_red[c(which(a[,2]<0))]
segments(a_down,-7,a_down, a_down_pol, lwd=2, col="blue")

a_up<-a[,1][c(which(a[,2]>0))]
a_up_pol<-points_on_red[c(which(a[,2]>0))]
segments(a_up,7,a_up,a_up_pol, lwd=2, col="blue")
points(a[,1],points_on_red, pch=19,
       lwd=3, cex=1.5, col="blue" )
text(6.5,-1, labels = "<-- Projection on the")
text(6.5,-2, labels = "   logit line")

box(lwd=2)



plot(x=a[,1], y=1/(1+exp(-a[,2])), xlab="Variable (v)", cex.lab=1.5, pch=16,
     ylab="Class Probability",font=9,font.lab=9, cex=1.5)

text(6.25,0.6, labels = "Logistic transformation")
text(6.25,0.5, labels = "Class Prob.= 1/(1+exp(-logit))")
par(new=T)
plot(x=a[,1],y=1/(1+exp(-points_on_red)), 
      type="p", lwd=5, col="blue", xaxt="n",
     yaxt="n", xlab="", ylab="", main= "Fig C (III)")
box(lwd=2)




par(bg="lightblue")
par(mfrow=c(1,1))

plot(1/(1+exp(-(seq(-10,10, length.out = 1000)))),
     type = "l", lwd=5, col= "red", xlab="real value",
     ylab=expression(paste(sigma,"(z)=probability in [0,1]")), xaxt="n", font.lab=9,
     main=" Logistic mapping (-inf, inf) to (0,1)", font.axis=2,
     cex.axis=1.2, cex.lab=1.2)
axis(1, at=seq(1, 1000, length.out = 21),
     labels = c("-inf", seq(-9,9,1), "inf"),
     font=2, cex=1.4)
abline(v=seq(1, 1000, length.out = 21), lwd=1.4, lty=3)
abline(h=seq(0,1,0.1), lwd=1.4, lty=3)
box(lwd=3)







