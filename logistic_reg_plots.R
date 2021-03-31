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





par(bg="black")

colo<-ifelse(test$Classes==0,"red", "blue")
a<-sigmoid(as.matrix(cbind(rep(1,nrow(test)),
                           test[,-ncol(test)]))%*%as.matrix(myresult2$beta))
colo<-colo[order(a, decreasing = F)]
a<-a[order(a, decreasing = F)]

plot(a, lwd=2, type = "n", col.axis="white", xaxt="n",
     ylab="Probability", xlab="Examples", col.lab="white",
     main="Gradient Desc. Prediction",
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



par(bg="black")

colo<-ifelse(test$Classes==0,"red", "blue")
a<-sigmoid(as.matrix(cbind(rep(1,nrow(test)),
                           test[,-ncol(test)]))%*%as.matrix(myresult1$beta))
colo<-colo[order(a, decreasing = F)]
a<-a[order(a, decreasing = F)]

plot(a, lwd=2, type = "n", col.axis="white", xaxt="n",
     ylab="Probability", xlab="Examples", col.lab="white",
     main="Newton Method Prediction",
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









plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime, decreasing = T),]
sorted.png.names <- row.names(plots.png.detials)

numbered.png.names <- paste0("C:/Protein/logistic/", length(sorted.png.names):1, ".png")
file.rename(from=sorted.png.names, to=numbered.png.names)
# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.copy(from=plots.png.paths, to="C:/Protein/logistic/")
# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
#file.rename(from=sorted.png.names, to=numbered.png.names)

setwd("C:/Protein/logistic/")


