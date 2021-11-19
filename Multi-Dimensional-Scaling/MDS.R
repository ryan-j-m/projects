A <- matrix(1:36, nrow = 6, ncol = 6, dimnames =
              list(c("Tokyo", "Kofu", "Asahi", "Mt Fuji", "Yokohama", "Chiba"),
                   c("Tokyo", "Kofu", "Asahi", "Mt Fuji", "Yokohama", "Chiba")))

for(i in 1:6) {
  A[i,i]=0
}
A[1,2]=109
A[2,1]=109
A[1,3]=179
A[3,1]=179
A[1,4]=100
A[4,1]=100
A[1,5]=29
A[5,1]=29
A[1,6]=32
A[6,1]=32
A[2,3]=82
A[3,2]=82
A[2,4]=37
A[4,2]=37
A[2,5]=100
A[5,2]=100
A[2,6]=140
A[6,2]=140
A[3,4]=115
A[4,3]=115
A[3,5]=177
A[5,3]=177
A[3,6]=210
10
A[6,3]=210
A[5,4]=83
A[4,5]=83
A[4,6]=128
A[6,4]=128
A[5,6]=46
A[6,5]=46
A



# 1D
model_1<-cmdscale(A, k=1, eig = TRUE)
x1<-model_1$points
model_1

plot(x1,x1*0, xlim=c(-130,100),xlab="x",ylab = "y", main = "1-D Model Matrix A")

text(x1,x1*0,labels=rownames(model_1$points), cex=1, pos=3)

dist_1 <- as.matrix(dist(model_1$points))
Error_1_1<-(A-dist_1)/A
Error_1_1

GOF_1_1<-model_1$GOF
GOF_1_1

Eig_1_1<-model_1$eig
Eig_1_1

plot(Eig_1_1, xlab="Index of Eigenvalue", ylab="Eigenvalue",
     main="Eigenvalues of Model A")

plot(A,dist_1,xlab = "True distance", ylab = "1-D Model distance",
     main = "1-D Model Distance Between Cities (Matrix A)", asp = 1)
abline(0,1)

hist_1_ob<-hist(abs(Error_1_1), breaks=15)
hist_1_ob$counts <- hist_1_ob$counts/2

plot(hist_1_ob, xlab="% error between model distances and actual distances",
     main = "Absolute Error 1-D Model Matrix A" ,col = "gray")




# 2D
model_2<- cmdscale(A, k=2, eig = TRUE)
x2<-model_2$points[,1]
y2<-model_2$points[,2]
plot(x2,y2, xlim=c(-130,100), ylim = c(-40, 50),xlab="x",ylab = "y",
     main = "2-D Model Matrix A", asp = 1)
text(x2,y2,labels=rownames(model_2$points), cex=1, pos=3)
dist_2 = as.matrix(dist(model_2$points))
Error_1_2<-(A-dist_2)/A
Error_1_2
GOF_1_2<-model_2$GOF
GOF_1_2
plot(A,dist_2,xlab = "True distance", ylab = "2-D Model distance",
     main = "2-D Model Distance Between Cities (Matrix A)", asp = 1)
abline(0,1)
hist_2_ob<-hist(abs(Error_1_2), breaks=15)
hist_2_ob$counts<-hist_2_ob$counts/2
plot(hist_2_ob, xlab="% error between model distances and actual distances",
     main = "Absolute Error 2-D Model Matrix A" ,col = "gray")
#3D
model_3<-cmdscale(A,k=3, eig = TRUE)
model_3
dist_3<-as.matrix(dist(model_3$points))
Error_1_3<-(A-dist_3)/A
Error_1_3
GOF_1_3<-model_3$GOF
GOF_1_3
plot(A,dist_3,xlab = "True distance", ylab = "3-D Model distance",
     main = "3-D Model Distance Between Cities (Matrix A)", asp = 1)
abline(0,1)
hist_3_ob<-hist(abs(Error_1_3), breaks=15)
hist_3_ob$counts<-hist_3_ob$counts/2
plot(hist_3_ob, xlab="% error between model distances and actual distances",
     main = "Absolute Error 3-D Model Matrix A" ,col = "gray")



## Larger map
B <- matrix(1:36, nrow = 6, ncol = 6, dimnames = list(c("Tokyo",
                                                        "St Petersburg", "Seattle", "Cape Town", "Buenos Aires", "Melbourne"),
                                                      c("Tokyo", "St Petersburg", "Seattle", "Cape Town", "Buenos Aires",
                                                        "Melbourne")))
for(i in 1:6) {
  B[i,i]=0
}
B[1,2]=7586
B[2,1]=7586
B[1,3]=7695
B[3,1]=7695
B[1,4]=14718
B[4,1]=14718
B[1,5]=18362
B[5,1]=18362
B[1,6]=8185
B[6,1]=8185
B[2,3]=7799
B[3,2]=7799
B[2,4]=10461
B[4,2]=10461
B[2,5]=13207
B[5,2]=13207
B[2,6]=14897
B[6,2]=14897
B[3,4]=16469
B[4,3]=16469
B[3,5]=11177
B[5,3]=11177
B[3,6]=13200
B[6,3]=13200
B[5,4]=6865
B[4,5]=6865
B[4,6]=10308
B[6,4]=10308
B[5,6]=11603
B[6,5]=11603




# 1D
model_2_1<-cmdscale(B,k=1, eig = TRUE)
plot(model_2_1$points[,1], model_2_1$points[,1]*0,xlab="x",ylab = "y",
     main = "1-D Model Matrix B", asp = 1)
text(model_2_1$points[,1], model_2_1$points[,1]*0,
     labels=rownames(model_2_1$points), cex=.55, pos=3)
dist_2_1<-as.matrix(dist(model_2_1$points))
Error_2_1<-(B-dist_2_1)/B
Error_2_1
GOF_2_1<-model_2_1$GOF
GOF_2_1
Eig_2_1<-model_2_1$eig
Eig_2_1
plot(Eig_2_1, xlab="Index of Eigenvalue", ylab="Eigenvalue",
     main="Eigenvalues of Model B")
plot(B,dist_2_1,xlab = "True distance", ylab = "1-D Model distance",
     main = "1-D Model Distance Between Cities (Matrix B)", asp = 1)
abline(0,1)
hist_2_1_ob<-hist(abs(Error_2_1), breaks = 15)
hist_2_1_ob$counts<-hist_2_1_ob$counts/2
plot(hist_2_1_ob, xlab="% error between model distances and actual distances",
     main = "Absolute Error 1-D Model Matrix B" ,col = "gray")





#2-D
model_2_2<-cmdscale(B,k=2, eig = TRUE)
x2<-model_2_2$points[,1]
y2<-model_2_2$points[,2]
plot(x2,y2, xlim=c(min(x2)-500, max(x2)+500), ylim = c(min(y2)-500, max(y2)+500),xlab="x",ylab = "y", main = "2-D Model Matrix B", asp = 1)
text(x2,y2,labels=rownames(model_2_2$points), cex=1, pos=3)
dist_2_2 = as.matrix(dist(model_2_2$points))
Error_2_2<-(B-dist_2_2)/B
Error_2_2
max_dist_2_2<-max(dist_2_2)
GOF_2_2<-model_2_2$GOF
GOF_2_2
plot(B,dist_2_2,xlab = "True distance", ylab = "2-D Model distance",
     main = "2-D Model Distance Between Cities (Matrix B)", asp = 1)
abline(0,1)
hist_2_2_ob<-hist(abs(Error_2_2), breaks = 15)
hist_2_2_ob$counts<-hist_2_2_ob$counts/2
plot(hist_2_2_ob, xlab="% error between model distances and actual distances",
     main = "Absolute Error 2-D Model Matrix B" ,col = "gray")



#3-D
model_2_3<-cmdscale(B,k=3, eig = TRUE)
dist_2_3 <- as.matrix(dist(model_2_3$points))
Error_2_3<- (B-dist_2_3)/B
Error_2_3
GOF_2_3<-model_2_3$GOF
GOF_2_3
plot(B,dist_2_3,xlab = "True distance", ylab = "3-D Model distance",
     main = "3-D Model Distance Between Cities (Matrix B)", asp = 1)
abline(0,1)
hist_2_3_ob<-hist(abs(Error_2_3), breaks = 15)
hist_2_3_ob$counts<-hist_2_3_ob$counts/2
plot(hist_2_3_ob, xlab="% error between model distances and actual distances",
     main = "Absolute Error 3-D Model Matrix B" ,col = "gray")
hist_1<-hist(A, breaks=10)
hist_2<-hist(dist_1, breaks=10)
max(abs(dist(model_2$points)-as.dist(A)))
mean(abs(dist(model_2$points)-as.dist(A)))
max(abs(dist(model_2_2$points)-as.dist(B)))
mean(abs(dist(model_2_2$points)-as.dist(B)))
max(abs(dist(model_3$points)-as.dist(A)))
mean(abs(dist(model_3$points)-as.dist(A)))
max(abs(dist(model_2_3$points)-as.dist(B)))
mean(abs(dist(model_2_3$points)-as.dist(B)))
