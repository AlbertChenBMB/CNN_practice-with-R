#Gradient Descent note
##matrix need %*%

lmod<-lm(dist~speed,data = cars)
lmod
plot(dist~speed,data = cars)
abline(lmod,col="red")
##### Gardient Descent 
GardD<-function(x,y,alpha=0.05,epsilon=10^-10){
        iter<-0
        i<-0
        x<-cbind(rep(1,nrow(x)),x)# eatch data add bias
        theta<-matrix(c(1,1),ncol(x),1)# theta
        cost<-(1/(2*nrow(x)))*t(x %*% theta -y) %*% (x %*% theta -y)
        delta<-1
        while(delta>epsilon){
                i<-i+1
                theta<-theta -(alpha/nrow(x))*(t(x)%*%(x %*% theta-y))
                cval<-(1/(2*nrow(x))) * t(x %*% theta - y) %*% (x %*% theta - y)
                cost <- append(cost, cval)
                delta <- abs(cost[i+1] - cost[i])
                if((cost[i+1]-cost[i])>0){
                        print("The cost is increasing, Try reducing alpha.")
                        return()
                }
                iter<-append(iter,1)
        }
        print(sprintf("completed in %i iterations.",i))
        return(theta)
}

### predictions
TPredict<-function(theta,x){
        x<-cbind(rep(1,nrow(x)),x)
        return(x %*% theta)
}
#######################
##test
x<-as.matrix(cars$speed)
y<-as.matrix(cars$dist)
theta<-GardD(x,y,alpha = 0.006)
theta
#### add scale make it faster
stheta<-GardD(scale(x),y,alpha = 0.006)
stheta
#############
#converted back
convert_theta<-stheta[c(2:nrow(stheta)),]/t(t(apply(x,2,sd)))
cthetazero<-stheta[1,1]-sum(stheta[c(2:nrow(stheta)),])/t(t(apply(x,2,sd)))*t(t(apply(x,2,mean)))
convert_theta<- matrix(c(cthetazero,convert_theta))
##################
# normal equation
#used generalized inverse of a matrix (MASS::ginv)
normal_equ<-function(x,y){
        require(MASS)
        x<-cbind(rep(1,nrow(x)),x)
        ntheta<-ginv(t(x)%*%x) %*% t(x) %*% y
        return(ntheta)
}
#### example
x<-as.matrix(cars$speed)
y<-as.matrix(cars$dist)
ntheta<-normal_equ(x,y)
ntheta
