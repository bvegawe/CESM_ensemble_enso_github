# Test the hypothesis that the means of two different distributions
#  are equal
#
# Null hypothesis: true difference in means is equal to zero
#

#fcesm = read.table("./data/cesm_temp.txt") 
#  cesm = as.vector(fcesm[,3])
#fcmip = read.table("./data/cmip5_temp.txt") 
#  cmip = as.vector(fcmip[,3])

fcesm = read.table("./data/cesm_precip.txt") 
  cesm = as.vector(fcesm[,1])
fcmip = read.table("./data/cmip5_precip.txt") 
  cmip = as.vector(fcmip[,1])

#print(cesm)
#print(cmip)


#  par(mfrow=c(2,2))  # 2x2 panel plot

#hist(cesm,xlim=c(0.10,0.5))
#abline(v=mean(cesm))

#hist(cmip,xlim=c(0.10,0.5))
#abline(v=mean(cmip))

# perform standard two-sample t-test
test=t.test(cesm,cmip,mu=0,
    conf.level=0.80,
    var.equal=FALSE,
    alternative="two.sided")
  print(test)


degf=length(cesm)+length(cmip)-2

# plot the results
x=seq(-4,4,length=1000)
plot(x,dt(x,df=degf),type="l",lwd=5)
  abline(v=test$statistic,col="red",lwd=5)
  abline(v=qt(0.05,df=degf),lwd=3,lty="dashed")
  abline(v=qt(0.95,df=degf),lwd=3,lty="dashed")

stop()


# creat two different samples
size=50
s1=rnorm(size,mean=0,sd=1)
s2=rnorm(size,mean=0,sd=1)

# perform standard two-sample t-test
test=t.test(s1,s2,mu=0,
    conf.level=0.95,
    var.equal=TRUE,  
    alternative="two.sided")
  print(test)


# plot the results
x=seq(-4,4,length=1000)
plot(x,dt(x,df=2*size-2),type="l",lwd=5)
  abline(v=test$statistic,col="red",lwd=5)
  abline(v=qt(0.025,df=2*size-2),lwd=3,lty="dashed")
  abline(v=qt(0.975,df=2*size-2),lwd=3,lty="dashed")

# plot the confidence interval
#  abline(v=abs(mean(s1))-abs(mean(s2)),lwd=3,col="blue")
#  abline(v=test$conf.int[1],lwd=3,lty="dashed",col="blue")
#  abline(v=test$conf.int[2],lwd=3,lty="dashed",col="blue")




# Compare t-stats and z-stats

#plot(x,dnorm(x),type="l",lwd=10)
#lines(x,dt(x,df=1),lwd=3,col="red")
#lines(x,dt(x,df=2),lwd=3,col="blue")
#lines(x,dt(x,df=3),lwd=3,col="green")

#Create Legend
#  legend(x="topleft",
#       legend=c("Normal Distribution", "T-distribution, N=2", 
#       "T-distribution, N=3", "T-distribution, N=4"),
#       col=c("black","red","blue","green"),
#       lty=c("solid","solid","solid","solid"),
#       lwd=c(5,5,5,5)
#        )


