x<-c(1,2,3,4,5,6,7,8,9,10)

x_new=11

data<-read.csv(file.choose(), header = T, sep = ",")
attach(data)
names(data)
plot(x,Close)

M=3
N=10
alpha=5*10^-3
beta=11.1
a=1


i = 0:M
phix=x_new^i

phix=rbind(phix)

phixn[]=numeric(M)
phixn=matrix(x,nrow=M,ncol=N)
for (n in 1:N)
{
  for(i in 1:M)
    phixn[i,n]=x[n]^i
}
b=matrix(1,nrow=1,ncol=N)
phixn=rbind(b,phixn)

mult=matrix(0,nrow=4,ncol=4)
for (n in 1:N)
  mult=mult+(phixn[ ,n]%*%phix)

## Implementing formula 1.72 to obatin S and Sinverse
sinv=alpha*diag(1,4,4)+beta*mult   
s=solve(sinv)

summation=matrix(0,nrow=1,ncol=1)
for (n in 1:N)
  summation=summation+(phixn[ ,n]*Close[n])

## Implementing formula 1.70 to obtain mean of x
mean1=s%*%summation
mean=beta*(phix%*%mean1)

## Implementing formula 1.71 to obtain variance of x
var=s%*%t(phix)
variance=(1/beta)+(phix%*%var)

###t_new=(1/sqrt(2*pi*var))*exp((-1/(2*var))*(x_new-mean)^2)

x1=append(x,x_new)
y=rnorm(x1,mean=mean,sd=sqrt(variance))

predicted=y[11]
actual=Close[11]

##to calculate absolute mean error
abs_mean_error=abs(predicted-actual)
##to calculate average relative error
avg_rel_error=(abs_mean_error)/actual
