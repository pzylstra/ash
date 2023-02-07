# Fits a negative exponential curve to the Mokany data

Mokany <- read.csv("Mokany.csv")
control=nls.control(maxiter=maxiter, tol=1e-7, minFactor = 1/999999999)
init <- c(a=0.6)
x <- as.numeric(Mokany$X)
y <- as.numeric(Mokany$Y)

Mok<-nls(y~a*x,data=Mokany,start=init,trace=T, control = control)
BSum <- base::summary(Mok)
Ba <- BSum$coefficients[1]
BRSE <- BSum$sigma

#Negative exponential
init1<-c(r=0.5)
NE<-nls(y~60 * (1-exp(-r*x)),data=Mokany,start=init1,trace=T)
NESum <- base::summary(NE)
r <- NESum$coefficients[1]
NERSE <- NESum$sigma
NERsq <- cor(predict(NE, newdata=x), y)**2
NEp <- round(NESum$coefficients[4],5)

windows(7,7)
plot(x,y)