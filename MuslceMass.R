MuscleMass
x <- MuscleMass$Age
y <- MuscleMass$MuscleMass
quadmodel <- lm(y ~ x + I(x^2),data=MuscleMass)
summary(quadmodel)
x_seq <- seq(0,100,0.1)
predicted<-predict(quadmodel,list(x=x_seq,x=x_seq^2)) 
plot(x,y,pch=16,xlab="Age",ylab="MuscleMass",main="Quadratic Model Fit",col="blue")
lines(x_seq,predicted,col="pink",lwd=3)
MuscleMass$Age
#Print Quadratic Residuals and test nonconstant variance with mean zero
#Age variable are identically and independently distributed
#The errors are iid
quadmodel$residuals

plot(x,quadmodel$residuals,col="red",pch=20)
plot(quadmodel$fitted.values,quadmodel$residuals,col="red",pch=20)

#Is the model a good fit?
plot(quadmodel)

