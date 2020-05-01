getwd()
MuscleMass<-read.csv("MuscleMass.csv")

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

#Are the variables normally distributed
par(mfrow=c(1,1))
qqnorm(MuscleMass$Age,col="red",pch=20,main="Normal Q-Q Plot Age")
qqline(MuscleMass$Age,col="steelblue",lwd=2)
qqnorm(MuscleMass$MuscleMass,col="red",pch=20,main="Normal Q-Q Plot Muscle Mass")
qqline(MuscleMass$MuscleMass,col="steelblue",lwd=2)
#histogram
hist(x,col="blue")
hist(y,col="red")
plot(y,type="o",lwd=3,col="red",xlab="Age of Woman",main="Over Plot Muscle Mass vs. Age")

install.packages("ggpubr")
library(ggpubr)
MuscleMass_Group
MuscleMass_Group<-MuscleMass_Group[-c(2)]
MuscleMass_Group<-as.data.frame(MuscleMass_Group)
MuscleMass_Group
str(MuscleMass_Group)
MuscleMass_Group$Group <- ordered(MuscleMass_Group$Group,levels=c("Age40","Age50","Age60","Age70"))
levels(MuscleMass_Group$Group)

ggboxplot(MuscleMass_Group, x = "Group", y = "MuscleMass", 
          color = "Group", palette = c("red", "blue","black","green"),
          order = c("Age40", "Age50", "Age60","Age70"),
          ylab = "MuscleMass", xlab = "Age")
