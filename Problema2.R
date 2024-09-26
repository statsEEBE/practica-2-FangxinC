#Codigo para problema 2
mis_dades <- iris
y <- mis_dades$Sepal.Length
x <- mis_dades$Petal.Length
plot(x,y)
xbar <- mean(x)
ybar <- mean (y)
sum((x-xbar)*(y-ybar))
sum((x-xbar)^2)
m<-sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
b=ybar-m*xbar
m*1.5+b
mod<-lm(y~x)
Y<- predict(mod,data.frame(x=x))
plot(x,y,pch=16,col='blue')
lines(x,Y)
# R^2 coeficiente de determinacion R^2 = sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
# R coeficiente de correlacion R=sqrt(R^2) // R=cor.tes(x,y)
sum((Y-ybar)^2)/sum((y-ybar)^2)
summary(mod)
