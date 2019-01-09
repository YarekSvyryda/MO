source('numGradient.R')
source('gradientDescent.R')
source('numGradient.R')
source('steepestDescent.R')
source('Newton.R')

#Regression
data=read.csv('50_Startups.csv')
data=data[,c('R.D.Spend','Profit')]
data=(data-colMeans(data))/apply(data,2,sd)

predict=function(weights,data){
  result=weights[1]+data*weights[2]
  return(result)
}

mse=function(weights){
  result=sum((data$Profit-predict(weights,data$R.D.Spend))^2)
  return(result)
}





gd=gradientDescent(f =mse,x = c(0,0),a = 10^-2,e =  10^-10,maxIter = 1000  )
sd=steepestDescent(f =mse,x = c(0,0),a = 10^-2,e =  10^-10,maxIter = 1000  )
newton=newton_nD(f =mse,x = c(0,0),tol=  10^-10,h=10^-6 )

plot(gd$f_hist,type='l',col='blue')
lines(sd$f_hist,col='red')
lines(newton$f_hist,col='green')

RD_spend=seq(min(data$R.D.Spend),max(data$R.D.Spend),0.1)
plot(data$R.D.Spend,data$Profit)
lines(RD_spend,predict(gd$x_opt,data=RD_spend),col='blue')
lines(RD_spend,predict(sd$x_opt,data=RD_spend),col='red')
lines(RD_spend,predict(newton$x_opt,data=RD_spend),col='green')

n_grid=100
x_seq <- seq(0, 1, length = n_grid)
matrVal <- matrix(0, nrow = n_grid, ncol = n_grid)
for(iRow in 1 : n_grid){
  for(iCol in 1 : n_grid){
    matrVal[iRow, iCol] <- mse(c(x_seq[iRow], x_seq[iCol]))    
  }
}
contour(x_seq, x_seq, matrVal, nlevels = 200)
lines(gd$x_hist, col = 'blue', type = 'l')

lines(sd$x_hist, col = 'red', type = 'l')

lines(newton$x_hist, col = 'green', type = 'l')



#Zadanie 2
# Załóżmy, że jest 8 klientów oraz 3 dostawcy. Losowo wybieramy ich położenie w przestrzeni 2 wymiarowej. Musimy wybrać taki punkt dla naszego sklepu, by maksymalizować wartość oczekiwaną klientów, która jest odwrotnie proporcjonalna do odległości. 

#Funkcja do losowania
Random_numbers=function(count,min_v,max_v){
  mat=c()
  for(i in 1:count){
    mat=rbind(mat,runif(2,min_v,max_v))
  }
  return(mat)
}

set.seed(123)

Clients=Random_numbers(8,-1,1)
Shops=Random_numbers(3,-1,1)


plot(Clients)
points(Shops,col='red')


distances=function(Clients,Shops){
  distance=c()
  for(i in 1:nrow(Shops)){
    distance=rbind(distance,sqrt(rowSums((Clients-Shops[i,])^2)))
  }
  rownames(distance)=seq(1,nrow(Shops))
  colnames(distance)=seq(1,nrow(Clients))

  distance=exp(t(1/distance))/rowSums(exp(t(1/distance)))
  return(colSums(distance)) 
}


distances(Clients ,Shops)



maximise=function(our_position){
  new_shops=rbind(Shops,our_position)
  result=distances(Clients,new_shops)
  result=tail(result,1)
  return(-result)
}



n_grid=100
x_seq <- seq(-1, 1, length = n_grid)
matrVal <- matrix(0, nrow = n_grid, ncol = n_grid)
for(iRow in 1 : n_grid){
  for(iCol in 1 : n_grid){
    matrVal[iRow, iCol] <- maximise(c(x_seq[iRow], x_seq[iCol]))    
  }
}

#3d Plot of objective
library(lattice)
wireframe(-matrVal,drape=T, col.regions=rainbow(100))
library(plotly)
plot_ly(z = ~matrVal) %>% add_surface()



our_position=c(-1,-1)


gd=gradientDescent(f =maximise,x = our_position,a = 10^-2,e =  10^-4,maxIter = 10000  )
sd=steepestDescent(f =maximise,x = our_position,a = 10^-1,e =  10^-4,maxIter = 5000  )
newton=newton_nD(f =maximise,x =our_position,tol=  10^-4,h=10^-5 )






contour(x_seq, x_seq, matrVal, nlevels =50)
points(Clients,col='yellow',cex =2,pch=19)
points(Shops,col='orange',cex =2,pch=19)
text(t(gd$x_opt),'GD',col='blue',cex =2,pch=19)
text(t(sd$x_opt),'SD',col='red',cex =2,pch=19)
text(t(newton$x_opt),'N',col='green',cex =2,pch=19)
points(t(our_position),col='BLACK',cex =2,pch=19)
lines(gd$x_hist,col='blue',cex=5)
lines(sd$x_hist,col='red')


plot_ly(x=~x_seq,y=~x_seq,z=~matrVal,type='contour')%>%add_trace(x=~gd$x_hist[,2],y=~gd$x_hist[,1],type="scatter", mode = 'lines' ,name='GD')%>%add_trace(x=~sd$x_hist[,2],y=~sd$x_hist[,1], mode = 'lines',name='SD')


plot_ly(x=~x_seq,y=~x_seq,z=~matrVal)%>%add_surface()%>%add_trace(x=~gd$x_hist[,2],y=~gd$x_hist[,1],z=~gd$f_hist,type="scatter3d", mode = 'lines' ,name='GD',width = 4)%>%add_trace(x=~sd$x_hist[,2],y=~sd$x_hist[,1],z=~sd$f_hist,type="scatter3d", mode = 'lines',name='SD')
