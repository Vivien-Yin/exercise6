# exercise6
#exercise_6
#1
circle<- function(d) {return(area=pi*(d/2)^2)}

#2
trans<- function(oF) {(oC = (oF - 32) * 5/9)
cat("Farenheit :value of",oF, "is equivalent to value", oC, "centigrade")
}
#3
no<- rnorm(100,35,15)
summary_norm<- function(s){
  cat("mean is", mean(s),"\n",
      "median is", median(s),"\n",
      "range is", range(s)) 
}
summary_norm(no)
dens<- density(no)
hist(no,main = "Histogram",freq = FALSE)
lines(dens,lty=1,col="blue")

#4
mi<-function(x) { sort_data<- sort(x);n<- length(x)
  if (n%%2==0) {median_value<- (sort_data[n/2]+sort_data[n/2+1])/2
  }
  else {
    median_value<- n%/%2+1
  }
  return(median_value)
}
#test
mi(1:10) #even:5.5
mi(1:11) #odd:6

#5
Nt_1 <- function(nzero,r,time,K=100) {
  N<-c()
  N[1]<- nzero
  for (t in 1:time) {
    N[t+1]<- N[t]*exp(r*(1-N[t]/K))
  }
    return(N)
}