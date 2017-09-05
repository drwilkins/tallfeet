
#Create mode function (weirdly doesn't exist in base R)
   getmode <- function(v) {
   uniqv <- unique(v)
   uniqv<-uniqv[which(!is.na(uniqv))]
   uniqv[which.max(tabulate(match(v, uniqv)))]
   }
   
   #import school data
x<-read.csv("data/footvheight.csv")
x$class<-factor(x$class)
