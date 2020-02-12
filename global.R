
#Create mode function (weirdly doesn't exist in base R)
   getmode <- function(v) {
   uniqv <- unique(v)
   uniqv<-uniqv[which(!is.na(uniqv))]
   uniqv[which.max(tabulate(match(v, uniqv)))]
   }
   
   #extract line equation
ymxb<-function(data,form){
   mod<-lm(as.formula(form),data=data)
   b=round(coef(mod)[1],2)
   m=round(coef(mod)[2],2)
   return(paste0("y= ",m,"x + ",b))
}
   
   #import school data
x<-read.csv("data/footvheight_Davis 2020.csv")
names(x)<-tolower(names(x)) #lower case everything
names(x)<-gsub("period","class",names(x),fixed=T)#if period is used instead of class
x$class<-factor(x$class)
x$height<-as.numeric(x$height)
x$footsize<-as.numeric(x$footsize)
