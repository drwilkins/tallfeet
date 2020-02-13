
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
   eq=paste0("y= ",m,"x + ",b)
   out=list(m,b,eq)
   names(out)<-c("slope","intercept","LineEQ")
   return(out)
}
   
   #import school data
x0<-read.csv("data/footvheight_Davis_2020.csv")
names(x0)<-tolower(names(x0)) #lower case everything
names(x0)<-gsub("period","class",names(x0),fixed=T)#if period is used instead of class


g5<-read.csv("data/footvheight_Hayle_2017.csv")
names(g5)[4:5]<-c("height","footsize")
g5.b<-g5[,c("height","footsize","class")]
g5.b$gender=NA
x<-rbind(x0,g5.b)
x$height<-as.numeric(x$height)
x$footsize<-as.numeric(x$footsize)
x$class<-factor(x$class,exclude=NULL)
x$grade<-as.factor(c(rep(7,nrow(x0)),rep(5,nrow(g5))))
