
library(shiny);require(cowplot);require(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring Class Height and Foot Size Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=3,
         sliderInput("xrange",
                     "Foot Size Range (cm)",
                     min = min(x$footsize),
                     max = max(x$footsize),
                     value = c(min(x$footsize),max(x$footsize))),
        sliderInput("yrange",
                     "Height Range (cm)",
                     min = min(x$height),
                     max = max(x$height),
                     value = c(min(x$height),max(x$height))),
        checkboxInput("classgroup","Color by Class?",FALSE),
        checkboxInput("fitline","Fit lines?",FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("g")
      )
   ),
  hr(),
  fluidRow(
    column(8,tableOutput("summary"),offset=0)
  )
)


#**********************************************************
#**********************************************************
# Define server logic required to draw a histogram
server <- function(input, output) {

#import school data
x<-read.csv("footvheight.csv")
x$class<-factor(x$class)

#define global variables  
vals<-reactiveValues()
observe(vals$xrange<-input$xrange)
observe(vals$yrange<-input$yrange)
observe(vals$x2<-subset(x,footsize>vals$xrange[1]&footsize<vals$xrange[2]&height>vals$yrange[1]&height<vals$yrange[2]) )

#Start Plot def
   output$g <- renderPlot({
     options(viewer=NULL)

     
     #If user wants to group by class, set this
     if(input$classgroup==T){
     grouping="class"}else{grouping=NULL}
     
  #make initial plot (no trend line, color depends on grouping)
    g<-ggplot(vals$x2,aes(x=footsize,y=height))+geom_point(size=3,pch=21,col="#202020",stroke=.9)+aes_string(fill=grouping)+theme_linedraw()+
      theme(axis.text=element_text(size=16,margin = margin(r = 20,t=6) ),axis.title=element_text(size=18,face="bold"),aspect.ratio=1)+xlab("Foot Size (cm)")+ylab("Height (cm)")+ggtitle("Combined Class Data")
    
    #if user wants to fit lines, add smoother to plot
    if(input$fitline==1){
      g1<-g+geom_smooth(method="lm",se=F)+aes_string(col=grouping)+geom_smooth(method="lm",aes(group=1),se=F,linetype="dashed",size=1.2,col="black",show.legend=F)
      gpanels<-g+geom_smooth(method="lm",se=F)+aes_string(col=grouping,fill=grouping)+facet_wrap(~class)
    }else{g1<-g;gpanels<-g+facet_wrap(~class)}
    
    
    g2<-plot_grid(g1+guides(fill=F,col=F),gpanels+ggtitle("Separated Class Data"))
    
    if(input$classgroup)
    {
    plot(g2)
    }else{plot(g1)}
    
    # #create interactive Plot.ly plot (don't really like the output)
    # m <- list(l=150, r=20, b=50, t=30) # l = left; r = right; t = top; b = bottom
    # ggplotly(g,dynamicTicks = T)%>% layout(margin=m)
    
   })#End Plot
   
###################   
#Make a table with summary stuffs
   #Create mode function (weirdly doesn't exist in base R)
   getmode <- function(v) {
   uniqv <- unique(v)
   uniqv<-uniqv[which(!is.na(uniqv))]
   uniqv[which.max(tabulate(match(v, uniqv)))]
    }

# Define summary table   
    output$summary<-renderTable({
    s<-data.frame(Class="Overall",N=nrow(vals$x2),MeanFootLn=mean(vals$x2$footsize,na.rm=T),MedianFootLn=median(vals$x2$footsize,na.rm=T),ModeFootLn=getmode(vals$x2$footsize),MeanHeight=mean(vals$x2$height,na.rm=T),MedianHeight=median(vals$x2$height,na.rm=T),ModeHeight=getmode(vals$x2$height))
    if(input$classgroup==1){
      classes<-sort(unique(vals$x2$class))
      tmp<-data.frame()
      for(i in 1:length(classes)){
        currclass<-classes[i]
        cdf<-subset(vals$x2,class==currclass)
        cl<-data.frame(Class=as.character(currclass),N=nrow(cdf),MeanFootLn=mean(cdf$footsize,na.rm=T),MedianFootLn=median(cdf$footsize,na.rm=T),ModeFootLn=getmode(cdf$footsize),MeanHeight=mean(cdf$height,na.rm=T),MedianHeight=median(cdf$height,na.rm=T),ModeHeight=getmode(cdf$height))
        tmp<-rbind(tmp,cl)
      }
      s<-rbind(tmp,s)
    }
    names(s)<-c("Class","N","Mean Foot Len","Median Foot Len","Mode Foot Len","Mean Height","Median Height","Mode Height")  
    s
    })#End summary table
   
}#End server

# Run the application 
shinyApp(ui = ui, server = server)

