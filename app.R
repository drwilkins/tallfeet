# Define server logic required to draw a histogram
source("global.R")
server <- function(input, output, session) {
library(shiny);require(cowplot);require(ggplot2);require(rlang)

#define variables for all server modules
vals<-reactiveValues()
isolate(vals$grouping<-NULL)
observe(vals$xrange<-input$xrange)
observe(vals$yrange<-input$yrange)
observe(vals$x2<-subset(x,footsize>vals$xrange[1]&footsize<vals$xrange[2]&height>vals$yrange[1]&height<vals$yrange[2]) )


observeEvent(input$classgroup,
      {vals$grouping <- "class"
      vals$x2_group=lapply(levels(vals$x2$class),FUN=function(x) {subset(vals$x2,class==x)})
      names(vals$x2_group)<-levels(vals$x2$class)
      },ignoreNULL = F)

#Start Plot def
observe(vals$aes<-paste("x=footsize","y=height",paste0("fill=",ifelse(is.null(vals$grouping),"NULL",vals$grouping)),sep=",")) 

observe(#create custom theme for axis formatting
     vals$mytheme<-theme_linedraw()+theme(axis.text=element_text(size=16,margin = margin(r = 20,t=6) ),axis.title=element_text(size=18,face="bold"), strip.text.x = element_text(size = 15)))

output$g <- renderPlot({
  #function def  
  myplot<-function(data,x,y,grp,...){
      x<-enquo(x)
      y<-enquo(y)
      grp<-enquo(grp)
      g<-ggplot(data,aes(!!x,!!y,col=!!grp))+vals$mytheme+theme(aspect.ratio=1)+xlab("Foot Size (in)")+ylab("Height (in)")+ggtitle("Combined Class Data")+
        {if(input$fitline==T){geom_smooth(method="lm",aes(group=1),se=F,linetype="dashed",size=1.2,col="black",show.legend=F)}else{}
        }
      
    if(input$classgroup==T){ 
    #Fit colored lines for ea class for multipanel
    gpanels<-g+geom_point(aes(col=!!grp),stroke=1.2,size=4,pch=21)+geom_point(aes(fill=!!grp),alpha=.2,size=4,pch=21)+facet_wrap(as.formula(paste0(as.character(grp),collapse="")))+if(input$fitline==T){geom_smooth(method="lm",se=F,aes(!!x,!!y,col=!!grp))}else{}
    g1<-plot_grid(g+geom_point(size=4,pch=21,col="#202020",stroke=.9)+guides(fill=F,col=F),gpanels+ggtitle("Separated Class Data")) #combined plot w/ 2 panels & all fit lines
    }else{g1<-g+geom_point(size=4,pch=21,col="#202020",stroke=.9)}
      g1

      }
    
  myplot(vals$x2,x=footsize,y=height,grp=class)
    
    
  #make initial plot (no trend line, color depends on grouping)
    # g<-ggplot(vals$x2,aes_string(vals$aes))+geom_point(size=3,pch=21,col="#202020",stroke=.9)+vals$mytheme+theme(aspect.ratio=1)+xlab("Foot Size (cm)")+ylab("Height (cm)")+ggtitle("Combined Class Data")
    # 
    # #if user wants to fit lines, add smoother to plot
    # if(input$fitline==1){
    #   #Fit dashed line for use in single panel
    #   
    #   # vals$lm<-lm(formula(paste0("height~footsize",ifelse(grouping=="class","+class",""))),data=x)
    #   g1<-g+geom_smooth(method="lm",se=F)+aes_string(col=grouping)+geom_smooth(method="lm",aes(group=1),se=F,linetype="dashed",size=1.2,col="black",show.legend=F)
    #   if(input$classgroup==T){ #Fitline & classgroup
    #   #Fit colored lines for ea class for multipanel
    #   gpanels<-g+geom_smooth(method="lm",se=F)+aes_string(col=grouping,fill=grouping)+facet_wrap(~class)
    #   g2<-plot_grid(g1+guides(fill=F,col=F),gpanels+ggtitle("Separated Class Data")) #combined plot w/ 2 panels & all fit lines
    #   }else{g1<-g+geom_smooth(method="lm",aes(group=1),se=F,linetype="dashed",size=1.2,col="black",show.legend=F)}#Fitline, no classgroup (1 panel)
    # }else{
    #   if(input$classgroup==T)#Classgroup, no fitline
    #   {
    #   g1<-g;gpanels<-g+aes_string(col=grouping,fill=grouping)+facet_wrap(~class)
    #   g2<-plot_grid(g1+guides(fill=F,col=F),gpanels+ggtitle("Separated Class Data"))
    #   }else{g1<-g}#no classgroup or fitline
    # }#End if/else statements 
    # 
    # if(input$classgroup)
    # {
    # plot(g2)
    # }else{plot(g1)}
    # 
    # # #create interactive Plot.ly plot (don't really like the output)
    # # m <- list(l=150, r=20, b=50, t=30) # l = left; r = right; t = top; b = bottom
    # # ggplotly(g,dynamicTicks = T)%>% layout(margin=m)
    
   })#End Plot
   
###################   
#Make a table with summary stuffs


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

#########################      
 #plot histograms if checked
 observe(if(input$hist==T){output$histplot<-renderPlot({

    if(input$classgroup==T){#facet if classgroup checked
  foothist<- ggplot(vals$x2,aes(footsize))+vals$mytheme+geom_histogram(aes(fill=class),binwidth=3,col="#202020",alpha=1)+geom_vline(aes(xintercept=mean(footsize,na.rm=T),col="Mean"),size=1.2)+geom_vline(aes(xintercept=median(footsize,na.rm=T),col="Median"),size=1.2,linetype="dashed")+geom_vline(aes(xintercept=getmode(footsize),col="Mode"),size=1.2,linetype="dotted")+facet_wrap(~class)

  heighthist<-ggplot(vals$x2,aes(height))+vals$mytheme+geom_histogram(aes(fill=class),binwidth=3,col="#202020",alpha=1)+geom_vline(aes(xintercept=mean(height,na.rm=T),col="Mean"),size=1.2)+geom_vline(aes(xintercept=median(height,na.rm=T),col="Median"),size=1.2,linetype="dashed")+geom_vline(aes(xintercept=getmode(height),col="Mode"),size=1.2,linetype="dotted")+facet_wrap(~class)
   }else{#For pooled class data
     foothist<-ggplot(vals$x2,aes(footsize))+vals$mytheme+geom_histogram(binwidth=3,col="#202020",fill="#202020",alpha=.5)+geom_vline(aes(xintercept=mean(footsize,na.rm=T),col="Mean"),size=1.2)+geom_vline(aes(xintercept=median(footsize,na.rm=T),col="Median"),size=1.2,linetype="dashed")+geom_vline(aes(xintercept=getmode(footsize),col="Mode"),size=1.2,linetype="dotted")

   heighthist<-ggplot(vals$x2,aes(height))+vals$mytheme+geom_histogram(binwidth=3,col="#202020",fill="#202020",alpha=.5)+geom_vline(aes(xintercept=mean(height,na.rm=T),col="Mean"),size=1.2)+geom_vline(aes(xintercept=median(height,na.rm=T),col="Median"),size=1.2,linetype="dashed")+geom_vline(aes(xintercept=getmode(height),col="Mode"),size=1.2,linetype="dotted")
   }#End histogram definitions

#Add custom color & legend
   foothist2<-foothist+scale_color_manual(name="Stats",values=c(Mean="#FFA630",Median="#2E5077",Mode="#E8437F"))+guides(col=guide_legend(override.aes=list(linetype=c("dashed","dashed","dotted"))))+xlab("Foot Size (cm)")
   heighthist2<-heighthist+scale_color_manual(name="Stats",values=c(Mean="#FFA630",Median="#2E5077",Mode="#E8437F"))+guides(col=guide_legend(override.aes=list(linetype=c("solid","dashed","dotted"))))+xlab("Height (cm)")

   #extract legend
   legend<-get_legend(heighthist2+theme(legend.position="top",legend.key.height=unit(1.5,"lines")))

   #return histogram plot
   plot_grid(foothist2+guides(col=F,fill=F),heighthist2+guides(col=F,fill=F),plot_grid(legend,align="h"),rel_widths=1,rel_heights=c(1,1,.4),nrow=3 )

 })
 }else{output$histplot<-renderPlot(NULL)} #If hist box not checked, return NULL
)#End observe for plot hist


#Reset button backend
    observeEvent(input$rst,{
      updateSliderInput(session,"xrange",min = min(x$footsize),max = max(x$footsize),value = c(min(x$footsize),max(x$footsize)))
      updateSliderInput(session,"yrange",min = min(x$height),max = max(x$height),value = c(min(x$height),max(x$height)) )
      updateCheckboxInput(session,"classgroup",value=FALSE)
      updateCheckboxInput(session,"fitline",value=FALSE)
      updateCheckboxInput(session,"hist",value=FALSE)
    })#End reset backend    


}#End server


#**********************************************************
#**********************************************************



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring Class Height and Foot Size Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=3,
         sliderInput("xrange",
                     "Foot Size Range (cm)",min = min(x$footsize),max = max(x$footsize),value = c(min(x$footsize),max(x$footsize))),
        sliderInput("yrange",
                     "Height Range (cm)",
                     min = min(x$height),
                     max = max(x$height),
                     value = c(min(x$height),max(x$height))),
        checkboxInput("classgroup","Color by Class?",FALSE),
        checkboxInput("fitline","Fit Lines?",FALSE),
        checkboxInput("hist","Show Histograms?",FALSE),
        actionButton("rst","Reset")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("g")
      )
   ),#sidebar layout
  hr(),
  fluidRow(column(8,tableOutput("summary"),offset=0)),
  fluidRow(plotOutput("histplot"),height=600)
    
)



# Run the application 
shinyApp(ui = ui, server = server)

