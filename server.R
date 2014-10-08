
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library (ggplot2)
load("data.Rda")

names(means) = c("m","condition","task","participant","z","x","y")
means$x = means$x*-1
shiftedMeans$modelX = shiftedMeans$modelX*-1

##
fitClu = na.omit(fitAllRF)
cluLabels = paste(as.character(fitClu$participant),as.character(fitClu$task))
clu2 = hclust(dist(fitClu[c("fitHead","fitTrunk","fitRoom")]),method="ward.D")
##

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    
    meanSubset = subset(means,participant==input$participant & condition==input$condition & task==input$task)
    shiftedMeanSubset = subset(shiftedMeans,participant==input$participant & task==input$task)
    
    label = input$participant
    fit = input$model
    
    m = lm(z ~ x, meanSubset);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2~~","~italic("angle")~"="~angle, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3),
                          angle = format(atan(m$coef[2])*360/2/pi, digits = 3)
                          )
                     )
    eq2 = as.character(as.expression(eq));
    
    #angle = format(atan(m$coef[2])*360/2/pi, digits = 3)
    
    #gg = ggplot(test,aes(x=x,y=z,color=type,group=lines))+coord_equal()+geom_point()+geom_path()
    gg = ggplot(meanSubset,aes(x=x,y=z)) +coord_equal() +geom_point() +ylim(1000,3000)
    gg = gg +geom_point(x=0,y=2160,size=3,color="blue")
    
    gg = gg +geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
    gg = gg +scale_x_continuous(limits=c(-1500,1500)) +labs(x=label)
    gg = gg +geom_text(x=-100,y=1200,label=eq2,parse=TRUE,size=7)
    if(fit == "plot 45deg shift"){
      gg = gg +geom_point(data=shiftedMeanSubset,aes(x=modelX,y=modelz,color="red"),show_guide=FALSE)
    }
    
    print(gg)
    
    if(fit == "cluster plot"){
      plot(clu2,labels=cluLabels,main="hierarchical clustering (method Ward)")
    }
    
  })
  
    output$textOut = renderText({
      sRF = subset(fitAllRF,participant==input$participant & task==input$task)
      
      
      outText = paste("Fit Head: ",sRF$fitHead," Fit Trunk: ",sRF$fitTrunk," Fit Room: ",sRF$fitRoom)
      
    })
  
  output$textOut2 = renderText({
    sRF = subset(fitAllRF,participant==input$participant & task==input$task)
    vect = with(sRF,c(fitHead,fitTrunk,fitRoom))
    sorted = sort(vect)
    ratio = format((sorted[2]/sorted[1]),digits=3)
    ratio2 = format((sum(sorted)/sorted[1]),digits=3)
    outTxt = paste("Proportions - (Next to best fit/best fit)= ",ratio," and (Sum of all/best fit)= ",ratio2)
    outTxt
    })
})
