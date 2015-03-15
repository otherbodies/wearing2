
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
options(rgl.useNULL=TRUE)

library(shiny)
library(shinyRGL)
library(rgl)

library (ggplot2)
load("data.Rda")
load("dat2d.Rda")

## ordering wholedout_mini
library(plyr)

wholedout_mini2 = arrange(wholedout_mini,participant,condition,m)
wholedout_mini = wholedout_mini2


altSlopeAnalysis = function(subs){
  
  distanceList =  data.frame(distances=numeric(nrow(subs)),id=numeric(nrow(subs))) 
  pointId = 999
  
  # for each point {
  for (i in 1:nrow(subs)){
    
    largestDist = 0
    point = c(subs$z[i],subs$x[i]) 
    
    #debug 
    # point = c(subs$z[1],subs$X[1])
    
    # create a list of distances from that one point to all others
    for (j in 1:nrow(subs)){
      secondPoint = c(subs$z[j],subs$x[j])
      currentDist = dist(rbind(point,secondPoint))
      # distanceList$distances[j] = currentDist
      #  }
      #distanceList    
      
      if(currentDist>largestDist){
        largestDist = currentDist
        pointId = j   
      }  
    }
    distanceList$distances[i]=largestDist
    distanceList$id[i]=pointId
    
  }
  distanceList    
}


names(means) = c("m","condition","task","participant","type","rounds","z","x","y")

names(wholedout_mini)[names(wholedout_mini) == 'X'] <- 'x'

means$x = means$x*-1
shiftedMeans$modelX = shiftedMeans$modelX*-1

wholedout_mini$x = wholedout_mini$x*-1

texteq = "equation"
##
#fitClu = na.omit(fitAllRF)
#cluLabels = paste(as.character(fitClu$participant),as.character(fitClu$task))
#clu2 = hclust(dist(fitClu[c("fitHead","fitTrunk","fitRoom")]),method="ward.D")
##

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    
    label = input$participant
    fit = input$model
    task= input$task
    plane = input$plane
    
    meanSubset = subset(means,participant==input$participant & condition==input$condition & task==input$task)
    shiftedMeanSubset = subset(shiftedMeans,participant==input$participant & task==input$task)
    
    if(task=="mini"){
      meanSubset = subset(wholedout_mini,participant==input$participant & condition==input$condition)
    }
    
    
    
    if(fit == "furthest points"){
      if(task=="mini")
      {
        meanSubset = subset(wholedout_mini,participant==input$participant & condition==input$condition)
        meanSubset2 = subset(wholedout_mini,participant==input$participant & condition=="str")}
        else
      {meanSubset2 = subset(means,participant==input$participant & condition=="str" & task==input$task)}
      
      distList = altSlopeAnalysis(meanSubset2)
      which(distList$distances==max(distList$distances))
      meanSubset = meanSubset[which(distList$distances==max(distList$distances)),]
    }
    
    
    if(plane == "zy plane"){m = lm(y ~ z, meanSubset);}
    else {m = lm(z ~ x, meanSubset);}
    
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic("rse")~"="~r2~~","~italic("angle")~"="~angle~~","~italic("F")~"="~fstat~~","~italic("p")~"="~pval, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$sigma, digits = 3),
                          angle = format(atan(m$coef[2])*360/2/pi, digits = 3),
                          fstat= format(summary(m)$fstatistic[1],digits = 2),
                          pval = format(pf(summary(m)$fstatistic[1],summary(m)$fstatistic[2],summary(m)$fstatistic[3],lower.tail=FALSE),digits = 3, scientific=F)
                          )
                     )
    eq2 = as.character(as.expression(eq));
    texteq = eq2
    #angle = format(atan(m$coef[2])*360/2/pi, digits = 3)
    
    
    
    #gg = ggplot(test,aes(x=x,y=z,color=type,group=lines))+coord_equal()+geom_point()+geom_path()
    if(plane=="2d data"){
      p2d = paste(input$participant,"_2D",sep="")
      #p2d = "Sa1_2D"
      
      temp2d = subset(data2d,user_id==p2d)
      
      gg = ggplot(temp2d,aes(x=mouse_x,y=mouse_y,colour=month_no))+coord_equal()+geom_point()+ylim(0,1050)+xlim(0,1250)
      gg = gg+ scale_colour_brewer(palette="Paired")
      gg
    }
    
    else if(plane=="zy plane"){
      gg = ggplot(meanSubset,aes(x=z,y=y,color=m)) +scale_colour_brewer(palette="Paired") +coord_equal() +geom_point() +xlim(1000,3000)
      gg = gg +geom_point(x=0,y=2160,size=3,color="blue")
      
      gg = gg +geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
      gg = gg +scale_y_continuous(limits=c(-1500,1500)) +labs(x=as.expression(eq))
      
    }    
    else      
    {
    gg = ggplot(meanSubset,aes(x=x,y=z,color=m)) +scale_colour_brewer(palette="Paired") +coord_equal() +geom_point() +ylim(1000,3000)
        
    gg = gg +geom_point(x=0,y=2160,size=3,color="blue")
    
    gg = gg +geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
    gg = gg +scale_x_continuous(limits=c(-1500,1500)) +labs(x=label)
    }
    
    gg = gg +geom_text(x=-100,y=1200,label=eq2,parse=TRUE,size=5,color="black")
    
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
  
  output$textOut3 = renderText({
    outTxt = texteq
    outTxt
  })
  
  output$myWebGL <- renderWebGL({
    
    with(mainTable,points3d(trunk_fit_fur,head_fit_fur,room_fit_fur,col=plot3d_col_month,size=10))
    with(mainTable,text3d(trunk_fit_fur,head_fit_fur,room_fit_fur,bestfit_fur,cex=0.5))
    
    #points3d(1:10, 1:10, 1:10)
    axes3d()
  })
})
