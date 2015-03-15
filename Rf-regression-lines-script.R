load("data.Rda")
names(means) = c("m","condition","task","participant","z","x","y")

lm_eqn = function(df){
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


meanSubset = subset(means,participant=="Co2" & condition=="hd" & task=="month")
fitSubset = subset(fitAllRF,participant=="Co2" & task=="month")
vect = with(fitSubset,c(fitHead,fitTrunk,fitRoom))
sorted = sort(vect)
ratio = sorted[2]/sorted[1]
ratio = sum(sorted)/sorted[1]



testlm = lm(z ~ x, meanSubset)
testlm = summary(testlm)
pval = pf(testlm$fstatistic[1],testlm$fstatistic[2],testlm$fstatistic[3],lower.tail=FALSE) 
pval

coef = lm_eqn(meanSubset)
coef



fitClu = na.omit(fitAllRF)
cluLabels = paste(as.character(fitClu$participant),as.character(fitClu$task))
clu2 = hclust(dist(fitClu[c("fitHead","fitTrunk","fitRoom")]),method="ward.D")
plot(clu2,labels=cluLabels,main="hierarchical cluster ward")
groups = cutree(clu2,3)
rect.hclust(clu2, k=3, border="red")


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



## ordering wholedout_mini
library(plyr)

wholedout_mini2 = arrange(wholedout_mini,participant,condition,m)
wholedout_mini = wholedout_mini2
