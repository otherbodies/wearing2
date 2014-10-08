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


lm(z ~ x, meanSubset)

coef = lm_eqn(meanSubset)
coef



fitClu = na.omit(fitAllRF)
cluLabels = paste(as.character(fitClu$participant),as.character(fitClu$task))
clu2 = hclust(dist(fitClu[c("fitHead","fitTrunk","fitRoom")]),method="ward.D")
plot(clu2,labels=cluLabels,main="hierarchical cluster ward")
groups = cutree(clu2,3)
rect.hclust(clu2, k=3, border="red")


