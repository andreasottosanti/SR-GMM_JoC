library(gear)
data(co)
v = evgram(Al ~ 1, co, ~ easting + northing)

r = list()
for(i in 1:500){
  
  D = data.frame(gene = dat[i,], x = pix[,1], y= pix[,2])
  Q = evgram(gene ~ 1, D, ~ x + y, nbins = 50) 
  
  Q = Q$semivariogram%>% as_tibble() %>% mutate(gene = i)
  r[[i]] = Q
  
}


R= do.call(rbind, r)
saveRDS(R,file = "01_OUTPUT/APPLICATION/RDS/semivariogr_50dots.RDS")

ggplot(R)+
  geom_line(aes(x=distance,y=(semivariance),group=gene),lty=1,
            alpha=.3, col="darkblue",lwd=.4)+
  scale_y_log10()
#  geom_point(aes(x=distance,y=log(semivariance),group=gene))




library(geoR)
data(co)
v = variog(Al ~ 1, co, ~ easting + northing)

r = list()

maxD = max(dist(coordinates))
r = list()

for(i in 1:500){
  
  
  cat(i)  
  geodata   <- as.geodata(cbind(pix, dat[i,]))
  variogram <- variog(geodata, breaks = seq(0, maxD, length = 50))
  
  r[[i]] = cbind( dist = variogram$u, 
                  vari = variogram$v,
                  gene = i )
  
}


R = do.call(rbind, r)
saveRDS(R,file = "01_OUTPUT/APPLICATION/RDS/semivariogr_50dots_sotto.RDS")

ggplot(R)+
  geom_line(aes(x=dist,y=(vari),group=gene),lty=1,
            alpha=.3, col="darkblue",lwd=.4)+
  scale_y_log10()
#  geom_point(aes(x=distance,y=log(semivariance),group=gene))
