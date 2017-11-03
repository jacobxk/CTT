`itemAnalysis` <-
function(items, itemReport=TRUE, NA.Delete=TRUE, rBisML=FALSE,
         hardFlag, easyFlag, pBisFlag, bisFlag, flagStyle = c("X","")){
  
if(!all(apply(items,c(1,2),is.numeric))) { items <- apply(items,c(1,2),as.numeric)
          warning("Data is not numeric. Data has been coerced to be numeric.")}

if(NA.Delete==FALSE) { items[is.na(items)] <- 0
                       warning("Missing values or NA values are converted to zeros.")} 

items <- na.omit(items)
s <- apply(items,2,var)
N <- ncol(items)

X <- rowSums(items)
alpha <- (N/(N-1))*(1 - sum(s)/var(X))

if(itemReport){
  alphad <- array(dim=N)
  pbis <- array(dim=N)
  bis <- array(dim=N)

  for(i in 1:N){
    Xd <- rowSums(items[,-i])
    pvalu <- colMeans(items)
    alphad[i] <- ((N-1)/(N-2))*(1 - sum(s[-i])/var(Xd))
    pbis[i] <- cor(items[,i],Xd)
    bis[i] <- polyserial(Xd, items[,i], ml=rBisML)
    if(itemReport){
      out <- list(nItem=N,nPerson=nrow(items),alpha=alpha, scaleMean=mean(X), scaleSD=sd(X),
                  itemReport = 
                  data.frame(itemName=names(pvalu), 
                             itemMean=pvalu, pBis=pbis, bis=bis, 
                             alphaIfDeleted=alphad, stringsAsFactors=FALSE))
      rownames(out$itemReport) <- NULL
      if(! missing(hardFlag)){
        out$itemReport <- cbind(out$itemReport, 
                                hard=ifelse(out$itemReport$itemMean < hardFlag, 
                                                 flagStyle[1], flagStyle[2]))
      }
      if(! missing(easyFlag)){
        out$itemReport <- cbind(out$itemReport, 
                                easy=ifelse(out$itemReport$itemMean > easyFlag, 
                                            flagStyle[1], flagStyle[2]))
      }
      if(! missing(pBisFlag)){
        out$itemReport <- cbind(out$itemReport, 
                                lowPBis=ifelse(out$itemReport$pBis < pBisFlag, 
                                               flagStyle[1], flagStyle[2]))
      }
      if(! missing(bisFlag)){
        out$itemReport <- cbind(out$itemReport, 
                                lowBis=ifelse(out$itemReport$bis < bisFlag, 
                                               flagStyle[1], flagStyle[2]))
      }
    }    
  }
} 
else out <- list(nItem=N,nPerson=nrow(items),alpha=alpha, scaleMean=mean(X), scaleSD=sd(X))
class(out) <- "itemAnalysis"
out
}
