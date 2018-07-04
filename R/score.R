`score` <-
function(items,key,output.scored=FALSE,ID=NA,rel=FALSE,multiKeySep="none",
         multiKeyScore=c("or","poly")){ 
  t<- as.vector(ID)                                          
  t<- table(ID)  
  if(any(t>1)){ for(i in 1:length(ID)){
                   for(j in 1:nrow(subset(t,t>1))){
                   if(ID[i]==(rownames(subset(t,t>1)))[j])
                   {ID[i]<- paste(ID[i],"/",i)}}}
        warning("Duplicate ID exists; the duplicate ID has been renamed and retained in the calculation")
               }
  
   if(!missing(ID)){
     if(length(ID)==nrow(items)) rownames(items) <- ID
        else warning("The length of ID vector does not match the sample size.")}
               
   if(missing(key)){
    warning("No key provided, assuming pre-scored data.")
	
    scored <- apply(items,2, function(XXX){
                                	if(! is.numeric(XXX)) XXX <- as.numeric(XXX)
									XXX
									})
   } 
  else {
    
    if(length(key)==ncol(items)){
    if(multiKeySep=="none"){
      scored <- t(apply(items,1,function(X){ifelse(X==(key),1,0)}))
    } else{ 
      scored <- array(0,dim=dim(items))
      for(colcol in 1:ncol(items)){
        thisKey <- strsplit(key[colcol],multiKeySep)[[1]]
        thisAnswer <- strsplit(items[,colcol],multiKeySep)
        thisScore <-lapply(thisAnswer,function(XXX,myKey=thisKey,mKS=multiKeyScore){
                       compare <- XXX %in% myKey
                       compare2 <- myKey %in% XXX
                  if(tolower(mKS[1]) == "or" & tolower(mKS[2]) == "poly") oot <- sum(compare) 
                  if(tolower(mKS[1]) == "or" & tolower(mKS[2]) == "dich") oot <- max(compare)
                  if(tolower(mKS[1]) == "and" & tolower(mKS[2]) == "poly") oot <- sum(all(c(compare,compare2))*compare) 
                  if(tolower(mKS[1]) == "and" & tolower(mKS[2]) == "dich") oot <- all(c(compare,compare2))*1 
                       oot})
        scored[,colcol] <- unlist(thisScore)
      }
    }  
    }else stop("Number of items is not equal to the length of key.")
  }
 scores <- rowSums(scored, na.rm = TRUE)
 names(scores)<-paste("P",c(seq(1:nrow(items))),sep="")
 if(!rel==FALSE)reli<-reliability(scored)
 if(output.scored==FALSE & rel==FALSE) out<-list(score=scores)
 if(output.scored==FALSE & rel==TRUE)out<-list(score=scores,reliability=reli) 
 if(output.scored==TRUE & rel==FALSE)out<-list(score=scores,scored=scored)
 if(output.scored==TRUE & rel==TRUE) out<- list(score=scores,reliability=reli, scored=scored)
 out
}

