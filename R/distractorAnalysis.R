`distractorAnalysis` <-
function(items, key, scores, nGroups=4, defineGroups, multiKeySep="none",
         multiKeyScore=c("or","poly"), validResp, csvReport, pTable=TRUE, digits){

items <- as.data.frame(items)

mKS <- multiKeySep
mKSc <- multiKeyScore
rm(multiKeySep)
rm(multiKeyScore)

hasUserGroups <- ! missing(defineGroups)
if(hasUserGroups) if(! sum(defineGroups) == 1){
  hasUserGroups <- FALSE
  warning(paste0("Your user defined groups do not sum to 1. Using nGroups = ", nGroups, " instead."))
}

if(missing(key)) warning("Answer key is not provided")
    else{
       if(is.data.frame(key)) key <- sapply(c(key),as.character)
    	 if(! length(key)==ncol(items)) {warning("Answer key is not provided or some item keys are missing.")}
    	 key <- c(key)
    	 }          

if(missing(scores)) scores<- as.data.frame(score(items,key,
                                      multiKeySep=mKS, multiKeyScore=mKSc)$score)

if(hasUserGroups){
  quantMaker  <- cumsum(defineGroups[-length(defineGroups)])
} else{
  quantMaker  <- (1:(nGroups-1))/nGroups
}

labelBaby <- c("lower",
                paste0("mid",as.character(trunc(100*quantMaker[-1]))),"upper")
 
 score.level <- quantile(scores[,1],c(0,quantMaker,1))
 score.level <- cut(scores[,1],score.level,include.lowest=TRUE,labels=labelBaby)
 
 
 itemtab  <- function(response,mKeySep = mKS,theLevels,theLabels) {
   if(! mKeySep == "none"){
     newResp <- response
     newSL   <- score.level
     posKiller <- array()
     posCount <- 0
     for(pospos in 1:length(response)){
       #cat(response[pospos],pospos,"\n")
       #tmpResp <- strsplit(as.character(response[pospos]),mKeySep)[[1]]
       tmpResp <- strsplit(newResp[pospos],mKeySep)[[1]]
       #cat(newResp[pospos],tmpResp,"\n")
       if(length(tmpResp)>1){
         posCount <- posCount+1
         posKiller[posCount] <- pospos
         #newResp <- newResp[-pospos]
         newResp <- c(newResp,tmpResp)
         newSL <- c(newSL,rep(score.level[pospos],length(tmpResp)))
       }
     }
     if(posCount>0){
       response    <- newResp[-posKiller]
       scLabels <- labels(score.level)
       scLevels <- levels(score.level)
       score.level <- newSL[-posKiller]
       #levels(score.level) <- scLabels
       #labels(score.level) <- scLevels
     } 
     #cat(unique(response),"nowREsp",unique(response),"\n")
     #score.level <- newSL
   }
   #cat(levels(score.level),"\n")
   xtabs(~ factor(response,levels=theLevels,labels=theLabels) + score.level)
 }

 all.levels<- sort(unique(unlist(items)))
   
   out<-list() 
   for(i in 1:ncol(items)){
     if(! missing(validResp)){
       if(validResp=="fromItem"){
         if(mKS == "none"){
           all.levels<- sort(unique(unlist(items[,i])))
         } else{
           all.levels<- sort(unique(unlist(strsplit(items[,i],mKS))))
         }   
       } else all.levels <- validResp[[i]]
     }
     outTmp <- as.data.frame.matrix(itemtab(items[,i],
                                           theLevels=all.levels,
                                           theLabels=all.levels))
     colnames(outTmp) <- labelBaby
     
     choiceSum <- rowSums(outTmp)
     pOutTmp <- outTmp/choiceSum
     if(pTable) outTmp <- pOutTmp
     
     if(mKS == "none"){
       correct <- ifelse(key[i] == rownames(outTmp),"*","")
     } else correct <- ifelse(rownames(outTmp) %in%
                                strsplit(key[i],mKS)[[1]],"*","") 
     pBis <- array(dim=nrow(outTmp))
     for(j in 1:nrow(outTmp)){
       if(mKS == "none"){
         tmpItScore <- ifelse(items[,i]==rownames(outTmp)[j],1,0)
         #cat(rownames(outTmp)[j],unique(items[,i]),"\n")         
       } else{
         #cat(unlist(lapply(strsplit(items[,i],mKS), function(XXX,checkUm=rownames(outTmp)[j]) checkUm %in% XXX)),"\n")
         tmpItScore <- ifelse(unlist(lapply(strsplit(items[,i],mKS), function(XXX,checkUm=rownames(outTmp)[j]) checkUm %in% XXX)),
                              1,0)
         #cat(tmpItScore,"\n")  
       }  
       suppressWarnings(pBis[j] <- cor(tmpItScore, scores[,1]-tmpItScore))
       #cat(rownames(outTmp)[j],pBis[j],"\n")
     }                 
     #cat(length(correct),"\n",rownames(outTmp),length(choiceSum),length(choiceSum/sum(choiceSum)),
         #length(pBis), length(pOutTmp$upper), "dim", colnames(pOutTmp),"\n")
     if(missing(digits)){
     frntTmp <- data.frame(correct, key = rownames(outTmp),n = choiceSum, 
                           rspP = choiceSum/sum(choiceSum), pBis, 
                           discrim = pOutTmp$upper - pOutTmp$lower)
     out[[i]] <- cbind(frntTmp,outTmp)
     } else{
     frntTmp <- data.frame(correct, key = rownames(outTmp),n = choiceSum, 
                           rspP = round(choiceSum/sum(choiceSum),digits), pbis = round(pBis,digits), 
                           discrim = round(pOutTmp$upper - pOutTmp$lower,digits))   
     out[[i]] <- cbind(frntTmp,round(outTmp,digits))
     }
     
   }
   
names(out) <- colnames(items)
 
if(! missing(csvReport)){
   for(i in 1:ncol(items)){
     appendIt <- ! i==1
   	 tmpItem <- out[[i]]
     write.table(t(c("Item",names(out)[i])), csvReport, row.names=FALSE, col.names=FALSE, na=" ", append=appendIt, sep=",")
     write.table(t(colnames(tmpItem)),csvReport,row.names=FALSE, col.names=FALSE, na=" ",append=TRUE, sep=",")
   	 write.table(tmpItem,csvReport,row.names=FALSE, col.names=FALSE, na=" ",append=TRUE, sep=",")
     write.table(t(c("","")),csvReport,row.names=FALSE, col.names=FALSE, na=" ",append=TRUE, sep=",")
     write.table(t(c("","")),csvReport,row.names=FALSE, col.names=FALSE, na=" ",append=TRUE, sep=",")
   }
                }
out 
}

