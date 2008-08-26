`distractor.analysis` <-
function(items,key=NA,scores=NA,p.table=FALSE,write.csv=NA){

items <- as.data.frame(items)
if(length(key)==1) key<-c(rep(key,ncol(items)))

if(missing(scores)) scores<- as.data.frame(score(items,key)$score)
if(missing(key)) warning("Answer key is not provided")
    else if(! length(key)==ncol(items)) {warning("Answer key is not provided or some item keys are missing.")}          

 score.level <- quantile(scores[,1],c(0,1/3,2/3,1))
 score.level <- cut(scores[,1],score.level,include.lowest=TRUE,labels=c("lower","middle","upper"))
 
 
 itemtab <- function(response) {xtabs(~ response + score.level)}
 itemtabp <- function(response) {round(prop.table(xtabs(~ response + score.level),2),3)}
 all.levels<- sort(unique(unlist(items)))
 for(i in 1:ncol(items)){
    items[,i]<-factor(items[,i],levels=all.levels,
                                   labels=ifelse(all.levels==key[i],paste("*",all.levels,sep=""),paste(" ",all.levels,sep="")))
}
  
   out<-list() 
   if(p.table==FALSE) for(i in 1:ncol(items)){
     out[[i]]<-itemtab(items[,i])
    }
    else for(i in 1:ncol(items)){
      out[[i]]<-itemtabp(items[,i])
    }
 names(out) <- colnames(items)
 
if(! is.na(write.csv=="NA")){
   response<- ifelse(all.levels==key[i],paste("*",all.levels,sep=""),paste(" ",all.levels,sep=""))
   item<-c(rep(NA,length(all.levels)))
   for(i in 1:ncol(items)){
         x<-as.data.frame(cbind(item,response,as.vector(out[[i]][,1]),as.vector(out[[i]][,2]),as.vector(out[[i]][,3])))
         names(x)<- c(paste("item_",i),"response","lower","middle","upper")
         
         suppressWarnings(write.csv(x,write.csv,row.names=FALSE,na=" ",append=TRUE))}
                }
names(out)<-paste("item_",c(seq(1:ncol(items))))
out 
 }

