library(ggplot2)
library(stringr)
library(lubridate)
library(ggthemes)
library(scales)
library(dplyr)

head(maindata)



#main<-maindata[,c(maindata$bucket,maindata$Stopcombo)]

#ggplot(data = maindata, aes(x =bucket)) + geom_histogram()+facet_grid(~Stopcombo) main
#
#ggplot(data = maindata, aes(x =bucket)) + geom_histogram()+facet_wrap(~Stopcombo,ncol = 1)

#ggplot(data = datatravel, aes(x =Bucket))+geom_bar()+facet_wrap(~Combo)

#qplot(x=main$bucket,data=main)+facet_wrap(~main$Stopcombo)

#rm(list=ls())

setwd("D:/Data_mining/DATA")

maindataold = read.csv("outA_new1version3.csv")

maindata=maindataold
maindata<- subset(maindataold,maindataold$Status=="tranist")

maindata<- subset(maindataold,maindataold$Status=="tranist" & maindataold$Frequency >200)

#maindata[,c(maindata$bucket,maindata$Stopcombo)]

#save(maindata,file = "BusdataA.Rda")

tail(maindata,5)
summary(maindata)
##########################################
maindata$bucket<- ifelse(maindata$Totaltime<=50,"0-50",ifelse(maindata$Totaltime>=51 & maindata$Totaltime<=100,"51-100",
                  ifelse(maindata$Totaltime>=101 & maindata$Totaltime<=150,"101-150",ifelse(maindata$Totaltime>=151 & maindata$Totaltime<=200,"151-200",
                  ifelse(maindata$Totaltime>=201 & maindata$Totaltime<=250,"201-250",ifelse (maindata$Totaltime>=251 & maindata$Totaltime<=300,"251-300",
                  ifelse(maindata$Totaltime>=301 & maindata$Totaltime<=350,"301-350",ifelse(maindata$Totaltime>=351 & maindata$Totaltime<=400,"351-400",
                  ifelse(maindata$Totaltime>=401 & maindata$Totaltime<=450,"401-450",ifelse(maindata$Totaltime>=451 & maindata$Totaltime<=500,"451-500",
                  ifelse(maindata$Totaltime>=501 & maindata$Totaltime<=550,"501-550",ifelse(maindata$Totaltime>=551 & maindata$Totaltime<=600,"551-600",
                  ifelse(maindata$Totaltime>=601 & maindata$Totaltime<=650,"601-650",ifelse(maindata$Totaltime>=651 & maindata$Totaltime<=700,"651-700",
                  ifelse (maindata$Totaltime>=701 & maindata$Totaltime<=750,"701-750",ifelse(maindata$Totaltime>=751 & maindata$Totaltime<=800,"751-800",
                  ifelse(maindata$Totaltime>=801 & maindata$Totaltime<=850,"801-850",ifelse(maindata$Totaltime>=851 & maindata$Totaltime<=900,"850-900",
                  ifelse(maindata$Totaltime>=901 & maindata$Totaltime<=950,"901-950",ifelse(maindata$Totaltime>=951 & maindata$Totaltime<=1000,"951-1000","above 1000"))))))))))))))))))))
##########################################
# head(maindata)
# 
# maindata$name<-paste(maindata$Start,maindata$Stop,sep ='-' )
# amonday<-subset(maindata,maindata$DOW=="Monday")
# atuesday<-subset(maindata,maindata$DOW=="Tuesday")
# awednesday<-subset(maindata,maindata$DOW=="Wednesday")
# athursday<-subset(maindata,maindata$DOW=="Thursday")
# afriday<-subset(maindata,maindata$DOW=="Friday")
# 
# stop1<- subset(amonday,amonday$name=="Alumni Center-Briscoe")
# stop1m<- subset(amonday,amonday$name=="Alumni Center-Briscoe" & amonday$Totaltime < 140)# Monday and stop and removing time greater than 140
# 
# stop1t<- subset(atuesday,atuesday$name=="Alumni Center-Briscoe"& atuesday$Totaltime < 130)# Tuesday and stop and removing time greater than 140
# 
# stop1w<- subset(awednesday,awednesday$name=="Alumni Center-Briscoe"& awednesday$Totaltime <120)#wednesday and stop and removing time greater than 140
# 
# 
# 
# head(m)
# 
# mean(stop1$Totaltime)
# 
# 
# 
# barplot(stop1w$Totaltime,main="Alumni_Center-Briscoe",xlab =stop1w$name,ylim=c(0,200))
# 
# 
# 
# counter<- count()
# 
# head(nnd)




# tail(ok)
# 
# 
# di<-kmeans(ok,2)
# 
# di
# di$cluster
# 
# 
# require(dplyr)
# 
# 
# 
# 
# y <- table(data.frame(di$cluster,basedata$Class))
# 
# y
# 
# X<-data.frame(di$cluster,basedata$Sample_code_number)
# 
# X
# 
# #Replacing ? values with mean
# 
# 
# 
# x<- table(maindata$Class,di$cluster)
# #x
# 
# 
# write.csv(y,"original.csv")
# 
# di$totss
# #rm(list=ls())
# 
# # 
# # dcpick$TAT<- ifelse(dcpick$timediff>5, "Above TAT","Within TAT")
# # head(dcpick,n=18)
# #     
# # dcpick$rtn_created_date <- mdy(dcpick$rtn_created_date)
# # 
# # 
# # 
# # tail(dcpick)
# # 
# # dcpick$month <- floor_date(dcpick$rtn_created_date, "month")
# # 
# # dcpick2 <- dcpick %>%
# #   group_by(month,DC_name,TAT)%>%
# #   summarise (TotalDCwise = sum(count))
# # 
# # head(dcpick3)
# #              
# # dcpick3 <- dcpick %>%
# #   group_by(month,DC_name)%>%
# #   summarise (groupDCwise = sum(count))
# 
# dcpick4<- merge(dcpick2 <- dcpick2 <- dcpick %>%
#                   group_by(month,DC_name,TAT)%>%
#                   summarise (TotalDCwise = sum(count)),dcpick3 <- dcpick %>%
#                   group_by(month,DC_name)%>%
#                   summarise (groupDCwise = sum(count)),by=c("DC_name","DC_name","month","month"))
#              
# # another way of code
# 
# dcpick4<- merge(dcpick2 <- dcpick2,dcpick3,by=c("DC_name","month"))
# 
# head(dcpick4)
# 
# dmy("1/11/2015")
# 
# head(dcpick)
# 
# head(dcpick4)
# 
# dcpick5<-dcpick4 %>%
#         group_by(DC_name) %>%
#         mutate(per=round((TotalDCwise/groupDCwise)*100,2))
# 
# 
#                   
# head(dcpick5,n=15)                  
#   
# ggplot(data=dcpick5, aes(x=month, y=DC_name, fill=TAT)) +
#   geom_bar(stat="identity")
# 
# dcpick5$TAT <- cut(dcpick5$TAT, breaks=c(-101, 20, 30, 40, 50, 100))
# dcpick5$month <- paste(month(dcpick5$month, label=TRUE), substring(year(dcpick5$month),3),sep="-")
# dcpick5$month <- factor(dcpick5$month, levels = unique(dcpick5$month), ordered = TRUE)  
# 
# ggplot(data=dcpick5, aes(x=month, y=DC_name, fill=TAT)) +
#   geom_tile() +
#  
#   geom_text(aes(fill =TAT, label = per), size =1, colour="Dark Gray") +
#   theme_tufte()  +
#   theme(axis.text.x = element_text(angle=20, hjust=1, vjust=1)) +
#   scale_fill_brewer(palette="RdYlGn")  
# 
# ggplot(data=dcpoick5, aes(x=month, y=DC_name, fill=TAT)) +
# geom_tile() +
# geom_bar(stat="identity") +
# geom_text(aes(fill = TAT, label = per), size = 10, colour="Dark Gray") +
# theme_tufte() + ggtitle("Title goes here") +
# theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
# scale_fill_brewer(palette="RdYlGn")
# scale_fill_gradient2(low="red", mid="yellow", high="green", midpoint=40)
# scale_y_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
# geom_hline(yintercept=c(30,40,50), linetype=4)
# scale_y_continuous(limits=c(0, 8))
# 
# #  attempt
# 
# ggplot(data=dcpick, aes(x=month, y=per, fill=TAT)) +
#   geom_bar(stat="identity")+ 
#   theme_tufte()
# 
# geom_text(vjust= 1, size=3, colour="Black")
# 
# geom_text(vjust= 1, size=3, colour="Black") +
#   theme_tufte() + 
#   theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
#   scale_y_continuous(limits=c(0,100), breaks=seq(0,100,10))
# 
# rm(list=ls())


library(stringr)
for (i in unique(maindata$Stopcombo)) 
{   
      d<-subset(maindata,maindata$Stopcombo==i)
      #assign(i, d)
      fit<- kmeans(d[,-(1:23)],4)
#    aggregate(d,by=list=(fit$cluster),FUN=mean)
     
      #newdata<-data.frame(d,fit$cluster)
      out<-cbind(d,clusterno=fit$cluster)
      #inner<-data.frame(table(out$Totaltime, out$clusterno))
      #ddply(out,~,summarise,mean=mean(out$Totaltime))
      d <- aggregate(out$Totaltime, by= list(out$clusterno), FUN = mean)
      ds <- aggregate(out$Totaltime, by= list(out$clusterno), FUN = sum)
      d1 <- count(out, clusterno)
      d2 <- cbind(d1, d, ds)
      
      #print (a)
      #assign(i,inner)
      write.csv(out,paste("D:/Data1/Data_", i, ".csv"))
      write.csv(d2,paste("D:/Data1/Data_Summary_", i, ".csv"))
      #print (inner)
      #save(i,file=paste(i,".csv",sep=""))
}


save.image()
