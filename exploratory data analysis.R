

data<-read.table("H:/Teaching/UndergraDataMining/data/churn01.txt",
sep="\t", header=TRUE)

data<-data[,-c(4)] #remove the phone numbers


quantile(data$CustServ.Calls,c(0.05,0.1,0.25,0.33, 0.5, 0.66, 0.75,0.9,0.95))

Binned.CustServ.Calls<-1*(data$CustServ.Calls<2)+
2*(data$CustServ.Calls<4)*(data$CustServ.Calls>1) +3*(3<data$CustServ.Calls)
Binned.CustServ.Calls<-factor(Binned.CustServ.Calls,
label=c("Low","Medium","High"))

prop.table(table(Binned.CustServ.Calls))

prop.table(table(data$Churn.,Binned.CustServ.Calls ),2) 


T1<-table(data$Churn.) 
T1R<-T1[c(2,1)]
prop.test(T1R)

T2<-table(data$Intl.Plan,data$Churn.)
T2R<-T2[,c(2,1)]
prop.test(T2R)

prop.test(T1R,p=0.15)

fisher.test(T2)

mean(data$CustServ.Calls)

t.test(data$CustServ.Calls)

t.test(data$CustServ.Calls[data$Churn.=="True."])

t.test(data$CustServ.Calls[data$Churn.=="False."])

t.test(data$CustServ.Calls~data$Churn.)

cor(data$Day.Mins,data$Eve.Mins)