setwd("/Users/sergej/Documents/Master/Thesis/June_Empirical_Analysis/Send_Liza/Original_Data")
library(reshape2)
library(car)
library(qpcR)
library(plyr)
library(Hmisc)
#reconcile aggregation in GGPC with TiVA and ANBRED
#three steps 1.aggregate industries 2. drop industries used for aggregation (keep only the result) 
#3. check with loop for consistency 

tiva2<- read.csv("TIVA2015_EXGR_EXGR_DVA_FD_VA.csv", dec=".", sep=",", header=T)
tiva2$TIME<-NULL
tiva2$Unit.Code<-NULL
tiva2$PowerCode.Code<-NULL
tiva2$PowerCode<-NULL
tiva2$Reference.Period.Code<-NULL
tiva2$Reference.Period<-NULL
tiva2$Flag.Codes<-NULL
tiva2$Flags<-NULL
tiva2$Unit<-NULL
tiva2$Indicator<-NULL
tiva.ind<-as.data.frame(c((as.character(tiva2$Industry),as.character(tiva2$IND))))
tiva2$Industry<-as.character(tiva2$Industry)
tiva2$IND<-as.character(tiva2$IND)
tiva.ind<-c(unique(tiva2$IND),unique(tiva2$Industry))
xtable(as.data.frame(tiva.ind))
tiva2$IND<-gsub("C", "",tiva2$IND)
tiva2$IND<-gsub("T", "-",tiva2$IND)
tiva2$Industry <- gsub(", ", ".",tiva2$Industry)
tiva2$Industry <- gsub("; ", ".",tiva2$Industry)
tiva2$Industry <- gsub(" ", ".",tiva2$Industry)
tiva2$Industry <-revalue(tiva2$Industry,c("Transport.and.storage.post.and.telecommunication"="Transport.storage.post.telecommunication"))
tiva2$Country<-revalue(tiva2$Country,c("Russia"="Russian Federation","China (People's Republic of)"="China"))
tiva2$Partner<-revalue(tiva2$Partner,c("Russia"="Russian Federation","China (People's Republic of)"="China"))


tiva2.2000<-subset(tiva2, Time== 2000)
tiva2.1995<-subset(tiva2, Time== 1995)
tiva2.2005<-subset(tiva2, Time== 2005)

tiva2.EXGR.1995<-subset(tiva2.1995, VAR== "EXGR")
tiva2.EXGR.1995$Time<-NULL
tiva2.EXGR.1995$VAR<-NULL
tiva2.EXGR.1995<-rename(tiva2.EXGR.1995, c("Value"="EXGR.1995"))

tiva2.EXGR.2000<-subset(tiva2.2000, VAR== "EXGR")
tiva2.EXGR.2000$Time<-NULL
tiva2.EXGR.2000$VAR<-NULL
tiva2.EXGR.2000<-rename(tiva2.EXGR.2000, c("Value"="EXGR.2000"))

tiva2.EXGR.2005<-subset(tiva2.2005, VAR== "EXGR")
tiva2.EXGR.2005$Time<-NULL
tiva2.EXGR.2005$VAR<-NULL
tiva2.EXGR.2005<-rename(tiva2.EXGR.2005, c("Value"="EXGR.2005"))

tiva2.VA.1995<-subset(tiva2.1995, VAR== "EXGR_DVA")
tiva2.VA.1995$Time<-NULL
tiva2.VA.1995$VAR<-NULL
tiva2.VA.1995<-rename(tiva2.VA.1995, c("Value"="VA.1995"))

tiva2.VA.2000<-subset(tiva2.2000, VAR== "EXGR_DVA")
tiva2.VA.2000$Time<-NULL
tiva2.VA.2000$VAR<-NULL
tiva2.VA.2000<-rename(tiva2.VA.2000, c("Value"="VA.2000"))

tiva2.VA.2005<-subset(tiva2.2005, VAR== "EXGR_DVA")
tiva2.VA.2005$Time<-NULL
tiva2.VA.2005$VAR<-NULL
tiva2.VA.2005<-rename(tiva2.VA.2005, c("Value"="VA.2005"))

tiva2.FD.VA.1995<-subset(tiva2.1995, VAR== "FD_VA")
tiva2.FD.VA.1995$Time<-NULL
tiva2.FD.VA.1995$VAR<-NULL
tiva2.FD.VA.1995<-rename(tiva2.FD.VA.1995, c("Value"="FD.VA.1995"))

tiva2.FD.VA.2000<-subset(tiva2.2000, VAR== "FD_VA")
tiva2.FD.VA.2000$Time<-NULL
tiva2.FD.VA.2000$VAR<-NULL
tiva2.FD.VA.2000<-rename(tiva2.FD.VA.2000, c("Value"="FD.VA.2000"))

tiva2.FD.VA.2005<-subset(tiva2.2005, VAR== "FD_VA")
tiva2.FD.VA.2005$Time<-NULL
tiva2.FD.VA.2005$VAR<-NULL
tiva2.FD.VA.2005<-rename(tiva2.FD.VA.2005, c("Value"="FD.VA.2005"))

tiva2.FD.VA<-merge(tiva2.FD.VA.2005,tiva2.FD.VA.2000, by=c("COU","Country","PAR","Partner","IND","Industry" ))
tiva2.FD.VA<-merge(tiva2.FD.VA, tiva2.FD.VA.1995, by=c("COU","Country","PAR","Partner","IND","Industry" ))

tiva2.VA<-merge(tiva2.VA.2005,tiva2.VA.2000, by=c("COU","Country","PAR","Partner","IND","Industry" ))
tiva2.VA<-merge(tiva2.VA, tiva2.VA.1995, by=c("COU","Country","PAR","Partner","IND","Industry" ))

tiva2.EXGR<-merge(tiva2.EXGR.2005,tiva2.EXGR.2000, by=c("COU","Country","PAR","Partner","IND","Industry" ))
tiva2.EXGR<-merge(tiva2.EXGR, tiva2.EXGR.1995,  by=c("COU","Country","PAR","Partner","IND","Industry" ))
tiva2.EXGR.VA.FD.VA<-merge(tiva2.EXGR,tiva2.VA, by=c("COU","Country","PAR","Partner","IND","Industry" ))
library(foreign)


# write.dta(merge2, "thesis_data_va_exgr_rd_price_06_july_2015.dta")
#save(tiva2.EXGR.VA.FD.VA, file="tiva_EXGR_VA.Rdata")
#exclusion<-c("Brunei Darussalam" ,"Cambodia","Iceland" ,"Saudi Arabia" ,"Malta" ,"Costa Rica")
#tiva2.EXGR.VA.FD.VA<-subset(tiva2.EXGR.VA.FD.VA, tiva2.EXGR.VA.FD.VA$Country %nin% exclusion)
tiva2.EXGR.VA.FD.VA$Country<-as.character(tiva2.EXGR.VA.FD.VA$Country)
tiva2.EXGR.VA.FD.VA$Partner<-as.character(tiva2.EXGR.VA.FD.VA$Partner)
tiva2.EXGR.VA.FD.VA$Industry<-as.character(tiva2.EXGR.VA.FD.VA$Industry)
write.dta(tiva2.EXGR.VA.FD.VA, "thesis_data2_va_exgr_06_july_2015.dta")
tiva2.EXGR.VA.FD.VA<-merge(tiva2.EXGR.VA.FD.VA, tiva2.FD.VA , by=c("COU","Country","PAR","Partner","IND","Industry"))
tiva2.EXGR.VA.FD.VA$Country<-as.character(tiva2.EXGR.VA.FD.VA$Country)
tiva2.EXGR.VA.FD.VA$Partner<-as.character(tiva2.EXGR.VA.FD.VA$Partner)
setwd("/Users/sergej/Documents/Master/Thesis/Send_Liza/Original_Data")
##TIVA Dollar guess Current dollar
# tiva<- read.csv("TIVA2015_C1_18062015172524082.csv", dec=".", sep=",", header=T)
# tiva$TIME<-NULL
# tiva$Unit.Code<-NULL
# tiva$PowerCode.Code<-NULL
# tiva$PowerCode<-NULL
# tiva$Reference.Period.Code<-NULL
# tiva$Reference.Period<-NULL
# tiva$Flag.Codes<-NULL
# tiva$Flags<-NULL
# tiva$Unit<-NULL
# unique(tiva$IND)
# tiva.2000<-subset(tiva, Time== 2000)
# tiva.1995<-subset(tiva, Time== 1995)
# tiva.2005<-subset(tiva, Time== 2005)
# tiva.EXGR.1995<-subset(tiva.1995, VAR== "EXGR")
# tiva.EXGR.2000<-subset(tiva.2000, VAR== "EXGR")
# tiva.EXGR.2005<-subset(tiva.2005, VAR== "EXGR")
# 
# tiva.VA.1995<-subset(tiva.1995, VAR== "EXGR_DVA")
# tiva.VA.2000<-subset(tiva.2000, VAR== "EXGR_DVA")
# tiva.VA.2005<-subset(tiva.2005, VAR== "EXGR_DVA")
# 
# #ISIC C50T52 C55 "Transport.storage.post.telecommunication"
# 
# #ISIC 60-64- C60T63 - Transport and storage+ C64 - Post and telecommunications
# 
# 
# nms     <- c(paste0("tiva.EXGR.",c(1995,2000, 2005)),paste0("tiva.VA.",c(1995,2000, 2005)))
# df.list <- lapply(nms, get)
# myFunc <- function(DF) {
#  DF$VAR<-NULL
#  DF$IND <- NULL
#  DF$Indicator<- NULL 
#  DF$COU<-NULL
#  DF$PAR<-NULL
#  DF<-dcast(DF,Country+Partner+Time ~ Industry, value.var="Value")
# return(DF) 
# }
# df.list<-lapply(df.list, myFunc)
# 
# for (i in seq_along(df.list)){
#   colnames(df.list[[i]]) <- gsub(", ", ".",names(df.list[[i]]))
#   colnames(df.list[[i]]) <- gsub("; ", ".",names(df.list[[i]]))
#   colnames(df.list[[i]]) <- gsub(" ", ".",names(df.list[[i]]))
# }
# 
# 
# myaggregate<-function(DF){
# DF$Transport.storage.post.telecommunication<-rowSums(DF[c(26,21)], na.rm = TRUE)
# DF<-DF[, !(colnames(DF) %in% c("Transport.and.storage", "Post.and.telecommunications","Wood.paper.paper.products.printing.and.publishing"))]
# return(DF) 
# }
# 
# 
# df.list<-lapply(df.list,myaggregate)
# melting<- function(DF){
# DF<- melt(DF, id=c("Country", "Partner","Time"), measure.vars=c("Agriculture.hunting.forestry.and.fishing"         
#                                                                            , "Basic.metals.and.fabricated.metal.products"        ,  "Chemicals.and.chemical.products"                  
#                                                                            , "Coke.refined.petroleum.products.and.nuclear.fuel"  , "Community.social.and.personal.services."          
#                                                                            , "Construction"                                      ,"Electrical.and.optical.equipment"                 
#                                                                            , "Electricity.gas.and.water.supply"                  ,"Financial.intermediation"                         
#                                                                            , "Food.products.beverages.and.tobacco"               ,"Hotels.and.restaurants"                           
#                                                                            , "Machinery.and.equipment.nec."                      ,"Manufacturing.nec.recycling."                     
#                                                                            , "Mining.and.quarrying"                              ,"Other.non-metallic.mineral.products"              
#                                                                            , "Pulp.paper.paper.products.printing.and.publishing" ,"Real.estate.renting.and.business.activities"      
#                                                                            , "Rubber.and.plastics.products"                      ,"Textiles.textile.products.leather.and.footwear"   
#                                                                            ,"Transport.equipment"                                ,"Wholesale.and.retail.trade.repairs"               
#                                                                            , "Wood.and.products.of.wood.and.cork"             
#                                                                            , "Transport.storage.post.telecommunication"  )       , variable.name='Industry', value.name='exports')
# return(DF) 
# }
# 
# df.list<-lapply(df.list, melting)
# 
# #merge both indicators Gross Exports & Direct Value Added of Exports for all three periods into one DF 
# names(df.list)<- c("EXGR1995","EXGR2000","EXGR2005","VA1995","VA2000","VA2005")
# 
# colnames(df.list$EXGR1995)[colnames(df.list$EXGR1995)=="exports"] <- "EXGR1995"
# colnames(df.list$VA1995)[colnames(df.list$VA1995)=="exports"] <- "VA1995"
# 
# colnames(df.list$EXGR2000)[colnames(df.list$EXGR2000)=="exports"] <- "EXGR2000"
# colnames(df.list$VA2000)[colnames(df.list$VA2000)=="exports"] <- "VA2000"
# 
# colnames(df.list$EXGR2005)[colnames(df.list$EXGR2005)=="exports"] <- "EXGR2005"
# colnames(df.list$VA2005)[colnames(df.list$VA2005)=="exports"] <- "VA2005"
# 
# va.exgr.1995<-merge(df.list$EXGR1995,df.list$VA1995, by=c("Country","Industry","Partner"))
# va.exgr.2000<-merge(df.list$EXGR2000,df.list$VA2000, by=c("Country","Industry","Partner" ))
# va.exgr.2000$Time.x<-NULL
# va.exgr.2000$Time.y<-NULL
# va.exgr.2005<-merge(df.list$VA2005, df.list$EXGR2005, by=c("Country","Industry","Partner" ))
# 
# va.exgr.1995.2000<-merge(va.exgr.1995,va.exgr.2000, by=c("Country","Industry","Partner" ))
# va.exgr.1995.2000$Time.x<-NULL
# va.exgr.1995.2000$Time.y<-NULL
# 
# 
# va.exgr.1995.2000.2005<-merge(va.exgr.1995.2000,va.exgr.2005, by=c("Country","Industry","Partner" ))
# va.exgr.1995.2000.2005$Time.x<-NULL
# va.exgr.1995.2000.2005$Time.y<-NULL
# va.exgr.1995.2000.2005$Country<-revalue(va.exgr.1995.2000.2005$Country,c("Russia"="Russian Federation","China (People's Republic of)"="China"))
# va.exgr.1995.2000.2005$Partner<-revalue(va.exgr.1995.2000.2005$Partner,c("Russia"="Russian Federation","China (People's Republic of)"="China"))
# 
# #remove temporary results of previous merging
# tiva.1995<-NULL
# tiva.2005<-NULL
# va.exgr.1995<-NULL
# va.exgr.2000<-NULL
# va.exgr.2005<-NULL
# va.exgr.1995.2005<-NULL
# tiva.EXGR.1995<-NULL
# tiva.VA.1995<-NULL
# tiva.EXGR.2000<-NULL
# tiva.VA.2000<-NULL
# tiva.EXGR.2005<-NULL
# tiva.VA.2005<-NULL
# tiva.2000<-NULL
# va.exgr.1995.2000<-NULL
#consistency check compare if for each value of till industry 50 the values are the same as for the basic data frames 
# link Tiva RCA ranking to endowment *** complementary measure centrality measures importance (relative) 
#R%D 2005 Dollars constant PPP 

  
R.D.STAN<-read.csv("ANBERD2011_REV3_18062015172305316.csv", dec=".", sep=",", header=T)
R.D.STAN$Flag.Codes <- NULL
R.D.STAN$Flags <- NULL
R.D.STAN$PowerCode <- NULL
R.D.STAN$PowerCode.Code <- NULL
R.D.STAN$Reference.Period.Code <- NULL
R.D.STAN$Reference.Period <- NULL
R.D.STAN$CUR <- NULL
R.D.STAN$Measure<-NULL 
R.D.STAN$TIME <- NULL
R.D.STAN$Unit <- NULL
R.D.STAN$Unit.Code <- NULL
R.D.STAN$IND  <- NULL

levels(R.D.STAN$COU) <- gsub("_MA","",levels(R.D.STAN$COU))
levels(R.D.STAN$COU) <- gsub("_PF","",levels(R.D.STAN$COU))
R.D.STAN<-subset(R.D.STAN,Time==2005)
R.D.STAN$Time<-NULL
R.D.STAN <- dcast(R.D.STAN, Country+COU  ~ Industry, value.var="Value")
colnames(R.D.STAN) <- gsub(", ", ".",names(R.D.STAN))
colnames(R.D.STAN) <- gsub("; ", ".",names(R.D.STAN))
colnames(R.D.STAN) <- gsub(" ", ".",names(R.D.STAN))
#ISIC 27-28 Basic metals and fabricated metal products
names(R.D.STAN)
R.D.STAN$Basic.metals.fabricated.metal.products <-rowSums(R.D.STAN[c(4,11)],na.rm=T)
R.D.STAN[,c("Basic.metals" ,"Fabricated.metal.products.except.machinery.and.equipment","Basic.metals.fabricated.metal.products")]
R.D.STAN$Basic.metals.fabricated.metal.products[19] <- NA #19 all NA 
for (i in 1:35){
  check<-all.equal(R.D.STAN[i,4]+ R.D.STAN[i,11],R.D.STAN[i,35])
  print(check)
}

R.D.STAN <- R.D.STAN[, !(colnames(R.D.STAN) %in% c("Basic.metals" ,"Fabricated.metal.products.except.machinery.and.equipment"))]

#ISIC 30-33
names(R.D.STAN)
R.D.STAN$Electrical.optical.equipment <-rowSums(R.D.STAN[c(8,17,20,24)],na.rm=T) #8"Electrical.machinery.and.apparatus.nec" 17 Medical.precision.and.optical.instruments 20"Office.accounting.and.computing.machinery"  24"Radio.television.and.communication.equipment"
R.D.STAN[,c("Electrical.machinery.and.apparatus.nec","Office.accounting.and.computing.machinery","Radio.television.and.communication.equipment",  
            "Medical.precision.and.optical.instruments","Electrical.optical.equipment" )]  
R.D.STAN$Electrical.optical.equipment[c(8,19,22)] <- NA #write function which sets value of R.D.STAN colum na if sum NA=4
for (i in 1:35){
  check<-all.equal(R.D.STAN[i,8]+R.D.STAN[i,17]+ R.D.STAN[i,20]+ R.D.STAN[i,24],R.D.STAN[i,34])
  print(check)
}
R.D.STAN <- R.D.STAN[, !(colnames(R.D.STAN) %in% c("Electrical.machinery.and.apparatus.nec","Office.accounting.and.computing.machinery","Radio.television.and.communication.equipment",  
                                                   "Medical.precision.and.optical.instruments"))]
#ISIC 34-35


names(R.D.STAN)
R.D.STAN$Transport.equipment<-rowSums(R.D.STAN[c(17,19)],na.rm=T) #17] "Motor.vehicles.trailers.and.semi-trailers" 19] "Other.transport.equipment"
R.D.STAN[,c("Motor.vehicles.trailers.and.semi-trailers","Other.transport.equipment","Transport.equipment")]
R.D.STAN$Transport.equipment[c(15,19,22)] <- NA # all NA rows correct
for (i in 1:35){
  check<-all.equal(R.D.STAN[i,19]+ R.D.STAN[i,17],R.D.STAN[i,31])
  print(check)
}

R.D.STAN <- R.D.STAN[, !(colnames(R.D.STAN) %in% c("Motor.vehicles.trailers.and.semi-trailers","Other.transport.equipment"))]
R.D.STAN <- R.D.STAN[, !(colnames(R.D.STAN) %in% c("Recycling","Furniture.manufacturing.nec"))]
#ISIC 17-19
names(R.D.STAN)
R.D.STAN$Textiles.textile.products.leather.and.footwear<-rowSums(R.D.STAN[c(12,22,20)],na.rm=T) # 12 "Leather.leather.products.and.footwear" 20 Textiles"  22Wearing.apparel.dressing.and.dying.of.fur"    
R.D.STAN[,c("Leather.leather.products.and.footwear","Textiles","Wearing.apparel.dressing.and.dying.of.fur", "Textiles.textile.products.leather.and.footwear")]

R.D.STAN$Textiles.textile.products.leather.and.footwear[c(35,34,31,29,22,19,17,15)] <- NA # all NA rows correct
for (i in 1:35){
  check<-all.equal(R.D.STAN[i,12]+ R.D.STAN[i,22]+ R.D.STAN[i,20],R.D.STAN[i,28])
  print(check)
}
R.D.STAN <- R.D.STAN[, !(colnames(R.D.STAN) %in% c("Leather.leather.products.and.footwear","Textiles","Wearing.apparel.dressing.and.dying.of.fur"))]
R.D.STAN.final<- melt(R.D.STAN , id=c("Country","COU"), measure.vars=c("AGRICULTURE.HUNTING.FORESTRY.AND.FISHING",               "Chemicals.and.chemical.products",                  
                                                                       "Coke.refined.petroleum.products.and.nuclear.fuel",  "Community.social.and.personal.services",           
                                                                       "CONSTRUCTION",                                      "ELECTRICITY.GAS.AND.WATER.SUPPLY",                 
                                                                       "Financial.intermediation" ,                         "Food.products.beverages.and.tobacco",              
                                                                       "Hotels.and.restaurants",                            "Machinery.and.equipment.nec",                      
                                                                       "Manufacturing.n.e.c..and.recycling",                "MINING.AND.QUARRYING",                             
                                                                       "Other.non-metallic.mineral.products" ,              "Pulp.paper.paper.products.printing.and.publishing",
                                                                       "Real.estate.renting.and.business.activities" ,      "Rubber.and.plastics.products",                     
                                                                       "Transport.storage.and.communications"   ,           "Wholesale.and.retail.trade.repairs" ,              
                                                                       "Wood.and.products.of.wood.and.cork"      ,          "Basic.metals.fabricated.metal.products",           
                                                                       "Electrical.optical.equipment"          ,            "Transport.equipment",                              
                                                                       "Textiles.textile.products.leather.and.footwear"      ), variable.name='Industry', value.name='R.D.expenditure')

#ANBRED Country Adjustment

levels(R.D.STAN.final$Country)<-gsub(" - Main activity", "",levels(R.D.STAN.final$Country))
levels(R.D.STAN.final$Country)<-gsub(" - Product field", "",levels(R.D.STAN.final$Country))
R.D.STAN.final$Country<-tolower(R.D.STAN.final$Country)
R.D.STAN.final$Country<-capitalize(R.D.STAN.final$Country)

R.D.STAN.final$Country<-revalue(R.D.STAN.final$Country, c("Chinese taipei"="Chinese Taipei","Czech republic"="Czech Republic","New zealand"="New Zealand", 
                                                          "Russian federation"= "Russian Federation","Slovak republic"="Slovak Republic","South africa"="South Africa","United kingdom" ="United Kingdom","United states" ="United States")   )

#Find common countries in three datasets and reduce observations to these

#read price data
GGPC <-read.csv(file="2005_industry_prices.csv",sep=";", header = T, dec=",") 

GGPC$Country<-revalue(GGPC$Country,c("South Korea"="Korea"))

Stan.country <- as.character(unique(R.D.STAN.final$Country))

GGPC.Countries <-as.character(unique((GGPC$Country)))


# TiVA.Countries <- as.character(unique((va.exgr.1995.2000.2005$Country)))
TiVA.Countries <-as.character(unique(tiva2.EXGR.VA.FD.VA$Country))
tiva.anbred<-intersect(Stan.country,TiVA.Countries)
tiva.anbred.GGPC <- intersect(GGPC.Countries, tiva.anbred)

#weights for aggregation of GGPC Price data from output
Sectoral_output<-read.csv(file="STAN_Sectoral_Output_2005.csv",header = T, dec=",") 
Sectoral_output$VAR<-NULL
Sectoral_output$IND<- NULL
Sectoral_output$TIME<-NULL
Sectoral_output$Time<-NULL
Sectoral_output$Flag.Codes<-NULL
Sectoral_output$Flags<-NULL
Sectoral_output$Variable<-NULL
Sectoral_output$COU<-NULL

sectoral.output.country<-as.character(levels(Sectoral_output$Country)) 
intersect(tiva.anbred.GGPC,sectoral.output.country)
# missing countries in STAN compared to previous  "Russian Federation" "South Africa" Romania" "Turkey" 

Sectoral_output.short<-subset(Sectoral_output,Sectoral_output$Country %in% tiva.anbred.GGPC)

#added the countries without observation

missing.countries<-rep(c( "Russian Federation","South Africa" ,"Romania","Turkey"),35)
values<-rep(".",140)
industries<-as.character(unique(Sectoral_output.short$Industry))
industries<-rep(industries,4)

Missing.value.countries <- data.frame(missing.countries,industries,values)
Missing.value.countries$values<-"."
is.na(Missing.value.countries$values) <- Missing.value.countries$values=="."
Missing.value.countries$values<-as.numeric(Missing.value.countries$values)
Missing.value.countries<-rename(Missing.value.countries, c("missing.countries"="Country", "values"="Value","industries"="Industry"))
Missing.value<-Missing.value.countries
Missing.value$values<-NULL
test<-rbind(Sectoral_output.short,Missing.value)
test<- subset(test,test$Country%in% tiva.anbred.GGPC)



test$Industry<-revalue(test$Industry, c( "C01T05 AGRICULTURE, HUNTING, FORESTRY AND FISHING"  =  "Agriculture..forestry...fishing" ,                                               
                                         "C10T14 MINING AND QUARRYING" =  "Mining...quarying",                                                                       
                                         "C15T16 Food products, beverages and tobacco"= "Food..beverage...tobacca"  ,                                                      
                                         "C17T18 Textiles and textile products"=         "Textile.products"         ,                                                       
                                         "C19 Leather, leather products and footwear"=                "Leather...footwear"    ,                                                      
                                         "C20 Wood and products of wood and cork"="Wood.products"                ,                                             
                                         "C21T22 Pulp, paper, paper products, printing and publishing"  =                 "Paper..printing...publishing" ,                                   
                                         "C23 Coke, refined petroleum products and nuclear fuel"="Coke...refined.petroleum",                                              
                                         "C24 Chemicals and chemical products" ="Chemical.products"             ,                                                  
                                         "C25 Rubber and plastics products"      =          "Rubber...plastics",                                                             
                                         "C26 Other non-metallic mineral products"  =     "Non.metallic.mineral.products"      ,                                                    
                                         "C27T28 Basic metals and fabricated metal products"= "Basic...fabricated.metal" ,                                                  
                                         "C29 Machinery and equipment, n.e.c."    =      "Machinery"   ,                                                            
                                         "C30T33 Electrical and optical equipment"="Electrical...optical.equipment" ,                                                           
                                         "C34T35 Transport equipment"="Transport.Equipment"     ,                                                                
                                         "C36T37 Manufacturing n.e.c. and recycling"        =             "Other.manufacturing" ,                                                       
                                         "C40T41 ELECTRICITY GAS AND, WATER SUPPLY" = "Utiliteis"  ,                                                   
                                         "C45 CONSTRUCTION"                         = "Construction",                                                               
                                         "C50 Sale, maintenance and repair of motor vehicles and motorcycles - retail sale of automotive fuel"="Motor.vehicle...fuel.trade" ,
                                         "C51 Wholesale, trade and commission excl. motor vehicles"  ="Wholesale.trade"  ,                                         
                                         "C52 Retail trade excl. motor vehicles - repair of household goods"        = "Retail.trade" ,                          
                                         "C55 Hotels and restaurants"=  "Hotels...restaurants",                                                                         
                                         "C60 Land transport - transport via pipelines"  ="Land.transport" ,                                                     
                                         "C61 Water transport"="Water.transport",                                                                                
                                         "C62 Air transport"=                  "Air.transport",                                                                                  
                                         "C63 Supporting and auxiliary transport activities"= "Transport.services",                                                   
                                         "C64 Post and telecommunications"= "Post...telecommunications" ,                                                                    
                                         "C65T67 Financial intermediation"=       "Financial.services"    ,                                                                    
                                         "C70 Real estate activities"=           "Real.estate",                                                                         
                                         "C71T74 Renting of mach. and equip. - other business activities" ="Business.services"  ,                                    
                                         "C75 Public admin. and defence - compulsory social security"  = "Government" ,                                       
                                         "C80 Education"=        "Education"   ,                                                                                      
                                         "C85 Health and social work"= "Health" ,                                                                         
                                         "C90T93 Other community, social and personal services" =                          "Other.services" ,                                              
                                         "C95 Private households with employed persons"   = "Households.with.employed.persons"))
test<-dcast(test,Country ~ Industry, value.var="Value")



#compute weights for weighted average
 
test$weight17<- test$Textile.products/rowSums(test[,5:6], na.rm=F)


#
test$weight50<-test$Motor.vehicle...fuel.trade/rowSums(test[,20:23],  na.rm=F)
test$weight51<-test$Wholesale.trade/rowSums(test[,20:23],  na.rm=F) 
test$weight52<-test$Retail.trade/rowSums(test[,20:23],  na.rm=F) 
test$weight55<-test$Hotels...restaurants/rowSums(test[,20:23],  na.rm=F)


#consistency check
one<-rep(1,29)
test$one<-one
for (i in 1:29){
  check <- all.equal(test[i,38]+test[i,40]+test[i,41]+test[i,39],test[i,42]) 
  print(check)
}
#next weights of 75,80,85,90-93,95
test$weight75<-test$Government/rowSums(test[,32:36], na.rm=F)
test$weight80<-test$Education/rowSums(test[,32:36], na.rm=F)
test$weight85<-test$Health/rowSums(test[,32:36], na.rm=F)
test$weight90.93<-test$Other.services/rowSums(test[,32:36], na.rm=F)
test$weight95<-test$Households.with.employed.persons/rowSums(test[,32:36], na.rm=F)

# test$weight95+test$weight90.93+test$weight90+test$weight80+test$weight75==1 
#weights ISIC 70 71-74 
test$weight70 <-test$Real.estate/rowSums(test[,30:31])
test$weight71.74<-test$Business.services/rowSums(test[,30:31])


# #consistency check
# test$weight70+test$weight71.74==1 


test$weight60<-test$Land.transport  /rowSums(test[,24:28], na.rm=F) 
test$weight61<-test$Water.transport /rowSums(test[,24:28], na.rm=F)
test$weight62<-test$Air.transport    /rowSums(test[,24:28], na.rm=F)
test$weight63<-test$Transport.services/rowSums(test[,24:28], na.rm=F)
test$weight64<-test$Post...telecommunications/rowSums(test[,24:28], na.rm=F)

# for (i in 1:29){
#   check[i] <- all.equal(test[i,50]+test[i,51]+test[i,52]+test[i,53]+test[i,54],test[i,42]) 
#   print(check)
# }
#equal up to tolerance of 1.5e-8
# detach(test)

test <-subset(test,select =c("Country","weight17","weight70","weight71.74","weight55","weight52","weight51","weight50","weight75","weight80","weight85","weight90.93","weight95","weight60","weight61","weight62","weight63","weight64"))
#weighted average of prices
GGPC.orig<-GGPC
GGPC<-GGPC.orig
GGPC<-merge(GGPC,test,by=c("Country"))
#ISIC 17-19 Textiles_leather_and_footwear
#first save weighted prices
#second sum prices ignore NA
#check if only NA rows, in case replace price with NA 
#ISC 17-19 Textiles and Textile Products + Leather, Leather and Footwear

GGPC$Textiles.textile.products.leather.and.footwear<-0
GGPC$V54<-NULL
names(GGPC)
for (i in 1:29) {
GGPC[i,54] <-  if(sum(is.na(GGPC[i,37])>0)) {1/2*GGPC[i,5]+1/2*GGPC[i,6]} else {GGPC[i,37]*GGPC[i,5]+(1-GGPC[i,37])*GGPC[i,6]} 
}
                                                              names(GGPC)
GGPC[,c('Textile.products','Leather...footwear','Textiles.textile.products.leather.and.footwear')]

GGPC <- GGPC[, !(colnames(GGPC) %in% c("weight17","Textile.products", "Leather...footwear"))]

#ISIC 50-52 wholesale.retail.trade.repairs
#"C50 Sale, maintenance and repair of motor vehicles and motorcycles - retail sale of automotive fuel"="Motor.vehicle...fuel.trade" ,
#"C51 Wholesale, trade and commission excl. motor vehicles"  ="Wholesale.trade"  ,                                         
#"C52 Retail trade excl. motor vehicles - repair of household goods"        = "Retail.trade" ,                          
#"C55 Hotels and restaurants"=  "Hotels...restaurants",     
#Motor vehicle trade and repair and fuel sales+Wholesale Trade, except motor vehicle+Retail Trade, except motor vehicles; repair of household goods
#[37] "weight55"                                       "weight52"                                      
#[39] "weight51"                                       "weight50"  

#[18]                           "Motor.vehicle...fuel.trade"                    
#[19] "Wholesale.trade"                                "Retail.trade"                                  
#[21] "Hotels...restaurants"                           "Land.transport"    

GGPC$Wholesale.and.retail.trade.repairs <-0
names(GGPC)
for (i in 1:29) {
  GGPC[i,52]<-if(sum(is.na(GGPC[i,37])) >0 | sum(is.na(GGPC[i,38])) >0 |sum(is.na(GGPC[i,39])) >0 | sum(is.na(GGPC[i,40]))>0)  {1/4*GGPC[i,18]+1/4*GGPC[i,19]+1/4*GGPC[i,20]+1/4*GGPC[i,21]} else{GGPC[i,40]*GGPC[i,18]+GGPC[i,39]*GGPC[i,19]+GGPC[i,38]* GGPC[i,20]+GGPC[i,37]*GGPC[i,21]} 
}
GGPC[,c("Motor.vehicle...fuel.trade",'Wholesale.trade','Retail.trade',"Hotels...restaurants",'Wholesale.and.retail.trade.repairs')]
#check for loop correct

GGPC <- GGPC[, !(colnames(GGPC) %in% c("Motor.vehicle...fuel.trade",'Wholesale.trade','Retail.trade',"weight55","weight52","weight51","weight50"))]
#ISIC 60-64 Transport.storage.post.telecommunication
#"C60 Land transport - transport via pipelines"  ="Land.transport" ,
#"C61 Water transport"="Water.transport", "C62 Air transport"= "Air.transport",
#"C63 Supporting and auxiliary transport activities"= "Transport.services",
#"C64 Post and telecommunications"= "Post...telecommunications" ,                                                                    

# [39] "weight60"                                       "weight61"                                      
# [41] "weight62"                                       "weight63"                                      
# [43] "weight64"    
# [19] "Land.transport"                                 "Water.transport"                               
# [21] "Air.transport"                                  "Transport.services"                            
# [23] "Post...telecommunications"   

GGPC$Transport.storage.post.telecommunication<-0
names(GGPC)
for (i in 1:29) {
GGPC[i,46]<- if(sum(is.na( GGPC[i,39]))>0 |sum(is.na(GGPC[i,40]))>0 |sum(is.na(GGPC[i,41]))>0 | sum(is.na(GGPC[i,42]))>0 | sum(is.na(GGPC[i,43]))>0) {1/5*GGPC[i,19]+1/5*GGPC[i,20]+1/5*GGPC[i,21]+1/5*GGPC[i,22]+1/5*GGPC[i,23]} else{GGPC[i,39]* GGPC[i,19]+GGPC[i,40]*GGPC[i,20]+GGPC[i,41]*GGPC[i,21]+GGPC[i,42]*GGPC[i,22]+GGPC[i,43]*GGPC[i,23]} 
}
names(GGPC)

GGPC[,c("Land.transport","Water.transport","Air.transport","Transport.services","Post...telecommunications",'Transport.storage.post.telecommunication')]

GGPC <- GGPC[, !(colnames(GGPC) %in% c("Land.transport","Water.transport","Air.transport","Transport.services","Post...telecommunications","weight60" , "weight61", "weight62", "weight63", "weight64"))]

#ISIC 70-74 Real.estate.renting.business.activities 
#Real Estate Activities Renting of M&Eq and Other Business Activities
# "Real.estate"     [27] weight70         [28]weight71.74                   
# [21] "Business.services"
names(GGPC)
GGPC$Real.estate.renting.business.activities<-0
names(GGPC)

for (i in 1:29)  {
 GGPC[i,37] <-if(sum(is.na(GGPC[i,27]))>0 |sum(is.na(GGPC[i,28]))>0 ) {1/2*GGPC[i,20]+1/2*GGPC[i,21] } else{ GGPC[i,27]*GGPC[i,20]+GGPC[i,28]*GGPC[i,21]}
}
names(GGPC)

GGPC[,c("Real.estate" ,"Business.services" ,'Real.estate.renting.business.activities')]

GGPC <- GGPC[, !(colnames(GGPC) %in% c("Real.estate" ,"Business.services","weight70","weight71.74"))]

#ISIC 75-95 community.social.personal.services
# C75 - Public administration and defence; compulsory social security
# C80 - Education
# C85 - Health and social work
# C90T93 - Other community, social and personal services
# C95 - Private households with employed persons
# [21] "Education"                                      "Health"                                        
# [23] "Other.services"                                 "Households.with.employed.persons"              
# [25] "weight75"                                       "weight80"                                      
# [27] "weight85"                                       "weight90.93"                                   
# [29] "weight95" 

 
GGPC$Community.social.and.personal.services.  <-0
names(GGPC)
for (i in 1:29) {
GGPC[i,34]<-if ( sum(is.na(GGPC[i,25]))>0 | sum(is.na(GGPC[i,26])) >0 | sum(is.na(GGPC[i,27])) >0  | sum(is.na(GGPC[i,28])) >0 | sum(is.na(GGPC[i,29])) >0 ) {
    1/5*GGPC[i,20]+1/5*GGPC[i,21] + 1/5*GGPC[i,22] + 1/5*GGPC[i,23]+ 1/5*GGPC[i,24]
  }  else{ GGPC[i,25]*GGPC[i,20]+GGPC[i,26]*GGPC[i,21]+ GGPC[i,27]*GGPC[i,22] + GGPC[i,28]*GGPC[i,23]+ GGPC[i,29]*GGPC[i,24]
  }
}
names(GGPC)
 
GGPC[,c("Government","Education","Health","Other.services", "Households.with.employed.persons","Community.social.and.personal.services.")]

#if missing values in Community.social.and.personal.services. due to NA in house holds personal, than compute simple average of remaining four categories
for (i in 1:29) {
  GGPC[i,34]<-if(sum(is.na(GGPC[i,24])) > 0 ) { (GGPC[i,20]+GGPC[i,21] + GGPC[i,22] + GGPC[i,23])/4} else{GGPC[i,34]}
}

GGPC[,c("Government","Education","Health","Other.services", "Households.with.employed.persons","Community.social.and.personal.services.")]

GGPC <- GGPC[, !(colnames(GGPC) %in% c("Government","Education","Health","Other.services", "Households.with.employed.persons","weight75","weight80","weight85","weight90.93", "weight95"))]

names(GGPC)
#reshape to long #check here if R.D.STAN.final industries okay
GGPC_final<- melt(GGPC, id=c("Country"), measure.vars=c(  "Agriculture..forestry...fishing",          "Mining...quarying" ,                      
                                                          "Food..beverage...tobacca"          ,       "Wood.products",                            "Paper..printing...publishing",            
                                                          "Coke...refined.petroleum"            ,     "Chemical.products"     ,                   "Rubber...plastics",                       
                                                          "Non.metallic.mineral.products"      ,      "Basic...fabricated.metal"  ,               "Machinery",                               
                                                          "Electrical...optical.equipment"       ,    "Transport.Equipment"     ,                 "Other.manufacturing",                     
                                                          "Utiliteis"                         ,       "Construction"      ,                       "Financial.services" ,                     
                                                          "Textiles.textile.products.leather.and.footwear",     "Hotels...restaurants" ,       
                                                          "Wholesale.and.retail.trade.repairs"        ,   "Transport.storage.post.telecommunication" ,"Real.estate.renting.business.activities", 
                                                          "Community.social.and.personal.services." ), variable.name='Industry', value.name='Producer.price')


GGPC_final$Industry <-revalue(GGPC_final$Industry, c("Agriculture..forestry...fishing"="Agriculture.hunting.forestry.and.fishing",
                                                     "Mining...quarying"="Mining.and.quarrying",
                                                     "Food..beverage...tobacca"="Food.products.beverages.and.tobacco"     ,      "Wood.products"="Wood.and.products.of.wood.and.cork" ,                                
                                                     "Paper..printing...publishing"="Pulp.paper.paper.products.printing.and.publishing"  ,                 "Coke...refined.petroleum"="Coke.refined.petroleum.products.and.nuclear.fuel",                      
                                                     "Chemical.products"="Chemicals.and.chemical.products",                              "Rubber...plastics"="Rubber.and.plastics.products",                             
                                                     "Non.metallic.mineral.products"="Other.non-metallic.mineral.products"   ,               "Basic...fabricated.metal"="Basic.metals.and.fabricated.metal.products"  ,                    
                                                     "Machinery"="Machinery.and.equipment.nec."                       ,               "Electrical...optical.equipment" ="Electrical.and.optical.equipment",               
                                                     "Real.estate.renting.business.activities"="Real.estate.renting.and.business.activities",      "Other.manufacturing"="Manufacturing.nec.recycling.",                           
                                                     "Utiliteis"="Electricity.gas.and.water.supply",  "Financial.services"="Financial.intermediation"                 ,             
                                                     "Hotels...restaurants" ="Hotels.and.restaurants","Transport.Equipment"="Transport.equipment"))


R.D.STAN.final$Industry <- revalue(R.D.STAN.final$Industry, c(
  "AGRICULTURE.HUNTING.FORESTRY.AND.FISHING"="Agriculture.hunting.forestry.and.fishing", "Chemicals.and.chemical.products"="Chemicals.and.chemical.products",                     
  "Coke.refined.petroleum.products.and.nuclear.fuel"="Coke.refined.petroleum.products.and.nuclear.fuel",    "Community.social.and.personal.services"= "Community.social.and.personal.services.",             
  "CONSTRUCTION"="Construction", "ELECTRICITY.GAS.AND.WATER.SUPPLY"="Electricity.gas.and.water.supply",                   
  "Financial.intermediation"="Financial.intermediation" ,                            "Food.products.beverages.and.tobacco"="Food.products.beverages.and.tobacco",                
  "Hotels.and.restaurants"="Hotels.and.restaurants",                               "Machinery.and.equipment.nec"= "Machinery.and.equipment.nec.",                        
  "Manufacturing.n.e.c..and.recycling"="Manufacturing.nec.recycling." ,                  "MINING.AND.QUARRYING"="Mining.and.quarrying",                                
  "Other.non-metallic.mineral.products"="Other.non-metallic.mineral.products",                  "Pulp.paper.paper.products.printing.and.publishing"="Pulp.paper.paper.products.printing.and.publishing",
  "Real.estate.renting.and.business.activities"="Real.estate.renting.and.business.activities",         "Rubber.and.plastics.products"="Rubber.and.plastics.products",                        
  "Transport.storage.and.communications"="Transport.storage.post.telecommunication",               
  "Wholesale.and.retail.trade.repairs"="Wholesale.and.retail.trade.repairs", "Wood.and.products.of.wood.and.cork"="Wood.and.products.of.wood.and.cork",                  
  "Basic.metals.fabricated.metal.products"="Basic.metals.and.fabricated.metal.products", "Electrical.optical.equipment"  ="Electrical.and.optical.equipment"))                                      

#merging
#reduce sample to matching countries


R.D.STAN.final<-subset(R.D.STAN.final,R.D.STAN.final$Country %in% tiva.anbred.GGPC)

R.D.STAN.final$COU<-NULL
GGPC_final<-subset(GGPC_final,GGPC_final$Country %in% tiva.anbred.GGPC)

# va.exgr.1995.2000.2005<- subset(va.exgr.1995.2000.2005, va.exgr.1995.2000.2005$Country %in% tiva.anbred.GGPC)
# va.exgr.1995.2000.2005<- subset(va.exgr.1995.2000.2005, va.exgr.1995.2000.2005$Partner %in% tiva.anbred.GGPC)
tiva2.EXGR.VA.FD.VA<-subset(tiva2.EXGR.VA.FD.VA, tiva2.EXGR.VA.FD.VA$Country %in% tiva.anbred.GGPC)
tiva2.EXGR.VA.FD.VA<-subset(tiva2.EXGR.VA.FD.VA, tiva2.EXGR.VA.FD.VA$Partner %in% tiva.anbred.GGPC)
tiva2.EXGR.VA.FD.VA$COU<-NULL
tiva2.EXGR.VA.FD.VA$PAR<-NULL
tiva2.EXGR.VA.FD.VA$IND<-NULL
# va.exgr.1995.2000.2005$COU<-NULL
# va.exgr.1995.2000.2005$PAR<-NULL
#check number of industries
intersect(R.D.STAN.final$Industry,GGPC_final$Industry)
intersect(R.D.STAN.final$Industry,tiva2.EXGR.VA.FD.VA$Industry)

#merge
merge1 <- merge(R.D.STAN.final,GGPC_final, by=c("Country" ,"Industry"))

unique(merge2$Country)
# merge2 <-join(merge1, va.exgr.1995.2000.2005, by=c("Country" ,"Industry"),type="right")
merge2 <-join(merge1,tiva2.EXGR.VA.FD.VA, by=c("Country" ,"Industry"),type="right")
merge2$Country<-as.character(merge2$Country)
merge2$Industry<-as.character(merge2$Industry)
merge2$Partner<-as.character(merge2$Partner)

merge2$productivity <- 1/merge2$Producer.price

library(foreign)

setwd("/Users/sergej/Documents/Master/Thesis/June_Empirical_Analysis/Send_Liza/Working_Data")
# write.dta(merge2, "thesis_data_va_exgr_rd_price_06_july_2015.dta")
write.dta(merge2, "thesis_data2_FD_VA_va_exgr_rd_price_19_july_2015.dta")
#write.dta(merge1, "thesis_data2_va_exgr_19_july_2015.dta")
# save(merge2, file="thesis_data_va_exgr_rd_price_06_july_2015.Rdata")
save(merge2, file="thesis_data2_FD_VA_va_exgr_rd_price_19_july_2015.Rdata")
