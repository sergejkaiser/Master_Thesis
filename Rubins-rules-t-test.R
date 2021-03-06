#create data prep functions for esttab output of complet imputed data sets, than function to aggregate results for beta, se, and R2 according to Rubins Rules. 
#example of df created from reading in a csv created with esttab from m complte data set analysis with Stata
#several problems, = infront of each data row, the se are in brackets, 
#     rownames    Imputation    m=0                 m=1           m=2                 m=3               m=4
# Example df (only first five columns, in total 41 columns) : 
#               X.     X.l_export_va   X.l_export_va.1   X.l_export_va.2   X.l_export_va.3   X.l_export_va.4
#1 =l_productivity  =16.165578444015  =12.411368078645  =12.195573856652  =12.538878934601  =11.690369366100
#2               = =(2.196214952028) =(1.306724968504) =(1.285309339346) =(1.444170448829) =(1.273732644875)
#3              =N            =13588            =16538            =16538            =16538            =16538
#4           =R-sq           =-0.146            =0.252            =0.273            =0.242            =0.313
#5              =F  =54.179227793776  =90.213375258364  =90.030394842161  =75.384231239494  =84.236421154529
#6        =widstat  =59.614382257757 =112.163067072036 =113.787759175129  =93.690074069563 =108.607499284190

data.prep <- function(df,n) {
  #create
      df[,1]<-as.character(df[,1])      
      df[2,1]<-"SE"
      rownames(df)<-df[[1]]
      rownames(df)<-gsub("=","", rownames(df))
      df<-df[,-1]
for (i in 1:ncol(df) ) {
 df[,i] = gsub("=","", df[,i])
}
      #extract brackets
      for (i in 1:ncol(df) ) {
      df[n,i]  = gsub("\\(|\\)","",  df[n,i])
      }
# names(df)[n]<-"Concept"
 
  #factor labels contain the correct numbers
  
 for (i in 1:ncol(df)){
  df[[i]] = as.numeric(paste(df[[i]]))
 }
      return(df)   }
# Result of applying the data prep function 
#            X.l_export_va X.l_export_va.1 X.l_export_va.2 X.l_export_va.3 X.l_export_va.4 X.l_export_va.5
#l_productivity     16.165578       12.411368       12.195574        12.53888       11.690369       11.586706
#SE                  2.196215        1.306725        1.285309         1.44417        1.273733        1.246413
#N               13588.000000    16538.000000    16538.000000     16538.00000    16538.000000    16538.000000
#R-sq               -0.146000        0.252000        0.273000         0.24200        0.313000        0.322000
#F                  54.179228       90.213375       90.030395        75.38423       84.236421       86.416319
#widstat            59.614382      112.163067      113.787759        93.69007      108.607499      111.167882

results.rubin <- function(df) {
#first column of df contains the regression results without imputation 
  m<-ncol(df)-1
  n<-ncol(df)
  df.result<-NULL
  df.result$beta<-1/(m)*rowSums(df[1,2:n])
  #between variance mean of squared deviation of beta from grand mean imputed results starting from column 2
  df.result$Between<-(1/(m-1)*sum((df[1,2:n]-df.result$beta)^2)) 
  #standard error is the sqrt of the estimated variance
  #within variance mean of squared se in row 2 for the imputed results starting from column 2
  df.result$Within<-(1/m)*sum(df[2,2:n]^2)
  df.result$total.var<-df.result$Within+(1+1/m)*df.result$Between
  df.result$SE<-sqrt(df.result$total.var)
  #R -squared is obtained in two steps first the square root of r2 is trasnformed by fisher transformation in to z scores
  # in the next step the mean of the scores (Rubins Rules) is transfromed back and squared. Ref.: Ofer Harel (2009) The estimation of R 2 and adjusted R 2 in incomplete data sets using multiple imputation, Journal of Applied Statistics, 36:10, 1109-1118.
  df.result$r.sq<-sum(atanh(sqrt(df[4,2:n])))
  df.result$r.sq<-tanh(df.result$r.sq/m)^2
  return(df.result)
}
#applying result.rubin 
#$beta
#l_productivity 
 #     11.76206 
#$Between
#[1] 0.4553582
#$Within
#[1] 1.628466
#$total.var
#[1] 2.095208
#$SE
#[1] 1.447483
#$r.sq
#[1] 0.3041374

library(dplyr)
#this function creates clean df with the results of four different mi regression analysis results 
 report.final<-function(df){ 
    df<- dplyr::select(df,beta,beta.1, beta.2,beta.3,SE,SE.1,SE.2,SE.3,r.sq,r.sq.1,r.sq.2,r.sq.3) 
    df<-rename(df,beta.0=beta)
    df<-reshape(df, direction='long', 
            varying=1:12, 
            timevar='var',
            times=c('beta', 'SE',"r.sq"),
            new.row.names=c("Log Productivity","","R-squared"),
            v.names=c("OLS","Full","Without primary industries high","Without primary industries"))
    df<- df[,c("OLS","Full","Without primary industries","Without primary industries high")]
    #numeric rows are of factor type and factor labels contain the numerical values of interest 
    #paste function to retrieve the factor labels (numerical values) 
    for (i in 1:ncol(df)){
      df[[i]] = as.numeric(paste(df[[i]]))
    } 
    #change order of data frames 
       df<-df[,c(2,1,3,4)]  
       names(df)<-c("OLS","Full","Without primary industries high","Without primary industries")
       return(df)
 }
#applying report.final to the combined data frame with the results of a df , 

#                  OLS      Full Without primary industries high Without primary industries
#Log Productivity 0.47584681 12.910914                      11.7620633                  15.080497
#                 0.06614105  1.339625                       1.4474834                   2.180471
#R-squared        0.77500000  0.180218                       0.3041374                   0.128230
 #equal fmi test for H_O: beta.1=beta.2 
 #for modest m and large n intereference is absed on t-distribution
 #Rubin (1987) Multiple Imputation for Nonresponse in Surveys. p.77 
 
 #this function takes a df of the format displayed after applying the data prep function
 #it calculates the pooled beta and the between, within, and total variance. 
 #After this step it either takes another df and caluclates the second df's beta and 
 #tests wether the first beta is equal to the second beta. Equivalently the function may also take instead of a second df just a numerical value of some beta 
 #we want to test to be equal to our first beta
 # the degrees of freedom caluclation is based on Rubin (1987) Multiple Imputation for Nonresponse in Surveys p.77 
 t.test<-function(df,dv=NULL,q) {
   #first column contains not imputed results hence we drop them
   #hypothesis test HO: Q_M=q under assumed equal FMI (see Rubin (1987))
   drop.a<-names(df)
   drop.a<-drop.a[1]
   
   df <- df[,!(names(df)) %in% c(drop.a) ];
   
   #n m denote the number of imputations
   
   m <- ncol(df) ;
   
   df.a <- NULL
   
   df.b <-NULL
   #beta is the mean of the beta estimates 
   df.a$Q.m <- (1/m) * sum( df[1 , 1:m])
   #between variance 
   df.a$B <- (1/(m-1)) *sum( (df[1,1:m]-df.a$Q.m)^2 ) 
   #within variance mean of squared se in row 2 for the imputed results 
   df.a$W <- (1/m)*sum(df[2,1:m]^2) ;
   #total variance
   df.a$t<-df.a$W+(1+1/m)*df.a$B ;
   #if dv is a data frame with m imputed datasets than beta is calculated according to Rubins Rules (1987)
   if (is.null(dv)==FALSE) { 
     drop.b<-names(dv)
     drop.b<-drop.b[1]
     dv <- dv[,!(names(dv)) %in% c(drop.b) ];
     n <- ncol(dv) ;
     df.b$Q <- (1/n) * sum( dv[1 , 1:n])
     } 
   else { 
     df.b$Q<-q
     df.b<-as.data.frame(df.b)
   }
 #rubin (1986)
   ttest<-(df.b$Q-df.a$Q.m)/sqrt(df.a$t)
   v.m<-(m-1)*(1+ ((1+m^-1)*df.a$B/df.a$W)^-1)^2
   if ( (df.a$Q.m < df.b$Q)==TRUE) { 
     pval<-1- pt(q=ttest,df=v.m)
     }
else {  
  pval<-pt(q=ttest,df=v.m)
}
#some code for a simplified f-test for a scalar 
   #relative increase of variance due to missingness
      #r<-(1+(m^-1))*df.a$B*(df.a$W^-1) ;
      #F-stat Li, Raghunathan, and Rubin 1991
      #fstat<-((df.b$Q-df.a$Q.m)^2*df.a$W^(-1))/(1+r) ;
   #The F stat is 1,w distributed
   #tau<- (m-1)
   #df w best according to Li, Raghunathan, and Rubin 1991
   #v<-4+(tau-4)*(1+ (1-2*(tau^-1)/r))^2
#p.value <-  pf(fstat,df1=1, df2=v)
t.stat.p.val<-list(df.a$Q.m, df.b$Q, test,pval,v.m)  #fstat,p.value,v)
t.stat.p.val<-setNames(f.stat.p.val, c("mean Q_m","Q","T-statistic", "P-value", "Degrees of Freedom (appox. Rubin (1987))"))
return(t.stat.p.val)
 }        
 df.b$Q<-NULL
   df.a$Q.m<-NULL
 F.stat<-NULL
 #relative increase of variance due to missingness
 r<-NULL
 #The F stat is 1,w distributed
 tau<-NULL
 #df w best according to Li, Raghunathan, and Rubin 1991
 v<-NULL
 #exgr
 exgr.txt<-readLines("gross-exports.csv")
 skip.exgr.txt = exgr.txt[-c(1:2,4,7,10:11)]
 df.exgr = read.csv(textConnection(skip.exgr.txt), header = TRUE, stringsAsFactors = FALSE)
 df.exgr<-data.prep(df.exgr,2)
 exgr.ols<-results.rubin(df.exgr)
 
 
 all_content.4 = readLines("gross-exports-iv.csv")
 skip.4 = all_content.4[-c(1:2,4,7,12:13)]
 df.5 = read.csv(textConnection(skip.4), header = TRUE, stringsAsFactors = FALSE)
 exgr.prep<-data.prep(df.5,2)
 exgr<-results.rubin(exgr.prep)
 
 all_content.5 = readLines("gross-exports-manuf-iv.csv")
 skip.5 = all_content.5[-c(1:2,4,7,12:13)]
 df.6 = read.csv(textConnection(skip.5), header = TRUE, stringsAsFactors = FALSE)
 exgr.manuf.prep<-data.prep(df.6,2)
 exgr.manuf<-results.rubin(exgr.manuf.prep)
 
 all_exgr = readLines("gross-exports-manuf-high-iv.csv")
 #eliminate all empty lines
 skip_exgr = all_exgr[-c(1:2,4,7,12:13)]
 #create an R object which can be worked with in the further steps
 skip_exgr = read.csv(textConnection(skip_exgr), header = TRUE, stringsAsFactors = FALSE)
 exgr.manuf.high.prep<-data.prep(skip_exgr) 
 exgr.manuf.high<-results.rubin(exgr.manuf.high.prep)
 
 results.exgr<-as.data.frame(c(exgr.ols,exgr,exgr.manuf,exgr.manuf.high))
 
 #fddva
 fddva.txt<-readLines("gross-export-fddva.csv")
 skip.fddva.txt = fddva.txt[-c(1:2,4,7,10:11)]
 df.fddva = read.csv(textConnection(skip.fddva.txt), header = TRUE, stringsAsFactors = FALSE)
 df.fddva<-data.prep(df.fddva,2)
 fddva.ols<-results.rubin(df.fddva)
 
 all_content.3 = readLines("gross-export-fddva-iv.csv")
 skip.3 = all_content.3[-c(1:2,4,7,12:13)]
 df.4 = read.csv(textConnection(skip.3), header = TRUE, stringsAsFactors = FALSE)
 fddva.prep<-data.prep(df.4,2)
 fddva<-results.rubin(fddva.prep)
 
 all_content.7 = readLines("gross-export-manuf-fddva-iv.csv")
 skip.7 = all_content.7[-c(1:2,4,7,12:13)]
 df.8 = read.csv(textConnection(skip.7), header = TRUE, stringsAsFactors = FALSE)
 fddva.manuf.prep<-data.prep(df.8,2)
 fddva.manuf<-results.rubin(fddva.manuf.prep)
 
 all_content = readLines("gross-export-manuf-high-fddva-iv.csv")
 skip = all_content[-c(1:2,4,7,12:13)]
 df.3 = read.csv(textConnection(skip), header = TRUE, stringsAsFactors = FALSE)
 fddva.manuf.high.prep<-data.prep(df.3,2)
 fddva.manuf.high<-results.rubin(fddva.manuf.high.prep)
 results.fddva<-as.data.frame(c(fddva.ols,fddva,fddva.manuf,fddva.manuf.high))
 
 
 #dva
 dva.txt<-readLines("gross-export-va.csv")
 skip.dva.txt = dva.txt[-c(1:2,4,7,10:11)]
 df.ba = read.csv(textConnection(skip.dva.txt), header = TRUE, stringsAsFactors = FALSE)
 df.ba<-data.prep(df.ba,2)
 dva.ols<-results.rubin(df.ba)
 
 all_content.2 = readLines("gross-export-va-iv.csv")
 skip.2 = all_content.2[-c(1:2,4,7,12:13)]
 df.1 = read.csv(textConnection(skip.2), header = TRUE, stringsAsFactors = FALSE)
 df.exgr.dva.prep<-data.prep(df.1,2)
 exgr.dva<-results.rubin(df.exgr.dva.prep)
 
 all_content.6 = readLines("gross-export-manuf-va-iv.csv")
 skip.6 = all_content.6[-c(1:2,4,7,12:13)]
 df.7 = read.csv(textConnection(skip.6), header = TRUE, stringsAsFactors = FALSE)
 df.manuf.prep<-data.prep(df.7,2)
 dva.manuf<-results.rubin(df.manuf.prep)
 
 
 all = readLines("gross-export-manuf-high-va-iv.csv")
 #eliminate all empty lines
 skip_dva = all[-c(1:2,4,7,12:13)]
 dva.skip =  read.csv(textConnection(skip_dva), header = TRUE, stringsAsFactors = FALSE)
 df.manuf.high.prep<-data.prep(dva.skip) 
 dva.manuf.high<-results.rubin(df.manuf.high.prep)
 
 results.dva<-as.data.frame(c(dva.ols,exgr.dva,dva.manuf,dva.manuf.high))

  t.test(df=exgr.manuf.prep, q= 9.286 )
 
 results.dva.final<-report.final(results.dva)
  results.fddva.final<-report.final(results.fddva)
  results.exgr.final<-report.final(results.exgr)
  #print results as latex table
  xtable(results.dva.final,
         digits=3)
  xtable(results.fddva.final,
         digits=3)
  xtable(results.exgr.final,
         digits=3)
