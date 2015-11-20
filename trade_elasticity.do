version 12
set more off /*full output is displayed
w/o having to hit the space bar in between*/
capture log close //closes any previously opened log-files
clear all
global dir "/Users/sergej/Documents/Master/Thesis/June_Empirical_Analysis/Send_liza/"
cd "$dir/Working_Data"
log using "thesis_regression_02_11_2015", text replace
*use "tiva_stan_ggpc.dta"
set matsize 11000

use "thesis_data2_FD_VA_va_exgr_rd_price_19_july_2015.dta",clear

rename Producer_price Producer_price2005
rename R_D_expenditure R_D_exp2005
rename productivity productivity2005
rename Industry industry
rename Country country
rename Partner partner
rename country exporter
rename partner importer
drop FD_VA*
gen COU="."
replace COU="AUS" if exporter== "Australia"                                                                          
replace COU="AUT" if exporter=="Austria"    
replace COU="BEL" if exporter== "Belgium"  
replace COU="CAN" if exporter=="Canada" 
replace COU="CZE" if exporter=="Czech Republic" 
replace COU="EST" if exporter=="Estonia"     
replace COU="FIN" if exporter=="Finland"
replace COU="FRA" if exporter=="France"
replace COU="DEU" if exporter=="Germany"
replace COU="GRC" if exporter=="Greece"
replace COU="HUN" if exporter=="Hungary"
replace COU="IRL" if exporter=="Ireland"   
replace COU="ITA" if exporter=="Italy"  
replace COU="JPN" if exporter=="Japan" 
replace COU="KOR" if exporter=="Korea"   
replace COU="LUX" if exporter=="Luxembourg"     
replace COU="MEX" if exporter=="Mexico"  
replace COU="NLD" if exporter=="Netherlands" 
replace COU="POL" if exporter=="Poland"  
replace COU="PRT" if exporter=="Portugal" 
replace COU="ROU" if exporter=="Romania" 
replace COU="RUS" if exporter=="Russian Federation"
replace COU="SVK" if exporter=="Slovak Republic"
replace COU="SVN" if exporter=="Slovenia" 
replace COU="ZAF" if exporter=="South Africa" 
replace COU="ESP" if exporter=="Spain" 
replace COU="TUR" if exporter=="Turkey" 
replace COU="GBR" if exporter=="United Kingdom"
replace COU="USA" if exporter=="United States"


gen PAR="."

replace PAR="AUS" if importer== "Australia"                                                                          
replace PAR="AUT" if importer=="Austria"    
replace PAR="BEL" if importer== "Belgium"  
replace PAR="CAN" if importer=="Canada" 
replace PAR="CZE" if importer=="Czech Republic" 
replace PAR="EST" if importer=="Estonia"     
replace PAR="FIN" if importer=="Finland"
replace PAR="FRA" if importer=="France"
replace PAR="DEU" if importer=="Germany"
replace PAR="GRC" if importer=="Greece"
replace PAR="HUN" if importer=="Hungary"
replace PAR="IRL" if importer=="Ireland"   
replace PAR="ITA" if importer=="Italy"  
replace PAR="JPN" if importer=="Japan" 
replace PAR="KOR" if importer=="Korea"   
replace PAR="LUX" if importer=="Luxembourg"     
replace PAR="MEX" if importer=="Mexico"  
replace PAR="NLD" if importer=="Netherlands" 
replace PAR="POL" if importer=="Poland"  
replace PAR="PRT" if importer=="Portugal" 
replace PAR="ROU" if importer=="Romania" 
replace PAR="RUS" if importer=="Russian Federation"
replace PAR="SVK" if importer=="Slovak Republic"
replace PAR="SVN" if importer=="Slovenia" 
replace PAR="ZAF" if importer=="South Africa" 
replace PAR="ESP" if importer=="Spain" 
replace PAR="TUR" if importer=="Turkey" 
replace PAR="GBR" if importer=="United Kingdom"
replace PAR="USA" if importer=="United States"
gen IND="."
replace IND="01-05" if industry=="Agriculture.hunting.forestry.and.fishing"
replace IND="10-14" if industry=="Mining.and.quarrying"
replace IND="15-16" if industry=="Food.products.beverages.and.tobacco"
replace IND="17-19" if industry=="Textiles.textile.products.leather.and.footwear"
replace IND="20" if industry=="Wood.and.products.of.wood.and.cork"
replace IND="21-22" if industry=="Pulp.paper.paper.products.printing.and.publishing"
replace IND="23" if industry=="Coke.refined.petroleum.products.and.nuclear.fuel"
replace IND="24" if industry=="Chemicals.and.chemical.products"
replace IND="25" if industry=="Rubber.and.plastics.products"
replace IND="26" if industry=="Other.non-metallic.mineral.products"
replace IND="27-28" if industry=="Basic.metals.and.fabricated.metal.products"
replace IND="29"	 if industry=="Machinery.and.equipment.nec."  
replace IND="30-33" if industry=="Electrical.and.optical.equipment"
replace IND="34-35" if industry=="Transport.equipment"
replace IND="36-37" if industry=="Manufacturing.nec.recycling."
replace IND="40-41" if industry=="Electricity.gas.and.water.supply"
replace IND="45"    if industry=="Construction" 
replace IND="50-52" if industry=="Wholesale.and.retail.trade.repairs" 
replace IND="55"    if industry=="Hotels.and.restaurants" 
replace IND="60-64" if industry=="Transport.storage.post.telecommunication" 
replace IND="65-67" if industry=="Financial.intermediation" 
replace IND="70-74" if industry=="Real.estate.renting.and.business.activities" 
replace IND="75-95" if industry=="Community.social.and.personal.services." 
drop if IND=="40-41"

gen EU=0
replace EU=1 if  importer=="Austria" 
 replace EU=1 if  importer=="Belgium" 
 replace EU=1 if  importer=="Czech Republic" 
 replace EU=1 if  importer=="Estonia" 
 replace EU=1 if  importer== "Finland" 
replace EU=1 if   importer=="France" 
replace EU=1 if   importer=="Germany"  
replace EU=1 if   importer=="Greece" 
replace EU=1 if   importer=="Ireland" 
replace EU=1 if   importer=="Italy" 
replace EU=1 if importer=="Spain" 
replace EU=1 if   importer=="Hungary" 
replace EU=1 if   importer=="Slovak Republic"  
replace EU=1 if   importer=="Slovenia"  
replace EU=1 if   importer=="Portugal"  
replace EU=1 if   importer=="Netherlands"  
replace EU=1 if   importer=="Luxembourg"  
replace EU=1 if   importer=="Poland" 
replace EU=1 if   importer=="United Kingdom"
  
 replace EU=1  if   exporter=="Austria" 
   replace EU=1  if exporter=="Belgium" 
  replace EU=1  if  exporter=="Czech Republic" 
  replace EU=1  if  exporter=="Estonia" 
  replace EU=1  if  exporter== "Finland" 
 replace EU=1  if   exporter=="France" 
 replace EU=1  if   exporter=="Germany"  
 replace EU=1  if   exporter=="Greece" 
 replace EU=1  if   exporter=="Ireland" 
 replace EU=1  if   exporter=="Italy" 
 replace EU=1  if   exporter=="Spain" 
 replace EU=1  if   exporter=="Poland"
 replace EU=1  if   exporter=="Hungary" 
 replace EU=1  if   exporter=="Slovak Republic"  
  replace EU=1  if  exporter=="Slovenia"  
   replace EU=1  if exporter=="Portugal"  
  replace EU=1  if  exporter=="Netherlands"  
  replace EU=1  if  exporter=="Luxembourg"   
  replace EU=1  if  exporter=="United Kingdom"
  
gen D_1995=0 
replace D_1995=1 if VA_1995!=0

gen D_2000=0 
replace D_2000=1 if VA_2000!=0

gen D_2005=0 
replace D_2005=1 if VA_2005!=0

gen D_EXGR_1995=0 
replace D_EXGR_1995=1 if EXGR_1995!=0

gen D_EXGR_2000=0
replace D_EXGR_2000=1 if EXGR_2000!=0

gen D_EXGR_2005=0 
replace D_EXGR_2005=1 if EXGR_2005!=0

bysort D_2000 D_1995: tab D_2005 if D_2000==1 & D_1995==1
bysort IND: tab D_2005 if D_2000==1 & D_1995==1 

gen D_im_2000=0
gen D_im=0
gen D_im_exgr=0
 replace D_im_2000=1 if D_2005==0 & D_2000==1  
 replace D_im=1 if D_2005==0 & (D_2000==1 | D_1995==1)
 replace D_im_exgr=1 if D_EXGR_2005==0 & (D_EXGR_2000==1 | D_EXGR_1995==1)
 
replace VA_2005=. if D_im==1
replace EXGR_2005=. if D_im_exgr==1
 replace VA_1995=. if D_1995==0 &VA_2000!=0 & VA_2005!=0
replace EXGR_1995=. if  D_EXGR_1995==0 & D_EXGR_2000!=0 & EXGR_2005!=0

ipolate VA_2005 VA_2000, gen(VA_2005_2) epolate
ipolate VA_1995 VA_2000, gen(VA_1995_2) epolate
replace VA_2005 =VA_2005_2 if VA_2005==.
replace VA_1995  =VA_1995_2 if VA_1995==.
ipolate EXGR_2005 EXGR_2000, gen(EXGR_2005_2) epolate
replace EXGR_2005 =EXGR_2005_2 if EXGR_2005==.
ipolate EXGR_1995 EXGR_2000, gen(EXGR_1995_2) epolate
replace EXGR_1995 =EXGR_1995_2 if EXGR_1995==.
 
 rename EXGR_2005 EXGR2005 
rename EXGR_2000 EXGR2000 
rename EXGR_1995 EXGR1995 
rename VA_2005 VA2005 
rename VA_2000 VA2000
rename VA_1995 VA1995 

drop VA_2005_2
drop VA_1995_2
drop EXGR_2005_2
drop EXGR_1995_2

reshape long Producer_price R_D_exp EXGR  VA productivity, i(exporter industry importer) j(year)


drop if year!=2005
eststo clear
*eststo: estpost tab IND  
*esttab est* using industry.tex, cells("b ") replace booktabs longtable compress  noobs 
*eststo: estpost tab COU 
*esttab est2 using cou.tex, cells("b ") replace booktabs longtable compress  label  noobs  

egen pair=group(exporter importer)
*construct dest-sector dummies
egen dest_sector=group(industry importer)
gen l_gross_export=ln(EXGR) 
gen l_export_va=log(VA)
gen l_productivity=ln(productivity)
gen logrdexpend=ln(R_D_exp)



gen manufac_services=1
replace  manufac_services=0 if IND=="01-05" | IND=="10-14"
gen  no_aggr=1
replace no_aggr=0 if  IND=="17-19" | IND=="50-52" |  IND=="60-64"  |IND=="70-74"|IND=="75-95"
 gen mining=1
replace  mining=0 if IND=="10-14"

gen manufac=1
replace manufac=0 if IND=="01-05" | IND=="10-14"  | IND=="45" | IND=="50-52" | IND=="55" | IND=="60-64" | IND=="65-67"|  IND=="70-74" |  IND=="75-95"
eststo clear 
set more off
	sort exporter industry importer 
		*exporter dummies
	quietly:tabulate exporter, generate(idum) 
*importer dummies
	quietly:tabulate importer, generate(jdum) 

	foreach var1 of varlist idum* {
		foreach var2 of varlist jdum* {
 generate alpha`var1'`var2'=`var1'*`var2'
			}
		}

	quietly: tabulate industry, generate(kdum)
	foreach var1 of varlist idum* {
		foreach var2 of varlist kdum* {
		 generate gamma`var1'`var2'=`var1'*`var2'
			}
		}
  	
  	
	foreach var1 of varlist kdum* {
		foreach var2 of varlist jdum* {
		generate beta`var1'`var2'=`var1'*`var2'
		}
	}

	/*  Collinearity
areg  l_productivity logrdexpend I.dest_sector , absorb(pair) 
vif , uncentered
predict productivity_hat_2, xb
areg l_gross_export productivity_hat_2 I.dest_sector, absorb(pair)  vce(robust)
vif , uncentered
both stages collinearity problem first stage more severe than second stage
*/


#OLS IV with two dimensions of fixed effects
eststo clear
eststo: reghdfe  l_gross_export l_productivity , vce(robust) absorb(pair dest_sector)
    eststo: reghdfe l_gross_export (l_productivity=logrdexpend)   , absorb(pair dest_sector) vce(robust)  ffirst 
  eststo: reghdfe l_gross_export  (l_productivity=logrdexpend )    if  manufac_services==1, absorb(pair dest_sector) vce(robust) ffirst 
   eststo: reghdfe l_gross_export  (l_productivity=logrdexpend )    if  mining==1, vce(robust) ffirst absorb(pair dest_sector)
    eststo: reghdfe l_gross_export (l_productivity=logrdexpend )    if  EU==1, vce(robust) ffirst absorb(pair dest_sector)
*esttab est1  est2 est3 est4 est5  using ricardo_gr_exp_second.tex , replace scalar(F r2u widstat) r2 se keep ( l_productivity ) b(2) 	 booktabs  mtitles("OLS" "Full Sample"  "Manufacturing Service" "Without mining" "EU")
esttab est1  est2 est3 est4 est5 using ricardo_gr_exp_second.html , replace scalar(F r2u widstat) r2 se keep ( l_productivity ) b(2) 	   mtitles("OLS"  "Full Sample"  "Manufacturing Service" "Without mining" "EU")
eststo:reghdfe l_export_va  l_productivity   , vce(robust) absorb(pair dest_sector)
 eststo: reghdfe l_export_va  (l_productivity=logrdexpend  )   , vce(robust) ffirst absorb(pair dest_sector)
  eststo: reghdfe l_export_va  (l_productivity=logrdexpend )    if  manufac_services==1, vce(robust) ffirst absorb(pair dest_sector)
   eststo: reghdfe l_export_va  (l_productivity=logrdexpend )    if  mining==1, vce(robust) ffirst absorb(pair dest_sector)
    eststo: reghdfe l_export_va (l_productivity=logrdexpend )    if  EU==1, vce(robust) ffirst absorb(pair dest_sector)
    
*esttab est6  est7 est8 est9 est10  using ricardo_va_exp_new.tex, replace scalar(F r2u widstat) r2 se keep ( l_productivity ) b(2) 	 booktabs    mtitles("OLS"  "Full Sample"  "Manufacturing Service" "Without mining" "EU")
esttab est6  est7 est8 est9 est10    using  ricardo_va_exp_new.html, replace scalar(F r2u widstat) r2 se keep ( l_productivity ) b(2)     mtitles("OLS"  "Full Sample"  "Manufacturing Service" "Without mining" "EU")
save thesis_data2_FD_VA_va_exgr_rd_price_03_sep_2015.dta


log close
