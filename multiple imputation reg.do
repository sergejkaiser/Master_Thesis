version 12
set more off /*full output is displayed
w/o having to hit the space bar in between*/
capture log close //closes any previously opened log-files
clear all
set maxvar  32767
set matsize 11000
log using "thesis_regression_01_02_2015", text

global dir "/Users/sergej/Google Drive/Send_liza"
cd "$dir/Working_Data"

use regression_data_mi_01_02_2016.dta, clear
*without imputation first stage and second stage of IV regression of EXGR,DVA, FD_DVA on productivity
 areg    l_productivity  l_R_D  I.pair    , vce(robust) absorb(dest_sector)
predict prod_hat
 areg l_gross_exports prod_hat  I.pair  , vce(robust) absorb(dest_sector )

 areg    l_productivity  l_R_D  I.pair  if manufac_services==1  , vce(robust) absorb(dest_sector)
predict prod_hat_manufac   
 areg l_gross_exports prod_hat_manufac I.pair if manufac_services==1 , vce(robust) absorb(dest_sector )
 
  areg    l_productivity  l_R_D  I.pair  if manufac_services==1 & inc_group=="High income: OECD" | inc_group=="High income: nonOECD"  , vce(robust) absorb(dest_sector)
predict prod_hat_manufac_high   
 areg l_gross_exports prod_hat_manufac I.pair if manufac_services==1 & inc_group=="High income: OECD" | inc_group=="High income: nonOECD", vce(robust) absorb(dest_sector )

 areg l_fd_dva prod_hat  I.pair  , vce(robust) absorb(dest_sector )
 areg l_fd_dva prod_hat_manufac I.pair if manufac_services==1 , vce(robust) absorb(dest_sector )
 areg l_fd_dva prod_hat_manufac I.pair if manufac_services==1 & inc_group=="High income: OECD" | inc_group=="High income: nonOECD", vce(robust) absorb(dest_sector )

misstable sum l_R_D l_prod, gen(miss_)
mi set mlong
mi register imputed l_R_D l_prod
set seed 2214 
/*
*m=40 chained imputations using predictive mean matching 
mi impute chained (pmm, knn(10)) l_R_D l_prod= idum* kdum*, add(40)
*check the distribution of imputed and not imputed data
mi xeq 1 :   twoway    kdensity l_R_D,   by(miss_l_R_D)
mi xeq 1 2 3 : twoway    kdensity l_prod, by(miss_l_prod)

 mi estimate, saving(miFS4, replace): areg l_productivity l_R_D   I.pair if manufac==1, vce(robust) absorb( dest_sector) 
mi test l_R_D
     
  mi estimate, saving(miFS2, replace)  imputations(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 21 22 23 24 25 26 27 28 29  30 31 32 33 34 35 36 37 38 39 40) mcerror: areg l_productivity l_R_D   I.pair, vce(robust) absorb( dest_sector)
mi test l_R_D
mi estimate , saving(miFS3,replace) mcerror:  areg   l_productivity  l_R_D  I.pair  if manufac_services==1  , vce(robust) absorb(dest_sector)  
mi test l_R_D

mi estimate ,  saving(miFS5, replace): areg l_productivity l_R_D  I.pair if manufac_services==1 & inc_group=="High income: OECD" | inc_group=="High income: nonOECD" , vce(robust) absorb(dest_sector ) 
mi test l_R_D


mi estimate ,  saving(miFS6,replace) dots:  areg   l_productivity  l_R_D  I.pair  if COU!="LUX", absorb(dest_sector) vce(robust)
mi test l_R_D

use regression_data_mi_prod_r_d_01_02_2016, clear


save regression_data_mi_prod_r_d_01_04_2016.dta

eststo clear
  capture log close
log using "thesis_regression_01_03_2016-exgr", text

use regression_data_mi_prod_r_d_01_02_2016, clear
set seed 2214 
log using "thesis_regression_01_03_2016-exgr", text replace

 *however this is very convenient speeding up the estimation quite a lot
*OLS
 eststo clear
qui mi xeq: eststo:  reghdfe  l_gross_exports l_productivity     , vce(robust) absorb(dest_sector pair) 
 esttab  using gross-exports.csv,  scalar(N)  r2 se keep( l_productivity) b(12) nostar replace
 eststo clear
qui mi xeq: eststo:  reghdfe  l_export_va l_productivity     , vce(robust) absorb(dest_sector pair)   
 esttab  using gross-export-va.csv,  scalar(N) r2 se keep( l_productivity) b(12) nostar replace
 eststo clear
qui mi xeq: eststo:  reghdfe  l_fd_dva l_productivity     , vce(robust) absorb(dest_sector pair)   
 esttab  using gross-export-fddva.csv, scalar(N) r2 se keep( l_productivity) b(12) nostar replace

*Full IV

eststo clear
 mi xeq : qui: eststo: reghdfe l_gross_exports (l_productivity=l_R_D)  , vce(robust) absorb( dest_sector pair) stages(first) 
 esttab  using gross-exports-iv.csv,  scalar(F   widstat N) r2 se keep( l_productivity) b(12) nostar replace
  eststo clear
 mi xeq : qui: eststo: reghdfe l_export_va  (l_productivity=l_R_D)  , vce(robust) absorb( dest_sector pair) stages(first) 
  esttab  using gross-export-va-iv.csv,  scalar(F   widstat N) r2 se keep( l_productivity) b(12) nostar replace
   eststo clear
 mi xeq : qui: eststo: reghdfe l_fd_dva_2005  (l_productivity=l_R_D)  , vce(robust) absorb( dest_sector pair)  stages(first) 
esttab  using gross-export-fddva-iv.csv,  scalar(F   widstat N) df r2 se keep( l_productivity) b(12) nostar replace
*excluding primary industries 
 eststo clear
 mi xeq : qui: eststo: reghdfe l_gross_exports (l_productivity=l_R_D) if manufac_services==1 , vce(robust) absorb( dest_sector pair)  
 esttab  using gross-exports-manuf-iv.csv,  scalar(F   widstat N) r2  df se keep( l_productivity) b(12) nostar replace
 eststo clear
 mi xeq : qui: eststo: reghdfe l_export_va  (l_productivity=l_R_D)  if manufac_services==1, vce(robust) absorb( dest_sector pair) 
  esttab  using gross-export-manuf-va-iv.csv,  scalar(F   widstat N ) df r2 se keep( l_productivity) b(12) nostar replace
  eststo clear
 mi xeq : qui: eststo: reghdfe l_fd_dva_2005  (l_productivity=l_R_D)  if manufac_services==1, vce(robust) absorb( dest_sector pair)  
esttab  using gross-export-manuf-fddva-iv.csv,  scalar(F   widstat N ) df r2 se keep( l_productivity) b(12) nostar replace
*excluding primary industries and only high income countries
 eststo clear
 mi xeq : qui: eststo: reghdfe l_gross_exports (l_productivity=l_R_D) if manufac_services==1 & (inc_group=="High income: OECD" | inc_group=="High income: nonOECD") , vce(robust) absorb( dest_sector pair)  
 esttab  using gross-exports-manuf-high-iv.csv,  scalar(F   widstat N) r2 df se keep( l_productivity) b(12) nostar replace
 eststo clear
 mi xeq : qui: eststo: reghdfe l_export_va  (l_productivity=l_R_D)  if manufac_services==1 & (inc_group=="High income: OECD" | inc_group=="High income: nonOECD") , vce(robust) absorb( dest_sector pair) 
  esttab  using gross-export-manuf-high-va-iv.csv,  scalar(F   widstat N  ) r2 se keep( l_productivity) b(12) nostar replace
  eststo clear
 mi xeq : qui: eststo: reghdfe l_fd_dva_2005  (l_productivity=l_R_D)  if manufac_services==1 & (inc_group=="High income: OECD" | inc_group=="High income: nonOECD") , vce(robust) absorb( dest_sector pair)  
esttab  using gross-export-manuf-high-fddva-iv.csv,  scalar(F   widstat N ) r2 se keep( l_productivity) b(12) nostar replace

log close
