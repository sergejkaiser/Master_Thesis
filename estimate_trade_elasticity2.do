version 12
set more off /*full output is displayed
w/o having to hit the space bar in between*/
capture log close //closes any previously opened log-files
clear all
global dir "/Users/sergej/Documents/Master/Thesis/June_Empirical_Analysis/Send_liza/"
cd "$dir/Working_Data"
log using "thesis_regression_part2_03_11_2015", text replace
use thesis_data2_FD_VA_va_exgr_rd_price_03_sep_2015.dta, clear
eststo clear
set matsize 11000 
eststo: areg l_productivity logrdexpend  I.dest_sector, absorb(pair)
predict productivity_hat4, xb

*first stage

  eststo:  areg l_productivity logrdexpend  I.dest_sector if manufac_services==1, absorb(pair)
predict productivity_hat6, xb
eststo:  areg l_productivity logrdexpend  I.dest_sector if EU==1, absorb(pair)
predict productivity_hat7, xb
eststo:  areg l_productivity logrdexpend   I.dest_sector if mining==1, absorb(pair)
predict productivity_hat8, xb
*second stage PPML estimation based on Silva Log of Gravity
eststo: ppml EXGR productivity_hat4 alpha* beta*
 
eststo: ppml EXGR productivity_hat6 alpha* beta* if manufac_services==1
eststo: ppml EXGR productivity_hat8 alpha* beta*  if mining==1

eststo: ppml EXGR productivity_hat7 alpha* beta* if EU==1 
 esttab est5 est6 est7 est8  , replace r2 scalars(ll, converged, aic, bic)   se b(2) keep(productivity_hat4 productivity_hat8 productivity_hat6 productivity_hat7) mtitles("Full Sample" "Manufacutring Service" "Without mining" "EU")
esttab est5 est6 est7 est8 using ppml.tex, replace pr2 scalar(ll) scalar(converged) booktabs  se b(2) keep(productivity_hat4   productivity_hat8 productivity_hat6 productivity_hat7) mtitles("Full Sample" "Manufacutring Service" "Without mining" "EU")
 e(converged) 
eststo: ppml VA productivity_hat4 alpha* beta*
 
eststo: ppml VA productivity_hat6 alpha* beta* if manufac_services==1
eststo: ppml VA productivity_hat8 alpha* beta*  if mining==1

eststo: ppml VA productivity_hat7 alpha* beta* if EU==1 
 
 
 esttab est9 est10 est11 est12 using ppml_va.tex, replace r2 scalar(ll) booktabs  se b(2) keep(productivity_hat4   productivity_hat8 productivity_hat6 productivity_hat7) mtitles("Full Sample" "Manufacutring Service" "Without mining" "EU")
*ivreset
*eststo: ppml EXGR productivity_hat4 alpha* beta* 
    *predict yhat, xb
   * g yf=yhat^2
  * eststo: ppml EXGR productivity_hat4 alpha* beta*  yf
  *test yf=0
  log close
