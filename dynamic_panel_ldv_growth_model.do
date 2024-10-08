cd "~/Dropbox/Audrey's Thesis"
global origin "~/Dropbox/ECON692H-2023DB"

* use this one: in class example 
/*
use nc_ppp_county_merged.dta, clear
save "$origin/audrey_ppploans.dta", replace
*/
/*
		Class notes

* Using Audrey's project on PPP loans, I'll show the following:
- labeling variables in a loop (from line 114 in data creation do file)
- collapsing the data and preserving labels (from line 140 in data creation do file)
- creating a moving average and moving sum (from line 188 in data creation do file)
- lagged dependent variable model
- sensitivity to lags
- brief introduction to dynamic panel estimators
- growth model
*/

********************************************************************************

* use "$origin/audrey_ppploans.dta", clear
use nc_ppp_county_merged.dta, clear

xtset fips week
xtdes

	*** Some descriptive analysis
* omitted from Xc: totalcountybranchdeps, lnpoptot, avgassets**
	
tab week
global Xc "lnpop18up c0_edu_collgrad c0_inc_poverfam lnmedinc c0_med_noinsura c0_pop_poprural c0_pop_shrblack c0_pop_shrasian hhi_index avgcapital avg_coredeposits banks_pc insured_ur"
global Xt "cum4_cases cw_emp cw_smallbus_num loannum loanamt"
xtsum $Xc $Xt
sum loannum, d
* will see from xtsum that time varying variables have equal within/btwn variation
* concerns: many zeroes in loannum and many missing in cw_emp cw_cardspend cw_smallbus_num
replace cw_emp=cw_emp*100   // percent change in employment

		** See if county characteristics predict loan allocation 
		** did loans go to hardest hit counties? 
reg cap_loanamt lagemp banks_pc hhi_index insured_ur avgassets lncovid4 c.lncovid4#month c.week##c.week, cluster(fips) 
outreg2 using loanallocation.doc, append ctitle(Total week loan amount per capita) adjr2 drop(c.lncovid4#month) addnote("Other control: 4-week sum of covid cases per capita interacted with month")

* ran below w loannum and loanamt
reg loanamt lncovid4 lnpop18up banks_pc c0_edu_collgrad c0_inc_poverfam lnmedinc c0_med_noinsura c0_pop_poprural c0_pop_shrblack c0_pop_shrasian insured_ur hhi_index avgassets avg_coredeposits c.week##c.week, cluster(fips)
outreg2 using loanallocation2.doc, append ctitle(Week loan total amount)

***** see if predictors are correlated w each other + drop some
pwcorr loanamt loannum cum4_cases month lnpoptot lnpop18up c0_edu_collgrad c0_inc_poverfam lnmedinc c0_med_noinsura c0_pop_poprural c0_pop_shrblack c0_pop_shrasian totalcountybranchdeps hhi_index avgcapital avgassets avg_coredeposits banks_pc insured_ur

	*** Basic regression  analysis
	* initial ols: cluster SEs first - shows neg effect, need to adjust/add controls
    * instead of creating dummy variables for each week do c. week_

gen sample=1 if e(sample)==1

reg cw_emp cap10_loanamt c.week##c.week, cluster(fips)
outreg2 using initialreg2.doc, append ctitle(OLS) adjr2 stats(coef pval)
* major concern#1: statistically significant counter-intuitive negative effect of loans on employment - check if results are robust/hold with variations 
* minor concern#2: coefficients are too small in magnitude -> hard to interpret -> re-scale either dependent (multiply by X) or independent variables (divide by X)

* negative result wrt to 10-week rolling sum of loans per capita
* same negative result in RE (with controls) and FE models
* same negative result wrt to 10-week rolling total of loan amount
* same negative result wrt both number and size in same regression
reg cw_emp cap10_loanamt lnpop18up lncovid4 c.cum4cases_pc#month c.week##c.week banks_pc c0_edu_collgrad lnmedinc c0_pop_poprural c0_pop_shrblack c0_pop_shrasian hhi_index avgassets avg_coredeposits insured_ur, cluster(fips)
outreg2 using regvariations.doc, replace ctitle(OLS, Controls) stats(coef pval) adjr2 drop(c.cum4cases_pc#month c0_edu_collgrad hhi_index avgassets avg_coredeposits insured_ur lnmedinc c0_pop_shrblack) addnote(Other controls: c.cum4_cases#month c0_edu_collgrad hhi_index avgassets avg_coredeposits insured_ur lnmedinc c0_pop_shrblack)

xtreg cw_emp cap10_loanamt lnpop18up lncovid4 c.cum4cases_pc#month banks_pc c0_edu_collgrad lnmedinc c0_pop_poprural c0_pop_shrblack c0_pop_shrasian hhi_index avgassets avg_coredeposits insured_ur c.week##c.week, cluster(fips) re
outreg2 using regvariations.doc, append ctitle(Random Effects) stats(coef pval) drop(c.cum4cases_pc#month c0_edu_collgrad hhi_index avgassets avg_coredeposits insured_ur lnmedinc c0_pop_shrblack)

xtreg cw_emp cap10_loanamt cum4cases_pc c.week##c.week, cluster(fips) fe
outreg2 using regvariations.doc, append ctitle(Fixed Effects) 

	*** Separating zeroes from loan amount; creating T/C groups
	* (dc its creating lnnaics and lnrural correctly?)
	
gen loanamtD=(loanamt>0) if loanamt<.
label var loanamtD "=1 if any PPP loan is distributed in county-week"
gen naicsamtD=(naics72T>0) if naics72T<. 
label var naicsamtD "=1 if any PPP loan distributed in county-week to NAICS72 businesses"
gen ruralamtD=(ruralT>0) if ruralT<.
label var ruralamtD "=1 if any PPP loan distributed in county-week to Rural communities"

gen lnloanamt=ln(cap_loanamt+1)
label var lnloanamt "Log of loans per capita, ln(x+1)"
gen lnnaicsamt=ln((naics72T/county_pop2019)+1)
label var lnnaicsamt "Log of naics72T per capita, ln(x+1)"
gen lnruralamt=ln((ruralT/county_pop2019)+1)
label var lnruralamt "Log of ruralT per capita, ln(x+1)"


reg cw_emp loanamtD lnloanamt c.cum4cases_pc#month lnpoptot c0_edu_collgrad lnmedinc c0_med_noinsura c0_pop_poprural c0_pop_shrblack c0_pop_shrasian hhi_index avgassets avg_coredeposits c.banks_pc#c.loanamtD insured_ur c.week##c.week, cluster(fips)	
xtreg cw_emp loanamtD lnloanamt c.cum4_cases#month c.week##c.week, cluster(fips) fe
* (add to slides?) strong negative employment effect of loans along both extensive and intensive margins

	*** Understanding the issue

tab date_sat loanamtD if sample==1
tab date_sat loanamtD if sample==1, sum(loannum) nof nost
* discovery: N of counties in the control group per week of PPP program is very small
* what will happen if we use loanamtD as a treatment indicator? By default, we will get a negative effect because the program occurred during COVID lockdown period (high unemployment) and during Omicron wave -> selection on dependent variable


	*** Lagged dependent variable (LDV) model :)
	*TD: adjust covidcases - control for the nine weeks before f10

reg f10.cw_emp cw_emp loanamtD lnloanamt lncovid4 $Xc  c.week##c.week, cluster(fips) // OLS
outreg2 using ldv.doc, replace ctitle(OLS) drop(c0_med_noinsura lnmedinc lnpop18up c0_edu_collgrad c0_pop_shrblack avgcapital avg_coredeposits hhi_index c0_pop_shrasian c0_inc_poverfam) addnote(Other controls: c0_med_noinsura, lnmedinc, c0_edu_collgrad, c0_pop_shrblack, avgcapital, avg_coredeposits, c0_pop_shrasian, c0_inc_poverfam, hhi_index)

xtreg f10.cw_emp cw_emp loanamtD lnloanamt lncovid4 $Xc c.week##c.week, cluster(fips) re // RE model
outreg2 using ldv.doc, append ctitle(Random Effects) drop(c0_med_noinsura lnmedinc lnpop18up c0_edu_collgrad c0_pop_shrblack avgcapital avg_coredeposits hhi_index c0_pop_shrasian c0_inc_poverfam)
xtreg f10.cw_emp cw_emp loanamtD lnloanamt lncovid4 c.week##c.week, cluster(fips) fe // FE model
outreg2 using ldv.doc, append ctitle(Fixed Effects)
foreach v in cw_emp loanamtD lnloanamt cw_cases_new {
    egen ave`v'=mean(`v') if e(sample)==1, by(fips)
}
xtreg f10.cw_emp cw_emp loanamtD lnloanamt lncovid4 ave* c.week##c.week, cluster(fips) re // CRE
outreg2 using ldv.doc, append ctitle(Correlated Random Effects)

	*** Sensitivity to the choice of lags
	
xtreg f1.cw_emp cw_emp loanamtD lnloanamt c.lncovid4#c.loanamtD $Xc ave* c.week##c.week, cluster(fips) re 
outreg2 using lagdepvar.doc, replace ctitle(lag1) keep(cw_emp loanamtD lnloanamt)
foreach v of numlist 2/10 {
    qui xtreg f`v'.cw_emp cw_emp loanamtD lnloanamt lncovid4 $Xc ave* c.week##c.week, cluster(fips) re 
	outreg2 using lagdepvar.doc, append ctitle(lag`v')
}

	*** TD: Caveats
	
* The above is sufficient for an undergraduate thesis; you may consider adding interaction terms,  considering other types of loans, using alternative dependent variables, etc.
xtreg f10.cw_emp cw_emp i.loanamtD##c.c0_pop_shrblack c.lnloanamt##c.c0_pop_shrblack lncovid4 $Xc ave* c.week##c.week, cluster(fips) re 
* However, there are some issues with the above estimates
* Random effects are correlated with the lagged dependent variable, which violates the assumption of zero corr between random effects and covariates
* Demeaning in LDV models introduces so called Nickell bias due to correlation between LDV and the error. The bias is especially large when N is large and T is small
* These slides explain the Nickell bias well : http://fmwww.bc.edu/EC-C/S2013/823/EC823.S2013.nn05.slides.pdf
* You can argue that T is relatively large in your case
* More advanced solution is using Arellano-Bond or Bond-Blundell estimators: xtabond or xtabond2

	*** Growth model (td- add new reg w )

gen growth=f.cw_emp-cw_emp
xtreg growth loanamtD lnloanamt lncovid4 c.lncovid4#month $Xc ave* c.week##c.week, cluster(fips) re 
outreg2 using growth.doc, replace ctitle(grw1) keep(loanamtD lnloanamt c0_inc_poverfam lnmedinc)
drop growth
foreach v of numlist 2/10 {
	gen growth=f`v'.cw_emp-cw_emp
    qui xtreg growth loanamtD lnloanamt lncovid4 c.lncovid4#month $Xc ave* c.week##c.week, cluster(fips) re 
	outreg2 using growth.doc, append ctitle(grw`v') keep(loanamtD lnloanamt c0_inc_poverfam lnmedinc)
	drop growth
}

* LO: Again you may extend this model by adding interaction terms, considering other types of loans, using alternative dependent variables, including other determinants of employment growth (growth in spending)

* Differential Impacts of Loans Using Growth Model 
gen growth=f10.cw_emp-cw_emp

xtreg growth i.loanamtD##c.c0_pop_shrblack c.lnloanamt##c.c0_pop_shrblack lncovid4 $Xc ave* c.week##c.week, cluster(fips) re 
outreg2 using differential.doc, replace ctitle(% Black Population) keep(i.loanamtD##c.c0_pop_shrblack c.lnloanamt##c.c0_pop_shrblack) 

xtreg growth loanamtD c.lnnaicsamt#month c.lncovid4#c.lnnaicsamt $Xc ave* c.week##c.week, cluster(fips) re 
outreg2 using differential.doc, append ctitle(Naics72 Businesses) keep(loanamtD c.lnnaicsamt#month c.lncovid4#c.lnnaicsamt) 



gen grwspnd=f10.cw_cardspend-cw_cardspend
xtreg growth grwspnd i.loanamtD lnloanamt lncovid $Xc ave* c.week##c.week, cluster(fips) re 
* You may also add financial sector development as an employment growth factor or use some of the banking sector indicators as instrumental variables for loans. 




