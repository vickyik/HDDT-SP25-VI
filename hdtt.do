cd "/Users/victoryikpea/Desktop/HDDT IMAGE"
import delimited "fund_stata_ready.csv", clear
describe
codebook
summarize
describe   // Overview of variables
list       // View the dataset
summarize  // Summary statistics

misstable summarize
gen rate=rateper100000population
replace rate100000 = (cases / population) * 100000 if missing(rate100000)
list population if missing(real(population))
tab rate medianincome
tab medianincome
list _id medianincome if medianincome>200000 & medianincome~=.   
scatter rate100000  no2mean1hr
scatter rate100000  pm25weight~r
scatter rate100000 medianincome
drop v10 v11
drop if medianincome == 576137
summarize population

gen incomecat = .
replace incomecat = 1 if medianincome < 40000
replace incomecat = 2 if medianincome >= 40000 & medianincome < 60000
replace incomecat = 3 if medianincome >= 60000 & medianincome < 80000
replace incomecat = 4 if medianincome >= 80000 & medianincome < 100000
replace incomecat = 5 if medianincome >= 100000
label define inc_labels 1 "Low Income" 2 "Lower-Middle Income" 3 "Middle Income" 4 "Upper-Middle Income" 5 "High Income"
label values incomecat inc_labels



tab incomecat
tab pm25weightedmean24hr
save "hdttcali.dta", replace
summarize rate100000
drop if cases == 0
tab cases
use "hdttcali.dta", replace
tab rate100000
regress rate100000 i.incomecat no2mean1hr pm25weightedmean year 
predict iniadj, xb

regress rate100000 i.incomecat
regress rate100000 no2mean1hr
regress rate100000 pm25weightedmean24hr
regress rate100000 year


stepwise, pr(0.15): ///
	regress rate100000 i.incomecat no2mean1hr pm25weightedmean year



label define incomelab 1 "Low" 2 "Low-Mid" 3 "Mid" 4 "Upper-Mid" 5 "High"
label values incomecat incomelab
*Predictions for fitted values and standard errors
predict tbrate, xb
predict tbratestdp, stdp 
predict tbratestdf, stdf

twoway scatter tbrate no2mean1hr || lfit tbrate no2mean1hr, lwidth(1) ///
	title("Tuberculosis rate by NO2 (Mean per 1hr)") ///
	ytitle("Tuberculosis rate") ///
	legend(ring(0) col(1) pos(1)) ///
	ylab(, angle(0))
	


twoway scatter tbrate pm25weightedmean24hr || lfit tbrate pm25weightedmean24hr, lwidth(1) ///
	title("Tuberculosis rate by PM2.5 (Mean per 24hrs)") ///
	ytitle("Tuberculosis rate") ///
	legend(ring(0) col(1) pos(1)) ///
	ylab(, angle(0))
	
correlate rate100000 pm25weightedmean	
correlate rate100000 no2mean1hr
	
tab incomecat
graph box rate100000, over(incomecat) ///
    title("Tuberculosis Rate by Income Category") ///
    ytitle("Tuberculosis Rate") ///
    scheme(s3color)
	
graph box rate100000, over(incomecat) ///
    title("Tuberculosis Rate by Income Category") ///
    ytitle("Tuberculosis Rate") ///
	box(1, color(red)) marker(1, mcolor(red)) ///
	box(2, color(blue)) marker(2, mcolor(blue)) box(3, color(green)) marker(3, mcolor(green)) ///
	box(4, color(brown)) marker(4, mcolor(brown)) box(5, color(purple)) marker(5, mcolor(purple)) ///
		asyvars showyvars leg(off)

	



	
	graph hbox rate100000, over(year) ///
    title("Tuberculosis Rate by Year") ///
    ytitle("Tuberculosis Rate per 100,000")
	
	
	

collapse (mean) rate100000 no2mean1hr pm25weight~r, by(year)
twoway ///
    (line rate100000 year, lcolor(blue) lwidth(large)) ///
    (line no2mean1hr year, yaxis(2) lcolor(red) lpattern(dash) lwidth(large)) ///
    (line pm25weight~r year, yaxis(2) lcolor(green) lpattern(dash) lwidth(large)), ///
    title("Trend of TB Rate, NO₂, and PM2.5 Over Time") ///
    ytitle("TB Rate per 100,000", axis(1)) ///
    ytitle("Pollution Levels (NO₂ & PM2.5)", axis(2)) ///
    xtitle("Year") ///
    legend(order(1 "TB Rate" 2 "NO₂ Levels" 3 "PM2.5 Levels"))

xi: regress rate100000 no2mean1hr pm25weightedmean i.incomecat year
scatter rate100000 no2mean1hr, mcolor(blue) msymbol(o)
lpoly rate100000 no2mean1hr, degree(2) lcolor(red)

regress rate100000 c.no2mean1hr##c.pm25weightedmean i.incomecat year
regress rate100000 no2mean1hrpm25weightedmean i.incomecat year
predict intr, xb
regress rate100000 no2mean1hr pm25weightedmean

twoway ///
    (scatter inted pm25weightedmean24hr, mcolor(%30 green) msymbol(circle) msize(medium)) || ///
    (lfit inted pm25weightedmean24hr, lcolor(green) lwidth(thick) lpattern(dash)), ///
    title("Tuberculosis Rate by PM2.5 (Annual µg/m³)", size(large) color(black)) ///
    ytitle("Tuberculosis Rate per 100,000", size(medium) color(black)) ///
    xtitle("PM2.5 Levels (Annual µg/m³)", size(medium) color(black)) ///
    legend(order(1 "Observed TB Rate" 2 "Fitted Line") ///
           pos(6) ring(0) col(1) size(medium) region(fcolor(white))) ///
    ylabel(, angle(0) labsize(small)) ///
    xlabel(, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(lcolor(black))

twoway ///
    (scatter inted no2mean1hr, mcolor(%30 blue) msymbol(circle) msize(medium)) || ///
    (lfit inted no2mean1hr, lcolor(blue) lwidth(thick) lpattern(dash)), ///
    title("Tuberculosis Rate by NO₂ (Annual ppb)", size(large) color(black)) ///
    ytitle("Tuberculosis Rate per 100,000", size(medium) color(black)) ///
    xtitle("NO₂ Levels (Annual ppb)", size(medium) color(black)) ///
    legend(order(1 "Observed TB Rate" 2 "Fitted Line") ///
           pos(5) ring(0) col(1) size(medium) region(fcolor(white))) ///
    ylabel(, angle(0) labsize(small)) ///
    xlabel(, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(lcolor(black))

lowess rate100000 pm25weightedmean24hr
imtest
predict resid, residuals
histogram red, normal
qnorm red
regress rate100000 c.no2mean1hr##c.pm25weightedmean i.incomecat year vce(robust)
regress rate100000 c.no2mean1hr##c.pm25weightedmean i.incomecat year, vce(robust)
predict inted, xb
predict red, residuals

summarize red, detail
drop if red > 15

	
**scatter pot showing tbrate and no2 and pm2,5
	twoway ///
    (scatter inted no2mean1hr, mcolor(%30 blue) msize(medium) msymbol(circle)) ///
    (lfit inted no2mean1hr, lcolor(blue) lpattern(dash)) ///
    (scatter inted pm25weightedmean, mcolor(%30 red) msize(medium) msymbol(triangle)) ///
    (lfit inted pm25weightedmean, lcolor(red) lpattern(dash)), ///
    title("TB Rate vs. NO₂ (ppb) & PM2.5 (ug/m3)", size(large) color(black)) ///
    ytitle("TB Rate per 100,000", size(medium) color(black)) ///
    xtitle("Pollutant Levels", size(medium) color(black)) ///
    legend(order(1 "NO₂ Levels" 2 "NO₂ Fit" 3 "PM2.5 Levels" 4 "PM2.5 Fit") ///
           size(medium) position(5) ring(0) symxsize(3) symysize(3)) ///
    graphregion(color(white)) ///
    plotregion(lcolor(black))
**scatter pot showing tbrate and no2 and pm2,5 across incomecat	
	twoway ///
    (scatter rate100000 no2mean1hr, mcolor(%30 blue) msymbol(circle)) ///
    (lfit rate100000 no2mean1hr, lcolor(blue) lpattern(dash)) ///
    (scatter rate100000 pm25weightedmean, mcolor(%30 green) msymbol(triangle)) ///
    (lfit rate100000 pm25weightedmean, lcolor(red) lpattern(dash)), ///
    by(incomecat, title("TB Rate vs. NO₂ (ppb) & PM2.5 (ug/m3) by Income") ///
             note("Each panel represents different year income categories")) ///
    ytitle("TB Rate per 100,000") ///
    xtitle("Pollutant Levels") ///
    legend(order(1 "NO₂ Levels" 2 "NO₂ Fit" 3 "PM2.5 Levels" 4 "PM2.5 Fit") ///
		size(vsmall) position(6) ring(0) col(1) region(fcolor(white)))

		
*checking model assumptions	
regress rate100000 i.incomecat no2mean1hr pm25weightedmean year 	
predict his, residual
hist his, norm	
imtest 
hettest
vif
xi: regress rate100000 no2mean1hr pm25weightedmean i.incomecat year
twoway scatter rate100000 no2mean1hr
twoway scatter rate100000 no2mean1hr || line tbrate no2mean1hr, lwidth(1) ///
	title(Tuberculosis rate by No2(Mean per 1hr)) ///
	legend(ring(0) col(1) pos(1)) ///
	ylab(, angle(0)) xlab(0(10)30)

*fixing model

gen income_group = .
replace income_group = 1 if incomecat == 1 | incomecat == 2
replace income_group = 2 if incomecat == 3 | incomecat == 4
replace income_group = 3 if incomecat == 5
label define inc_labe 1  
label values income_group inc_labe
tab income_group

regress rate100000 i.income_group no2mean1hr pm25weightedmean year, robust
predict iniadj2, xb
regress rate100000 i.income_group
imtest 
hettest
vif

* robust standard error used for heteroskdacity


*adding interaction term
regress rate100000 i.income_group c.no2mean1hr##c.pm25weightedmean24hr year, robust
predict intnal, xb
 *testing assumpions
predict residd, residual
hist residd, norm
vif
imtest
**scatter pot showing tbrate and no2 and pm2,5
	twoway ///
    (scatter intnal no2mean1hr, mcolor(%30 blue) msize(medium) msymbol(circle)) ///
    (lfit intnal no2mean1hr, lcolor(blue) lpattern(dash)) ///
    (scatter intnal pm25weightedmean, mcolor(%30 red) msize(medium) msymbol(triangle)) ///
    (lfit intnal pm25weightedmean, lcolor(green) lpattern(dash)), ///
    title("TB Rate vs. NO₂ (ppb) & PM2.5 (ug/m3)", size(large) color(black)) ///
    ytitle("TB Rate per 100,000", size(medium) color(black)) ///
    xtitle("Pollutant Levels", size(medium) color(black)) ///
    legend(order(1 "NO₂ Levels" 2 "NO₂ Fit" 3 "PM2.5 Levels" 4 "PM2.5 Fit") ///
           size(medium) position(5) ring(0) symxsize(3) symysize(3)) ///
    graphregion(color(white)) ///
    plotregion(lcolor(black))

	
**scatter pot showing tbrate and no2 and pm2,5 across incomecat	

		
	twoway ///
    (scatter intnal no2mean1hr, mcolor(%30 blue) msymbol(circle)) ///
    (lfit intnal no2mean1hr, lcolor(blue) lpattern(dash)) ///
    (scatter intnal pm25weightedmean, mcolor(%30 green) msymbol(triangle)) ///
    (lfit intnal pm25weightedmean, lcolor(green) lpattern(dash)), ///
    by(income_group, title("TB Rate vs. NO₂ (ppb) & PM2.5 (ug/m3) by Income") ///
        note("Each panel displays pollutant-TB relationships within income categories, dashed lines represent fitted trends. Associations vary by income level.")) ///
    ytitle("TB Rate per 100,000") ///
    xtitle("Pollutant Levels") ///
    legend(order(1 "NO₂ Levels" 2 "NO₂ Fit" 3 "PM2.5 Levels" 4 "PM2.5 Fit") ///
        size(vsmall) position(6) ring(0) col(1) region(fcolor(white)))
gen interaction = no2mean1hr * pm25weightedmean24hr	
preserve
 collapse (mean) rate100000 no2mean1hr pm25weightedmean24hr, by(year)

twoway ///
    (line rate100000 year, lcolor(red) lwidth(large)) ///
    (line no2mean1hr year, yaxis(2) lcolor(blue) lpattern(dash) lwidth(large)) ///
    (line pm25weightedmean24hr year, yaxis(2) lcolor(green) lpattern(dash) lwidth(large)), ///
    title("Trend of TB Rate, NO₂, and PM2.5 Over Time") ///
    ytitle("TB Rate per 100,000", axis(1)) ///
    ytitle("Pollution Levels (NO₂ & PM2.5)", axis(2)) ///
    xtitle("Year") ///
    legend(order(1 "TB Rate" 2 "NO₂ Levels" 3 "PM2.5 Levels"))

use "hdttcali.dta", replace

graph box rate100000, over(income_group) ///
    title("Tuberculosis Rate by Income Category") ///
    ytitle("Tuberculosis Rate") ///
	box(1, color(red)) marker(1, mcolor(red)) ///
	box(2, color(blue)) marker(2, mcolor(blue)) box(3, color(green)) marker(3, mcolor(green)) ///
		asyvars showyvars leg(off)
		
graph bar year, over(rate100000, gap(10)) over(pm25weightedmean) over(no2mean1hr)

*descriptive statistics
summarize population cases rate100000 no2mean1hr pm25weightedmean24hr, detail
