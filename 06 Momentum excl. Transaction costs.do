***********************************************
*** MOM excl NC *******************************
***********************************************
clear all
cd "C:\Users\nicol\CBS - Copenhagen Business School\MasterThesis N&J - General\04 - Data and Code\03 Stata\Most current code\01 Data"
use "Data_1M_1976.dta", clear

// Generate fakedate and use as a time series 
gen fakedate = _n
order date fakedate
tsset fakedate


// Define parameters
scalar currencies = 9   // Total number of currency pairs
scalar top = 3            // Number of currencies in high-momentum portfolio
scalar bottom = 3         // Number of currencies in low-momentum portfolio
scalar lookback = 1  // Lookback period in months for momentum calculation

//For comparison with Hutchinson et al
//drop if _n<48
//drop if _n>485

// Full sample: no change
// 1976 to 1985 (incl)
// drop if _n>120

// 1986 to 1995 (incl)
//drop if _n<120
//drop if _n>121

// 1996 to 2005 (incl)
//drop if _n<240
//drop if _n>121

// 2006 to 2015 (incl)
//drop if _n<360
// drop if _n>121

// 2016 to 2024 (incl)
//drop if _n<480

// Calculate logs
forvalues i=1/`=currencies' {
	replace usdmid`i' = log(usdmid`i')
	replace usdbid`i' = log(usdbid`i')  
	replace usdask`i' = log(usdask`i')  
}

forvalues i=1/`=currencies' {
	replace fwusdmid`i' = log(fwusdmid`i')
	replace fwusdbid`i' = log(fwusdbid`i') 
	replace fwusdask`i' = log(fwusdask`i') 
}


***********************************************
*** Momentum Strategy *************************
***********************************************

// This means we look at returns from previous months
forvalues i=1/`=currencies' {
	gen mom`i'  = (usdmid`i' - L.fwusdmid`i')     // Long USD case, lookback = 1
}

// for other lookback periods
// +(L.usdmid`i' - L2.fwusdmid`i')+(L2.usdmid`i' - L3.fwusdmid`i')+(L3.usdmid`i' - L4.fwusdmid`i')+(L4.usdmid`i' - L5.fwusdmid`i')+(L5.usdmid`i' - L6.fwusdmid`i')

forvalues i = 1/`=currencies' {
    gen rank_mom`i' = 1 if mom`i'!=.
     forvalues j = 1/`=currencies' {
       replace rank_mom`i' = rank_mom`i' + 1 if mom`j' < mom`i' & mom`i' != . 
    }
}


egen currmiss = rowmiss(rank_mom1-rank_mom9) if _n>1
gen currused = 9-currmiss


// Manually adjust rank as we have seven datapoints which have the same fdfp in 1985 - 1994 only
replace rank_mom2 = 2 in 15
replace rank_mom7 = 3 in 17
replace rank_mom2 = 6 in 110
replace rank_mom2 = 4 in 131 // 11/28/1986 in 131 (12) when doing full samplel (subsample)

forvalues i=1/`=top' {
	gen mretlong`i' = .
}

forvalues i=1/`=top' {
	gen mretshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace mretlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_mom`i' == `j'+(L.currused-`=top')  & L.rank_mom`i' !=.
		replace mretshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_mom`i' == `j'  & L.rank_mom`i' !=.
	}
}


// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
	replace mretlong`i' = exp(mretlong`i')-1
}

forvalues i=1/`=bottom' {
	replace mretshort`i' = exp(mretshort`i')-1
}
	

// now combine the returns. EQUAL WEIGHT
egen mpflong = rowtotal(mretlong*) if _n>`=lookback'*2
replace mpflong = 1/`=top' * mpflong

egen mpfshort = rowtotal(mretshort*) if _n>`=lookback'*2
replace mpfshort = 1/`=bottom' * mpfshort 

// Calculate HML return and accumulative return
gen mHML = mpflong + mpfshort 

// Reconvert to log returns
replace mHML = log(1+mHML) 

gen msumHML = sum(mHML) if _n>`=lookback'*2

exit

**********************************************************************
*** Data analytics ***************************************************
**********************************************************************

graph set window fontface "Times New Roman"
format date %tdm/Y


**********************************************************************
*** Line graph over total cumulative return **************************
**********************************************************************

line msumHML date if _n>1 , title("Cumulative MOM returns") xtitle("Date") ytitle("Cumulative excess returns") ///
graphregion(margin(r=5)) xlabel(, angle(horizontal)) xsize(10) ysize(6) xlabel(#6) ylabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%")


**********************************************************************
*** Line Graph Maximum Drawdown **************************************
**********************************************************************

// Percentage drawdown (log returns)

gen maxmsumHML = msumHML if _n == 2
replace maxmsumHML = max(L.maxmsumHML, msumHML) if _n > 2
gen mDD = (maxmsum - msumHML)*-1

line mDD date if _n>1, title("Maximum percentage drawdown") xtitle("Date") ytitle("Drawdown") ///
graphregion(margin(r=5)) lcolor(red) xlabel(, angle(horizontal)) xsize(10) ysize(6) xlabel(#6) ylabel(0 "0%" -0.1 "-10%" -0.2 "-20%" -0.3 "-30%")
sum mDD, detail


// Calculate Summary statistics automatically
sum mHML, detail
scalar myrlymean = 12*r(mean)*100
scalar myrlysd = sqrt(12)*r(sd)*100
scalar mSR = myrlymean / myrlysd
scalar myrlyskew = r(skewness)/sqrt(12)
scalar myrlykurt = (r(kurtosis)-3)/12
scalar min = r(min)*100 
scalar max = r(max)*100 
sum mDD, detail
scalar maxmdd = r(min)*100 

// Store statistics in a row matrix
matrix mstats = (myrlymean, myrlysd, mSR, myrlyskew, myrlykurt, maxmdd, min, max)
matrix rownames mstats = "3L3S/MOM/1M/NC"
matrix colnames mstats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "Max DD" "Min" "Max"

matlist mstats, format(%9.2f)


hist mHML, bins(70) normal title("Histogram Currency Momentum Returns") xtitle("Return distribution") ytitle("Density")  xlabel(-0.1 "-10%" -0.05 "-5%" 0 "0" 0.05 "5%" 0.1 "10%") ylabel(0(5)30)


//////////////////////////////////////////
exit
//////////////////////////////////////////

// time horizon for Okunev:

// 1985 to 2000
drop if _n<108
drop if _n>193

// 2000 to 2024
drop if _n<300

summarize mDD
local avg_dd = r(mean)
local avg_pct = string(`avg_dd'*100, "%9.1f")

line mDD date if _n>1, title("Maximum percentage drawdown") xtitle("Date") ytitle("Drawdown") ///
graphregion(margin(r=5)) lcolor(red) xlabel(, angle(horizontal)) xsize(10) ysize(6) ///
xlabel(#6) ylabel(0 "0%" -0.1 "-10%" -0.2 "-20%" -0.3 "-30%" `avg_dd' "`avg_pct'%") ///
yline(`avg_dd', lpattern(dash) lcolor(gray) lwidth(normal))










