***********************************************
*** MOM incl TC *******************************
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


// Sample period
// Full sample: no change

// 1985 to 1994 (incl.)
** drop if fakedate > 121

// 1995 to 2004 (incl.)
** drop if fakedate < 121
** drop if fakedate > 241

// 2005 to 2014 (incl.)
** drop if fakedate < 241
** drop if fakedate > 361

// 2015 to 2024 (incl.)
** drop if fakedate < 361


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
replace rank_mom2 = 4 in 131


// calculate the returns of every "enterexit" combination. 
// EX = exit
// NX = no exit,remain

// EX return, exit
forvalues i=1/`=top' {
	gen exmretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen exmretshort`i' = .
}


forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace exmretlong`j' = (usdbid`i' - L.fwusdask`i') if L.rank_mom`i' == `j'+(L.currused-`=top') & L.rank_mom`i' !=.
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace exmretshort`j' = (L.fwusdbid`i' - usdask`i') if L.rank_mom`i' == `j' & L.rank_mom`i' !=.
	}
}


// NX return, no exit

forvalues i=1/`=top' {
	gen nxmretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen nxmretshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace nxmretlong`j' = (usdmid`i' - L.fwusdask`i') if L.rank_mom`i' == `j'+(L.currused-`=top') & L.rank_mom`i' !=.
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace nxmretshort`j' = (L.fwusdbid`i' - usdmid`i') if L.rank_mom`i' == `j' & L.rank_mom`i' !=.	
	}
}


**************************************************
// combine the returns, depending on EX or NX
**************************************************

// Indicators for portfolio membership (both long and short). If currency 1 (usd1=usdeur) is in the long portfolio, in_long1 has indicator variable "1". By this, we know which three currencies are in the long portfolio and in the short portfolio.

// In long portfolio. For each time period, identify if currency is in long portfolio
forvalues i = 1/`=currencies' {
        gen in_long`i' = (rank_mom`i' == currused-`=top'+1 | rank_mom`i' == currused-`=top'+2 | rank_mom`i' == currused-`=top'+3) if _n>1
}

// In short portfolio. For each time period, identify if currency is in short portfolio. Here, the formula is a bit more simple as our counter always starts at 1. 
// NOTE: Here we need a manual adjustment if we change the number of currencies in the portfolio. 4L4S requres the inclusion of rank 4 as well.

forvalues i = 1/`=currencies' {
		gen in_short`i' = (rank_mom`i' == 1| rank_mom`i' == 2 | rank_mom`i' == 3) if _n>1
}



// now generate indicator for two different scenarios. 
// EX = enter at t, EXIT at t+1
// NX = enter at t, REMAIN at t+1


// Generate scenario indicators (note: if condition is true, value of variable = 1)
forvalues i = 1/`=currencies' {
    gen ex_long`i' = (in_long`i' == 0 & L.in_long`i' == 1 )            // exit
    gen nx_long`i' = (in_long`i' == 1 & L.in_long`i' == 1 )            // remain
        
    gen ex_short`i' = (in_short`i' == 0 & L.in_short`i' == 1 )         // exit
    gen nx_short`i' = (in_short`i' == 1 & L.in_short`i' == 1 )         // remain  
 }

 
//Calculate final returns using the existing return variables
forvalues i = 1/`=top' {
    gen final_long`i' = . & _n>2
}

forvalues i = 1/`=bottom' {
    gen final_short`i' = . & _n>2
}

// Apply returns based on scenarios
forvalues i=1/`=top' {
    forvalues j = 1/`=currencies' {
        replace final_long`i' = exmretlong`i' if ex_long`j' == 1 & L.rank_mom`j'== `i'+(L.currused-`=top') 
        replace final_long`i' = nxmretlong`i' if nx_long`j' == 1 & L.rank_mom`j'== `i'+(L.currused-`=top') 
          
        replace final_short`i' = exmretshort`i' if ex_short`j' == 1 & L.rank_mom`j' == `i' 
        replace final_short`i' = nxmretshort`i' if nx_short`j' == 1 & L.rank_mom`j' == `i' 
       }
}

forvalues i = 1/`=top' {
    replace final_long`i' = . if _n<3 
	replace final_short`i' = . if _n<3 
	}

	

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
    rename final_long`i' retlong`i'
	replace retlong`i' = exp(retlong`i')-1
}

forvalues i=1/`=bottom' {
    rename final_short`i' retshort`i'
	replace retshort`i' = exp(retshort`i')-1
}
	

// now combine the returns. EQUAL WEIGHT
egen mpflong = rowtotal(retlong*) if _n>2
replace mpflong = 1/`=top' * mpflong if _n>2

egen mpfshort = rowtotal(retshort*) if _n>2
replace mpfshort = 1/`=bottom' * mpfshort if _n>2

// Calculate HML return and accumulative return
gen mHML = mpflong + mpfshort if _n>2

// Reconvert to log returns
replace mHML = log(1+mHML)

gen msumHML = sum(mHML) if _n>2

exit

**********************************************************************
*** Data analytics ***************************************************
**********************************************************************

graph set window fontface "Times New Roman"
format date %tdm/Y


**********************************************************************
*** Line graph over total cumulative return **************************
**********************************************************************

line msumHML date if _n>1 , title("Cumulative MOM returns incl. TC") xtitle("Date") ytitle("Cumulative excess returns") ///
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
sum mDD, detail
scalar maxmdd = r(min)*100 

// Store statistics in a row matrix
matrix mstats = (myrlymean, myrlysd, mSR, myrlyskew, myrlykurt, maxmdd)
matrix rownames mstats = "MOM"
matrix colnames mstats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "Max DD"

matlist mstats, format(%9.2f)


//////////////////////////////////////////
exit
//////////////////////////////////////////

// time horizon for Okunev:

// 1985 to 2000
drop if _n<108
drop if _n>193

// 2000 to 2024
drop if _n<300














