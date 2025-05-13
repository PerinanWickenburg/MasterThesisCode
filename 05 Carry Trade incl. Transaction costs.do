***********************************************
*** CCT 3L3S 1M TC ****************************
***********************************************
clear all
cd "C:\Users\nicol\CBS - Copenhagen Business School\MasterThesis N&J - General\04 - Data and Code\03 Stata\Most current code\01 Data"
use "Data_Halfspreads_1M_1976.dta", clear

// Generate fakedate and use as a time series 
gen fakedate = _n
order date fakedate
tsset fakedate

// Define parameters
scalar currencies = 9   // Total number of currency pairs
scalar top = 3            // Number of currencies in high portfolio
scalar bottom = 3         // Number of currencies in low portfolio
scalar period = 12  // Lookback period in months for momentum calculation 

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

// 1985 to 2024. CHANGE ALSO BELOW WHEN DOING THIS ANALYSIS
drop if _n<108

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

// Calculate forward discount / forward premium USING MID PRICES
forvalues i=1/`=currencies' {
	gen fdfp`i' = fwusdmid`i' - usdmid`i'  
}

***********************************************
*** Traditional Carry Trade with TC ***********
***********************************************
// Generate rank of currency based on fdfp
forvalues i = 1/`=currencies' {
     gen rank_fdfp`i' = 1 if fdfp`i'!=.
    
    forvalues j = 1/`=currencies' {
        replace rank_fdfp`i' = rank_fdfp`i' +1 if fdfp`j' > fdfp`i' & fdfp`j' !=.
    }
}

// generate help variable to find correct number of currency pairs. currmiss shows how many of the 9 currencies are missing in the dataset at the given datapoint. currused shows how many currencies are consequently used in the pf building.

egen currmiss = rowmiss(fdfp1-fdfp9)
gen currused = 9-currmiss

// Manually adjust rank as we have seven datapoints which have the same fdfp in 1985 - 1994 only
//replace rank_fdfp2 = L.rank_fdfp2 in 3
//replace rank_fdfp6 = L.rank_fdfp6 in 16
//replace rank_fdfp2 = L.rank_fdfp2 in 25
//replace rank_fdfp1 = 5 in 66
//replace rank_fdfp9 = 3 in 84
//replace rank_fdfp6 = L.rank_fdfp6 in 32 // 07/31/1987 "in 139 (20) when doing full sample (subsample)"
//replace rank_fdfp2 = 3 in 79 // 06/30/1992 "in 198 (79) when doing full sample (subsample)"

// 1985 to 2024. CHANGE ALSO ABOVE WHEN DOING THIS ANALYSIS
replace rank_fdfp6 = L.rank_fdfp6 in 32 // when doing subsample 1985-2024
replace rank_fdfp2 = 3 in 91 // when doing subsample 1985-2024
 



// calculate the returns of every "enterexit" combination. 
// EX = exit
// NX = no exit,remain

// EX return, exit
forvalues i=1/`=top' {
	gen exretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen exretshort`i' = .
}


forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace exretlong`j' = (usdbid`i' - L.fwusdask`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace exretshort`j' = (L.fwusdbid`i' - usdask`i') if L.rank_fdfp`i' == `j'	
	}
}


// NX return, no exit

forvalues i=1/`=top' {
	gen nxretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen nxretshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace nxretlong`j' = (usdmid`i' - L.fwusdask`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace nxretshort`j' = (L.fwusdbid`i' - usdmid`i') if L.rank_fdfp`i' == `j'	
	}
}


**************************************************
// now combine the returns, depending on EX or NX
**************************************************

// Create indicators for portfolio membership (both long and short). If currency 1 (usd1=usdeur) is in the long portfolio, in_long1 has indicator variable "1". By this, we know which three currencies are in the long portfolio and in the short portfolio.

// In long portfolio. For each time period, identify if currency is in long portfolio
forvalues i = 1/`=currencies' {
        gen in_long`i' = (rank_fdfp`i' == currused-`=top'+1 | rank_fdfp`i' == currused-`=top'+2 | rank_fdfp`i' == currused-`=top'+3)
}

// In short portfolio. For each time period, identify if currency is in short portfolio. Here, the formula is a bit more simple as our counter always starts at 1. 
// NOTE: Here we need a manual adjustment if we change the number of currencies in the portfolio. 4L4S requres the inclusion of rank 4 as well.

forvalues i = 1/`=currencies' {
		gen in_short`i' = (rank_fdfp`i' == 1| rank_fdfp`i' == 2 | rank_fdfp`i' == 3)
}

// now generate indicator for two different scenarios. 
// EX = enter at t, EXIT at t+1
// NX = enter at t, REMAIN at t+1


// Generate scenario indicators (if condition is true, the value of variable = 1)
forvalues i = 1/`=currencies' {
    gen ex_long`i' = (in_long`i' == 0 & L.in_long`i' == 1 )             // exit
    gen nx_long`i' = (in_long`i' == 1 & L.in_long`i' == 1 )             // remain
        
    gen ex_short`i' = (in_short`i' == 0 & L.in_short`i' == 1 )         // exit
    gen nx_short`i' = (in_short`i' == 1 & L.in_short`i' == 1 )         // remain  
 }


//Calculate final returns using the existing return variables to trigger transaction costs
forvalues i = 1/`=top' {
    gen final_long`i' = 0 
}

forvalues i = 1/`=bottom' {
    gen final_short`i' = 0	
}

// Apply returns based on scenarios
forvalues i=1/`=top' {
    forvalues j = 1/`=currencies' {
        replace final_long`i' = exretlong`i' if ex_long`j' == 1 & L.rank_fdfp`j'== `i'+(L.currused-`=top') 
        replace final_long`i' = nxretlong`i' if nx_long`j' == 1 & L.rank_fdfp`j'== `i'+(L.currused-`=top') 
          
        replace final_short`i' = exretshort`i' if ex_short`j' == 1 & L.rank_fdfp`j' == `i'
        replace final_short`i' = nxretshort`i' if nx_short`j' == 1 & L.rank_fdfp`j' == `i'
       }
}

// To have correct statistics, we kick out the zero values in the first row, as we don't have a return yet.
forvalues i=1/`=top' {
		replace final_long`i' =. in 1	
		replace final_short`i' =. in 1	
}

// manually adjust last return, all exit (use 588 when doing full sample)
forvalues i=1/`=top'{
forvalues j = 1/`=currencies' {
       replace final_long`i' = exretlong`i' in 481 if L.rank_fdfp`j'== `i'+(L.currused-`=top') 
       replace final_short`i' = exretshort`i' in 481 if L.rank_fdfp`j' == `i' 
}
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
egen pflong = rowtotal(retlong*) if _n>1
replace pflong = 1/`=top' * pflong if _n>1

egen pfshort = rowtotal(retshort*) if _n>1
replace pfshort = 1/`=bottom' * pfshort if _n>1

// Calculate HML return and accumulative return
gen HML = pflong + pfshort if _n>1

// Reconvert to log returns
replace HML = log(1+HML)

gen sumHML = sum(HML) if _n>1

exit




**********************************************************************
*** Data analytics ***************************************************
**********************************************************************

graph set window fontface "Times New Roman"
format date %tdm/Y


**********************************************************************
*** Line graph over total cumulative return **************************
**********************************************************************

line sumHML date if _n>1 , title("Cumulative CAR returns incl. TC") xtitle("Date") ytitle("Cumulative exess returns") ///
graphregion(margin(r=5)) xlabel(, angle(horizontal)) xsize(10) ysize(6) xlabel(#6) ylabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%")


**********************************************************************
*** Maximum Drawdown *************************************************
**********************************************************************

// Percentage drawdown (log returns)

gen maxsumHML = sumHML if _n == 2
replace maxsumHML = max(L.maxsumHML, sumHML) if _n > 2

gen DD = (maxsum - sumHML)*-1
line DD date if _n>1, title("Maximum percentage drawdown") xtitle("Date") ytitle("Drawdown") ///
graphregion(margin(r=5)) lcolor(red) xlabel(, angle(horizontal)) xsize(10) ysize(6) xlabel(#6) ylabel(0 "0%" -0.1 "-10%" -0.2 "-20%" -0.3 "-30%")
sum DD, detail

**********************************************************************
*** Summary Statistics **********************************************
**********************************************************************

sum HML, detail
scalar yrlymean = period*r(mean)*100
scalar yrlysd = sqrt(period)*r(sd)*100
scalar SR = yrlymean / yrlysd
scalar yrlyskew = r(skewness)/sqrt(period)
scalar yrlykurt = (r(kurtosis)-3)/period
scalar min = r(min)*100 
scalar max = r(max)*100 

sum DD, detail
scalar maxdd = r(min)*100 

// Store statistics in a row matrix
matrix stats = (yrlymean, yrlysd, SR, yrlyskew, yrlykurt, maxdd, min, max)
matrix rownames stats = "CAR3MC"
matrix colnames stats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist stats, format(%9.2f)






