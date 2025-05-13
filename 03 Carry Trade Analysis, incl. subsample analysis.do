***********************************************
*** Carry Trade *******************************
***********************************************
clear all
cd "C:\Users\nicol\CBS - Copenhagen Business School\MasterThesis N&J - General\04 - Data and Code\03 Stata\Most current code\01 Data"
use "Data_1M_1976.dta", clear

// Generate fakedate and use as a time series 
gen fakedate = _n
order date fakedate
tsset fakedate


// Define parameters
scalar currencies = 9     // Total number of currency pairs
scalar top = 3            // Number of currencies in high portfolio
scalar bottom = 3         // Number of currencies in low portfolio
scalar period = 12        // Periods per year


// Use below restrictions for sample period analysis.  
// Full sample: no change
// 1976 to 1985 (incl)
// drop if _n>120

// 1986 to 1995 (incl)
// drop if _n<120
// drop if _n>121

// 1996 to 2005 (incl)
//drop if _n<240
//drop if _n>121

// 2006 to 2015 (incl)
//drop if _n<360
//drop if _n>121

// 2016 to 2024 (incl)
//drop if _n<480

// to only include 12/1984 to compare to 3M 6M 12M
// drop if _n<108

// Calculate logs from spot and forward rates
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

// Calculate forward discount / forward premium
forvalues i=1/`=currencies' {
	gen fdfp`i' = fwusdmid`i' - usdmid`i'  if fwusdmid`i'!=. & usdmid`i' !=.
}

***********************************************
*** Traditional Carry Trade *******************
***********************************************
// Generate the rank of the currency based on fdfp
forvalues i = 1/`=currencies' {
     gen rank_fdfp`i' = 1 if fdfp`i'!=.
    
    forvalues j = 1/`=currencies' {
        replace rank_fdfp`i' = rank_fdfp`i' +1 if fdfp`j' > fdfp`i' & fdfp`j' !=.
    }
}

// generate a help variable to find the correct number of currency pairs. currmiss shows how many of the 9 currency pairs are missing in the dataset at the given datapoint. currused shows how many currencies are consequently used in the pf building.

egen currmiss = rowmiss(fdfp1-fdfp9)
gen currused = 9-currmiss

// Manually adjust rank as we have seven datapoints which have the same fdfps. This has to be done manually for different subsample period.
// 1976 to 1985
replace rank_fdfp2 = L.rank_fdfp2 in 3
replace rank_fdfp6 = L.rank_fdfp6 in 16
replace rank_fdfp2 = L.rank_fdfp2 in 25
replace rank_fdfp1 = 5 in 66
replace rank_fdfp9 = 3 in 84
// 1986 - 1995: 
replace rank_fdfp6 = L.rank_fdfp6 in 139 // 07/31/1987 "in 139 (20) when doing full sample (subsample)"
replace rank_fdfp2 = 3 in 198 // 06/30/1992 "in 198 (79) when doing full sample (subsample)"

// Generate return portfolios
forvalues i=1/`=top' {
	gen retlong`i' = .
}

forvalues i=1/`=bottom' {
	gen retshort`i' = .
}

//Long return portfolio
forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace retlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
}
}

//Short return portfolio
forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace retshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_fdfp`i' == `j'	
	}
}

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
	replace retlong`i' = exp(retlong`i')-1
}

forvalues i=1/`=bottom' {
	replace retshort`i' = exp(retshort`i')-1
}
	

/// now combine the returns with equal weights
egen pflong = rowtotal(retlong*) if _n>1
replace pflong = 1/`=top' * pflong if _n>1

egen pfshort = rowtotal(retshort*) if _n>1
replace pfshort = 1/`=bottom' * pfshort if _n>1

// Calculate HML return and accumulative return
gen HML = pflong + pfshort if _n>1

// Reconvert to log returns
replace HML = log(1+HML)

// Calculate cumulative return over time
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

line sumHML date if _n>1 , title("Cumulative CCT returns") xtitle("Date") ytitle("Cumulative exess returns") ///
graphregion(margin(r=5)) xlabel(, angle(horizontal)) xsize(10) ysize(6) xlabel(#6) ylabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%")


**********************************************************************
*** Line Graph Maximum Drawdown **************************************
**********************************************************************

// Percentage drawdown of returns

gen maxsumHML = sumHML if _n == 2
replace maxsumHML = max(L.maxsumHML, sumHML) if _n > 2
gen DD = (maxsum - sumHML)*-1

summarize DD
local avg_dd = r(mean)
local avg_pct = string(`avg_dd'*100, "%9.1f")

line DD date if _n>1, title("Maximum percentage drawdown") xtitle("Date") ytitle("Drawdown") ///
graphregion(margin(r=5)) lcolor(red) xlabel(, angle(horizontal)) xsize(10) ysize(6) ///
xlabel(#6) ylabel(0 "0%" -0.1 "-10%" -0.2 "-20%" -0.3 "-30%" `avg_dd' "`avg_pct'%") ///
yline(`avg_dd', lpattern(dash) lcolor(gray) lwidth(normal))

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
matrix rownames stats = "CAR3MN"
matrix colnames stats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist stats, format(%9.2f)

exit


// Create a histogram 

hist HML, bins(70) normal title("Histogram Carry Trade Returns") xtitle("Return distribution") ytitle("Density")  xlabel(-0.1 "-10%" -0.05 "-5%" 0 "0" 0.05 "5%" 0.1 "10%") ylabel(0(5)30)

**********************************************************************
/// Analyse which currency is involved in most trades (long and short)
**********************************************************************

matrix LS = J(9,3,.)
matrix rownames LS = EUR GBP JPY AUD NZD CAD CHF NOK SEK 
matrix colnames LS = short long total(%)

// When the rank of currency`i' is 1,2,3, we don't go short the currency, WE GO SHORT THE USD AND LONG THE CURRENCY. Likewise, when we have rank 7,8,9, we GO LONG THE USD AND SHORT THE CURRENCY. Therefore, the rule to compute the inclusion of the currencies into either long or short must be switched. For example the Yen: when the Yen is in rank 7,8,9, it means that we go long the USD, which effectively means that we go short the Yen. Thus, to see how often the Yen was shorted, we don't look into PF1,2,3, but into PF7,8,9. Thus, the decision rule is "flipped".

// Inclusion in short portfolio
forvalues i = 1/`=currencies' {
		gen in_short`i' =  (rank_fdfp`i' == currused-`=top'+1 | rank_fdfp`i' == currused-`=top'+2 | rank_fdfp`i' == currused-`=top'+3) if _n>1
}


// Inclusion in long portfolio
forvalues i = 1/`=currencies' {
        gen in_long`i' = (rank_fdfp`i' == 1| rank_fdfp`i' == 2 | rank_fdfp`i' == 3) if _n>1
}


	forvalues i = 1/9 {
        quietly count if in_short`i' == 1
		matrix LS[`i',1] = r(N)
		quietly count if in_long`i' == 1
		matrix LS[`i',2] = r(N)
		quietly count if in_short`i' == 1 | in_long`i' == 1
		matrix LS[`i',3] = r(N)/587*100
}


matlist LS, format(%9.0f %9.1f %9.4f) 


**********************************************************************
/// Long PF vs Short PF Analysis *************************************
**********************************************************************

// Generate returns over time for both portfolio legs
gen sumlong = sum(pflong) if _n>1
gen sumshort = sum(pfshort) if _n>1
sum pflong pfshort

// Generate chart of long and short leg in carry trade
graph set window fontface "Times New Roman"
format date %tdm/Y
line sumHML sumshort sumlong  date if _n>1, ///
    title("Long and short contribution to total carry trade return") ///
    xtitle("Date") ///
    ytitle("Cumulative excess returns") ///
    graphregion(margin(r=5)) ///
    xlabel(, angle(horizontal)) ///
    xsize(10) ///
    ysize(6) ///
    xlabel(#6) ///
    ylabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%") ///
    legend(pos(11) ring(0) col(1) textfirst) ///
    legend(label(1 "Combined portfolio") ///
           label(2 "Short portfolio") ///
           label(3 "Long portfolio") ///
           )
	
// Generate summary statistics of long PF

gen maxsumlong = sumlong if _n == 2
replace maxsumlong = max(L.maxsumlong, sumlong) if _n > 2
gen longDD = (maxsumlong - sumlong)*-1

sum pflong, detail
scalar yrlymean = period*r(mean)*100
scalar yrlysd = sqrt(period)*r(sd)*100
scalar SR = yrlymean / yrlysd
scalar yrlyskew = r(skewness)/sqrt(period)
scalar yrlykurt = (r(kurtosis)-3)/period
scalar min = r(min)*100 
scalar max = r(max)*100 

sum longDD, detail
scalar maxddlong = r(min)*100 

// Store statistics in a row matrix
matrix pflong = (yrlymean, yrlysd, SR, yrlyskew, yrlykurt, maxddlong, min, max)
matrix rownames pflong = "Long only"
matrix colnames pflong = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist pflong, format(%9.2f)


// Generate summary statistics of short PF

gen maxsumshort = sumlong if _n == 2
replace maxsumshort = max(L.maxsumshort, sumshort) if _n > 2
gen shortDD = (maxsumshort - sumshort)*-1

sum pfshort, detail
scalar yrlymean = period*r(mean)*100
scalar yrlysd = sqrt(period)*r(sd)*100
scalar SR = yrlymean / yrlysd
scalar yrlyskew = r(skewness)/sqrt(period)
scalar yrlykurt = (r(kurtosis)-3)/period
scalar min = r(min)*100 
scalar max = r(max)*100 

sum shortDD, detail
scalar maxddshort = r(min)*100 

// Store statistics in a row matrix
matrix pfshort = (yrlymean, yrlysd, SR, yrlyskew, yrlykurt, maxddshort, min, max)
matrix rownames pfshort = "Short^ only"
matrix colnames pfshort = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist pfshort, format(%9.2f)


// Generate scatter plot of long and short leg in carry trade

scatter pflong pfshort, ///
    title("Long and short return observations") ///
    xtitle("Return of long portfolio") ///
    ytitle("Return of short portfolio") ///
	xlabel(-0.15 "-15%" -0.1 "-10%" -0.05 "-5%" 0 "0%" 0.05 "5%" 0.1 "10%" 0.15 "15%") ///
    ylabel(-0.15 "-15%" -0.1 "-10%" -0.05 "-5%" 0 "0%" 0.05 "5%" 0.1 "10%" 0.15 "15%") ///
    xline(0, lcolor(gray)) ///
    yline(0, lcolor(gray)) 


**********************************************************************
*** Available currencies over time ***********************************
**********************************************************************

// Generate code for the currency availability over time
gen currnumber = currused+1
line currnumber date, title("G10 currencies included over time") ytitle("Number of currencies") xtitle("Date") graphregion(margin(r=5)) xlabel(, angle(horizontal)) xsize(10) ysize(6) xlabel(#6)
