 /*Mobile Phone and Household Welfare: Analysis*/

/*Author: Masanori Matsuura*/
clear all
set more off
* Install reghdfe
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")
* Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")
* Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2

*install quantile regression
ssc install xtqreg

* Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from(https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/)

* depict lorenz curve
ssc install lorenz

* Propensity score matching
ssc install psmatch2

*Coefficient plot
ssc install coefplot

*gini decomposition
ssc install descogini

*set the pathes
global climate = "C:\Users\user\Documents\Masterthesis\climatebang"
global BIHS18Community = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2018\dataverse_files\BIHSRound3\Community"
global BIHS18Female = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2018\dataverse_files\BIHSRound3\Female"
global BIHS18Male = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2018\dataverse_files\BIHSRound3\Male"
global BIHS15 = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2015"
global BIHS12 = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2012"
global table = "C:\Users\user\Documents\research\saiful\mobile_phone\table"
global graph = "C:\Users\user\Documents\research\saiful\mobile_phone\graph"
cd "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\Do"

/*creating a panel dataset*/
use 2015.dta, clear
append using 2012.dta, force
append using 2018.dta, force
drop if dcode==.


/*some cleaning*/
gen lnfrm=log(farmsize) // logarithem of farm size 100 decimal = 0.4 ha
label var lnfrm "Farmsize (log)"
recode year (2012=1)(nonm=0), gen(year2012) //year dummy
recode year (2015=1)(nonm=0), gen(year2015)
label var year2012 "Year 2012"
label var year2015 "Year 2015"
recode dvcode (55=1)(nonm=0), gen(Rangpur) //division dummy
label var aginc "Farm self"
label var frmwage "Farm wage"
label var nonself "Off-farm self"
label var nonwage "Off-farm wage and salary"
label var nonearn "Non-earned"
label var ttinc "Total household income"
label var sdst "20-year summer temperature (SD)"
label var sdrt "20-year rainy season temperature (SD)"
label var sdat "20-year autumn temperature (SD)"
label var sdwt "20-year winter temperature (SD)"
label var sds "20-year summer rainfall (SD)"
label var sdr "20-year rainy season rainfall (SD)"
label var sdw "20-year winter rainfall (SD)"
label var sda "20-year autumn rainfall (SD)"
label var hdds "Household Dietary Diversity Score"
label var asset "Asset index"
replace crp_div=. if crp_div==1
replace inc_div=. if inc_div==1
replace shnc=. if crp_div==1
replace shni=. if inc_div==1

*mobile ownership
label var mobile "Mobile phone (dummy)"

*create peer effect variables
sort Village year
by Village year: egen mobile_nc=sum(mobile) 
by Village year: egen total_nc=count(a01)
gen mobile_village=(mobile_nc-mobile)/(total_nc) //creating peer effect

label var mobile_village "Share of households adopting mobile phone in the village"

*create log hdds and expenditure
gen lnhdds=log(hdds)
gen lnexp=log(pc_expm_d)
gen lnfexp=log(pc_foodxm_d)

*shannon index
label var shni "Income diversification (Shannon)"

*off-farm emlpoyment/self employment (dummy)
recode nonwage (0=0 "No")(nonm=1 "Yes"), gen("offfarm")
label var offfarm "Off-farm employment (dummy)"
recode nonself (0=0 "No")(nonm=1 "Yes"), gen("nonfarmself")
label var nonfarmself "Off-farm self employment (dummy)"
*per capita total income
gen pcti=ttinc/hh_size
label var pcti "Per capita total income"

*log total income, per capita total income
gen ln_ttlinc=log(ttinc+1)
gen ln_pctinc=log(pcti+1)
label var ln_ttlinc "Total household income (log)"
label var ln_pctinc "Per capita total income (log)"

gen povertyhead=p190hcgcpi/100
label var povertyhead "Povery headcount (1/0)"
recode Male (1=0 "no")(0=1 "yes"), gen(female)
label var female "Female household head"

save panel.dta, replace
export delimited using panel.csv, replace //output as csv


*Descriptive statistics
bysort year a01: egen treat=sum(mobile)

sort year mobile
by year mobile:  summarize offfarm povertyhead deppov190gcpi mpiscore ttinc pcti mobile mobile_village hs hr ha hw s r a w hst hrt hat hwt ts tr ta tw female age_hh hh_size schll_hh asset farmsize bazaar road if mobile !=.

by year:  summarize offfarm povertyhead deppov190gcpi mpiscore ttinc pcti mobile mobile_village hs hr ha hw s r a w hst hrt hat hwt ts tr ta tw female age_hh hh_size schll_hh asset farmsize bazaar road if mobile !=.

ttest offfarm if year==2012, by(mobile)
ttest povertyhead if  year==2012, by(mobile)
ttest deppov190gcpi if  year==2012, by(mobile)
ttest mpiscore if  year==2012, by(mobile)
ttest ttinc if  year==2012, by(mobile)
ttest pcti if  year==2012, by(mobile)

ttest female if  year==2012, by(mobile)
ttest age_hh if  year==2012, by(mobile)
ttest hh_size if  year==2012, by(mobile)
ttest schll_hh if  year==2012, by(mobile)
ttest asset if  year==2012, by(mobile)
ttest farmsize if  year==2012, by(mobile)
ttest bazaar if  year==2012, by(mobile)
ttest road if  year==2012, by(mobile)


ttest offfarm if  year==2015, by(mobile)
ttest povertyhead if  year==2015, by(mobile)
ttest deppov190gcpi if  year==2015, by(mobile)
ttest mpiscore if  year==2015, by(mobile)
ttest ttinc if  year==2015, by(mobile)
ttest pcti if  year==2015, by(mobile)
ttest female if  year==2015, by(mobile)
ttest age_hh if  year==2015, by(mobile)
ttest hh_size if  year==2015, by(mobile)
ttest schll_hh if  year==2015, by(mobile)
ttest asset if  year==2015, by(mobile)
ttest farmsize if  year==2015, by(mobile)
ttest bazaar if  year==2015, by(mobile)
ttest road if  year==2015, by(mobile)

ttest offfarm if  year==2018, by(mobile)
ttest povertyhead if  year==2018, by(mobile)
ttest deppov190gcpi if  year==2018, by(mobile)
ttest mpiscore if  year==2018, by(mobile)
ttest ttinc if  year==2018, by(mobile)
ttest pcti if  year==2018, by(mobile)
ttest female if  year==2018, by(mobile)
ttest age_hh if  year==2018, by(mobile)
ttest hh_size if  year==2018, by(mobile)
ttest schll_hh if  year==2018, by(mobile)
ttest asset if  year==2018, by(mobile)
ttest farmsize if  year==2018, by(mobile)
ttest bazaar if  year==2018, by(mobile)
ttest road if  year==2018, by(mobile)

//povertyhead deppov190gcpi mpiscore ttinc pcti mobile_village hs hr ha hw s r a w hst hrt hat hwt ts tr ta tw Male age_hh hh_size schll_hh asset lnfrm bazaar road irrigation extension 
*mobile phone ownership overtime
graph bar mobile, over(year) ytitle("Mobile phone ownership") title("mobile phone ownership from 2011 to 2019") note("Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19") scheme(s1mono)
graph export $graph/phone_overtime.jpg, replace

*numbe of households in the sample using and not using mobile phones
eststo clear
sort year
by year: eststo: quietly estpost tab mobile
esttab  using $table\mobileowner_year.rtf, label nodepvar replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)

*composition of income
graph pie aginc frmwage nonself nonwage nonearn if year==2012, plabel(_all percent, color(white)) subtitle("2011/12") saving(pie12) 
graph pie aginc frmwage nonself nonwage nonearn if year==2015,  plabel(_all percent, color(white)) saving(pie15) subtitle("2015")
graph pie aginc frmwage nonself nonwage nonearn if year==2018,  plabel(_all percent, color(white)) saving(pie18) subtitle("2018/19")
gr combine pie12.gph pie15.gph pie18.gph, title("Breakdown of household income by source") note(Source: "BIHS2011/12, 2015, and 2018/19 calculated by author") 
graph display, scheme(s1mono) 
graph export $figure\income_dist.png, replace


*the effect of mobile phone total income, off-farm income, poverty headcount, poverty gap, MPI score, MPI
**probit model 1st stage, adoption of mobile phone
*RE
xtset a01 year
eststo clear
eststo: xtprobit mobile mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015, vce(robust)
quietly estadd local MK No, replace
quietly estadd local year Yes, replace

**Mundlak Estimator CRE
sort a01
by a01: egen m1=mean(female) 
by a01: egen m2=mean(age_hh )
by a01: egen m3=mean(hh_size)
by a01: egen m4=mean(schll_hh)
by a01: egen m5=mean(asset)
by a01: egen m6=mean(road)
by a01: egen m7=mean(bazaar)
by a01: egen m8=mean(irrigation)
by a01: egen m9=mean(extension)

eststo: xtprobit mobile mobile_villag srshock rrshock arshock wrshock  stshock rtshock atshock wtshock femal age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 m1 m2 m3 m4 m5 m6 m7 m9, vce(robust) 
predict xb
gen residualm=mobile-xb
drop xb 
quietly estadd local MK Yes, replace
quietly estadd local year Yes, replace

esttab  using $table\1ststage.rtf, b(%4.3f) se replace label wide nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. Estimated by random effect probit model) keep(mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension) s(MK year N, label("Covariate mean values" "Year dummy" "Observations")) mtitles("RE probit" "MK probit") star(* 0.10 ** 0.05 *** 0.01)


**second stage the impact of mobile phone on off-farm employment, poverty, and income
eststo clear
xtprobit offfarm mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm m1 m2 m3 m4 m5 m6 m7 m9, vce(robust)
eststo: margins, dydx(mobile residualm)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

xtprobit povertyhead mobile srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm m1 m2 m3 m4 m5 m6 m7 m9, vce(robust)
eststo: margins, dydx(mobile residualm)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace
 

xttobit deppov190gcpi mobile srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm m1 m2 m3 m4 m5 m6 m7 m9, ll(0) 
eststo: margins, dydx(mobile residualm)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpiscore mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road extension bazaar year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_ttlinc mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_pctinc mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

label var residualm "Residual-mobile"
esttab  using $table\2ndstage.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19.) keep(mobile residualm) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Off-farm employment" "Poverty Headcount" "Depth of poverty" "MPI score" "Total household income (log)" "Per capita total income (log)")star(* 0.10 ** 0.05 *** 0.01)

esttab  using $table\2ndstage_full.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. (1) and (3): Estimated by RE probit model. (2) and (4): Estimated by FE LPM, (5), (6), (7) and (8): Estimated by OLS FE.) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Off-farm employment" "Off-farm employment " "Poverty Headcount " "Poverty Headcount" "Depth of poverty" "MPI score" "Total household income (log)" "Per capita total income (log)")star(* 0.10 ** 0.05 *** 0.01)

* Coefficient plot
eststo clear
reghdfe offfarm mobile srshock rrshock arshock wrshock  stshock rtshock atshock wtshock Male age_hh hh_size schll_hh asset lnfrm bazaar road irrigation extension year2012 year2015 residualm,a(a01) vce(robust)
estimates store A

reghdfe povertyhead mobile srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh asset lnfrm bazaar road irrigation extension year2012 year2015 residualm, a(a01)vce(robust)
estimates store B


reghdfe deppov190gcpi mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh asset lnfrm road bazaar irrigation extension year2012 year2015 residualm, absorb(a01) vce(r)
estimates store C


reghdfe mpiscore mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh asset lnfrm road bazaar irrigation extension year2012 year2015 residualm, absorb(a01) vce(r)
estimates store D


reghdfe ln_ttlinc mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh asset lnfrm road bazaar irrigation extension year2012 year2015 residualm, absorb(a01) vce(r)
estimates store E


reghdfe ln_pctinc mobile srshock rrshock arshock wrshock stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh asset lnfrm road bazaar irrigation extension year2012 year2015 residualm, absorb(a01) vce(r)
estimates store F

coefplot (A, label(Off-farm employment) msymbol(circle)) (B,label(Poverty headcount(1/0)) msymbol(diamond)) (C,label(Poverty depth) msymbol(triangle)) (D, label(MPI score) msymbol(square)) (E, label(Total household income (log)) msymbol(plus)) (F,label(per capita total income (log)) msymbol(X)), drop(srshock rrshock arshock wrshock stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh asset lnfrm road bazaar irrigation extension year2012 year2015 residualm _cons) xline(0) scheme(s1mono) title("Impact of mobile phone ownership") note(Source: "BIHS2011/12, 2015, and 2018/19 calculated by author")

graph export $graph/impact_coef.jpg, replace

drop residualm


**Validity of IV
eststo clear
xtset a01 year
eststo: probit mobile mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015, vce(robust)
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: probit offfarm mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 if mobile==0, vce(r)
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: probit povertyhead mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension  year2012 year2015 if mobile==0, vce(robust)
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe deppov190gcpi mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 if mobile==0, vce(robust)
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpiscore mobile_village srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 if mobile==0,vce(r)
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_ttlinc mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 if mobile==0, vce(r)
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_pctinc mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 if mobile==0, vce(robust)
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

ivreghdfe ln_pctinc srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 (mobile=mobile_village), a(a01) vce(robust)

esttab  using $table\falsification_iv.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. (1), (2), and (3): Estimated by probit model. (4), (5), and (6): Estimated by OLS.) keep(mobile_village) s(year control N, label("Year dummy" "Control variables" "Observations")) mtitles("Mobile phone ownership (1/0)" "Off-farm employment (1/0)" "Headcount poverty (1/0)" "Depth of poverty" "MPI score" "Total household income (log)" "Per capita total income (log)")star(* 0.10 ** 0.05 *** 0.01)

*heterogeneous analysis
** gender

gen fmlmbl=female*mobile
gen fmlvllg=mobile_village*female

reghdfe fmlmbl fmlvllg srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015, a(a01)vce(robust) 
predict xb
gen residualfmlmbl=fmlmbl-xb
drop xb 

eststo clear
xtprobit offfarm mobile fmlmbl residualfmlmbl srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm  m1 m2 m3 m4 m5 m6 m7 m9, vce(robust)
eststo: margins, dydx(mobile fmlmbl)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

xtprobit povertyhead mobile fmlmbl residualfmlmbl srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm m1 m2 m3 m4 m5 m6 m7 m9, vce(robust)
eststo: margins, dydx(mobile fmlmbl)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace
 

xttobit deppov190gcpi mobile fmlmbl residualfmlmbl srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm m1 m2 m3 m4 m5 m6 m7 m9, ll(0) 
eststo: margins, dydx(mobile fmlmbl)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpiscore mobile fmlmbl residualfmlmbl srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road extension bazaar year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_ttlinc mobile fmlmbl residualfmlmbl srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_pctinc mobile fmlmbl residualfmlmbl srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

label var residualm "Residual-mobile"
esttab  using $table\heterogender.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19.) keep(mobile residualm) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Off-farm employment" "Poverty Headcount" "Depth of poverty" "MPI score" "Total household income (log)" "Per capita total income (log)")star(* 0.10 ** 0.05 *** 0.01)

** farm size
gen frmmbl=farmsize*mobile
gen frmvllg=mobile_village*farmsize

reghdfe frmmbl frmvllg srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015, a(a01)vce(robust) 
predict xb
gen residualfrmmbl=frmmbl-xb
drop xb 

eststo clear
xtprobit offfarm mobile frmmbl residualfrmmbl srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm  m1 m2 m3 m4 m5 m6 m7 m9, vce(robust)
eststo: margins, dydx(mobile frmmbl)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

xtprobit povertyhead mobile frmmbl residualfrmmbl srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015 residualm m1 m2 m3 m4 m5 m6 m7 m9, vce(robust)
eststo: margins, dydx(mobile frmmbl)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace
 

xttobit deppov190gcpi mobile frmmbl residualfrmmbl srshock rrshock arshock wrshock  stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm bazaar road extension year2012 year2015  residualm m1 m2 m3 m4 m5 m6 m7 m9, ll(0) 
eststo: margins, dydx(mobile frmmbl)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpiscore mobile frmmbl residualfrmmbl srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road extension bazaar year2012 year2015  residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_ttlinc mobile frmmbl residualfrmmbl srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_pctinc mobile frmmbl residualfrmmbl srshock rrshock arshock wrshock stshock rtshock atshock wtshock female age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 residualm, absorb(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace 

esttab  using $table\heterofarm.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19.) keep(mobile frmmbl) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Off-farm employment" "Poverty Headcount" "Depth of poverty" "MPI score" "Total household income (log)" "Per capita total income (log)")star(* 0.10 ** 0.05 *** 0.01)

*Robustness check
*PSM DID
use panel, clear
sort a01
by a01: egen m1=mean(Male) 
by a01: egen m2=mean(age_hh )
by a01: egen m3=mean(hh_size)
by a01: egen m4=mean(schll_hh)
by a01: egen m5=mean(asset)
by a01: egen m6=mean(road)
by a01: egen m7=mean(bazaar)
by a01: egen m8=mean(irrigation)
by a01: egen m9=mean(extension)

psmatch2 mobile mobile_village srshock rrshock arshock wrshock stshock rtshock atshock wtshock Male age_hh hh_size schll_hh asset lnfrm road bazaar extension year2012 year2015 m1 m2 m3 m4 m5 m6 m7 m9, out(offfarm) norepl n(1) com caliper(0.05) 

bysort a01: egen matched=sum(_support)

twoway (kdensity _pscore if mobile==1, color(emerald))(kdensity _pscore if mobile==0), ytitle(Density) xtitle(Propensity score) legend(label(1 "Ownership") label(2 "Non-ownership")) saving(pre) scheme(s1mono) 

twoway (kdensity _pscore if mobile==1 & _support==1, color(emerald))(kdensity _pscore if mobile==0 & _support==1), ytitle(Density) xtitle(Propensity score) legend(label(1 "Ownership") label(2 "Non-ownership")) saving(post) scheme(s1mono) 


graph combine pre.gph post.gph
graph export $graph/psm_density.jpg, replace


*Inequality visualization, Lorenz Curve
lorenz ttinc if _support==1, over(mobile)
lorenz graph, overlay aspectratio(1) xlabel(, grid) scheme(s1mono) title(Household total income) label( "Non-ownership" "MP ownership" ) //addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. Calculated by authors)
graph export $graph/lorenz_totalincome.jpg, replace

lorenz nonwage if _support==1 & nonwage>0, over(mobile)
lorenz graph, overlay aspectratio(1) xlabel(, grid) scheme(s1mono) title(Off-farm employment wage and salary) label( "Non-ownership" "MP ownership" ) 
graph export $graph/lorenz_nonwage.jpg, replace


graph combine pcti.gph ttinc.gph ofew.gph ,scheme(s1mono)
graph export $graph/lorenz_ineq.jpg, replace


*Gini
eststo clear
eststo: descogini ttinc aginc frmwage nonself nonwage nonearn if _support==1, d(3)
eststo: descogini ttinc aginc frmwage nonself nonwage nonearn if _support==1 & mobile==1, d(3)
eststo: descogini ttinc aginc frmwage nonself nonwage nonearn if _support==1 & mobile==0, d(3)


esttab  using $table\gini.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19.) 
