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

* Endogenous Switching Regression
ssc install movestay

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

save panel.dta, replace
export delimited using panel.csv, replace //output as csv

*correlation among mobile phone and poverty line
eststo clear
sort year
by year: eststo: quietly estpost correlate  mobile hdds pcexp_da p190hcgcpi p190hcfcpi p320hcfcpi deppov190gcpi deppov190fcpi deppov320gcpi deppov320fcpi hc_mpi mpiscore
esttab  using $table\corr_mobile_hw.rtf, label replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)

eststo clear
sort year
by year: eststo: quietly estpost correlate mobile inc_div offfarm ttinc pcti nonself nonwage
esttab  using $table\corr_inc_mobile.rtf, label replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)

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

*Correlation IV and mobile phone ownership
eststo clear
sort year
by year: eststo: quietly estpost correlate bazaar mobile mobile_q  p190hcgcpi p190hcfcpi p320hcfcpi deppov190gcpi deppov190fcpi deppov320gcpi deppov320fcpi hc_mpi mpiscore inc_div offfarm ttinc pcti nonself nonwage
esttab  using $table\bazaar_mobile.rtf, label replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)
eststo clear
sort year
by year: eststo: quietly estpost correlate shop mobile mobile_q  p190hcgcpi p190hcfcpi p320hcfcpi deppov190gcpi deppov190fcpi deppov320gcpi deppov320fcpi hc_mpi mpiscore inc_div offfarm ttinc pcti nonself nonwage
esttab  using $table\shop_mobile.rtf, label replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)

*the effect of mobile phone total income, off-farm income, poverty headcount, poverty gap, MPI score, MPI
**probit model 1st stage, adoption of mobile phone
xtset a01 year
xtlogit mobile bazaar srshock rrshock arshock wrshock ln_sds ln_sdr ln_sda ln_sdw stshock rtshock atshock wtshock ln_sdst ln_sdrt ln_sdat ln_sdwt Male age_hh hh_size schll_hh lvstck lnfrm market road irrigation extension year2012 year2015,


**linear probability model 1st stage, adoption of mobile phone and determinants of off-farm employment/self employment
reghdfe mobile bazaar srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm bazaar road irrigation extension year2012 year2015, a(a01) vce(robust)

reghdfe offfarm srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015, a(a01) vce(robust)

reghdfe nonfarmself bazaar srshock rrshock arshock wrshock  stshock rtshock atshock wtshock Male age_hh hh_size schll_hh lvstck lnfrm bazaar road irrigation extension year2012 year2015, a(a01) vce(robust)
**second stage the impact of mobile phone on off-farm employment, poverty index
xtprobit hc_mpi mobile_q bazaar srshock rrshock arshock wrshock ln_sds ln_sdr ln_sda ln_sdw stshock rtshock atshock wtshock ln_sdst ln_sdrt ln_sdat ln_sdwt Male age_hh hh_size schll_hh lvstck lnfrm road extension market irrigation year2012 year2015 , vce(robust)

** 2SLS IV probit
reghdfe ln_ttlinc srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 (mobile=mobile_village), a(01) vce(r)

ivprobit offfarm srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 (mobile=mobile_village),vce(r)

ivprobit ln_pctinc srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 (mobile=mobile_village),vce(r)


ivprobit mpiscore srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 (mobile=mobile_village),vce(r)

ivprobit hc_mpi srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 (mobile=mobile_village),vce(r)

ivprobit p190hcgcpi srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 (mobile=mobile_village),vce(r)


** endogenous switching regression
movestay hc_mpi srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015, select(mobile=mobile_village) vce(robust)

**Validity of IV
eststo clear

eststo: xtprobit mobile mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015, vce(robust)
quietly estadd local fe Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: xtprobit offfarm mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015, vce(r)
quietly estadd local fe Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpiscore mobile_village srshock rrshock arshock wrshock stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015,a(a01) vce(r)
quietly estadd local fe Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_ttlinc mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015,a(a01) vce(r)
quietly estadd local fe Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe ln_pctinc mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015,a(a01) vce(robust)
quietly estadd local fe Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

xtprobit hc_mpi mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 , vce(robust)

xtprobit p190hcgcpi mobile_village srshock rrshock arshock wrshock  stshock rtshock atshock wtshock  Male age_hh hh_size schll_hh lvstck lnfrm road bazaar irrigation extension year2012 year2015 , vce(robust)

esttab  using $table\falsification_iv.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. (1) and (2): Linear probability model. (3), (4), and (5): OLS fixed effect model.) keep(mobile_village) s(fe year control N, label("Individual FE" "Year FE" "Control variables" "Observations")) mtitles("Adoption of mobile phone (1/0), LPM" "Off-farm employment (1/0), LPM" "MPI score, FE" "Total household income (log), FE" "Per capita total income (log)")

*Inequality visualization, Lorenz Curve
lorenz ttinc, over(mobile)
lorenz graph, overlay aspectratio(1) xlabel(, grid) scheme(s1mono) title(Lorenz cureve of household total income) addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. Calculated by authors)
graph export $graph/lorenz_totalincome.jpg, replace

lorenz pcti, over(mobile)
lorenz graph, overlay aspectratio(1) xlabel(, grid) scheme(s1mono) title(Lorenz cureve of per capita total income) addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. Calculated by authors)
graph export $graph/lorenz_pcti.jpg, replace 

lorenz nonwage, over(mobile)
lorenz graph, overlay aspectratio(1) xlabel(, grid) scheme(s1mono) title(Lorenz cureve of off-farm wage and employment) addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19. Calculated by authors)
graph export $graph/lorenz_nonwage.jpg, replace

*Robustness check