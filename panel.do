/*empirical analysis*/
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

ssc install ranktest

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
label var nonself "Non-farm self"
label var nonwage "Non-farm wage and salary"
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
      
replace crp_div=. if crp_div==1
replace inc_div=. if inc_div==1
replace shnc=. if crp_div==1
replace shni=. if inc_div==1

*create peer effect variables
recode crp_div (0=0)(nonm=1), gen(crp_div_i)
recode inc_div (0=0)(nonm=1), gen(inc_div_i)
recode shnc (0=0)(nonm=1), gen(shnc_i)
recode shni (0=0)(nonm=1), gen(shni_i)
sort uncode year
by uncode year: egen adaptation_nc=sum(crp_div_i) 
by uncode year: egen total_nc=count(a01)
gen preff_crp_div=(adaptation_nc-crp_div_i)/(total_nc) //creating peer effect
sort uncode year
by uncode year: egen adaptation_ni=sum(inc_div_i) 
by uncode year: egen total_ni=count(a01)
gen preff_incdiv=(adaptation_ni-inc_div_i)/(total_ni) //creating peer effect
sort uncode year
by uncode year: egen adaptation_nshc=sum(shnc_i)
by uncode year: egen total_nshc=count(a01)
gen preff_shc=(adaptation_nshc-shnc_i)/(total_nshc) //creating peer effect shannon crop
sort uncode year
by uncode year: egen adaptation_nshi=sum(shni_i)
by uncode year: egen total_nshi=count(a01)
gen preff_shi=(adaptation_nshi-shni_i)/(total_nshi) //creating peer effect shannon income
label var preff_crp_div "share of households adopting crop diversification within the union"
label var preff_incdiv "share of households adopting income diversification within the union"

*create log hdds and expenditure
gen lnhdds=log(hdds)
gen lnexp=log(pc_expm_d)
gen lnfexp=log(pc_foodxm_d)

*shannon index
label var shnc "Crop diversificaion (Shannon)"
label var shni "Income diversification (Shannon)"

*mobile ownership
label var mobile "Mobile phone (dummy)"

*off-farm emlpoyment (dummy)
recode nonwage (0=0 "no")(nonm=1 "yes"), gen("offfarm")
label var offfarm "Off-farm employment (dummy)"

*per capita total income
gen pcti=ttinc/hh_size
label var pcti "Per capita total income"

save panel.dta, replace

export delimited using panel.csv, replace //output as csv

*correlation among mobile phone and poverty line
eststo clear
sort year
by year: eststo: quietly estpost correlate  mobile hdds pcexp_da p190hcgcpi p190hcfcpi p320hcfcpi deppov190gcpi deppov190fcpi deppov320gcpi deppov320fcpi hc_mpi mpiscore
esttab  using $table\corr_mobile_hw.rtf, label replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)

eststo clear
sort year
by year: eststo: quietly estpost correlate mobile inc_div offfarm ttinc pcti
esttab  using $table\corr_inc_mobile.rtf, label replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)

*mobile phone ownership overtime
graph bar mobile, over(year) ytitle("Mobile phone ownership") title("mobile phone ownership from 2011 to 2019") note("Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19") scheme(s1mono)
graph export $graph/phone_overtime.jpg, replace