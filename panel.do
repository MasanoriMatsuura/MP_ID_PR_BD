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
cd "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\Do"
//use panel.dta, clear 
/*creating a panel dataset*/
use 2015.dta, clear
append using 2012.dta, force
append using 2018.dta, force
drop if dvcode==.


/*some cleaning*/
gen lnfrm=log(farmsize) // logarithem of farm size 100 decimal = 0.4 ha
label var lnfrm "Farmsize (log)"
recode year (2012=1)(nonm=0), gen(year2012) //year dummy
recode year (2015=1)(nonm=0), gen(year2015)
label var year2012 "Year 2012"
label var year2015 "Year 2015"
recode dvcode (55=1)(nonm=0), gen(Rangpur) //division dummy
/*label var Rangpur "Rangpur division (dummy)"
label var ttinc "Total yearly income (taka)"
gen ttinc10000=ttinc/10000
label var ttinc10000 "Total yearly income (10,000taka)"
gen lninc=log(ttinc)
label var lninc "Total yearly income (log)"*/
label var ln_rinsd "Monthly st.dev rainfall (log)"
label var frmdiv "Farm diversification (Num of species of crop, livestocks, and fish)" 
label var frm_div "Farm diversification index"
/*label var rinsd "Monthly st.dev rainfall(mm)"
label var tmpsd "Monthly st.dev temperature(\textdegree{}C)"*/
label var rw "Winter rainfall(mm)"
label var rs "Summer rainfall(mm)" 
label var rr "Rainy season rainfall(mm)"
label var ra "Autumn rainfall(mm)"
label var tw "Winter average temperature(\textdegree{}C)"
label var ts "Summer average temperature(\textdegree{}C)"
label var tr "Rainy season average temperature(\textdegree{}C)"
label var ta "Autumn season average temperature(\textdegree{}C)"
//rw rs rr ra tmpsd tw ts tr ta rinsd
replace frm_div=. if frm_div==1
replace inc_div=. if inc_div==1
replace shnf=. if frm_div==1
replace shni=. if inc_div==1

*create peer effect variables
/*sort uncode year
by uncode year: egen adaptation_n=count(a01) if frmdiv>1
by uncode year: egen total_n=count(a01)
gen preff_frmdiv=(adaptation_n-1)/total_n //creating peer effect*/
recode frm_div (0=0)(nonm=1), gen(frm_div_i)
recode inc_div (0=0)(nonm=1), gen(inc_div_i)
recode shnf (0=0)(nonm=1), gen(shnf_i)
recode shni (0=0)(nonm=1), gen(shni_i)

sort uncode year
by uncode year: egen adaptation_nf=sum(frm_div_i) 
by uncode year: egen total_nf=count(a01)
gen preff_frm_div=(adaptation_nf-frm_div_i)/(total_nf) //creating peer effect
sort uncode year
/*by uncode year: egen adaptation_nc=count(a01) if crp_div>0
by uncode year: egen total_nc=count(a01)
gen preff_crpdiv=(adaptation_nc-1)/total_nc //creating peer effect
sort uncode year*/
by uncode year: egen adaptation_ni=sum(inc_div_i) 
by uncode year: egen total_ni=count(a01)
gen preff_incdiv=(adaptation_ni-inc_div_i)/(total_ni) //creating peer effect
sort uncode year
by uncode year: egen adaptation_nshf=sum(shnf_i)
by uncode year: egen total_nshf=count(a01)
gen preff_shf=(adaptation_nshf-shni_i)/(total_nshf) //creating peer effect shannon farm
sort uncode year
by uncode year: egen adaptation_nshi=sum(shni_i)
by uncode year: egen total_nshi=count(a01)
gen preff_shi=(adaptation_nshi-shni_i)/(total_nshi) //creating peer effect shannon income

/*label var preff_crpdiv "share of crop diversification household within the union"*/
/*label var preff_frmdiv "share of farm diversification household within the union"*/
label var preff_frm_div "share of households adopting diversification within the union"
label var preff_incdiv "share of households adopting diversification within the union"


*create log hdds and expenditure
gen lnhdds=log(hdds)
gen lnexp=log(pc_expm_d)
gen lnfexp=log(pc_foodxm_d)

*shannon index
label var shnf "Farm diversificaion (Shannon)"
label var shni "Income diversification (Shannon)"

* poverty line


/*label market participation variable
label var marketp "Market participation (=1 if yes)"*/
save panel.dta, replace

export delimited using panel.csv, replace //output as csv



** dependent variable by regional level
collapse (mean) divfexp=pc_foodxm_d divhdds=hdds, by(dcode)
save dependent.dta, replace
**Visualization

label var lnfexp "Per capita food consumption expenditure(log)"
graph twoway (scatter hdds farmsize , msymbol(circle_hollow) yaxis(1) ytitle("HDDS", axis(1))) (scatter lnfexp farmsize, msymbol(triangle_hollow) yaxis(2) ytitle("Per capita food consumption expenditure(log)", axis(2))), xtitle("Farmland size (decimal)")  title("Household food security over the scale of farmers")  note(Source: "BIHS2011/12, 2015, and 2018/19 calculated by author")
graph display, scheme(s1mono) 
graph export $figure\fsecurity_farm.png, replace

