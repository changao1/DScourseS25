use "DSFinal_Gao.dta", clear
xtset city_id date_index

* construct purchasing restrictions events
local event_dummies event_k_12 event_k_11 event_k_10 event_k_9 event_k_8 ///
    event_k_7 event_k_6 event_k_5 event_k_4 event_k_3 event_k_2 event_k_1 ///
    event_k0 event_k1 event_k2 event_k3 event_k4 event_k5 event_k6 event_k7 ///
    event_k8 event_k9 event_k10 event_k11 event_k12 event_k13 event_k14 ///
    event_k15 event_k16 event_k17 event_k18 event_k19 event_k20 event_k21 ///
    event_k22 event_k23 event_k24

* OLS without IV
reghdfe ln_price_index ///
    Employment_Rate ln_GDP_Per_Capita_Yuan ln_Urban_Pension_Insurance_Parti ///
    `event_dummies', ///
    absorb(date_index) cluster(city_id)

* 2SLS
ivreghdfe ln_price_index ///
    (Employment_Rate = avg_natural_growth_90s_final) ///
    ln_GDP_Per_Capita_Yuan ln_Urban_Pension_Insurance_Parti ///
    `event_dummies', ///
    cluster(city_id)
