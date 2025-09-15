cd "/Users/ChangGao/house-price-analysis/final-paper"
use "event_data_final_iv_with_pension.dta", clear

* Step 1: 定义面板数据结构
gen ym = ym(year, month)
format ym %tm
*gen date_index = ym - ym(2010,1) + 1
xtset city_id date_index

* Step 2: 创建 event_time（以 restriction_city = 1 为政策事件）
gen policy_month = .
bysort city_id (date_index): replace policy_month = date_index if restriction_city == 1 & missing(policy_month)
bysort city_id (date_index): replace policy_month = policy_month[_n-1] if missing(policy_month)



* Step 5: 定义 event dummies 宏变量
local event_dummies event_k_12 event_k_11 event_k_10 event_k_9 event_k_8 ///
    event_k_7 event_k_6 event_k_5 event_k_4 event_k_3 event_k_2 ///
    event_k0 event_k1 event_k2 event_k3 event_k4 event_k5 event_k6 ///
    event_k7 event_k8 event_k9 event_k10 event_k11 event_k12 event_k13 ///
    event_k14 event_k15 event_k16 event_k17 event_k18 event_k19 ///
    event_k20 event_k21 event_k22 event_k23 event_k24

* Step 6: 执行 event-study OLS 回归
reghdfe ln_price_index ///
    Employment_Rate ln_GDP_Per_Capita_Yuan ln_Urban_Pension_Insurance_Parti ///
    `event_dummies', absorb(date_index) cluster(city_id)

	
	* Step 1: 保存系数和置信区间
preserve
keep if !missing(event_time)
gen lb = .
gen ub = .
gen b = .

foreach k of numlist -12/-2 0/24 {
    local varname = cond(`k' < 0, "event_k_`=abs(`k')'", "event_k`k'")
    quietly {
        replace b = _b[`varname'] if event_time == `k'
        replace lb = _b[`varname'] - 1.96 * _se[`varname'] if event_time == `k'
        replace ub = _b[`varname'] + 1.96 * _se[`varname'] if event_time == `k'
    }
}

* Step 2: 画图
twoway (rcap lb ub event_time, color(gs12)) ///
       (line b event_time, lcolor(blue)), ///
       xline(-1, lpattern(dash)) ///
       title("Event Study: Impact of HPR on Log House Price") ///
       ytitle("Log price change") xtitle("Event time (months)") ///
       legend(off)
restore
