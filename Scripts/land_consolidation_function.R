# function to make variables and test the model
# used only in model construction

set.seed(254)

land_consolidation_function <- function(x, varnames)
{
  # A) Benefits from Do nothing option ####
  # We use maize as the reference crop 
  # Maize production risks facing farmers and their impact on crop production benefit
  
  prop_maize_yield_lost_hazard <- chance_event(natural_hazard, 
                                               yield_loss_drought/100, 0, gen_CV, n=n_years,
                                               one_draw = FALSE)
  
  # Input related constraints as listed in this paper https://doi.org/10.1007/s12571-010-0053-8. Inclusive in the input constraint risk include issues of seed quality, fertilizer, herbicides and pesticides
  
  prop_maize_yield_lost_input_constraint <- 
    chance_event(farmer_inadequate_funds_risk, yield_loss_due_to_input_constraint,
                 0, gen_CV, n=n_years, one_draw = FALSE)
  
  
  prop_maize_yield_lost_management <- 
    chance_event(min(late_planting_risk, health_risk),
                 yield_loss_due_to_management, 0, gen_CV, n=n_years,
                 one_draw = FALSE)
  
  # effects of all the above risks                                                                        
  expected_impact_of_risks <- sapply(c(prop_maize_yield_lost_hazard + 
                                         prop_maize_yield_lost_input_constraint
                                       + prop_maize_yield_lost_management),function(x) min(x,1))
  
  # maize yield can range as low as 1  to 8 Mg/ha  Ngeno 2024 https://doi.org/10.1016/j.heliyon.2024.e24657
  farmer_yield_t_ha <- vv(maize_yield_t_ha, gen_CV, n_years) * no_of_seasons
  
  adjusted_farmer_yield_t_ha <- farmer_yield_t_ha * (1 - expected_impact_of_risks)
  
  
  crop_benefit <- 
    adjusted_farmer_yield_t_ha * 
    ha_per_hh * 1000 * # conversion to kg/ha
    vv(maize_price_kes_kg, gen_CV, n_years) 
  
  # natural capital  
  # The community values land security not only as having title deeds but also the ability to invest in perennial plants such as trees. Trees provide numerous benefits including provision of fuel wood, fruits, soil protection, among others. 
  
  adjusted_value_of_assets <- vv(value_of_farm_assets, gen_CV, n_years,relative_trend = inflation_rate)
  
  # Food cost saved from food accessible from the farm 
  # pm is per month
  annual_saved_food_cost <- vv(saved_food_cost_pm, gen_CV, n_years,relative_trend = inflation_rate) * 12 
  
  status_quo_benefit <- crop_benefit + adjusted_value_of_assets + annual_saved_food_cost
  
  
  # B) Costs of do nothing option ####
  
  # Medical Bills from farm related stresses
  # Farmers are at a high risk of diseases, and stress from higher exposure to farm related accidents, air pollution and pesticides David et al.2021 (https://doi.org/10.3390/su132011384). Conversely, Active lifestyles through working on farm has been associated with a reduction in health costs. For instance, Aoyagi & Shephard 2012 (https://doi.org/10.2165/11590530-000000000-00000) found a 3.7% reduction, and Sato et al. 2020 (https://doi.org/10.1016/j.amepre.2019.12.009) a 0.4% in 2 years and 1% in 3 years. 
  
  medical_bills <- prop_hhincome_spent_on_hospital/100 * vv(hh_income_pa, gen_CV,n_years)
  
  do_nothing_medical_bills <- medical_bills * (1-pc_healthcosts_reduced_physical_activities/100) # percent saved due to active lifestyle
  
  # production costs for the farm
  farming_costs <- vv(production_costs_acre, gen_CV, n_years,relative_trend = inflation_rate) * 
    ha_per_hh * ha_acre_conversion * no_of_seasons
  
  # planning cost involves legal fee, cost of knowledge acquisition, stationery, and printing of documents
  planning_cost
  
  
  status_quo_cost <- do_nothing_medical_bills + farming_costs 
  status_quo_cost[1] <- status_quo_cost[1] + planning_cost
  
  
  # C) Farmer  costs  with land consolidation ####
  
  farmer_one_time_cost <- cost_of_disruption_kes + # Damages on physical assets
    hhupkeep_prior_to_first_payment + # HH income needed to sustain the hh prior 1st payment
    planning_cost # planning cost involves legal fee and the cost of knowledge acquisition
  
  
  farmer_recurring_cost <- adjusted_value_of_assets + # loss
    annual_saved_food_cost + # Food unavailable from farm directly
    medical_bills  # hospital bills  (proxy for long term health) from farm related stresses.Retiring from active lifestyle through farming might reduce over time, therefore, the benefit is reduced by cost of inactivity. Stiernström et al. 1998 (https://doi.org/10.1097/00043764-199810000-00013) found that non-farmers had higher rates of hospital admissions than farmers within the same rural setting. 
  
  land_consolidation_cost <-  farmer_recurring_cost 
  land_consolidation_cost[1] <- land_consolidation_cost[1] + farmer_one_time_cost 
  
  
  # D) Farmer Benefits with land consolidation ####
  # A farmer who leases their land, gets a compensation fee commensurate to their land size. 
  income_from_land_leasing <- vv(compensation_income_pm_acre, gen_CV, n_years)* 2 * 
    ha_per_hh * ha_acre_conversion
  
  
  # Off farm employment and wages
  # Employment options might be low due to the average age of the farmers which is ar 54years Kamau et al. 2018 (https://doi.org/10.1016/j.jrurstud.2017.12.014). Nevertheless, according to the Murang'a CIDP 2018, rural self employment opportunities are increasing with increasing expansion of towns and market centres. Additionally, the the Murang'a people have on average of 9 years in education with a maximum of 19 years and about 88% of the households can read and write Kamau et al. 2018 (https://doi.org/10.1016/j.jrurstud.2017.12.014).  
  off_farm_income <- vv(off_farm_income_kes_pm, gen_CV, n_years) * 12   # pm is per month
  
  #income saved: production costs for farming
  production_costs_saved <- farming_costs
  
  # social cohesion benefit
  # social cohesion is expressed as a factor of free time as a result of the the intervention. With the intervention, free time is associated with social cohesion through community participation and involvement. However, social cohesion can e disrupted by urbanization of the rural areas and migration. Urbanization, free time, and alternative work can threaten social cohesion and lead to anti-social behaviors e.g crime, drug use, theft and domestic conflict. Migration from rural to urban areas can also result into rural hollowing  Wang et al. 2020 (https://doi.org/10.1016/j.landusepol.2020.105146). They take up time that would otherwise be used for community cooperation or activities. We quantify it by the cost of labor per hour.
  social_time_risk <- max(vice_risk,domesticconflict_risk, migration_risk)
  
  adjusted_social_time <- vv(social_time_hr_day, gen_CV, n_years) * (1-social_time_risk)
  social_cohesion <- adjusted_social_time * vv(labour_cost_kes_hr, gen_CV, n_years, 
                                               relative_trend = inflation_rate)
  
  # Childhood benefit
  # Small scale farming employ family labor including children to help with farm labor which reduces the overall production costs. Scholars argue that when children are involved in such activities, they lose the opportunity to just being children and enjoy childhood. Land consolidation, allows for childhood benefit. To determine this benefit, we determined the number of hours a child(ren) contribute(s) to family farm labor weekly. The value of childhood benefit was equated to the value of hiring labor (wage/hour)
  
  childhood_benefit <- child_farm_time_hr_week * children_per_hh * n_weeks_child_farm_time *
    vv(labour_cost_kes_hr, gen_CV, n_years, relative_trend = inflation_rate ) 
  
  
  land_consolidation_benefit <- income_from_land_leasing + off_farm_income + production_costs_saved + 
    social_cohesion + childhood_benefit
  
  land_consolidation_benefit[1] <-  land_consolidation_benefit[1] + sale_of_hh_items_not_needed
  
  
  # E) Calculate net benefit ####
  status_quo_netbenefit <- status_quo_benefit - status_quo_cost
  status_quo_netbenefit_usd <- status_quo_netbenefit/currency_change
  
  land_consolidation_netbenefit <- land_consolidation_benefit - land_consolidation_cost
  land_consolidation_netbenefit_usd <- land_consolidation_netbenefit/currency_change
  
  
  # NPV ####
  NPV_land_consolidation_netbenefit <- discount(land_consolidation_netbenefit, discount_rate, calculate_NPV = TRUE)
  NPV_land_consolidation_netbenefit_usd <- discount(land_consolidation_netbenefit_usd, discount_rate, calculate_NPV = T)
  
  
  NPV_status_quo_netbenefit <- discount(status_quo_netbenefit, discount_rate, calculate_NPV = TRUE)
  NPV_status_quo_netbenefit_usd <- discount(status_quo_netbenefit_usd, discount_rate, calculate_NPV = T)
  
  
  return(list(benefit_lc_kes = NPV_land_consolidation_netbenefit,
              benefit_status_quo_kes = NPV_status_quo_netbenefit,
              net_benefit_lc_kes = NPV_land_consolidation_netbenefit - NPV_status_quo_netbenefit, 
              benefit_lc_usd = NPV_land_consolidation_netbenefit_usd,
              benefit_status_quo_usd = NPV_status_quo_netbenefit_usd,
              net_benefit_lc_usd = NPV_land_consolidation_netbenefit_usd - NPV_status_quo_netbenefit_usd,
              Cashflow_decision_do = land_consolidation_netbenefit - status_quo_netbenefit))
}