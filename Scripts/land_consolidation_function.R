# function to make variables and test the model
# used only in model construction

make_variables <- function(est,n=1)
{x <- decisionSupport::random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

# CW NOTE I had to do some work to get the separators to work here
# CW NOTE had to change ; to , and update the file somehow 
make_variables(decisionSupport::estimate_read_csv(paste("Input_tables/land_consolidation_input.csv", sep=",")))


set.seed(254)

farmer <- function(x, varnames)
{
  
  # A) do nothing benefits ####
  # We use maize as the reference crop 
  # ex-ante risks to farmers crops
  
  # CW NOTE no need to call these here, theya are in the input table already
  natural_hazard
  pest_disease_risk
  farmer_inadequate_funds_risk
  late_planting_risk
  health_risk
  
  prop_maize_yield_lost_hazard <- chance_event(natural_hazard, 
                                               yield_loss_drought/100, 0, gen_CV, n=n_years,
                                               one_draw = FALSE)
  
  prop_maize_yield_lost_disease <- chance_event(pest_disease_risk,
                                                yield_loss_disease/100, 0, gen_CV,n=n_years, 
                                                one_draw = FALSE)
  # CW NOTE seems like a lot of losses here for inout constraints.. is this againat some ideal super high yield?
  
  prop_maize_yield_lost_input_constraint <- 
    chance_event(farmer_inadequate_funds_risk, yield_loss_due_to_input_constraint,
                 0, gen_CV, n=n_years, one_draw = FALSE)
  
  prop_maize_yield_lost_management <- 
    chance_event(min(late_planting_risk, health_risk),
                 yield_loss_due_to_management, 0, gen_CV, n=n_years,
                 one_draw = FALSE)
  
  #effects of all the above risks                                                                        
  farm_risks <- sapply(c(prop_maize_yield_lost_hazard + 
                           prop_maize_yield_lost_disease + prop_maize_yield_lost_input_constraint
                         + prop_maize_yield_lost_management),function(x) min(x,1))
  # CW NOTE farm risks is not really 'risks' but rather expected impacts of all risks.. 
  # CW NOTE seems rather high - 100% losses in  few cases
  farmer_yield_t_ha <- vv(maize_yield_t_ha, gen_CV, n_years) * no_of_seasons
  # CW NOTE expected yields 12-14 should be expectes of a high functioning maize crop.. 
  # CW NOTE here we have already a low expecation of yield so i wonder if some of the risks above are already included
  adjusted_farmer_yield_t_ha <- farmer_yield_t_ha * (1 - farm_risks)
  # CW NOTE generally very low and sometimes none at all - can you chekc this against the 'real worls' observations
  # CW NOTE are these reasonable annual harvest numbers? 4,4,7,2,9,5,4,0,0, etc. 
  
  crop_benefit <- 
    adjusted_farmer_yield_t_ha * 
    ha_per_hh * 1000 * # conversion to kg/ha
    vv(maize_price_kes_kg, gen_CV, n_years) 
  # CW NOTE should shcck ethe numbers here and see if this is all reasonable
  # CW NOTE do we have the right shillings per kg? we might expect higher incomes
  
  # natural capital  
  adjusted_value_of_assets <-vv(value_of_farm_assets, gen_CV, n_years,relative_trend = inflation_rate)
  # CW NOTE a but fuzzy for me here.. as a farmer do I get an annual return from my assests? 
  # CW NOTE is this in the form of land gaining value? 
  
  # Food cost saved from food accessible from the farm 
  annual_saved_food_cost <- vv(saved_food_cost_pm, gen_CV, n_years,relative_trend = inflation_rate) * 12 
  # CW NOTE names could be more clear for my taste - does pm mean something about the condolidation?  
  # CW NOTE if we add a relative trend to some variabvles we should be sure to be consistent
  # CW NOTE this should also be excluded from the NPV calcualtion (not part of discounting anymore)
  
  status_quo_benefit <- crop_benefit + adjusted_value_of_assets + annual_saved_food_cost
  # CW NOTE here I suppose if there are no yields the reare also no saved food costs 
  # CW NOTE no yields in (several cases when I run this model)
  
  # B) do nothing costs ####
  # medical Bills from farm related stresses
  medical_bills <- prop_hhincome_spent_on_hospital/100 * vv(hh_income_pa, gen_CV,n_years)
  # CW NOTE can there also be medical savings when people have an active lifestyle working in the field
  # CW NOTE just a thought that farms can also lead to health (I think this is not unfounded)
  
  # production costs for the farm
  farming_costs <- vv(production_costs_saved_acre, gen_CV, n_years,relative_trend = inflation_rate)*
    ha_per_hh * ha_acre_conversion * no_of_seasons
  # CW NOTE why 'saved' production costs? 
  
  # planning cost involves legal fee and the cost of knowledge acquisition
  farmer_plan_cost  <- planning_cost
  # CW NOTE this is a single total and is very high
  # CW NOTE four times the farming_costs for all years of the scenario... 
  
  status_quo_cost <- medical_bills + farming_costs + farmer_plan_cost
  # CW NOTE #########
  # CW NOTE related to the note above 
  # CW NOTE the farmer plan costs (40,000 shillings) is added to each year
  # CW NOTE + farmer_plan_costs is happening each year as a constant
  
  # C) Farmer  costs  with land consolidation ####
  farmer_one_time_cost <- cost_of_disruption_kes + # Damages on physical assets
    hhupkeep_prior_to_first_payment # HH income needed to sustain the hh prior 1st payment
  # CW NOTE maybe 'planning costs' go here? 
  
  # planning cost involves legal fee and the cost of knowledge acquisition
  farmer_plan_cost  <- planning_cost
  # CW NOTE already calcualted above and not sure where it fits
  
  farmer_recurring_cost <- adjusted_value_of_assets + # loss
    annual_saved_food_cost # Food unavailable from farm directly
  
  land_consolidation_cost <-  farmer_one_time_cost +  farmer_plan_cost + farmer_recurring_cost
  # CW NOTE added again here to each year of the list
  # CW NOTE planning costs should probably only happen in year 1
  
  # D) Farmer Benefits with land consolidation ####
  # farmer's annual compensation 
  lessor_fee <-vv(compensation_income_pm_acre, gen_CV, n_years)* 2 * ha_per_hh * ha_acre_conversion
  # CW NOTE I have not heard of lesor before
  # CW NOTE this is important and could be explained more
  
  #off farm employments and wages
  off_farm_income <-vv(off_farm_income_kes_pm, gen_CV, n_years) *12
  # CW NOTE this is abit critical since we can imagine off farm jobs can be hard to get for farmers
  # CW NOTE depends on the education and other factors
  # CW NOTE my experience is that farmers can go to be low wage workers
  # CW NOTE not a lot of options for them
  # CW NOTE I would expect this to be lower
  
  #income saved: production costs for farming
  production_costs_saved <- farming_costs
  # hospital bills saved (proxy for long term health) from farm related stresses
  medical_bills_saved <- medical_bills  
  # CW NOTE all medical bills are imminiated? 
  # CW NOTE seems a bit overly hopeful - should we explain this a bit more? 
  
  # social cohesion benefit
  # social cohesion is expressed as a factor of free time as a result of the the
  # intervention. With the intervention, free time is associated with social
  # cohesion through community participation and involvement. We quantify it by
  # the cost of labor per hour. First, risks that threaten social cohesion are
  # anti-social behaviors e.g crime, drug use, theft and domestic conflict -
  # They take up time that would otherwise be used for community cooperation or
  # activities
  
  # CW NOTE  I like that you make notes in your scripts - would be good to see more of them 
  # CW NOTE use indentations when writing in line notes
  social_time_risk <- max(vice_risk,domesticconflict_risk)
  
  adjusted_social_time <- vv(social_time_hr_day, gen_CV, n_years) * (1-social_time_risk)
  social_cohesion <- adjusted_social_time * vv(labour_cost_kes_hr, gen_CV, n_years, 
                                               relative_trend = inflation_rate)
  # CW NOTE Is it possible that the consolidation can be bad for cohesion? 
  # CW NOTE getting a job off farm adn all that can cause less contact
  # CW NOTE farmers who lose land and connection to land and culture
  # CW NOTE maybe this is not as black and white as this
  
  # Better childhood benefit = long term benefit is education quantified by
  # child's contribution to family farm labor weekly and the value of hired
  # labor
  
  better_childhood <- child_farm_time_hr_week * children_per_hh * n_weeks_child_farm_time *
    vv(labour_cost_kes_hr, gen_CV, n_years, relative_trend = inflation_rate ) 
  # CW NOTE generally keep direction out of varible names and values
  # CW NOTE 'better' should just be beenfit or something.. this can go up or down
  # CW NOTE  could also be a negative effect right? 
  # CW NOTE labor costs are income? or is this something else? childcare?
  # CW NOTE is is income for kids? They get 30ks per hour? 
  # CW NOTE why is their income added to the benefots of land consolidation?
  
  land_consolidation_benefit <-lessor_fee + off_farm_income + production_costs_saved + 
    sale_of_hh_items_not_needed + medical_bills_saved + 
    social_cohesion + better_childhood
  # CW NOTE a bit confused here how all this is a benefit only for consolidaation
  # CW NOTE seems like some of this also exists in the baseline
  # CW NOTE agree? 
  # CW NOTE perhaps we should mention that in the model and adjust values accordingly
  
  # E) Calculate net benefit ####
  status_quo_netbenefit <- status_quo_benefit - status_quo_cost
  status_quo_netbenefit_usd <- status_quo_netbenefit/currency_change
  
  land_consolidation_netbenefit <- land_consolidation_benefit - land_consolidation_cost
  land_consolidation_netbenefit_usd <- land_consolidation_netbenefit/currency_change
  
  # Calculate NPV categorized costs and benefits ####
  food_cost_saved_npv <- discount(annual_saved_food_cost, discount_rate,calculate_NPV = TRUE)
  natural_assets_npv <- discount(adjusted_value_of_assets, discount_rate,calculate_NPV = T)
  lease_npv <- discount(lessor_fee, discount_rate, calculate_NPV = T)
  prdn_costs_npv <- discount(farming_costs, discount_rate, calculate_NPV = T)
  alt_income_npv <- discount(off_farm_income, discount_rate, calculate_NPV = T)
  better_childhood_npv <- discount(better_childhood, discount_rate,calculate_NPV = T)
  social_npv <- discount(social_cohesion, discount_rate, calculate_NPV = T)
  medical_npv <- discount(medical_bills_saved, discount_rate, calculate_NPV = T)
  crop_npv <- discount(crop_benefit, discount_rate, calculate_NPV = T)
  
  
  # NPV ####
  NPV_land_consolidation_netbenefit <- discount(land_consolidation_netbenefit, discount_rate, calculate_NPV = TRUE)
  NPV_land_consolidation_netbenefit_usd <- discount(land_consolidation_netbenefit_usd, discount_rate, calculate_NPV = T)
  
  
  NPV_status_quo_netbenefit <- discount(status_quo_netbenefit, discount_rate, calculate_NPV = TRUE)
  NPV_status_quo_netbenefit_usd <- discount(status_quo_netbenefit_usd, discount_rate, calculate_NPV = T)
  
  # Benefit cost ratio ####
  # without intervention
  npv_status_quo_benefit <- discount(status_quo_benefit, discount_rate, calculate_NPV = T)
  npv_status_quo_cost <- discount(status_quo_cost, discount_rate, calculate_NPV = T)
  bcr_status_quo <-npv_status_quo_benefit/npv_status_quo_cost
  
  # with intervention
  npv_land_consolidation_benefit <- discount(land_consolidation_benefit, discount_rate, calculate_NPV = T)
  npv_land_consolidation_cost <- discount(land_consolidation_cost, discount_rate, calculate_NPV = T)
  bcr_land_consolidation  <- npv_land_consolidation_benefit/npv_land_consolidation_cost
  
  
  return(list(benefit_lc_kes = NPV_land_consolidation_netbenefit,
              benefit_status_quo_kes = NPV_status_quo_netbenefit,
              net_benefit_lc_kes = NPV_land_consolidation_netbenefit - NPV_status_quo_netbenefit, 
              benefit_lc_usd = NPV_land_consolidation_netbenefit_usd,
              benefit_status_quo_usd = NPV_status_quo_netbenefit_usd,
              net_benefit_lc_usd = NPV_land_consolidation_netbenefit_usd - NPV_status_quo_netbenefit_usd,
              BCR_status_quo = bcr_status_quo,
              BCR_lc = bcr_land_consolidation,
              Food_money_saved = food_cost_saved_npv,
              Natural_assets = natural_assets_npv,
              Planning = farmer_plan_cost, 
              Disruption_cost = farmer_one_time_cost,
              Lease = lease_npv, 
              Production_costs = prdn_costs_npv, 
              Alternative_income = alt_income_npv, 
              Childhood = better_childhood_npv,
              Social = social_npv,
              Medical = medical_npv,
              Annual_yield = crop_npv,
              Cashflow_decision_do = land_consolidation_netbenefit - status_quo_netbenefit))
}