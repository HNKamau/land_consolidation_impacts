library(decisionSupport)
library(tidyverse)
library(ggplot2)
# assumptions - general
#
set.seed(123)

source(file = "Scripts/make_variables.R")

test_decision <- function(x, varnames)
{
  source(file = "Scripts/test3.R")
  source(file = "Scripts/without_loops.R")
  return(list(farmer_Interv_NPV = NPV_intervention,
              farmer_No_Interv_NPV = NPV_n_intervention,
              farmer_NPV_decision_do = NPV_intervention - NPV_n_intervention, 
              Implementer_crop_NPV  = crop_NPV,
              Implementer_mango_NPV = mango_NPV,
              Implementer_hay_NPV = hay_NPV,
              Implementer_carbon_benefit_NPV = carbon_benefit_NPV,
              Implementer_Interv_NPV = Implementer_NPV_intervention,
              Implementer_Interv_burden_NPV = Implementer_NPV_n_intervention,
              Implementer_NPV_decision_do = Implementer_NPV_intervention - Implementer_NPV_n_intervention,
              farmer_Cashflow_decision_do = result_intervention - result_n_intervention,
              Implementer_Cashflow_decision_do = LC_returns - LC_costs))
}

#simulate results
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = estimate_read_csv("Input_tables/farmer_input_table.csv"),
  model_function = test_decision,
  numberOfModelRuns = 1e3, #1000
  functionSyntax = "plainNames")

# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
#                                     vars = c("farmer_Interv_NPV", "farmer_No_Interv_NPV"),
#                                     method = 'smooth_simple_overlay',
#                                     base_size = 8)
# 
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
#                                     vars = c("Implementer_Interv_NPV", "Implementer_Interv_burden_NPV"),
#                                     method = 'smooth_simple_overlay',
#                                     base_size = 8)
# 
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
#                                     vars = "Implementer_NPV_decision_do", 
#                                     method = 'boxplot_density')
# 
# decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results, 
#                                cashflow_var_name = "Cashflow_decision_do")

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[10], 
                                ncomp = 1)
# 
# input_table <- read.csv("Input_tables/farmer_input_table.csv")
# 
# decisionSupport::plot_pls(pls_result, 
#                           input_table = input_table, 
#                           threshold = 0)

mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[1:10])

evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table, 
                                    first_out_var = "farmer_Interv_NPV")

# decisionSupport::plot_evpi(evpi, decision_vars = "Implementer_NPV_decision_do")

compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = read.csv("Input_tables/farmer_input_table.csv"), 
                plsrResults = pls_result, 
                EVPIresults = evpi,
                decision_var_name = "Implementer_NPV_decision_do", 
                cashflow_var_name = "Implementer_Cashflow_decision_do",
                base_size = 7)
# farmer
compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = read.csv("Input_tables/farmer_input_table.csv"), 
                plsrResults = pls_result, 
                EVPIresults = evpi,
                decision_var_name = "farmer_NPV_decision_do", 
                cashflow_var_name = "farmer_Cashflow_decision_do",
                base_size = 7)


#### 
Intervention_NPV <- mcSimulation_table %>% 
  dplyr::select(farmer_Interv_NPV:Implementer_NPV_decision_do) 

# visualizing all the implementer interventions ####
implementer_figure <- Intervention_NPV %>%
  select(Implementer_crop_NPV,Implementer_mango_NPV, Implementer_hay_NPV, 
         Implementer_carbon_benefit_NPV) %>% 
  pivot_longer(., cols = c("Implementer_crop_NPV", "Implementer_mango_NPV",
                           "Implementer_hay_NPV", "Implementer_carbon_benefit_NPV"),
               names_to = "Interventions",
               values_to = "Project_outcome") %>% 
  ggplot2::ggplot(.,aes(x = Project_outcome/1e9, fill = Interventions)) +
  geom_boxplot(alpha = 0.7)+
  #geom_density(alpha= 0.5) +
  labs(x = "Project Outcome in billion USD ", y = "Density") +
  scale_fill_manual(name = "",
                    labels = c("Carbon benefit", " Crop benefit", " Hay benefit",
                               "Mango benefit"),
                    values = c("#882255", "navyblue", "lightblue", "yellow")) +
  theme_minimal()
print(implementer_figure)


# excluding crop  #### 
implementer_figure2 <-Intervention_NPV %>%
  select(Implementer_mango_NPV, Implementer_hay_NPV, 
         Implementer_carbon_benefit_NPV) %>% 
  pivot_longer(., cols = c( "Implementer_mango_NPV",
                            "Implementer_hay_NPV", "Implementer_carbon_benefit_NPV"),
               names_to = "Interventions",
               values_to = "Project_outcome") %>% 
  ggplot2::ggplot(.,aes(x = Project_outcome/1e9, fill = Interventions)) +
  #geom_boxplot()+
  geom_density(alpha= 0.5) +
  labs(x = "Project Outcome in billion USD ", y = "Density") +
  scale_fill_manual(name = "",
                    labels = c("Carbon benefit", " Hay benefit",
                               "Mango benefit"),
                    values = c("#999933", "brown", "#88CCEE")) +
  theme_minimal()

print(implementer_figure2)


# looking at farmer and implementer only - their overall benefit ####

stakeholder_figure <-Intervention_NPV %>% 
  select (farmer_Interv_NPV,Implementer_Interv_NPV) %>% 
  pivot_longer(., cols = c("farmer_Interv_NPV", "Implementer_Interv_NPV"),
             names_to = "Stakeholder",
             values_to = "Project_Outcome") %>% 
  ggplot2::ggplot(.,aes( x = Project_Outcome/1e6, fill = Stakeholder)) +
  #geom_boxplot()+
  geom_density(alpha = 0.5) +
  labs(x = "Project returns million USD)", y = "Density") +
  scale_fill_manual(name = "Stakeholders",
                    labels = c("Implementer", "Farmer"),
                    values = c("palegreen", "yellow")) +
  theme_minimal()
print(stakeholder_figure)

# looking at farmer and implementer only - NPV decision do ####

stakeholder_figure <-Intervention_NPV %>% 
  select (farmer_NPV_decision_do,Implementer_NPV_decision_do) %>% 
  pivot_longer(., cols = c("farmer_NPV_decision_do", "Implementer_NPV_decision_do"),
               names_to = "Stakeholder",
               values_to = "Project_Outcome") %>% 
  ggplot2::ggplot(.,aes( x = Project_Outcome/1e6, fill = Stakeholder)) +
  geom_boxplot()+
  #geom_density(alpha = 0.5) +
  labs(x = "Project Outcome million USD)", y = "Density") +
  scale_fill_manual(name = "Stakeholders",
                    labels = c("Implementer", "Farmer"),
                    values = c("pink", "skyblue")) +
  theme_minimal()
print(stakeholder_figure)

# looking at the farmer only  ####
farmer_figure <- Intervention_NPV %>% 
  select(farmer_Interv_NPV, farmer_No_Interv_NPV) %>% 
  pivot_longer(., cols = c("farmer_Interv_NPV", "farmer_No_Interv_NPV"),
               names_to = "Value",
               values_to = "Overall_value") %>% 
  ggplot2::ggplot(., aes(x = Overall_value/1e3, fill = Value)) +
  geom_boxplot()+
  #geom_density(alpha = 0.5) +
  labs(x = "Outcome in thousand USD", y = "Density")+
  scale_fill_manual(name = "",
                    labels =c("Benefit with intervention", 
                              "Benefit without intervention"),
                    values = c("brown", "#377EB8"))+
  theme_minimal()
print(farmer_figure)


