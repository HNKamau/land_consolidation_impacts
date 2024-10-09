

library(decisionSupport)
library(tidyverse)
library(ggplot2)

set.seed(254)

source(file = "make_variables.R")
make_variables(estimate_read_csv("FarmerLC_inputdata.csv",
                                 sep = ";"))

# The model ####

lease_land_function <- function(x, varnames){
  
  source(file = "Farmer_lowMP_lowLI.R")
  source(file = "Farmer_lowMP_mediumLI.R")
  source(file = "Farmer_lowMP_highLI.R")
  source(file = "Farmer_mediumMP_lowLI.R")
  source(file = "Farmer_mediumMP_mediumLI.R")
  source(file = "Farmer_mediumMP_highLI.R")
  source(file = "Farmer_highMP_lowLI.R")
  source(file = "Farmer_highMP_mediumLI.R")
  source(file = "Farmer_highMP_highLI.R")
  return(list(Interv_NPV_kes = NPV_interv_kes,     
              No_Interv_NPV_kes = NPV_n_interv_kes,
              NPV_decision_kes = NPV_interv_kes - NPV_n_interv_kes,
              
              Interv_NPV_kes01 = NPV_interv_kes01,
              No_Interv_NPV_kes01 = NPV_n_intervention01,
              NPV_decision_kes01 = NPV_interv_kes01 - NPV_n_interv_kes,
              
              Interv_NPV_kes02 = NPV_interv_kes02,
              No_Interv_NPV_kes02 = NPV_n_interv_kes02,
              NPV_decision_kes02 = NPV_interv_kes02 - NPV_n_interv_kes02,
              
              Interv_NPV_kes10 = NPV_interv_kes10,
              No_Interv_NPV_kes10 = NPV_n_interv_kes10,
              NPV_decision_kes10 = NPV_interv_kes10 - NPV_n_interv_kes10,
              
              Interv_NPV_kes11 = NPV_interv_kes11,
              No_Interv_NPV_kes11 = NPV_n_interv_kes11,
              NPV_decision_kes11 = NPV_interv_kes11 - NPV_n_interv_kes11,
              
              Interv_NPV_kes12 = NPV_interv_kes12,
              No_Interv_NPV_kes12 = NPV_n_interv_kes12,
              NPV_decision_kes12 = NPV_interv_kes12 - NPV_n_interv_kes12,
              
              Interv_NPV_kes20 = NPV_interv_kes20,
              No_Interv_NPV_kes20 = NPV_n_interv_kes20,
              NPV_decision_kes20 = NPV_interv_kes20 - NPV_n_interv_kes20,
              
              Interv_NPV_kes21 = NPV_interv_kes21,
              No_Interv_NPV_kes21 = NPV_n_interv_kes21,
              NPV_decision_kes21 = NPV_interv_kes21 - NPV_n_interv_kes21,
              
              Interv_NPV_kes22 = NPV_interv_kes22,
              No_Interv_NPV_kes22 = NPV_n_interv_kes22,
              NPV_decision_kes22 = NPV_interv_kes22 - NPV_n_interv_kes22,
              
              Cashflow_decision_do = farmer_interv_netbenefit_kes -
                            farmer_pre_interv_netbenefit_kes,
              Cashflow_decision_s0l1 = farmer_interv_netbenefit_kes01 -
                                         farmer_pre_interv_netbenefit_kes,
              Cashflow_decision_s0l2 = farmer_interv_netbenefit_kes02 -
                                farmer_pre_interv_netbenefit_kes,
              Cashflow_decision_s1l0 = farmer_interv_netbenefit_kes10 -
                                     farmer_pre_interv_netbenefit_kes10,
              Cashflow_decision_s1l1 = farmer_interv_netbenefit_kes11 -
                                        farmer_pre_interv_netbenefit_kes11,
              Cashflow_decision_s1l2 = farmer_interv_netbenefit_kes12 -
                                farmer_pre_interv_netbenefit_kes12,
              Cashflow_decision_s2l0 = farmer_interv_netbenefit_kes20 -
                                   farmer_pre_interv_netbenefit_kes20,
              Cashflow_decision_s2l1 = farmer_interv_netbenefit_kes21 -
                                       farmer_pre_interv_netbenefit_kes21,
              Cashflow_decision_s2l2 = farmer_interv_netbenefit_kes22 -
                                      farmer_pre_interv_netbenefit_kes22
              
              

  ))
  
}
              
# Model runs ####
mcSimulation_results <-  decisionSupport::mcSimulation(
  estimate = estimate_read_csv("FarmerLC_inputdata.csv",sep =";"),
  model_function = lease_land_function,
  numberOfModelRuns = 1e4, #10000
  functionSyntax = "plainNames")

# Outcome distribution ####
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("Interv_NPV_kes", "No_Interv_NPV_kes"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 8)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("Interv_NPV_kes1", "No_Interv_NPV_kes1"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 8)
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("Interv_NPV_kes2", "No_Interv_NPV_kes2"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 8)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("NPV_decision_kes",
                                             "NPV_decision_kes1","NPV_decision_kes2"),
                                    method = 'boxplot')

# decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results,
#                                cashflow_var_name = "Cashflow_decision_do")

# Variable Importance ####
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[27],
                                ncomp = 1)
#write.csv(pls_result, 'Outcome/pls.csv')
decisionSupport::plot_pls(pls_result,
                          input_table = read.csv("FarmerLC_inputdata.csv", sep=";"),
                          threshold = 1)

# ggsave("Graph/Farmer/vip.png", width = 7, height = 5, units = 'in', dpi = 300)

# Information value ####
mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[1:27])
evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table,
                                    first_out_var = "Interv_NPV_kes")
write.csv(evpi,'Outcome/evpi.csv')
decisionSupport::plot_evpi(evpi,
                           decision_vars = "NPV_decision_kes",
                           input_table =read.csv("farmerLC_size0.csv", sep=";"),
                           bar_color = "#1a80bb")


# Compound figure ####
compound_figure(mcSimulation_object = mcSimulation_results,
                input_table = read.csv("FarmerLC_inputdata.csv", sep =";"),
                plsrResults = pls_result,
                EVPIresults = evpi,
                decision_var_name = "NPV_decision_kes02",
                cashflow_var_name = "Cashflow_decision_s0l2",
                base_size = 7)



# post_analysis ####
outcome <- as.data.frame(mcSimulation_results$y)
theXs <- as.data.frame(mcSimulation_results$x)
str(outcome)

# Executing suggestions from the meeting ####

#summary stat

summary_stats<- outcome %>% 
  select(Interv_NPV_kes:NPV_decision_kes22) %>% 
  pivot_longer(cols = everything(), names_to = "variables",
               values_to = "project_outcome") %>%
  group_by(variables) %>%
  summarise(
    min_value = min(project_outcome/1e6), #in billions kes
    lower_bound = quantile(project_outcome/1e6, 0.05, na.rm = T),
    median_value = median(project_outcome/1e6),
    upper_bound = quantile(project_outcome/1e6, 0.95, na.rm = T),
    max_value = max(project_outcome/1e6),
    chance_of_loss = sum(project_outcome < 0, na.rm = TRUE) / n() * 100,
    chance_of_gain = sum(project_outcome >= 0, na.rm = TRUE) / n() * 100
  )
print(summary_stats, n=27)

stat.table <- as.data.frame(summary_stats)
names(stat.table) <- c("Cost/Benefit", "Minimum value","5% quantile", "Median", 
                       "95% quantile", "Maximum value", "% chance of loss",
                       "% chance of gain")

nicer_table <- rempsyc::nice_table(stat.table)
print(nicer_table, preview = "docx")
flextable::save_as_docx(nicer_table, path = "Scenario/scenario_summary_stats.docx")



# Violin plot... looks good. 


outcome %>% 
  select(NPV_decision_kes:NPV_decision_kes22) %>% 
  mutate(NPV_00 = NPV_decision_kes/ha_per_hh,
         NPV_01 = NPV_decision_kes01/ha_per_hh, 
         NPV_02 = NPV_decision_kes02/ha_per_hh, 
         NPV_10 = NPV_decision_kes10/ha_per_hh,
         NPV_11 = NPV_decision_kes11/ha_per_hh, 
         NPV_12 = NPV_decision_kes12/ha_per_hh,
         NPV_20 = NPV_decision_kes20/ha_per_hh, 
         NPV_21 = NPV_decision_kes21/ha_per_hh, 
         NPV_22 = NPV_decision_kes22/ha_per_hh) %>% 
  pivot_longer(., cols =c('NPV_00', 'NPV_01', 'NPV_02', 'NPV_10', 
                          'NPV_11', 'NPV_12', 'NPV_20', 'NPV_21',
                          'NPV_22'), names_to = 'treatments', 
               values_to = 'decision_do') %>% 
  mutate(treatments = factor(treatments, 
                             labels = c('NPV_00', 'NPV_01', 'NPV_02', 'NPV_10', 
                                        'NPV_11', 'NPV_12', 'NPV_20', 'NPV_21',
                                          'NPV_22'))) %>% 
  ggplot(aes(x = treatments, y = decision_do/1e6, fill = treatments)) +
  geom_violin(trim = FALSE, show.legend = F) +
  theme_bw() +
  labs(x = "Maize price (KES/kg) and lease income (acre/season) scenarios",
       y = "NPV (with LC - without LC) per hectare (million) KES") +
  theme(axis.text.x = element_text( hjust = 1)) +
  scale_x_discrete(
    labels =c("NPV_00" = str_wrap("Low maize price, low land price", width = 10), 
              "NPV_01" = str_wrap("Low maize price, medium land price",width =10),
              "NPV_02" = str_wrap("Low maize price, high land price", width = 10),
              "NPV_10" = str_wrap("Medium maize price, low land price", width = 10), 
              "NPV_11" = str_wrap("Medium maize price, medium land price", width = 10),
              "NPV_12" = str_wrap("Medium maize price, high land price", width = 10),
              "NPV_20" = str_wrap("High maize price, low land price", width = 10),
              "NPV_21" = str_wrap("High maize price, medium land price", width = 10),
              "NPV_22" = str_wrap("High maize price, high land price", width = 10)))

  
ggsave("Scenario/Farmer2/m&p_violin.png", width = 8, height = 6, units = "in", dpi = 300)



# Suggestion 3 ####
# lease scenario 

custom_labels <-c(
  "NPV_00" = "Low maize price, low land price", 
  "NPV_01" = "Low maize pricee, medium land price",
  "NPV_02" = "Low maize price, high land price", 
  "NPV_10" = "Medium maize price, low land price",  
  "NPV_11" = "Medium maize price, medium land price", 
  "NPV_12" = "Medium land size, high land price", 
  "NPV_20" = "High maize price, low land price", 
  "NPV_21" = "High maize price, medium land price",
  "NPV_22" = "High maize price, high land price")


outcome %>% 
  select(NPV_decision_kes:NPV_decision_kes22) %>% 
  mutate(NPV_00 = NPV_decision_kes/ha_per_hh,
         NPV_01 = NPV_decision_kes01/ha_per_hh, 
         NPV_02 = NPV_decision_kes02/ha_per_hh, 
         NPV_10 = NPV_decision_kes10/ha_per_hh,
         NPV_11 = NPV_decision_kes11/ha_per_hh, 
         NPV_12 = NPV_decision_kes12/ha_per_hh,
         NPV_20 = NPV_decision_kes20/ha_per_hh, 
         NPV_21 = NPV_decision_kes21/ha_per_hh, 
         NPV_22 = NPV_decision_kes22/ha_per_hh) %>% 
  pivot_longer(., cols =c('NPV_00', 'NPV_01', 'NPV_02', 'NPV_10', 
                          'NPV_11', 'NPV_12', 'NPV_20', 'NPV_21',
               'NPV_22'), names_to = 'treatments', 
               values_to = 'decision_do') %>% 
  # group_by(treatments) %>%
  # mutate(lower_bound = quantile(decision_do, 0.05, na.rm = T),
  #        uper_bound = quantile(decision_do, 0.95, na.rm = T)) %>%
  # filter(decision_do >=lower_bound & decision_do <= upper_bound) %>%
  # ungroup() %>%
  mutate(treatments = factor(treatments, 
                             levels = c('NPV_00', 'NPV_01', 'NPV_02', 
                                        'NPV_10', 'NPV_11', 'NPV_12', 
                                        'NPV_20', 'NPV_21','NPV_22'))) %>% 
  
  ggplot(aes(x = treatments, y = decision_do/1e6, fill = treatments)) +
  geom_boxplot(width = 0.3, show.legend = F) +
  theme_bw() +
  labs(x = "Land size and lease income  scenarios",
       y = "NPV per ha (million KES)") +
  theme(axis.text.x = element_text(hjust = 1)) +

 
  
  ggplot(aes(x = decision_do/1e6, fill = treatments)) +
  geom_histogram(binwidth = 0.01, color = "black", show.legend = F) +
  facet_wrap(~ treatments, scales = "free_x",
             labeller = as_labeller(custom_labels)) +
  theme_bw() +
  labs( x = "NPV per hectare (million KES)",
       y = "Frequency")


  
  scale_x_discrete(labels = c(
    "NPV_00" = str_wrap("Low maize price, low land price", width = 10),
    "NPV_01" = str_wrap("Low maize price, medium land price",width =10),
    "NPV_02" = str_wrap("Low maize price, high land price", width = 10),
    "NPV_10" = str_wrap("Medium maize price, low land price", width = 10),
    "NPV_11" = str_wrap("Medium maize price, medium land price", width = 10),
    "NPV_12" = str_wrap("Medium maize price, high land price", width = 10),
    "NPV_20" = str_wrap("High maize price, low land price", width = 10),
    "NPV_21" = str_wrap("High maize price, medium land price", width = 10),
    "NPV_22" = str_wrap("High maize price, high land price", width = 10)))


# ggsave("Scenario/Farmer2/m&p_boxplot.png", width = 8, height = 6, units = "in",
#        dpi = 300)
#   
  
  ggplot(., aes(decision_do/1e6, x=treatments))+
  geom_boxplot(width = 0.2)+
  # coord_flip()+
  labs(x = "Maize price (KES/Kg) and lease income (acre/season) scenarios", 
       y = "NPV with - NPV without LC (per ha) (Million KES)") +
  scale_x_discrete(labels =c("NPV_00" = str_wrap("Low maize price, low land price", width = 10), 
                             "NPV_01" = str_wrap("Low maize price, medium land price",width =10),
                             "NPV_02" = str_wrap("Low maize price, high land price", width = 10),
                             "NPV_10" = str_wrap("Medium maize price, low land price", width = 10), 
                             "NPV_11" = str_wrap("Medium maize price, medium land price", width = 10),
                             "NPV_12" = str_wrap("Medium maize price, high land price", width = 10),
                             "NPV_20" = str_wrap("High maize price, low land price", width = 10),
                             "NPV_21" = str_wrap("High maize price, medium land price", width = 10),
                             "NPV_22" = str_wrap("High maize price, high land price", width = 10)))

              
ggsave("Scenario/Farmer2/m&l.png", width = 7, height = 5, units = "in", dpi = 300)



processed_data <- outcome %>% 
  select(NPV_decision_kes, NPV_decision_kes01, NPV_decision_kes02,
         NPV_decision_kes10, NPV_decision_kes11, NPV_decision_kes12, 
         NPV_decision_kes20, NPV_decision_kes21, NPV_decision_kes22) %>% 
  mutate(NPV_00 = NPV_decision_kes/ha_per_hh,
         NPV_01 = NPV_decision_kes01/ha_per_hh, 
         NPV_02 = NPV_decision_kes02/ha_per_hh, 
         NPV_10 = NPV_decision_kes10/ha_per_hh,
         NPV_11 = NPV_decision_kes11/ha_per_hh, 
         NPV_12 = NPV_decision_kes12/ha_per_hh,
         NPV_20 = NPV_decision_kes20/ha_per_hh, 
         NPV_21 = NPV_decision_kes21/ha_per_hh, 
         NPV_22 = NPV_decision_kes22/ha_per_hh) %>% 
  pivot_longer(., cols =c('NPV_00', 'NPV_01', 'NPV_02', 'NPV_10', 
                          'NPV_11', 'NPV_12', 'NPV_20', 'NPV_21',
                          'NPV_22'), names_to = 'treatments', 
               values_to = 'decision_do') %>% 
  group_by(treatments) %>%
  mutate(decision_do2 =decision_do/1e6) %>%
  mutate(lower_bound = quantile(decision_do2, 0.05, na.rm = T),
         upper_bound = quantile(decision_do2, 0.95, na.rm = T)) %>%
  filter(decision_do2 >=lower_bound & decision_do2 <= upper_bound) %>%
  ungroup() %>%
  mutate(treatments = factor(treatments, 
                             levels = c('NPV_00', 'NPV_01', 'NPV_02', 
                                        'NPV_10', 'NPV_11', 'NPV_12', 
                                        'NPV_20', 'NPV_21','NPV_22')))


vio <- ggviolin(processed_data, x = "treatments", y = "decision_do2", 
                fill = "treatments")
vio + stat_anova_test() + 
  theme_bw() +
  theme(legend.position = "none")+
  labs(
    x = 'Maize price and lease income scenarios',
    y = "(NPV with - NPV without LC) per hectare (Million KES)") +
  scale_x_discrete(labels =c("NPV_00" = str_wrap("Low maize price, low land price", width = 10), 
                             "NPV_01" = str_wrap("Low maize price, medium land price",width =10),
                             "NPV_02" = str_wrap("Low maize price, high land price", width = 10),
                             "NPV_10" = str_wrap("Medium maize price, low land price", width = 10), 
                             "NPV_11" = str_wrap("Medium maize price, medium land price", width = 10),
                             "NPV_12" = str_wrap("Medium maize price, high land price", width = 10),
                             "NPV_20" = str_wrap("High maize price, low land price", width = 10),
                             "NPV_21" = str_wrap("High maize price, medium land price", width = 10),
                             "NPV_22" = str_wrap("High maize price, high land price", width = 10))) 
      # annotate("text",
    #          label = "Significane level: **** p < 0.0001",
    #          x= 2.5, y = 49, size = 4, colour = "black")
    # 
  

# Bar plot of mean +/-se
ggbarplot(processed_data, x = "treatments", y = "decision_do", add = "mean_se")+
  stat_compare_means() +                                         # Global p-value
  stat_compare_means(ref.group = "NPV_00", label = "p.signif",
                     label.y = c(40, 150,270, 60,180,200,60, 180,300))                   # compare to ref.group

# Line plot of mean +/-se
ggline(ToothGrowth, x = "dose", y = "len", add = "mean_se")+
  stat_compare_means() +                                         # Global p-value
  stat_compare_means(ref.group = "0.5", label = "p.signif",
                     label.y = c(22, 29))     


write.csv(processed_data, "Outcome/modifiable.csv")




# compare_means(decision_do2 ~ treatments,  data = processed_data,
#               ref.group = "NPV_00",
#               method = "t.test")
# 
# ggboxplot(processed_data, x="treatments", y = "decision_do2",
#             width = .4) +
#   stat_compare_means(method = "anova", label.y = 350 ) +
#   stat_compare_means(label = "p.signif", method = "t.test",
#                      ref.group = ".all.") +
#   scale_y_continuous(expand = c(0, 0),
#                      # limits = c(0,400),
#                      labels = scales::comma) +
#   theme_bw()+
#   labs(x = "Land size and lease income scenarios", 
#        y = "NPV with - NPV without LC (per ha) (million KES)") +
#   scale_x_discrete(labels =c("NPV_00" = str_wrap("Small land size, low land price", width = 10), 
#                              "NPV_01" = str_wrap("Small land size, medium land price",width =10),
#                              "NPV_02" = str_wrap("Small land size, high land price", width = 10),
#                              "NPV_10" = str_wrap("Medium land size, low land price", width = 10), 
#                              "NPV_11" = str_wrap("Medium land size, medium land price", width = 10),
#                              "NPV_12" = str_wrap("Medium land size, high land price", width = 10),
#                              "NPV_20" = str_wrap("high land size, low land price", width = 10),
#                              "NPV_21" = str_wrap("high land size, medium land price", width = 10),
#                              "NPV_22" = str_wrap("high land size, high land price", width = 10)))
# ggsave("Scenario/land&lease.png", width = 7, height = 5, units = "in",
#        dpi = 300)

  





comparisons <- list(
  c("NPV_00", "NPV_01"),
  c("NPV_00", "NPV_02"),
  c("NPV_00", "NPV_10"),
  c("NPV_00", "NPV_11"),
  c("NPV_00", "NPV_12"),
  c("NPV_00", "NPV_20"),
  c("NPV_00", "NPV_21"),
  c("NPV_00", "NPV_22"),
  c("NPV_10", "NPV_12"),
  c("NPV_10", "NPV_12"),
  c("NPV_20", "NPV_21"),
  c("NPV_20", "NPV_22"))

# Plot the data
ggplot(processed_data, aes(x = treatments, y = decision_do / 1e6)) +
  geom_boxplot(width=.4, colour = "black", fill = "grey",
               position = position_dodge(width = NULL)) +
  theme_bw() +
  labs(
    x = 'Maize price and lease income scenarios',
    y = "NPV with - NPV without LC (per ha) (Million KES)",
    subtitle = "Combinations of Lease income (acre/season) and maize price (KES/Kg)") +
  scale_x_discrete(labels =c("NPV_00" = str_wrap("Low maize price, low land price", width = 10), 
                             "NPV_01" = str_wrap("Low maize price, medium land price",width =10),
                             "NPV_02" = str_wrap("Low maize price, high land price", width = 10),
                             "NPV_10" = str_wrap("Medium maize price, low land price", width = 10), 
                             "NPV_11" = str_wrap("Medium maize price, medium land price", width = 10),
                             "NPV_12" = str_wrap("Medium maize price, high land price", width = 10),
                             "NPV_20" = str_wrap("High maize price, low land price", width = 10),
                             "NPV_21" = str_wrap("High maize price, medium land price", width = 10),
                             "NPV_22" = str_wrap("High maize price, high land price", width = 10)))+
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-50, 150),
                     labels = scales::comma) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.ticks = element_blank(),) +
  stat_compare_means(
    comparisons = comparisons,
    method = "t.test",  # Use t-test for pairwise comparisons
    label = "p.signif",
    hide.ns = T)
    # aes(color(..p.signif..))) 
# +
#   annotate("text",
#            label = "Significane level: **** p < 0.0001",
#            x= 2.5, y = 50, size = 4, colour = "black")


ggsave("Scenario/Farmer2/maize&lease_sig.png", width = 7, height = 5, units = "in",
       dpi = 300)
