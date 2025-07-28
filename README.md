# **Modeling Land Consolidation Decisions for Smallholder Farmers in Kenya**

Farmers often face complex and risky decisions regarding their farming systems. These decisions are influenced by a range of socio-cultural, economic, political, and environmental factors. While many farmers base their choices on expected returns or profits, they often rely on simple cost-benefit analyses that fail to account for risks and uncertainties. As a result, some decisions may lead to suboptimal outcomes that leave the farmers worse off.

In the near future, farmers may need to decide whether to participate in land consolidation programs aimed at improving livelihoods and addressing land fragmentation. To support this decision-making process, we apply a **Decision Analysis** approach where we:

- Map out the impact pathways of joining a land consolidation program and assess likely outcomes.  
- Model risks and uncertainties, evaluating how they affect expected benefits and costs.  
- Develop a set of equations that compare:  
  - The farmer’s current system  
  - The potential system under land consolidation  
- Simulate outcomes over a 25-year horizon to assess long-term value and implications.  
- Identify critical variables where additional information could significantly influence the decision.

To simulate the outcomes of the decision on whether to participate in the land consolidation program, you will need:

- The input data file: `land_consolidation_input.csv` (containing probabilistic ranges of variables)  
- The model function: `land_consolidation_function.R`  
- The analysis report: `Analysis_with_mc_scenarios.Rmd`, which demonstrates how outcome decisions vary under different scenarios driven by key variables.
