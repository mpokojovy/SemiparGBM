Based on paper "Quantitative Stock Market Modeling Using Multivariate Geometric Random Walk"
by Michael Pokojovy, Andrews T. Anum, Obed Amo, Maria C. Mariani and Michael C. Orosz (2024)

INSTRUCTIONS
1) (Optional) Run combine.R in /data/ (if you want to recreate stocks.combined.RData)
2) Run shell.R in /imputation/ to impute missing values in stock price log-increments.
   The output will be put into projct main directory
3) Run shell.R in /plots/ to run basic EDA. The output will be written to /plots/fig/
4) Run shell.R in /forecast/ to simulate and plot forecast regions.
   The output will be written to /forecast/fig/
5) a) Run shell.assessment.R in /assessment/ to compute MSEs and MAPEs and produce plots.
   The output will be saved to /assessment/fig/
   b) Run shell.analysis.R in /assessment/ to produce density plots and a 2D t-SNE.
   The output will be saved to /assessment/
