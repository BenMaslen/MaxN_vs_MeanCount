# README

This respository contains the code and data that produced the results in the paper titled "MaxN vs MeanCount: Using AI to evaluate population abundance metrics in underwater monitoring videos" (currently in review).

In order to obtain the results in the paper, download the zipped respoitory, add a folder in the repository titled 'results' and run the code files in the following order (note that `Goal_3_data_prep.R` takes a while to run as it is performing block bootstraps (2-10 hours depending on your machine, and Simulation_Goal_4.R is quite computationally intensive and as such should be run on high performance computing - which can be done using the `goal_4_sims` file).  :

1. `Goal_1_2_4_data_prep.R`
2. `Goal_3_data_prep.R`
3. `Simulation_Goal_1.R`
4. `Simulation_Goal_2.R`
5. `Simulation_Goal_3.R`
6. `Simulation_Goal_4.R`
7. `Goal_1_results.R`
8. `Goal_2_results.R`
9. `Goal_3_results.R`
10. `Goal_4_results.R`
11. `Goal_4_plotting.R`

Within each data and code folder, contains a '30_seconds' and '60_seconds' folder where the analysis can be repeated using a subsampled estimate of MeanCount calculated at 30 and 60 seconds respectively.



