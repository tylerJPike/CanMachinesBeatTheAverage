# Combining forecasts: Can machines beat the average?
This repository holds the replication files for experiments from:

Tyler Pike and Francisco Vazquez-Grande. **Combining forecasts: Can machines beat the average?** September 2020. [\[working paper\]](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3691117)

  
Abstract: Yes. This paper documents the benefits of combining forecasts using weights built with non-linear models.  We introduce our tree-based forecast combinations and compare them with benchmark equal weight combination as well as other nonlinear forecast weights. We find that nonlinear models can improve consistently upon the equal weight alternative -- breaking the so-called "forecast combination puzzle" -- and that our proposed methods compete well with other nonlinear methods.

## Run instructions
Everything needed to run the experiments and appendix material of the paper should be included in this repo. All one needs to do to replicate the work is:   
1. The required directory structure should be pulled with the project. However, if you need to re-instantiate output directories that begin empty, run [set_up.sh](./Scripts/set) (if in unix).
2. Open [nonLinear_master.R](./Scripts/nonLinear_master.R), which will act as the control board for all experiments, and  
    a. set the working directory  
    b. ensure you have all required packages installed, listed at the top of the script, excluding policyPlot, an FRB specific charting package  
    c. set the version number of the experiment   
    d. select wich experiments you would like to run by switching flow control variables to TRUE   
3. Source the [nonLinear_master.R](./Scripts/nonLinear_master.R) file

## Notes
1. File dependencies and outputs are listed in [nonLinear_master.R](./Scripts/nonLinear_master.R)
2. The use of multi-core processing is highly recommended, and is supported, otherwise the recusive forecast estimation will take a very long time (i.e. weeks)
3. RAM considerations should not be a problem given the relatively small sample size used, however, be careful is using parallel processing
4. Charting output relies on the use of an FRB specific package called policyPlot, and therefore will not be accessible to those outside of the Board's computing enviorment
5. Additionally, we provide our pre-trained [FFORMA model](./Data/Models), but one may train their own with [nonLinear_FFORMA_training.R](./Scripts/nonLinear_FFORMA_training.R). See the [fforma R package]{https://github.com/pmontman/fforma} for more details.

## Related work
Related time series forecasting examples with these techniques:
1. Forecasting country- and state-level covid-19 infections and deaths with FFORMA [\[kaggle\]](https://www.kaggle.com/tylerpike/covid-19-forecasting-fforma)
2. Forecasting walmart store-level product sales in the M5 copmetition [\[kaggle\]](https://www.kaggle.com/tylerpike/m5-forecasting-fforma)
