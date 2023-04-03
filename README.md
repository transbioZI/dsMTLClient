# dsMTLClient: dsMTL client site functions

## Introduction
dsMTL (Federated Multi-Task Learning based on DataSHIELD) provided federated, privacy-preserving MTL analysis. dsMTL was developed based on DataSHIELD, an ecosystem supporting the federated analysis of sensitive individual-level data that remains stored behind the data owner’s firewall throughout analysis. Multi-task Learning (MTL) aimed at simultaneously learning the outcome (e.g. diagnosis) associated patterns across datasets with dataset-specific, as well as shared, effects. MTL has numerous exciting application areas, such as comorbidity modeling, and has already been applied successfully for e.g. disease progression analysis.  

<p align="center"> 
<img src="inst/overview.png" style="width: 70%; height: 70%"/>​
</p>

dsMTL currently includes three supervised and one unsupervised federated multi-task learning as well as two federated machine learning algorithms. Each algorithm captured a specific form of cross-cohort heterogeneity, which was linked to different applications in molecular studies.

| Name  | Type | Task | Effect |
| --- | --- | --- | --- |
| `dsLasso`  | ML | Classification/Regression | Train a Lasso model on the conbained cohorts |
| `dsLassoCov`  | ML | Classification/Regression | Federated ML model that can capture the covariate effect |
| `dsMTL_L21`  | MTL | Classification/Regression | Screen out unimportant features to all tasks |
| `dsMTL_trace`  | MTL | Classification/Regression | Identify models represented in low-dimentional spcae |
| `dsMTL_net`  | MTL | Classification/Regression | Incorporate task-relatedness described as a graph |
| `dsMTL_iNMF`  | MTL | Matrix factorization | Factorize matrices into shared and specific components |


## Server-side package
The server side package can be found: [dsMTLBase](https://github.com/transbioZI/dsMTLBase)


## Installation

```r
install.packages("devtools")
library("devtools")
install_github("transbioZI/dsMTLClient")
```


## Run dsMTL functions

dsMTL server-side package has been pre-installed in the [opal demo server](https://opal-demo.obiba.org/). Thus the most convenient way to test dsMTL functions is using opal demo server as the back end. If you want to use dsMTL in real applications, please follow the tutorial to install dsMTL server-side package on your server. The simulation datasets based on two-server scenario were provided. For each algorithm, we provided codes for testing the optimization solvers with different opinions, multiple training procedures of the model as well as the cross-validatin.

### Run using opal demo server

The testing files were [here](https://github.com/transbioZI/dsMTLClient/tree/main/tests/opal-demo). Please download the file for each algorithm and run line by line.

### Run using DSLite
DSLite is a R package allowing the simulation of DataSHIELD servers environment on a single machine. We provided tests files for every algorithm running with DSLite. The files are located [here](https://github.com/transbioZI/dsMTLClient/tree/main/tests/DSLite)

### Run using own servers
1. Install two DataSHIELD servers and dsMTL server-side package. Please find the tutorial in the server-side repositary [dsMTLBase](https://github.com/transbioZI/dsMTLBase). 
2.  Upload and import [simulation datasets](https://github.com/transbioZI/dsMTLClient/tree/main/inst/simuData/opal-demo) in your servers. Please find the tutorial in the server-side repositary [dsMTLBase](https://github.com/transbioZI/dsMTLBase). 
3.  Download and run the testing files [here](https://github.com/transbioZI/dsMTLClient/tree/main/tests/opal-demo/). 





## Contact

Han Cao (hank9cao@gmail.com)



## Useful links
1. dsMTLBase - federated, privacy-preserving machine-learning and multi-task learning analysis: https://github.com/transbioZI/dsMTLBase
2. Documents of opal servers: https://opaldoc.obiba.org/en/latest/index.html
3. Tutorial of DataSHIELD for beginers: https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/12943395/Beginners+Hub
4. Forum of DataSHIELD: https://datashield.discourse.group/
5. opalr - an R package for managing DataSHIELD server from script: https://cran.r-project.org/web/packages/opalr/index.html
6. resources - an R package for importing data of different sources: https://opaldoc.obiba.org/en/latest/resources.html
7. Tutorial of resources: https://rpubs.com/jrgonzalezISGlobal/tutorial_resources
8. dsOmics - an R package based on DataSHIELD for omics analysis: https://github.com/isglobal-brge/dsOmics
9. Tutorial of omics analysis using dsOmics: https://rpubs.com/jrgonzalezISGlobal/tutorial_DSomics
10. Tutorial of omics analysis using dsOmics2： https://htmlpreview.github.io/?https://github.com/isglobal-brge/dsOmicsClient/blob/master/vignettes/dsOmics.html
11. A book of DataSHIELD book with detailed explainations of esential packages: https://isglobal-brge.github.io/resource_bookdown/  
