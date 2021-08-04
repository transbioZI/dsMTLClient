# dsMTLClient: dsMTL client site functions

## Introduction
dsMTL (Federated Multi-Task Learning based on DataSHIELD) provided federated, privacy-preserving MTL analysis. dsMTL was developed based on DataSHIELD, an ecosystem supporting the federated analysis of sensitive individual-level data that remains stored behind the data owner’s firewall throughout analysis. Multi-task Learning (MTL) aimed at simultaneously learning the outcome (e.g. diagnosis) associated patterns across datasets with dataset-specific, as well as shared, effects. MTL has numerous exciting application areas, such as comorbidity modeling, and has already been applied successfully for e.g. disease progression analysis.  

<p align="center"> 
<img src="inst/overview.png" style="width: 70%; height: 70%"/>​
</p>

dsMTL currently includes three supervised and one unsupervised federated multi-task learning as well as one federated machine learning algorithms. Each algorithm captured a specific form of cross-cohort heterogeneity, which was linked to different applications in molecular studies.

| Name  | Type | Task | Effect |
| --- | --- | --- | --- |
| `dsLasso`  | ML | Classification/Regression | Train a Lasso model on the conbained cohorts |
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


## Test dsMTL functions
To test dsMTL client-side functions. Three steps must be performed.

1, Install two DataSHIELD servers and dsMTL server-side package. Please find the tutorial in the server-side repositary [dsMTLBase](https://github.com/transbioZI/dsMTLBase). The testing files were based on two-server scenario

2, Upload and import [simulation datasets](https://github.com/transbioZI/dsMTLClient/tree/main/inst/simuData) into the DataSHIELD server. Five datasets were provided for all five dsMTL algorithms. Specific to prediction task, the data of regression and classification tasks was provided in parallel. The tutorial of server management can be found [here](https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/12943477/Opal+management)

3, Download and run the testing file [here](https://github.com/transbioZI/dsMTLClient/tree/main/tests). For each algorithm, we provided codes for testing the optimization solvers with different opinions, multiple training procedures of the model as well as the cross-validatin.








