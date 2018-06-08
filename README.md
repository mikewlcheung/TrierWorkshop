# [Big Data in Psychology 2018](https://conferences.leibniz-psychology.org/index.php/bigdata/bigdata2018) post-conference workshop: Testing Model-Driven Hypotheses with Big Data with R
* By Mike Cheung
   
## Goals of this workshop
* Introduce the Split/Analyze/Meta-analyze (SAM) Approach to test model-driven hypotheses with R;
* Run the proposed approach with two real data sets;
* Exchange ideas on the best way to analyze Big Data in psychology. 

## Download the materials of this workshop from Github: https://github.com/mikewlcheung/TrierWorkshop
    
## Installing the R packages
* R can be downloaded at http://www.r-project.org/.

```
## Installing the required R packages from the CRAN
for (i in c("dplyr", "lavaan", "semPlot", "lme4", "metaSEM")) {
  if (!(i %in% rownames(installed.packages()))) install.packages(i)
}
```

* Participants are expected to have a basic knowledge in R.

## Acknowledgment
* This workshop is based on the joint work with [Suzanne Jak](http://www.suzannejak.nl/):
    + Cheung, M. W.-L., & Jak, S. (2016). Analyzing big data in psychology: A split/analyze/meta-analyze approach. *Frontiers in Psychology*, *7*(738). https://doi.org/10.3389/fpsyg.2016.00738
