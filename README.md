# 420-Data-Simulation
Functions for simulating data for the PSY 420 class at UT Austin

## Description
In our PSY420 class, students propose an experiment for their final projects. Once the proposals are acceptable, we simulate data sets for the students based upon what they proposed (and what we consider reasonable outcomes. This is as opposed to having the students try to actually do the experiments, which would be a logistical and practical nightmare.

These functions are written to generate some of the most commonly data types and formats.

## Functions

### grouped data

* **grpData** - single factor multiple group data
* **twoIndGrpData** - wrapper for grpData make two group data more easily
* **pairedT** - two group within-subjects group data
* **combine** (in combineDFs.R) - stack data frames and add a factor to tag the original DFs

### continuous x-y data

* **regData** - x-y data; user specifies slope and y-intercept
* **regDataCor** - x-y data; user specifies correlation

### x-y data with sequential evenly spaced x-values

* **regDataSeqX** - x-y data with sequential evenly spaced x-values; user specifies slope and y-intercept
* **regDataSeqXCor** - x-y data with sequential evenly spaced x-values; user specifies correlation

## Function specifics (work in progress)

### Grouped data (data with a factor specifying conditions)

#### n independent groups

#### 2 independent groups

#### 2 groups repeated measures

#### combine data frames

### continuous x-y data

### x-y data with sequential evenly spaced x-values
