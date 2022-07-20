
## CalSim Data Source

Amanda Becker from Reclamation provided FlowWest with the following two 
DV outputs from CALSIM on 07/19: 

1) 2008/2009 Biop (COS_rev19_Q0_JanEstExpCVP.dss)
2) 2019 BiOp and 2020 ITP (HistBenchmark_041822_DV.dss)

These files can be located in the `data-raw` folder. Excel outputs for nodes of interst from these two files are also located in the  `data-raw` folder. 

## Data Wrangling Methodology

FlowWest mapped locations of interest to CalSim nodes using the [Calsim Schematic](https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg). We then used HEC-DSSVue to extract the following CalSim Nodes for DS TWG: 

*Delta Outflow Node:* C407  

*Sacramento River Flow at Freeport:* C400

*OMR Flows:* C408

*SWP Export:*  DEL_SWP_TOTAL
*CVP Export:*  DEL_CVP_TOTAL

*Reservoir Levels:* 

* Trinity Lake = S1 
* Shasta Lake = S4
* Orville Lake = S6 
* Folsom Lake = S8 

FlowWest exported these relevant nodes into a excel document (accessible in the `data-raw` folder) and reformatted it into the following structure for both Calsim runs: 

**Data Dictionary:** 

* **date**: Calsim Date
* **calsim_node**: CalSim node for each location of interest
* **location**: Location name COMPASS requested mapped based on calsim_node
* **value**: CalSim value output for node values describe either flow or storage 
* **units**: Units of value (cubic feet per second (cfs) for flow records and total acre feet (TAF) for storage records) 
* **calsim_run**: CalSim Run ("2008 2009 Biop" and "2019 Biop 2020 ITP") 

Code used to reformat and view data is found in `CALSIM_wrangling.R` script. 

Final reformatted data objects are named: 

1) `calsim_2008_2009_biop.csv`
2) `calsim_2019_biop_2020_itp.csv` 

And saved in the `data` folder. 
