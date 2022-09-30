# Western US Water Systems Model (WWSM)
A model of water resources of the Western US designed to support climate-resilient electricity system planning

This repository archives the Western U.S. Water Systems Model (WWSM) created within the WEAP software platform and documented in the manuscript in submission:

D.Yates, J.K. Szinai, and A.D. Jones. (202X). Modeling the Water Systems of the Western US to Support Climate-Resilient Electricity System Planning. (In submission).

# Abstract

Electricity and water systems in the Western US (WUS) are closely connected, with hydropower comprising up to 80% of generation, and electricity related to water comprising up to 20% of electricity use in certain states. Because of these interdependencies, the serious threat of climate change to WUS resources will likely have compounding electricity impacts, yet water system models rarely estimate energy implications, especially at the geographic scale of the expansive WUS water and electricity networks. This study, therefore, develops a WUS-wide water system model with a particular emphasis on estimating climate impacts on hydropower generation and water-related energy use, which can be linked with a grid expansion model to support climate-resilient electricity planning. The water system model combines climatically-driven physical hydrology and management of both water supply and demand allocation, and is applied to an ensemble of 15 climate scenarios out to 2050. Model results show decreasing streamflow in key basins of the WUS under most scenarios. Annual electricity use related to water increases up to 4%, driven by growing agricultural demand and shifts to energy-intensive groundwater to replace declining surface water. Total annual hydropower generation changes by +5% to -20% by mid-century, but declines in most scenarios. Energy use increases coincide with hydropower generation declines, suggesting additional energy capacity may be needed to achieve WUS grid reliability and decarbonization goals, and demonstrating the importance of concurrently evaluating the climate signal on both water-for-energy and energy-for-water.

# Software Download

The Western U.S. Water Systems Model (WWSM) was developed within the WEAP (Water Evaluation And Planning) software platform. WEAP is developed by the research non-profit Stockholm Environment Institute's (SEI) U.S. Center. 

1. To view the WWSM and see the results, you must first download and install WEAP software from https://www.weap21.org/
An evaluation version of the WEAP model is available for free without a license. This evaluation version allows users to open and view the WWSM, and view the saved results from the climate scenarios associated with the above manuscript in submission. To run new scenarios and create new results with different model parameters, a WEAP license is needed and must be acquired through the WEAP website from SEI. A free tutorial on using the WEAP software is available on the website. 

2. Once you have downloaded the WEAP software, download the zipped "backup" WWSM model file (WWSM_V1_0) from the model folder in this repository. Open the backup file with the WEAP software to unzip the model, input data, and saved results.

3. When the WWSM_V1_0 file is unzipped by WEAP, the model's input data files will also be unzipped to the WEAP Areas folder on your computer, including climate data for the scenarios (LOCA downscaled) and observed data for calibration (observed hydropower generation, streamflow gage data, reservoir gage data). Climate data originates from 

# Results
1. Review the results from the 15 climate scenarios for the 2010 - 2060 period by clicking the Results panel within the WEAP software. 

2. We have output the relevant results from the WEAP software into csv files that are saved in the data\results folder that is created when you unzip the backup WWSM model file. You can also rerun the VBA scripts within the WWSM to recreate the csvs.

# Figures

1. The R script "XXX" is used to create Figures X, X, X, X, X, with the data output from WEAP into the csv files in the X folder




