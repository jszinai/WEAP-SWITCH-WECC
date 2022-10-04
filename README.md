# Western US Water Systems Model (WWSM)
A model of water resources of the Western US designed to support climate-resilient electricity system planning

This repository archives the Western U.S. Water Systems Model (WWSM) created within the WEAP software platform and documented in the manuscript in submission:

D.Yates <sup>1</sup>, J.K. Szinai <sup>2,3</sup>, and A.D. Jones <sup>2,3</sup>. (202X). Modeling the Water Systems of the Western US to Support Climate-Resilient Electricity System Planning. (In submission).

<sup>1</sup> National Center for Atmospheric Research

<sup>2</sup> Lawrence Berkeley National Lab

<sup>3</sup> Energy and Resources Group, UC Berkeley

Data reference:
https://doi.org/10.5281/zenodo.543244816

# Abstract

Electricity and water systems in the Western US (WUS) are closely connected, with hydropower comprising up to 80% of generation, and electricity related to water comprising up to 20% of electricity use in certain states. Because of these interdependencies, the serious threat of climate change to WUS resources will likely have compounding electricity impacts, yet water system models rarely estimate energy implications, especially at the geographic scale of the expansive WUS water and electricity networks. This study, therefore, develops a WUS-wide water system model with a particular emphasis on estimating climate impacts on hydropower generation and water-related energy use, which can be linked with a grid expansion model to support climate-resilient electricity planning. The water system model combines climatically-driven physical hydrology and management of both water supply and demand allocation, and is applied to an ensemble of 15 climate scenarios out to 2050. Model results show decreasing streamflow in key basins of the WUS under most scenarios. Annual electricity use related to water increases up to 4%, driven by growing agricultural demand and shifts to energy-intensive groundwater to replace declining surface water. Total annual hydropower generation changes by +5% to -20% by mid-century, but declines in most scenarios. Energy use increases coincide with hydropower generation declines, suggesting additional energy capacity may be needed to achieve WUS grid reliability and decarbonization goals, and demonstrating the importance of concurrently evaluating the climate signal on both water-for-energy and energy-for-water.

# Software Download for WWSM

The Western U.S. Water Systems Model (WWSM) was developed within the WEAP (Water Evaluation And Planning) software platform. WEAP is developed by the research non-profit Stockholm Environment Institute's (SEI) U.S. Center. 

1. To view the WWSM and see the results, you must first download and install WEAP software from https://www.weap21.org/
An evaluation version of the WEAP model is available for free without a license. This evaluation version allows users to open and view the WWSM, and view the saved results from the climate scenarios associated with the above manuscript in submission. To run new scenarios and create new results with different model parameters, a WEAP license is needed and must be acquired through the WEAP website from SEI. A free tutorial on using the WEAP software is available on the website. 

2. Once you have downloaded the WEAP software, download the zipped "backup" WWSM model file (WWSM_WEAPSwitch_V1_0.WEAP) from this repository. Open the backup file with the WEAP software to unzip the model, input data, and saved results.

3. The input and result csv files that pertain to the model and are included in the zipped backup file are also saved under the data folder of this repo for easier access without the need to download the WEAP software.

# Input data files
When the WWSM_WEAPSwitch_V1_0.WEAP file is unzipped by WEAP, the model's associated input data files will also be unzipped to the "WEAP Areas" folder on your computer, including climate data for the scenarios (LOCA downscaled) and observed data for calibration (observed hydropower generation, streamflow gage data, reservoir gage data). These data files are also available in the data folder in this repo.

data\ClimateLOCA:

Climate data originates from an archive of Downscaled CMIP5 climate projections, based on LOCA downscaling: https://gdodcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html

data\Streamflow and data\gages and data\Reservoir:

Data for calibration of streamflow, reservoirs, and statewide water use originates from the USGS Water Data for the Nation: https://waterdata.usgs.gov/nwis

data\Reservoir\Monthly_historical_hydropower_generation:

Hydropower data for calibration originates from the EIA: https://www.eia.gov/electricity/data/eia923/

# Saved Results
1. Under the "Manage Scenarios" option in WEAP, check the boxes for all the 15 climate scenarios and RefLOCA scenario. Then review the results from the 15 climate scenarios and Reference scenario (RefLOCA) for the 2010 - 2060 period by clicking the Results panel within the WEAP software. Different result variables have been saved under the "Favorites" menu.

2. We have output the relevant results from the WEAP software into csv files that are saved in the data\Results folder that is created when you unzip the backup WWSM model file. These csvs are also in the data\Results folder in this repo. You can also rerun the VBA scripts within the WWSM in the WEAP software (Advanced -> Scripting -> Run Scripts) to recreate the csvs.

# Results Figures

1. Post_process_and_plot_WWSM_water_hydropower_energy_resultsV1_0.R:
This R script is used to create Figures 4C, 6A, 6B, 6C, 6D, 7B, 7C, 8, and 9 with the data output from WEAP into the csv files in the data
\Results folder, and 2 additional Excel files "Mapping_WEAP_Objects_V1_0.xlsx" and "WEAP_SWITCH_mapping_with_avg_annual_generation.csv" that map some attributes to the WEAP results for easier aggregation and plotting. The data for Figure 4C is in the "GCM_summary.xlsx" file saved in this repository.





