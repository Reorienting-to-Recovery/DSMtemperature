# DSMtemperature V1.0 for R2R

The Reorienting to Recovery and 2022 update to DSMtemperature includes the following changes: 

* **CALSIM updates**. Updated `stream_temperature` and `degree_days` datasets include new 2019 biop flows. The result of
doing so means accessing each temperature dataset must be done using the following format: 
`stream_temperature$biop_2008_2009`, `stream_temperature$biop_itp_2018_2019`, and `run_of_river`.

* **BiOp: Modeling updates:** The Hec5q models associated with the 2019 biop no longer support the San Joaquin river and its tributaries. We had to do additional temperature modeling to make up for these data gaps. Data for Merced River, Stanislaus River, Tuolumne River, and the San Joaquin River is now modeled using an air to water linear regression. 

* **Run of River: Modeling updates:** The Hec5q models associated with the Run of River bookend scenario includes Clear Creek, Cottonwood Creek, Stony Creek, Cow Creek and the American River. FlowWest performed air to temperature regression to fill data gaps for the Lower-mid Sacramento River, Upper-mid Sacramento River, Upper Sacramento River, and Battle Creek. 2019 Hec5q BiOp temperature was used for Thomes Creek due to lack of instream temperature data for a regression. 

Temperature related datasets for use with the following DSM models:

* [Fall Run DSM (v3.0)](https://github.com/CVPIA-OSC/fallRunDSM/releases/tag/v3.0)
* [Late Fall Run DSM (v2.0)](https://github.com/CVPIA-OSC/fallRunDSM/releases/tag/v2.0)
* [Winter Run DSM (v3.0)](https://github.com/CVPIA-OSC/winterRunDSM/releases/tag/v3.0)
* [Spring Run DSM (v3.0)](https://github.com/CVPIA-OSC/springRunDSM/releases/tag/v3.0)

# DSMtemperature 1.0

Temperature related datasets for use with the following DSM models:

* [Fall Run DSM (v2.0)](https://github.com/CVPIA-OSC/fallRunDSM/releases/tag/v2.0)
* [Late Fall Run DSM (v1.0)](https://github.com/CVPIA-OSC/fallRunDSM/releases/tag/v1.0)
* [Winter Run DSM (v2.0)](https://github.com/CVPIA-OSC/winterRunDSM/releases/tag/v2.0)
* [Spring Run DSM (v2.0)](https://github.com/CVPIA-OSC/springRunDSM/releases/tag/v2.0)

# DSMtemperature 0.0.1

Alpha release of temperature input data migrated from CVPIA-OSC/DSMtemperature
