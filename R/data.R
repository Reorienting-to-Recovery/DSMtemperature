#' Monthly Mean Water Temperature
#' @description A named list containing the 1980-2000 monthly mean water temperatures in °C.
#' Each named list element is a temperature datasets of Hec5q and other modeled temps corresponding to 2008-2009 and 2018-2019 Calsim II BiOp runs.
#'
#' The named list components are:
#' \itemize{
#'     \item{biop_2008_2009: monthly stream temperature in degrees C for watersheds from the 2008-2009 Calsim II BiOp run.}
#'     \item{biop_itp_2018_2019: monthly stream temperature in degrees C for watersheds from the 2018-2019 Calsim II BiOp & ITP run.}
#'     \item{run_of_river: monthly stream temperature in degrees C for watersheds from the Run of River Calsim run.}
#' }
#'
#' @format A named list with two components, each containing a 3 dimensional array [31 watersheds, 12 months, 21 years]
#'
#' @details The following four methods were used to estimate the monthly mean water
#' temperature for the watersheds during 1979-1999:
#'
#'
#' \strong{HEC-5Q} \cr
#' Temperature assignments were determined by Mike Wright using the input files
#' for the three HEC-5Q runs, i.e. AR_5QCS.dat and AR_5Q-CL.OUT.
#'
#'
#' \strong{Empirical Data} \cr
#' Only the Lower Sacramento had sufficient measured water temperature during
#' the period of the R2R salmon life cycle model. The few missing values were
#' imputed using \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}.
#'
#'  \strong{Double Regression Modeling} \cr
#'  Data from many temperature gauges was collected from several R2R watersheds.
#'  In these watersheds, a double regression technique was employed to predict water
#'  temperature based on equilibrium temperature and the river mile of interest.
#'  Equilibrium temperature is the temperature a pool of water would approach if
#'  left in contact with the air for an infinite time, generally equivalent to the
#'  air temperature. The results were applied to both watersheds of origin and as
#'  surrogate data for ungauged watersheds.
#'
#'  \href{https://cvpiatemperature-r-package.s3-us-west-2.amazonaws.com/Temperature_Regression_Model_for_CVPIA_Streams.pdf}{Modeling Details}
#'
#' \strong{Additional Temperature Modeling} \cr
#' For streams without major dams, water temperature is highly correlated with air temperature.
#' For each watershed not included in the HEC-5Q model that had partial water temperature data,
#' a linear model was fitted to estimate water temperature as a function of air temperature.
#'
#' Generally, the air temperature record spans both the period of the R2R salmon life cycle model
#' and the complete period of record of available water temperature data. In the
#' cases where there were missing air temperature values between 1979-1999, we imputed the missing values using
#' \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}
#' in order to have a complete air temperature dataset for water temperature estimation.
#'
#'  Air Temperature Data Source:
#'  \itemize{
#'    \item NOAA Climate Data Online \href{https://www.ncdc.noaa.gov/cdo-web/}{(CDO)} accessed using the \href{https://github.com/ropensci/rnoaa}{\code{rnoaa}} R package developed by \href{https://ropensci.org/}{rOpenSci}.
#'    }
#'
#'  Stream Water Temperature Data Sources:
#'  \itemize{
#'    \item DWR California Data Exchange Center \href{http://cdec.water.ca.gov/index.html}{(CDEC)} accessed using the \href{https://github.com/flowwest/cdecretrieve}{\code{CDECRetrieve}} R packaged developed by \href{http://www.flowwest.com/}{FlowWest}.
#'    \item USGS National Water Information System \href{https://waterdata.usgs.gov/nwis}{(NWIS)} accessed using the \href{https://github.com/USGS-R/dataRetrieval}{\code{dataRetrieval}} R package developed by \href{https://www.usgs.gov/}{USGS}.
#'  }
#'
#' @section Watershed Modeling Details:
#' \itemize{
#'    \item \strong{Upper Sacramento River}
#'      \itemize{
#'        \item{biop_2008_2009: HEC-5Q model output}
#'        \item{biop_itp_2018_2019: HEC-5Q model output}
#'        \item{run_of_river: Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=jlf}{JLF} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}}
#'    }
#'
#'    \item \strong{Antelope Creek} Regression method was fitted to data from in-river temperature gauges
#'    \item \strong{Battle Creek}
#'      \itemize{
#'        \item{biop_2008_2009: HEC-5Q model output}
#'        \item{biop_itp_2018_2019: HEC-5Q model output}
#'        \item{run_of_river: Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=bnf}{BNF} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024257/detail}{USW00024257}}
#'    }
#'    \item \strong{Bear Creek} Regression method was fitted to data from in-river temperature gauges
#'    \item \strong{Big Chico Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{http://cdec4gov.water.ca.gov/dynamicapp/QueryF?s=BIC}{BIC} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00046685/detail}{USC00046685}
#'    \item \strong{Butte Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BCD}{BCD} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00046685/detail}{USC00046685}
#'    \item \strong{Clear Creek} HEC-5Q model  output at IGO in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Cottonwood Creek} HEC-5Q model output at COTTONWOOD CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Cow Creek} HEC-5Q model  output at COW CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Deer Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11383500}{11383500} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00041715/detail}{USC00041715}
#'    \item \strong{Elder Creek} Regression method was fitted to data from Antelope River temperature gauges
#'    \item \strong{Mill Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11381500}{11381500} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}
#'    \item \strong{Paynes Creek} Regression method was fitted to data from Antelope River temperature gauges
#'    \item \strong{Stony Creek} HEC-5Q model  output at STONY CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Thomes Creek} HEC-5Q model output at THOMES CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Upper-mid Sacramento River}
#'     \itemize{
#'       \item{biop_2008_2009: HEC-5Q model output}
#'        \item{biop_itp_2018_2019: HEC-5Q model output}
#'        \item{run_of_river: Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=rdb}{RDB} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}}
#'    }
#'    \item \strong{Sutter Bypass} Tisdale Rotary Screw Trap monthly mean water temperature 2011-2017
#'    \item \strong{Bear River} Regression method was fitted to data from Deer Creek temperature gauges
#'    \item \strong{Feather River} Regression method was fitted to data from Deer Creek temperature gauges
#'    \item \strong{Yuba River} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11421000}{11421000} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}
#'    \item \strong{Lower-mid Sacramento River}
#'      \itemize{
#'        \item{biop_2008_2009: HEC-5Q model output}
#'        \item{biop_itp_2018_2019: HEC-5Q model output}
#'        \item{run_of_river: Estimated mean monthly water temperature from a mean monthly water temperature linear model fitted with water temperature from USGS Gage ID:\href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11391100}{11391100} with imputed missing values using \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}}
#'      }
#'    \item \strong{Yolo Bypass} Knights Landing Rotary Screw Trap monthly mean water temperature 2004-2018
#'    \item \strong{American River} HEC-5Q model output at WILLIAM POND PARK in SSJB_AR_Reference_063015/AR/AR_CL_Temp.dss
#'    \item \strong{Lower Sacramento River} Measured mean monthly water temperature from  USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11447650}{11447650} with imputed missing values using \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}
#'    \item \strong{Calaveras River} Regression method was fitted to data from Deer Creek temperature gauges
#'    \item \strong{Cosumnes River} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11335000}{11335000} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023271/detail}{USW00023271}
#'    \item \strong{Mokelumne River} Estimated mean monthly water temperature from a linear model fitted with water temperature data provided by EBMUD measured near Victor, CA and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00045032/detail}{USC00045032}
#'    \item \strong{Merced River}
#'        \itemize{
#'            \item{biop_2008_2009: HEC-5Q model output}
#'            \item{biop_itp_2018_2019: HEC-5Q model for San Joaquin is no longer supported. Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage Id: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CRS}{CRS} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00045532/detail}{USC00045532}}
#'            \item{run_of_river: used same water to air temperature regression data as biop_itp_2018_2019}
#'         }
#'    \item \strong{Stanislaus River}
#'         \itemize{
#'            \item{biop_2008_2009: HEC-5Q model output at BLW MCHENRY BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS}
#'            \item{biop_itp_2018_2019: HEC-5Q model for San Joaquin is no longer supported. Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage Id: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=RPN}{RPN} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023258/detail}{USW00023258}}
#'            \item{run_of_river: used same water to air temperature regression data as biop_itp_2018_2019}
#'         }
#'    \item \strong{Tuolumne River}
#'          \itemize{
#'            \item{biop_2008_2009: HEC-5Q model output at GEER ROAD BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS}
#'            \item{biop_itp_2018_2019: HEC-5Q model for San Joaquin is no longer supported. Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage Id: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=MOD}{MOD} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023258/detail}{USW00023258}}
#'            \item{run_of_river: used same water to air temperature regression data as biop_itp_2018_2019}
#'         }
#'    \item \strong{San Joaquin River}
#'          \itemize{
#'            \item{biop_2008_2009: HEC-5Q model output at ABV TUOL in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS}
#'            \item{biop_itp_2018_2019: HEC-5Q model for San Joaquin is no longer supported. Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage Id: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=CRS}{CRS} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00045532/detail}{USC00045532}}
#'            \item{run_of_river: used same water to air temperature regression data as biop_itp_2018_2019}
#'         }
#' }
#'
#'
#' @source
#' \itemize{
#'   \item HEC-5Q Model Output and Double Regression Temperature Modeling: Michael Wright \email{mwright@@usbr.gov}
#'   \item Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}
#' }
#'
"stream_temperature"

#' Degree Days
#'
#' @description A named list containing the monthly accumulated degree days °C.
#' Each named list element is a temperature datasets of Hec5q and other modeled temps corresponding to 2008-2009 and 2018-2019 Calsim II BiOp runs.
#'
#' The named list components are:
#' \itemize{
#'     \item{biop_2008_2009: degree days for watersheds from the 2008-2009 Calsim II BiOp run.}
#'     \item{biop_itp_2018_2019: degree days for watersheds from the 2018-2019 Calsim II BiOp & ITP run.}
#' }
#'
#' @format A named list with two components, each containing a 3 dimensional array [31 watersheds, 12 months, 21 years]
#'
#' @details
#' For watersheds with HEC-5Q modeled results, the calculation for degree days
#' is the sum of the daily mean values for each month.
#'
#' For the other regions, the calculation for degree days is the monthly mean
#' temperature multiplied by the number of days in the month.
#'
#' For more details about the temperature modeling see \code{\link{stream_temperature}}
#'
#' @source
#' \itemize{
#'   \item HEC-5Q Model Output and Double Regression Temperature Modeling: Michael Wright \email{mwright@@usbr.gov}
#'   \item Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}, Erin Cain \email{ecain@@flowwest.com}
#' }
#'
"degree_days"

#' Delta Temperature
#'
#' @description The 1980-2000 rearing temperature in the North and South Delta in °C
#' @format a 3 dimensional array [12 months, 21 years, 2 deltas]
#'
#' @details
#' Water temperature in the Deltas were modeled as a function of air temperature.
#'
#' North Delta was modeled using a sample of recent measured water temperature at
#' CDEC station \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=EMM}{EMM}
#' and NOAA CDO air temperature data from \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00040232/detail}{Antioch}.
#'
#' The North Delta is defined as the area west of and including the Sacramento River below Freeport to Chips Island.
#'
#' South Delta was modeled using a sample of recent measured water temperature at
#' CDEC station \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=MTB}{MTB}
#' and NOAA CDO air temperature data \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023237/detail}{Stockton Airport}.
#'
#' The South Delta is defined as the area east of the Sacramento River below Freeport to Chips Island and the San Joaquin River
#' below Vernalis.
#'
#' Air Temperature Data Source:
#'  \itemize{
#'    \item NOAA Climate Data Online \href{https://www.ncdc.noaa.gov/cdo-web/}{(CDO)} accessed using the \href{https://github.com/ropensci/rnoaa}{\code{rnoaa}} R package developed by \href{https://ropensci.org/}{rOpenSci}.
#'    }
#'
#'  Stream Water Temperature Data Sources:
#'  \itemize{
#'    \item DWR California Data Exchange Center \href{http://cdec.water.ca.gov/index.html}{(CDEC)} accessed using the \href{https://github.com/flowwest/cdecretrieve}{\code{CDECRetrieve}} R packaged developed by \href{http://www.flowwest.com/}{FlowWest}.
#'  }
#'
#' @source
#' Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}
#'
"delta_temperature"

#' Migratory Corridor Water Temperature
#'
#' @description The median proportion of days over 20°C per month
#'
#' @format a 31 by 12 matrix [watershed by month]
#'
#' @details
#' Watersheds were assigned membership to one of the following three locations to
#' represent the migratory corridor temperature conditions. The data was summarised
#' by calculating the median proportion of days over 20°C for each month during the available period of record.
#'
#' Sacramento River, \href{http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=WLK}{Sacramento River Below Wilkins Slough}:
#' Upper Sacramento River, Antelope Creek, Battle Creek, Bear Creek, Big Chico Creek Butte Creek, Clear Creek, Cottonwood Creek,
#' Cow Creek, Deer Creek, Elder Creek, Mill Creek, Paynes Creek, Stony Creek, Thomes Creek, Bear River, Feather River, Yuba River
#' American River
#'
#' South Delta, \href{http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=MOK}{Mokelumne River At San Joaquin River}:
#' Calaveras River, Cosumnes River, Mokelumne River
#'
#' San Joaquin River, \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11303500}{San Joaquin River Near Vernalis}:
#' Merced River, Stanislaus River, Tuolumne River
#'
#' No spawning in these regions (assigned value of zero):
#' Upper-mid Sacramento River, Sutter Bypass, Lower-mid Sacramento River, Yolo Bypass, Lower Sacramento River, San Joaquin River
#'
#' @source
#' \itemize{
#'   \item Method developed by James T. Peterson \email{jt.peterson@oregonstate.edu}
#'   \item Data wrangling by Sadie Gill \email{sgill@@flowwest.com}
#' }
#'
"migratory_temperature_proportion_over_20"

#' Temperature Effect on Egg Mortality
#' @description The mean estimate of temperature effect used for egg to fry survival
#' @format  A dataframe with 4 columns (\code{watershed}, \code{fall_run}, \code{spring_run}, \code{winter_run}), and 31 rows (a row for each watershed).
#' @details
#' Fall and Spring Run values are the same and were estimated by C. Hammersmark (CBEC ECOengineering Inc.).
#' These Fall and Spring Run values were calculated by taking the mean of dry and wet egg temp effects from previous model.
#' Winter run values were calculated through model calibration.
#'
"egg_temperature_effect"


#' Vernalis Water Temperature
#' @description Monthly water temperature (°C) at Vernalis from 1980-2000
#' @format A matrix 12x21 [12 months by 21 years]
#' @details Temperature at Vernalis was developed as a function of air
#' temperature. The most recent water temperature at San Joation River (SJR)
#' \href{https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=SJR}{CDEC: RRI} was used for water temperature. Air temperature was obtained using
#' NOAA CDO at \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023237/detail}{Stockton Metropolitan Airport}.
"vernalis_temperature"


#' Prisoner's Point Water Temperature
#' @description Monthly water temperature (°C) at Prisoner's Point from 1980-2000
#' @format A matrix 12x21 [12 months by 21 years]
#' @details Temperature at Prisoner's Point was developed as a function of air
#' temperature. The most recent water temperature at Rough and Ready Island
#' \href{https://cdec.water.ca.gov/dynamicapp/staSearch?sta_chk=on&sta=RRI&sensor=211&collect=NONE+SPECIFIED&dur=&active=&lon1=&lon2=&lat1=&lat2=&elev1=-5&elev2=99000&nearby=&basin=NONE+SPECIFIED&hydro=NONE+SPECIFIED&county=NONE+SPECIFIED&agency_num=160&display=sta}{CDEC: RRI} was used for water temperature. Air temperature was obtained using
#' NOAA CDO at \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023237/detail}{Stockton Metropolitan Airport}.
"prisoners_point_temperature"

