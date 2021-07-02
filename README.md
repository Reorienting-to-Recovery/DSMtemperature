<img src="man/figures/cvpia_logo.jpg" align="right" width="40%"/>

### Modeled Temperature Data for the CVPIA SIT Model

*This data package contains modeled temperature data for each of the watersheds within the CVPIA salmon life cycle model.*

#### Installation

``` r
# install.packages("remotes")
remotes::install_github("CVPIA-OSC/DSMtemperature")
```

#### Usage
This package provides temperature related datasets to the [`fallRunDSM,`](https://github.com/CVPIA-OSC/fallRunDSM)
[`springRunDSM,`](https://github.com/CVPIA-OSC/springRunDSM) [`winterRunDSM,`](https://github.com/CVPIA-OSC/winterRunDSM) and [`latefallRunDSM`](https://github.com/CVPIA-OSC/latefallRunDSM) packages.

``` r
# datasets within the package
data(package = 'DSMtemperature')

# explore temperature modeling metadata
?DSMtemperature::stream_temperature
```

#### About the Models
Temperature inputs to the CVPIA Decision Support Model (DSM) were developed using one of the following methods:    
1. HEC5Q water temperature model
2. Measured water temperatures
3. Correlation between measured air and water temperatures
4. Water temperature from the closest, most hydrologically and geomorphically similar watershed with available data. 

The HEC5Q model accepts flow inputs from the CALSIM II water resources system operations planning model. Watershed specific methods are detailed on the Reference tab. 

### Dependencies
The `DSMTemperature` package provides data for several other packages within the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). These relationships are visualized in the dependency graph below. 

<img src="man/figures/dependencyChain.svg" width="100%"/>
   
<div style="margin-top: 40px;">Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/></div>
