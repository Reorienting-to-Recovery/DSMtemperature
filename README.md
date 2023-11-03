### Modeled Temperature Data for the Reorienting to Recovery Model

*This data package contains modeled temperature data for each of the watersheds within the Reorienting to Recover (R2R) salmon life cycle model.*

#### Installation

``` r
# install.packages("remotes")
remotes::install_github("reorienting-to-recovery/DSMtemperature")
```

#### Usage
This package provides temperature related datasets to the [`fallRunDSM,`](https://github.com/reorienting-to-recovery/fallRunDSM)
[`springRunDSM,`](https://github.com/reorienting-to-recovery/springRunDSM) and [`winterRunDSM`](https://github.com/reorienting-to-recovery/winterRunDSM) packages.

``` r
# datasets within the package
data(package = 'DSMtemperature')

# explore temperature modeling metadata
?DSMtemperature::stream_temperature
```

#### About the Models
Temperature inputs to the Reorienting to Recovery models were developed using one of the following methods:   

1. HEC5Q water temperature model
2. Measured water temperatures
3. Correlation between measured air and water temperatures
4. Water temperature from the closest, most hydrologically and geomorphically similar watershed with available data. 

The HEC5Q model accepts flow inputs from the CALSIM II water resources system operations planning model. The DSMtemperature package contains HEC5Q from two distinct model runs: 2008-2009 CalSim II run and 2018-2019 CalSim II BiOp runs. Watershed specific methods are detailed on the Reference tab. 

### Dependencies
The `DSMTemperature` package provides data for several other packages within the [Reorienting to Recovery Organization](https://github.com/reorienting-to-recovery). These relationships are visualized in the dependency graph below. 

<img src="man/figures/dependencyChain.svg" width="100%"/>
   
<div style="margin-top: 40px;">Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="man/figures/TransLogoTreb.png" width="150px"/></div>
