# Quantifying the relationship between climatic indicators and leptospirosis incidence in Fiji: a modelling study

Last update 10th Jan 2023

This repository contains the data and code used to produce the results presented in "Quantifying the relationship between climatic indicators and leptospirosis incidence in Fiji: a modelling study". Weekly and monthly case data can be found in the `data` folder (`data\weeklyCaseDate.csv` and `data\monthlyData.csv`). Case data with climatic variables can also be found in this folder:
* `data\casesClimDatElisa.rds` - weekly case data with weekly climate data at different lags. Climate data has been standardised.
* `data\casesClimDatNonStdElisa.rds` - weekly case data with weekly climate data at different lags. Data has not been standardised.
* `data\casesClimDatMonth.rds` - monthly case data with monthly climate data at different lags. Climate data has been standardised.


The folder `weeklyModels` contains the code used to create the weekly models. The file `models.R` includes the code for the weekly models used within the paper. These models are saved within the folder `savedModels`. The file `modelOutputs.R` contains the code used to produce the weekly figures and tables. The tables are saved within the `savedModelOutputs` folder. The file called `modelOutputsSupp5.R` is the code used to created supplementary figure 5 which uses the raw climate data that has not been standardised. 

The folder `monthlyModels` contains the code used to create the monthly models. The file `models.R` includes the code for the monthly models used within the paper. These models are saved within the folder `savedModels`. The file `modelOutputs.R` contains the code used to produce the monthly figures and tables. The tables are saved within the `savedModelOutputs` folder. 

