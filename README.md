# Personal project
**Main goal:** The development of an investment model based on behavioural finance papers that identified how, using momentum and overreaction patterns, it is possible to predict the future trend of stock prices. From those papers, I developed investment signals for buying or selling based on specific momentum patterns observed in the price movements of the U.S. equity market.

First, I identified momentum patterns based on price and volume. Subsequently, I trained and fine-tuned various supervised learning models to predict future returns using these momentum patterns and other market data.

## Project Organization
```
.
├── Data analysis/                         
│   ├── Clustering - Long Strategy.R       : Cluster analysis for buy signals
│   ├── Clustering - Short Strategy.R      : Cluster analysis for sell signals
│   └── Statistical Analysis.R             : Data analysis and visualization
├── Data collection/                       
│   ├── Bloomberg Equity Data.R            : Downloading Stock data using the Bloomberg API
│   └── Bloomberg Market Data.R            : Downloading Economic and Market data using the Bloomberg API
├── Data preparation and preprocessing/    
│   ├── Final Dataset.R                    : Merging economic and market data sets with equity data / Feature engineering and final data transformations
│   ├── Index Data.R                       : Preprocessing and transformation of economic and market data sets
│   └── Investment Signals.R               : Identifying investment signals / merging, cleaning, transformation, and filtering of equity data sets
├── Models/                               
│   ├── Logistic-Models.R                  : Logistic regression model and model evaluation
│   └── Random Forest Model.R              : Random Forest model and model evaluation
└── README.md                              : Project Description
```
