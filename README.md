# Personal project
**Main goal:** The development of an investment model based on behavioural finance papers that identified how, using momentum and overreaction patterns, it is possible to predict the future trend of stock prices. 

From those papers, I developed the following strategy: selling past winners and buying past losers. Winners and losers were identified based on specific momentum patterns observed in the price movements of the U.S. equity market. I have uploaded a PDF file titled “Investment Strategy”, in which I detail how I identified the momentum patterns and generated the investment signals for selling or buying.

First, I identified momentum patterns based on price and volume. Subsequently, I trained and fine-tuned various supervised learning models to predict future returns using these momentum patterns and other market data.

## Project Organization
```
.
├── Data analysis/                      : contains html template for flask app
│   ├── Clustering - Long Strategy.R
│   ├── Clustering - Short Strategy.R
│   └── Statistical Analysis.R
├── Data collection/                     : Contains R files for data collection
│   ├── Bloomberg Equity Data.R          : Downloading Stock data using the Bloomberg API
│   └── Bloomberg Market Data.R          : Downloading Economic and Market data using the Bloomberg API
├── Data preparation and preprocessing/  : contains html template for flask app
│   ├── Final Dataset.R
│   ├── Index Data.R
│   └── Investment Signals.R
├── Models/                             : contains html template for flask app
│   ├── Logistic-Models.R
│   └── Random Forest Model.R
├── Investment Strategy.pdf             : Survival Analysis kaplan-Meier curve, log-rank test and Cox-proportional Hazard model
└── README.md                           : Report
```
