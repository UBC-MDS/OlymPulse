# OlymPulse project proposal

## Motivation and purpose

Our role:
> International Olympic Committee Data Science Team (fictional role).

Target audience:
> Enthusiasts who want to know more about the Olympic Games medal winners.
> Countries' sports federations to gather insights from the historical data of its athletes.

Dashboard motivation:
> The Olympic Games are one of the most anticipated sports competitions for sports fans and the most important competition for athletes and countries' sports federations. The level of skills demonstrated by the athletes in their respective disciplines during this competition raises the bar for what a human being is capable of, challenging the impossible and astonishing people worldwide.

> Being able to access statistics and indicators on the performance of Olympic athletes is of utmost importance in the planning of resources for sports federations, for these institutions it is necessary to understand what are the disciplines that have given the best results to their country and which ones have a great opportunity for improvement.

> As sports fans, knowing the statistics of our favorite athletes and sports is necessary to analyze a game or competition, as well as to be able to chat and debate with other sports enthusiasts.

## Description of the data

We will visualize a historical dataset on the modern Olympic Games athletes, including both Summer and Winter games from Athens 1896 to Rio 2016. The dataset contains 271,116 registers including both medal winners and non-winners. However, we will focus on athletes who won an Olympic medal (39,783 records).

The dataset is made up of 15 variables, both categorical and numerical, these variables represent attributes associated with the athlete, the competition, and the discipline in which the athlete participated. Below is a summary of the variables of the dataset:

|variable |class     |description |
|:--------|:---------|:-----------|
|id       |double    | Athlete ID |
|name     |character | Athlete Name |
|sex      |character | Athlete Sex |
|age      |double    | Athlete Age |
|height   |double    | Athlete Height in cm|
|weight   |double    | Athlete weight in kg |
|team     |character | Country/Team competing for|
|noc      |character | noc region |
|games    |character | Olympic games name |
|year     |double    | Year of olympics |
|season   |character | Season either winter or summer |
|city     |character | City of Olympic host |
|sport    |character | Sport |
|event    |character | Specific event |
|medal    |character | Medal (Gold, Silver, Bronze or NA) |

Attribution:

> The data set is public and can be found in [tidytuesday](https://github.com/rfordatascience/tidytuesday).

> [olympics.csv](https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv): follow this link to access to the source dataset.
