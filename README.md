[![shiny-deploy](https://github.com/UBC-MDS/OlymPulse/actions/workflows/deploy-app.yaml/badge.svg)](https://github.com/UBC-MDS/OlymPulse/actions/workflows/deploy-app.yaml) [![Testing workflow](https://github.com/UBC-MDS/OlymPulse/actions/workflows/testing.yaml/badge.svg)](https://github.com/UBC-MDS/OlymPulse/actions/workflows/testing.yaml)

# OlymPulse 🥇

## Welcome, sports enthusiast! 🤸🏽‍♂️ 🏊🏻 🤽🏾‍♀️

We are happy that you are interested in exploring the history of the medal-winning countries in the most important sports competition worldwide, the Olympic Games. In this app you will find a summary of the Olympic performance of countries throughout history.

## Table of contents

- [Motivation](#motivation)
- [Explore the app](#explore-the-app)
- [Description](#description)
- [About the data](#about-the-data)
- [Installation](#installation)
- [Contributing](#contributing)

## Motivation

The Olympic Games are one of the most anticipated sports competitions for sports fans and the most important competition for athletes and countries' sports federations. The level of skills demonstrated by the athletes in their respective disciplines during this competition raises the bar for what a human being is capable of, challenging the impossible and astonishing people worldwide.

Being able to access statistics and indicators on the performance of Olympic athletes is of utmost importance in the planning of resources for sports federations, for these institutions it is necessary to understand what are the disciplines that have given the best results to their country and which ones have a great opportunity for improvement.

As sports fans, knowing the statistics of our favorite athletes and sports is necessary to analyze a game or competition, as well as to be able to chat and debate with other sports enthusiasts.


## Explore the app

You can access the deployed app on [shinyapps.io here](https://tetrahydrofuran.shinyapps.io/olympulse/)!


## Description

![](img/Olympulse_demo_2.gif)

The app contains two tabs:

1. `Country Level Overview` includes an interactive map that allows the users to click into each country and view the country's records in both Summer and Winter Olympic Games, furthermore, showcases the most successful sport for each country in the history of the Olympic Games. In the side panel, there is a double-sided slider that allows the users to select the range of years they are interested in from 1896 to 2016. There are three menus that enable the users to select their country of interest (as an alternative way to using the interactive map), their sport of interest, and their season of interest (Summer or Winter). With the options selected, the users can view the trend (a line chart) in the total number of medals over the given period of time, the top five medal-winning years, and the medal count by type.

2. `Medal Tally Breakdown` similar to the first tab, includes a double-sided slider that allows the users to select the range of years in addition to three menus that enable the users to select their country of interest, their season of interest, the medal type, sport and event. This section contains a table showing the medals by sport and a treemap showing the main sports.

## About the data

This was created using a historical dataset on the modern Olympic Games athletes, including both Summer and Winter games from Athens 1896 to Rio 2016. The dataset contains 271,116 registers including both medal winners and non-winners. However, we will focus on athletes who won an Olympic medal (39,783 records).

The data set is public and can be found in [tidytuesday](https://github.com/rfordatascience/tidytuesday). Follow this link to access to the source dataset [olympics.csv](https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv).

## Installation

To install `OlymPulse` locally, you can do as follows:

1. Clone this repository to your local directory.

2. Install all the packages required to run this app by executing the following command in your R console:

    ``` bash
    install.packages(c("shiny", "ggplot2", "tidyverse", "plotly", "leaflet", "leaflet.extras", "sf", "countrycode", "RColorBrewer", "treemapify", "bslib", "shinycssloaders", "shinytest2"))
    ```

3. After installing the packages, execute the following command to run the app:

        RScript app.R

4. Copy the address and paste it in your browser to load the dashboard.

## Contributing

Interested in contributing? We are glad you are interested, please check out the [contributing guidelines](https://github.com/UBC-MDS/OlymPulse/blob/main/CONTRIBUTING.md). Please note that this project is released with a Code of Conduct. By contributing to this project, you agree to abide by its terms.

## License

`OlymPulse` was created by Raul Aguilar Lopez, Manvir Kohli and Crystal Geng. The materials of this project are licensed under the MIT License. If re-using/re-mixing please provide attribution and link to this webpage.

## Contributors

Crystal Geng, Manvir Kohli, Raul Aguilar

<a href="https://github.com/UBC-MDS/OlymPulse/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=UBC-MDS/OlymPulse" />
</a>
