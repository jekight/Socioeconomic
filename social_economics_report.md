---
title: "Social Economic Visualizations"
author: 'By: Jeremy Kight'
date: "12/12/2019"
output:
  html_document:
    keep_md: true
    toc: true
    toc_depth: 4
    toc_float: yes
---


***

# Introduction


|        In an attempt for the government to improve public access to datasets, the U.S. General Services Administration, Technology Transformation Service, starting managing and hosting Data.gov. This website provides access to over 180,000 datasets available for conducting research or just practicing data cleaning and visualizations. This report will be using the "County-level Data Sets" provided by the Economic Research Service, found [here](https://catalog.data.gov/dataset/county-level-data-sets). More specifically, the datasets that will be used for analysis are: Unemployment, Population Estimates, and Education. These datasets provide an excellent opportunity for data cleaning and visualizations. This report will describe one way to transform and visualize the data to see how certain social economic factors differ across the United States.


# Procedure

|        Since the objective of this project was to visualize various datasets and compare the results across different states in the United States, the procedure is relatively straight forward. Below are the steps used to carry out this project. If need be, please refer to the code at the end of the document to see how these steps were applied. 

<br>

Step 1: Download the three files from  [Data.gov](https://catalog.data.gov/dataset/county-level-data-sets) and place them in the directory created for this project.  
<br>
Step 2: Delete the first few rows containing contact information in each excel sheet.  
<br>
Step 3: Load in the appropriate libraries needed for this project.  
<br>
Step 4: Read in the excel files into RStudio.   
<br>
Step 5: Observe the unemployment dataset to see any irregularities or errors.    
<br>
Step 6: Fix the error in the unemployment dataset regarding Colorado.    
<br>
Step 7: Convert the "State" column to a factor.    
<br>
Step 8: Remove any columns that are unnecessary.    
<br>
Step 9: Filter the state and county data within the unemployment dataset.   
<br>
Step 10: Convert the "county_fips" variable to a character type.    
<br>
Step 11: Repeat steps 5 through 10 for the population and education datasets.   
<br>
Step 12: In the education dataset, rename the columns with spaces, commas, and numbers to make formatting easier.   
<br>
Step 13: Prep the mapping data using the urbanmapr package. Create new datasets for state and county data.   
<br>
Step 14: Plot the data across all of the states.    
<br>
Step 15: Filter out GA, OH, and TX and use bar graphs and scatterplots to compare the various quantaties.  
<br>
Step 16: Use urbanmapr to show the differences in the counties in the states GA, OH, and TX.    


<br>

<br>

## State Visualizations


|        The driving force behind this project was to visualize the state and county data and see how the different states compare, with respect to unemployment, population, and education data. This section contains static quantaties, as well as, rates over specific periods of time. To Calculate the rate of change of the various variables, the following formula was use:

$$Rate\:of\:Change = \frac{old - new}{old}*100\%$$

Below is the compilation of the different plots created to visualize the data in this project. Urbnmapr and ggplot were two packages used for visualizations. To see the codes used to generate the following plots, please refer [here](# My Code).  

<br>

|       The first plot that will be created will be the percent of change in the unemployment rate from 2010 to 2018. By observing **Figure 1** below, one can see that the unemployment rate changed for the better over the years. The darker states indicate a 20% to 40% drop in unemployment rate, while the lighter shaded states had a drop of 50% to 60%! Also, another pattern that can be noticed is that the central states are shaded darker compared to the coastal states. Now, there are numerous factors that effect unemployment rates and the datasets used in this report do not contain enough information to describe exactly why the unemployment rate dropped greater amounts in some regions compared to others. However, there may be enough data to identify some connections among the variables.   
<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-2-1.png" alt="Figure 1: Unemployment Rates 2010 to 2018."  />
<p class="caption">Figure 1: Unemployment Rates 2010 to 2018.</p>
</div>
<br>

|        By comparing **Figure 1** to **Figure 2**, one can see that people moved to where the jobs were. **Figure 2** shows the change in civilian labor force, where the lighter shades represent very little change and the darker shades of green show areas where the labor force increased about 10% to 15%. Thus, since the labor force has risen over the years, one could assume that the population of those states should also have the highest increase as well. The figures below will test this.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-3-1.png" alt="Figure 2: The rate of change regarding civilian labor force from 2010 to 2018."  />
<p class="caption">Figure 2: The rate of change regarding civilian labor force from 2010 to 2018.</p>
</div>
<br>

**Figure 3** is a static visualization to show where the densly populated areas are in the United States. California is the most populated, with Texas, Florida and New York coming in next. 

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-4-1.png" alt="Figure 3: Population estimate of each state in 2018."  />
<p class="caption">Figure 3: Population estimate of each state in 2018.</p>
</div>
<br>

From 2010 to 2018, the coastal states, as well as, the western states had the best unemployment numbers by far. Then it was observed that those areas also had the largest increase in civilian labor force. Now, by observing **Figure 4**, one can see that the same areas are also growing the fastest. It appears that people do migrate where the jobs are. Of course, there are numerous other reasons such as state taxes, etc. but there is definitely a connection.

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-5-1.png" alt="Figure 4: The rate of change of the states' population from 2010 to 2018."  />
<p class="caption">Figure 4: The rate of change of the states' population from 2010 to 2018.</p>
</div>
<br>

|        So far, the graphs have shown how unemployment numbers differ across the nation, as well as, how they change over the years. The change in population appears to be connected with the unemployment, at least in the sense that people tend to live where there are jobs available. The argument that there are more jobs available where more people live can also be made. Now this report will analyze the education levels among the states. 


<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-6-1.png" alt="Figure 5: The amount of people with a high school diploma or less in 2017."  />
<p class="caption">Figure 5: The amount of people with a high school diploma or less in 2017.</p>
</div>
<br>

|        **Figure 5** displays the states with the largest amount of people with a high school diploma or less. While **Figure 6** shows the states with the most people who have at least a Bachelors degree. While neither **Figure 5** or **Figure 6** give any important insights about the data, the plots do serve a purpose. Both plots demonstrate the the sheer amount of people the densly populated areas contain need to be taken into account when trying to find connections or trends in the data. Both static plots show that the areas with the most people with a high school education or less are the same areas with the most people with at least a bachelors degree. Instead of diplaying the number of people in each category, a rate showing how these numbers have changed over time will give much more valuable information.


<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-7-1.png" alt="Figure 6: The amount of people with at least a Bachelors degree in 2017."  />
<p class="caption">Figure 6: The amount of people with at least a Bachelors degree in 2017.</p>
</div>
<br>

|        There is a much more interesting story to tell when a percent of change is used compared to quantaties of people. **Figure 7** visualizes the change in the amount of people with at a high school diploma or less. The darker shades represent an increase, where as, the lighter shades represent a decrease in the amount of people with a high school diploma or less. It is important to note that both figures range from the year 2000 to 2017. Which is a different range compared to figures 1, 2, and 4. What is interesting about **Figure 7** is that it does resemble figures 2 and 4. Meaning that the areas where the population and civilian labor force grew, so did the amount of people with a high school diploma or less. Another important aspect of **Figure 7** is that the scales include negative percentages. This is to account for the areas where less people were stopping their education at the high school level.

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-8-1.png" alt="Figure 7: Percent of change for people with high school diploma or less from 2000 to 2017."  />
<p class="caption">Figure 7: Percent of change for people with high school diploma or less from 2000 to 2017.</p>
</div>
<br>

|        The result shown in **Figure 8** indicate that people are becoming more likely to pursue higher education. In fact, there was not a state in the United States where less people were obtaining at least a bachelors degree. Now if **Figure 8** is compared to **Figure 1**, one can clam that the unemployment rate has decreased the most in areas where the education level seem to be rising. To see how this effects the median income of the states, please see **Figure 9**.

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-9-1.png" alt="Figure 8: Percent of change for people with a bachelors degree or higher from 2000 to 2017."  />
<p class="caption">Figure 8: Percent of change for people with a bachelors degree or higher from 2000 to 2017.</p>
</div>
<br>

|        Figures 1 through 9 tell an interesting story, but in no way is this report saying that the story is complete. For example, a state such as California, seems to be leading the way in all the categories so far. California had one of the biggest drops in unemployment rate from 2010 to 2018. California was also in the top half in both change in civilain labor force and population. It was also noted to be one of the largest states in both geographical terms and population quantites. California had more people stopping their education at the high school level but also saw an increase in the amount of people obtaining at least a bachelors degree. Lastly, California had one of the highest median household incomes in 2017, as seen in **Figure 9**. These plots do not take into consideration immigration or migration numbers, nor do they consider the cost of living in that state. It is well known that California is one of the most expensive states to live in, thus, households need to have a higher income to afford living there. Climate also plays a factor in where people choose to live. Again, all of this is to say that these plots are just to notice differences in specific domains.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-10-1.png" alt="Figure 9: The average median household income in 2017."  />
<p class="caption">Figure 9: The average median household income in 2017.</p>
</div>
<br>

## Filtered State Plots

|        Now the focus of this report will shift to three specific states: Georgia, Ohio, and Texas. By filtering and narrowing the amount of observations, more in depth comparisons can be drawned about relative states. In this case, Georgia, Ohio, and Texas were chosen simply out of curiousity. Also, residents of specific states tend to have a better or deeper understanding of their particular state. Georgia, Ohio, and Texas are special because that is where this particular aurthor has lived over the years.





<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-13-1.png" alt="Figure 10: Bar graph representing the population of people in 2018."  />
<p class="caption">Figure 10: Bar graph representing the population of people in 2018.</p>
</div>
<br>

|        The first graph that shows the three filtered states' characteristics is a bar graph representing the population of people in the year 2018. **Figure 10** shows that Georgia and Ohio have very similar populations, while Texas is nearly three times their size! This is not necessarily surprising since Texas is one of the largest and most populated state in the United States. However, what is surprising is the results in **Figure 11**. 


<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-14-1.png" alt="Figure 11: Line graph representing the change in population of people from 2010 to 2018."  />
<p class="caption">Figure 11: Line graph representing the change in population of people from 2010 to 2018.</p>
</div>
<br>

In **Figure 11**, the rate of change in population is plotted as a scatterplot ranging from 2010 to 2018. So not only is Texas the most populated out of the three states, it is also the fastest growing. According to the next figure below, **Figure 12**, Texas is also the state with the most unemployed people. As mentioned earlier, a plot showing the amount of people will not give a lot of valuable information. What would be more interesting and valuable is to see the rate in which the amount of people unemployed changes.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-15-1.png" alt="Figure 12: Bar graph representing the amount of unemployed people in 2018."  />
<p class="caption">Figure 12: Bar graph representing the amount of unemployed people in 2018.</p>
</div>
<br>

|        Both **Figure 13** and **Figure 14** show how the unemployment numbers change. **Figure 13** shows how the amount of people unemployed have change from 2010 to 2018. Over this span of years, more and more people have been able to find jobs. However, it appears in the year 2014 there was a slight increase in the number of people unemployed. It seems that Georgia was more resilent to this increase in unemployment than Ohio and Texas. While Texas was the most volatile. 

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-16-1.png" alt="Figure 13: Line graph representing the change in unemployed people from 2010 to 2018."  />
<p class="caption">Figure 13: Line graph representing the change in unemployed people from 2010 to 2018.</p>
</div>
<br>

Here, **Figure 14** communicates the same thing as **Figure 13**. Except **Figure 14** shows the unemployment rate. This plot is very interesting. Georgia had the biggest drop in unemployment rate from the years 2010 to 2018. However, this could be due to Georgia starting with the largest unemployment rate out of the three states. Thus, Georgia does not seem to be as resilent to unemployment as it appeared in the plots before. 

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-17-1.png" alt="Figure 14: Line graph representing the change in the unemployment rate from 2010 to 2018."  />
<p class="caption">Figure 14: Line graph representing the change in the unemployment rate from 2010 to 2018.</p>
</div>
<br>

|        In this next section of analysis, the education level of Georgia, Ohio, and Texas are plotted together to compare them. First, in **Figure 15**, the amount of people in the year 2000 in each level of education are plotted. One interesting bit of information is that Ohio and Texas nearly have the same amount of individuals! With Texas being so populated, this is quite impressive. However, looking at **Figure 16**, Ohio clearly has the highest percentage of people with a high school diploma. Apart from the high school level, the rest of the education levels appear to be relatively close. Another interesting conclusion is that while Texas has the most at each level, according to **Figure 16**, Texas seems to be the least educated state out of the group. Texas even has the highest percentage of adults with no high school diploma.

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-18-1.png" alt="Figure 15: Bar graph representing the education levels in 2000."  />
<p class="caption">Figure 15: Bar graph representing the education levels in 2000.</p>
</div>
<br>

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-19-1.png" alt="Figure 16: Bar graph representing the percent of education levels in 2000."  />
<p class="caption">Figure 16: Bar graph representing the percent of education levels in 2000.</p>
</div>
<br>

|        The previous plots showed how the states compared in the year 2000. Now the states will be analyzed with data from 2017. **Figure 17** shows the amount of people in each education level. One significant change from the year 2000 to 2017 is that Ohio has reduced the amount of people with no high school diploma significantly. In terms of percentages, as shown in **Figure 18**, Ohio is leading the way in terms of a baseline education for its citizens. Also, Georgia has the highest percentage of higher education individuals. 

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-20-1.png" alt="Figure 17: Bar graph representing the education levels in 2017."  />
<p class="caption">Figure 17: Bar graph representing the education levels in 2017.</p>
</div>
<br>


<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-21-1.png" alt="Figure 18: Bar graph representing the percent of education levels in 2017."  />
<p class="caption">Figure 18: Bar graph representing the percent of education levels in 2017.</p>
</div>

## Filtered County Plots

|        The final part of this analysis will be dealing with the county data for Georgia, Ohio, and Texas. The first plot, **Figure 19** shows the population estimates of the counties in 2018. The results came out as expected, meaning all the densest populated areas are located around the major cities.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-22-1.png" alt="Figure 19: Population estimate of the counties in 2018"  />
<p class="caption">Figure 19: Population estimate of the counties in 2018</p>
</div>
<br>

Now looking **Figure 20**, the unemployment rate distribution can be observed among the counties in the states in the year 2018. Both Texas and Ohio had the greatest unemployment rate in their southeast counties. However, Georgia experience the greatest unemployment rate in its central counties. Interestingly, non of the counties with the largest cities were shown to have the highest unemployment rate. Remember earlier, that the idea was that people moved to where the jobs were. While this may be true on a state level, on the county level it does not appear to be true.   

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-23-1.png" alt="Figure 20: Unemployment rate of the counties in 2018"  />
<p class="caption">Figure 20: Unemployment rate of the counties in 2018</p>
</div>
<br>

|        The next few plots demonstrate the amount of people in each education level each county has. **Figure 21** and **Figure 22** show the education level in the year 2000. While **Figure 23** and **Figure 24** show the same levels, except in the year 2017. By observing the four plots, one can see no much changes. Since the cities have more people, their respective counties will have a greater volume of educated and uneducated people. It is interesting to visually see that most of the statistics come from only a few counties. 

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-24-1.png" alt="Figure 21: Amount of people without a high school diploma in 2000."  />
<p class="caption">Figure 21: Amount of people without a high school diploma in 2000.</p>
</div>
<br>

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-25-1.png" alt="Figure 22: Amount of people at least a Bachelors degree in 2000."  />
<p class="caption">Figure 22: Amount of people at least a Bachelors degree in 2000.</p>
</div>
<br>


<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-26-1.png" alt="Figure 23: Amount of people without a high school diploma 2017."  />
<p class="caption">Figure 23: Amount of people without a high school diploma 2017.</p>
</div>
<br>


<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-27-1.png" alt="Figure 24: Amount of people with at least a Bachelors degree 2017."  />
<p class="caption">Figure 24: Amount of people with at least a Bachelors degree 2017.</p>
</div>
<br>

|        **Figure 25** gives the change in unemployment rate from 2010 to 2018 for all the counties in GA, OH, and TX. Here, the darker shades represent a 30% drop in unemployment rate and the lighter shades represent a 70% drop. By referring back to **Figure 14**, one can see that Georgia had the greatest change in unemployment rate while Texas had the least amount of change. This result starts to make sense after seeing **Figure 25**. In Georgia, the counties containing its largest cities was were the greatest drop in unemployment was. Whereas, Texas and Ohio was pretty evenly distributed across the states.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-28-1.png" alt="Figure 25: Change in the unemployment rate from 2010 to 2018."  />
<p class="caption">Figure 25: Change in the unemployment rate from 2010 to 2018.</p>
</div>
<br>

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-29-1.png" alt="Figure 26: Percent of Change in Civilian Labor Force from 2010 to 2018."  />
<p class="caption">Figure 26: Percent of Change in Civilian Labor Force from 2010 to 2018.</p>
</div>
<br>

|        When it comes to where people are moving to, it still appears people follow where the jobs are. **Figure 25** show were people are able to find jobs, whereas, **Figure 26** shows the change in civilian labor force (or the people able to work). The counties with the greatest drop in unemployment rate were also the counties with the largest increase in civilian labor force.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-30-1.png" alt="Figure 27: Percent of Change in Population from 2010 to 2018."  />
<p class="caption">Figure 27: Percent of Change in Population from 2010 to 2018.</p>
</div>
<br>

Then by comparing **Figure 27** with both figures 25 and 26, it appears that job opportunities really does influence where people live. Some counties even had a growth of 80% over the eight year span.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-31-1.png" alt="Figure 28: Percent of change for people with high school diploma or less from 2010 to 2018."  />
<p class="caption">Figure 28: Percent of change for people with high school diploma or less from 2010 to 2018.</p>
</div>
<br>

|        In terms of education, the counties with the largest cities had a greater increase in the amount of people with a high school diploma or less. This can be seen in **Figure 28**. Then in **Figure29**, the percent of change for people with a bachelors degree or higher for the counties is displayed. While it doesn't appear that there was much change, by looking at the scale the darker shaded counties experience a 400% growth! Thus, most counties showed a dramatic increase in educated individuals. 

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-32-1.png" alt="Figure 29: Percent of change for people with a bachelors degree or higher from 2010 to 2018."  />
<p class="caption">Figure 29: Percent of change for people with a bachelors degree or higher from 2010 to 2018.</p>
</div>
<br>

|        Lastly, the median household income for the counties in 2017 are shown in **Figure 30**. In this plot, it is easy to see that the wealthiest households live near to cities. Again, this connects to the idea that people live where the jobs are. However, one thing to note is that individuals that make more money are less likely to report their salaries. Thus, this could skew the map. At the very least, it is just something to consider.  

<br>
<div class="figure" style="text-align: center">
<img src="social_economics_report_files/figure-html/unnamed-chunk-33-1.png" alt="Figure 30: The median household income for each county in 2017."  />
<p class="caption">Figure 30: The median household income for each county in 2017.</p>
</div>
<br>

# Conclusion

|        This report was created in order to visualize socioeconomic data of the states and counties of the United States. By visualizing the data, some connections were able to be identified. However, just by the massive amount of factors that influence socioeconomic data, these conclusions are not an end all identification. Nontheless, the first interesting detail indentified is that people migrate to areas where jobs are available. However, this seems to be intensified on the state level. On the county level, people are more spaced out because of their ability to drive to the larger cities for work.

<br>

|        Another aspect that was realized was that the percent of change in all of the variables provided more useful information. Since highly populated areas have more people, plots such as **Figure 21** become almost useless. Because a city like Houston, TX has such a large population. Trying to compare the education levels of other cities like Columbus, OH are always going to come up short, in terms of quantaties. However, when looking at percentages or rates, it becomes easier to draw more valid conclusions. For example, while Texas has a higher amount of individuals with a high school diploma, Ohio has a greater percentage of individuals with this education level. Texas also had a higher percentage of uneducated individuals compared to Ohio. 

<br>

|        This leads into another important idea: that subsetting data into smaller amounts allows for more in depth analysis. The county level data could have been plotted over the entire United States. However, it was more difficult to spot trends as compared to when the three states were filtered out. Of course, depending on the objective of the project, this approach may change.

<br>

|        Lastly, there is still more that can be done on this project. Immigration and migration numbers and be plotted to see how these variables play a role in socioeconomic data. Also, plots describing life expectancy could also be beneficial. Projects such as this one can be elaborated on much more but that depends on the objective. In this report, the objective was to transform the data in order to visualize socioeconomic data and analyze the results. The code to carry out the data transformation and visualization is in the section below.


# My Code

## Load in Appropriate Libraries


|        First, to perform analysis on the three datasets, Unemployment, Population Estimates, and Education, certain libraries need to be loaded into R. In this project, tidyverse, readxl, ggplot2, and urbnmapr will be utilized in some capacity, thus, those libraries need to be loaded in. Remember that before getting started, creating a project in RStudio will help organize all the files and set the appropriate working directory. 

<br>

```r
library(tidyverse)
library(readxl)
library(ggplot2)
library(urbnmapr)
```
<br>

## Load In Excel Files


|        First, download the three files from  [Data.gov](https://catalog.data.gov/dataset/county-level-data-sets) and place them in the directory created for this project. The next thing that should be done, is to open the three excel files and look at the spreadsheets. While the information in the first few rows are important, they will serve no purpose in the data analysis performed in this report. Thus, the rows before each table were removed in Excel (note that this could have been done in R as well). Once the files are saved in the appropriate location, they are ready to be loaded into RStudio. 

<br>

```r
unemployment <- read_excel("Unemployment.xls")
population <- read_excel("PopulationEstimates.xls")
education <- read_excel("Education.xls")
```
<br>

## Data Transformation

### Unemployment Data

|        Using RStudio is great for several reasons, however, one particular aspect that is quite useful is being able to see how large a dataset is within the working environment. In this case, it is easy to see that the unemployment dataset has 3275 observations and 56 variables (3275 rows and 56 columns). To see the actual dataset, run the code below. It should be noted that to see only a small portion of the dataset, then the head() function should be utilized.

<br>

```r
view(unemployment)
```
<br>

|        Looking at the unemployment dataset, it should be noticed that the view() function does not show the type of variables that the dataset contains. This can be checked with the str() function. 

<br>

```r
str(unemployment)
```
<br>

The results of the code above will not be shown in this report to save space, but it is recommended to run. Upon viewing the results, some important details that should be noticed are: the "FIPS" variable is a numeric vector and the "State" variable is a character vector instead of a factor. Before converting the "State" variable to a factor, check to see how many different factors the dataset contains.

<br>

```r
table(unemployment$State)
```

```
## 
##  AK  AL  AR  AZ  CA  Co  CO  CT  DC  DE  FL  GA  HI  IA  ID  IL  IN  KS  KY  LA 
##  33  68  76  16  59   1  64   9   2   4  68 160   5 100  45 103  93 106 121  65 
##  MA  MD  ME  MI  MN  MO  MS  MT  NC  ND  NE  NH  NJ  NM  NV  NY  OH  OK  OR  PA 
##  15  25  17  84  88 116  83  57 101  54  94  11  22  34  18  63  89  78  37  68 
##  PR  RI  SC  SD  TN  TX  US  UT  VA  VT  WA  WI  WV  WY 
##  79   6  47  67  96 255   1  30 134  15  40  73  56  24
```
<br>

Notice that there is a "Co" and "CO" factor. This is most likely a typo error that occurred when the data was being inputed. Before proceeding, this error needs to be dealt with. To do this, find the location of the "Co" value, replace the "Co" value with "CO" ,and then check to ensure the factors are correct.

<br>

```r
#Find the Co value
unemployment$State
```

```r
#Change Co to CO
unemployment$State[254] <- "CO"
```

```r
#Double Check
table(unemployment$State)
```

```
## 
##  AK  AL  AR  AZ  CA  CO  CT  DC  DE  FL  GA  HI  IA  ID  IL  IN  KS  KY  LA  MA 
##  33  68  76  16  59  65   9   2   4  68 160   5 100  45 103  93 106 121  65  15 
##  MD  ME  MI  MN  MO  MS  MT  NC  ND  NE  NH  NJ  NM  NV  NY  OH  OK  OR  PA  PR 
##  25  17  84  88 116  83  57 101  54  94  11  22  34  18  63  89  78  37  68  79 
##  RI  SC  SD  TN  TX  US  UT  VA  VT  WA  WI  WV  WY 
##   6  47  67  96 255   1  30 134  15  40  73  56  24
```
<br>

|        Next, convert the "State" column into a factor. Then, proceed to take out columns that are not going to be used in this analysis. This will help in decluttering the dataset. Of course, it would be a good habit to double check to ensure the code worked properly.


<br>

```r
#Convert State into a factor
unemployment$State <- as.factor(unemployment$State)

#Remove columns
unemployment <- select(unemployment, FIPS:Area_name, Civilian_labor_force_2010:Median_Household_Income_2017)
view(unemployment)
```
<br>

|        The unemployment dataset contains both county and state data. While they don't have to be separated into two different datasets, doing so will make graphing with urban maps a little easier. The easiest way to filter the state data from the county data is through the FIPS code. Notice that the state FIPS codes are the only ones divisable by 1000. Because the FIPS variable is numeric, this can be done easily with the code below.

<br>

```r
unemployment_state <- unemployment %>%
  filter(FIPS %% 1000 == 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_name")
```


```r
unemployment_county <- unemployment %>%
  filter(FIPS %% 1000 != 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_name")
```
<br>

|        From this point forward, only **unemployment_state** and **unemployment_county** will be used when dealing with unemployment data. There is one last thing that needs to be done with these datasets and that is to convert the FIPS codes into a character type.

<br>

```r
unemployment_state$county_fips <- as.character(unemployment_state$county_fips)

unemployment_county$county_fips <- as.character(unemployment_county$county_fips)
```
<br>


### Population Data

|        The same process will be taken with the population dataset. It is encouraged to peak at the dataset to see what kind of data it holds and what variables it contains. Here, view() is used but head() could have also been used. After looking at the dataset, check to see what type of variables are present. Then check if the **State** variable has any input errors, especially after seeing that this was the case with the unemployment dataset. The results of the following code will not be shown here to save space.

<br>

```r
view(population)
```


```r
str(unemployment)
```


```r
table(population$State)
```
<br>


|        Unlike the unemployment dataset, there are no errors present! However, since the **FIPS** variable is a character type and it needs to be numeric in order to filter the state and county data, it needs to be converted. Also, the **State** variable needs to be converted into a factor type as well. Once this is completed, any unnecessary columns should be removed from the dataset. The below code will take care of all of this.

<br>

```r
#Conver FIPS to numeric
population$FIPS <- as.numeric(population$FIPS)
```

```r
#Convert State to a factor
population$State <- as.factor(population$State)
```

```r
#Take out columns that aren't needed
population <- select(population, FIPS:Area_Name, POP_ESTIMATE_2010:POP_ESTIMATE_2018, Births_2010:Deaths_2018)
```
<br>

|        Now it is time to separate the state and county data. This will be done with the same procedure that was used when separating the unemployment data.

<br>

```r
population_state <- population %>%
  filter(FIPS %% 1000 == 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_Name")
```


```r
population_county <- population %>%
  filter(FIPS %% 1000 != 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_Name")
```
<br>

|        From this point forward, only **population_state** and **population_county** will be used when dealing with population data. Again, there is one last thing that needs to be done with these datasets and that is to convert the FIPS codes into a character type.

<br>

```r
population_state$county_fips <- as.character(population_state$county_fips)

population_county$county_fips <- as.character(population_county$county_fips)
```
<br>

### Education Data

|        Just as was done with the unemployment and population datasets, start the data transformation by viewing the education dataset to get an idea of what it entails. Then check the data types and **State** variable to make sure there were not any errors.

<br>

```r
view(education)
```


```r
str(education)
```


```r
table(education$State)
```
<br>

|        One thing that should have been noticed when looking at the columns in the education dataset was that the names were really complicated. The column names contained many spaces, commas, and numbers which would make using those variablesin R more difficult. So adjusting the names is highly recommended.

<br>

```r
#Adjust column names
names(education) <- str_replace_all(names(education), c(" " = "_", "," = "_", "-" = "_to_", "'"="", "__"  = "_"))
view(education)
```
<br>

|        Just as before, convert the **State** variable to a factor and change the **FIPS_Code** variable from a character variable type to a numeric variable type. Then, take out the columns that are not going to be used.

<br>

```r
#Conver FIPS to numeric
education$FIPS_Code <- as.numeric(education$FIPS_Code)
```


```r
#Convert State to a factor
education$State <- as.factor(education$State)
```


```r
#Take out columns that aren't needed
education <- select(education, FIPS_Code:Area_name,                Less_than_a_high_school_diploma_2000:Percent_of_adults_with_a_bachelors_degree_or_higher_2013_to_17)
```

|        Finally, separate the state and county data. Again, this will be done using the same procedure as before with the unemployment and population datasets. Once this is completed, only **education_state** and **education_county** will be used in the analysis. After the state and county data is separated, convert the FIPS codes back to character types.

<br>

```r
education_state <- education %>%
  filter(FIPS_Code %% 1000 == 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS_Code", state = "State", state_name = "Area_name")
```


```r
education_county <- education %>%
  filter(FIPS_Code %% 1000 != 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS_Code", state = "State", state_name = "Area_name")
```


```r
education_state$county_fips <- as.character(education_state$county_fips)
```


```r
education_county$county_fips <- as.character(education_county$county_fips)
```
<br>

### Mapping Data

|        This section encompasses how to set up the mapping data using the **urbnmapr** package. The urbnmapr package is really convient and makes plotting data on maps easy, without sacrificing quality. This package creates excellent state maps and county maps, depending on the type of data that needs to be visualized. Since the unemployment, population, and education datasets contain both state and county data, both types of maps will be required. To begin, create a dataset that contains the U.S. states mapping data from **urbnmapr**. Then, peak at the new dataset using the head() function.

<br>

```r
#Create blank map of the US states
blank_states <- get_urbn_map("states", sf = TRUE)
```


```r
head(blank_states)
```
<br>

|        Notice that the **state_abbv** variable contains the same information as the **state** variable in all three of the datasets created earlier. Since these datasets will need to be joined together for plotting the data, go ahead and change the variable name to match. Then convert the **state** variable to a factor.

<br>

```r
blank_states <- rename(blank_states, state = "state_abbv")
```


```r
blank_states$state <- as.factor(blank_states$state)
```
<br>

|        The same procedure will be used to create the county mapping dataset. Again, by creating these datasets now will save time later since many plots will be created. 

<br>

```r
#Create blank map of the US counties
blank_counties <- get_urbn_map("counties", sf = TRUE)
```


```r
#head(blank_map)
```


```r
blank_counties <- rename(blank_counties, state = "state_abbv")
```


```r
#Convert state to a factor
blank_counties$state <- as.factor(blank_counties$state)
```


```r
blank_counties <- arrange(blank_counties, county_fips)
```
<br>

## State Visualizations

### USA Data Plots

|        Now that all the data is transformed and the mapping data is loaded in, it is now time to start visualizing the data. Again, this will be done using the **urbanmapr** package. Below are the codes used to create each plot shown earlier. Remember, in order to plot the percent of change, the following equation must be used:    

$$Rate\:of\:Change = \frac{old - new}{old}*100\%$$

**FIGURE 1:**
<br>

```r
unemployment_state <- mutate(unemployment_state, 
                             unemployment_percent_change = (Unemployment_rate_2018 - Unemployment_rate_2010)
                             /Unemployment_rate_2010)

left_join(blank_states,unemployment_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = unemployment_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Change in Unemployment Rate from 2010 to 2018")
```
<br>

**FIGURE 2:**
<br>

```r
unemployment_state <- mutate(unemployment_state,
                             laborforce_percent_change = (Civilian_labor_force_2018 - Civilian_labor_force_2010)
                             /Civilian_labor_force_2010)

left_join(blank_states,unemployment_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = laborforce_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Civilian Labor Force")
```
<br>

**FIGURE 3:**
<br>

```r
left_join(blank_states,population_state) %>%
  ggplot() +
    geom_sf(mapping = aes(fill = POP_ESTIMATE_2018),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Amount of People",
       x = "",
       y = "",
       title = "Population Estimate 2018")
```
<br>

**FIGURE 4:**
<br>

```r
population_state <- mutate(population_state,
                           population_percent_change = (POP_ESTIMATE_2018 - POP_ESTIMATE_2010)
                           / POP_ESTIMATE_2010)

left_join(blank_states,population_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = population_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Population")
```
<br>

**FIGURE 5:**
<br>

```r
education_state <- mutate(education_state,
                           High_school_or_less = Less_than_a_high_school_diploma_2013_to_17 + 
                             High_school_diploma_only_2013_to_17)

left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = High_school_or_less),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Amount of People",
       x = "",
       y = "",
       title = "Number of People with a High School Education or Less 2017")
```
<br>

**FIGURE 6:**
<br>

```r
left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_degree_or_higher_2013_to_17),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Bachelors or Higher",
       x = "",
       y = "",
       title = "Number of People with a Bachelors Degree or Higher 2017")
```
<br>

**FIGURE 7:**
<br>

```r
education_state <- mutate(education_state,
                          highschool_or_less_percent_change = (High_school_or_less-(Less_than_a_high_school_diploma_2000 + 
                                                                 High_school_diploma_only_2000)) 
                            /(Less_than_a_high_school_diploma_2000 + 
                                High_school_diploma_only_2000))

left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = highschool_or_less_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with High School Diploma or Less")
```
<br>

**FIGURE 8:**
<br>

```r
education_state <- mutate(education_state,
                          Bachelors_or_higher_percent_change = (Bachelors_degree_or_higher_2013_to_17 
                                                               - Bachelors_degree_or_higher_2000) 
                          /Bachelors_degree_or_higher_2000)

left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_or_higher_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with a Bachelors Degree or Higher")
```
<br>

**FIGURE 9:**
<br>

```r
left_join(blank_states,unemployment_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Median_Household_Income_2017),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::dollar, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Income",
       x = "",
       y = "",
       title = "Median Household Income 2017")
```
<br>

## Filtered State Plots

|        The objective of this section is to narrow in on Georgia, Ohio, and Texas in order to see the differences between the states, with respect to the data. Bar graphs and scatterplots are the primary plots used to represent the three states data. Bar graphs do an excellent job visualizing the differences in quantaties. While scatterplots show how quantatites change over time. Further data transformations are required before plotting the three states against each other. 


<BR>

```r
unemployment_state <- filter(unemployment_state, state == "GA" | 
                               state == "OH" | state == "TX")

population_state <- filter(population_state, state == "GA" | 
                              state == "OH" | state == "TX")

education_state <- filter(education_state, state == "GA" | 
                             state == "OH" | state == "TX")

blank_states <- filter(blank_states, state == "GA" | 
                         state == "OH" | state == "TX")
# Joining them together

dataset_state <- left_join(unemployment_state, population_state)

dataset_state <- left_join(dataset_state, education_state)

#Mapping dataset for states
state_data <- left_join(blank_states, dataset_state)
```
<br>

<br>

```r
#Before we filter out the data, remove Area_name from unemployment because it is inputed differently.
unemployment_county <- select(unemployment_county, county_fips:state, Civilian_labor_force_2010:Median_Household_Income_2017)
#view(unemployment)

unemployment_county <- filter(unemployment_county, state == "GA" | 
                         state == "OH" | state == "TX")

population_county <- filter(population_county, state == "GA" | 
                       state == "OH" | state == "TX")

education_county <- filter(education_county, state == "GA" | 
                      state == "OH" | state == "TX")

blank_counties <- filter(blank_counties, state == "GA" | 
                           state == "OH" | state == "TX")
# Joining them together

dataset_county <- left_join(unemployment_county, population_county)

dataset_county <- left_join(dataset_county, education_county)

dataset_county <- filter(dataset_county, !(state_name == "Georgia" | 
                               state_name == "Ohio" | state_name == "Texas"))


#Mapping dataset for counties
county_data <- left_join(blank_counties, dataset_county, by = "county_fips")
```
<br>


**FIGURE 10:**
<br>

```r
ggplot(data = state_data, aes(x = state_name, y = POP_ESTIMATE_2018)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  ggtitle("Population of People 2018")
```
<br>


**FIGURE 11:**
<br>

```r
state_data %>%
  select(state_fips:state_name, POP_ESTIMATE_2010:POP_ESTIMATE_2018, geometry) %>%
  rename(
    "2010" = POP_ESTIMATE_2010,
    "2011" = POP_ESTIMATE_2011,
    "2012" = POP_ESTIMATE_2012,
    "2013" = POP_ESTIMATE_2013,
    "2014" = POP_ESTIMATE_2014, 
    "2015" = POP_ESTIMATE_2015,
    "2016" = POP_ESTIMATE_2016,
    "2017" = POP_ESTIMATE_2017,
    "2018" = POP_ESTIMATE_2018
  ) %>%
  gather(Year, Population, "2010":"2018") %>%
  ggplot() +
  geom_point(mapping = aes(x = Year, y = Population, color = state)) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50)) +
  ggtitle("Change in Population 2010-2018")
```
<br>


**FIGURE 12:**
<br>

```r
ggplot(data = state_data, aes(x = state_name, y = Unemployed_2018)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  ggtitle("Number of Unemployed People 2018")
```
<br>


**FIGURE 13:**
<br>

```r
state_data %>%
  select(state_fips:state_name, Unemployed_2010, Unemployed_2011, Unemployed_2012,
         Unemployed_2013, Unemployed_2014, Unemployed_2015, Unemployed_2016, 
         Unemployed_2017, Unemployed_2018) %>%
  rename(
    "2010" = Unemployed_2010,
    "2011" = Unemployed_2011,
    "2012" = Unemployed_2012,
    "2013" = Unemployed_2013,
    "2014" = Unemployed_2014, 
    "2015" = Unemployed_2015,
    "2016" = Unemployed_2016,
    "2017" = Unemployed_2017,
    "2018" = Unemployed_2018
  ) %>%
  gather(Year, Unemployed, "2010":"2018") %>%
  ggplot() +
  geom_point(mapping = aes(x = Year, y = Unemployed, color = state)) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50)) +
  ggtitle("Change in Unemployment 2010 - 2018")
```
<br>


**FIGURE 14:**
<br>

```r
state_data %>%
  select(state_fips:state_name, Unemployment_rate_2010, Unemployment_rate_2011, Unemployment_rate_2012,
         Unemployment_rate_2013, Unemployment_rate_2014, Unemployment_rate_2015, Unemployment_rate_2016, 
         Unemployment_rate_2017, Unemployment_rate_2018) %>%
  rename(
    "2010" = Unemployment_rate_2010,
    "2011" = Unemployment_rate_2011,
    "2012" = Unemployment_rate_2012,
    "2013" = Unemployment_rate_2013,
    "2014" = Unemployment_rate_2014, 
    "2015" = Unemployment_rate_2015,
    "2016" = Unemployment_rate_2016,
    "2017" = Unemployment_rate_2017,
    "2018" = Unemployment_rate_2018
  ) %>%
  gather(Year, Unemployment_rate, "2010":"2018") %>%
  ggplot() +
  geom_point(mapping = aes(x = Year, y = Unemployment_rate, color = state)) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Percent") +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50)) +
  ggtitle("Change in Unemployment Rate 2010 - 2018")
```
<br>


**FIGURE 15:**
<br>

```r
state_data %>%
  select(state_fips:state_name, Less_than_a_high_school_diploma_2000: Bachelors_degree_or_higher_2000) %>%
  rename(
    "No High School Diploma 2000" = Less_than_a_high_school_diploma_2000,
    "High School Diploma 2000" = High_school_diploma_only_2000,
    "Some College 2000" = Some_college_or_associates_degree_2000,
    "Bachelors or Higher 2000" = Bachelors_degree_or_higher_2000
  ) %>%
  gather(Education, People, "No High School Diploma 2000":"Bachelors or Higher 2000") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Amount of People with Different Education Levels, 2000")
```
<br>


**FIGURE 16:**
<br>

```r
state_data %>%
  select(state_fips:state_name, Percent_of_adults_with_less_than_a_high_school_diploma_2000: 
           Percent_of_adults_with_a_bachelors_degree_or_higher_2000) %>%
  rename(
    "No High School Diploma 2000" = Percent_of_adults_with_less_than_a_high_school_diploma_2000,
    "High School Diploma 2000" = Percent_of_adults_with_a_high_school_diploma_only_2000,
    "Some College 2000" = Percent_of_adults_completing_some_college_or_associates_degree_2000,
    "Bachelors or Higher 2000" = Percent_of_adults_with_a_bachelors_degree_or_higher_2000
  ) %>%
  gather(Education, People, "No High School Diploma 2000":"Bachelors or Higher 2000") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Percent") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Percent of Different Education Levels, 2000")
```
<br>


**FIGURE 17:**
<br>

```r
state_data %>%
  select(state_fips:state_name, Less_than_a_high_school_diploma_2013_to_17: Bachelors_degree_or_higher_2013_to_17) %>%
  rename(
    "No High School Diploma 2017" = Less_than_a_high_school_diploma_2013_to_17, 
    "High School Diploma 2017" = High_school_diploma_only_2013_to_17,
    "Some College 2017" = Some_college_or_associates_degree_2013_to_17,
    "Bachelors or Higher 2017" = Bachelors_degree_or_higher_2013_to_17
  ) %>%
  gather(Education, People, "No High School Diploma 2017":"Bachelors or Higher 2017") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Amount of People with Different Education Levels, 2017")
```
<br>

**FIGURE 18:**
<br>

```r
state_data %>%
  select(state_fips:state_name, Percent_of_adults_with_less_than_a_high_school_diploma_2013_to_17: 
           Percent_of_adults_with_a_bachelors_degree_or_higher_2013_to_17) %>%
  rename(
    "No High School Diploma 2017" = Percent_of_adults_with_less_than_a_high_school_diploma_2013_to_17, 
    "High School Diploma 2017" = Percent_of_adults_with_a_high_school_diploma_only_2013_to_17,
    "Some College 2017" = Percent_of_adults_completing_some_college_or_associates_degree_2013_to_17,
    "Bachelors or Higher 2017" = Percent_of_adults_with_a_bachelors_degree_or_higher_2013_to_17
  ) %>%
  gather(Education, People, "No High School Diploma 2017":"Bachelors or Higher 2017") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Percent") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Percent of Different Education Levels, 2017")
```
<br>

## Filtered County Plots

|        It is finally time to plot the county data. While the county data can be plotted across the entire United States, here, it is only plotted over Georgia, Ohio, and Texas. The goal was to see how the different regions within these three selected states compare to one another. The county plot codes are shown below.

**FIGURE 19:**
<br>

```r
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = POP_ESTIMATE_2018),
          color = "green", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Population Estimate 2018")
```
<br>

**FIGURE 20:**
<br>

```r
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Unemployment_rate_2018),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Unemployment Rate 2018")
```
<br>

**FIGURE 21:**
<br>

```r
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Less_than_a_high_school_diploma_2000),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People Without a High School Diploma 2000")
```
<br>

**FIGURE 22:**
<br>

```r
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_degree_or_higher_2000),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People with At Least a Bachelors Degree 2000")
```
<br>

**FIGURE 23:**
<br>

```r
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Less_than_a_high_school_diploma_2013_to_17),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People Without a High School Diploma 2017")
```
<br>


**FIGURE 24:**
<br>

```r
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_degree_or_higher_2013_to_17),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People with At Least a Bachelors Degree 2017")
```
<br>


**FIGURE 25:**
<br>

```r
county_data <- mutate(county_data, 
                             unemployment_percent_change = (Unemployment_rate_2018 - Unemployment_rate_2010)
                             /Unemployment_rate_2010)

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = unemployment_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Change in Unemployment Rate from 2010 to 2018")
```
<br>


**FIGURE 26:**
<br>

```r
county_data <- mutate(county_data,
                             laborforce_percent_change = (Civilian_labor_force_2018 - Civilian_labor_force_2010)
                             /Civilian_labor_force_2010)

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = laborforce_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Civilian Labor Force")
```
<br>


**FIGURE 27:**
<br>

```r
county_data <- mutate(county_data,
                           population_percent_change = (POP_ESTIMATE_2018 - POP_ESTIMATE_2010)
                           / POP_ESTIMATE_2010)

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = population_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Population")
```
<br>


**FIGURE 28:**
<br>

```r
county_data <- mutate(county_data,
                          highschool_or_less_percent_change = ((Less_than_a_high_school_diploma_2013_to_17
                                                               + High_school_diploma_only_2013_to_17)
                      - (Less_than_a_high_school_diploma_2000 + High_school_diploma_only_2000)) 
                          / (Less_than_a_high_school_diploma_2000 + 
                              High_school_diploma_only_2000))

county_data %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = highschool_or_less_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with High School Diploma or Less")
```
<br>

**FIGURE 29:**
<br>

```r
county_data <- mutate(county_data,
                          Bachelors_or_higher_percent_change = (Bachelors_degree_or_higher_2013_to_17 
                                                                - Bachelors_degree_or_higher_2000) 
                          /Bachelors_degree_or_higher_2000)

county_data %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_or_higher_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with a Bachelors Degree or Higher")
```
<br>

**FIGURE 30:**
<br>

```r
county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Median_Household_Income_2017),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::dollar, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Income",
       x = "",
       y = "",
       title = "Median Household Income 2017")
```
<br>

