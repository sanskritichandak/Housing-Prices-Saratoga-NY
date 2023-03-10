**Housing-Prices-Saratoga-NY**

Abstract

The housing crisis in the United States has impacted the housing market in numerous ways. In tandem with the pandemic, Upstate New York is experiencing a surge in prices and an increase in demand for housing. However, the $200 million legislation aims to bring market conditions back to pre-COVID times (Bruno, 2022). Therefore, the aim of this study is to predict the prices and probability of having central air for houses in Saratoga, New York. The data is from 2015 and includes information about 16 home features (“StatCrunch,” n.d.). The models created predict how different home features affect the prices of houses and their probability of having central air. The three models created include linear regression, logistic regression, and KNN regression. Since the data set is relatively small, models were created with and without outliers to find the best possible model. The findings indicate that the linear regression model without outliers, logistic regression model with outliers, and KNN regression model without outliers can be used to predict prices and/or the probability of having central air for houses in Saratoga and are better than not using any model. 

Keywords: Linear Regression, Logistic Regression, KNN Regression, RMSE, MAPE, sensitivity, specificity, p-value, outliers, error rate, benchmark

**Housing Crisis**

The United States is currently in the midst of a housing crisis. The country is facing a nationwide affordable housing crisis. Harvard researchers found that nearly half of renters are cost-burdened (Sisson, 2020). Home prices are rising at twice the rate of wage growth (Sisson, 2020). This issue is highly relevant because the United States has a long history of redlining, segregation, and racist housing policies. Regardless of party affiliation, Americans living in urban areas are more likely to see affordable housing availability locally as a major problem (Schaeffer, 2022). The top 5 home features buyers are looking for are price, air conditioning, number of rooms, number of bathrooms, and living area (“6 critical things,” 2022). 82% of buyers cited budget as very or extremely important (“6 critical things,” 2022). 79% said air condition was the second most important, and similarly, 77% said the number of rooms, 72% chose the number of bathrooms, and 69% stated that they had a preferred size or square footage (“6 critical things,” 2022). 
Upstate New York is one of the major regions experiencing an affordable housing crisis. The Capital Region has had the highest population growth rate in New York (Media, 2022). In recent years, more people are moving away from New York City which is increasing the demand for housing in Upstate New York. The issue has worsened due to the COVID-19 Pandemic and because of wealthy investors who are buying second homes (Media, 2022). Almost every county in upstate New York has shown an increase in median home prices. For example, the Hudson Valley region saw an increase of $49,000 (Doar, 2021). In Dutchess County, there are zero listings for any of the identified subsidized housing in any of the three towns (Doar, 2021). The consequences of the affordable housing shortage and the resulting lack of labor has the potential to be alarming . Local fire departments and EMTS are required to live within a certain radius of the towns they serve (Doar, 2021). However, as affordable housing becomes less accessible, this becomes increasingly concerning. 
Lawmakers and local government leaders are calling for state funding to make living in upstate neighborhoods more affordable. Given that upstate communities are becoming more popular as a result of the pandemic, it has had a subsequent negative impact on the housing market. Thus, a $200 million budget has been proposed by the senator to increase the supply of housing in these areas for purchase and for rent (Bruno, 2022). The legislation aims to construct new houses by partnering with non-governmental organizations that aim to build new affordable housing or restore the existing housing in a way that makes it affordable to low-income and at-risk communities (Bruno, 2022).
Saratoga has a population of 28,491 (“US Census,” 2021). 88.3% of the community is White and 4.2% of the community is black/African American (“US Census,” 2021). 52.3% of residents are female and 47.7% are male (“US Census,” 2021). The average number of persons per household is 2.03 (“US Census,” 2021). The median household income is $82,816 and median property values are $365,900 and the homeownership rate is 55.7% (“US Census,” 2021). Although this data is from 2015, and COVID has increased the demand and prices for housing in Saratoga, the proposed legislation is aimed at helping bring the prices back to pre-COVID times. Therefore, the data set is relevant in predicting future house prices in Saratoga.

**Data Management**

The data set consists of data related to houses in Saratoga, New York from 2015. It consists of the following information: The selling price of the house in USD, the lot size in acres, whether or not it has a waterfront (0 = No, 1 = Yes), age in years, land value in USD as the assessed value of the property without the structures, whether or not it is newly constructed (0 = No, 1 = Yes), whether or not it has a central air system (0 = No, 1 = Yes), the type of fuel used (1 = Gas, 2 = Elective, 2 = Oil), the heat type used (1 = Forced Hot Air, 2 = Hot Air, 3 = Electric), sewer type (1 = None/Unknown, 2 = Private (Septic System), 3 = Commercial/Public Living), living area in square feet, Pct College which is the percentage of residents of the given zip code that attended a four-year college, the number of half-baths, the number of bedrooms, the number of fireplaces, and the number of rooms (“StatCrunch,” n.d). 
The raw data introduced above consists of 1,728 points. The first step involved in cleaning the data is checking for duplicates and missing values. Since there are no duplicates or missing values found in the data set, the next step in cleaning the data is removing the outliers. There are two methods to remove outliers – the box-plot method and the z-score method. The z-score method measures the deviation of data points from the mean. Using the z-score method in RStudio, the data points furthest away from the mean are removed. The removeOutliers function is run only once, since running it repeatedly changes the range that is considered when removing outliers.
Below are the boxplots and histograms for the data set with and without the outliers. As observed, after removing the outliers, the data is less skewed and the histogram has a more normal distribution. 

**Figure 1**

Boxplots of all the variables from the data set with outliers

<img width="310" alt="image" src="https://user-images.githubusercontent.com/123300713/213914186-ab428936-0656-48ce-bc26-bb01dc95df07.png">

**Figure 2**

Boxplots of all the variables from the data set without outliers

<img width="315" alt="image" src="https://user-images.githubusercontent.com/123300713/213914238-7eb19d56-d4c2-4363-af88-f7dbe63476d2.png">

**Figure 3**

Histogram of the price variable from the data set with outliers

<img width="264" alt="image" src="https://user-images.githubusercontent.com/123300713/213914269-c9490e3b-95a7-462a-9974-92dee01a3942.png">

**Figure 4**

Histogram of the price variable from the data set without outliers

<img width="283" alt="image" src="https://user-images.githubusercontent.com/123300713/213914278-0f0a92a5-e061-45e5-83ac-6a7ad2b5899e.png">

In addition to removing the outliers, the classification of certain variables was changed, too. Fuel Type and Heat Type have 3-factor levels, each. The factor levels were labeled as 2, 3, and 4, respectively, i.e., none of the variables has a Fuel Type of 1 or a Heat Type of type 1. In order to improve the comprehension and visual representation of the data, the order, and label of the factor levels were changed from 2, 3, and 4, to 1, 2, and 3, respectively, for both the Fuel Type and Heat Type variables.
Moreover, the Pct variable, which indicates the percentage of people who have attended a 4-year college in a certain zip code area, was removed. This variable was removed because the zip codes of each area are missing; thus, it is not possible to connect the percentages to their respective zip code. Secondly, since the zip codes are missing, and hypothetically more than one area can have the same value, it is not possible to manually classify the values. In addition, Pct is not one of the essential home features or variables in the data set. Therefore, it was removed from the data before creating the models.
Finally, each model uses a 70:30 data split. This implies that 70% of the data is used in the training set and 30% of the data is used in the test set. Given that the data set is relatively small, this specific data split helps avoid overfitting and uses more data in the training set to make more accurate predictions. 
