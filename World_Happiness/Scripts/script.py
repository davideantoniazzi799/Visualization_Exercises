"""
SCRIPT CONTENT
- Data loading and cleaning (for 2015, 2016, 2017, 2018, 2019)
- Merge of the datasets (142 remaining countries, 141 with no NA(UAE Government_Corruption in 2018))
- Export final dataset

Explanation of the datasets available at https://www.kaggle.com/datasets/unsdsn/world-happiness/data

The following columns: 

- GDP per Capita, 
- Family, 
- Life Expectancy, 
- Freedom, 
- Generosity, 
- Trust Government Corruption 

describe the extent to which these factors contribute in evaluating the happiness in each country.

Dystopia column meaning:
Dystopia is an imaginary country that has the world's least-happy people. 
The purpose in establishing Dystopia is to have a benchmark against 
which all countries can be favorably compared (no country performs more poorly than Dystopia) 
in terms of each of the six key variables, thus allowing each sub-bar to be of positive width. 
The lowest scores observed for the six key variables, therefore, characterize Dystopia. 
Since life would be very unpleasant in a country with the world's lowest incomes, 
lowest life expectancy, lowest generosity, most corruption, least freedom and least social support, 
it is referred to as "Dystopia", in contrast to Utopia.

The Dystopia Residual metric actually is the Dystopia Happiness Score(1.85) + 
the Residual value or the unexplained value for each country.
"""

import pandas as pd

# Uploading data + cleaning
# DATA FOR 2015
df_2015 = pd.read_csv('Data/wh_2015.csv', sep=',')
#df_2015.info()

df_2015.rename(columns={
    'Economy (GDP per Capita)':'GDP_capita',
    'Health (Life Expectancy)':'Life_Expectancy',
    'Trust (Government Corruption)':'Government_Corruption'
}, inplace=True)

df_2015.columns = df_2015.columns.str.replace(' ', '_')
df_2015.drop(columns = ['Standard_Error'], inplace = True)

#df_2015.info()

df_2015.loc[df_2015['Country'] == 'Congo (Kinshasa)', 'Country'] = 'Democratic Republic of Congo'
df_2015.loc[df_2015['Country'] == 'Congo (Brazzaville)', 'Country'] = 'Congo'
#print(df_2015['Country'].unique())

#print(df_2015['Region'].unique())

#print("Count of NaN in each column:", df_2015.isnull().sum()) # 0


# DATA FOR 2016
df_2016 = pd.read_csv('Data/wh_2016.csv', sep=',')
#df_2016.info()

df_2016.rename(columns={
    'Economy (GDP per Capita)':'GDP_capita',
    'Health (Life Expectancy)':'Life_Expectancy',
    'Trust (Government Corruption)':'Government_Corruption'
}, inplace=True)

df_2016.columns = df_2016.columns.str.replace(' ', '_')
df_2016.drop(columns = ['Lower_Confidence_Interval', 'Upper_Confidence_Interval'], inplace = True)

#df_2016.info()

df_2016.loc[df_2016['Country'] == 'Congo (Kinshasa)', 'Country'] = 'Democratic Republic of Congo'
df_2016.loc[df_2016['Country'] == 'Congo (Brazzaville)', 'Country'] = 'Congo'
#print(df_2016['Country'].unique())
#print(df_2016['Region'].unique())

#print("Count of NaN in each column:", df_2016.isnull().sum()) # 0


# DATA FOR 2017
df_2017 = pd.read_csv('Data/wh_2017.csv', sep=',')
#df_2017.info()

df_2017.columns = df_2017.columns.str.replace('.', '_')

df_2017.rename(columns={
    'Economy__GDP_per_Capita_':'GDP_capita',
    'Health__Life_Expectancy_':'Life_Expectancy',
    'Trust__Government_Corruption_':'Government_Corruption'
}, inplace=True)

df_2017.drop(columns = ['Whisker_high', 'Whisker_low'], inplace = True)

#df_2017.info()

#print(df_2017['Country'].unique())

df_2017.loc[df_2017['Country'] == 'Congo (Kinshasa)', 'Country'] = 'Democratic Republic of Congo'
df_2017.loc[df_2017['Country'] == 'Congo (Brazzaville)', 'Country'] = 'Congo'
df_2017.loc[df_2017['Country'] == 'Hong Kong S.A.R., China', 'Country'] = 'Hong Kong'
#print(df_2017['Country'].unique())

#print("Count of NaN in each column:", df_2017.isnull().sum()) # 0

# DATA FOR 2018
df_2018 = pd.read_csv('Data/wh_2018.csv', sep=',')
#df_2018.info()

df_2018.rename(columns={
    'Overall rank':'Happiness_Rank',
    'Country or region':'Country',
    'Score':'Happiness_Score',
    'GDP per capita': 'GDP_capita',
    'Healthy life expectancy':'Life_Expectancy',
    'Freedom to make life choices':'Freedom',
    'Perceptions of corruption':'Government_Corruption',
    'Social support':'Family'
}, inplace=True)

#df_2018.info()

#print(df_2018['Country'].unique())

df_2018.loc[df_2018['Country'] == 'Congo (Kinshasa)', 'Country'] = 'Democratic Republic of Congo'
df_2018.loc[df_2018['Country'] == 'Congo (Brazzaville)', 'Country'] = 'Congo'
#print(df_2018['Country'].unique())

#print("Count of NaN in each column:", df_2018.isnull().sum()) # 1: UAE Government_Corruption


# DATA FOR 2019
df_2019 = pd.read_csv('Data/wh_2019.csv', sep=',')
#df_2019.info()

df_2019.rename(columns={
    'Overall rank':'Happiness_Rank',
    'Country or region':'Country',
    'Score':'Happiness_Score',
    'GDP per capita': 'GDP_capita',
    'Healthy life expectancy':'Life_Expectancy',
    'Freedom to make life choices':'Freedom',
    'Perceptions of corruption':'Government_Corruption',
    'Social support':'Family'
}, inplace=True)

#df_2019.info()

#print(df_2019['Country'].unique())

df_2019.loc[df_2019['Country'] == 'Congo (Kinshasa)', 'Country'] = 'Democratic Republic of Congo'
df_2019.loc[df_2019['Country'] == 'Congo (Brazzaville)', 'Country'] = 'Congo'
#print(df_2019['Country'].unique())

#print("Count of NaN in each column:", df_2019.isnull().sum()) # 0


# MERGING DATAFRAMES
# 2015 and 2016
df_15_16 = pd.merge(df_2015, df_2016, 
                      on=['Country'], 
                      how="inner", 
                      suffixes=('_2015', '_2016'))

#df_15_16.info()
#print(df_2015.shape)
#print(df_15_16.shape) # 7 countries are not merged

# 2015_2016 and 2017
df_15_16_17 = pd.merge(df_15_16, df_2017, 
                      on=['Country'], 
                      how="inner")

df_15_16_17.rename(columns={
    'Happiness_Rank':'Happiness_Rank_2017',
    'Happiness_Score':'Happiness_Score_2017',
    'GDP_capita':'GDP_capita_2017',
    'Family':'Family_2017',
    'Life_Expectancy':'Life_Expectancy_2017',
    'Freedom':'Freedom_2017',
    'Generosity':'Generosity_2017',
    'Government_Corruption':'Government_Corruption_2017',
    'Dystopia_Residual':'Dystopia_Residual_2017'
}, inplace=True)

#df_15_16_17.info()
#print(df_15_16_17.shape) # 4 countries are not merged

# 2015_2016_2017 and 2018
df_15_16_17_18 = pd.merge(df_15_16_17, df_2018, 
                      on=['Country'], 
                      how="inner")

df_15_16_17_18.rename(columns={
    'Happiness_Rank':'Happiness_Rank_2018',
    'Happiness_Score':'Happiness_Score_2018',
    'GDP_capita':'GDP_capita_2018',
    'Family':'Family_2018',
    'Life_Expectancy':'Life_Expectancy_2018',
    'Freedom':'Freedom_2018',
    'Generosity':'Generosity_2018',
    'Government_Corruption':'Government_Corruption_2018'
}, inplace=True)

#df_15_16_17_18.info()
#print(df_15_16_17_18.shape) # 2 countries are not merged


# 2015_2016_2017_2018 and 2019
df_final = pd.merge(df_15_16_17_18, df_2019, 
                      on=['Country'], 
                      how="inner")

df_final.rename(columns={
    'Region_2015':'Region',
    'Happiness_Rank':'Happiness_Rank_2019',
    'Happiness_Score':'Happiness_Score_2019',
    'GDP_capita':'GDP_capita_2019',
    'Family':'Family_2019',
    'Life_Expectancy':'Life_Expectancy_2019',
    'Freedom':'Freedom_2019',
    'Generosity':'Generosity_2019',
    'Government_Corruption':'Government_Corruption_2019'
}, inplace=True)
df_final.drop(columns = ['Region_2016'], inplace = True)

#df_final.info()
#print(df_final.shape) #3 countries are not merged
#print(df_final.isna().sum()) #1 NA in Government_Corruption_2018 (UAE)

# EXPORTING FINAL DATSET
df_final.to_csv('Data/wh_complete.csv', index=False)