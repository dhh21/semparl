# -*- coding: utf-8 -*-
"""Topic modeling - analysis and visualization.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1aaeco54YBXJM1qx-1N6OLF2jBnwm3X14
"""
import pandas as pd, matplotlib.pyplot as plt, math

""" Dataframejen import ja yhdistäminen

Dataframen rakenne: SPARQL-haun tuottamat sarakkeet + topicien todennäköisyydet esiintyä tässä puheessa + dominant topic
--> käsiteltävässä dataframessa yksi rivi per mainittu kaupunki (puheista duplikaatit tässä vaiheessa mukana)
"""

doc_topic_path = 'doc_topic_matrix_WITH_index.csv'
speech_data_path = 'city_mentions_all_cities_periods.csv'

with open(doc_topic_path, 'r', newline='') as f:
    doc_topic_df = pd.read_csv(f, sep=',')

with open(speech_data_path, 'r', newline='') as f:
    speech_data_df = pd.read_csv(f, sep=',')

#print(speech_data_df) # 45370 rows x 8 cols
#print(doc_topic_df) # 29039 rows x 22 cols

# Yhdistetään dataframet puhe_id:n perusteella
# ei vielä poisteta duplikaatteja, jotta puheet, joissa mainittu useampi kaupunki, voidaan myöhemmin käsitellä
combined_data_df = pd.merge(speech_data_df, doc_topic_df, how='inner', on='puhe_id')

# Lisää päiväys dateform-muodossa
combined_data_df['dateform'] = pd.to_datetime(combined_data_df['päiväys'])

# Poimi päiväyksestä vuosi
combined_data_df['vuosi'] = combined_data_df['dateform'].dt.year

# valitaan tarkasteltavat vuodet
start_year = '1986'
end_year = '1995'
topic_prob_by_years_df = combined_data_df.query(f'vuosi >= {start_year} and vuosi <= {end_year}')

def plot_topics_by_year(df):
    # lasketaan ja plotataan topicien vuosittaiset keskiarvot:
    speech_data_no_duplicates_df = df.drop_duplicates(subset='puhe_id') #duplicates skew the result here
    mean_topic_prob_grouped_by_year_df = speech_data_no_duplicates_df.groupby('vuosi').mean()
    ax = mean_topic_prob_grouped_by_year_df.plot(y=['Topic0', 'Topic8', 'Topic13', 'Topic14', 'Topic17'])
    labels = ['Budgeting, state expenditure', 'Future and development', 'Budgeting, national economics', 'Wellbeing, families', 'Employment, econ. development']
    ax.legend(labels)
    ax.plot()
    ax.set(xlabel='year')
    plt.savefig('topics_by_year.png')
    plt.show()

def plot_topics_by_city(df):
    # lasketaan topicien ka:t per kunta
    firstseven = ['Helsinki', 'Turku', 'Vantaa', 'Espoo', 'Tampere', 'Oulu', 'Lahti']
    topic_prob_by_city_df = df[df['kaupunki'].isin(firstseven)]
    topic_prob_means_by_city_df = topic_prob_by_city_df.groupby('kaupunki').mean()
    ax = topic_prob_means_by_city_df.plot(y=['Topic0', 'Topic8', 'Topic13', 'Topic14', 'Topic17'])
    labels = ['Budgeting, state expenditure', 'Future and development', 'Budgeting, national economics', 'Wellbeing, families', 'Employment, econ. development']
    ax.legend(labels)
    ax.plot()
    ax.set(xlabel='city')
    plt.savefig('topics_by_cities.png')
    plt.show()

def plot_cities(df, cities):
    """
    A function that plots each city's topic per year
    :param df: topic probability dataframe
    :param cities: a list of cities to plot
    :return: a plot where each city is a subplot
    """
    # kaupunkikohtaiset topcit per vuosi:
    cities_df = {} # dict jossa avain on indeksi ja arvo on kaupunki, df -tuple
    for city in cities:
        city_df = (df.query(f'kaupunki == "{city}"')
                   .filter(['vuosi', 'Topic0', 'Topic8', 'Topic13', 'Topic14', 'Topic17'])
                   .rename(columns = {'Topic0': 'Budgeting, state expenditure', 'Topic8': 'Future and development',
                                      'Topic13': 'Budgeting, national economics', 'Topic14': 'Wellbeing, families',
                                      'Topic17': 'Employment, econ. development'})
                   .groupby('vuosi')
                   .mean())
        cities_df[cities.index(city)] = (city, city_df)

    # alustetaan subplottien dimensiot:
    ncol = 3
    nrow = math.ceil(len(cities_df) / ncol)
    fig, axes = plt.subplots(nrow, ncol, figsize=(20, 10))

    # luodaan kuvaaja jokaisesta df:stä:
    count=0 # laskuri, joka kirjaa plotatut
    locs = [] # lista subploteista [(0,0), (0,1), (0,2)...] täyttöjärjestyksessä, tyhjien poistamiseen
    for c in range(ncol):
        for r in range(nrow):
            if count < len(cities_df):
                df = cities_df.get(count)[1]
                df.plot(ax=axes[r,c])
                axes[r,c].set_title(cities_df.get(count)[0])
                axes[r,c].set(xlabel='year', ylim=(0, 0.2))
            locs.append((r, c))
            count+=1

    # poistetaan tyhjät plotit:
    for (r, c) in locs[len(cities_df):]:
        ax = axes[r, c]
        fig.delaxes(ax)

    plt.tight_layout()
    #plt.savefig('cities.png')
    plt.show()

plot_topics_by_year(topic_prob_by_years_df)
plot_topics_by_city(topic_prob_by_years_df)
plot_cities(topic_prob_by_years_df, cities=['Helsinki', 'Turku', 'Vantaa', 'Espoo', 'Tampere', 'Oulu', 'Lahti'])
