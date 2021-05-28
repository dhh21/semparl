# -*- coding: utf-8 -*-
"""semparl data extraction wrapper.py

# DHH 2021 semparl data extraction
21.5.2021

*Install* the dependencies:
"""
import os, csv, getpass
from datetime import datetime
from collections import namedtuple
from SPARQLWrapper import SPARQLWrapper, JSON

"""Define some useful functions for data conversion"""

from rdflib.namespace import XSD
import pandas as pd 

DATATYPECONVERTERS = {
      str(XSD.integer):  int,
      str(XSD.decimal):  float,
      str(XSD.date):     lambda v: datetime.strptime(v, '%Y-%m-%d').date()
  }

def convertDatatype(obj):
  return DATATYPECONVERTERS.get(obj.get('datatype'), str)(obj.get('value')) 

def JSON2Pandas(results):
    res = results["results"]["bindings"]
    data = [dict([(k, convertDatatype(v)) for k,v in r.items()]) for r in res]
    return pd.DataFrame(data)

def make_query(location, search_term, search_not, start, end):
    """
    A query constructor that returns a sparql query with desired parameters for data extraction.
    It takes given parameters and embeds them in the query.
    :param location: the city name
    :param search_term: word forms to include in search
    :param search_not: word forms NOT to include in search
    :param start: start year of query
    :param end: end year of query
    :return: a modified query as string
    """

    query = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX text: <http://jena.apache.org/text#>
    PREFIX semparls: <http://ldf.fi/schema/semparl/>
    PREFIX semparlling: <http://ldf.fi/schema/semparl/linguistics/>

    Select 
        ?kaupunki
        (?speech AS ?puhe_id) 
        (SAMPLE(?date) AS ?päiväys) 
        (SAMPLE(?speaker_label) as ?puhuja)
        (SAMPLE(?speaker) AS ?puhuja_id) 
        (SAMPLE(?content) AS ?sisältö) 
        # (SAMPLE(?party) AS ?puolue_id) 
        (SAMPLE(?party_label) AS ?puolue) 
        (GROUP_CONCAT(DISTINCT ?paikka_label ; separator=", ") AS ?labels) 
        # (COUNT(?paikka_label) AS ?lukumäärä)
    WHERE {

      { ?speech semparlling:referenceToPlaceName ?paikka }
      UNION
      { ?speech semparlling:referenceOrganizationName ?paikka }

      ?paikka semparlling:surfaceForm ?paikka_label .
      SEARCH_WORDS
      NOT_WORDS
      ?speech 
        a semparls:Speech ; # muuttujan tyyppi = a = speech
              #semparls:content ?content ; # speechin property, joita speechin tulee sisältää
              dct:date ?date .
      FILTER(?date >= "XXXX-01-01"^^xsd:date && ?date <= "YYYY-12-31"^^xsd:date) # filtteröidään aikaikkuna
      ?speech semparls:content ?content ;
              semparls:speaker ?speaker ;
              semparls:party ?party .
      ?speaker skos:prefLabel ?speaker_label .
      ?party skos:prefLabel ?party_label .
      FILTER(LangMatches(Lang(?party_label),"fi"))
      Bind(Year(?date) As ?year)
      Bind("LOCATION" as ?kaupunki)  # luo sarakkeen kaupungin nimelle
    }
    Group By ?kaupunki ?speech # yksi puhe per rivi
    # LIMIT 1000 #näytettävät tulosrivit
    """
    query = query.replace("LOCATION", location)
    query = query.replace("XXXX", start)
    query = query.replace("YYYY", end)
    query = query.replace("SEARCH_WORDS", include(search_term))
    query = query.replace("NOT_WORDS", exclude(search_not))
    return query

def include(hakutermit):
    """
    >>> include("Laht Lahd")
    'FILTER (STRSTARTS(?paikka_label, "Laht") || STRSTARTS(?paikka_label, "Lahd"))'

    >>> include("Espoo")
    'FILTER (STRSTARTS(?paikka_label, "Espoo"))'
    """
    hakusanat = hakutermit.split(" ")
    palikat = [f'STRSTARTS(?paikka_label, "{sana}")' for sana in hakusanat]
    string = 'FILTER (' + " || ".join(palikat) + ")"
    return string

def exclude(hakutermit):
    """
    >>> exclude("Lahtis, Lahdensuo, Lahdenpohja")
    'FILTER (!(CONTAINS(?paikka_label, "Lahtis") || CONTAINS(?paikka_label, "Lahdensuo") || CONTAINS(?paikka_label, "Lahdenpohja")))'

    >>> exclude("Lahtis")
    'FILTER (!(CONTAINS(?paikka_label, "Lahtis")))'

    >>> exclude("")
    ''
    """
    if not hakutermit:
        return ""
    hakusanat = hakutermit.split(",")
    palikat = [f'CONTAINS(?paikka_label, "{sana.strip()}")' for sana in hakusanat]
    string = 'FILTER (!(' + " || ".join(palikat) + "))"
    return string

authorization = getpass.getpass('Password:')

# read file:
filename = 'kaupunkilista_siistitty - kaupunkilista.csv' # file name here
if not os.path.isfile(filename):
    # Find a way to load the file. It's in the same folder as this script
    pass

# Returns a list in which each item has a .label, a .include and a .exclude property
City = namedtuple('City', 'label include exclude')
cities = []
with open(filename) as f:
    reader = csv.reader(f)
    for row in reader:
        if not row:
            continue
        cities.append(City(row[0], row[6], row[7]))

"""# SPARQL wrapper for extracting speeches mentioning designated cities
(List of city-specific search terms loaded above)
"""

sparql = SPARQLWrapper("http://ldf.fi/semparl/sparql")

timespans = [['1986', '1995'],
            ['1969','1978'],
            ['2004','2013']]

final_results = pd.DataFrame()
k = 0 # For showing query progress
for i in timespans[0]:
  start_year = i[0]
  end_year = i[1]
  for City in cities:
    k += 1 # For showing query progress
    city = City.label
    search_term = City.label
    search_not = City.exclude
    # show and execute query:
    print("Query ", k, " run: ", city, " ", search_term, " ", search_not, " ", start_year,"-", end_year)
    sparql.setQuery(make_query(city, search_term, search_not, start_year, end_year))
    sparql.setReturnFormat(JSON)
    sparql.addCustomHttpHeader("Authorization", authorization)
    results = sparql.query().convert()
    link_data = JSON2Pandas(results)
    final_results = final_results.append(link_data)

# write a csv file:
#final_results.to_csv('city_mentions.csv', index=False)
