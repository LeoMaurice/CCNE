# converted from R in 31/01/2024

from datetime import datetime

def president_CCNE_by_date(date):
    presidents = ["Bernard (83-91)", "Changeux (92-99)", "Sicard (00-08)",
                  "Grimfeld (09-11)", "Ameisen (12-15)", "Delfraissy (16-)"]

    if date >= datetime.strptime("2016-12-01", "%Y-%m-%d"):
        return presidents[5]  # Jean-François Delfraissy
    elif date >= datetime.strptime("2012-01-01", "%Y-%m-%d"):
        return presidents[4]  # Jean Claude Ameisen
    elif date >= datetime.strptime("2008-01-01", "%Y-%m-%d"):
        return presidents[3]  # Alain Grimfeld
    elif date >= datetime.strptime("1999-01-01", "%Y-%m-%d"):
        return presidents[2]  # Didier Sicard
    elif date >= datetime.strptime("1992-01-01", "%Y-%m-%d"):
        return presidents[1]  # Jean-Pierre Changeux
    else:
        return presidents[0]  # Jean Bernard
    

import pandas as pd
def open_avis(rescrap_texte=False):
    return pd.read_pickle("../data/intermediate/base_avis_ccne.pkl")

import pandas as pd
from datetime import datetime

def join_metadata(base_avis_ccne):
    list_saisine_obligatoire = ["PR", "PM", "MS", "DGS", "Parlement", "Gouvernement", "EP", "EPES", "CNRS", "INSERM", "IGAS", "MILDT"]
    
    metadata = pd.read_excel("../data/raw/collected_metadata/metadata_avis.xlsx")
    metadata['saisine'] = metadata['saisine_precise'].isin(list_saisine_obligatoire)
    metadata['date'] = pd.to_datetime(metadata['date'])

    base_avis_ccne = base_avis_ccne.merge(metadata, on='num', how='left')
    base_avis_ccne['nb_mots'] = base_avis_ccne['avis'].str.count('\\w+')
    base_avis_ccne['president'] = base_avis_ccne['date'].apply(president_CCNE_by_date)

    base_avis_ccne = base_avis_ccne.rename(columns={'titre': 'Titre', 'date': 'Date'})
    base_avis_ccne['Annee'] = base_avis_ccne['Date'].dt.year
    base_avis_ccne['theme'] = base_avis_ccne['theme'].astype('category')

    return base_avis_ccne


import nltk
from nltk.tokenize import sent_tokenize
nltk.download('punkt')

def extract_sentences_with_context(text, word, num_samples=10):
    sentences = sent_tokenize(text)
    selected_sentences = []

    for i in range(len(sentences)):
        if word in sentences[i]:
            context = ' '.join(sentences[max(i - 1, 0):min(i + 2, len(sentences))])
            selected_sentences.append(context)
    
    # Reduce to the requested number of samples, if necessary
    if len(selected_sentences) > num_samples:
        selected_sentences = selected_sentences[:num_samples]

    return selected_sentences