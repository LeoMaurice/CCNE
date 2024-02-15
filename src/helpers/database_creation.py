# converted from R in 31/01/2024

from datetime import datetime

def __president_CCNE_by_date(date):
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

import re

def __merge_hyphenated_words(text):
    """
    Cette fonction utilise une expression régulière (re.sub()) pour rechercher les mots qui sont coupés par un tiret (-) suivi d'un retour à la ligne (\s*) dans le texte.
    La partie (\w+) capture un ou plusieurs caractères alphanumériques (les parties des mots), et la partie \s* correspond à zéro ou plusieurs espaces (le retour à la ligne).
    La fonction de substitution r'\1\2' fusionne les parties des mots capturées sans espace intermédiaire.
    """
    # Utiliser une expression régulière pour fusionner les mots coupés par des tirets
    merged_text = re.sub(r'(\w+)-\s*(\w+)', r'\1\2', text)
    return merged_text

import nltk

def __separate_footnote_numbers(text):
    """
    (\w+) capture un ou plusieurs caractères alphanumériques (les parties des mots).
    (?<!H1N1) est un "negative lookbehind assertion", cela signifie que le mot précédent ne doit pas être "H1N1". 
    Ainsi, les mots comme "H1N1" resteront collés avec les chiffres qui suivent.
    (\d+) capture un ou plusieurs chiffres (les numéros de notes de bas de page).
    La fonction de substitution r'\1 \2' insère un espace entre les parties des mots et les numéros de notes de bas de page pour les séparer.
    """
    # Utiliser une expression régulière pour séparer les numéros de notes de bas de page des mots
    separated_text = re.sub(r'(\w+)(?<!H1N1)(\d+)', r'\1 \2', text)
    return separated_text


import unicodedata

def __fix_encoding(text):
    """
    Cette fonction utilise la fonction unicodedata.normalize() 
    pour normaliser le texte en utilisant la forme de compatibilité 
    de décomposition canonique (NFKD) de l'Unicode.
    Cette normalisation permet de convertir 
    les caractères mal encodés ou les caractères accentués 
    avec des accents cassés en leur équivalent correct. 
    Par exemple, 'a' avec un accent cassé deviendra simplement 'a'.
    """
    # Utiliser la fonction de normalisation Unicode pour corriger les caractères mal encodés
    normalized_text = unicodedata.normalize('NFKD', text)
    return normalized_text



import pandas as pd
import re

def __clean_text(text):
    # Remove '\n' and replace with spaces
    cleaned_text = text.replace('\n', ' ')
    
    # Remove multiple consecutive spaces
    cleaned_text = re.sub(r'\s+', ' ', cleaned_text)
    
    return cleaned_text.strip() 

def open_avis(rescrap_texte=False):
    df = pd.read_pickle("../data/intermediate/base_avis_ccne.pkl")
    df['avis'] = df['avis'].apply(__clean_text)
    return df

import pandas as pd
from datetime import datetime

def join_metadata(base_avis_ccne):
    list_saisine_obligatoire = ["PR", "PM", "MS", "DGS", "Parlement", "Gouvernement", "EP", "EPES", "CNRS", "INSERM", "IGAS", "MILDT"]
    
    metadata = pd.read_excel("../data/raw/collected_metadata/metadata_avis.xlsx")
    metadata['saisine'] = metadata['saisine_precise'].isin(list_saisine_obligatoire)
    metadata['date'] = pd.to_datetime(metadata['date'])
    metadata.drop(columns=['date_saisie', 'saisine_precise','invites_et_CCNPEN','mot_pr','auditions','détails'],inplace=True)

    base_avis_ccne = base_avis_ccne.merge(metadata, on='num', how='left')
    base_avis_ccne['nb_mots'] = base_avis_ccne['avis'].str.count('\\w+')
    base_avis_ccne['president'] = base_avis_ccne['date'].apply(__president_CCNE_by_date)

    base_avis_ccne = base_avis_ccne.rename(columns={'titre': 'Titre', 'date': 'Date'})
    base_avis_ccne['Annee'] = base_avis_ccne['Date'].dt.year
    base_avis_ccne['theme'] = base_avis_ccne['theme'].astype('category')

    return base_avis_ccne

import pandas as pd
import nltk
nltk.download('punkt')
from nltk.tokenize import sent_tokenize

nltk.download('punkt')

def __transform_one_corpus_to_sentences(row):
    tokenized_sentences = row["tokenized_avis"]
    num_sentences = len(tokenized_sentences)
    sentence_list = []
    for i, sentence in enumerate(tokenized_sentences):
        previous = tokenized_sentences[i - 1] if i - 1 >= 0 else None
        next = tokenized_sentences[i + 1] if i + 1 < num_sentences else None

        sentence_list.append({'previous': previous,
                              'sentence': sentence,
                              'next': next,
                              'sentence_index': i,
                              'number_sentences':num_sentences,
                              'num': row['num']})

    return pd.DataFrame(sentence_list)

def __check_number_of_sentences(df):
    # Group the DataFrame by 'num' and count the number of rows for each group
    grouped_counts = df.groupby('num').size().reset_index(name='sentence_count')
    
    # Merge the grouped counts with the original DataFrame on 'num'
    merged_df = pd.merge(df, grouped_counts, on='num')
    
    # Check if the 'sentence_count' for each 'num' group matches 'number_sentences'
    result = (merged_df['sentence_count'] == merged_df['number_sentences']).all()
    
    return result

def corpus_to_sentences_with_context(base, verbose = True):
    french_sent_tokenize = lambda x: sent_tokenize(x, language="french")
    base['tokenized_avis'] = base['avis'].apply(french_sent_tokenize)
    base_sentences = base.apply(__transform_one_corpus_to_sentences, axis=1)
    base_sentences = pd.concat(base_sentences.tolist(), ignore_index=True)
    base.drop(columns = ['tokenized_avis'], inplace=True)

    base_sentences = base_sentences.merge(base,on='num', validate = "many_to_one")

    if(verbose):
        print(f"""
              corpus_to_sentences_with_context :
              is  the number of sentences counted corresponding to the number of rows by 'num' ?
              {__check_number_of_sentences(base_sentences)}""")
    return base_sentences

import re

def has_words(sentence, words):
    for word in words:
        if re.search(r'\b' + re.escape(word) + r'\b', sentence):
            return True
    return False


def filter_sentences_with_words(df, words):
    # Function to check if a sentence contains at least one word from 'words'
    has_specific_words = lambda x: has_words(x, words=words)
    # Apply the 'has_word' function to filter sentences
    filtered_df = df[df['sentence'].apply(has_specific_words)]
    
    return filtered_df