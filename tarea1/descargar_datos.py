import pandas as pd
from huggingface_hub import list_repo_files
import os

def obtener_df_taco_restaurant(file_name:str ) -> pd.DataFrame:
    """Método para descargar un parquet de Hugging Face"""
    url = "https://huggingface.co/datasets/foursquare/fsq-os-places/resolve/main/" + file_name
    df = pd.read_parquet(url)

    # No nos interesan missing values
    df = df.dropna(subset=['fsq_category_labels'])

    # El parquet nos da una lista en "fsq_category_labels" por lo que unimos los strings de la lista y pasamos el string a minúsculas
    df['labels_str'] = df['fsq_category_labels'].apply(lambda x: ', '.join(x).lower())

    # Obtenemos una variable que contenga 1 si el valor de "labels_str" contiene "taco restaurant" y cero en otro cado
    df['taco_restaurant'] = df['labels_str'].apply(
        lambda x: 1 if 'taco restaurant' in x else 0
    )

    # Filtramos para quedarnos con las observaciones que contienen "taco restaurant" en el valor de "labels_str"
    taco_restaurant = df.loc[df['taco_restaurant']==1]
    
    return taco_restaurant


if __name__=="__main__":
    # Obtenemos lista con ñ
    files = list_repo_files(
        repo_id='foursquare/fsq-os-places',
        repo_type='dataset'
    )

    # Filtramos soilamente los que son en parquet
    parquet_files = [file for file in files if file.endswith('.parquet')]
    
    os.makedirs('data', exist_ok=True)

    i = 0
    for file in files[1:]:
        try:
            df_taco = obtener_df_taco_restaurant(file)
            df_taco = df_taco.loc[df_taco['country']=='MX']
            df_taco = df_taco.dropna(subset=['latitude', 'longitude'])
            df_taco.to_csv(f'data/taco_restaurant_{i}.csv', index=False)
            print(f"{i}: File {file} guardado con éxito")
            i += 1
        except Exception as e:
            print(f'{i} Error con el archivo {file}: {e}')
