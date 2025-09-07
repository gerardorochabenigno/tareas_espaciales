import pandas as pd
from huggingface_hub import list_repo_files
import os

def obtener_df_taco_restaurant(file_name: str) -> pd.DataFrame:
    """Método para descargar un parquet de Hugging Face"""
    url = "https://huggingface.co/datasets/foursquare/fsq-os-places/resolve/main/" + file_name
    df = pd.read_parquet(url)

    # No nos interesan missing values
    df = df.dropna(subset=['fsq_category_labels'])

    # El parquet nos da una lista en "fsq_category_labels" por lo que unimos los strings de la lista y pasamos el string a minúsculas
    df['labels_str'] = df['fsq_category_labels'].apply(lambda x: ', '.join(x).lower())

    # Obtenemos una variable que contenga 1 si el valor de "labels_str" contiene "taco restaurant" y cero en otro caso
    df['taco_restaurant'] = df['labels_str'].apply(
        lambda x: 1 if 'taco restaurant' in x else 0
    )

    # Filtramos para quedarnos con las observaciones que contienen "taco restaurant" en el valor de "labels_str"
    taco_restaurant = df.loc[df['taco_restaurant'] == 1]
    
    return taco_restaurant


if __name__ == "__main__":
    # Obtenemos lista con todos los archivos
    files = list_repo_files(
        repo_id='foursquare/fsq-os-places',
        repo_type='dataset'
    )

    # Filtramos solamente los que son archivos parquet
    parquet_files = [file for file in files if file.endswith('.parquet')]
    
    # Obtenemos las carpetas de particiones (que empiezan con 'dt=')
    partition_folders = [file for file in files if file.startswith('release/dt=') ]
    
    # Ordenamos las particiones para obtener la más reciente
    partition_folders.sort()
    latest_partition = partition_folders[-1]  # La última partición (más reciente)
    fecha = latest_partition.split("dt=")[1].split("/")[0]

    filtrados = [a for a in partition_folders if f"dt={fecha}" in a]
    filtrados = [
    a for a in partition_folders
    if f"dt={fecha}" in a and "categories" not in a
    ]
    
    print(f"Partición más reciente: {fecha}")
    
    
    os.makedirs('data', exist_ok=True)

    i = 0
    for file in filtrados:
        try:
            df_taco = obtener_df_taco_restaurant(file)
            df_taco = df_taco.loc[df_taco['country'] == 'MX']
            df_taco = df_taco.dropna(subset=['latitude', 'longitude'])
            
            if len(df_taco) > 0:  # Solo guardar si hay datos
                df_taco.to_csv(f'data/taco_restaurant_{i}.csv', index=False)
                print(f"{i}: File {file} guardado con éxito - {len(df_taco)} registros")
                i += 1
            else:
                print(f"File {file} no tiene datos de taco restaurants en México")
                
        except Exception as e:
            print(f'Error con el archivo {file}: {e}')
    
    print(f"Proceso completado. Se guardaron {i} archivos.")