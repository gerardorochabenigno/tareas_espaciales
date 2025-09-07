import pandas as pd
from huggingface_hub import list_repo_files
import os

def obtener_df_taco_restaurant(file_name:str ) -> pd.DataFrame:
    """Método para descargar un parquet de Hugging Face y preprocesarlos"""

    # Descargamos los datos
    url = "https://huggingface.co/datasets/foursquare/fsq-os-places/resolve/main/" + file_name
    df = pd.read_parquet(url)

    # No nos interesan missing values en la variable "fsq_category_labels", ya que no nos permite saber el giro del comercio
    df = df.dropna(subset=['fsq_category_labels'])

    # El parquet nos da una lista en "fsq_category_labels" por lo que unimos los strings de la lista y pasamos el string a minúsculas
    df['labels_str'] = df['fsq_category_labels'].apply(lambda x: ', '.join(x).lower())

    # Obtenemos una variable que contenga 1 si el valor de "labels_str" contiene "taco restaurant" y cero en otro cado
    df['taco_restaurant'] = df['labels_str'].apply(
        lambda x: 1 if 'taco restaurant' in x else 0
    )

    # Filtramos para quedarnos con las observaciones que contienen "taco restaurant" en el valor de "labels_str"
    taco_restaurant = df.loc[df['taco_restaurant']==1]

    # No necesitamos observaciones con  missing values en "latitude" y longitud, debido a que necesitamos que forzozamente que las observaciones contengan valores váidos de ambas variables
    taco_restaurant = taco_restaurant.dropna(subset=['latitude', 'longitude'])

    # Nos quedamos con información de restaurantes de tacos en México solamente
    taco_restaurant = taco_restaurant.loc[taco_restaurant['country']=='MX']

    # Nos interesan únicamente restaurantes que aún se encuentran abiertos, es decir "date_closed" es un missing value
    taco_restaurant = taco_restaurant[taco_restaurant['date_closed'].isnull()]

    # Filtramos aquellas taquerías que podrían estar en CDMX
    
    ### DISCLAIMER: PARA OBTENER LAS LISTAS "cdmx_region" y "cdmx_locality", PRIMERO JUNTÉ TODOS LOS DATAFRAMES CUYO VALOR EN "country" FUE "MX"
    # EN UNO SOLO Y REALICÉ LA OPERACIÓN SIGUIENTE SOBRE LAS VARIABLES "region" Y "locality"

    # filtro = df[var].str.contains('ciudad|méxico|cdmx|df|distrito|mexico', case=False, na=False)
    # resultado = df[filtro]
    # resultado[var].unique()
    
    # EL ARRAY RESULTANTE FUE PASADO A CHATGPT DEL unique FUE PASADO A UNA IA GENERATIVA PARA ENCONTRAR UNA LISTA CON LOS POSIBLES VALOORES DE CDMX
    # UTILIZANDO  EL SIGUIENTE PROMPT:
    # "Construye una lista de Python que incluya solamente aquellos lugares que consideras que podrían pertenecer a Ciudad de México
    # 
    # array = [...]" 

    cdmx_region = [
        'Ciudad de México', 'DF', 'Distrito Federal', 'Ciudad de Mexico', 'df',
        'Df', 'CDMX', 'México D.F', 'Distrito federal', 'México, DF',
        'distrito federal', 'Mexico City', 'México D.F.', 'México DF', 'Mexico df',
        'CIUDAD DE MÉXICO', 'México  df', 'Mexico D.F.', 'México, D.F.', 'Ciudadad México',
        'Distrito Federak', 'ciudad de México', 'México City', 'DF GAM', 'Ciudad De México',
        'México df', 'DISTRITO FEDERAL', 'Cuauhtémoc DF', 'Mexico DF', 'Distrito Federal cdmx',
        'cdmx', 'México d.f', 'Cd. de México', 'distrito federak', 'distrito Federal',
        'Distrito Federal, México', 'DF Iztapalapa', 'mexico df', 'm3xico df', 'Cdmx',
        'CIUDAD DE MEXICO', 'cd mexico', 'Cd. México', 'México, Distrito Federal', 'Cd. Mexico',
        'México Df.', 'Distrito federa', 'Ciudad de mexico', 'Ciudas de México', 'México D,F',
        'Del Valle Centro, CDMX', 'Mexico Df', 'ciudad de méxico', 'ciudad de Mexico', 'MEXICO, DF',
        'ciudad Mexico', 'CDMXP', 'ciudad de mexico', 'Mexico, D.F.', 'cuidad de México',
        'CdMx', 'Mexico D.F', 'México Df', 'México D. F.', 'DIstrito Federal',
        'Cd de México', 'Distrito Federa', 'Mexico city', 'México, D. F.', 'Df Azcapotzalco',
        'DF, Cuajimalpa', 'CiUDAD DE MEXICO', 'Cuidad de México', 'DISTRITO FEDERA', 'Ciudad De Mexico',
        'MEXICO DF', 'méxico d.f.', 'Distrito Federsl', 'mexico D.F.', 'CIUDA DE MEXICO',
        'Tláhuac, Ciudad de México', 'DF Tlalpan'
    ]
    
    cdmx_locality = [
        'Ciudad de México', 'México, D. F.', 'Ciudad De México', 'De México', 'Df',
        'DF', 'Distrito Federal', 'México D.F', 'Ciudad de Mexico', 'Cd Mexico',
        'Mexico D.F.', 'México DF', 'Azcapotzalco, Ciudad de México, DF', 'CDMX', 'Ciudad De Mexico',
        'Mexico Df', 'Mexico City, DF', 'México D.F.', 'Mexico city', 'Cuidad de México',
        'df', 'CIUDAD DE MEXICO', 'San Rafael, Cuauhtémoc, Ciudad De México', 'Mexico df', 'distrito federal',
        'Mexico DF', 'mexico df', 'Mexico Distrito Federal, DF', 'México, D.F.', 'Cuauhtémoc, Ciudad de México, DF',
        'Col. Anáhuac México, DF', 'MEXICO DF', 'Mexico,DF', 'México City', 'México, DF',
        'Ciudad México', 'Distrito Federa', 'Mexico, D.F.', 'Mexico,D.F.', 'Mexico, DF',
        'México Distrito Federal', 'México Df', 'Zacatenco, DF', 'Gustavo A. Madero, DF', 'México, D.F',
        'México city', 'San Felipe De Jesus Mexico DF', 'Venustiano Carranza, DF', 'Mexico City, Mexico', 'Mexioco DF',
        'Jardín Balbuena, Mexico City', 'Distrito Federal (Iztacalco)', 'Iztacalco, Ciudad de México, DF', 'Cuidad de mexico', 'Mexico D.f',
        'Mexico,df', 'Unidad Hab Vicente Guerrero, Iztapalapa, Mexico, Distrito Federal', 'Iztapalapa, DF', 'Mexico D.F', 'Iztapalapa, Mexico City',
        'Iztacalco DF', 'Iztacalco CD de México', 'Ciudad de mexico', 'Iztapalapa Ciudad De México', 'México, D.F. Iztapalapa',
        'de México', 'Coyoacán, DF', 'México Df.', 'Mexico D F', 'México D.f',
        'México Centró', 'Centro DF', 'Mexico d.f', 'Narvarte CDMX', 'Narvarte Poniente , Ciudad de México',
        'Benito Juárez, DF', 'Benito Juárez, Ciudad de México, DF', 'Cdmx', 'De Mexico', 'Distrito federal (Mexico city)',
        'Juarez , Ciudad de México', 'Juárez DF', 'Ciudad De mexico', 'MexicoCity', 'Cuauhtémoc, Mexico City, DF',
        'Distrito federal', 'Miguel Hidalgo, Ciudad de México, DF', 'Ciudad de méxico', 'Col. Obrera México', 'Mexico d.f.',
        'Escandon, DF', 'México D.f.', 'México, DF.', 'cdmx', 'Credito Constructor, Ciudad de México',
        'Del Valle ,  Ciudad de México', 'CdMx', 'MEXICO,DISTRITO FEDERAL', 'DISTRITO FEDERAL', 'México D. F.',
        'General Anaya , Ciudad de México', 'México df', 'Alvaro Obregon, Mexico City', 'Granjas Palo Alto Ciudad de México', 'ciudad de mexico',
        'Álvaro Obregón, DF', 'México ,polanco', 'Periodista Ciudad de México', 'Azcapotzalco DF', 'Cuajimalpa de Morelos, DF',
        'Lomas de Vista Hermosa, Ciudad de México, DF', 'Cuajimalpa, DF. Contadero', 'Ciudad de México, DF', 'Tlalpan, DF', 'méxico city',
        'Mexico D. F', 'Ciudad de México, Tlalpan', 'Coyoacan DF', 'Cd de Mexico', 'Centro, Ciudad de México, DF',
        'ciudad de méxico', 'México Xochimilco', 'Xochimilco DF', 'Xochimilco CDMX', 'Coyoacan, DF',
        'Mexico, D.F', 'Mexico,D.F', 'mexico d.f', 'Tlahuac, DF'
    ]

    filtro_cdmx = (
        taco_restaurant['region'].isin(cdmx_region) | taco_restaurant['locality'].isin(cdmx_locality)
    )
    
    taco_restaurant = taco_restaurant[filtro_cdmx]

    taco_restaurant = taco_restaurant.drop(columns=['taco_restaurant'])
    
    return taco_restaurant


if __name__=="__main__":
    # Obtenemos lista con ñ
    files = list_repo_files(
        repo_id='foursquare/fsq-os-places',
        repo_type='dataset'
    )

    
    # Filtramos solamente los que necesitamos
    hf_path = 'release/dt=2025-08-07/places/parquet' # La partición más reciente de la información
    parquet_files = [file for file in files if file.startswith(hf_path) and file.endswith('.parquet')]
    os.makedirs('data', exist_ok=True)

    # Descargamos los archivos individuales y los preprocesamos
    i = 0
    for file in parquet_files:
        try:
            # Cambiamos carácteres para poder guardar
            path = file
            path = path.replace('/', '_')
            path = path.replace('=', '_')
            path = path.replace('-', '_')
            
            df_taco = obtener_df_taco_restaurant(file)
            df_taco.to_csv(f"data/taco_{path[:-13]}.csv", index=False)
            print(f"{i}: File {file} preprocesado y guardado con éxito")
            i += 1
        except Exception as e:
            print(f'{i} Error con el archivo {file}: {e}')

    # Juntamos los dataframes individuales y lo escribimos en "taquerias_cdmx.csv"
    try:
        # Leemos los archivos de la carpeta "data"
        files = os.listdir("data") 

        # Listamos los archivos que terminan en ".csv" de la carpeta "data"
        csv_files = [file for file in files if file.endswith('.csv') and file.startswith('taco_release_dt')]

        # Cargamos los DataFrames individuales
        dfs = [pd.read_csv(os.path.join("data", file)) for file in csv_files]

        # Descartamos DataFrames vacíos
        dfs = [df for df in dfs if df.shape[0] != 0]

        # Armamos un solo DataFrame
        df = pd.concat(dfs, ignore_index=True)
        df.to_csv("taquerias_cdmx.csv", index=False)

    except Exception as e:
        print(f"Error al cargar y concatenar los archivos CSV: {e}")

