import os # Diretórios
from pathlib import Path # Organização de pastas no computador
(Path('bases/SQLs')).mkdir(parents = True, exist_ok = True)
import sqlite3 # Armazenamento dinâmico em SQL

import pandas as pd # Tabelas

import requests # Coleta de HTMLs
from bs4 import BeautifulSoup as BS # Decodificação de HTMLs

from multiprocessing import cpu_count, Pool # Paralelização
from time import perf_counter # Cronômetro

def request_robusta(url):
    '''
    Como na paralelização o código ocorre em massa, é 
    preciso que ele seja robusto a erros. Sendo assim,
    esta função tenta uma request com limite de tempo
    e, se não funcionar, retorna "Nnoe", o que se 
    integra à gestão de erros posterior.
     
    '''
    
    try:
        html = requests.get(url, timeout = 3).content
    except:
        return None
    return html

classes = {'html': (None, None, 'authorship', None),
           'ghtml': ('content-head__title', 'content-head__subtitle', 
                     'content-publication-data', 'content-text__container')} 
classes_permitidas = classes.keys()

def interpretador_de_HTMLs(informação):
    '''Extrai manchete, resumo e conteúdo das notícias.'''
    
    url, html = informação
	
    if html == None:
        return tuple([None]*4)

    # Tipo de HTML
    classe = url.split('.')[-1]
    if classe not in classes_permitidas:
        return tuple([None]*4)
    C1, C2, C3, C4 = classes[classe]

    # BeuatifulSoup
    soup = BS(html, 'html.parser')
    
    # Manchete
    manchete = soup.find('h1') if classe == None else soup.find('h1', class_ = C1)
    if manchete != None:
        manchete = manchete.text.strip()

    # Resumo
    resumo = soup.find('h2') if classe == None else soup.find('h2', class_ = C2)
    if resumo != None:
        resumo = resumo.text.strip()

    # Data
    data = soup.find('div') if classe == None else soup.find('div', class_ = C3)
    if data != None:
        data = data.find('time')
        if data != None:
            data = data['datetime'].strip()

    # Conteúdo
    ps = soup.find_all('p') if C4 == None else soup.find_all('p', class_ = C4)
    if ps != []:
        parágrafos = []
        for parágrafo in ps:
            subtítulos = parágrafo.find_all('strong')
            parágrafo = parágrafo.text.strip()
            
            if subtítulos != []:
                for subtítulo in subtítulos:
                    subtítulo = subtítulo.text.strip()
                    if subtítulo == '':
                        continue
                    fragmentos = list(map(str.strip, parágrafo.split(subtítulo)))
                    parágrafo = (f' {subtítulo} '.join(fragmentos)).strip()
                    
            parágrafos.append(parágrafo)
        conteúdo = ' /// '.join(parágrafos).replace('  ', ' ')
        for pontuação in (',', '.', '"', '?', '!'):
            conteúdo = conteúdo.replace(f' {pontuação}', pontuação).replace(f'///{pontuação}', f'/// {pontuação}')
    else:
        conteúdo = None

    return data, manchete, resumo, conteúdo
	
def multiprocessador_de_HTMLs(urls, cpus_i = cpu_count(), cpus_ii = cpu_count()):
    '''
    Realiza múltiplas requests e, em seguida, processa seus
    HTMLs em massa. Para evitar banimento de IP, recomenda-se
    utilizar poucos processadores às requests. Por outro lado,
    pode-se utilizar 100% da CPU na decodificação de HTMLs.
     
    '''
    
	with Pool(cpus_i) as pool:
		htmls = pool.map(request_robusta, urls)

	informações = [(url, html) for url, html in zip(urls, htmls)]
	with Pool(cpus_ii) as pool:
		informações = pool.map(interpretador_de_HTMLs, informações)

	informações = pd.DataFrame(informações, columns = ['data_do_html', 'manchete', 'resumo', 'conteúdo'])
	informações['url'] = urls # Indexador para posterior concatenação.
	informações.dropna(subset = 'manchete', inplace = True) # Retirando as notícias não lidas.

	return informações

def coletar_e_decodificar_HTMLs(portal, notícias_por_multirequest = 1000, CPUs = 3, número_de_iterações = None):
    '''
    Esta função cataloga manchete, resumo e conteúdo de 
    todas as notícias de um dado portal em um arquivo SQL 
    local, sendo que não há output e o progresso é 
    reconhecido através de tal arquivo externo.

    Sendo assim, ela pode ser interrompida quantas vezes 
    for preciso e utilizada rotineiramente para 
    atualizar uma mesma base em SQL.
    
    '''

    # Armazanemento dinâmico e check-points
    
    conn = sqlite3.connect(f'bases/SQLs/htmls {portal}.db')
    cursor = conn.cursor()
    
    cursor.execute(f"""
    CREATE TABLE IF NOT EXISTS htmls (
        data_do_html, 
        manchete,
        resumo, 
        conteúdo,
        url 
    );
    """)
    conn.commit()

    diretório = os.getcwd() # Diretório atual e sua raíz imediata:
    diretório_anterior = '\\'.join(diretório.split('\\')[:-1])
    
    urls_SQL = pd.read_parquet(f'{diretório_anterior}\\urls\\bases\\urls.parquet')
    urls_SQL = urls_SQL[urls_SQL.portal == portal].copy()
    
    htmls_SQL = pd.read_sql('SELECT * FROM htmls', conn)
    htmls_SQL.dropna(subset = ['data_do_html', 'resumo', 'conteúdo'], 
                     how = 'all', inplace = True) # Releia os sem dados.
    htmls_SQL = htmls_SQL.copy()[~htmls_SQL.manchete.str.contains('Error')] 
    
    urls_por_ler = urls_SQL[~urls_SQL.url.isin(htmls_SQL.url)].url.tolist()
    del urls_SQL, htmls_SQL # Liberando espaço na memória...

        
    # Coleta e decoficação com paralelização

    i, j = 0, notícias_por_multirequest
    intervalo = j - i 
    
    teto = len(urls_por_ler)
    iterações = 0
    while i != j:
        início = perf_counter()
        
        subgrupo_de_urls = urls_por_ler[i:j]
        dataframe = multiprocessador_de_HTMLs(subgrupo_de_urls, CPUs)
        dataframe.to_sql('htmls', conn, if_exists = 'append', index = False)
        conn.commit()
        
        final = perf_counter()
        tempo = int(final - início)
        if tempo != 0:
            velocidade = len(subgrupo_de_urls) / tempo
            print(f'{tempo} segundos, ou {velocidade:.2f} notícias por segundo.')
        else:
            print('Um instante..! Talvez todas as possívis já foram lidas.')
        
        i = j
        j += intervalo
        j = min(j, teto)
        
        if número_de_iterações != None:
            iterações += 1
            if iterações >= número_de_iterações:
                break