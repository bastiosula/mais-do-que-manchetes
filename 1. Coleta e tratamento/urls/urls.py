# Requests
import requests # Indireta
from selenium import webdriver # Direta
from time import sleep # Congela o código

import urllib.parse # Coisa de URLs etc.
from bs4 import BeautifulSoup as BS # Interpretação de HTMLs.

import pandas as pd # Tabelas.
from datetime import datetime, timedelta # Datas.

from pathlib import Path # Organização de pastas no computador.
(Path('bases/buscas')).mkdir(parents = True, exist_ok = True)
import sqlite3 # Arquivamento dinâmico em SQL.

import socket # Conexão à internet
def há_conexão(host = '8.8.8.8', porta = 53, timeout = 3):
    '''Verifica se o computador está conectado.'''
    
    try:
        socket.setdefaulttimeout(timeout)
        socket.socket(socket.AF_INET, socket.SOCK_STREAM).connect((host, porta))
        return True
    except Exception:
        return False

def formatar_url_de_pesquisa(portal, pesquisa, data_inicial, data_final):
    '''
    Formata a URL de pesquisa para certo portal, 
    texto por pesquisar e recorte temporal.
     
    '''
    
    data_inicial, data_final  = data_inicial.isoformat().split('.')[0], data_final.isoformat().split('.')[0]
    url = f'https://{portal}.globo.com/busca/?q={pesquisa}&order=recent&from={data_inicial}-0300&to={data_final}-0300'
    return url

def interpretador_de_datas(texto):
    '''
    Traduz datas em texto para datetime.date(), o 
    que se aplica tanto a datas, literalmente, 
    quanto a sentenças como "há 3 horas".
     
    '''
    
    if 'há' in texto: # Data implícita como dif. de tempo.
        agora = datetime.now()
        número, métrica = texto.split(' ')[1:]
        
        if not número.isnumeric():
            data = agora
        if 'minuto' in métrica:
            data = agora - timedelta(minutes = int(número))
        elif 'hora' in métrica:
            data = agora - timedelta(hours = int(número))
        elif 'dia' in métrica:
            data = agora - timedelta(days = int(número))   
            
    else: # Data convencional, mas em formato de texto.
        data = datetime.strptime(texto, "%d/%m/%Y %Hh%M")
        
    return data.date()

def decodificador_de_URLs(url):    
    '''
    Por vezes, a URL nas buscas vem embaralhada com 
    outra URL estranha. Esta função identifica 
    isso e desembaralha a URL de interesse.
     
    '''
    
    url = urllib.parse.unquote(url) 
    
    if '.html' in url:
        url = url.split('.html')[0] + '.html'
    elif '.ghtml' in url:
        url = url.split('.ghtml')[0] + '.ghtml'

    if '=http://' in url:
        url = 'http://' + url.split('=http://')[1]
    if '=https://' in url:
        url = 'https://' + url.split('=https://')[1]
        
    return url
	
def decodificador_de_HTMLs(html):
    '''
    Identifica as URLs presentes em um 
    HTML e suas respectivas datas.
     
    '''

    soup = BS(html, 'html.parser')
    blocos = soup.find_all('li', class_ = 'widget widget--card widget--info')
    
    dados = []
    for bloco in blocos: # Onde bloco são os objetos 'li' do HTML.
        objeto_de_data = bloco.find('div', class_ = 'widget--info__meta').text
        data = interpretador_de_datas(objeto_de_data)
        objeto_de_url = bloco.find('a', class_ = lambda c: c is None)['href']
        url = decodificador_de_URLs(objeto_de_url)
        dados.append([data, url])

    if len(dados) == 0:
        return None
    else:
        return pd.DataFrame(dados, columns = ['data', 'url'])

def pesquisar_na_globo(driver, portal, pesquisa, data_inicial, data_final):
    '''
    Dado um webdriver pré-existente, pesquisa em determinado 
    portal o texto em questão no recorte temporal assinaldado, 
    e retorna a URL de todas as notícias resultantes da busca.
     
    '''
    
    url = formatar_url_de_pesquisa(portal, pesquisa, data_inicial, data_final)

    coleções = []
    página = 1
    espera = 1.75
    while página < 69: 
        sleep(espera) # Freio para não ser banido.

        # Selenium
        
        url_1 = url + f'&page={página}'

        try:
            driver.get(url + f'&page={página}')
        except: 
            return True # Erro inesperado no driver.         
        
        if not url_1 == urllib.parse.unquote(driver.current_url):
            break # Se a url não corresponder, acabou.
            
        html_1 = driver.page_source

        if '429 Too Many Requests' in html_1: 
            espera += 0.01 # Ajuste prudencial de tempo de espera.
            sleep(60) # De castigo por 1 minuto pelo ban.
            continue # E releia a página atual.
        
        notícias_1 = decodificador_de_HTMLs(html_1)

        if type(notícias_1) == type(None):
            break

        coleções.append(notícias_1)

        # Requests
        
        url_2 = url + f'&page={página+1}'
        html_2 = requests.get(url_2, timeout = 69).content
        notícias_2 = decodificador_de_HTMLs(html_2)
        
        if type(notícias_2) == type(None):
            break

        coleções.append(notícias_2)
        página += 2

    if len(coleções) == 0:
        return False # Nenhuma notícia foi detectada.
    
    try:
        coleção = pd.concat(coleções, ignore_index = True).drop_duplicates(subset = 'url')
    except:
        return False # Erro ao concatenar informações.

    coleção['portal'] = portal
    return coleção[['portal', 'data', 'url']]

palavras_mágicas = ['para', 'no', 'foi']#, 'são', 'já', 'até']
def coletar_notícias_de_um_dia(driver, portal, data):
    '''
    Dado um webdriver pré-existente, retorna todas as 
    notícias publicadas em determinado portal e dia.
     
    '''
    
    data_inicial = data.replace(hour = 0, minute = 0, second = 0, microsecond = 0) # 1º instante.
    data_final = data_inicial + timedelta(days = 1, seconds = -1) # Último segundo do dia.

    coleções = []
    for palavrinha in palavras_mágicas:
        coleção = pesquisar_na_globo(driver, portal, palavrinha, data_inicial, data_final)
        
        if isinstance(coleção, bool):
            reiniciar_driver = coleção
            if reiniciar_driver:
                return True # Problema grave no driver, é preciso reiniciar.
            else: 
                continue # Erro menos importante, só continue.
        else:
            coleções.append(coleção)            

    if len(coleções) == 0:
        data = data.strftime('%d/%m/%y')
        return pd.DataFrame([data_inicial.date()] + [None]*3, index = ['recorte', 'portal', 'data', 'url']).T
        
    try:
        coleção = pd.concat(coleções, ignore_index = True).drop_duplicates(subset = 'url')
    except:
        return False # Erro inesperado ao concatenar informações.

    coleção['recorte'] = data_inicial.date()
    return coleção[['recorte', 'portal', 'data', 'url']]

def mapear_URLs_via_busca(portal):
    '''
    Esta função cataloga todas as notícias de um dado 
    portal em um arquivo SQL local, sendo que não há 
    output e o progresso é reconhecido através 
    de tal arquivo externo.

    Sendo assim, ela pode ser interrompida quantas 
    vezes for preciso e utilizada rotineiramente 
    para atualizar uma mesma base em SQL.
    
    '''

    conn = sqlite3.connect(f'bases/buscas/urls {portal}.db')
    cursor = conn.cursor()
    
    cursor.execute(f"""
    CREATE TABLE IF NOT EXISTS urls (
        recorte, 
        portal,
        data, 
        url 
    );
    """)
    conn.commit()
    
    SQL = pd.read_sql('SELECT * FROM urls', conn, parse_dates = ['recorte', 'data'])
    
    dias_já_lidos = SQL.recorte.unique().tolist()
    dias = pd.date_range(datetime(2011,7,25), datetime.today())
    dias_por_ler = list(dias[~dias.isin(dias_já_lidos)])[::-1]
    
    zeros = 0
    driver = webdriver.Chrome()
    for dia_por_ler in dias_por_ler:
        dataframe = coletar_notícias_de_um_dia(driver, portal, dia_por_ler)
        
        if isinstance(dataframe, bool):
            reiniciar_driver = dataframe
            if reiniciar_driver:
                driver.close()
                sleep(1) # Espere um pouquinho...

                if not há_conexão(): 
                    sleep(5*60) # Espere 5 minutos se não houver internet.
                else:
                    sleep(60) # Caso contrário, somente 1 já basta.
                
                driver = webdriver.Chrome()
            continue # Após reiniciar o driver, pulamos para o próximo.
        
        dataframe.to_sql('urls', conn, if_exists = 'append', index = False)
        conn.commit()    
        if (tamanho := len(dataframe)) == 1 and set(dataframe.iloc[0,1:].tolist()) == {None}:
            tamanho = 0
            zeros += 1
            if zeros > 30:
                break # No caso de 30 dias seguidos sem nada, chegamos ao fim (provavelmente).
        else:
            zeros = 0