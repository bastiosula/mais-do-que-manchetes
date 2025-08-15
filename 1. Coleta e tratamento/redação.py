import pandas as pd
import re

# # # Filtragem # # #

# Para remover espaços múltiplos seguidos

def reduzir_espaços(texto):
    while texto.count('  ') != 0:
        texto = texto.replace('  ', ' ')
    return texto.strip()


# Cidades possíveis de publicação da Valor

cidades = [
    'amsterdã', 'assunção', 'atenas', 'berlim', 'berna', 'bogotá', 'bruxelas', 'buenos aires', 'cairo', 
    'caracas', 'cidade do cabo', 'cidade do méxico', 'copenhague', 'dublin', 'estocolmo', 'jacarta', 'jerusalém', 
    'kiev', 'la paz', 'lima', 'lisboa', 'londres', 'luanda', 'luxemburgo', 'madri', 'montevidéu', 'moscou', 'nairóbi', 
    'nova déli', 'oslo', 'ottawa', 'palmas', 'paris', 'pequim', 'quito', 'roma', 'santiago', 'seul', 'singapura', 
    'teerã', 'tóquio', 'varsóvia', 'viena', 'washington', 'washington, dc', 'washington, d.c.', 'wellington'
] + pd.read_excel('Nomes compostos.xlsx', sheet_name = 'Capitais estaduais e nacional', header = None)[0].str.lower().tolist()


# Função para filtrar o texto

def filtrar(texto):

    # Se não for um texto, pule
    if not isinstance(texto, str):
        return ''

    # Removendo o excesso de espaços
    texto = reduzir_espaços(texto)

    # Somente letras minúsculas
    texto = texto.lower()

    
    # Redução de tamanho
    
    palavras, seleção = 0, []
    parágrafos = texto.split(' /// ')
    
    for i in range(len(parágrafos)):
    
        # Limite 
        if palavras > 50:
            break
    
        # Parágrafo
        parágrafo = parágrafos[i]
    
        # Retirando a autoria de fotografias
        parágrafo = parágrafo.replace('(imagem:', '(foto:')
        if '(foto:' in parágrafo:
            recortes = parágrafo.split('(foto:')
            if ')' in recortes[1]:
                parágrafo = recortes[0].strip() + recortes[1].split(')')[1]
    
        # Filtragem de tamanho
        palavras += parágrafo.count(' ')
        seleção.append(parágrafo)
    
    seleção = ' '.join(seleção).strip()


    # Removendo o histórico de atualização
    if 'atualizad' in seleção:
        seleção = seleção.replace('atualizado às', '').replace('atualizada às', '')
        
    
    # Retirando a cidade de publicação
    
    seleção = seleção.replace('–', '-').replace('—', '-')
    recortes = seleção.split('-')
    localidade = recortes[0].strip()
    for cidade in cidades:
        if cidade == localidade:
            seleção = '-'.join(recortes[1:]).strip()
            break

    
    # Revisão da redução de tamanho
    
    if seleção.count(' ') > 70:
        parágrafos = seleção.split('. ')
        palavras, seleção = 0, []
        
        for i in range(len(parágrafos)):
        
            # Limite 
            if palavras > 70:
                break
        
            # Parágrafo
            parágrafo = parágrafos[i].strip()
        
            # Filtragem de tamanho
            palavras += parágrafo.count(' ')
            seleção.append(parágrafo)
        
        seleção = ' /// '.join(seleção).strip()


    # Finalmente, o output
    return seleção.strip(' -')


# # # Refinamento # # #

# Remoção de caracteres acentuados.

def unidecode(texto): 
    '''Normaliza caracteres acentuados na língua portuguesa.'''
    return texto.translate(str.maketrans('àáâãäéèêëíìîïóòôõöúùûüñÿšžč', 
                                         'aaaaaeeeeiiiiooooouuuunyzsc'))


# Nomes compostos por destacar

nomes = pd.read_excel('Nomes compostos.xlsx', sheet_name = None, header = None)
nomes = pd.concat(list(nomes.values()))[0].str.lower().str.strip().drop_duplicates()
nomes_compostos = nomes[nomes.str.count(' ') + 1 > 1].tolist() 

nomes_compostos = sorted(set(nomes_compostos + list(map(unidecode, nomes_compostos))))

nomes_por_destacar = dict(zip(nomes_compostos, 
                              map(lambda x: x.replace(' ', '_'), nomes_compostos)))

padrão_1 = re.compile(r'\b(' + '|'.join(map(re.escape, nomes_por_destacar.keys())) + r')\b')


# Monopalavras aceitáveis
vogais = ('a', 'e', 'o', 'à', 'é')


# Palavras descartáveis

stopwords = pd.concat(pd.read_excel('Palavras descartáveis.xlsx', 
                                    sheet_name = None,
                                    header = None).values()).map(str.strip).drop_duplicates()

stopwords = pd.DataFrame(set(stopwords[0].tolist() + stopwords[0].apply(unidecode).tolist()))
stopwords[1] = stopwords[0].str.count(' ') + 1 # Número de palavras.

    # Para identificar palavras como blocos de ' palavra ':
def espaçador(palavra):
    if '-' in palavra[0]:
        return palavra + ' '
    return ' ' + palavra + ' '
    
stopwords[0] = stopwords[0].apply(espaçador)
stopwords = stopwords.sort_values([1,0], ascending = False)[0].tolist()
stopwords.remove(' ue ') # União Europeia (UE) é igual a unidecode("Ué")...


# Função para filtrar caracteres alfabéticos e exceções

acentuados = 'à á â ã ä é è ê ë í ì î ï ó ò ô õ ö ú ù û ü ñ ÿ œ æ š ž č'.split()
alfabeto = 'a b c ç d e f g h i j k l m n o p q r s t u v w x y z'.split()
letras_alfabéticas = alfabeto + acentuados + [' '] + ['$']
pontuações = ('.', '-', '_', "'", 'ʻ', 'ʼ', 'ʽ', '&')

def filtrar_letras_alfabéticas(texto):
    seleção = ''
    máx = len(texto)
    for i in range(máx):
        h, j = i-1, i+1
    
        caractere = texto[i]
        if (h > 0 and j < máx) and caractere in pontuações:
            if texto[h].isalpha() and texto[j].isalpha():
                seleção += caractere
                continue
    
        if caractere in letras_alfabéticas:
            seleção += caractere
        else:
            seleção += ' '
    
    return seleção.strip()


# Função para refinar parcialmente
def refinar_parcialmente(texto):
    '''Para a busca por poligramas.'''

    # Filtrando caracteres alfabéticos e exceções.
    texto = reduzir_espaços(filtrar_letras_alfabéticas(texto))

    # Excluindo sobras monoletra excetuando-se as vogais possíveis.
    texto = ' ' + '  '.join([palavra.strip(' -') for palavra in texto.split(' ') 
                             if not (len(palavra) == 1 and palavra not in vogais)]) + ' '

    # Output, finalmente.
    return texto.replace('  ', ' ').strip()


# Função para refinar o texto

def refinar(texto):

    # Destacando conceitos de nome composto.
    texto = padrão_1.sub(lambda match: nomes_por_destacar[match.group(0)], texto)

    # Filtrando caracteres alfabéticos e exceções.
    texto = reduzir_espaços(filtrar_letras_alfabéticas(texto))

    # Excluindo sobras monoletra excetuando-se as vogais possíveis.
    texto = ' ' + '  '.join([palavra.strip(' -') for palavra in texto.split(' ') 
                             if not (len(palavra) == 1 and palavra not in vogais)]) + ' '

    # Removendo as palavras descartáveis.
    for stopword in stopwords:
        texto = texto.replace(stopword, '')

    # Output, finalmente.
    return texto.replace('  ', ' ').strip()