# Função para remover palavras duplicadas

def remover_palavras_duplicadas(texto):
    return ' '.join(list(set(texto.split())))


# Função para remover palavras raras

def filtrar_palavreado(texto, palavras_frequentes):

    # Retirando palavras raras
    palavras = []
    for palavra in texto.split(' '):
        if palavra in palavras_frequentes:
            palavras.append(palavra)

    return ' '.join(palavras)