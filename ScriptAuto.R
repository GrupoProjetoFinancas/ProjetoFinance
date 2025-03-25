# PACOTES NECESSÁRIOS ----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ollamar, git2r, quarto, glue, rstudioapi, fs)

# CONFIGURAÇÕES GLOBAIS --------------------------------------------------------

RAMO_PRINCIPAL <- "main"
MODELO_PADRAO <- "gemma3:latest"  # Modelo de linguagem padrão

# FUNÇÕES PRINCIPAIS -----------------------------------------------------------

#' Verifica se o servidor Ollama está em execução
#' 
#' @return TRUE se o servidor estiver ativo, FALSE caso contrário
verificar_servidor_ollama <- function() {
  tryCatch({
    test_connection()
    return(TRUE)
  }, error = function(e) {
    message("Erro: O servidor Ollama não está em execução ou o endpoint está incorreto.")
    return(FALSE)
  })
}

#' Comunica com o Ollama usando a biblioteca `ollamar`
#' 
#' @param prompt Texto com a instrução para o modelo
#' @param modelo Nome do modelo a ser utilizado
#' @return Resposta textual do modelo
gerar_resposta_ollama <- function(prompt, modelo = MODELO_PADRAO) {
  if (!verificar_servidor_ollama()) {
    stop("Servidor Ollama não está disponível. Verifique se o Ollama está em execução.")
  }
  
  resposta <- ollamar::generate(
    model = modelo,
    prompt = prompt,
    stream = FALSE,
    output = "text"  # Retorna diretamente o texto
  )
  
  if (is.null(resposta)) {
    stop("Falha na comunicação com o Ollama. Verifique a conexão.")
  }
  
  return(resposta)
}

#' Lê o conteúdo de um arquivo .qmd
#' 
#' @param caminho_arquivo Caminho do arquivo .qmd
#' @return Conteúdo do arquivo como texto
ler_arquivo_qmd <- function(caminho_arquivo) {
  if (!file_exists(caminho_arquivo)) {
    stop("Arquivo .qmd não encontrado.")
  }
  return(readLines(caminho_arquivo, warn = FALSE) |> paste(collapse = "\n"))
}

#' Solicita instruções adicionais ao usuário
#' 
#' @return Texto com as instruções fornecidas
solicitar_instrucoes <- function() {
  cat("Insira suas instruções adicionais (pressione Enter duas vezes para finalizar):\n")
  instrucoes <- c()
  while (TRUE) {
    linha <- readline()
    if (linha == "") {  # Se o usuário pressionar Enter sem digitar nada
      if (length(instrucoes) > 0) {  # Verifica se já há instruções
        break
      }
    } else {
      instrucoes <- c(instrucoes, linha)  # Adiciona a linha às instruções
    }
  }
  return(paste(instrucoes, collapse = "\n"))
}

#' Gera uma solução em R com base nas instruções
#' 
#' @param conteudo_original Conteúdo original do .qmd
#' @param instrucoes Instruções adicionais do usuário
#' @return Solução em R e documentação em Markdown
gerar_solucao <- function(conteudo_original, instrucoes) {
  prompt <- glue(
    "Você é um assistente de análise de dados. Abaixo está o conteúdo de um arquivo .qmd:\n\n",
    "---\n{conteudo_original}\n---\n\n",
    "Com base nas seguintes instruções adicionais, forneça uma solução em R e documente tudo em Markdown:\n\n",
    "---\n{instrucoes}\n---\n\n",
    "Formate a saída como um bloco de código R e uma explicação em Markdown."
  )
  
  return(gerar_resposta_ollama(prompt))
}

#' Valida a solução com interação do usuário
#' 
#' @param solução Conteúdo gerado pelo Ollama
#' @return TRUE se válido, FALSE caso contrário
validar_solucao <- function(solucao) {
  cat("Solução gerada:\n\n")
  cat(solucao)
  resposta <- tolower(readline("\nA solução está correta? (s/n): "))
  return(resposta == "s")
}

#' Atualiza o arquivo .qmd com a nova solução
#' 
#' @param caminho_arquivo Caminho do arquivo .qmd
#' @param conteudo_original Conteúdo original do arquivo
#' @param solucao Solução gerada pelo Ollama
atualizar_qmd <- function(caminho_arquivo, conteudo_original, solucao) {
  novo_conteudo <- glue("{conteudo_original}\n\n## Nova Solução\n\n{solucao}")
  writeLines(novo_conteudo, caminho_arquivo)
}

#' Publica o arquivo atualizado no GitHub
publicar_no_github <- function(caminho_arquivo) {
  ambiente <- tempfile()
  dir.create(ambiente)
  
  # Clonar o repositório
  repositorio <- clone(REPOSITORIO_GITHUB, ambiente, credentials = credenciais)
  checkout(repositorio, RAMO_PRINCIPAL)
  
  # Copiar o arquivo atualizado para o repositório clonado
  file_copy(caminho_arquivo, ambiente, overwrite = TRUE)
  add(repositorio, basename(caminho_arquivo))
  
  # Fazer commit
  mensagem_commit <- glue("[Automação] Atualização do arquivo .qmd")
  commit(repositorio, mensagem_commit)
  
  # Fazer push com autenticação
  push(repositorio, credentials = credenciais)
  
  message("✅ Arquivo publicado com sucesso no GitHub!")
}
# FLUXO PRINCIPAL --------------------------------------------------------------

#' Controla o fluxo de execução do programa
#' 
#' @param caminho_arquivo_qmd Caminho do arquivo .qmd a ser processado
executar_fluxo_principal <- function(caminho_arquivo_qmd) {
  # Verificar se o servidor Ollama está em execução
  if (!verificar_servidor_ollama()) {
    stop("Servidor Ollama não está disponível. Verifique se o Ollama está em execução.")
  }
  
  # Ler o conteúdo original
  conteudo_original <- ler_arquivo_qmd(caminho_arquivo_qmd)
  
  # Solicitar instruções adicionais
  instrucoes <- solicitar_instrucoes()
  
  # Gerar solução
  solucao <- gerar_solucao(conteudo_original, instrucoes)
  
  # Validar e aplicar a solução
  if (validar_solucao(solucao)) {
    atualizar_qmd(caminho_arquivo_qmd, conteudo_original, solucao)
    message("Processo concluído com sucesso!")
  } else {
    message("Processo cancelado pelo usuário.")
  }
}

# INICIALIZAÇÃO ----------------------------------------------------------------
if (interactive()) {
  caminho_arquivo <- "index.qmd"  # Defina o caminho do arquivo .qmd
  message("🟢 Iniciando processo de análise e atualização...")
  executar_fluxo_principal(caminho_arquivo)
}

