# PACOTES NECESSÁRIOS ----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ollamar, git2r, quarto, glue, rstudioapi, fs)

# CONFIGURAÇÕES GLOBAIS --------------------------------------------------------
REPOSITORIO_GITHUB <- "https://github.com/GrupoProjetoFinancas/ProjetoFinance.git"
RAMO_PRINCIPAL <- "main"
MODELO_PADRAO <- "gemma3:latest"  # Modelo de linguagem padrão

# Configurar credenciais do GitHub
GITHUB_TOKEN <- Sys.getenv("GITHUB_TOKEN")
if (GITHUB_TOKEN == "") stop("❌ Token do GitHub não encontrado. Defina a variável de ambiente GITHUB_TOKEN.")
credenciais <- cred_token(GITHUB_TOKEN)

# FUNÇÕES PRINCIPAIS -----------------------------------------------------------

#' Verifica se o servidor Ollama está em execução
verificar_servidor_ollama <- function() {
  tryCatch({
    test_connection()
    return(TRUE)
  }, error = function(e) {
    message("Erro: O servidor Ollama não está em execução ou o endpoint está incorreto.")
    return(FALSE)
  })
}

#' Solicita instruções adicionais ao usuário
solicitar_instrucoes <- function() {
  cat("Insira suas instruções adicionais (pressione Enter para cada linha e digite 'FIM' para finalizar):\n")
  instrucoes <- c()
  repeat {
    linha <- readline()
    if (toupper(trimws(linha)) == "FIM") break
    instrucoes <- c(instrucoes, linha)
  }
  return(paste(instrucoes, collapse = "\n"))
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

executar_fluxo_principal <- function(caminho_arquivo_qmd) {
  if (!verificar_servidor_ollama()) {
    stop("Servidor Ollama não está disponível. Verifique se o Ollama está em execução.")
  }
  
  instrucoes <- solicitar_instrucoes()
  message("Instruções recebidas:\n", instrucoes)
  
  # Publicar no GitHub
  publicar_no_github(caminho_arquivo_qmd)
  message("Processo concluído com sucesso!")
}

# INICIALIZAÇÃO ----------------------------------------------------------------
if (interactive()) {
  caminho_arquivo <- "index.qmd"
  message("🟢 Iniciando processo de análise e atualização...")
  executar_fluxo_principal(caminho_arquivo)
}