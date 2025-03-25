# PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ollamar, git2r, quarto, glue, rstudioapi, fs)

# CONFIGURAÇÕES GLOBAIS
REPOSITORIO_GITHUB <- "https://github.com/GrupoProjetoFinancas/ProjetoFinance.git"
RAMO_PRINCIPAL <- "main"
MODELO_PADRAO <- "gemma3:latest"  # Modelo de linguagem padrão

# Configurar credenciais do GitHub
credenciais <- cred_token(token = "SEU_TOKEN_AQUI")  # Substitua pelo seu token


# Verificar se o servidor Ollama está em execução
verificar_servidor_ollama <- function() {
  tryCatch({
    test_connection()
    return(TRUE)
  }, error = function(e) {
    message("Erro: O servidor Ollama não está em execução ou o endpoint está incorreto.")
    return(FALSE)
  })
}

# Verificar se o modelo especificado está disponível
verificar_modelo_disponivel <- function(modelo = MODELO_PADRAO) {
  modelos_disponiveis <- list_models()
  if (modelo %in% modelos_disponiveis$name) {
    return(TRUE)
  } else {
    message(paste("Erro: O modelo", modelo, "não está disponível. Use `ollama pull", modelo, "` para baixá-lo."))
    return(FALSE)
  }
}

# Executar verificações
if (!verificar_servidor_ollama()) {
  stop("Servidor Ollama não está disponível. Verifique se o Ollama está em execução.")
}

if (!verificar_modelo_disponivel(MODELO_PADRAO)) {
  stop(paste("O modelo", MODELO_PADRAO, "não está disponível. Use `ollama pull", MODELO_PADRAO, "` para baixá-lo."))
}


# Ler o conteúdo de um arquivo .qmd
ler_arquivo_qmd <- function(caminho_arquivo) {
  if (!file_exists(caminho_arquivo)) {
    stop("Arquivo .qmd não encontrado.")
  }
  return(readLines(caminho_arquivo, warn = FALSE) |> paste(collapse = "\n"))
}

# Caminho do arquivo .qmd
caminho_arquivo <- "index.qmd"

# Ler o conteúdo original
conteudo_original <- ler_arquivo_qmd(caminho_arquivo)

# Salvar o conteúdo original em uma variável global
assign("conteudo_original", conteudo_original, envir = .GlobalEnv)


# Solicitar instruções adicionais ao usuário
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

# Solicitar instruções
instrucoes <- solicitar_instrucoes()

# Salvar as instruções em uma variável global
assign("instrucoes", instrucoes, envir = .GlobalEnv)


# Comunica com o Ollama usando a biblioteca `ollamar`
gerar_resposta_ollama <- function(prompt, modelo = MODELO_PADRAO) {
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

# Gerar solução
solucao <- gerar_resposta_ollama(
  glue(
    "Você é um assistente de análise de dados. Abaixo está o conteúdo de um arquivo .qmd:\n\n",
    "---\n{conteudo_original}\n---\n\n",
    "Com base nas seguintes instruções adicionais, forneça uma solução em R e documente tudo em Markdown:\n\n",
    "---\n{instrucoes}\n---\n\n",
    "Formate a saída como um bloco de código R e uma explicação em Markdown."
  )
)

# Salvar a solução em uma variável global
assign("solucao", solucao, envir = .GlobalEnv)

# Exibir solução gerada
cat("Solução gerada:\n\n", solucao, "\n")

# Valida a solução com interação do usuário
validar_solucao <- function(solucao) {
  cat("Solução gerada:\n\n")
  cat(solucao)
  resposta <- tolower(readline("\nA solução está correta? (s/n): "))
  return(resposta == "s")
}

# Validar solução
if (validar_solucao(solucao)) {
  message("Solução aprovada pelo usuário.")
} else {
  stop("Processo cancelado pelo usuário.")
}

# Atualiza o arquivo .qmd com a nova solução
atualizar_qmd <- function(caminho_arquivo, conteudo_original, solucao) {
  novo_conteudo <- glue("{conteudo_original}\n\n## Nova Solução\n\n{solucao}")
  writeLines(novo_conteudo, caminho_arquivo)
}

# Atualizar o arquivo .qmd
atualizar_qmd(caminho_arquivo, conteudo_original, solucao)
message("Arquivo .qmd atualizado com sucesso.")

