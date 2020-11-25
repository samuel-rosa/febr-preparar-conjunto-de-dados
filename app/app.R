# Title: Repositório Brasileiro Livre para Dados Abertos do Solo | Preparar Conjunto de Dados
# Version: 1.0.0
# Date: 2020-11-25
# Authors: Alessandro Samuel-Rosa (alessandrorosa@utfpr.edu.br)
# License: GPL (>= 2)
# Encoding: UTF-8

# Pacotes
library(shiny)
library(shinythemes)
library(openxlsx)

# Definir UI para aplicação
ui <- fluidPage(
    theme = shinytheme("simplex"),
    tags$head(
        tags$style(".btn{width: 100%;}"),
        tags$style(".well{padding:10px;}")
    ),
    # Título da aplicação
    titlePanel(title = a(href = 'https://www.pedometria.org/febr/'), windowTitle = 'FEBR'),
    fluidRow(
        column(
            width = 2,
            wellPanel(
                htmltools::HTML('<h3>FEBR</h3><h4>Preparar Conjunto de Dados</h4>'),
                tags$hr(),
                # Entrada dos dados
                fileInput(
                    inputId = "file", 
                    label = HTML("Carregar dados (csv, tsv, txt)"),
                    buttonLabel = "Navegar...",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", "text/plain"),
                    placeholder = "Formatos aceitos: csv, tsv e txt"
                ),
                # Selecionar separador de valores
                radioButtons(
                    inputId = "sep", 
                    label = "Separador de valores",
                    choices = c("Vírgula" = ",", "Ponto e vírgula" = ";", "Tabulação" = "\t"),
                    selected = ","),
                # Selecionar separador decimal
                shiny::radioButtons(
                    inputId = "dec", 
                    label = "Separador decimal",
                    choices = c("Vírgula" = ",", "Ponto" = "."),
                    selected = ","),
                # Selecionar delimitador de texto ----
                radioButtons(
                    inputId = "quote", 
                    label = "Delimitador de texto",
                    choices = c(None = "", "Aspas duplas" = '"', "Aspas simples" = "'"),
                    selected = '"'),
                tags$hr(),
                # Unidades de medida na segunda linha?
                checkboxInput(
                    inputId = 'rowunits',
                    label = "Unidades de medida na segunda linha",
                    value = FALSE),
                # Seleção da coluna identificadora das observações
                htmlOutput(outputId = "idcolumn"),
                # Seleção das variáveis da tabela 'observacao'
                shiny::htmlOutput(outputId = "varselect"),
                # Incluir colunas padrão
                checkboxInput(
                    inputId = 'stdcols',
                    label = "Incluir campos padrão (recomendado)",
                    value = TRUE),
                shiny::tags$hr(),
                # Descarregar dados
                downloadButton(
                    outputId = 'downloadSave', 
                    label = 'Descarregar dados (.xlsx)')
            ),
            HTML(paste0(
                '<div><p>Desenvolvido por <a href="https://www.pedometria.org/alessandro-samuel-rosa/">',
                'Alessandro Samuel-Rosa</a>.</p>')),
            HTML(paste0(
                '<p>Exceto quando proveniente de outras fontes ou onde especificado o contrário, o ',
                'conteúdo desta página está licenciado sob uma licença ',
                '<a href = "https://creativecommons.org/licenses/by-nc-sa/4.0/">CC BY NC SA 4.0</a>.</p>')),
            HTML(paste0(
                '<p>Página construída com <a href = "https://shiny.rstudio.com/">Shiny</a> e ',
                '<a href = "https://www.datatables.net/">DataTables</a>.</p></div>'))
        ),
        column(
            width = 10,
            mainPanel(
                tableOutput(outputId = "table1"),
                tableOutput(outputId = "table2"),
                tags$head(
                    tags$style(HTML(".shiny-output-error-validation { color: red; font-size: 20px } ")))
            )
        )
    )
)
# server ####
server <- function (input, output) {
    Dataset <- reactive(
        x = {
            if (is.null(input$file)) { # User has not uploaded a file yet
                Dataset <- data.frame()
            } else {
                if (input$rowunits) {
                    Dataset <- read.table(
                        file = input$file$datapath, 
                        header = FALSE, 
                        skip = 2,
                        sep = input$sep, 
                        dec = input$dec, 
                        quote = input$quote,
                        stringsAsFactors = FALSE, 
                        na.strings = c('NA', ''))
                    Units <- read.table(
                        file = input$file$datapath, 
                        header = TRUE, 
                        nrows = 1,
                        sep = input$sep, 
                        dec = input$dec, 
                        quote = input$quote,
                        stringsAsFactors = FALSE, 
                        na.strings = c('NA', ''))
                    colnames(Dataset) <- colnames(Units)
                    attr(x = Dataset, which = 'units') <- Units[1, ]
                } else {
                    Dataset <- read.table(
                        file = input$file$datapath, 
                        header = TRUE, 
                        sep = input$sep, 
                        dec = input$dec, 
                        quote = input$quote,
                        stringsAsFactors = FALSE, 
                        na.strings = c('NA', ''))
                }
            }
            return (Dataset)
        }
    )
    # Seleção da coluna identificadora das observações
    output$idcolumn <-
        renderUI(
            expr = {
                if (identical(Dataset(), '') || identical(Dataset(), data.frame())) {
                    return(NULL)
                } else {
                    selectInput(
                        inputId = "idvar",
                        label = "Identificação das observações",
                        choices = names(Dataset()),
                        selected = names(Dataset())[1],
                        multiple = FALSE,
                        selectize = FALSE)
                }
            }
        )
    # Seleção das variáveis da tabela 'observacao'
    output$varselect <- 
        renderUI(
            expr = {
                if (identical(Dataset(), '') || identical(Dataset(), data.frame())) {
                    return(NULL)
                } else {
                    selectInput(
                        inputId = "vars", 
                        label = "Informações dos locais de observação",
                        choices = names(Dataset()),
                        selected = names(Dataset())[1],
                        multiple = TRUE,
                        selectize = FALSE)
                }
            }
        )
    # Mostrar table1 (observacao)
    output$table1 <- 
        renderTable(
            expr = {
                if (is.null(input$vars) || length(input$vars) == 0) {
                    return (NULL)
                } else if (any(is.na(Dataset()[, input$idvar]))) {
                    validate(
                        need(
                            expr = any(is.na(Dataset()[, input$idvar])) == FALSE, 
                            message = paste0(
                                "Problemas encontrados nos dados carregados:\n",
                                "1) O campo de identificação das observações (", input$idvar,
                                ") possui células vazias. Isso ",
                                "significa que é impossível identificar as observações às quais ', 
                                'algumas camadas pertencem.\n",
                                "\nCorrija o problema antes de carregar os dados novamente."))
                    )
                } else {
                    return (head(Dataset()[, input$vars, drop = FALSE], n = 3)) 
                }
            },
            caption = "Dados incluídos na tabela 'observacao':", 
            caption.placement = "top",
            format.args = list(big.mark = ".", decimal.mark = ",")
        )
    # Mostrar table2 (camada)
    output$table2 <- 
        renderTable(
            expr = {
                if (is.null(input$vars) || length(input$vars) == 0) {
                    return (NULL)
                } else if (any(is.na(Dataset()[, input$idvar]))) {
                    return (NULL)
                } else {
                    cam_vars <- names(Dataset())[!names(Dataset()) %in% input$vars[input$vars != input$idvar]]
                    return (head(Dataset()[, cam_vars, drop = FALSE], n = 3))
                }
            },
            caption = "Dados incluídos na tabela 'camada'",
            caption.placement = "top",
            format.args = list(big.mark = ".", decimal.mark = ",")
        )
    # Download save
    output$downloadSave <- 
        downloadHandler(
            filename = "conjunto-de-dados-para-febr.xlsx",
            content = function (con) {
                # Get workbook template from Google Sheets
                id <- "1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI"
                file <- paste0("https://docs.google.com/spreadsheets/d/", id, "/export?format=xlsx")
                wb <- openxlsx::loadWorkbook(file = file)
                # Add style to comments to avoid errors when saving the workbook
                # Error in comment_list[[i]]$style[[j]] : subscript out of bounds
                # https://github.com/ycphs/openxlsx/issues/60
                wb_style <- '<rPr><sz val=\"11\"/><color rgb=\"FF000000\"/><name val=\"Inconsolata\"/></rPr>'
                for (i in seq_along(wb$comments)) {
                    for (j in seq_along(wb$comments[[i]])) {
                        wb$comments[[i]][[j]]$style <- wb_style
                    }
                }
                openxlsx::modifyBaseFont(wb, fontSize = 11, fontColour = "black", fontName = "Inconsolata")
                # metadado
                cam_vars <- names(Dataset())[!names(Dataset()) %in% input$vars[input$vars != input$idvar]]
                metadado <-
                    data.frame(
                        tabela_id = c(rep("observacao", length(input$vars)), rep("camada", length(cam_vars))),
                        campo_id = c(input$vars, cam_vars),
                        campo_nome = c(input$vars, cam_vars),
                        campo_descricao = NA_character_,
                        campo_unidade = NA_character_,
                        campo_tipo = NA_character_,
                        lab_nome = NA_character_,
                        lab_endereco = NA_character_
                    )
                # Unidades de medida na segunda linha da tabela de dados
                if ('units' %in% names(attributes(x = Dataset()))) {
                    units <- as.character(attr(Dataset(), 'units'))
                    metadado$campo_unidade <-
                        c(units[1:length(input$vars)], units[1], units[(length(input$vars) + 1):ncol(Dataset())])
                }
                if (input$stdcols) {
                    # Quando for requisitada a inclusão dos campos padrão, então mantem-se o nome original da
                    # coluna de identificação das observações. Necessária para identificar corretamente as
                    # posições dos dados nas tabelas 'observacao' e 'camada'.
                    std <- openxlsx::read.xlsx(xlsxFile = wb, sheet = 'metadado')
                    metadado <-
                        rbind(
                            metadado[metadado$tabela_id == "observacao", ][1, ],
                            std[std$tabela_id == "observacao", ][-1, ],
                            metadado[metadado$tabela_id == "observacao", ][-1, ],
                            metadado[metadado$tabela_id == "camada", ][1, ],
                            std[std$tabela_id == "camada", ][-1, ],
                            metadado[metadado$tabela_id == "camada", ][-1, ])
                }
                # O conteúdo da tabela metadado é deletado antes de inserir os dados. Isso é feito porque ela 
                # é originalmente usada para armazenar os dados dos campos padrão.
                openxlsx::deleteData(wb = wb, sheet = 'metadado', cols = 1:6, rows = 2:26, gridExpand = TRUE)
                for (i in 1:ncol(metadado)) {
                    openxlsx::writeData(
                        wb = wb, sheet = 'metadado', startCol = i, startRow = 2, x = metadado[, i])
                }
                # observacao
                observacao <- Dataset()[!duplicated(Dataset()[, input$idvar]), input$vars, drop = FALSE]
                pos <- match(x = input$vars, table = metadado[metadado$tabela_id == "observacao", "campo_id"])
                for (i in 1:length(input$vars)) {
                    openxlsx::writeData(
                        wb = wb, sheet = 'observacao', x = observacao[, i], startCol = pos[i], startRow = 2)
                }
                # camada
                camada <- Dataset()[, cam_vars, drop = FALSE]
                pos <- match(x = cam_vars, table = metadado[metadado$tabela_id == "camada", "campo_id"])
                for (i in 1:length(cam_vars)) {
                    openxlsx::writeData(
                        wb = wb, sheet = 'camada', x = camada[, i], startCol = pos[i], startRow = 2)
                }
                # Export XLSX file
                openxlsx::saveWorkbook(wb = wb, file = con)
            }
        )
}
# Executar a aplicação
shinyApp(ui = ui, server = server)
