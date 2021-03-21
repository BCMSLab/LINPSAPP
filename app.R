# load required libraries
library(shiny)
library(shinyBS)
library(DT)

library(DBI)
library(RSQLite)

library(tidyverse)

library(plotly)
library(igraph)
library(visNetwork)

# source('../www/utils.R')
source('www/utils.R')

# Sourcing ----
# load lincs info
# db_fl <- 'LINPS/results/LINPS.sqlite'
db_fl <- "www/LINPS.sqlite"
file.exists(db_fl)

# UI ----
ui <- navbarPage(title = 'LINPS',
    ## Main ----
    tabPanel(
        title = 'Main',
        fluidPage(
            ### Input Panels ----
            splitLayout(
                #### Cell ----
                wellPanel(
                    tags$h4('Cell'),
                    selectizeInput(
                        inputId = 'tissue',
                        label = 'Tissue',
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE
                    ),
                    bsTooltip(placement = 'top', id = 'tissue',
                              title = 'Select one or more tissue to see which cell lines is available.'),
                    selectizeInput(
                        inputId = 'cell_id',
                        label = 'Cell Line',
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE
                    ),
                    bsTooltip(placement = 'top', id = 'cell_id',
                              title = 'Select one or more cell lines to check which networks are perturbed in them.'),
                    # actionButton('select_cells',
                    #              'Select'),
                    actionButton('reset_cells',
                                 'Reset'),
                    bsTooltip(placement = 'top', id = 'reset_cells',
                              title = 'Click "Reset" to go back to default selection.'),
                    actionButton('lucky_cells',
                                 'Feeling lucky!'),
                    bsTooltip(placement = 'top', id = 'lucky_cells',
                              title = 'Click "Feeling lucky!" to get 3 cells selected at random.')
                ),
                #### Network ----
                wellPanel(
                    tags$h4('Network'),
                    selectizeInput(
                        inputId = 'family',
                        label = 'Family',
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE
                    ),
                    bsTooltip(placement = 'top', id = 'family',
                              title = 'Select one or more families to see which networks are available.'),
                    selectizeInput(
                        inputId = 'network',
                        label = 'Model',
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE
                    ),
                    bsTooltip(placement = 'top', id = 'network',
                              title = 'Select one or more networs to see which of them are perturbed.'),
                    # actionButton('select_network',
                    #              'Select'),
                    actionButton('reset_network',
                                 'Reset'),
                    bsTooltip(placement = 'top', id = 'reset_network',
                              title = 'Click "Reset" to go back to default selection.'),
                    actionButton('lucky_network',
                                 'Feeling lucky!'),
                    bsTooltip(placement = 'top', id = 'lucky_network',
                              title = 'Click "Feeling lucky!" to get one network selected at random.')
                ),
                #### Perturbation ----
                wellPanel(
                    tags$h4('Perturbation'),
                    selectizeInput(
                        inputId = 'pert_type',
                        label = 'Type',
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE
                    ),
                    bsTooltip(placement = 'top', id = 'pert_type',
                              title = 'Select one type of perturbations to which perturbations are available'),
                    selectizeInput(
                        inputId = 'pert_iname',
                        label = 'Name',
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE
                    ),
                    bsTooltip(placement = 'top', id = 'pert_iname',
                              title = 'Select one or more perturbation to see which of them perturb the networks.'),
                    # actionButton('select_perturb',
                    #              'Select'),
                    actionButton('reset_perturb',
                                 'Reset'),
                    bsTooltip(placement = 'top', id = 'reset_perturb',
                              title = 'Click "Reset" to go back to default selection.'),
                    actionButton('lucky_perturb',
                                 'Feeling lucky!'),
                    bsTooltip(placement = 'top', id = 'lucky_perturb',
                              title = 'Click "Feeling lucky!" to get 3 cells selected at random.')
                )
            ),
            ### Output Panels ----
            tabsetPanel(
                #### RBIF ----
                tabPanel(
                    title = 'RBIF',
                    splitLayout(
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Table) Relative imact factor of the perturbations.'))
                            ),
                            tags$br(),
                            dataTableOutput(outputId = 'rbif'),
                            tags$br()
                        ),
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Figure) Relative imact factor of the perturbations.')),
                                tags$br(),
                                actionButton(
                                    inputId = 'rbif_back',
                                    label = 'Back'
                                ),
                                bsTooltip(
                                    id = 'rbif_back',
                                    title = 'Click to go back to previous cell line.'
                                ),
                                actionButton(
                                    inputId = 'rbif_next',
                                    label = 'Next'
                                ),
                                bsTooltip(
                                    id = 'rbif_next',
                                    title = 'Click to go to next cell line.'
                                ),
                                tags$br(),
                                tags$br(),
                                textOutput(outputId = 'rbif_cell')
                            ),
                            tags$br(),
                            plotlyOutput(outputId = 'rbif_plot'),
                            tags$br()
                        )
                    )
                ),
                #### BIF ----
                tabPanel(
                    title = 'BIF',
                    splitLayout(
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Table) Networks impact factor of the perturbations.'))
                            ),
                            tags$br(),
                            dataTableOutput(outputId = 'bif'),
                            tags$br()
                        ),
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Figure) Networks impact factor of the perturbations.')),
                                tags$br(),
                                actionButton(
                                    inputId = 'bif_back',
                                    label = 'Back'
                                ),
                                bsTooltip(
                                    id = 'bif_back',
                                    title = 'Click to go back to previous family of networks.'
                                ),
                                actionButton(
                                    inputId = 'bif_next',
                                    label = 'Next'
                                ),
                                bsTooltip(
                                    id = 'bif_next',
                                    title = 'Click to go to next family of networks.'
                                ),
                                tags$br(),
                                tags$br(),
                                textOutput(outputId = 'bif_family')
                            ),
                            tags$br(),
                            plotlyOutput(outputId = 'bif_plot'),
                            tags$br()
                        )
                    )
                ),
                #### NPA ----
                tabPanel(
                    title = 'NPA',
                    splitLayout(
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Table) Networks perturbation amplitudes.'))
                            ),
                            tags$br(),
                            dataTableOutput(outputId = 'npa'),
                            tags$br()
                        ),
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Figure) Networks perturbation amplitudes.')),
                                tags$br(),
                                actionButton(
                                    inputId = 'npa_back',
                                    label = 'Back'
                                ),
                                bsTooltip(
                                    id = 'npa_back',
                                    title = 'Click to go back to previous network.'
                                ),
                                actionButton(
                                    inputId = 'npa_next',
                                    label = 'Next'
                                ),
                                bsTooltip(
                                    id = 'npa_next',
                                    title = 'Click to go to next network.'
                                ),
                                tags$br(),
                                tags$br(),
                                textOutput(outputId = 'npa_network')
                            ),
                            tags$br(),
                            plotlyOutput(outputId = 'npa_plot'),
                            tags$br()
                        )
                    )
                ),
                #### NODES ----
                tabPanel(
                    title = 'NODES',
                    splitLayout(
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Table) Contributions of leading nodes to the perturbations.'))
                            ),
                            tags$br(),
                            dataTableOutput(outputId = 'nodes'),
                            tags$br()
                        ),
                        verticalLayout(
                            tags$br(),
                            fluidRow(
                                align = 'center',
                                tags$p(strong('(Figure) Networks perturbation amplitudes.')),
                                tags$br(),
                                actionButton(
                                    inputId = 'nodes_back',
                                    label = 'Back'
                                ),
                                bsTooltip(
                                    id = 'nodes_back',
                                    title = 'Click to go back to previous cell line.'
                                ),
                                actionButton(
                                    inputId = 'nodes_next',
                                    label = 'Next'
                                ),
                                bsTooltip(
                                    id = 'nodes_next',
                                    title = 'Click to go to next cell line.'
                                ),
                                tags$br(),
                                tags$br(),
                                textOutput(outputId = 'nodes_cell')
                            ),
                            tags$br(),
                            plotlyOutput(outputId = 'nodes_plot'),
                            tags$br()
                        )
                    )
                ),
                #### GRAPH ----
                tabPanel(
                    title = 'GRAPH',
                    sidebarLayout(
                        sidebarPanel(
                            selectizeInput(
                                inputId = 'overlay_pert_type',
                                label = 'Overlay Type',
                                choices = NULL,
                                selected = NULL
                            ),
                            bsTooltip(
                                id = 'overlay_pert_type',
                                title = 'Select one perturbation type to overlay on the network.'
                            ),
                            selectizeInput(
                                inputId = 'overlay_pert_iname',
                                label = 'Overlay Perturbation',
                                choices = NULL,
                                selected = NULL
                            ),
                            bsTooltip(
                                id = 'overlay_pert_iname',
                                title = 'Select one perturbation to overlay on the network.'
                            ),
                            selectizeInput(
                                inputId = 'overlay_cell_id',
                                label = 'Cell Line',
                                choices = NULL,
                                selected = NULL
                            ),
                            bsTooltip(
                                id = 'overlay_cell_id',
                                title = 'Select one cell line to overlay on the network.'
                            ),
                            checkboxInput(
                                inputId = 'overlay_signif',
                                label = 'Show significant nodes only',
                                value = FALSE
                            ),
                            bsTooltip(
                                id = 'overlay_signif',
                                title = 'Check to show only significantly perturbed nodes in color.'
                            ),
                            checkboxInput(
                                inputId = 'overlay_scale',
                                label = 'Change node size',
                                value = FALSE
                            ),
                            bsTooltip(
                                id = 'overlay_scale',
                                title = 'Check to scale the node sizes by how strong they are perturbed.'
                            ),
                            tags$hr(),
                            checkboxInput(
                                inputId = 'highlight_nearest',
                                label = 'Highlight nearest nodes'
                            ),
                            bsTooltip(
                                id = 'highlight_nearest',
                                title = 'Check to highlight the nearst nodes when selecting a node.'
                            ),
                            checkboxInput(
                                inputId = 'select_node',
                                label = 'Select nodes',
                                value = FALSE
                            ),
                            bsTooltip(
                                id = 'select_node',
                                title = 'Check to add the option to select individual nodes by name.'
                            ),
                            checkboxInput(
                                inputId = 'select_cluster',
                                label = 'Select modules',
                                value = FALSE
                            ),
                            bsTooltip(
                                id = 'select_cluster',
                                title = 'Check to add the option to select modules.'
                            )
                        ),
                        mainPanel(
                            verticalLayout(
                                tags$br(),
                                fluidRow(
                                    align = 'center',
                                    actionButton(
                                        inputId = 'graph_back',
                                        label = 'Back'
                                    ),
                                    bsTooltip(
                                        id = 'graph_back',
                                        title = 'Click to go to next network'
                                    ),
                                    actionButton(
                                        inputId = 'graph_next',
                                        label = 'Next'
                                    ),
                                    bsTooltip(
                                        id = 'graph_ext',
                                        title = 'Click to go to next network.'
                                    ),
                                    tags$br(),
                                    tags$br(),
                                    textOutput(outputId = 'graph_name'),
                                ),
                                tags$br(),
                                visNetworkOutput('graph'),
                                tags$br(),
                                dataTableOutput('test_tab')
                            )
                        )
                    )
                ),
                #### SIMILARITY ----
                tabPanel(
                    title = 'SIMILARITY',
                    sidebarLayout(
                        sidebarPanel(
                            tags$h4('Perturbation Similarity'),
                            selectInput(
                                inputId = 'select_pert_type',
                                label = 'Type',
                                choices = NULL,
                                selected = NULL
                            ),
                            bsTooltip(placement = 'top', id = 'select_pert_type',
                                      title = 'Select one type of perturbations to which perturbations are available.'),
                            selectizeInput(
                                inputId = 'select_pert_iname',
                                label = 'Name',
                                choices = NULL,
                                selected = NULL
                            ),
                            bsTooltip(placement = 'top', id = 'select_pert_iname',
                                      title = 'Select one perturbation to see its similarity with others.'),
                            numericInput(
                                inputId = 'select_pert_number',
                                label = 'Number',
                                value = 5,
                                min = 2,
                                max = 50,
                                step = 1
                            ),
                            bsTooltip(placement = 'top', id = 'select_pert_number',
                                      title = 'Select the number of perturbations to show.'),
                            selectInput(
                                inputId = 'sim_type',
                                'Similarity',
                                choices = 'SCC',
                                selected = 'SCC'
                            ),
                            bsTooltip(placement = 'top', id = 'sim_type',
                                      title = 'Select a measure of similarity.')
                        ),
                        mainPanel(
                            verticalLayout(
                                fluidRow(
                                    align = 'center',
                                    tags$br(),
                                    fluidRow(
                                        align = 'center',
                                        tags$p(strong('(Figure) Similarity between the pertubrations.'))
                                    ),
                                    actionButton(
                                        inputId = 'select_pert_back',
                                        label = 'Back'
                                    ),
                                    bsTooltip(
                                        id = 'select_pert_back',
                                        title = 'Click to go to previous cell line'
                                    ),
                                    actionButton(
                                        inputId = 'select_pert_next',
                                        label = 'Next'
                                    ),
                                    bsTooltip(
                                        id = 'select_pert_next',
                                        title = 'Click to go to next cell_line'
                                    ),
                                    tags$br(),
                                    tags$br(),
                                    textOutput(outputId = 'select_pert_text'),
                                    tags$br(),
                                    plotlyOutput('select_pert'),
                                    tags$br(),
                                    fluidRow(
                                        align = 'center',
                                        tags$p(strong('(Table) Similarity between the pertubrations.'))
                                    ),
                                    tags$br(),
                                    dataTableOutput('select_pert_tab')
                                )
                            )
                        )
                    )

                    # TODO: add cell line selection (similarity)
                    # ,
                    # sidebarLayout(
                    #     sidebarPanel(
                    #         tags$h4('Selecting Cell Lines')
                    #     ),
                    #     mainPanel()
                    # )
                ),
                #### DOWNLAOD ----
                tabPanel(
                    title = 'DOWNLAOD',
                    verticalLayout(
                        tags$br(),
                        tags$h2('Download data for selected items'),
                        htmlOutput('selected_info'),
                        tags$br(),
                        downloadLink(
                            outputId = 'down_rbif',
                            label = 'Relative Biological Impact Factor (RBIF)'
                        ),
                        downloadLink(
                            outputId = 'down_bif',
                            label = 'Biological Impact Factor (BIF)'
                        ),
                        downloadLink(
                            outputId = 'down_npa',
                            label = 'Network Perturbation Amplitudes (NPA)'
                        ),
                        downloadLink(
                            outputId = 'down_nodes',
                            label = 'Node perturbations'
                        ),
                        downloadLink(
                            outputId = 'down_graph',
                            label = 'Network Graph'
                        ),
                        downloadLink(
                            outputId = 'down_sim',
                            label = 'Perturbations Similarity'
                        ),
                        tags$hr(),
                        tags$h2('Download the database files'),
                        htmlOutput('database_info'),
                        tags$br(),
                        downloadLink(
                            outputId = 'down_database',
                            label = 'SQLite full database file'
                        ),
                        downloadLink(
                            outputId = 'down_perturbations',
                            label = 'LINCS perturbations file'
                        ),
                        downloadLink(
                            outputId = 'down_networks',
                            label = 'Causal Biological Networks (CBN)'
                        )
                    )
                )
            )
        )
    ),
    ## Glossary ----
    tabPanel(
        title = 'Glossary',
        includeMarkdown('www/pages/Glossary.md')
    ),
    ## FAQ ----
    tabPanel(
        title = 'FAQ',
        includeMarkdown('www/pages/FAQ.md')
    ),
    ## Tutorial ----
    tabPanel(
        title = 'Tutorial',
        includeMarkdown('www/pages/Tutorial.md')
    ),
    ## Contact ----
    tabPanel(
        title = 'Contact',
        includeMarkdown('www/pages/Contact.md')
    )
)

# server ----
server <- function(input, output, session) {
    # Input panels ----
    ## CELL ----
    tissue_choices <- reactive({
        # update the tissue
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        vec <- tbl(con, 'perturbations') %>%
            pull(tissue) %>%
            unique()
        
        dbDisconnect(con)
        
        vec
    })
    observe({
        updateSelectizeInput(
            session,
            inputId = 'tissue',
            choices = tissue_choices(),
            selected = head(tissue_choices(), 3),
            server = TRUE
        )
    })
    
    cell_id_choices <- reactive({
        # update the cell_id based on tissue selection
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        vec <- tbl(con, 'perturbations') %>%
            filter(tissue %in% local(input$tissue)) %>%
            pull(cell_id) %>%
            unique()
        
        dbDisconnect(con)
        
        vec
    })
    
    observe({
        updateSelectizeInput(
            session,
            inputId = 'cell_id',
            choices = cell_id_choices(),
            selected = head(cell_id_choices(), 5),
            server = TRUE
        )
    })
    
    observeEvent(input$reset_cells, {
                     # reset tissue selection
                     updateSelectizeInput(
                         session,
                         inputId = 'tissue',
                         choices = tissue_choices(),
                         selected = head(tissue_choices(), 3),
                         server = TRUE
                     )
                     
                     # reset cell selection
                     updateSelectizeInput(
                         session,
                         inputId = 'cell_id',
                         choices = cell_id_choices(),
                         selected = head(cell_id_choices(), 5),
                         server = TRUE
                     )
                 })
    
    observeEvent(input$lucky_cells, {
                     # select 3 cells at random
                     updateSelectizeInput(
                         session,
                         inputId = 'cell_id',
                         choices = cell_id_choices(),
                         selected = sample(cell_id_choices(), 3),
                         server = TRUE
                     )
                 })
    
    ## PERTURBATION ----
    pert_type_choices <- reactive({
        # update the pert_type based on cell_id
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        vec <- tbl(con, 'rbif') %>%
            filter(cell_id %in% local(input$cell_id)) %>%
            pull(pert_type) %>%
            unique()
        
        dbDisconnect(con)
        
        vec
    })
    
    observe({
       updateSelectizeInput(
            session,
            inputId = 'pert_type',
            choices = pert_type_choices(),
            selected = head(pert_type_choices(), 3),
            server = TRUE
        )
    })
    
    pert_iname_choices <- reactive({
        # update the pert_iname based on pert_type and cell_id
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        # TODO: use rbif to extract significant perturbations
        vec <- tbl(con, 'rbif') %>%
            filter(pert_type %in% local(input$pert_type),
                   cell_id %in% local(input$cell_id)) %>%
            pull(pert_iname) %>%
            unique()
        
        dbDisconnect(con)
        
        vec
    })
    
    observe({
        updateSelectizeInput(
            session,
            inputId = 'pert_iname',
            choices = pert_iname_choices(),
            selected = head(pert_iname_choices(), 10),
            server = TRUE
        )
    })
    
    observeEvent(input$reset_perturb, {
        
        # reset pert_type selection
        updateSelectizeInput(
            session,
            inputId = 'pert_type',
            choices = pert_type_choices(),
            selected = head(pert_type_choices(), 3),
            server = TRUE
        )
        
        # reset pert_iname selection
        updateSelectizeInput(
            session,
            inputId = 'pert_iname',
            choices = pert_iname_choices(),
            selected = head(pert_iname_choices(), 10),
            server = TRUE
        )
    })
    
    observeEvent(input$lucky_perturb, {
                     # select 3 perturbations at random
                     updateSelectizeInput(
                         session,
                         inputId = 'pert_iname',
                         choices = pert_iname_choices(),
                         selected = sample(pert_iname_choices(), 3),
                         server = TRUE
                     )
                 })
    
    ## NETWORK ----
    family_choices <- reactive({
        # update the pert_type based on cell_id
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        vec <- tbl(con, 'models') %>%
            pull(family) %>%
            unique()
        
        dbDisconnect(con)
        
        vec
    })
    
    observe({
        updateSelectizeInput(
            session,
            inputId = 'family',
            choices = family_choices(),
            selected = head(family_choices(), 3),
            server = TRUE
        )
    })
    
    network_choices <- reactive({
        # update the pert_type based on cell_id
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        vec <- tbl(con, 'models') %>%
            filter(family %in% local(input$family)) %>%
            pull(model) %>%
            unique()
        
        dbDisconnect(con)
        
        vec
    })
    
    observe({
        # update the network based on family
        updateSelectizeInput(
            session,
            inputId = 'network',
            choices = network_choices(),
            selected = head(network_choices(), 3),
            server = TRUE
        )
    })
    
    observeEvent(input$reset_network, {
                     # reset family selection
                     updateSelectInput(
                         session,
                         inputId = 'family',
                         choices = family_choices(),
                         selected = head(family_choices(), 3)
                     )
                     
                     # reset network selection
                     updateSelectInput(
                         session,
                         inputId = 'network',
                         choices = network_choices(),
                         selected = head(network_choices(), 3),
                     )
                 })
    
    observeEvent(input$lucky_network, {
                     # select 1 network at random
                     updateSelectInput(
                         session,
                         inputId = 'network',
                         choices = network_choices(),
                         selected = sample(network_choices(), 1),
                     )
                 })
    
    # Output tabs ----
    ## RBIF ----
    rbif <- reactive({
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        df <- tbl(con, 'rbif') %>%
            filter(
                cell_id %in% local(input$cell_id),
                pert_type %in% local(input$pert_type),
                pert_iname %in% local(input$pert_iname)
            ) %>%
            collect() %>%
            mutate_if(is.numeric, ~ round(.x, 2))
        
        dbDisconnect(con)
        
        df
    })
    
    output$rbif <- renderDataTable({
        datatable({
            rbif()
        },
        
        extensions = 'Buttons',
        
        options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display")
    })
    
    wh <- reactiveValues(network = 1,
                         cell = 1,
                         cell_sim = 1,
                         family = 1)
    
    observeEvent(input$rbif_next, {
        wh$cell <- counter(wh$cell, length(input$cell_id))
    })
    observeEvent(input$rbif_back, {
        wh$cell <- counter(wh$cell, direction = 'back')
    })
    
    output$rbif_cell <- renderText({
        input$cell_id[wh$cell]
    })
    
    output$rbif_plot <- renderPlotly({
        df <- rbif() %>%
            filter(cell_id == input$cell_id[wh$cell]) %>%
            mutate(pert_iname = fct_reorder(pert_iname, desc(RBIF)))
        
        validate(need(
            nrow(df) > 0,
            'No significant perturbations were found for this cell line.'
        ))
        
        ggplotly(
            df %>%
                ggplot(aes(x = pert_iname, y = RBIF)) +
                geom_col() +
                theme(axis.text.x = element_text(
                    angle = 90, hjust = 1
                )) +
                labs(x = '', y = 'RBIF')
        )
    })
    
    ## BIF ----
    bif <- reactive({
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        df <- tbl(con, 'bif') %>%
            filter(
                cell_id %in% local(input$cell_id),
                pert_type %in% local(input$pert_type),
                pert_iname %in% local(input$pert_iname)
            ) %>%
            collect() %>%
            mutate_if(is.numeric, round, digits = 2)
        
        dbDisconnect(con)
        
        df
    })
    
    output$bif <- renderDataTable({
        datatable({
            bif()
        },
        
        extensions = 'Buttons',
        
        options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display")
    })
    
    observeEvent(input$bif_next, {
        wh$family <- counter(wh$family, length(input$family))
    })
    observeEvent(input$bif_back, {
        wh$family <- counter(wh$family, direction = 'back')
    })
    
    output$bif_family <- renderText({
        input$family[wh$family]
    })
    
    output$bif_plot <- renderPlotly({
        df <- bif() %>%
            pivot_longer(cols = which(sapply(., is.numeric)),
                         names_to = 'family',
                         values_to = 'BIF') %>%
            filter(family == input$family[wh$family]) %>%
            mutate(pert_iname = fct_reorder(pert_iname, desc(BIF)))
        
        validate(
            need(
                nrow(df) > 0,
                'No significant perturbations to this family were found for these cell lines.'
            )
        )
        
        ggplotly(
            df %>%
                ggplot(aes(x = pert_iname, y = BIF)) +
                geom_col(position = position_dodge()) +
                theme(axis.text.x = element_text(
                    angle = 90, hjust = 1
                )) +
                facet_grid(. ~ cell_id,
                           scales = 'free_x',
                           space = 'free_x') +
                labs(x = '', y = 'BIF')
        )
    })
    
    ## NPA ----
    npa <- reactive({
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        df <- tbl(con, 'networks') %>%
            filter(
                cell_id %in% local(input$cell_id),
                pert_type %in% local(input$pert_type),
                pert_iname %in% local(input$pert_iname),
                family %in% local(input$family),
                network %in% local(input$network)
            ) %>%
            filter(coefficients > 0) %>%
            collect() %>%
            mutate_if(is.numeric, round, digits = 2)
        
        dbDisconnect(con)
        df
    })
    
    output$npa <- renderDataTable({
        datatable({
            npa()
        },
        
        extensions = 'Buttons',
        
        options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display")
    })
    
    observeEvent(input$npa_next, {
        wh$network <- counter(wh$network, length(input$network))
    })
    observeEvent(input$npa_back, {
        wh$network <- counter(wh$network, direction = 'back')
    })
    
    output$npa_network <- renderText({
        input$network[wh$network]
    })
    
    output$npa_plot <- renderPlotly({
        df <- npa() %>%
            filter(network == input$network[wh$network]) %>%
            filter(coefficients > 0) %>%
            mutate(pert_iname = fct_reorder(pert_iname, desc(coefficients)))
        
        validate(
            need(
                nrow(df) > 0,
                'No significant perturbations to this network were found for these cell lines.'
            )
        )
        
        ggplotly(
            df %>%
                ggplot(aes(x = pert_iname, y = coefficients)) +
                geom_col(position = position_dodge()) +
                geom_linerange(aes(
                    ymin = down, ymax = up
                )) +
                theme(axis.text.x = element_text(
                    angle = 90, hjust = 1
                )) +
                facet_grid(. ~ cell_id,
                           scales = 'free_x',
                           space = 'free_x') +
                labs(x = '', y = 'NPA')
        )
    })
    
    ## NODES ----
    nodes <- reactive({
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        df <- tbl(con, 'nodes') %>%
            filter(
                cell_id %in% local(input$cell_id),
                pert_type %in% local(input$pert_type),
                pert_iname %in% local(input$pert_iname),
                family %in% local(input$family),
                network %in% local(input$network)
            ) %>%
            filter(abs(coefficient) > 0) %>%
            collect() %>%
            mutate_if(is.numeric, round, digits = 2)
        
        dbDisconnect(con)
        df
    })
    
    output$nodes <- renderDataTable({
        datatable({
            nodes()
        },
        
        extensions = 'Buttons',
        
        options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display")
    })
    
    observeEvent(input$nodes_next, {
        wh$cell <- counter(wh$cell, length(input$cell_id))
    })
    observeEvent(input$nodes_back, {
        wh$cell <- counter(wh$cell, direction = 'back')
    })
    
    output$nodes_cell <- renderText({
        input$cell_id[wh$cell]
    })
    
    output$nodes_plot <- renderPlotly({
        df <- nodes() %>%
            filter(cell_id == input$cell_id[wh$cell]) %>%
            mutate(node = fct_reorder(node, desc(abs(coefficient)))) %>%
            group_by(pert_iname) %>%
            top_n(coefficient, n = 5) # TODO: deal with many entries
        
        validate(
            need(
                nrow(df) > 0,
                'No significant nodes in this network were significantly perturbed in this cell line.'
            )
        )
        
        ggplotly(
            df %>%
                ggplot(
                    aes(
                        x = node,
                        y = coefficient,
                        ymin = down,
                        ymax = up
                    )
                ) +
                geom_col() +
                geom_linerange() +
                theme(
                    axis.text.x = element_text(angle = 90, hjust = 1),
                    panel.spacing = unit(0, 'mm')
                ) +
                facet_grid(
                    . ~ pert_iname,
                    scales = 'free_x',
                    space = 'free_x',
                    drop = TRUE
                ) +
                labs(x = '', y = 'Node Contribution')
        )
    })
    
    ## GRAPH ----
    observeEvent(input$graph_next, {
        wh$network <- counter(wh$network, length(input$network))
    })
    observeEvent(input$graph_back, {
        wh$network <- counter(wh$network, direction = 'back')
    })
    
    output$graph_name <- renderText({
        input$network[wh$network]
    })
    
    observe({
        updateSelectizeInput(
            session,
            inputId = 'overlay_pert_type',
            choices = input$pert_type,
            selected = head(input$pert_type, 1),
            server = TRUE
        )
        
        updateSelectizeInput(
            session,
            inputId = 'overlay_pert_iname',
            choices = input$pert_iname,
            selected = head(input$pert_iname, 1),
            server = TRUE
        )
        
        updateSelectizeInput(
            session,
            inputId = 'overlay_cell_id',
            choices = input$cell_id,
            selected = head(input$cell_id, 1),
            server = TRUE
        )
    })
    
    # extract graph info
    model <- reactive({
        # get nodes and edges
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        g <- tbl(con, 'models') %>%
            filter(model == local(input$network[wh$network])) %>%
            pull(graph) %>%
            unlist() %>%
            unserialize()
        
        dbDisconnect(con)
        
        g
    })
    
    graph <- reactiveValues(nodes = 0, edges = 0)
    
    # extract node coefficients
    graph_nodes_coeff <- reactive({
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        df <- tbl(con, 'nodes') %>%
            filter(pert_type == local(input$overlay_pert_type),
                   pert_iname == local(input$overlay_pert_iname),
                   cell_id == local(input$overlay_cell_id),
                   network == local(input$network[wh$network])) %>%
            dplyr::select(id = node, coefficient) %>%
            collect() %>%
            unique() %>%
            mutate(color = ifelse(coefficient > 0, 'red', 'green'),
                   clusters = ifelse(coefficient > 0, 'Module 1', 'Module 2'))
        
        dbDisconnect(con)
        
        df
    })
    
    output$test_tab <- renderDataTable({
        graph_nodes_coeff()
    })
    
    observe({
        graph$nodes <- reactive({
            as_data_frame(model(), 'vertices') %>%
                select(id = name) %>%
                left_join(graph_nodes_coeff())
        })
        graph$edges <- reactive({
                e <- as_data_frame(model(), 'edges')
                mutate(e, color = c('green', 'red')[factor(e$weight)])
        })
    })
    
    # observeEvent(input$overlay_signif, {
    #     graph$nodes <- reactive({
    #         mutate(graph$nodes(), color = ifelse(coefficient.pvalue < .05, color , 'gray'))
    #     })
    # })
    # observeEvent(input$overlay_scale, {
    #     graph$nodes <- reactive({
    #         mutate(graph$nodes(), size = 10 * as.numeric(abs(scale(coefficient + .001))))
    #     })
    # })
    
    output$graph <- renderVisNetwork({
        # TODO: find an elegant way to do this
        if (input$select_cluster) {cluster_by = 'clusters'}
        else {cluster_by = NULL}
        
        visNetwork(nodes = graph$nodes(),
                   edges = graph$edges()) %>%
            visEdges(arrows = list(to = list(
                enabled = TRUE, scaleFactor = 2
            ))) %>%
            visLayout(randomSeed = 123) %>%
            visInteraction(navigationButtons = TRUE) %>%
            visOptions(nodesIdSelection = input$select_node,
                       highlightNearest = input$highlight_nearest,
                       selectedBy = cluster_by) %>%
            visExport()
    })
    
    ## SIMILARITY ----
    observe({
        # update the pert_type based on pert_type and cell_id
        updateSelectInput(
            session,
            inputId = 'select_pert_type',
            choices = input$pert_type,
            selected = head(input$pert_type, 1)
        )
    })
    
    observe({
        # update the pert_iname based on pert_type and cell_id
        updateSelectizeInput(
            session,
            inputId = 'select_pert_iname',
            choices = pert_iname_choices(),
            selected = head(pert_iname_choices(), 1),
            server = TRUE
        )
    })
    
    observe({
        # update the number of similar
        validate(
            need(
                nrow(sim() > 1),
                'None is similar.'
            )
        )
        updateNumericInput(
            session,
            inputId = 'select_pert_number',
            label = 'Number',
            value = 1,
            min = 1,
            max = length(unique(sim()$Var2)),
            step = 1
        )
    })
    
    observeEvent(input$select_pert_next, {
        wh$cell_sim <- counter(wh$cell_sim,
                               length(unique(sim()$cell_id)))
    })
    
    observeEvent(input$select_pert_back, {
        wh$cell_sim <- counter(wh$cell_sim, direction = 'back')
    })
    
    output$select_pert_text <- renderText({
        unique(sim()$cell_id)[wh$cell_sim]
    })
    
    sim <- reactive({
        con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
        
        df <- tbl(con, 'similarity') %>%
            filter(pert_type == local(input$select_pert_type),
                   Var1 == local(input$select_pert_iname)) %>%
            collect()

        dbDisconnect(con)
        
        df
    })
    
    output$select_pert <- renderPlotly({
        df <- sim() %>%
            filter(cell_id == unique(sim()$cell_id)[wh$cell_sim])  %>%
            top_n(SCC, n = local(input$select_pert_number))
        
        validate(need(
            nrow(df) > 0,
            'No similar perturbations were found for this cell line.'
        ))
        
        ggplotly(
            df %>%
                mutate(Var2 = fct_reorder(Var2, dplyr::desc(abs(SCC)))) %>%
                ggplot(aes(x = Var2, y = SCC)) +
                geom_col() +
                lims(y = c(0, 1)) +
                labs(x = '') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                ggtitle(unique(df$cell_id))
        )
    })
    
    output$select_pert_tab <- renderDataTable({
        datatable({
            sim()
        },
        
        extensions = 'Buttons',
        
        options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display")
    })
    
    ## DOWNLOAD ----
    ### Selected files ----
    output$selected_info <- renderUI({
        HTML(paste(paste('The files below contain the data for selected items:'),
                   '<ul>',
                   paste('<li>Cell lines:', length(input$cell_id)), '</li>',
                   paste('<li>Networks: ', length(input$network)),'</li>',
                   paste('<li>Perturbations: ', length(input$pert_iname)), '</li>',
                   '</ul>',
                   sep = ''))
    })
    
    output$down_rbif <- downloadHandler(
        filename = function() {paste0('rbif-', Sys.Date(), '.tsv')},
        content = function(file) {write_tsv(rbif(), file)}
    )
    
    output$down_bif <- downloadHandler(
        filename = function() {paste0('bif-', Sys.Date(), '.tsv')},
        content = function(file) {write_tsv(bif(), file)}
    )
    
    output$down_npa <- downloadHandler(
        filename = function() {paste0('npa-', Sys.Date(), '.tsv')},
        content = function(file) {write_tsv(npa(), file)}
    )
    
    output$down_nodes <- downloadHandler(
        filename = function() {paste0('nodes-', Sys.Date(), '.tsv')},
        content = function(file) {write_tsv(nodes(), file)}
    )
    
    output$down_graph <- downloadHandler(
        filename = function() {paste0('graph-', Sys.Date(), '.rds')},
        content = function(file) {write_rds(graph(), file)}
    )
    
    output$down_sim <- downloadHandler(
        filename = function() {paste0('similarity-', Sys.Date(), '.tsv')},
        content = function(file) {write_tsv(sim(), file)}
    )
    
    ### Full database ----
    # network stats
    con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
    
    network_stats <- list(
        cell_lines = tbl(con, 'perturbations') %>% pull(cell_id) %>% unique(),
        perturbations = tbl(con, 'perturbations') %>% pull(pert_iname) %>% unique(),
        networks = tbl(con, 'models') %>% pull(model) %>% unique()
    )
    
    dbDisconnect(con)
    
    output$database_info <- renderUI({
        # TODO: values need to be hard coded.
        HTML(paste(paste('The files below contain the full data in the database:'),
                   '<ul>',
                   paste('<li>Cell lines:', length(network_stats$cell_lines)), '</li>',
                   paste('<li>Perturbations: ', length(network_stats$networks)),  '</li>',
                   paste('<li>Networks: ', length(network_stats$perturbations)), '</li>',
                   '</ul>',
                   sep = ''))
    })
    
    output$down_database <- downloadHandler(
        filename = function() {paste0('LINPS-', Sys.Date(), '.sqlite')},
        content = function(file) {file.copy(db_fl, file)}
    )

    output$down_perturbations <- downloadHandler(
        filename = function() {paste0('perturbations-', Sys.Date(), '.tsv')},
        content = function(file) {
            con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
            tbl(con, 'perturbations') %>%
                collect() %>%
                write_tsv(file)
            dbDisconnect(con)
        }
    )

    output$down_networks <- downloadHandler(
        filename = function() {paste0('networks-', Sys.Date(), '.rds')},
        content = function(file) {
            con <- dbConnect(RSQLite::SQLite(), dbname = db_fl)
            tbl(con, 'models') %>%
                collect() %>%
                write_rds(file)
            dbDisconnect(con)
        }
    )
}

shinyApp(ui = ui, server = server)
