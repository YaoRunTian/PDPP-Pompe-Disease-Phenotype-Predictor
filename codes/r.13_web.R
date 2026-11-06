library(shiny)
library(bslib)
library(DT)

# ============== Helper Functions ==============
icon_text <- function(icon_name, text) {
  tags$span(icon(icon_name), text)
}

# ============== Data Options ==============
Location_text <- c(
  "exon 1", "exon 2", "exon 3", "exon 4", "exon 5", "exon 6", "exon 7", 
  "exon 8", "exon 9", "exon 10", "exon 11", "exon 12", "exon 13",
  "exon 14", "exon 15", "exon 16", "exon 17", "exon 18", "exon 19", "exon 20", 
  "intron 1A", "intron 1B", "intron 2", "intron 3", "intron 5", "intron 6",
  "intron 7", "intron 8", "intron 9", "intron 10", "intron 11", "intron 12", 
  "intron 13", "intron 14", "intron 15", "intron 16", "intron 17", "intron 18", "intron 19"
)

SNP_text <- c(
  "A>C", "A>G", "A>T", "C>A", "C>G", "C>T", 
  "G>A", "G>C", "G>T", "T>A", "T>C", "T>G", "E"
)

# ============== UI ==============
ui <- page_navbar(
  title = tags$span(
    icon("dna", class = "me-2"),
    "Pompe Disease Phenotype Predictor"
  ),
  id = "nav",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#18BC9C",
    success = "#18BC9C",
    font_scale = 0.95,
    "navbar-bg" = "#2C3E50"
  ),
  
  header = tags$head(
    tags$style(HTML("
      /* ===== Card Styles ===== */
      .card {
        border: none;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
        margin-bottom: 1rem;
        overflow: visible !important;  /* Allow dropdown to overflow */
      }
      .card:hover {
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.12);
      }
      .card-header {
        background-color: #ffffff;
        border-bottom: 2px solid #f0f0f0;
        font-weight: 600;
        color: #2C3E50;
        border-radius: 12px 12px 0 0 !important;
        padding: 1rem 1.25rem;
      }
      .card-body {
        overflow: visible !important;  /* Allow dropdown to overflow */
      }
      
      /* ===== Button Styles ===== */
      .btn-primary {
        border-radius: 8px;
        font-weight: 600;
        padding: 10px 20px;
      }
      .form-control, .form-select {
        border-radius: 8px;
      }
      
      /* ===== Allele Card Styles ===== */
      .allele-card {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
        border-left: 4px solid #18BC9C;
        overflow: visible !important;
      }
      .allele-card.allele-2 {
        border-left-color: #3498db;
      }
      
      /* ===== Prediction Box Styles ===== */
      .prediction-box {
        font-size: 2rem;
        font-weight: 700;
        padding: 2rem;
        border-radius: 12px;
        text-align: center;
      }
      .prediction-IOPD {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
      }
      .prediction-LOPD {
        background: linear-gradient(135deg, #f1c40f 0%, #f39c12 100%);
        color: white;
      }
      .prediction-waiting {
        background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%);
        color: white;
      }
      
      .input-section-title {
        font-size: 1.1rem;
        font-weight: 600;
        color: #2C3E50;
        margin-bottom: 1rem;
        padding-bottom: 0.5rem;
        border-bottom: 2px solid #18BC9C;
      }
      
      /* ===== Fixed Layout Styles ===== */
      .main-container {
        display: flex;
        gap: 1.5rem;
        align-items: flex-start;
      }
      
      .input-panel {
        flex: 0 0 65%;
        max-width: 65%;
      }
      
      .result-panel {
        flex: 0 0 33%;
        max-width: 33%;
        position: sticky;
        top: 80px;
        align-self: flex-start;
      }
      
      /* ===== CRITICAL: Dropdown Overflow Fix ===== */
      .selectize-control {
        overflow: visible !important;
      }
      
      .selectize-dropdown {
        position: absolute !important;
        z-index: 99999 !important;
        overflow: visible !important;
      }
      
      .selectize-dropdown-content {
        max-height: 300px;
        overflow-y: auto;
      }
      
      /* Make all parent containers allow overflow */
      .allele-input-container,
      .allele-input-container .row,
      .allele-input-container .col-md-6,
      .allele-input-container .card,
      .allele-input-container .card-body,
      .input-panel,
      .input-panel > .card,
      .input-panel > .card > .card-body {
        overflow: visible !important;
      }
      
      .allele-input-container {
        min-height: 300px;
        position: relative;
        z-index: 100;
      }
      
      /* Responsive adjustments */
      @media (max-width: 992px) {
        .main-container {
          flex-direction: column;
        }
        .input-panel, .result-panel {
          flex: 0 0 100%;
          max-width: 100%;
          position: relative;
          top: 0;
        }
      }
    "))
  ),
  
  # -------- Page 1: Prediction Tool --------
  nav_panel(
    title = icon_text("calculator", "Prediction"),
    
    div(
      class = "container-fluid py-4",
      
      # Page title
      div(
        class = "text-center mb-4",
        tags$h2(icon("dna"), " Pompe Disease Phenotype Predictor"),
        tags$p(class = "text-muted lead", 
          "Random Forest model for predicting IOPD (Infantile-Onset) / LOPD (Late-Onset) phenotype"
        )
      ),
      
      # Main container with fixed layout
      div(
        class = "main-container",
        
        # Left: Input area
        div(
          class = "input-panel",
          card(
            card_header(
              icon("edit"), " Enter Variant Information"
            ),
            card_body(
              # Input format selection
              div(
                class = "mb-4",
                tags$label(class = "input-section-title", "Select Input Format"),
                radioButtons(
                  "input_format",
                  label = NULL,
                  choices = c(
                    "Format 1: Dropdown Selection (Location + SNP)" = "format1",
                    "Format 2: Direct Variant Name Input" = "format2"
                  ),
                  selected = "format1",
                  inline = TRUE
                )
              ),
              
              hr(),
              
              # Allele input container
              div(
                class = "allele-input-container",
                
                # Format 1: Dropdown selection
                conditionalPanel(
                  condition = "input.input_format == 'format1'",
                  
                  div(
                    class = "row",
                    
                    # Allele 1
                    div(
                      class = "col-md-6",
                      card(
                        class = "allele-card",
                        card_header(
                          tags$span(icon("circle", class = "text-success"), " Allele 1")
                        ),
                        card_body(
                          # Use selectizeInput with dropdownParent option
                          tags$div(
                            class = "mb-3",
                            tags$label(
                              class = "form-label",
                              tags$span(icon("map-marker-alt"), " Location:")
                            ),
                            selectizeInput(
                              "a1_location",
                              label = NULL,
                              choices = c("Please select..." = "", Location_text),
                              selected = "",
                              width = "100%",
                              options = list(
                                dropdownParent = 'body',
                                placeholder = 'Please select...'
                              )
                            )
                          ),
                          tags$div(
                            class = "mb-3",
                            tags$label(
                              class = "form-label",
                              tags$span(icon("exchange-alt"), " SNP Type:")
                            ),
                            selectizeInput(
                              "a1_snp",
                              label = NULL,
                              choices = c("Please select..." = "", SNP_text),
                              selected = "",
                              width = "100%",
                              options = list(
                                dropdownParent = 'body',
                                placeholder = 'Please select...'
                              )
                            )
                          )
                        )
                      )
                    ),
                    
                    # Allele 2
                    div(
                      class = "col-md-6",
                      card(
                        class = "allele-card allele-2",
                        card_header(
                          tags$span(icon("circle", class = "text-info"), " Allele 2")
                        ),
                        card_body(
                          tags$div(
                            class = "mb-3",
                            tags$label(
                              class = "form-label",
                              tags$span(icon("map-marker-alt"), " Location:")
                            ),
                            selectizeInput(
                              "a2_location",
                              label = NULL,
                              choices = c("Please select..." = "", Location_text),
                              selected = "",
                              width = "100%",
                              options = list(
                                dropdownParent = 'body',
                                placeholder = 'Please select...'
                              )
                            )
                          ),
                          tags$div(
                            class = "mb-3",
                            tags$label(
                              class = "form-label",
                              tags$span(icon("exchange-alt"), " SNP Type:")
                            ),
                            selectizeInput(
                              "a2_snp",
                              label = NULL,
                              choices = c("Please select..." = "", SNP_text),
                              selected = "",
                              width = "100%",
                              options = list(
                                dropdownParent = 'body',
                                placeholder = 'Please select...'
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                
                # Format 2: Direct input
                conditionalPanel(
                  condition = "input.input_format == 'format2'",
                  
                  div(
                    class = "row",
                    
                    # Allele 1 input
                    div(
                      class = "col-md-6",
                      card(
                        class = "allele-card",
                        card_header(
                          tags$span(icon("circle", class = "text-success"), " Allele 1")
                        ),
                        card_body(
                          textInput(
                            "a1_variant_name",
                            label = tags$span(icon("font"), " Variant Name:"),
                            placeholder = "e.g., c.525delT, p.Arg854X",
                            width = "100%"
                          )
                        )
                      )
                    ),
                    
                    # Allele 2 input
                    div(
                      class = "col-md-6",
                      card(
                        class = "allele-card allele-2",
                        card_header(
                          tags$span(icon("circle", class = "text-info"), " Allele 2")
                        ),
                        card_body(
                          textInput(
                            "a2_variant_name",
                            label = tags$span(icon("font"), " Variant Name:"),
                            placeholder = "e.g., c.-32-13T>G",
                            width = "100%"
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              hr(),
              
              # Submit button
              div(
                class = "text-center mt-4",
                actionButton(
                  "predict_btn",
                  label = tags$span(icon("play-circle"), " Run Prediction"),
                  class = "btn-primary btn-lg px-5"
                ),
                actionButton(
                  "reset_btn",
                  label = tags$span(icon("redo"), " Reset"),
                  class = "btn-outline-secondary btn-lg px-4 ms-2"
                )
              )
            )
          )
        ),
        
        # Right: Results display (sticky)
        div(
          class = "result-panel",
          
          # Prediction result card
          card(
            card_header(
              icon("chart-pie"), " Prediction Result"
            ),
            card_body(
              div(
                class = "prediction-box prediction-waiting",
                id = "prediction_result_box",
                uiOutput("prediction_result")
              )
            )
          ),
          
          # Input summary card
          card(
            card_header(
              icon("clipboard-list"), " Input Summary"
            ),
            card_body(
              uiOutput("input_summary")
            )
          ),
          
          # Probability distribution card
          card(
            card_header(
              icon("percentage"), " Prediction Probability"
            ),
            card_body(
              uiOutput("probability_display")
            )
          )
        )
      )
    )
  ),
  
  # -------- Page 2: Instructions --------
  nav_panel(
    title = icon_text("book", "Instructions"),
    
    div(
      class = "container py-4",
      style = "max-width: 900px;",
      
      card(
        card_body(
          class = "p-4",
          
          tags$h2(class = "text-center mb-4", "User Guide"),
          
          tags$h4(icon("info-circle", class = "text-primary"), " About This Tool"),
          tags$p(
            "This tool uses a Random Forest machine learning model to predict Pompe Disease phenotypes:",
            tags$ul(
              tags$li(tags$strong("IOPD"), " (Infantile-Onset Pompe Disease): Typically presents within the first year of life"),
              tags$li(tags$strong("LOPD"), " (Late-Onset Pompe Disease): Later age of onset")
            )
          ),
          
          hr(),
          
          tags$h4(icon("keyboard", class = "text-success"), " Input Format Instructions"),
          
          tags$h5("Format 1: Dropdown Selection"),
          tags$p("Select for each allele:"),
          tags$ul(
            tags$li(tags$strong("Location:"), " Variant location (exon or intron)"),
            tags$li(tags$strong("SNP:"), " Nucleotide change type (e.g., A>G, C>T) or E (for other variant types)")
          ),
          
          tags$h5("Format 2: Direct Input"),
          tags$p("Enter the HGVS variant nomenclature directly, for example:"),
          tags$ul(
            tags$li("c.525delT"),
            tags$li("c.-32-13T>G"),
            tags$li("p.Arg854X")
          ),
          
          hr(),
          
          tags$h4(icon("table", class = "text-info"), " Location Options"),
          tags$p("Available location options:"),
          tags$div(
            class = "bg-light p-3 rounded",
            tags$code(paste(Location_text, collapse = ", "))
          ),
          
          tags$h4(class = "mt-3", icon("dna", class = "text-warning"), " SNP Options"),
          tags$p("Available SNP type options:"),
          tags$div(
            class = "bg-light p-3 rounded",
            tags$code(paste(SNP_text, collapse = ", "))
          ),
          tags$p(class = "text-muted mt-2", 
            tags$em("Note: 'E' represents other variant types (e.g., deletions, insertions)")
          )
        )
      )
    )
  ),
  
  # -------- Page 3: About --------
  nav_panel(
    title = icon_text("info-circle", "About"),
    
    div(
      class = "container py-4",
      style = "max-width: 800px;",
      
      card(
        card_body(
          class = "p-4 text-center",
          
          tags$h2("About This Application"),
          tags$p(class = "lead", "Pompe Disease Phenotype Predictor v1.0"),
          
          hr(),
          
          layout_columns(
            col_widths = c(6, 6),
            
            div(
              tags$h5(icon("flask"), " Model Information"),
              tags$ul(
                class = "list-unstyled",
                tags$li("Algorithm: Random Forest"),
                tags$li("Framework: R / Shiny"),
                tags$li("Prediction Target: IOPD vs LOPD")
              )
            ),
            
            div(
              tags$h5(icon("envelope"), " Contact"),
              tags$p("For questions or suggestions:"),
              tags$p("Email: example@email.com")
            )
          ),
          
          hr(),
          
          tags$p(
            class = "text-muted",
            "Disclaimer: This tool is for research purposes only and should not replace professional medical diagnosis."
          )
        )
      )
    )
  ),
  
  nav_spacer()
)

# ============== Server ==============
server <- function(input, output, session) {
  
  prediction <- reactiveValues(
    result = NULL,
    prob_IOPD = NULL,
    prob_LOPD = NULL
  )
  
  # Reset button
 observeEvent(input$reset_btn, {
    updateSelectizeInput(session, "a1_location", selected = "")
    updateSelectizeInput(session, "a1_snp", selected = "")
    updateSelectizeInput(session, "a2_location", selected = "")
    updateSelectizeInput(session, "a2_snp", selected = "")
    updateTextInput(session, "a1_variant_name", value = "")
    updateTextInput(session, "a2_variant_name", value = "")
    
    prediction$result <- NULL
    prediction$prob_IOPD <- NULL
    prediction$prob_LOPD <- NULL
  })
  
  # Predict button
  observeEvent(input$predict_btn, {
    
    if (input$input_format == "format1") {
      
      if (is.null(input$a1_location) || input$a1_location == "" || 
          is.null(input$a1_snp) || input$a1_snp == "" ||
          is.null(input$a2_location) || input$a2_location == "" || 
          is.null(input$a2_snp) || input$a2_snp == "") {
        showNotification("Please fill in all fields!", type = "warning")
        return()
      }
      
      input_data <- data.frame(
        A1_Location = input$a1_location,
        A1_SNP = input$a1_snp,
        A2_Location = input$a2_location,
        A2_SNP = input$a2_snp,
        stringsAsFactors = FALSE
      )
      
    } else {
      
      if (is.null(input$a1_variant_name) || input$a1_variant_name == "" || 
          is.null(input$a2_variant_name) || input$a2_variant_name == "") {
        showNotification("Please fill in all fields!", type = "warning")
        return()
      }
      
      input_data <- data.frame(
        A1_Variant = input$a1_variant_name,
        A2_Variant = input$a2_variant_name,
        stringsAsFactors = FALSE
      )
    }
    
    # ========================================
    # TODO: Connect your RDS model here
    # ========================================
    # model <- readRDS("your_model.rds")
    # pred <- predict(model, input_data)
    # prob <- predict(model, input_data, type = "prob")
    # 
    # prediction$result <- as.character(pred)
    # prediction$prob_IOPD <- prob[1, "IOPD"]
    # prediction$prob_LOPD <- prob[1, "LOPD"]
    # ========================================
    
    # Simulated prediction result (temporary)
    prediction$prob_IOPD <- runif(1, 0, 1)
    prediction$prob_LOPD <- 1 - prediction$prob_IOPD
    prediction$result <- ifelse(prediction$prob_IOPD > 0.5, "IOPD", "LOPD")
    
    showNotification("Prediction complete!", type = "message")
  })
  
  # Prediction result display
  output$prediction_result <- renderUI({
    if (is.null(prediction$result)) {
      tags$div(
        icon("hourglass-half", class = "fa-2x mb-2"),
        tags$br(),
        "Awaiting input..."
      )
    } else {
      result <- prediction$result
      
      icon_html <- icon("exclamation-triangle", class = "fa-2x mb-2", style = "color: #ffffff;")
      
      tags$div(
        id = "result_display",
        class = paste0("prediction-box prediction-", result),
        style = "margin: -2rem; padding: 2rem;",
        icon_html,
        tags$br(),
        result,
        tags$br(),
        tags$small(
          ifelse(result == "IOPD", "Infantile-Onset Pompe Disease", "Late-Onset Pompe Disease")
        )
      )
    }
  })
  
  # Input summary
  output$input_summary <- renderUI({
    if (input$input_format == "format1") {
      a1_loc <- ifelse(is.null(input$a1_location) || input$a1_location == "", "", input$a1_location)
      a1_snp <- ifelse(is.null(input$a1_snp) || input$a1_snp == "", "", input$a1_snp)
      a2_loc <- ifelse(is.null(input$a2_location) || input$a2_location == "", "", input$a2_location)
      a2_snp <- ifelse(is.null(input$a2_snp) || input$a2_snp == "", "", input$a2_snp)
      
      tags$div(
        tags$p(
          tags$strong("Allele 1: "), 
          if (a1_loc == "" || a1_snp == "") {
            tags$em("Not filled", class = "text-muted")
          } else {
            paste0(a1_loc, " | ", a1_snp)
          }
        ),
        tags$p(
          tags$strong("Allele 2: "), 
          if (a2_loc == "" || a2_snp == "") {
            tags$em("Not filled", class = "text-muted")
          } else {
            paste0(a2_loc, " | ", a2_snp)
          }
        )
      )
    } else {
      a1_var <- ifelse(is.null(input$a1_variant_name) || input$a1_variant_name == "", "", input$a1_variant_name)
      a2_var <- ifelse(is.null(input$a2_variant_name) || input$a2_variant_name == "", "", input$a2_variant_name)
      
      tags$div(
        tags$p(
          tags$strong("Allele 1: "), 
          if (a1_var == "") {
            tags$em("Not filled", class = "text-muted")
          } else {
            a1_var
          }
        ),
        tags$p(
          tags$strong("Allele 2: "), 
          if (a2_var == "") {
            tags$em("Not filled", class = "text-muted")
          } else {
            a2_var
          }
        )
      )
    }
  })
  
  # Probability display
  output$probability_display <- renderUI({
    if (is.null(prediction$prob_IOPD)) {
      tags$div(
        class = "text-muted text-center",
        tags$em("Probability distribution will display after prediction")
      )
    } else {
      prob_iopd <- round(prediction$prob_IOPD * 100, 1)
      prob_lopd <- round(prediction$prob_LOPD * 100, 1)
      
      tags$div(
        # IOPD progress bar - Blue
        tags$div(
          class = "mb-3",
          tags$div(
            class = "d-flex justify-content-between mb-1",
            tags$span("IOPD"),
            tags$span(paste0(prob_iopd, "%"))
          ),
          tags$div(
            class = "progress",
            style = "height: 20px;",
            tags$div(
              class = "progress-bar",
              role = "progressbar",
              style = paste0("width: ", prob_iopd, "%; background-color: #3498db;"),
              `aria-valuenow` = prob_iopd,
              `aria-valuemin` = "0",
              `aria-valuemax` = "100"
            )
          )
        ),
        # LOPD progress bar - Yellow
        tags$div(
          tags$div(
            class = "d-flex justify-content-between mb-1",
            tags$span("LOPD"),
            tags$span(paste0(prob_lopd, "%"))
          ),
          tags$div(
            class = "progress",
            style = "height: 20px;",
            tags$div(
              class = "progress-bar",
              role = "progressbar",
              style = paste0("width: ", prob_lopd, "%; background-color: #f1c40f;"),
              `aria-valuenow` = prob_lopd,
              `aria-valuemin` = "0",
              `aria-valuemax` = "100"
            )
          )
        )
      )
    }
  })
}

# ============== Run App ==============
shinyApp(ui, server)