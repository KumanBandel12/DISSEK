library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(dplyr)
library(psych)
library(nortest)
library(car)
library(broom)
library(lmtest)
library(sandwich)
library(readr)
library(GGally)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(tinytex)
library(psych)
library(gridExtra)
library(sf)
library(spdep)
library(shinycssloaders)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Statistik"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-line")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator")),
      menuItem("Regresi Linier", tabName = "regresi", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "beranda",
              tabsetPanel(
                tabPanel("Tentang Dashboard",
                         fluidRow(
                           box(
                             title = tags$b("Selamat Datang di Dashboard Interaktif Analisis Statistik Sosial Ekonomi (DISSEK) "),
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             h4("Alat Komprehensif untuk Seluruh Alur Kerja Analisis Data Anda"),
                             p("Dashboard ini dirancang untuk memandu Anda melalui setiap langkah analisis data, mulai dari persiapan awal, eksplorasi mendalam, hingga pemodelan statistik yang kompleks. Manfaatkan menu di samping untuk menavigasi setiap fitur yang telah kami sediakan.")
                           )
                         ),
                         
                         fluidRow(
                           h3("Fungsionalitas Utama", style = "padding-left: 15px;")
                         ),
                         fluidRow(
                           infoBox(
                             "Manajemen Data", "Transformasi & Persiapan",
                             icon = icon("database"), color = "yellow", width = 4
                           ),
                           infoBox(
                             "Eksplorasi Data", "Visualisasi & Statistik Deskriptif",
                             icon = icon("chart-line"), color = "aqua", width = 4
                           ),
                           infoBox(
                             "Uji Asumsi", "Normalitas & Homogenitas",
                             icon = icon("check-circle"), color = "light-blue", width = 4
                           )
                         ),
                         fluidRow(
                           infoBox(
                             "Statistik Inferensia", "Uji Hipotesis & Analisis Spasial",
                             icon = icon("calculator"), color = "green", width = 4
                           ),
                           # Memperbaiki nama ikon yang salah
                           infoBox(
                             "Regresi Linier", "Pemodelan & Uji Diagnostik",
                             icon = icon("chart-bar"), color = "red", width = 4
                           ),
                           infoBox(
                             "Unduh Laporan", "Hasil dalam format Word",
                             icon = icon("download"), color = "purple", width = 4
                           )
                         ),
                         
                         # Baris untuk Box Detail yang Bisa Diciutkan
                         fluidRow(
                           box(
                             title = tags$b("Panduan Penggunaan dan Detail Fitur"),
                             status = "info", solidHeader = FALSE, width = 12,
                             collapsible = TRUE, collapsed = TRUE,
                             
                             h3("Langkah-langkah Penggunaan Dashboard"),
                             tags$ol(
                               tags$li(strong("Mulai dari Beranda:"), " Kenali kapabilitas dashboard dan lihat metadata variabel yang tersedia pada tab di atas."),
                               tags$li(strong("Manajemen Data:"), " Gunakan data bawaan atau unggah data Anda, lalu terapkan transformasi yang diperlukan seperti log, standardisasi, atau kategorisasi."),
                               tags$li(strong("Eksplorasi Data:"), " Gunakan statistik deskriptif dan berbagai plot interaktif (Histogram, Boxplot, Peta) untuk mendapatkan intuisi awal."),
                               tags$li(strong("Uji Asumsi:"), " Lakukan uji prasyarat seperti normalitas dan homogenitas untuk menentukan kelayakan uji statistik selanjutnya."),
                               tags$li(strong("Pilih Uji Statistik:"), " Lanjutkan ke menu Statistik Inferensia atau Regresi Linier sesuai hipotesis penelitian Anda."),
                               tags$li(strong("Interpretasi & Laporan:"), " Manfaatkan output dan interpretasi otomatis yang disediakan, lalu unduh laporan lengkap dalam format Word.")
                             ),
                             hr(),
                             h3("Detail Fitur Dashboard"),
                             h4(icon("database"), " Manajemen Data Dinamis"),
                             p("Siapkan data Anda untuk analisis dengan berbagai teknik transformasi. Lakukan standardisasi (Z-score), terapkan transformasi log atau akar kuadrat, atau lakukan kategorisasi variabel kontinu menjadi beberapa kelompok."),
                             h4(icon("chart-line"), " Eksplorasi Data Interaktif"),
                             p("Dapatkan ringkasan statistik mendalam dan buat visualisasi canggih secara instan. Pilih dari Histogram, Boxplot, Bar Chart, Scatter Plot, hingga Peta Choropleth untuk analisis geospasial."),
                             h4(icon("check-circle"), " Uji Asumsi Statistik Fundamental"),
                             p("Pastikan data Anda memenuhi asumsi normalitas (Shapiro-Wilk, Kolmogorov-Smirnov) dan homogenitas varians (Levene's Test, Bartlett's Test) sebelum melakukan uji parametrik."),
                             h4(icon("calculator"), " Statistik Inferensia Lengkap"),
                             p("Lakukan berbagai uji hipotesis, mulai dari Uji-t, Uji Proporsi, Uji Varians, ANOVA, hingga analisis autokorelasi spasial dengan Global Moran's I dan Peta Klaster LISA."),
                             h4(icon("chart-bar"), " Analisis Regresi Linier Berganda"),
                             p("Bangun model regresi untuk prediksi dan validasi model Anda dengan serangkaian uji asumsi penting, termasuk Multikolinearitas (VIF), Autokorelasi (Durbin-Watson), Homoskedastisitas (Breusch-Pagan), Normalitas Residual, dan analisis spasial pada residual.")
                           )
                         )
                ),
                tabPanel("Metadata Variabel",
                         fluidRow(
                           box(
                             title = "Deskripsi Variabel dalam Dataset",
                             status = "info",
                             solidHeader = TRUE,
                             width = 12,
                             DT::dataTableOutput("metadata_table")
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "manajemen",
              fluidRow(
                box(
                  title = "Transformasi Data",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  conditionalPanel(
                    condition = "output.dataUploaded",
                    selectInput("var_to_transform", "Pilih Variabel:", choices = NULL),
                    radioButtons("transform_type", "Jenis Transformasi:",
                                 choices = list(
                                   "Kategorisasi (Continuous → Categorical)" = "categorize",
                                   "Log Transformation" = "log",
                                   "Square Root" = "sqrt",
                                   "Standardisasi (Z-score)" = "standardize"
                                 )),
                    conditionalPanel(
                      condition = "input.transform_type == 'categorize'",
                      numericInput("n_categories", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                      radioButtons("categorize_method", "Metode:",
                                   choices = list("Equal Intervals" = "equal", "Quantiles" = "quantile"))
                    ),
                    actionButton("apply_transform", "Terapkan Transformasi", class = "btn-warning"),
                    br(), br(),
                    downloadButton("download_data", "Unduh Data", class = "btn-primary"),
                    br(), br(),
                    downloadButton("download_transform", "Unduh Hasil Transformasi (Word)", class = "btn-success")
                  )
                ),
                box(
                  title = "Preview Data",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  DT::dataTableOutput("data_preview")
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Transformasi Data",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("transform_interpretation")
                )
              )
      ),
      
      tabItem(tabName = "eksplorasi",
              tabsetPanel(
                tabPanel("Statistik Deskriptif",
                         fluidRow(
                           box(
                             title = "Statistik Deskriptif",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,
                             conditionalPanel(
                               condition = "output.dataUploaded",
                               selectInput("desc_var", "Pilih Variabel:", choices = NULL),
                               DT::dataTableOutput("descriptive_table")
                             )
                           ),
                           box(
                             title = "Interpretasi Eksplorasi Data",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,
                             verbatimTextOutput("exploration_interpretation"),
                             br(),
                             downloadButton("download_exploration_interp", "Unduh Interpretasi (Word)", class = "btn-success")
                           )
                         )
                ),
                tabPanel("Visualisasi",
                         fluidRow(
                           box(
                             title = "Visualisasi Data",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 12,
                             conditionalPanel(
                               condition = "output.dataUploaded",
                               fluidRow(
                                 column(4,
                                        radioButtons("plot_type", "1. Pilih Jenis Plot:",
                                                     choices = list("Histogram" = "hist",
                                                                    "Boxplot" = "box",
                                                                    "Bar Chart" = "bar",
                                                                    "Scatter Plot" = "scatter"))
                                 ),
                                 column(8,
                                        h4("2. Pilih Variabel"),
                                        conditionalPanel(
                                          condition = "input.plot_type == 'hist' || input.plot_type == 'box'",
                                          selectInput("var_single_numeric", "Pilih Variabel Numerik:", choices = NULL)
                                        ),
                                        # Conditional UI for Bar Chart
                                        conditionalPanel(
                                          condition = "input.plot_type == 'bar'",
                                          selectInput("var_categorical", "Pilih Variabel Kategorik:", choices = NULL)
                                        ),
                                        # Conditional UI for Scatter Plot
                                        conditionalPanel(
                                          condition = "input.plot_type == 'scatter'",
                                          fluidRow(
                                            column(6, selectInput("var_scatter_x", "Pilih Variabel X:", choices = NULL)),
                                            column(6, selectInput("var_scatter_y", "Pilih Variabel Y:", choices = NULL))
                                          )
                                        )
                                 )
                               ),
                               hr(),
                               plotlyOutput("data_plot"),
                               br(),
                               downloadButton("download_plot", "Unduh Plot", class = "btn-success")
                             )
                           )
                         )
                ),
                tabPanel("Peta Wilayah",
                         fluidRow(
                           box(
                             title = "Peta Choropleth Wilayah",
                             status = "success",
                             solidHeader = TRUE,
                             width = 12,
                             selectInput("map_var", "Pilih Variabel untuk Peta:", choices = NULL),
                             leafletOutput("data_map", height = "500px")
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "asumsi",
              tabsetPanel(
                tabPanel("Uji Normalitas",
                         fluidRow(
                           box(
                             title = "Uji Normalitas",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,
                             conditionalPanel(
                               condition = "output.dataUploaded",
                               selectInput("norm_var", "Pilih Variabel:", choices = NULL),
                               radioButtons("norm_test", "Jenis Uji:",
                                            choices = list("Shapiro-Wilk" = "shapiro",
                                                           "Kolmogorov-Smirnov" = "ks",
                                                           "Anderson-Darling" = "ad")),
                               actionButton("run_norm_test", "Jalankan Uji", class = "btn-primary"),
                               br(), br(),
                               verbatimTextOutput("normality_result"),
                               br(), br(),
                               verbatimTextOutput("normality_interpretation"),
                               downloadButton("download_norm_test", "Unduh Hasil Uji & Interpretasi (Word)", class = "btn-success"),
                             )
                           ),
                           box(
                             title = "Visualisasi Normalitas",
                             status = "info",
                             solidHeader = TRUE,
                             width = 6,
                             conditionalPanel(
                               condition = "output.dataUploaded",
                               plotlyOutput("normality_plot"),
                               downloadButton("download_norm_plot", "Unduh Plot", class = "btn-success")
                             )
                           )
                         )
                ),
                tabPanel("Uji Homogenitas",
                         fluidRow(
                           box(
                             title = "Uji Homogenitas",
                             status = "warning",
                             solidHeader = TRUE,
                             width = 12,
                             conditionalPanel(
                               condition = "output.dataUploaded",
                               fluidRow(
                                 column(6, selectInput("homo_var", "Pilih Variabel Numerik:", choices = NULL)),
                                 column(6, selectInput("homo_group", "Pilih Variabel Grup:", choices = NULL))
                               ),
                               radioButtons("homo_test", "Jenis Uji:",
                                            choices = list("Levene Test" = "levene",
                                                           "Bartlett Test" = "bartlett")),
                               actionButton("run_homo_test", "Jalankan Uji", class = "btn-warning"),
                               br(), br(),
                               verbatimTextOutput("homogeneity_result"),
                               br(), br(),
                               verbatimTextOutput("homogeneity_interpretation"),
                               downloadButton("download_homo_test", "Unduh Hasil Uji & Interpretasi (Word)", class = "btn-success"),
                             )
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "inferensia",
              fluidRow(
                box(
                  title = "Pilih Uji Statistik",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 5,
                  conditionalPanel(
                    condition = "output.dataUploaded",
                    tabsetPanel(id = "inference_tabs",
                                #  Tab 1: Uji Beda Rata-Rata 
                                tabPanel("Uji Beda Rata-Rata", value = "ttest",
                                         br(),
                                         radioButtons("ttest_type", "Pilih Jenis Uji:",
                                                      choices = list("Uji T 1 Sampel" = "t_one", "Uji T 2 Sampel" = "t_two")),
                                         hr(),
                                         conditionalPanel(
                                           condition = "input.ttest_type == 't_one'",
                                           selectInput("t_one_var", "Pilih Variabel:", choices = NULL),
                                           numericInput("t_one_mu", "Nilai Hipotesis (μ₀):", value = 0),
                                           radioButtons("t_one_tail", "Jenis Uji:",
                                                        choices = list("Dua Arah" = "two.sided",
                                                                       "Lebih Kecil" = "less",
                                                                       "Lebih Besar" = "greater"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.ttest_type == 't_two'",
                                           selectInput("t_two_var", "Pilih Variabel:", choices = NULL),
                                           selectInput("t_two_group", "Pilih Variabel Grup:", choices = NULL),
                                           radioButtons("t_two_tail", "Jenis Uji:",
                                                        choices = list("Dua Arah" = "two.sided",
                                                                       "Lebih Kecil" = "less",
                                                                       "Lebih Besar" = "greater")),
                                           radioButtons("t_two_var_equal", "Asumsi Varians:",
                                                        choices = list("Varians Sama" = TRUE,
                                                                       "Varians Berbeda" = FALSE))
                                         )
                                ),
                                #  Tab 2: Uji Proporsi 
                                tabPanel("Uji Proporsi", value = "prop",
                                         br(),
                                         selectInput("prop_var", "Pilih Variabel:", choices = NULL),
                                         selectInput("prop_level", "Pilih Kategori Sukses:", choices = NULL),
                                         numericInput("prop_p0", "Proporsi Hipotesis (p₀):", value = 0.5, min = 0, max = 1),
                                         radioButtons("prop_tail", "Jenis Uji:",
                                                      choices = list("Dua Arah" = "two.sided",
                                                                     "Lebih Kecil" = "less",
                                                                     "Lebih Besar" = "greater"))
                                ),
                                #  Tab 3: Uji Varians 
                                tabPanel("Uji Varians", value = "vartest",
                                         br(),
                                         radioButtons("vartest_type", "Pilih Jenis Uji:",
                                                      choices = list("Uji Varians 1 Sampel" = "var_one", "Uji Varians 2 Sampel" = "var_two")),
                                         hr(),
                                         conditionalPanel(
                                           condition = "input.vartest_type == 'var_one'",
                                           selectInput("var_one_var", "Pilih Variabel:", choices = NULL),
                                           numericInput("var_one_sigma", "Varians Hipotesis (σ²):", value = 1, min = 0),
                                           radioButtons("var_one_tail", "Jenis Uji:",
                                                        choices = list("Dua Arah" = "two.sided",
                                                                       "Lebih Kecil" = "less",
                                                                       "Lebih Besar" = "greater"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.vartest_type == 'var_two'",
                                           selectInput("var_two_var", "Pilih Variabel:", choices = NULL),
                                           selectInput("var_two_group", "Pilih Variabel Grup:", choices = NULL),
                                           radioButtons("var_two_tail", "Jenis Uji:",
                                                        choices = list("Dua Arah" = "two.sided",
                                                                       "Lebih Kecil" = "less",
                                                                       "Lebih Besar" = "greater"))
                                         )
                                ),
                                #  Tab 4: Uji ANOVA 
                                tabPanel("Uji ANOVA", value = "anovatest",
                                         br(),
                                         radioButtons("anovatest_type", "Pilih Jenis Uji:",
                                                      choices = list("ANOVA 1 Arah" = "anova_one", "ANOVA 2 Arah" = "anova_two")),
                                         hr(),
                                         conditionalPanel(
                                           condition = "input.anovatest_type == 'anova_one'",
                                           selectInput("anova_one_dep", "Variabel Dependen:", choices = NULL),
                                           selectInput("anova_one_factor", "Faktor:", choices = NULL)
                                         ),
                                         conditionalPanel(
                                           condition = "input.anovatest_type == 'anova_two'",
                                           selectInput("anova_two_dep", "Variabel Dependen:", choices = NULL),
                                           selectInput("anova_two_factor1", "Faktor 1:", choices = NULL),
                                           selectInput("anova_two_factor2", "Faktor 2:", choices = NULL)
                                         )
                                ),
                                tabPanel("Uji Moran's I", value = "moran",
                                         br(),
                                         selectInput("var_moran", "Pilih Variabel:", choices = NULL)
                                )
                    ),
                    br(),
                    actionButton("run_stat_test", "Jalankan Uji", class = "btn-primary", width = '100%'),
                    br(), br(),
                    downloadButton("download_stat_test", "Unduh Hasil Uji & Interpretasi (Word)", class = "btn-success", style = "width: 100%;"),
                  )
                ),
                box(
                  title = "Hasil Uji Statistik & Interpretasi",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 7,
                  verbatimTextOutput("stat_test_result"),
                  hr(),
                  verbatimTextOutput("inference_interpretation")
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.inference_tabs == 'moran'",
                  box(
                    title = "Peta Klaster LISA (Local Moran's I)", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 12,
                    withSpinner(leafletOutput("lisa_map", height = 500))
                  ),
                  box(
                    title = "Interpretasi Peta Klaster LISA",
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    verbatimTextOutput("lisa_interpretation_inferensia") 
                  )
                )
              )
      ),
      
      tabItem(tabName = "regresi",
              tabsetPanel(
                tabPanel("Model Regresi",
                         fluidRow(
                           box(
                             title = "Model Regresi",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 6,
                             conditionalPanel(
                               condition = "output.dataUploaded",
                               selectInput("reg_y", "Variabel Dependen (Y):", choices = NULL),
                               selectInput("reg_x", "Variabel Independen (X):", choices = NULL, multiple = TRUE),
                               actionButton("run_regression", "Jalankan Regresi", class = "btn-primary"),
                               br(), br(),
                               verbatimTextOutput("regression_summary"),
                               downloadButton("download_reg_summary", "Unduh Hasil Uji & Interpretasi (Word)", class = "btn-success"),
                             )
                           ),
                           box(
                             title = "Interpretasi Regresi",
                             status = "success",
                             solidHeader = TRUE,
                             width = 6,
                             verbatimTextOutput("regression_interpretation")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Plot Diagnostik Regresi",
                             status = "info",
                             solidHeader = TRUE,
                             width = 12,
                             conditionalPanel(
                               condition = "output.regressionRun",
                               plotOutput("regression_plots", height = "500px"),
                               downloadButton("download_diag_plots", "Unduh Plot Diagnostik", class = "btn-success")
                             )
                           )
                         )
                ),
                tabPanel("Uji Asumsi Regresi",
                         conditionalPanel(
                           condition = "output.regressionRun",
                           fluidRow(
                             box(
                               title = "Uji Asumsi Regresi",
                               status = "warning",
                               solidHeader = TRUE,
                               width = 6,
                               
                               h4("Pilih Uji untuk Dijalankan:"),
                               
                               fluidRow(
                                 column(6,
                                        actionButton("test_linearity", "Uji Multikolinearitas", class = "btn-info btn-block", style="margin-bottom: 10px;"),
                                        actionButton("test_independence", "Uji Autokorelasi", class = "btn-info btn-block", style="margin-bottom: 10px;")
                                 ),
                                 column(6,
                                        actionButton("test_homoscedasticity_bp", "Uji Homoskedastisitas", class = "btn-info btn-block", style="margin-bottom: 10px;"),
                                        actionButton("test_normality_resid", "Uji Normalitas Residual", class = "btn-info btn-block", style="margin-bottom: 10px;")
                                 )
                               ),
                               
                               hr(),
                               
                               verbatimTextOutput("regression_assumptions"),
                               br(),
                               downloadButton("download_assumptions", "Unduh Hasil Uji Asumsi (Word)", class = "btn-success btn-block")
                             ),
                             
                             box(
                               title = "Matriks Korelasi",
                               status = "success",
                               solidHeader = TRUE,
                               width = 6,
                               
                               actionButton("show_correlation", "Tampilkan Matriks Korelasi", class = "btn-success btn-block"),
                               br(), br(),
                               
                               DT::dataTableOutput("correlation_matrix"),
                               plotlyOutput("correlation_heatmap")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "!output.regressionRun",
                           box(
                             title = "Informasi", status = "info", solidHeader = TRUE, width = 12,
                             p("Silakan jalankan model regresi terlebih dahulu di tab 'Model Regresi' untuk dapat melakukan uji asumsi.")
                           )
                         )
                ),
                tabPanel("Autokorelasi Spasial Residual (Moran's I)",
                         conditionalPanel(
                           condition = "output.regressionRun",
                           fluidRow(
                             box(
                               title = "Jalankan Analisis", status = "info", solidHeader = TRUE, width = 12,
                               p("Klik tombol di bawah untuk menjalankan analisis autokorelasi spasial pada residual dari model regresi."),
                               actionButton("run_residual_moran", "Jalankan Analisis Moran's I Residual", class = "btn-primary", icon = icon("play-circle"))
                             )
                           ),
                           fluidRow(
                             box(
                               title = "Uji Global Moran's I pada Residual", status = "primary", solidHeader = TRUE, width = 6,
                               withSpinner(verbatimTextOutput("moran_residual_output"))
                             ),
                             box(
                               title = "Interpretasi Uji Moran's I pada Residual", status = "warning", solidHeader = TRUE, width = 6,
                               verbatimTextOutput("moran_residual_interpretation_output"),
                               br(),
                               downloadButton("download_moran_residual_interpretation", "Unduh Laporan (Word)", class = "btn-warning")
                             ),
                             box(
                               title = "Peta Klaster LISA untuk Residual", status = "success", solidHeader = TRUE, width = 12,
                               withSpinner(leafletOutput("lisa_residual_map", height = 500))
                             ),
                             box(
                               title = "Interpretasi Peta Klaster LISA",
                               status = "success",
                               solidHeader = TRUE,
                               width = 12,
                               verbatimTextOutput("lisa_interpretation")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "!output.regressionRun",
                           box(
                             title = "Informasi", status = "info", solidHeader = TRUE, width = 12,
                             p("Silakan jalankan model regresi terlebih dahulu di tab 'Model Regresi' untuk melihat analisis autokorelasi spasial pada residual.")
                           )
                         )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Fungsi baru untuk menghasilkan laporan Word
  generate_word_report <- function(output_file, template_path, params_list) {
    tryCatch({
      rmarkdown::render(
        input = template_path,
        output_format = "word_document", # Mengubah output ke Word
        output_file = output_file,
        params = params_list,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
    }, error = function(e) {
      showNotification(
        paste("Gagal membuat dokumen Word. Detail:", e$message), # Pesan error yang diperbarui
        type = "error", duration = 15
      )
      stop("Gagal render Word karena terjadi error.") # Pesan stop yang diperbarui
    })
  }
  
  output$metadata_table <- DT::renderDataTable({
    metadata_df <- data.frame(
      Variabel = c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
                   "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE",
                   "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
      Alias = c("Anak-anak", "Perempuan", "Lansia", "KRT Perempuan", "Ukuran Keluarga",
                "Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan", "Kemiskinan", "Buta Huruf",
                "Tanpa Pelatihan", "Rawan Bencana", "Rumah Sewa", "Tanpa Sanitasi", "Air Bersih", "Populasi"),
      Definisi = c("Persentase penduduk berusia di bawah 15 tahun terhadap total populasi",
                   "Persentase penduduk berjenis kelamin perempuan terhadap total populasi",
                   "Persentase penduduk berusia 65 tahun ke atas terhadap total populasi",
                   "Persentase rumah tangga dengan kepala rumah tangga perempuan",
                   "Rata-rata jumlah anggota keluarga per rumah tangga",
                   "Persentase rumah tangga yang tidak memiliki akses listrik",
                   "Persentase penduduk dengan tingkat pendidikan di bawah SMP",
                   "Tingkat pertumbuhan penduduk tahunan dalam persen",
                   "Persentase penduduk yang hidup di bawah garis kemiskinan",
                   "Persentase penduduk usia 15+ yang tidak dapat membaca dan menulis",
                   "Persentase penduduk yang belum pernah mengikuti pelatihan kerja",
                   "Indeks yang mengukur tingkat kerawanan terhadap bencana alam",
                   "Persentase rumah tangga yang tinggal di rumah sewa/kontrak",
                   "Persentase rumah tangga tanpa akses sanitasi yang layak",
                   "Persentase rumah tangga dengan akses air bersih",
                   "Jumlah total penduduk dalam wilayah tersebut"),
      Satuan = c("Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Orang",
                 "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)",
                 "Persen (%)", "Indeks", "Persen (%)", "Persen (%)", "Persen (%)", "Jiwa"),
      Tipe_Data = rep("Numerik", 16),
      Referensi = rep("SUSENAS BPS 2017", 16),
      Kategori_SOVI = c("Demografi", "Demografi", "Demografi", "Perumahan", "Demografi",
                        "Infrastruktur", "Sosial-Ekonomi", "Demografi", "Sosial-Ekonomi", "Sosial-Ekonomi",
                        "Sosial-Ekonomi", "Bencana", "Perumahan", "Infrastruktur", "Infrastruktur", "Demografi")
    )
    DT::datatable(metadata_df, options = list(scrollX = TRUE, pageLength = 10), class = 'cell-border stripe')
  })
  
  # Reactive values
  values <- reactiveValues(
    data = data.frame(),
    weights_matrix = NULL,
    transformed_data = data.frame(),
    map_data = NULL,
    matriks_bobot_raw = NULL,
    regression_model = NULL,
    assumption_history = list(), # *** BARU: Untuk menyimpan riwayat uji asumsi
    transform_interpretation_text = "Pilih variabel dan jenis transformasi, lalu klik 'Terapkan Transformasi'.",
    last_transformed_var = NULL,
    exploration_interpretation_text = "",
    normality_raw_output = NULL,
    normality_interpretation_text = NULL,
    homogeneity_raw_output = NULL,
    homogeneity_interpretation_text = NULL,
    moran_interpretation_text = "Jalankan analisis untuk melihat interpretasi.",
    moran_residual_interpretation_text = "Jalankan analisis residual untuk melihat interpretasi.",
    stat_test_output = NULL,
    stat_interpretation_output = NULL,
    reg_summary_output = NULL,
    reg_interpretation_output = NULL,
    moran_residual_raw_output = NULL
  )
  
  # Load dataset
  observe({
    tryCatch({
      data_path <- "www/Data UAS.csv"
      if(file.exists(data_path)) {
        df <- read_csv(data_path)
        df <- df %>% mutate_if(is.character, as.factor)
        values$data <- df
        values$transformed_data <- df
      } else {
        showNotification("File data tidak ditemukan. Silakan pastikan file 'Data UAS.csv' ada di direktori yang benar.", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error loading dataset:", e$message), type = "error")
    })
    
    tryCatch({
      map_path <- "www/PetaKab_Variabel.geojson"
      if(file.exists(map_path)) {
        values$map_data <- st_read(map_path, quiet = TRUE)
      } else {
        showNotification("File peta 'PetaKab_Variabel.geojson' tidak ditemukan.", type = "error", duration = 15)
      }
    }, error = function(e) {
      showNotification(paste("Error memuat GeoJSON:", e$message), type = "error", duration = 15)
    })
    
    tryCatch({
      matrix_path <- "www/Data Matrix Pembobot.csv"
      if(file.exists(matrix_path)) {
        values$matriks_bobot_raw <- as.matrix(read.csv(matrix_path, header = TRUE, row.names = 1))
      } else {
        showNotification("File matriks pembobot 'Data Matrix Pembobot.csv' tidak ditemukan.", type = "error", duration = 15)
      }
    }, error = function(e) {
      showNotification(paste("Error memuat matriks pembobot:", e$message), type = "error", duration = 15)
    })
  })
  
  # Update variable choices when data changes
  observe({
    df <- values$transformed_data
    if(nrow(df) > 0) {
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      all_vars <- names(df)
      categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
      
      numeric_vars_for_plot <- numeric_vars[!numeric_vars %in% "DISTRICTCODE"]
      
      updateSelectInput(session, "var_to_transform", choices = all_vars)
      updateSelectInput(session, "desc_var", choices = all_vars)
      
      updateSelectInput(session, "var_single_numeric", choices = numeric_vars_for_plot)
      updateSelectInput(session, "var_categorical", choices = categorical_vars)
      updateSelectInput(session, "var_scatter_x", choices = numeric_vars_for_plot)
      updateSelectInput(session, "var_scatter_y", choices = numeric_vars_for_plot)
      
      updateSelectInput(session, "norm_var", choices = numeric_vars)
      updateSelectInput(session, "homo_var", choices = numeric_vars)
      updateSelectInput(session, "homo_group", choices = categorical_vars)
      updateSelectInput(session, "t_one_var", choices = numeric_vars)
      updateSelectInput(session, "t_two_var", choices = numeric_vars)
      updateSelectInput(session, "t_two_group", choices = categorical_vars)
      updateSelectInput(session, "prop_var", choices = categorical_vars)
      updateSelectInput(session, "var_one_var", choices = numeric_vars)
      updateSelectInput(session, "var_two_var", choices = numeric_vars)
      updateSelectInput(session, "var_two_group", choices = categorical_vars)
      
      updateSelectInput(session, "anova_one_dep", choices = numeric_vars)
      updateSelectInput(session, "anova_one_factor", choices = categorical_vars)
      updateSelectInput(session, "anova_two_dep", choices = numeric_vars)
      updateSelectInput(session, "anova_two_factor1", choices = categorical_vars)
      updateSelectInput(session, "anova_two_factor2", choices = categorical_vars)
      
      updateSelectInput(session, "reg_y", choices = numeric_vars)
      updateSelectInput(session, "reg_x", choices = numeric_vars)
      
      vars_to_exclude <- "DISTRICTCODE"
      final_map_choices <- numeric_vars[!numeric_vars %in% vars_to_exclude]
      updateSelectInput(session, "map_var", choices = final_map_choices)
      updateSelectInput(session, "var_moran", choices = final_map_choices)
      
      # Update prop_level choices when prop_var changes
      observeEvent(input$prop_var, {
        if (!is.null(input$prop_var) && input$prop_var != "") {
          levels <- unique(as.character(values$transformed_data[[input$prop_var]]))
          updateSelectInput(session, "prop_level", choices = levels)
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Check for coordinate variables
      has_coords <- any(grepl("lat|latitude", names(df), ignore.case = TRUE)) &&
        any(grepl("lon|longitude|lng", names(df), ignore.case = TRUE))
      
      if(has_coords) {
        lat_vars <- names(df)[grepl("lat|latitude", names(df), ignore.case = TRUE)]
        lon_vars <- names(df)[grepl("lon|longitude|lng", names(df), ignore.case = TRUE)]
        updateSelectInput(session, "lat_var", choices = lat_vars)
        updateSelectInput(session, "lon_var", choices = lon_vars)
      }
      
      vars_to_exclude <- "DISTRICTCODE"
      final_map_choices <- numeric_vars[!numeric_vars %in% vars_to_exclude]
      updateSelectInput(session, "map_var", choices = final_map_choices)
    }
  })
  
  # Output flags
  output$dataUploaded <- reactive({
    return(nrow(values$data) > 0)
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  output$hasCoordinates <- reactive({
    if(nrow(values$data) == 0) return(FALSE)
    has_coords <- any(grepl("lat|latitude", names(values$data), ignore.case = TRUE)) &&
      any(grepl("lon|longitude|lng", names(values$data), ignore.case = TRUE))
    return(has_coords)
  })
  outputOptions(output, "hasCoordinates", suspendWhenHidden = FALSE)
  
  output$regressionRun <- reactive({
    !is.null(values$regression_model)
  })
  outputOptions(output, "regressionRun", suspendWhenHidden = FALSE)
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$transformed_data)
    DT::datatable(values$transformed_data, options = list(scrollX = TRUE))
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("dataset_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$transformed_data, file, row.names = FALSE)
    }
  )
  
  generate_transform_interpretation <- function(df, var_name, transform_type, categorize_method, n_categories) {
    new_var_name <- ""
    
    if(transform_type == "categorize") {
      new_var_name <- paste0(var_name, "_cat")
      if(is.numeric(df[[var_name]])) {
        if(categorize_method == "equal") {
          df[[new_var_name]] <- cut(df[[var_name]], breaks = n_categories, labels = paste0("Cat_", 1:n_categories), include.lowest = TRUE)
        } else { # quantile
          df[[new_var_name]] <- cut(df[[var_name]], breaks = quantile(df[[var_name]], probs = seq(0, 1, length.out = n_categories + 1), na.rm = TRUE), labels = paste0("Cat_", 1:n_categories), include.lowest = TRUE)
        }
      }
    } else if(transform_type == "log") {
      new_var_name <- paste0(var_name, "_log")
      if(is.numeric(df[[var_name]]) && all(df[[var_name]] > 0, na.rm = TRUE)) { df[[new_var_name]] <- log(df[[var_name]]) }
    } else if(transform_type == "sqrt") {
      new_var_name <- paste0(var_name, "_sqrt")
      if(is.numeric(df[[var_name]]) && all(df[[var_name]] >= 0, na.rm = TRUE)) { df[[new_var_name]] <- sqrt(df[[var_name]]) }
    } else if(transform_type == "standardize") {
      new_var_name <- paste0(var_name, "_std")
      if(is.numeric(df[[var_name]])) { df[[new_var_name]] <- scale(df[[var_name]])[,1] }
    }
    
    new_data <- df[[new_var_name]][!is.na(df[[new_var_name]])]
    transform_desc <- switch(transform_type,
                             "categorize" = paste("Kategorisasi dengan metode",
                                                  ifelse(categorize_method == "equal", "interval sama", "kuantil"),
                                                  "menjadi", n_categories, "kategori"),
                             "log" = "Transformasi logaritma natural",
                             "sqrt" = "Transformasi akar kuadrat",
                             "standardize" = "Standardisasi (Z-score) dengan mean=0 dan sd=1")
    
    orig_data <- df[[var_name]][!is.na(df[[var_name]])]
    orig_stats <- if(is.numeric(orig_data)) {
      paste("Original - Mean:", round(mean(orig_data), 3),
            ", SD:", round(sd(orig_data), 3),
            ", Min:", round(min(orig_data), 3),
            ", Max:", round(max(orig_data), 3))
    } else {
      paste("Original - Tipe:", class(orig_data),
            ", Kategori:", length(unique(orig_data)))
    }
    
    if(transform_type == "categorize") {
      freq_table <- table(new_data)
      trans_stats <- paste(
        "Kategori:", paste(names(freq_table), collapse=", "), "\n",
        "Frekuensi:", paste(freq_table, collapse=", "), "\n",
        "Distribusi:", paste(round(prop.table(freq_table)*100, 1), "%", collapse=", ")
      )
    } else {
      trans_stats <- paste(
        "Mean:", round(mean(new_data), 3), "\n",
        "SD:", round(sd(new_data), 3), "\n",
        "Min:", round(min(new_data), 3), "\n",
        "Max:", round(max(new_data), 3), "\n",
        "Skewness:", round(psych::skew(new_data), 3)
      )
    }
    
    return(
      paste(
        "HASIL TRANSFORMASI DATA",
        "\n\nVariabel Asli:", var_name,
        "\nVariabel Hasil:", new_var_name,
        "\nJenis Transformasi:", transform_desc,
        "\n\nSTATISTIK DATA",
        "\n", orig_stats,
        "\n\nSTATISTIK HASIL TRANSFORMASI",
        "\n", trans_stats,"\n"
      )
    )
  }
  
  observeEvent(input$apply_transform, {
    req(values$data, input$var_to_transform, input$transform_type)
    
    df <- values$transformed_data
    var_name <- input$var_to_transform
    
    tryCatch({
      new_var_name <- ""
      
      if(input$transform_type == "categorize") {
        new_var_name <- paste0(var_name, "_cat")
        if(is.numeric(df[[var_name]])) {
          if(input$categorize_method == "equal") {
            df[[new_var_name]] <- cut(df[[var_name]],
                                      breaks = input$n_categories,
                                      labels = paste0("Cat_", 1:input$n_categories),
                                      include.lowest = TRUE)
          } else { # quantile
            df[[new_var_name]] <- cut(df[[var_name]],
                                      breaks = quantile(df[[var_name]],
                                                        probs = seq(0, 1, length.out = input$n_categories + 1),
                                                        na.rm = TRUE),
                                      labels = paste0("Cat_", 1:input$n_categories),
                                      include.lowest = TRUE)
          }
        } else {
          showNotification("Variabel harus numerik untuk kategorisasi.", type = "error")
          return()
        }
      } else if(input$transform_type == "log") {
        new_var_name <- paste0(var_name, "_log")
        if(is.numeric(df[[var_name]]) && all(df[[var_name]] > 0, na.rm = TRUE)) {
          df[[new_var_name]] <- log(df[[var_name]])
        } else {
          showNotification("Variabel harus numerik dan positif untuk transformasi log.", type = "error")
          return()
        }
      } else if(input$transform_type == "sqrt") {
        new_var_name <- paste0(var_name, "_sqrt")
        if(is.numeric(df[[var_name]]) && all(df[[var_name]] >= 0, na.rm = TRUE)) {
          df[[new_var_name]] <- sqrt(df[[var_name]])
        } else {
          showNotification("Variabel harus numerik dan non-negatif untuk transformasi sqrt.", type = "error")
          return()
        }
      } else if(input$transform_type == "standardize") {
        new_var_name <- paste0(var_name, "_std")
        if(is.numeric(df[[var_name]])) {
          df[[new_var_name]] <- scale(df[[var_name]])[,1]
        } else {
          showNotification("Variabel harus numerik untuk standardisasi.", type = "error")
          return()
        }
      }
      
      # Update reactive data
      values$transformed_data <- df
      showNotification("Transformasi berhasil diterapkan!", type = "message")
      
      # Generate and update interpretation text
      values$transform_interpretation_text <- generate_transform_interpretation(
        df = df,
        var_name = input$var_to_transform,
        transform_type = input$transform_type,
        categorize_method = input$categorize_method,
        n_categories = input$n_categories
      )
      
      values$last_transformed_var <- var_name
    }, error = function(e) {
      showNotification(paste("Error during transformation:", e$message), type = "error")
      values$transform_interpretation_text <- paste("Error:", e$message)
    })
  })
  
  output$transform_interpretation <- renderText({
    req(values$transform_interpretation_text)
    values$transform_interpretation_text
  })
  
  output$download_transform <- downloadHandler(
    filename = function() {
      var_name <- req(values$last_transformed_var)
      paste("laporan_transformasi_", var_name, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(values$transform_interpretation_text)
      
      params_for_pdf <- list(
        main_title = "Laporan Hasil Transformasi Data",
        subtitle = paste("Analisis Variabel:", input$var_to_transform),
        main_content = values$transform_interpretation_text
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_pdf
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  
  # Descriptive statistics
  output$descriptive_table <- DT::renderDataTable({
    req(values$transformed_data, input$desc_var)
    
    var_data <- values$transformed_data[[input$desc_var]]
    
    if(is.numeric(var_data)) {
      stats <- psych::describe(var_data)
      desc_df <- data.frame(
        Statistik = c("Jumlah", "Rata-rata", "Median", "Standar Deviasi", "Skewness", "Kurtosis", "Minimum", "Maksimum", "SE Mean", "Varians"),
        Nilai = c(stats$n, round(stats$mean, 3), round(stats$median, 3), round(stats$sd, 3),
                  round(stats$skew, 3), round(stats$kurtosis, 3), round(stats$min, 3), round(stats$max, 3),
                  round(stats$se, 3), round(stats$sd^2, 3))
      )
    } else {
      freq_table <- table(var_data)
      prop_table <- prop.table(freq_table) * 100
      desc_df <- data.frame(
        Kategori = names(freq_table),
        Frekuensi = as.numeric(freq_table),
        Persentase = round(as.numeric(prop_table), 2)
      )
    }
    
    DT::datatable(desc_df, options = list(pageLength = 10))
  })
  
  # Data plot
  plot_object <- reactive({
    req(values$transformed_data, input$plot_type)
    df <- values$transformed_data
    
    tryCatch({
      p <- NULL # Inisialisasi p
      
      if(input$plot_type %in% c("hist", "box")) {
        req(input$var_single_numeric)
        var_name <- input$var_single_numeric
        
        if(input$plot_type == "hist") {
          p <- ggplot(df, aes_string(x = var_name)) +
            geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
            theme_minimal() + labs(title = paste("Histogram of", var_name))
        } else if(input$plot_type == "box") {
          p <- ggplot(df, aes_string(y = var_name)) +
            geom_boxplot(fill = "steelblue", alpha = 0.7) +
            theme_minimal() + labs(title = paste("Boxplot of", var_name))
        }
        
      } else if(input$plot_type == "bar") {
        req(input$var_categorical)
        var_name <- input$var_categorical
        
        freq_table <- as.data.frame(table(df[[var_name]]))
        names(freq_table) <- c("Category", "Count")
        p <- ggplot(freq_table, aes(x = reorder(Category, -Count), y = Count)) +
          geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Bar Chart of", var_name), x = var_name, y = "Count") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else if(input$plot_type == "scatter") {
        req(input$var_scatter_x, input$var_scatter_y)
        var_x <- input$var_scatter_x
        var_y <- input$var_scatter_y
        
        p <- ggplot(df, aes_string(x = var_x, y = var_y)) +
          geom_point(color = "steelblue", alpha = 0.7) +
          geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
          theme_minimal() +
          labs(title = paste("Scatter Plot of", var_y, "vs", var_x), x = var_x, y = var_y)
      }
      
      return(p) # Kembalikan objek ggplot
      
    }, error = function(e) {
      showNotification(paste("Error in plotting:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$data_plot <- renderPlotly({
    p <- plot_object()
    if (!is.null(p)) {
      ggplotly(p)
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      plot_type <- input$plot_type
      var_name <- ""
      if (plot_type %in% c("hist", "box")) {
        var_name <- input$var_single_numeric
      } else if (plot_type == "bar") {
        var_name <- input$var_categorical
      } else if (plot_type == "scatter") {
        var_name <- paste(input$var_scatter_y, "vs", input$var_scatter_x, sep="_")
      }
      paste("plot_", plot_type, "_", var_name, "_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      p <- plot_object()
      if (!is.null(p)) {
        ggsave(file, plot = p, device = "jpeg", width = 10, height = 8, dpi = 300)
      }
    }
  )
  
  # Data map
  output$data_map <- renderLeaflet({
    req(values$map_data, input$map_var)
    
    map_data <- values$map_data
    variable_to_plot <- input$map_var
    
    if (!variable_to_plot %in% names(map_data)) {
      showNotification(paste("Variabel", variable_to_plot, "tidak ada di data peta."), type = "error")
      return(NULL)
    }
    
    plot_values <- map_data[[variable_to_plot]]
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = plot_values,
      na.color = "#bdbdbd"
    )
    
    popup_text <- paste(
      "<strong>Wilayah:</strong>", map_data$nmkab, "<br/>",
      "<strong>", variable_to_plot, ":</strong>", round(plot_values, 2)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron", group = "Peta Terang") %>%
      addProviderTiles("OpenStreetMap", group = "Peta Detail") %>%
      setView(lng = 118, lat = -2, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(plot_values),
        weight = 1.5,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          bringToFront = TRUE),
        label = popup_text,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~plot_values, opacity = 0.7, title = variable_to_plot,
                position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("Peta Terang", "Peta Detail"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Exploration interpretation
  exploration_interp_reactive <- eventReactive(input$desc_var, {
    req(values$transformed_data, input$desc_var)
    var_data <- values$transformed_data[[input$desc_var]]
    
    tryCatch({
      if(is.numeric(var_data)) {
        stats <- psych::describe(var_data)
        skew_interp <- ifelse(abs(stats$skew) < 0.5, "simetris",
                              ifelse(stats$skew > 0, "condong ke kanan (positif)", "condong ke kiri (negatif)"))
        kurt_interp <- ifelse(stats$kurtosis > 3, "leptokurtik (puncak tajam)",
                              ifelse(stats$kurtosis < 3, "platykurtik (puncak datar)", "mesokurtik (mirip normal)"))
        
        paste(
          "Interpretasi Eksplorasi Data untuk ", input$desc_var, ":",
          "\n\nStatistik Deskriptif:",
          "\n- Jumlah Observasi:", stats$n, "- Total data yang valid.",
          "\n- Rata-rata:", round(stats$mean, 3), "- Pusat distribusi data.",
          "\n- Median:", round(stats$median, 3), "- Nilai tengah setelah data diurutkan.",
          "\n- Standar Deviasi:", round(stats$sd, 3), "- Penyebaran data dari rata-rata.",
          "\n- Varians:", round(stats$sd^2, 3), "- Kuadrat standar deviasi, mengukur variabilitas.",
          "\n- Skewness:", round(stats$skew, 3), "- Distribusi", skew_interp, ".",
          "\n- Kurtosis:", round(stats$kurtosis, 3), "- Bentuk distribusi", kurt_interp, ".",
          "\n- Minimum:", round(stats$min, 3), ", Maksimum:", round(stats$max, 3), "- Rentang data.",
          "\n- SE Mean:", round(stats$se, 3), "- Ketelitian estimasi rata-rata.",
          "\n\nImplikasi Distribusi:",
          "\n• Skewness mendekati 0 menunjukkan distribusi simetris, cocok untuk uji parametrik.",
          "\n• Skewness besar (>|0.5|) menunjukkan distribusi tidak simetris, perlu transformasi.",
          "\n• Kurtosis > 3 menunjukkan banyak data di puncak/ekor; < 3 menunjukkan distribusi datar.",
          "\n• Standar deviasi besar menunjukkan variabilitas tinggi, perlu perhatian dalam analisis.",
          "\n\nRekomendasi:",
          "\n• Gunakan histogram/density plot untuk memahami bentuk distribusi.",
          "\n• Gunakan boxplot untuk mendeteksi outlier (jika ada titik ekstrem).",
          "\n• Jika skewness/kurtosis besar, pertimbangkan transformasi (log/sqrt) di tab Manajemen Data.",
          "\n• Lakukan uji normalitas di tab Uji Asumsi sebelum uji parametrik (t-test/ANOVA).",
          "\n• Jika variabilitas tinggi, perhatikan asumsi homogenitas varians untuk uji inferensial."
        )
      } else {
        freq_table <- table(var_data)
        prop_table <- prop.table(freq_table) * 100
        mode_val <- names(freq_table)[which.max(freq_table)]
        min_freq <- min(freq_table)
        min_cat <- names(freq_table)[which.min(freq_table)]
        
        paste(
          "Interpretasi Eksplorasi Data untuk ", input$desc_var, ":",
          "\n\nAnalisis Kategorikal:",
          "\n- Jumlah Kategori:", length(freq_table), "- Total kelompok unik dalam data.",
          "\n- Kategori Dominan:", mode_val, "(", max(freq_table), "observasi, ", round(max(prop_table), 2), "%).",
          "\n- Kategori Terkecil:", min_cat, "(", min_freq, "observasi, ", round(min(prop_table), 2), "%).",
          "\n\nDistribusi Kategori:",
          paste(sapply(names(freq_table), function(x) paste("- ", x, ": ", freq_table[x], " (", round(prop_table[x], 2), "%)")), collapse = "\n"),
          "\n\nImplikasi:",
          "\n• Kategori dengan frekuensi rendah (<5) dapat menyebabkan bias dalam uji statistik.",
          "\n• Distribusi tidak seimbang (kategori dominan >> lainnya) dapat memengaruhi interpretasi.",
          "\n• Kategori kecil mungkin tidak representatif untuk generalisasi.",
          "\n\nRekomendasi:",
          "\n• Gunakan bar chart untuk visualisasi distribusi kategori.",
          "\n• Jika kategori terlalu kecil, pertimbangkan penggabungan di tab Manajemen Data.",
          "\n• Lakukan uji chi-square di tab Statistik Inferensia untuk hubungan antar variabel kategorikal.",
          "\n• Pastikan ukuran sampel per kategori cukup untuk analisis inferensial."
        )
      }
    }, error = function(e) {
      paste("Error in exploration interpretation:", e$message)
    })
  })
  
  output$exploration_interpretation <- renderText({
    interp_text <- exploration_interp_reactive()
    values$exploration_interpretation_text <- interp_text
    interp_text
  })
  
  output$download_exploration_interp <- downloadHandler(
    filename = function() {
      paste("laporan_eksplorasi_", input$desc_var, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(values$exploration_interpretation_text)
      
      params_for_word <- list(
        main_title = "Laporan Eksplorasi Data",
        subtitle = paste("Analisis Variabel:", input$desc_var),
        main_content = values$exploration_interpretation_text
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_word
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  
  # Normality test
  observeEvent(input$run_norm_test, {
    req(values$transformed_data, input$norm_var, input$norm_test)
    
    var_data <- values$transformed_data[[input$norm_var]]
    var_data <- var_data[!is.na(var_data)]
    
    tryCatch({
      test_result <- NULL
      if(input$norm_test == "shapiro") {
        if(length(var_data) > 2 && length(var_data) <= 5000) {
          test_result <- shapiro.test(var_data)
        } else {
          test_result <- list(statistic = NA, p.value = NA,
                              method = "Shapiro-Wilk test requires sample size between 3 and 5000. Try another test.")
        }
      } else if(input$norm_test == "ks") {
        test_result <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
      } else if(input$norm_test == "ad") {
        test_result <- ad.test(var_data)
      }
      
      p_val <- test_result$p.value
      stat_val <- test_result$statistic
      
      conclusion_text <- if(is.na(p_val)) {
        "Tidak dapat ditarik kesimpulan karena uji tidak valid (misal: ukuran sampel di luar batas)."
      } else if(p_val < 0.05) {
        paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa data variabel '", input$norm_var, "' tidak berdistribusi normal.")
      } else {
        paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa data variabel '", input$norm_var, "' berdistribusi normal.")
      }
      
      interpretation_text <- paste(
        "Interpretasi Uji Normalitas untuk", input$norm_var, "dengan metode", test_result$method, ":",
        "\n\nHipotesis:",
        "\n H0: Data berdistribusi normal",
        "\n H1: Data tidak berdistribusi normal",
        "\n\nTingkat Signifikansi: 5%",
        "\n\nHasil Uji:",
        "\n Statistik Uji:", if(is.na(stat_val)) "NA" else round(stat_val, 4),
        "\n P-value:", if(is.na(p_val)) "NA" else format.pval(p_val),
        "\n\nKeputusan:",
        if(is.na(p_val)) {
          "\n Uji tidak dapat dilakukan. Cek pesan error di atas (misal, ukuran sampel terlalu besar/kecil)."
        } else if(p_val < 0.05) {
          "\n Karena P-value < 0.05 maka Tolak H0."
        } else {
          "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0."
        },
        "\n\nKesimpulan:",
        "\n", conclusion_text
      )
      
      # Update UI and reactive values for download
      output$normality_result <- renderPrint({ test_result })
      output$normality_interpretation <- renderText({ interpretation_text })
      values$normality_raw_output <- capture.output(print(test_result))
      values$normality_interpretation_text <- interpretation_text
      
    }, error = function(e) {
      showNotification(paste("Error in normality test:", e$message), type = "error")
    })
  })
  
  output$download_norm_test <- downloadHandler(
    filename = function() {
      paste("laporan_uji_normalitas_", input$norm_var, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(values$normality_raw_output, values$normality_interpretation_text)
      
      full_content <- paste(
        "HASIL UJI STATISTIK\n\n",
        paste(values$normality_raw_output, collapse = "\n"),
        "\n\n",
        values$normality_interpretation_text
      )
      
      params_for_word <- list(
        main_title = "Laporan Uji Normalitas",
        subtitle = paste("Variabel:", input$norm_var, "- Metode:", input$norm_test),
        main_content = full_content
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_word
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  
  # Homogeneity test
  observeEvent(input$run_homo_test, {
    req(values$transformed_data, input$homo_var, input$homo_group)
    
    df <- values$transformed_data
    df[[input$homo_group]] <- as.factor(df[[input$homo_group]])
    df <- df[complete.cases(df[c(input$homo_var, input$homo_group)]), ]
    
    tryCatch({
      if(n_distinct(df[[input$homo_group]]) < 2) {
        showNotification("Variabel grup harus memiliki setidaknya 2 kelompok.", type = "error")
        return()
      }
      
      test_result <- NULL
      if(input$homo_test == "levene") {
        test_result <- leveneTest(df[[input$homo_var]] ~ df[[input$homo_group]], data = df)
      } else if(input$homo_test == "bartlett") {
        test_result <- bartlett.test(df[[input$homo_var]] ~ df[[input$homo_group]], data = df)
      }
      
      p_val <- if(input$homo_test == "levene") test_result$`Pr(>F)`[1] else test_result$p.value
      stat_val <- if(input$homo_test == "levene") test_result$`F value`[1] else test_result$statistic
      
      conclusion_text <- if(p_val < 0.05) {
        paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa terdapat perbedaan varians yang signifikan (heterogen) untuk '", input$homo_var, "' antar kelompok '", input$homo_group, "'.")
      } else {
        paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak ada bukti perbedaan varians (homogen) untuk '", input$homo_var, "' antar kelompok '", input$homo_group, "'.")
      }
      
      interpretation_text <- paste(
        "Interpretasi Uji Homogenitas Varians untuk", input$homo_var, "berdasarkan kelompok", input$homo_group, ":",
        "\n\nMetode Uji:", ifelse(input$homo_test == "levene", "Levene's Test", "Bartlett's test"),
        "\n\nHipotesis:",
        "\n H0: Varians sama antar kelompok (homogen)",
        "\n H1: Varians tidak sama antar kelompok (heterogen)",
        "\n\nTingkat Signifikansi: 5%",
        "\n\nHasil Uji:",
        "\n Statistik Uji:", round(stat_val, 4),
        "\n P-value:", format.pval(p_val),
        "\n\nKeputusan:",
        if(p_val < 0.05) "\n Karena P-value < 0.05 maka Tolak H0." else "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0.",
        "\n\nKesimpulan:",
        "\n", conclusion_text
      )
      
      # Update UI and reactive values for download
      output$homogeneity_result <- renderPrint({ test_result })
      output$homogeneity_interpretation <- renderText({ interpretation_text })
      values$homogeneity_raw_output <- capture.output(print(test_result))
      values$homogeneity_interpretation_text <- interpretation_text
      
    }, error = function(e) {
      showNotification(paste("Error in homogeneity test:", e$message), type = "error")
    })
  })
  
  output$download_homo_test <- downloadHandler(
    filename = function() {
      paste("laporan_uji_homogenitas_", input$homo_var, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(values$homogeneity_raw_output, values$homogeneity_interpretation_text)
      
      full_content <- paste(
        "HASIL UJI STATISTIK\n\n",
        paste(values$homogeneity_raw_output, collapse = "\n"),
        "\n\n",
        values$homogeneity_interpretation_text
      )
      
      params_for_word <- list(
        main_title = "Laporan Uji Homogenitas",
        subtitle = paste("Variabel:", input$homo_var, "- Grup:", input$homo_group, "- Metode:", input$homo_test),
        main_content = full_content
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_word
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  
  # Normality plot
  output$normality_plot <- renderPlotly({
    req(values$transformed_data, input$norm_var)
    
    var_data <- values$transformed_data[[input$norm_var]]
    var_data <- var_data[!is.na(var_data)]
    
    tryCatch({
      p <- ggplot(data.frame(x = var_data), aes(sample = x)) +
        stat_qq() +
        stat_qq_line(color = "red") +
        theme_minimal() +
        labs(title = paste("Q-Q Plot for", input$norm_var),
             x = "Theoretical Quantiles",
             y = "Sample Quantiles")
      
      ggplotly(p)
    }, error = function(e) {
      showNotification(paste("Error in normality plot:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$download_norm_plot <- downloadHandler(
    filename = function() {
      paste("plot_normalitas_", input$norm_var, "_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(values$transformed_data, input$norm_var)
      var_data <- values$transformed_data[[input$norm_var]]
      var_data <- var_data[!is.na(var_data)]
      
      p <- ggplot(data.frame(x = var_data), aes(sample = x)) +
        stat_qq() +
        stat_qq_line(color = "red") +
        theme_minimal() +
        labs(title = paste("Q-Q Plot for", input$norm_var))
      
      ggsave(file, plot = p, device = "jpeg", width = 8, height = 6)
    }
  )
  
  # STATISTICAL TESTS
  observeEvent(input$run_stat_test, {
    req(values$transformed_data, input$inference_tabs)
    df <- values$transformed_data
    
    tryCatch({
      test_result <- NULL
      interpretation_text <- ""
      
      if (input$inference_tabs == "ttest") {
        req(input$ttest_type)
        if (input$ttest_type == "t_one") {
          req(input$t_one_var, input$t_one_mu)
          var_data <- df[[input$t_one_var]]
          var_data <- var_data[!is.na(var_data)]
          test_result <- t.test(var_data, mu = input$t_one_mu, alternative = input$t_one_tail)
          
          p_val <- test_result$p.value
          conclusion_text <- if(p_val < 0.05) {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa rata-rata populasi '", input$t_one_var, "' secara signifikan berbeda dari ", input$t_one_mu, ".")
          } else {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak ada bukti bahwa rata-rata populasi '", input$t_one_var, "' berbeda dari ", input$t_one_mu, ".")
          }
          interpretation_text <- paste("Interpretasi Uji T 1 Sampel untuk", input$t_one_var, ":",
                                       "\n\nHipotesis:",
                                       if(input$t_one_tail == "two.sided") {
                                         paste("\n H0: Rata-rata populasi (μ) sama dengan", input$t_one_mu,
                                               "\n H1: Rata-rata populasi (μ) tidak sama dengan", input$t_one_mu)
                                       } else if(input$t_one_tail == "less") {
                                         paste("\n H0: Rata-rata populasi (μ) lebih besar dari atau sama dengan", input$t_one_mu,
                                               "\n H1: Rata-rata populasi (μ) lebih kecil dari", input$t_one_mu)
                                       } else {
                                         paste("\n H0: Rata-rata populasi (μ) lebih kecil dari atau sama dengan", input$t_one_mu,
                                               "\n H1: Rata-rata populasi (μ) lebih besar dari", input$t_one_mu)
                                       },
                                       "\n\nTingkat Signifikansi: 5%",
                                       "\n\nHasil Uji:",
                                       "\n Statistik t:", round(test_result$statistic, 4),
                                       "\n Derajat kebebasan:", test_result$parameter,
                                       "\n P-value:", format.pval(test_result$p.value),
                                       "\n Interval kepercayaan 95%:", paste(round(test_result$conf.int[1], 4), "sampai", round(test_result$conf.int[2], 4)),
                                       "\n Rata-rata sampel:", round(mean(var_data), 4),
                                       "\n\nKeputusan:",
                                       if(p_val < 0.05) "\n Karena P-value < 0.05 maka Tolak H0." else "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0.",
                                       "\n\nKesimpulan:",
                                       "\n", conclusion_text)
        } else if (input$ttest_type == "t_two") {
          req(input$t_two_var, input$t_two_group)
          df[[input$t_two_group]] <- as.factor(df[[input$t_two_group]])
          if (nlevels(df[[input$t_two_group]]) < 2) {
            showNotification("Variabel grup harus memiliki setidaknya 2 kelompok.", type="error")
            return()
          }
          formula_str <- paste(input$t_two_var, "~", input$t_two_group)
          test_result <- t.test(as.formula(formula_str), data = df,
                                alternative = input$t_two_tail, var.equal = as.logical(input$t_two_var_equal))
          
          p_val <- test_result$p.value
          conclusion_text <- if(p_val < 0.05) {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa terdapat perbedaan rata-rata yang signifikan untuk '", input$t_two_var, "' antar kelompok.")
          } else {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak terdapat perbedaan rata-rata yang signifikan untuk '", input$t_two_var, "' antar kelompok.")
          }
          interpretation_text <- paste("Interpretasi Uji T 2 Sampel untuk", input$t_two_var, "berdasarkan kelompok", input$t_two_group, ":",
                                       "\n\nHipotesis:",
                                       if(input$t_two_tail == "two.sided") {
                                         paste("\n H0: Tidak ada perbedaan rata-rata antara kedua kelompok (μ1 = μ2)",
                                               "\n H1: Terdapat perbedaan rata-rata antara kedua kelompok (μ1 ≠ μ2)")
                                       } else if(input$t_two_tail == "less") {
                                         paste("\n H0: Rata-rata kelompok 1 lebih besar atau sama dengan kelompok 2 (μ1 ≥ μ2)",
                                               "\n H1: Rata-rata kelompok 1 lebih kecil dari kelompok 2 (μ1 < μ2)")
                                       } else {
                                         paste("\n H0: Rata-rata kelompok 1 lebih kecil atau sama dengan kelompok 2 (μ1 ≤ μ2)",
                                               "\n H1: Rata-rata kelompok 1 lebih besar dari kelompok 2 (μ1 > μ2)")
                                       },
                                       "\n\nTingkat Signifikansi: 5%",
                                       "\n\nHasil Uji:",
                                       "\n Asumsi Varians:", ifelse(as.logical(input$t_two_var_equal), "Sama (Student's t-test)", "Berbeda (Welch's t-test)"),
                                       "\n Statistik t:", round(test_result$statistic, 4),
                                       "\n Derajat kebebasan:", round(test_result$parameter, 4),
                                       "\n P-value:", format.pval(test_result$p.value),
                                       "\n\nKeputusan:",
                                       if(p_val < 0.05) "\n Karena P-value < 0.05 maka Tolak H0." else "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0.",
                                       "\n\nKesimpulan:",
                                       "\n", conclusion_text)
        }
      }
      
      else if (input$inference_tabs == "prop") {
        req(input$prop_var, input$prop_level, input$prop_p0)
        success <- sum(df[[input$prop_var]] == input$prop_level, na.rm = TRUE)
        total <- sum(!is.na(df[[input$prop_var]]))
        test_result <- prop.test(success, total, p = input$prop_p0, alternative = input$prop_tail)
        
        p_val <- test_result$p.value
        conclusion_text <- if(p_val < 0.05) {
          paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa proporsi untuk kategori '", input$prop_level, "' secara signifikan berbeda dari ", input$prop_p0, ".")
        } else {
          paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak ada bukti proporsi untuk kategori '", input$prop_level, "' berbeda dari ", input$prop_p0, ".")
        }
        interpretation_text <- paste("Interpretasi Uji Proporsi untuk", input$prop_var, "(kategori sukses:", input$prop_level, "):",
                                     "\n\nHipotesis:",
                                     if(input$prop_tail == "two.sided") {
                                       paste("\n H0: Proporsi populasi (p) sama dengan", input$prop_p0,
                                             "\n H1: Proporsi populasi (p) tidak sama dengan", input$prop_p0)
                                     } else if(input$prop_tail == "less") {
                                       paste("\n H0: Proporsi populasi (p) lebih besar atau sama dengan", input$prop_p0,
                                             "\n H1: Proporsi populasi (p) lebih kecil dari", input$prop_p0)
                                     } else {
                                       paste("\n H0: Proporsi populasi (p) lebih kecil atau sama dengan", input$prop_p0,
                                             "\n H1: Proporsi populasi (p) lebih besar dari", input$prop_p0)
                                     },
                                     "\n\nHasil Uji:",
                                     "\n Statistik chi-square:", round(test_result$statistic, 4),
                                     "\n Derajat kebebasan:", test_result$parameter,
                                     "\n P-value:", format.pval(test_result$p.value),
                                     "\n Proporsi teramati:", round(success/total, 4),
                                     "\n\nKeputusan:",
                                     if(p_val < 0.05) "\n Karena P-value < 0.05 maka Tolak H0." else "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0.",
                                     "\n\nKesimpulan",
                                     "\n", conclusion_text)
      }
      
      else if (input$inference_tabs == "vartest") {
        req(input$vartest_type)
        if (input$vartest_type == "var_one") {
          req(input$var_one_var, input$var_one_sigma)
          var_data <- df[[input$var_one_var]]
          var_data <- var_data[!is.na(var_data)]
          n <- length(var_data)
          sample_var <- var(var_data)
          chi_stat <- (n - 1) * sample_var / input$var_one_sigma
          p_value <- if(input$var_one_tail == "two.sided") {
            2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
          } else if(input$var_one_tail == "less") {
            pchisq(chi_stat, n-1)
          } else {
            1 - pchisq(chi_stat, n-1)
          }
          test_result <- list(statistic = c("chi-squared" = chi_stat), parameter = c(df = n-1), p.value = p_value,
                              method = "One-sample Chi-squared test for variance", data.name = input$var_one_var)
          
          p_val <- test_result$p.value
          conclusion_text <- if(p_val < 0.05) {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa varians populasi untuk '", input$var_one_var, "' secara signifikan berbeda dari ", input$var_one_sigma, ".")
          } else {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak ada bukti varians populasi untuk '", input$var_one_var, "' berbeda dari ", input$var_one_sigma, ".")
          }
          interpretation_text <- paste("Interpretasi Uji Varians 1 Sampel untuk", input$var_one_var, ":",
                                       "\n\nHipotesis:",
                                       if(input$var_one_tail == "two.sided") {
                                         paste("\n H0: Varians populasi (σ²) sama dengan", input$var_one_sigma,
                                               "\n H1: Varians populasi (σ²) tidak sama dengan", input$var_one_sigma)
                                       } else if(input$var_one_tail == "less") {
                                         paste("\n H0: Varians populasi (σ²) lebih besar atau sama dengan", input$var_one_sigma,
                                               "\n H1: Varians populasi (σ²) lebih kecil dari", input$var_one_sigma)
                                       } else {
                                         paste("\n H0: Varians populasi (σ²) lebih kecil atau sama dengan", input$var_one_sigma,
                                               "\n H1: Varians populasi (σ²) lebih besar dari", input$var_one_sigma)
                                       },
                                       "\n\nTingkat Signifikansi: 5%",
                                       "\n\nHasil Uji:",
                                       "\n Statistik chi-square:", round(test_result$statistic, 4),
                                       "\n Derajat kebebasan:", test_result$parameter,
                                       "\n P-value:", format.pval(test_result$p.value),
                                       "\n Varians sampel:", round(sample_var, 4),
                                       "\n\nKeputusan:",
                                       if(p_val < 0.05) "\n Karena P-value < 0.05 maka Tolak H0." else "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0.",
                                       "\n\nKesimpulan:",
                                       "\n", conclusion_text)
        }
        
        else if (input$vartest_type == "var_two") {
          req(input$var_two_var, input$var_two_group)
          formula_str <- paste(input$var_two_var, "~", input$var_two_group)
          test_result <- var.test(as.formula(formula_str), data = df, alternative = input$var_two_tail)
          
          p_val <- test_result$p.value
          conclusion_text <- if(p_val < 0.05) {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa rasio varians secara signifikan berbeda dari 1, artinya varians kedua kelompok tidak sama.")
          } else {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak ada bukti varians kedua kelompok berbeda.")
          }
          interpretation_text <- paste("Interpretasi Uji Varians 2 Sampel untuk", input$var_two_var, "berdasarkan kelompok", input$var_two_group, ":",
                                       "\n\nHipotesis:",
                                       if(input$var_two_tail == "two.sided") {
                                         paste("\n H0: Rasio varians populasi adalah 1 (σ₁²/σ₂² = 1)",
                                               "\n H1: Rasio varians populasi bukan 1 (σ₁²/σ₂² ≠ 1)")
                                       } else if(input$var_two_tail == "less") {
                                         paste("\n H0: Rasio varians populasi lebih besar atau sama dengan 1 (σ₁²/σ₂² ≥ 1)",
                                               "\n H1: Rasio varians populasi lebih kecil dari 1 (σ₁²/σ₂² < 1)")
                                       } else {
                                         paste("\n H0: Rasio varians populasi lebih kecil atau sama dengan 1 (σ₁²/σ₂² ≤ 1)",
                                               "\n H1: Rasio varians populasi lebih besar dari 1 (σ₁²/σ₂² > 1)")
                                       },
                                       "\n\nTingkat Signifikansi: 5%",
                                       "\n\nHasil Uji:",
                                       "\n Statistik F:", round(test_result$statistic, 4),
                                       "\n Derajat kebebasan:", paste(test_result$parameter[1], "dan", test_result$parameter[2]),
                                       "\n P-value:", format.pval(test_result$p.value),
                                       "\n Rasio varians sampel:", round(test_result$estimate, 4),
                                       "\n\nKeputusan:",
                                       if(p_val < 0.05) "\n Karena P-value < 0.05 maka Tolak H0." else "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0.",
                                       "\n\nKesimpulan:",
                                       "\n", conclusion_text)
        }
      }
      
      else if (input$inference_tabs == "anovatest") {
        req(input$anovatest_type)
        if (input$anovatest_type == "anova_one") {
          req(input$anova_one_dep, input$anova_one_factor)
          formula_str <- paste(input$anova_one_dep, "~", input$anova_one_factor)
          aov_result <- aov(as.formula(formula_str), data = df)
          test_result <- summary(aov_result)
          
          p_val <- test_result[[1]]$`Pr(>F)`[1]
          conclusion_text <- if(p_val < 0.05) {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa terdapat perbedaan rata-rata yang signifikan pada '", input$anova_one_dep, "' di antara setidaknya dua kelompok dari '", input$anova_one_factor, "'.")
          } else {
            paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak ada bukti perbedaan rata-rata yang signifikan pada '", input$anova_one_dep, "' antar kelompok '", input$anova_one_factor, "'.")
          }
          interpretation_text <- paste("Interpretasi ANOVA Satu Arah untuk", input$anova_one_dep, "berdasarkan", input$anova_one_factor, ":",
                                       "\n\nHipotesis:",
                                       "\n H0: Rata-rata semua kelompok sama (μ₁ = μ₂ = ... = μk)",
                                       "\n H1: Setidaknya satu rata-rata kelompok berbeda",
                                       "\n\nTingkat Signifikansi: 5%",
                                       "\n\nHasil Uji:",
                                       "\n Statistik F:", round(test_result[[1]]$`F value`[1], 4),
                                       "\n P-value:", format.pval(p_val),
                                       "\n\nKeputusan:",
                                       if(p_val < 0.05) "\n Karena P-value < 0.05 maka Tolak H0." else "\n Karena P-value ≥ 0.05 maka Gagal Tolak H0.",
                                       "\n\nKesimpulan:",
                                       "\n", conclusion_text)
        }
        
        else if (input$anovatest_type == "anova_two") {
          req(input$anova_two_dep, input$anova_two_factor1, input$anova_two_factor2)
          formula_str <- paste(input$anova_two_dep, "~", input$anova_two_factor1, "*", input$anova_two_factor2)
          aov_result <- aov(as.formula(formula_str), data = df)
          test_result <- anova(aov_result)
          
          p_val_f1 <- test_result$`Pr(>F)`[1]
          p_val_f2 <- test_result$`Pr(>F)`[2]
          p_val_int <- test_result$`Pr(>F)`[3]
          interp_f1 <- if(p_val_f1 < 0.05) "Signifikan (p < 0.05)" else "Tidak Signifikan (p >= 0.05)"
          interp_f2 <- if(p_val_f2 < 0.05) "Signifikan (p < 0.05)" else "Tidak Signifikan (p >= 0.05)"
          interp_int <- if(p_val_int < 0.05) "Signifikan (p < 0.05)" else "Tidak Signifikan (p >= 0.05)"
          conclusion_text <- paste(
            "1. Efek utama faktor '", input$anova_two_factor1, "': ", interp_f1, ".",
            "\n2. Efek utama faktor '", input$anova_two_factor2, "': ", interp_f2, ".",
            "\n3. Efek interaksi '", input$anova_two_factor1, ":", input$anova_two_factor2, "': ", interp_int, ".")
          interpretation_text <- paste("Interpretasi ANOVA Dua Arah untuk", input$anova_two_dep, ":",
                                       "\n\nHipotesis:",
                                       "\n1. Efek Utama", input$anova_two_factor1, ": H0: Tidak ada perbedaan rata-rata antar level faktor 1",
                                       "\n2. Efek Utama", input$anova_two_factor2, ": H0: Tidak ada perbedaan rata-rata antar level faktor 2",
                                       "\n3. Interaksi: H0: Tidak ada efek interaksi antara kedua faktor",
                                       "\n\nHasil Uji:",
                                       "\n P-value untuk Faktor 1:", format.pval(p_val_f1),
                                       "\n P-value untuk Faktor 2:", format.pval(p_val_f2),
                                       "\n P-value untuk Interaksi:", format.pval(p_val_int),
                                       "\n\nKesimpulan:",
                                       "\n", conclusion_text)
        }
      }
      else if (input$inference_tabs == "moran") {
        req(input$var_moran, values$map_data, values$matriks_bobot_raw)
        
        var <- input$var_moran
        sf_data <- values$map_data
        
        is_na <- is.na(sf_data[[var]])
        sf_clean <- sf_data[!is_na, ]
        
        if (nrow(sf_clean) == 0) {
          showNotification("Tidak ada data valid untuk variabel yang dipilih.", type = "error")
          return()
        }
        
        indices_clean <- which(!is_na)
        matriks_bobot_clean <- values$matriks_bobot_raw[indices_clean, indices_clean]
        listw_clean <- mat2listw(matriks_bobot_clean, style = "W")
        
        test_result <- moran.test(sf_clean[[var]], listw_clean, zero.policy = TRUE)
        
        # Menghasilkan dan menampilkan interpretasi Global Moran's I
        global_interpretation <- generate_global_moran_interpretation(test_result, var)
        output$inference_interpretation <- renderText({ global_interpretation })
        
        # Menghasilkan dan menampilkan interpretasi Peta LISA
        lisa_interpretation_text <- generate_lisa_map_interpretation()
        output$lisa_interpretation_inferensia <- renderText({ lisa_interpretation_text })
        
        # Menampilkan peta LISA
        output$lisa_map <- renderLeaflet({
          req(sf_clean, listw_clean)
          create_lisa_map(sf_clean, var, listw_clean)
        })
        
        # Setel output utama dan simpan untuk diunduh
        output$stat_test_result <- renderPrint({ test_result })
        values$stat_test_output <- capture.output(print(test_result))
        values$stat_interpretation_output <- paste(global_interpretation, lisa_interpretation_text, sep = "\n\n")
        
        values$moran_interpretation_text <- "" 
        
        return()
      }
      
      output$stat_test_result <- renderPrint({ test_result })
      output$inference_interpretation <- renderText({ interpretation_text })
      
      values$stat_test_output <- capture.output(print(test_result))
      values$stat_interpretation_output <- interpretation_text
      
    }, error = function(e) {
      showNotification(paste("Error in statistical test:", e$message), type = "error")
      output$stat_test_result <- renderPrint({ cat("Error:", e$message) })
      output$inference_interpretation <- renderText({ paste("Error in statistical test:", e$message) })
    })
  })
  
  output$download_stat_test <- downloadHandler(
    filename = function() {
      test_name <- req(input$inference_tabs)
      paste("laporan_uji_", test_name, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(values$stat_test_output, values$stat_interpretation_output)
      
      full_content <- paste(
        "HASIL UJI STATISTIK\n\n",
        paste(values$stat_test_output, collapse = "\n"),
        "\n\n",
        values$stat_interpretation_output
      )
      
      params_for_word <- list(
        main_title = "Laporan Hasil Uji Statistik Inferensia",
        subtitle = paste("Uji yang dilakukan:", input$inference_tabs),
        main_content = full_content
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_word
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  
  create_lisa_map <- function(sf_data, var_name, listw) {
    local_m <- localmoran(sf_data[[var_name]], listw, zero.policy = TRUE)
    
    sf_data$cluster <- "Not Significant"
    if (!var_name %in% names(sf_data) || !is.numeric(sf_data[[var_name]])) {
      stop("Nama variabel tidak valid atau bukan numerik.")
    }
    scaled_var <- scale(sf_data[[var_name]])[,1]
    lagged_var <- lag.listw(listw, scaled_var, zero.policy = TRUE)
    
    p_value <- local_m[, 5] 
    significant <- p_value < 0.05
    
    sf_data$cluster[significant & scaled_var > 0 & lagged_var > 0] <- "High-High"
    sf_data$cluster[significant & scaled_var < 0 & lagged_var < 0] <- "Low-Low"
    sf_data$cluster[significant & scaled_var > 0 & lagged_var < 0] <- "High-Low"
    sf_data$cluster[significant & scaled_var < 0 & lagged_var > 0] <- "Low-High"
    
    pal <- colorFactor(c("red", "blue", "pink", "skyblue", "grey50"), 
                       domain = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant"))
    
    popup_text <- paste(
      "<strong>Wilayah:</strong>", sf_data$nmkab, "<br/>",
      "<strong>Tipe Klaster:</strong>", sf_data$cluster, "<br/>",
      "<strong>Nilai:</strong>", round(sf_data[[var_name]], 2)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(sf_data) %>%
      addProviderTiles("CartoDB.Positron", group = "Peta Terang") %>%
      addProviderTiles("OpenStreetMap", group = "Peta Detail") %>%
      setView(lng = 118, lat = -2, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(cluster),
        weight = 1.5,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          bringToFront = TRUE),
        label = popup_text,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~cluster, opacity = 0.7, title = "LISA Cluster",
                position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("Peta Terang", "Peta Detail"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  generate_global_moran_interpretation <- function(test_result, variable_name) {
    p_val <- test_result$p.value
    moran_stat <- test_result$estimate[1]
    
    decision_text <- if (p_val < 0.05) "Karena P-value < 0.05, maka Tolak H0." else "Karena P-value >= 0.05, maka Gagal Tolak H0."
    
    conclusion_text <- if (p_val < 0.05) {
      if (moran_stat > 0) {
        paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa terdapat autokorelasi spasial positif (pola mengelompok) pada variabel '", variable_name, "'. Wilayah dengan nilai tinggi cenderung berdekatan dengan wilayah bernilai tinggi lainnya, dan sebaliknya.")
      } else {
        paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa terdapat autokorelasi spasial negatif (pola menyebar) pada variabel '", variable_name, "'. Wilayah dengan nilai tinggi cenderung berdekatan dengan wilayah bernilai rendah.")
      }
    } else {
      paste0("Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa tidak terdapat bukti autokorelasi spasial yang signifikan pada variabel '", variable_name, "'. Pola persebaran data bersifat acak.")
    }
    
    paste(
      "INTERPRETASI UJI GLOBAL MORAN'S I UNTUK:", variable_name,
      "\n",
      "\n\nHipotesis:",
      "\n H0: Tidak ada autokorelasi spasial (pola acak).",
      "\n H1: Terdapat autokorelasi spasial (pola mengelompok atau menyebar).",
      "\n\nTingkat Signifikansi: 5%",
      "\n\nHasil Uji:",
      "\n Statistik Moran's I :", round(moran_stat, 4),
      "\n P-value             :", format.pval(p_val),
      "\n\nKeputusan:",
      "\n ", decision_text,
      "\n\nKesimpulan:",
      "\n ", conclusion_text
    )
  }
  
  # Fungsi 2: Untuk Interpretasi Peta Klaster LISA
  generate_lisa_map_interpretation <- function() {
    paste(
      "INTERPRETASI PETA KLASTER LISA (Local Moran's I):",
      "\nPeta Klaster LISA menampilkan pola pengelompokan spasial, mengidentifikasi lokasi-lokasi yang memiliki nilai serupa (atau berbeda) dengan tetangganya. Berikut adalah definisi setiap klaster:",
      "\n\nHigh-High (Merah):", 
      "\nWilayah dengan nilai variabel tinggi dan dikelilingi oleh wilayah tetangga yang juga bernilai tinggi. Ini menunjukkan 'hotspot' atau area konsentrasi tinggi.",
      "\n\nLow-Low (Biru):",
      "\nWilayah dengan nilai variabel rendah dan dikelilingi oleh wilayah tetangga yang juga bernilai rendah. Ini menunjukkan 'coldspot' atau area konsentrasi rendah.",
      "\n\nHigh-Low (Pink):",
      "\nWilayah dengan nilai variabel tinggi tetapi dikelilingi oleh wilayah tetangga yang bernilai rendah. Ini menunjukkan anomali spasial (outlier) di mana nilai tinggi berada di tengah-tengah lingkungan bernilai rendah.",
      "\n\nLow-High (Biru Muda/Skyblue):",
      "\nWilayah dengan nilai variabel rendah tetapi dikelilingi oleh wilayah tetangga yang bernilai tinggi. Ini menunjukkan anomali spasial (outlier) di mana nilai rendah berada di tengah-tengah lingkungan bernilai tinggi.",
      "\n\nNot Significant (Abu-abu):",
      "\nWilayah yang tidak menunjukkan pola pengelompokan spasial yang signifikan secara statistik. Pola persebaran nilai di area ini cenderung acak."
    )
  }
  
  # REGRESSION ANALYSIS
  observeEvent(input$run_regression, {
    req(values$transformed_data, input$reg_y, input$reg_x)
    
    # *** BARU: Kosongkan riwayat uji asumsi setiap kali model baru dijalankan
    values$assumption_history <- list()
    # Kosongkan juga tampilan di UI
    output$regression_assumptions <- renderPrint({ cat("Jalankan uji asumsi untuk melihat hasil.") })
    
    df <- values$transformed_data
    df <- df[complete.cases(df[c(input$reg_y, input$reg_x)]), ]
    
    tryCatch({
      formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "))
      model <- lm(as.formula(formula_str), data = df, na.action = na.exclude)
      summary_model <- summary(model)
      
      r_squared <- round(summary_model$r.squared, 4)
      adj_r_squared <- round(summary_model$adj.r.squared, 4)
      f_stat <- round(summary_model$fstatistic[1], 4)
      p_val_f <- format.pval(pf(summary_model$fstatistic[1], summary_model$fstatistic[2],
                                summary_model$fstatistic[3], lower.tail = FALSE))
      
      coef_summary_text <- capture.output(print(summary_model$coefficients))
      
      interpretation_text <- paste(
        "Interpretasi Regresi Linier untuk ", input$reg_y, ":",
        "\n\nHasil Model:",
        "\n- R-squared:", r_squared, "- Proporsi varians", input$reg_y, "yang dijelaskan oleh model.",
        "\n- Adjusted R-squared:", adj_r_squared, "- R-squared disesuaikan dengan jumlah prediktor.",
        "\n- Statistik F:", f_stat, ", P-value:", p_val_f,
        "\n- Jumlah Observasi:", nobs(model),
        "\n\nInterpretasi:",
        "\n• R-squared menunjukkan", round(r_squared * 100, 2), "% variasi", input$reg_y, "dijelaskan oleh model.",
        "\n• P-value F < 0.05 menunjukkan model secara keseluruhan signifikan secara statistik.",
        "\n• Untuk koefisien individual, periksa kolom 'Pr(>|t|)' pada ringkasan di atas. P-value < 0.05 menunjukkan prediktor tersebut signifikan.",
        "\n\nRekomendasi:",
        "\n• Lanjutkan ke tab 'Uji Asumsi Regresi' untuk memvalidasi model Anda."
      )
      
      values$regression_model <- model
      output$regression_summary <- renderPrint({ summary_model })
      output$regression_interpretation <- renderText({ interpretation_text })
      
      values$reg_summary_output <- capture.output(print(summary_model))
      values$reg_interpretation_output <- interpretation_text
      
    }, error = function(e) {
      showNotification(paste("Error in regression:", e$message), type = "error")
    })
  })
  
  output$download_reg_summary <- downloadHandler(
    filename = function() {
      paste("laporan_regresi_linier_", input$reg_y, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(values$reg_summary_output, values$reg_interpretation_output)
      
      full_content <- paste(
        "HASIL RINGKASAN MODEL\n\n",
        paste(values$reg_summary_output, collapse = "\n"),
        "\n\n",
        values$reg_interpretation_output
      )
      
      params_for_word <- list(
        main_title = "Laporan Hasil Analisis Regresi Linier",
        subtitle = paste("Variabel Dependen (Y):", input$reg_y),
        main_content = full_content
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_word
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  
  # Regression diagnostic plots
  output$regression_plots <- renderPlot({
    req(values$regression_model)
    
    tryCatch({
      par(mfrow = c(2, 2))
      plot(values$regression_model)
      par(mfrow = c(1, 1))
    }, error = function(e) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "Gagal Membuat Plot", 
           xlab = "", ylab = "", axes = FALSE)
      text(0.5, 0.5, "Terjadi kesalahan saat membuat plot diagnostik.", col = "red")
    })
  })
  
  output$download_diag_plots <- downloadHandler(
    filename = function() {
      paste("plot_diagnostik_regresi_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(values$regression_model)
      jpeg(file, width = 800, height = 800, res = 100)
      tryCatch({
        par(mfrow = c(2, 2))
        plot(values$regression_model)
      }, finally = {
        par(mfrow = c(1, 1))
        dev.off()
      })
    }
  )
  
  # REGRESSION ASSUMPTION TESTS
  observeEvent(input$test_linearity, {
    req(values$regression_model)
    tryCatch({
      interpretation <- ""
      raw_output <- ""
      if (length(input$reg_x) < 2) {
        interpretation <- "Uji Multikolinearitas (VIF) memerlukan setidaknya 2 variabel independen."
        raw_output <- interpretation
      } else {
        vif_result <- vif(values$regression_model)
        raw_output <- capture.output(print(vif_result))
        interpretation <- paste(
          "Interpretasi Uji Multikolinearitas (VIF):",
          "\n\nWilayah Kritis: VIF > 10 (indikasi multikolinearitas kuat).",
          "\n\nHasil Uji:",
          "\nNilai VIF untuk setiap variabel prediktor ditampilkan di atas.",
          "\n\nKesimpulan:",
          if (any(vif_result > 10)) {
            "\nTerdapat setidaknya satu variabel dengan VIF > 10, menunjukkan adanya masalah multikolinearitas yang kuat dalam model."
          } else {
            "\nTidak ada variabel dengan VIF > 10, menunjukkan tidak ada masalah multikolinearitas yang kuat dalam model."
          }
        )
      }
      # Tampilkan hasil terbaru di UI
      output$regression_assumptions <- renderPrint({ cat(paste(raw_output, collapse = "\n"), "\n\n", interpretation) })
      # Simpan ke riwayat
      values$assumption_history$multikolinearitas <- list(
        title = "Uji Multikolinearitas (VIF)",
        raw = raw_output,
        interp = interpretation
      )
    }, error = function(e) { showNotification(paste("Error in VIF test:", e$message), type = "error") })
  })
  
  observeEvent(input$test_independence, {
    req(values$regression_model)
    tryCatch({
      dw_result <- dwtest(values$regression_model)
      raw_output <- capture.output(print(dw_result))
      p_val <- dw_result$p.value
      stat_val <- dw_result$statistic
      keputusan <- if(p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
      kesimpulan <- if(p_val < 0.05) {
        if(stat_val < 2) "terdapat autokorelasi positif pada residual." else "terdapat autokorelasi negatif pada residual."
      } else {
        "tidak terdapat bukti adanya autokorelasi pada residual (asumsi terpenuhi)."
      }
      interpretation <- paste(
        "Interpretasi Uji Autokorelasi (Durbin-Watson):",
        "\n\nHipotesis:",
        "\n H0: Tidak ada autokorelasi (residual independen).",
        "\n H1: Terdapat autokorelasi (residual tidak independen).",
        "\n\nTingkat Signifikansi: 5%",
        "\n\nHasil Uji:",
        "\n Statistik DW:", round(stat_val, 4),
        "\n P-value:", format.pval(p_val),
        "\n\nKeputusan:",
        paste("\n Karena P-value", ifelse(p_val < 0.05, "<", "≥"), "0.05, maka", keputusan, "."),
        "\n\nKesimpulan:",
        paste("\n Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa", kesimpulan)
      )
      # Tampilkan hasil terbaru di UI
      output$regression_assumptions <- renderPrint({ cat(paste(raw_output, collapse = "\n"), "\n\n", interpretation) })
      # Simpan ke riwayat
      values$assumption_history$autokorelasi <- list(
        title = "Uji Autokorelasi (Durbin-Watson)",
        raw = raw_output,
        interp = interpretation
      )
    }, error = function(e) { showNotification(paste("Error in Durbin-Watson test:", e$message), type = "error") })
  })
  
  observeEvent(input$test_homoscedasticity_bp, {
    req(values$regression_model)
    tryCatch({
      bp_result <- bptest(values$regression_model)
      raw_output <- capture.output(print(bp_result))
      p_val <- bp_result$p.value
      stat_val <- bp_result$statistic
      keputusan <- if(p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
      kesimpulan <- if(p_val < 0.05) {
        "terdapat bukti adanya heteroskedastisitas (varians residual tidak konstan)."
      } else {
        "tidak terdapat bukti adanya heteroskedastisitas (asumsi homoskedastisitas terpenuhi)."
      }
      interpretation <- paste(
        "Interpretasi Uji Homoskedastisitas (Breusch-Pagan):",
        "\n\nHipotesis:",
        "\n H0: Varians residual konstan (homoskedastisitas).",
        "\n H1: Varians residual tidak konstan (heteroskedastisitas).",
        "\n\nTingkat Signifikansi: 5%",
        "\n\nHasil Uji:",
        "\n Statistik BP:", round(stat_val, 4),
        "\n P-value:", format.pval(p_val),
        "\n\nKeputusan:",
        paste("\n Karena P-value", ifelse(p_val < 0.05, "<", "≥"), "0.05, maka", keputusan, "."),
        "\n\nKesimpulan:",
        paste("\n Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa", kesimpulan)
      )
      # Tampilkan hasil terbaru di UI
      output$regression_assumptions <- renderPrint({ cat(paste(raw_output, collapse = "\n"), "\n\n", interpretation) })
      # Simpan ke riwayat
      values$assumption_history$homoskedastisitas <- list(
        title = "Uji Homoskedastisitas (Breusch-Pagan)",
        raw = raw_output,
        interp = interpretation
      )
    }, error = function(e) { showNotification(paste("Error in Breusch-Pagan test:", e$message), type = "error") })
  })
  
  observeEvent(input$test_normality_resid, {
    req(values$regression_model)
    tryCatch({
      resid <- residuals(values$regression_model)
      raw_output <- ""
      interpretation <- ""
      if (length(resid) > 5000 || length(resid) < 3) {
        raw_output <- "Ukuran sampel di luar rentang (3-5000) untuk Uji Shapiro-Wilk."
        interpretation <- "Gunakan plot Q-Q pada tab 'Model Regresi' untuk evaluasi visual."
      } else {
        shapiro_result <- shapiro.test(resid)
        raw_output <- capture.output(print(shapiro_result))
        p_val <- shapiro_result$p.value
        stat_val <- shapiro_result$statistic
        keputusan <- if(p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
        kesimpulan <- if(p_val < 0.05) {
          "residual model tidak berdistribusi normal (asumsi dilanggar)."
        } else {
          "residual model berdistribusi normal (asumsi terpenuhi)."
        }
        interpretation <- paste(
          "Interpretasi Uji Normalitas Residual (Shapiro-Wilk):",
          "\n\nHipotesis:",
          "\n H0: Residual berdistribusi normal.",
          "\n H1: Residual tidak berdistribusi normal.",
          "\n\nTingkat Signifikansi: 5%",
          "\n\nHasil Uji:",
          "\n Statistik W:", round(stat_val, 4),
          "\n P-value:", format.pval(p_val),
          "\n\nKeputusan:",
          paste("\n Karena P-value", ifelse(p_val < 0.05, "<", "≥"), "0.05, maka", keputusan, "."),
          "\n\nKesimpulan:",
          paste("\n Dengan tingkat signifikansi 5%, dapat disimpulkan bahwa", kesimpulan)
        )
      }
      # Tampilkan hasil terbaru di UI
      output$regression_assumptions <- renderPrint({ cat(paste(raw_output, collapse = "\n"), "\n\n", interpretation) })
      # Simpan ke riwayat
      values$assumption_history$normalitas_residual <- list(
        title = "Uji Normalitas Residual (Shapiro-Wilk)",
        raw = raw_output,
        interp = interpretation
      )
    }, error = function(e) { showNotification(paste("Error in residual normality test:", e$message), type = "error") })
  })
  
  observeEvent(input$show_correlation, {
    req(values$regression_model, input$reg_y, input$reg_x)
    tryCatch({
      df <- values$transformed_data[c(input$reg_y, input$reg_x)]
      cor_matrix <- cor(df, use = "complete.obs")
      output$correlation_matrix <- DT::renderDataTable({ DT::datatable(round(cor_matrix, 4), options = list(pageLength = 10)) })
      output$correlation_heatmap <- renderPlotly({ p <- ggcorr(df, label = TRUE, label_round = 2); ggplotly(p) })
    }, error = function(e) { showNotification(paste("Error in correlation matrix:", e$message), type = "error") })
  })
  
  output$download_assumptions <- downloadHandler(
    filename = function() {
      paste("laporan_uji_asumsi_regresi_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # *** BARU: Logika unduh dinamis berdasarkan riwayat
      req(length(values$assumption_history) > 0) # Pastikan ada riwayat untuk diunduh
      
      # Gabungkan semua hasil dari riwayat
      full_content <- ""
      for(test_key in names(values$assumption_history)) {
        test_data <- values$assumption_history[[test_key]]
        
        section_content <- paste(
          "================================================",
          paste0("## ", test_data$title),
          "HASIL UJI STATISTIK",
          paste(test_data$raw, collapse = "\n"),
          "\n", # Spasi ekstra
          test_data$interp,
          sep = "\n\n"
        )
        full_content <- paste(full_content, section_content, sep="\n\n")
      }
      
      params_for_word <- list(
        main_title = "Laporan Hasil Uji Asumsi Regresi",
        subtitle = "Laporan ini berisi seluruh uji asumsi yang telah dijalankan untuk model regresi saat ini.",
        main_content = full_content
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_word
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
  
  # MORAN'S I RESIDUAL
  observeEvent(input$run_residual_moran, {
    req(values$regression_model, values$map_data, values$matriks_bobot_raw)
    
    tryCatch({ # Menambahkan tryCatch untuk penanganan error yang lebih baik
      model <- values$regression_model
      sf_data <- values$map_data
      residuals_full <- rep(NA, nrow(sf_data))
      model_rows <- as.integer(names(residuals(model)))
      
      # Pastikan model_rows berada dalam rentang yang valid untuk sf_data
      if (max(model_rows) > nrow(sf_data)) {
        showNotification("Error: Indeks residual model tidak sesuai dengan data spasial.", type = "error")
        return()
      }
      
      residuals_full[model_rows] <- residuals(model)
      sf_data$residuals <- residuals_full
      
      is_na_res <- is.na(sf_data$residuals)
      sf_clean_res <- sf_data[!is_na_res, ]
      
      if (nrow(sf_clean_res) == 0) {
        showNotification("Gagal menyelaraskan residual atau tidak ada data valid.", type = "error")
        return()
      }
      
      indices_res_clean <- which(!is_na_res)
      matriks_res_clean <- values$matriks_bobot_raw[indices_res_clean, indices_res_clean]
      listw_res_clean <- mat2listw(matriks_res_clean, style = "W")
      
      moran_res_result <- moran.test(sf_clean_res$residuals, listw_res_clean, zero.policy = TRUE)
      
      values$moran_residual_raw_output <- capture.output(print(moran_res_result))
      
      values$moran_residual_interpretation_text <- generate_global_moran_interpretation(moran_res_result, "Residual Model")
      
      lisa_interpretation_text <- generate_lisa_map_interpretation()
      
      # Memperbarui semua output yang relevan
      output$moran_residual_output <- renderPrint({ moran_res_result })
      output$moran_residual_interpretation_output <- renderText({ values$moran_residual_interpretation_text })
      output$lisa_residual_map <- renderLeaflet({ create_lisa_map(sf_clean_res, "residuals", listw_res_clean) })
      output$lisa_interpretation <- renderText({ lisa_interpretation_text }) # Mengisi output interpretasi LISA
      
    }, error = function(e) { # Menangkap error jika ada
      showNotification(paste("Terjadi error saat analisis Moran's I residual:", e$message), type = "error", duration = 10)
    })
  })
  
  output$download_moran_residual_interpretation <- downloadHandler(
    filename = function() {
      paste("laporan_moran_residual_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(values$moran_residual_raw_output, values$moran_residual_interpretation_text)
      
      full_content <- paste(
        "HASIL UJI STATISTIK\n\n",
        paste(values$moran_residual_raw_output, collapse = "\n"),
        "\n\n",
        values$moran_residual_interpretation_text
      )
      
      params_for_word <- list(
        main_title = "Laporan Uji Autokorelasi Spasial pada Residual",
        subtitle = "",
        main_content = full_content
      )
      
      generate_word_report(
        output_file = file,
        template_path = "template_laporan.Rmd",
        params_list = params_for_word
      )
    },
    contentType = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  )
}

# Run the application
shinyApp(ui = ui, server = server)