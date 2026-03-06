#' 运行 DeepSeek 文献综合分析与翻译系统
#'
#' @description 启动 sciwxzs 包的交互式 Shiny 应用程序。
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinycssloaders
#' @import shinyjs
#' @import DT
#' @import ggplot2
#' @import wordcloud2
#' @import colourpicker
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot2 webshot
#' @importFrom dplyr mutate filter select summarise group_by ungroup arrange pull n row_number case_when any_of
#' @importFrom tidyr complete unnest
#' @importFrom purrr map_int map_chr
#' @importFrom lubridate year
#' @importFrom scales percent_format
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#'
#' @export
run_sciwxzs <- function() {

  # ==========================================
  # 1. UI 定义
  # ==========================================
  # UI定义
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DeepSeek文献综合分析与翻译系统"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("数据上传与筛选", tabName = "search", icon = icon("search")),
      menuItem("摘要翻译", tabName = "translate", icon = icon("language")),
      menuItem("API配置", tabName = "api", icon = icon("key")),
      menuItem("分词处理", tabName = "segment", icon = icon("cut")),
      menuItem("词频分析", tabName = "wordfreq", icon = icon("chart-bar")),
      menuItem("时间趋势", tabName = "trends", icon = icon("line-chart")),
      menuItem("热力图", tabName = "heatmap", icon = icon("th")),
      menuItem("气泡图", tabName = "bubble", icon = icon("circle")),
      menuItem("词云图", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("文献综述", tabName = "review", icon = icon("file-text"))
    ),
    
    # 全局设置面板
    div(
      style = "padding: 15px;",
      h4("全局设置"),
      numericInput("max_tokens", "Max Tokens:", 1000, min = 100, max = 4000),
      numericInput("delay_seconds", "API延迟(秒):", 0.5, min = 0.1, max = 5, step = 0.1),
      numericInput("top_n_words", "显示词数:", 100, min = 10, max = 500),
      checkboxInput("test_mode", "测试模式(仅前10条)", FALSE),
      actionButton("reset_all", "重置所有", class = "btn-warning", style = "width: 100%; margin-top: 10px;")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #3c8dbc; }
        .progress-bar { background-color: #3c8dbc; }
        .info-box { min-height: 90px; }
        .shiny-notification { position: fixed; top: 60px; right: 10px; }
        .custom-select { width: 100%; margin-bottom: 5px; }
        .selectize-control .item .remove {
          color: #d9534f !important;
          font-weight: bold !important;
          font-size: 14px !important;
          padding: 0 3px !important;
          cursor: pointer !important;
          opacity: 1 !important;
          display: inline-block !important;
        }
        .selectize-control .item .remove:hover {
          color: #c9302c !important;
          background-color: rgba(217, 83, 79, 0.1) !important;
          border-radius: 2px !important;
        }
      "))
    ),
    
    tabItems(
      # ==================== 1. 数据上传与检索页面 ====================
      tabItem(tabName = "search",
              fluidRow(
                box(
                  title = "数据上传", status = "primary", solidHeader = TRUE, width = 4,
                  fileInput("data_file", "选择Excel文件", 
                            accept = c(".xlsx", ".xls"),
                            placeholder = "支持 .xlsx 或 .xls 格式"),
                  helpText("文件应包含: TI (标题), AB (英文摘要), PY (年份), DE (关键词)"),
                  checkboxInput("use_demo", "使用示例数据", FALSE),
                  actionButton("load_data", "加载数据", class = "btn-primary", style = "width: 100%;"),
                  hr(),
                  h4("数据统计"),
                  valueBoxOutput("total_docs_box", width = 12),
                  valueBoxOutput("year_range_box", width = 12),
                  valueBoxOutput("valid_docs_box", width = 12)
                ),
                box(
                  title = "关键词检索", status = "warning", solidHeader = TRUE, width = 4,
                  textAreaInput(
                    inputId = "keywords",
                    label = "输入检索关键词（多个关键词用逗号分隔）",
                    placeholder = "例如：Tibetan Plateau,青藏高原,Tibet",
                    rows = 5
                  ),
                  radioButtons(
                    inputId = "filter_logic",
                    label = "筛选逻辑",
                    choices = list(
                      "保留包含关键词的文献" = "include",
                      "排除包含关键词的文献" = "exclude"
                    ),
                    selected = "include"
                  ),
                  actionButton("filter_btn", "执行筛选", class = "btn-success", style = "width: 100%;"),
                  hr(),
                  uiOutput("action_buttons_ui")
                ),
                box(
                  title = "数据预览", status = "info", solidHeader = TRUE, width = 4,
                  withSpinner(DTOutput("data_preview"), type = 4)
                )
              ),
              fluidRow(
                box(
                  title = "筛选结果", status = "success", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("筛选结果概览", 
                             verbatimTextOutput("summary_info")),
                    tabPanel("筛选后数据", 
                             withSpinner(DTOutput("filtered_table"), type = 4)),
                    tabPanel("原始数据", 
                             withSpinner(DTOutput("original_table"), type = 4))
                  )
                )
              )
      ),
      
      # ==================== 2. 摘要翻译页面 ====================
      tabItem(tabName = "translate",
              fluidRow(
                box(
                  title = "翻译设置", status = "primary", solidHeader = TRUE, width = 4,
                  radioButtons(
                    inputId = "data_source",
                    label = "数据来源",
                    choices = list(
                      "上传Excel文件" = "upload",
                      "检索模块结果" = "search_result"
                    ),
                    selected = "upload"
                  ),
                  conditionalPanel(
                    condition = "input.data_source == 'upload'",
                    fileInput(
                      inputId = "file_translate",
                      label = "上传Excel文件（包含AB列）",
                      accept = c(".xlsx"),
                      multiple = FALSE
                    ),
                    tags$small("文件必须包含名为'AB'的列（存放英文摘要）")
                  ),
                  conditionalPanel(
                    condition = "input.data_source == 'search_result'",
                    wellPanel(
                      style = "background-color: #f8f9fa;",
                      h5(icon("info-circle"), " 检索结果数据"),
                      textOutput("search_data_info")
                    )
                  ),
                  hr(),
                  passwordInput("api_key", "DeepSeek API密钥", placeholder = "输入sk-开头的密钥"),
                  tags$small("密钥仅本次会话有效，不会存储"),
                  hr(),
                  numericInput(
                    inputId = "max_tokens_trans",
                    label = "最大Token数",
                    value = 2000,
                    min = 500,
                    max = 8192,
                    step = 500
                  ),
                  numericInput(
                    inputId = "delay_seconds_trans",
                    label = "请求间隔(秒)",
                    value = 1,
                    min = 0.1,
                    max = 10,
                    step = 0.1
                  ),
                  actionButton("run_translate", "开始翻译", class = "btn-primary", style = "width: 100%;"),
                  br(), br(),
                  actionButton("cancel_translate", "取消翻译", class = "btn-danger", style = "width: 100%;"),
                  hr(),
                  uiOutput("download_trans_btn")
                ),
                box(
                  title = "翻译进度与结果", status = "info", solidHeader = TRUE, width = 8,
                  tabsetPanel(
                    tabPanel("翻译进度",
                             h4("翻译状态"),
                             verbatimTextOutput("trans_status"),
                             uiOutput("progress_script_ui"),  # <--- 在这里添加这一行
                             tags$hr(),
                             h4("进度展示"),
                             tags$div(
                               class = "progress",
                               style = "height: 20px; background-color: #f5f5f5; border-radius: 4px;",
                               tags$div(
                                 id = "progress_bar",
                                 class = "progress-bar",
                                 style = "width: 0%; background-color: #337ab7; text-align: right; padding-right: 5px; color: white;",
                                 "0%"
                               )
                             ),
                             tags$hr(),
                             h4("翻译结果预览"),
                             tableOutput("preview_table")
                    ),
                    tabPanel("结果预览", 
                             tableOutput("preview_table_full")),
                    tabPanel("翻译统计", 
                             verbatimTextOutput("trans_stats"))
                  )
                )
              )
      ),
      
      # ==================== 3. API配置页面 ====================
      tabItem(tabName = "api",
              fluidRow(
                box(
                  title = "DeepSeek API配置", status = "warning", solidHeader = TRUE, width = 6,
                  passwordInput("api_key_config", "API密钥:", value = ""),
                  helpText("请输入您的DeepSeek API密钥。密钥仅在当前会话中保存。"),
                  hr(),
                  selectInput("model", "选择模型:", 
                              choices = c("deepseek-chat", "deepseek-coder"),
                              selected = "deepseek-chat"),
                  numericInput("timeout", "请求超时(秒):", 30, min = 10, max = 120),
                  actionButton("test_api", "测试连接", class = "btn-success"),
                  hr(),
                  h4("API状态"),
                  verbatimTextOutput("api_status")
                ),
                box(
                  title = "使用说明", status = "info", solidHeader = TRUE, width = 6,
                  HTML("
        <h4>分词参数说明</h4>
        <ul>
          <li><b>Max Tokens:</b> 控制API返回的最大token数</li>
          <li><b>API延迟:</b> 每次请求之间的间隔，避免触发频率限制</li>
          <li><b>测试模式:</b> 仅处理前10条记录，用于快速测试</li>
        </ul>
        <h4>数据要求</h4>
        <ul>
          <li>ABCN列必须包含中文文本（用于分词分析）</li>
          <li>AB列必须包含英文摘要（用于翻译）</li>
          <li>PY列必须为有效年份(1900-当前)</li>
        </ul>
      ")
                )
              )
      ),
      
      # ==================== 4. 分词处理页面 ====================
      tabItem(tabName = "segment",
              fluidRow(
                box(
                  title = "分词控制", status = "primary", solidHeader = TRUE, width = 4,
                  actionButton("start_segment", "开始分词", class = "btn-lg btn-success", 
                               style = "width: 100%; margin-bottom: 10px;"),
                  actionButton("stop_segment", "停止", class = "btn-danger", 
                               style = "width: 100%;"),
                  hr(),
                  progressBar("segment_progress", value = 0, total = 100, 
                              status = "info", display_pct = TRUE),
                  hr(),
                  h4("实时统计"),
                  verbatimTextOutput("segment_stats"),
                  hr(),
                  h4("数据导出"),
                  downloadButton("download_segment_data", "下载分词结果(Excel)", 
                                 class = "btn-info", style = "width: 100%;")
                ),
                box(
                  title = "处理日志", status = "info", solidHeader = TRUE, width = 8,
                  withSpinner(verbatimTextOutput("segment_log"), type = 4),
                  height = 500
                )
              ),
              fluidRow(
                box(
                  title = "分词结果预览", status = "success", solidHeader = TRUE, width = 12,
                  withSpinner(DTOutput("segment_preview"), type = 4)
                )
              )
      ),
      
      # ==================== 5. 词频分析页面 ====================
      tabItem(tabName = "wordfreq",
              fluidRow(
                box(
                  title = "词频统计设置", status = "primary", solidHeader = TRUE, width = 3,
                  numericInput("min_freq", "最小词频:", 2, min = 1),
                  numericInput("min_word_length", "最小词长:", 2, min = 1),
                  selectInput("stopwords_lang", "停用词库:", 
                              choices = c("中文", "英文", "无"),
                              selected = "中文"),
                  actionButton("refresh_freq", "刷新统计", class = "btn-primary"),
                  hr(),
                  h4("数据导出"),
                  downloadButton("download_freq_data", "下载高频词汇表(Excel)", 
                                 class = "btn-info", style = "width: 100%; margin-bottom: 5px;"),
                  tags$div(class = "custom-select",
                           selectInput("freq_plot_format", "词频图导出格式:", 
                                       choices = c("PDF" = "pdf", "JPG" = "jpg"),
                                       selected = "pdf")
                  ),
                  downloadButton("download_freq_plot", "下载词频分布图", 
                                 class = "btn-info", style = "width: 100%;")
                ),
                box(
                  title = "词频分布", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(plotOutput("freq_plot", height = 400), type = 4)
                )
              ),
              fluidRow(
                box(
                  title = "高频词汇表", status = "success", solidHeader = TRUE, width = 12,
                  withSpinner(DTOutput("freq_table"), type = 4)
                )
              )
      ),
      
      # ==================== 6. 时间趋势页面 ====================
      tabItem(tabName = "trends",
              fluidRow(
                box(
                  title = "趋势分析设置", status = "primary", solidHeader = TRUE, width = 3,
                  selectizeInput("trend_keywords", "选择关键词:", 
                                 choices = NULL, 
                                 multiple = TRUE,
                                 options = list(
                                   maxItems = 10,
                                   create = FALSE,
                                   persist = TRUE,
                                   openOnFocus = TRUE,
                                   placeholder = '输入关键词快速搜索...',
                                   plugins = list("remove_button")
                                 )),
                  sliderInput("smooth_span", "平滑系数:", 0.3, min = 0.1, max = 1),
                  checkboxInput("show_points", "显示数据点", TRUE),
                  checkboxInput("show_smooth", "显示趋势线", TRUE),
                  actionButton("plot_trend", "绘制趋势", class = "btn-primary"),
                  hr(),
                  h4("图表导出"),
                  tags$div(class = "custom-select",
                           selectInput("trend_plot_format", "趋势图导出格式:", 
                                       choices = c("PDF" = "pdf", "JPG" = "jpg"),
                                       selected = "pdf")
                  ),
                  downloadButton("download_trend_plot", "下载关键词趋势图", 
                                 class = "btn-info", style = "width: 100%;")
                ),
                box(
                  title = "关键词时间演变", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(plotOutput("trend_plot", height = 500), type = 4)
                )
              )
      ),
      
      # ==================== 7. 热力图页面 ====================
      tabItem(tabName = "heatmap",
              fluidRow(
                box(
                  title = "热力图设置", status = "primary", solidHeader = TRUE, width = 3,
                  numericInput("heatmap_words", "显示词数:", 30, min = 10, max = 100),
                  selectInput("color_scale", "配色方案:",
                              choices = c("红-绿", "蓝-黄-红", "紫-黄"),
                              selected = "红-绿"),
                  checkboxInput("cluster_rows", "对行聚类", FALSE),
                  actionButton("plot_heatmap", "生成热力图", class = "btn-primary"),
                  hr(),
                  h4("图表导出"),
                  tags$div(class = "custom-select",
                           selectInput("heatmap_plot_format", "热力图导出格式:", 
                                       choices = c("PDF" = "pdf", "JPG" = "jpg"),
                                       selected = "pdf")
                  ),
                  downloadButton("download_heatmap_plot", "下载时间分布热力图", 
                                 class = "btn-info", style = "width: 100%;")
                ),
                box(
                  title = "时间分布热力图", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(plotOutput("heatmap_plot", height = 600), type = 4)
                )
              )
      ),
      
      # ==================== 8. 气泡图页面 ====================
      tabItem(tabName = "bubble",
              fluidRow(
                box(
                  title = "气泡图设置", status = "primary", solidHeader = TRUE, width = 3,
                  numericInput("min_total_count", "最小总频次:", 5, min = 1),
                  selectInput("bubble_color", "配色:",
                              choices = c("Set1", "Set2", "Set3", "Dark2"),
                              selected = "Set2"),
                  actionButton("plot_bubble", "生成气泡图", class = "btn-primary"),
                  hr(),
                  h4("数据导出"),
                  downloadButton("download_trend_metrics", "下载趋势统计数据(Excel)", 
                                 class = "btn-info", style = "width: 100%; margin-bottom: 5px;"),
                  tags$div(class = "custom-select",
                           selectInput("bubble_plot_format", "气泡图导出格式:", 
                                       choices = c("PDF" = "pdf", "JPG" = "jpg"),
                                       selected = "pdf")
                  ),
                  downloadButton("download_bubble_plot", "下载关键词趋势气泡图", 
                                 class = "btn-info", style = "width: 100%;")
                ),
                box(
                  title = "关键词趋势气泡图", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(plotOutput("bubble_plot", height = 500), type = 4)
                )
              ),
              fluidRow(
                box(
                  title = "趋势统计", status = "success", solidHeader = TRUE, width = 12,
                  withSpinner(DTOutput("trend_metrics_table"), type = 4)
                )
              )
      ),
      
      # ==================== 9. 词云图页面 ====================
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(
                  title = "词云设置", status = "primary", solidHeader = TRUE, width = 3,
                  sliderInput("wordcloud_size", "字体大小:", 1, min = 0.5, max = 2, step = 0.1),
                  sliderInput("wordcloud_rotate", "旋转比例:", 0.4, min = 0, max = 1, step = 0.1),
                  selectInput("wordcloud_shape", "形状:",
                              choices = c("圆形", "cardioid", "菱形", "三角形"),
                              selected = "圆形"),
                  colourpicker::colourInput("bg_color", "背景颜色:", value = "white"),
                  actionButton("generate_cloud", "生成词云", class = "btn-primary"),
                  hr(),
                  downloadButton("download_cloud_jpg", "下载JPG", 
                                 class = "btn-success", style = "width: 100%; margin-bottom: 5px;"),
                  downloadButton("download_cloud_pdf", "下载PDF", 
                                 class = "btn-success", style = "width: 100%;")
                ),
                box(
                  title = "词云图", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(wordcloud2Output("wordcloud_plot", height = 600), type = 4)
                )
              )
      ),
      # ==================== 10. 文献综述页面 ====================
      tabItem(tabName = "review",
              fluidRow(
                box(
                  title = "综述设置", status = "primary", solidHeader = TRUE, width = 4,
                  
                  # 数据来源选择
                  radioButtons(
                    inputId = "review_data_source",
                    label = "数据来源",
                    choices = list(
                      "当前分析数据" = "current",
                      "筛选结果" = "filtered",
                      "上传新文件" = "upload"
                    ),
                    selected = "current"
                  ),
                  
                  conditionalPanel(
                    condition = "input.review_data_source == 'upload'",
                    fileInput("review_file", "上传Excel文件", 
                              accept = c(".xlsx", ".xls"),
                              placeholder = "支持 .xlsx 或 .xls 格式")
                  ),
                  
                  hr(),
                  
                  # 综述类型选择
                  radioButtons(
                    inputId = "review_type",
                    label = "综述类型",
                    choices = list(
                      "国内外研究现状" = "domestic_international",
                      "仅国内研究现状" = "domestic_only",
                      "仅国外研究现状" = "international_only"
                    ),
                    selected = "domestic_international"
                  ),
                  
                  # 写作风格
                  selectInput(
                    inputId = "review_style",
                    label = "写作风格",
                    choices = c(
                      "学术严谨型" = "academic",
                      "综述型" = "review",
                      "批判型" = "critical",
                      "综合性" = "comprehensive"
                    ),
                    selected = "academic"
                  ),
                  
                  # 字数控制
                  sliderInput(
                    inputId = "review_word_count",
                    label = "目标字数",
                    min = 500,
                    max = 5000,
                    value = 1500,
                    step = 100
                  ),
                  
                  # 时间范围
                  dateRangeInput(
                    inputId = "review_year_range",
                    label = "文献时间范围",
                    start = "2015-01-01",
                    end = Sys.Date(),
                    format = "yyyy"
                  ),
                  
                  hr(),
                  
                  # 写作建议输入
                  textAreaInput(
                    inputId = "review_suggestions",
                    label = "写作建议（可选）",
                    placeholder = "例如：重点关注方法创新、理论贡献、研究热点等...",
                    rows = 3
                  ),
                  
                  # API密钥（复用之前的）
                  passwordInput("api_key_review", "DeepSeek API密钥", 
                                placeholder = "输入sk-开头的密钥"),
                  
                  hr(),
                  
                  # 生成按钮
                  actionButton("generate_review", "生成文献综述", 
                               class = "btn-success btn-lg", 
                               style = "width: 100%; margin-bottom: 10px;"),
                  
                  actionButton("stop_review", "停止生成", 
                               class = "btn-danger", 
                               style = "width: 100%;"),
                  
                  hr(),
                  
                  # 下载按钮
                  uiOutput("download_review_btns")
                ),
                
                # 右侧主显示区
                box(
                  title = "生成的文献综述", status = "info", solidHeader = TRUE, width = 8,
                  tabsetPanel(
                    tabPanel(
                      "综述内容",
                      div(
                        style = "padding: 15px; background-color: #f9f9f9; border-radius: 5px; min-height: 500px;",
                        withSpinner(uiOutput("review_content"), type = 4)
                      )
                    ),
                    tabPanel(
                      "参考文献",
                      div(
                        style = "padding: 15px; background-color: #f9f9f9; border-radius: 5px;",
                        withSpinner(uiOutput("reference_list"), type = 4)
                      )
                    ),
                    tabPanel(
                      "生成过程",
                      verbatimTextOutput("review_progress")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "文献统计", status = "warning", solidHeader = TRUE, width = 12,
                  valueBoxOutput("review_total_papers", width = 3),
                  valueBoxOutput("review_year_range_box", width = 3),
                  valueBoxOutput("review_avg_citations", width = 3),
                  valueBoxOutput("review_keywords_count", width = 3)
                )
              )
      )
      
    )
  )
)

  # ==========================================
  # 2. Server 定义
  # ==========================================
  server <- function(input, output, session) {
  
  # 反应式值存储
  rv <- reactiveValues(
    # 检索模块数据
    raw_data_search = NULL,
    filtered_search_data = NULL,
    
    # 翻译模块数据
    raw_data_trans = NULL,
    translated_data = NULL,
    is_running = FALSE,
    is_canceled = FALSE,
    current_progress = 0,
    
    # 分析模块数据
    processed_data = NULL,                # 原始处理数据（包含ABCN）
    translated_processed_data = NULL,     # 翻译后的处理数据（用于分词）
    segmented_data = NULL,
    word_freq = NULL,
    keyword_trend = NULL,
    trend_metrics = NULL,
    segment_log = "",
    is_segmenting = FALSE,
    api_test_result = NULL,
    wordcloud_obj = NULL,
    
    
    # 文献综述相关
    review_data = NULL,
    review_result = NULL,
    reference_list = NULL,
    is_review_generating = FALSE,
    is_review_canceled = FALSE,
    review_progress_log = "" 
    
  )
  
  # 修复点1：重写通知函数，仅使用合法的type参数
  show_notification <- function(message, type = "message") {
    valid_types <- c("message", "warning", "error")
    type <- ifelse(type %in% valid_types, type, "message")
    showNotification(message, type = type, duration = 3)
  }
  
  # ========== 新增：添加JavaScript代码输出 ==========
  output$progress_script_ui <- renderUI({
    tags$script(HTML("
      Shiny.addCustomMessageHandler('initProgressBar', function(message) {
        $('#progress_bar').css('width', '0%').text('0%');
      });
      
      Shiny.addCustomMessageHandler('updateProgress', function(message) {
        var progress = message.progress;
        $('#progress_bar').css('width', progress + '%').text(progress + '%');
      });
    "))
  })
  # =================================================
  
  # 进度条初始化
  observe({
    session$onFlushed(function() {
      session$sendCustomMessage("initProgressBar", list())
    }, once = TRUE)
  })
  
  # ==================== API配置模块====================
  
  observeEvent(input$test_api, {
    rv$api_test_result <- "正在测试连接..."
    Sys.sleep(1)
    rv$api_test_result <- paste0(
      "状态: 就绪\n",
      "模型: ", input$model, "\n",
      "超时: ", input$timeout, "秒\n",
      "最后测试: ", format(Sys.time(), "%H:%M:%S")
    )
    show_notification("API连接测试完成", "message")
  })
  
  output$api_status <- renderText({
    rv$api_test_result %||% "未测试"
  })
  
  # ==================== 数据上传与检索模块 ====================
  
  # 1. 读取数据
  observeEvent(input$load_data, {
    if (input$use_demo) {
      # 创建示例数据
      rv$raw_data_search <- tibble(
        TI = c("示例标题1", "示例标题2", "示例标题3", "示例标题4", "示例标题5"),
        AB = c("This is a study about machine learning and deep learning.", 
               "Natural language processing for text mining applications.",
               "Artificial intelligence based data analysis methods.",
               "Climate change impacts on Tibetan Plateau ecosystem.",
               "Biodiversity conservation in Qinghai-Tibet region."),
        ABCN = c("这是一个关于机器学习和深度学习的研究示例。", 
                 "自然语言处理技术在文本挖掘中的应用研究。",
                 "基于人工智能的数据分析方法探讨。",
                 "气候变化对青藏高原生态系统的影响。",
                 "青藏地区生物多样性保护研究。"),
        PY = c(2023, 2024, 2023, 2022, 2024),
        DE = c("机器学习;深度学习", "NLP;文本挖掘", "人工智能;数据分析", 
               "气候变化;青藏高原", "生物多样性;青藏地区")
      )
      show_notification("已加载示例数据", "message")
    } else {
      req(input$data_file)
      tryCatch({
        df <- as.data.frame(read_excel(input$data_file$datapath, sheet = 1, col_names = TRUE))
        # 将所有列转换为字符类型
        for (i in 1:ncol(df)) {
          df[[i]] <- as.character(df[[i]])
        }
        df[is.na(df)] <- ""
        rv$raw_data_search <- df
        show_notification("数据加载成功！", "message")
      }, error = function(e) {
        show_notification(paste("加载失败:", e$message), "error")
      })
    }
    
    # 数据预处理 - 用于后续分析
    if (!is.null(rv$raw_data_search)) {
      required_cols <- c("PY")
      if (!all(required_cols %in% names(rv$raw_data_search))) {
        show_notification("数据缺少必要字段 (PY)", "error")
        rv$processed_data <- NULL
        rv$translated_processed_data <- NULL
      } else {
        # 原始处理数据（包含ABCN，用于分词）
        rv$processed_data <- rv$raw_data_search %>%
          select(any_of(c("TI", "AB", "ABCN", "PY", "DE"))) %>%
          mutate(
            across(everything(), as.character),
            PY = as.numeric(PY),
            doc_id = row_number()
          ) %>%
          filter(!is.na(PY), PY >= 1900, PY <= year(Sys.Date()))
        
        # 翻译后的处理数据（初始化为相同结构，但ABCN可能为空）
        rv$translated_processed_data <- rv$processed_data %>%
          mutate(ABCN = NA_character_)  # 清空ABCN，等待翻译后填充
      }
    }
  })
  
  # 数据预览
  output$data_preview <- renderDT({
    req(rv$raw_data_search)
    datatable(head(rv$raw_data_search, 10), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # 统计信息框
  output$total_docs_box <- renderValueBox({
    valueBox(
      nrow(rv$raw_data_search) %||% 0,
      "原始记录数",
      icon = icon("file"),
      color = "aqua"
    )
  })
  
  output$year_range_box <- renderValueBox({
    req(rv$processed_data)
    years <- range(rv$processed_data$PY, na.rm = TRUE)
    valueBox(
      paste(years[1], "-", years[2]),
      "年份范围",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$valid_docs_box <- renderValueBox({
    valueBox(
      nrow(rv$processed_data) %||% 0,
      "有效记录数",
      icon = icon("check"),
      color = "green"
    )
  })
  
  # 2. 筛选功能
  observeEvent(input$filter_btn, {
    req(rv$raw_data_search, input$keywords)
    
    if (trimws(input$keywords) == "") {
      show_notification("请输入至少一个有效关键词！", "warning")
      return()
    }
    
    tryCatch({
      data <- rv$raw_data_search
      
      # 处理关键词
      keywords_raw <- input$keywords
      keywords_raw <- gsub('["\']', '', keywords_raw)
      keywords_list <- strsplit(keywords_raw, ",")[[1]]
      keywords_list <- trimws(keywords_list)
      keywords_list <- keywords_list[keywords_list != ""]
      keywords_list <- tolower(keywords_list)
      keywords_list <- unique(keywords_list)
      
      if (length(keywords_list) == 0) {
        show_notification("请输入至少一个有效关键词！", "warning")
        return()
      }
      
      # 确定要搜索的列
      possible_cols <- c("TI", "AB", "DE", "title", "abstract", "keywords", 
                         "Title", "Abstract", "Keywords", "TITLE", "ABSTRACT", "KEYWORDS")
      search_cols <- intersect(possible_cols, names(data))
      if (length(search_cols) == 0) {
        search_cols <- names(data)
      }
      
      # 创建匹配标记向量
      n_rows <- nrow(data)
      match_flag <- rep(FALSE, n_rows)
      
      withProgress(message = '正在筛选数据...', value = 0, {
        for (i in 1:n_rows) {
          incProgress(1/n_rows, detail = paste("处理第", i, "行"))
          
          search_text <- ""
          for (col in search_cols) {
            val <- data[i, col]
            if (is.character(val) && !is.na(val) && nchar(val) > 0) {
              if (nchar(search_text) > 0) {
                search_text <- paste(search_text, val)
              } else {
                search_text <- val
              }
            }
          }
          
          search_text <- tolower(search_text)
          
          for (keyword in keywords_list) {
            if (grepl(keyword, search_text, fixed = TRUE)) {
              match_flag[i] <- TRUE
              break
            }
          }
        }
      })
      
      if (input$filter_logic == "include") {
        result <- data[match_flag, , drop = FALSE]
      } else {
        result <- data[!match_flag, , drop = FALSE]
      }
      
      rownames(result) <- NULL
      rv$filtered_search_data <- result
      
      show_notification(paste("筛选完成，共", nrow(result), "条记录"), "message")
      
    }, error = function(e) {
      show_notification(paste("筛选过程出错：", e$message), "error")
      rv$filtered_search_data <- NULL
    })
  })
  
  # 筛选结果概览
  output$summary_info <- renderPrint({
    req(rv$raw_data_search)
    
    original_n <- nrow(rv$raw_data_search)
    
    if (is.null(rv$filtered_search_data)) {
      cat("原始文献总数：", original_n, "\n")
      cat("筛选后文献数量：0\n")
      cat("状态：请点击'执行筛选'按钮进行筛选\n")
    } else {
      filtered_n <- nrow(rv$filtered_search_data)
      cat("原始文献总数：", original_n, "\n")
      cat("筛选后文献数量：", filtered_n, "\n")
      if (input$filter_logic == "include") {
        cat("匹配关键词的文献数量：", filtered_n, "\n")
        cat("匹配率：", round(filtered_n/original_n * 100, 2), "%\n")
      } else {
        cat("排除的关键词相关文献数量：", original_n - filtered_n, "\n")
        cat("排除率：", round((original_n - filtered_n)/original_n * 100, 2), "%\n")
      }
    }
  })
  
  # 筛选后数据表格
  output$filtered_table <- renderDT({
    req(rv$filtered_search_data)
    if (nrow(rv$filtered_search_data) > 0) {
      datatable(rv$filtered_search_data, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(data.frame(信息 = "没有找到匹配的文献"), options = list(dom = 't'))
    }
  })
  
  # 原始数据表格
  output$original_table <- renderDT({
    req(rv$raw_data_search)
    datatable(rv$raw_data_search, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 操作按钮组
  output$action_buttons_ui <- renderUI({
    req(rv$filtered_search_data)
    div(
      downloadButton("export_data", "导出筛选后的数据", class = "btn-success", style = "width: 100%; margin-bottom: 5px;"),
      br(),
      actionButton("send_to_translate", "发送到翻译模块", class = "btn-info", style = "width: 100%;")
    )
  })
  
  # 导出筛选数据
  output$export_data <- downloadHandler(
    filename = function() {
      paste0("筛选结果_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      tryCatch({
        write_xlsx(rv$filtered_search_data, file)
        show_notification("数据导出成功！", "message")
      }, error = function(e) {
        show_notification(paste("导出失败：", e$message), "error")
      })
    }
  )
  
  # 发送到翻译模块
  observeEvent(input$send_to_translate, {
    req(rv$filtered_search_data)
    
    if (!"AB" %in% colnames(rv$filtered_search_data)) {
      show_notification("筛选后的数据中未找到'AB'列（摘要列），无法进行翻译！", "error")
      return()
    }
    
    updateTabItems(session, "sidebarMenu", "translate")
    updateRadioButtons(session, "data_source", selected = "search_result")
    show_notification(paste("已将筛选后的", nrow(rv$filtered_search_data), "条记录发送到翻译模块！"), "message")
  })
  
  # ==================== 摘要翻译模块 ====================
  
  # 显示检索结果信息
  output$search_data_info <- renderText({
    req(rv$filtered_search_data)
    if ("AB" %in% colnames(rv$filtered_search_data)) {
      ab_col <- as.character(rv$filtered_search_data$AB)
      ab_count <- sum(!is.na(ab_col) & trimws(ab_col) != "")
      paste0("共 ", nrow(rv$filtered_search_data), " 条记录，",
             "非空摘要 ", ab_count, " 条")
    } else {
      "⚠️ 数据中未找到AB列（摘要列）"
    }
  })
  
  # 读取翻译文件
  observeEvent(input$file_translate, {
    if (input$data_source == "upload") {
      tryCatch({
        rv$raw_data_trans <- read_excel(input$file_translate$datapath, sheet = 1)
        if (!"AB" %in% colnames(rv$raw_data_trans)) {
          show_notification("上传的文件中未找到'AB'列！", "error")
          rv$raw_data_trans <- NULL
        } else {
          rv$raw_data_trans$AB <- as.character(rv$raw_data_trans$AB)
          
          # 同时更新processed_data用于分词
          if (!is.null(rv$processed_data)) {
            # 如果已有处理数据，更新ABCN列
            rv$processed_data <- rv$raw_data_trans %>%
              mutate(
                doc_id = row_number(),
                PY = as.numeric(PY) %||% NA_real_,
                ABCN = as.character(ABCN) %||% ""
              ) %>%
              filter(!is.na(PY), PY >= 1900, PY <= year(Sys.Date()))
          } else {
            # 如果没有处理数据，创建新的
            rv$processed_data <- rv$raw_data_trans %>%
              mutate(
                doc_id = row_number(),
                PY = as.numeric(PY) %||% NA_real_,
                ABCN = as.character(ABCN) %||% ""
              ) %>%
              filter(!is.na(PY), PY >= 1900, PY <= year(Sys.Date()))
          }
          
          # 初始化翻译后的处理数据
          rv$translated_processed_data <- rv$processed_data %>%
            mutate(ABCN = NA_character_)
          
          show_notification(paste("成功加载数据：共", nrow(rv$raw_data_trans), "条记录"), "message")
        }
      }, error = function(e) {
        show_notification(paste("读取文件出错：", e$message), "error")
        rv$raw_data_trans <- NULL
      })
    }
  })
  
  # 获取翻译数据
  get_translation_data <- reactive({
    if (is.null(input$data_source)) return(NULL)
    
    if (input$data_source == "search_result") {
      req(rv$filtered_search_data)
      if (!"AB" %in% colnames(rv$filtered_search_data)) {
        show_notification("检索结果中未找到'AB'列，无法翻译！", "error")
        return(NULL)
      }
      data <- rv$filtered_search_data
      data$AB <- as.character(data$AB)
      return(data)
    } else {
      req(rv$raw_data_trans)
      return(rv$raw_data_trans)
    }
  })
  
  # 取消翻译
  observeEvent(input$cancel_translate, {
    if (rv$is_running) {
      rv$is_canceled <- TRUE
      show_notification("正在取消翻译任务...", "warning")
    }
  })
  
  # 核心翻译逻辑 - 修复进度显示
  observeEvent(input$run_translate, {
    translation_data <- get_translation_data()
    if (is.null(translation_data)) {
      return()
    }
    
    api_key_to_use <- if (input$api_key != "") input$api_key else input$api_key_config
    if (api_key_to_use == "" || !grepl("^sk-", api_key_to_use)) {
      show_notification("请输入有效的API密钥（以sk-开头）！", "error")
      return()
    }
    
    max_tokens <- as.numeric(input$max_tokens_trans)
    delay_seconds <- as.numeric(input$delay_seconds_trans)
    
    if (is.na(max_tokens) || max_tokens < 500 || max_tokens > 8192) {
      show_notification("最大Token数必须是500-8192之间的数值！", "error")
      return()
    }
    if (is.na(delay_seconds) || delay_seconds < 0.1 || delay_seconds > 10) {
      show_notification("请求间隔必须是0.1-10之间的数值！", "error")
      return()
    }
    
    rv$is_running <- TRUE
    rv$is_canceled <- FALSE
    rv$current_progress <- 0
    total_records <- nrow(translation_data)
    translations <- character(total_records)
    
    # 初始化进度条
    session$sendCustomMessage("initProgressBar", list())
    
    # 创建用于分词处理的数据框（包含原始数据）
    if (is.null(rv$translated_processed_data) || nrow(rv$translated_processed_data) != total_records) {
      # 如果翻译处理数据不存在或行数不匹配，创建新的
      rv$translated_processed_data <- translation_data %>%
        mutate(
          doc_id = row_number(),
          PY = as.numeric(PY) %||% NA_real_,
          ABCN = NA_character_,
          AB = as.character(AB)
        ) %>%
        filter(!is.na(PY), PY >= 1900, PY <= year(Sys.Date()))
    }
    
    # 更新状态显示
    output$trans_status <- renderPrint({
      cat("开始翻译任务...\n")
      cat(sprintf("总记录数：%d 条\n", total_records))
      cat("正在处理中，请稍候...\n")
    })
    
    for (i in 1:total_records) {
      if (rv$is_canceled) {
        rv$is_running <- FALSE
        output$trans_status <- renderPrint({
          cat("翻译任务已取消！\n")
          cat(sprintf("已处理记录：%d/%d 条\n", i-1, total_records))
        })
        session$sendCustomMessage("updateProgress", list(progress = round((i-1)/total_records*100, 1)))
        return()
      }
      
      translations[i] <- translate_with_deepseek(
        text = translation_data$AB[i],
        api_key = api_key_to_use,
        max_tokens = max_tokens
      )
      
      # 实时更新translated_processed_data中的ABCN列
      if (!is.null(rv$translated_processed_data) && i <= nrow(rv$translated_processed_data)) {
        rv$translated_processed_data$ABCN[i] <- translations[i]
      }
      
      # 更新进度
      rv$current_progress <- round((i / total_records) * 100, 1)
      session$sendCustomMessage("updateProgress", list(progress = rv$current_progress))
      
      # 更新状态显示
      if (i %% 5 == 0 || i == total_records) {  # 每5条更新一次状态，减少刷新频率
        output$trans_status <- renderPrint({
          cat(sprintf("正在处理：%d/%d 条 (%.1f%%)\n", i, total_records, rv$current_progress))
          success_sofar <- sum(!grepl("失败", translations[1:i]) & translations[1:i] != "", na.rm = TRUE)
          cat(sprintf("当前成功：%d 条\n", success_sofar))
        })
      }
      
      Sys.sleep(delay_seconds)
    }
    
    rv$is_running <- FALSE
    if (!rv$is_canceled) {
      # 更新翻译结果数据
      rv$translated_data <- translation_data %>%
        mutate(
          ABCN_trans = translations,
          en_length = nchar(trimws(as.character(AB))),
          cn_length = nchar(trimws(as.character(ABCN_trans)))
        )
      
      # 确保translated_processed_data完整
      if (!is.null(rv$translated_processed_data)) {
        # 更新所有翻译结果
        for (i in 1:length(translations)) {
          if (i <= nrow(rv$translated_processed_data)) {
            rv$translated_processed_data$ABCN[i] <- translations[i]
          }
        }
      }
      
      # 同时更新processed_data（如果存在相同的doc_id）
      if (!is.null(rv$processed_data) && nrow(rv$processed_data) >= total_records) {
        for (i in 1:total_records) {
          doc_id <- rv$translated_processed_data$doc_id[i]
          if (doc_id <= nrow(rv$processed_data)) {
            rv$processed_data$ABCN[doc_id] <- translations[i]
          }
        }
      }
      
      # 最终状态显示
      output$trans_status <- renderPrint({
        translations_char <- as.character(translations)
        success_count <- sum(!grepl("失败", translations_char) & translations_char != "")
        fail_count <- sum(grepl("失败", translations_char))
        empty_count <- sum(translations_char == "")
        
        cat("翻译任务完成！\n")
        cat(sprintf("总记录数：%d 条\n", total_records))
        cat(sprintf("成功翻译：%d 条\n", success_count))
        cat(sprintf("翻译失败：%d 条\n", fail_count))
        cat(sprintf("空摘要：%d 条\n", empty_count))
      })
      
      session$sendCustomMessage("updateProgress", list(progress = 100))
      
      output$preview_table <- renderTable({
        rv$translated_data %>%
          select(AB, ABCN_trans) %>%
          head(5) %>%
          mutate(
            AB = ifelse(nchar(as.character(AB)) > 80, paste0(substr(as.character(AB), 1, 80), "..."), as.character(AB)),
            ABCN_trans = ifelse(nchar(as.character(ABCN_trans)) > 80, paste0(substr(as.character(ABCN_trans), 1, 80), "..."), as.character(ABCN_trans))
          ) %>%
          rename("英文摘要" = AB, "中文翻译" = ABCN_trans)
      }, striped = TRUE, hover = TRUE)
      
      output$preview_table_full <- renderTable({
        rv$translated_data %>%
          select(AB, ABCN_trans) %>%
          head(10) %>%
          mutate(
            AB = ifelse(nchar(as.character(AB)) > 100, paste0(substr(as.character(AB), 1, 100), "..."), as.character(AB)),
            ABCN_trans = ifelse(nchar(as.character(ABCN_trans)) > 100, paste0(substr(as.character(ABCN_trans), 1, 100), "..."), as.character(ABCN_trans))
          ) %>%
          rename("英文摘要" = AB, "中文翻译" = ABCN_trans)
      }, striped = TRUE, hover = TRUE)
      
      output$trans_stats <- renderPrint({
        stats_data <- rv$translated_data %>%
          summarise(
            数据来源 = ifelse(input$data_source == "search_result", "检索模块结果", "上传文件"),
            总记录数 = n(),
            空英文摘要数 = sum(en_length == 0, na.rm = TRUE),
            成功翻译数 = sum(!grepl("失败", as.character(ABCN_trans)) & as.character(ABCN_trans) != "", na.rm = TRUE),
            翻译成功率 = ifelse((总记录数 - 空英文摘要数) > 0, 
                           round(成功翻译数 / (总记录数 - 空英文摘要数) * 100, 1), 
                           0),
            平均英文摘要长度 = round(mean(en_length, na.rm = TRUE), 1),
            平均中文摘要长度 = round(mean(cn_length, na.rm = TRUE), 1)
          )
        print(stats_data)
      })
      
      output$download_trans_btn <- renderUI({
        downloadButton("download_trans_result", "下载翻译后的数据", class = "btn-success", style = "width: 100%;")
      })
      
      show_notification("翻译完成！", "message")
    }
  })
  
  # 下载翻译结果
  output$download_trans_result <- downloadHandler(
    filename = function() {
      paste0("翻译结果_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      output_data <- rv$translated_data %>%
        select(-any_of(c("en_length", "cn_length")))
      write_xlsx(output_data, file)
    }
  )
  
  # ==================== 分词处理模块（修改：使用翻译后的数据）====================
  
  observeEvent(input$start_segment, {
    # 优先使用翻译后的处理数据（translated_processed_data），如果没有则使用原始处理数据
    segment_data <- if (!is.null(rv$translated_processed_data) && nrow(rv$translated_processed_data) > 0) {
      # 检查是否有翻译后的中文摘要
      has_translation <- any(!is.na(rv$translated_processed_data$ABCN) & 
                               rv$translated_processed_data$ABCN != "")
      if (has_translation) {
        rv$translated_processed_data
      } else {
        rv$processed_data
      }
    } else {
      rv$processed_data
    }
    
    req(segment_data)
    
    api_key_to_use <- if (input$api_key != "") input$api_key else input$api_key_config
    if (api_key_to_use == "" || !grepl("^sk-", api_key_to_use)) {
      show_notification("请输入有效的API密钥（以sk-开头）！", "error")
      return()
    }
    
    if (rv$is_segmenting) {
      show_notification("正在分词中，请等待...", "warning")
      return()
    }
    
    rv$is_segmenting <- TRUE
    rv$segment_log <- "开始分词处理...\n"
    
    data <- segment_data
    if (input$test_mode && nrow(data) > 10) {
      data <- head(data, 10)
      rv$segment_log <- paste(rv$segment_log, "测试模式：仅处理前10条记录\n")
    }
    
    total <- nrow(data)
    if (total == 0) {
      show_notification("无有效数据可处理", "warning")
      rv$is_segmenting <- FALSE
      return()
    }
    results <- vector("list", total)
    
    # 确定使用哪一列进行分词（优先使用ABCN）
    text_col <- if ("ABCN" %in% names(data) && any(!is.na(data$ABCN) & data$ABCN != "")) {
      "ABCN"
    } else if ("AB" %in% names(data)) {
      "AB"
    } else {
      show_notification("未找到可用于分词的文本列（需要ABCN或AB列）", "error")
      rv$is_segmenting <- FALSE
      return()
    }
    
    rv$segment_log <- paste(rv$segment_log, sprintf("使用列 '%s' 进行分词\n", text_col))
    
    withProgress(message = '正在分词...', value = 0, {
      for (i in seq_len(total)) {
        if (!rv$is_segmenting) break
        
        text <- data[[text_col]][i]
        doc_id <- data$doc_id[i]
        
        progress_pct <- round(i/total * 100)
        incProgress(1/total, detail = paste("处理第", i, "篇文献"))
        updateProgressBar(session, "segment_progress", value = progress_pct)
        
        result <- segment_chinese_with_deepseek(
          text, 
          api_key_to_use, 
          doc_id,
          max_tokens = input$max_tokens,
          model = input$model,
          timeout_sec = input$timeout
        )
        
        results[[i]] <- result$words
        
        status <- if(result$success) "成功" else paste("失败:", result$error %||% "未知错误")
        log_entry <- sprintf("[%d/%d] 文档 %s: %s (提取 %d 个词)\n", 
                             i, total, doc_id, status, length(result$words))
        rv$segment_log <- paste(rv$segment_log, log_entry, sep = "")
        
        if (i < total) Sys.sleep(input$delay_seconds)
      }
    })
    
    if (!rv$is_segmenting) {
      rv$segment_log <- paste(rv$segment_log, "\n用户手动停止")
      show_notification("已停止分词", "warning")
      rv$is_segmenting <- FALSE
      return()
    }
    
    if (length(results) > 0) {
      rv$segmented_data <- data %>%
        mutate(
          segmented_words = results,
          word_count = map_int(segmented_words, length)
        )
      
      all_words <- unlist(results)
      if (length(all_words) == 0) {
        show_notification("未提取到任何词汇", "warning")
        rv$is_segmenting <- FALSE
        return()
      }
      
      rv$word_freq <- tibble(word = all_words) %>%
        count(word, sort = TRUE) %>%
        mutate(weight = log(n + 1), rank = row_number())
      
      rv$keyword_trend <- rv$segmented_data %>%
        select(doc_id, PY, segmented_words) %>%
        unnest(segmented_words) %>%
        rename(word = segmented_words) %>%
        group_by(PY, word) %>%
        summarise(count = n(), .groups = "drop") %>%
        complete(PY, word, fill = list(count = 0)) %>%
        filter(!is.na(PY), !is.na(word))
      
      top_keywords <- rv$word_freq %>% 
        top_n(30, n) %>% 
        pull(word)
      
      rv$trend_metrics <- rv$keyword_trend %>%
        filter(word %in% top_keywords) %>%
        group_by(word) %>%
        summarise(
          total_count = sum(count),
          mean_freq = mean(count),
          trend = ifelse(n() > 2, cor(PY, count, method = "spearman"), 0),
          first_active = min(PY[count > 0], na.rm = TRUE),
          last_active = max(PY[count > 0], na.rm = TRUE)
        ) %>%
        mutate(
          trend_category = case_when(
            trend > 0.3 ~ "上升趋势",
            trend < -0.3 ~ "下降趋势",
            TRUE ~ "平稳趋势"
          )
        ) %>%
        filter(!is.na(first_active), !is.na(last_active))
      
      updateSelectizeInput(session, "trend_keywords", 
                           choices = rv$word_freq$word,
                           selected = head(rv$word_freq$word, 5))
      
      rv$segment_log <- paste(rv$segment_log, "\n分词完成！")
      show_notification("分词处理完成！", "message")
    }
    
    rv$is_segmenting <- FALSE
    updateProgressBar(session, "segment_progress", value = 0)
  })
  
  observeEvent(input$stop_segment, {
    rv$is_segmenting <- FALSE
    rv$segment_log <- paste(rv$segment_log, "\n用户手动停止")
    show_notification("已停止分词", "warning")
  })
  
  output$segment_stats <- renderText({
    req(rv$segmented_data)
    sprintf(
      "总文档数: %d\n成功分词: %d\n总词数: %d\n平均词数/文档: %.1f\n唯一词数: %d",
      nrow(rv$segmented_data),
      sum(rv$segmented_data$word_count > 0),
      sum(rv$segmented_data$word_count),
      mean(rv$segmented_data$word_count),
      nrow(rv$word_freq)
    )
  })
  
  output$segment_log <- renderText({
    rv$segment_log
  })
  
  output$segment_preview <- renderDT({
    req(rv$segmented_data)
    datatable(
      rv$segmented_data %>% 
        select(doc_id, PY, word_count, segmented_words) %>%
        mutate(segmented_words = map_chr(segmented_words, paste, collapse = ", ")),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$download_segment_data <- downloadHandler(
    filename = function() {
      paste0("分词结果_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$segmented_data)
      export_data <- rv$segmented_data %>%
        mutate(segmented_words = map_chr(segmented_words, paste, collapse = "; "))
      write_xlsx(export_data, file)
      show_notification("分词结果导出成功！", "message")
    }
  )
  
  # ==================== 词频分析模块 ====================
  
  output$freq_plot <- renderPlot({
    req(rv$word_freq)
    input$refresh_freq
    
    data <- rv$word_freq %>%
      filter(n >= input$min_freq, nchar(word) >= input$min_word_length) %>%
      head(input$top_n_words)
    
    if (nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x=1, y=1, label="无符合条件的词汇"), size=12) + 
        theme_void()
    } else {
      ggplot(data, aes(x = reorder(word, n), y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
        labs(x = "词汇", y = "频次", title = "高频词汇分布") +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
  output$freq_table <- renderDT({
    req(rv$word_freq)
    datatable(
      rv$word_freq %>%
        filter(n >= input$min_freq) %>%
        head(100),
      options = list(pageLength = 10, order = list(list(1, 'desc')))
    )
  })
  
  output$download_freq_data <- downloadHandler(
    filename = function() {
      paste0("高频词汇表_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$word_freq)
      export_data <- rv$word_freq %>%
        filter(n >= input$min_freq, nchar(word) >= input$min_word_length) %>%
        head(input$top_n_words)
      write_xlsx(export_data, file)
      show_notification("高频词汇表导出成功！", "message")
    }
  )
  
  output$download_freq_plot <- downloadHandler(
    filename = function() {
      paste0("词频分布图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$freq_plot_format)
    },
    content = function(file) {
      req(rv$word_freq)
      plot_data <- rv$word_freq %>%
        filter(n >= input$min_freq, nchar(word) >= input$min_word_length) %>%
        head(input$top_n_words)
      
      p <- ggplot(plot_data, aes(x = reorder(word, n), y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
        labs(x = "词汇", y = "频次", title = "高频词汇分布") +
        theme_minimal() +
        theme(legend.position = "none")
      
      if (input$freq_plot_format == "pdf") {
        ggsave(file, plot = p, device = "pdf", width = 12, height = 8, dpi = 300)
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 12, height = 8, dpi = 300)
      }
      show_notification("词频分布图导出成功！", "message")
    }
  )
  
  # ==================== 时间趋势模块 ====================
  
  output$trend_plot <- renderPlot({
    req(rv$keyword_trend)
    input$plot_trend
    
    if (is.null(input$trend_keywords) || length(input$trend_keywords) == 0) {
      ggplot() + 
        geom_text(aes(x=1, y=1, label="请先选择至少一个关键词"), size=12) + 
        theme_void()
      return()
    }
    
    plot_data <- rv$keyword_trend %>%
      filter(word %in% input$trend_keywords) %>%
      group_by(PY) %>%
      mutate(rel_freq = count / sum(count) * 100) %>%
      ungroup()
    
    p <- ggplot(plot_data, aes(x = PY, y = rel_freq, color = word, group = word)) +
      geom_line(size = 1.2)
    
    if (input$show_points) {
      p <- p + geom_point(size = 2, alpha = 0.7)
    }
    
    if (input$show_smooth) {
      p <- p + geom_smooth(method = "loess", se = FALSE, span = input$smooth_span)
    }
    
    p + labs(
      title = "关键词时间演变趋势",
      x = "年份",
      y = "相对频率 (%)",
      color = "关键词"
    ) +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  output$download_trend_plot <- downloadHandler(
    filename = function() {
      paste0("关键词趋势图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$trend_plot_format)
    },
    content = function(file) {
      req(rv$keyword_trend, input$trend_keywords)
      
      plot_data <- rv$keyword_trend %>%
        filter(word %in% input$trend_keywords) %>%
        group_by(PY) %>%
        mutate(rel_freq = count / sum(count) * 100) %>%
        ungroup()
      
      p <- ggplot(plot_data, aes(x = PY, y = rel_freq, color = word, group = word)) +
        geom_line(size = 1.2)
      if (input$show_points) {
        p <- p + geom_point(size = 2, alpha = 0.7)
      }
      if (input$show_smooth) {
        p <- p + geom_smooth(method = "loess", se = FALSE, span = input$smooth_span)
      }
      p <- p + labs(
        title = "关键词时间演变趋势",
        x = "年份",
        y = "相对频率 (%)",
        color = "关键词"
      ) +
        theme_minimal() +
        theme(legend.position = "right")
      
      if (input$trend_plot_format == "pdf") {
        ggsave(file, plot = p, device = "pdf", width = 12, height = 8, dpi = 300)
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 12, height = 8, dpi = 300)
      }
      show_notification("关键词趋势图导出成功！", "message")
    }
  )
  
  # ==================== 热力图模块 ====================
  
  output$heatmap_plot <- renderPlot({
    req(rv$keyword_trend)
    input$plot_heatmap
    
    top_words <- rv$word_freq %>%
      head(input$heatmap_words) %>%
      pull(word)
    
    data <- rv$keyword_trend %>%
      filter(word %in% top_words) %>%
      mutate(freq_scaled = scale(count)[, 1])
    
    if (nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x=1, y=1, label="无足够数据生成热力图"), size=12) + 
        theme_void()
      return()
    }
    
    colors <- switch(input$color_scale,
                     "红-绿" = scale_fill_gradient2(low = "#2E8B57", mid = "#F5F5DC", high = "#8B0000"),
                     "蓝-黄-红" = scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026"),
                     "紫-黄" = scale_fill_gradient(low = "#FDE725", high = "#440154"))
    
    ggplot(data, aes(x = PY, y = reorder(word, count), fill = freq_scaled)) +
      geom_tile(color = "white") +
      colors +
      labs(title = "关键词时间分布热力图", x = "年份", y = "关键词") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_heatmap_plot <- downloadHandler(
    filename = function() {
      paste0("热力图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$heatmap_plot_format)
    },
    content = function(file) {
      req(rv$keyword_trend)
      
      top_words <- rv$word_freq %>%
        head(input$heatmap_words) %>%
        pull(word)
      
      plot_data <- rv$keyword_trend %>%
        filter(word %in% top_words) %>%
        mutate(freq_scaled = scale(count)[, 1])
      
      colors <- switch(input$color_scale,
                       "红-绿" = scale_fill_gradient2(low = "#2E8B57", mid = "#F5F5DC", high = "#8B0000"),
                       "蓝-黄-红" = scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026"),
                       "紫-黄" = scale_fill_gradient(low = "#FDE725", high = "#440154"))
      
      p <- ggplot(plot_data, aes(x = PY, y = reorder(word, count), fill = freq_scaled)) +
        geom_tile(color = "white") +
        colors +
        labs(title = "关键词时间分布热力图", x = "年份", y = "关键词") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (input$heatmap_plot_format == "pdf") {
        ggsave(file, plot = p, device = "pdf", width = 14, height = 10, dpi = 300)
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 14, height = 10, dpi = 300)
      }
      show_notification("热力图导出成功！", "message")
    }
  )
  
  # ==================== 气泡图模块 ====================
  
  output$bubble_plot <- renderPlot({
    req(rv$trend_metrics)
    input$plot_bubble
    
    data <- rv$trend_metrics %>%
      filter(total_count >= input$min_total_count)
    
    if (nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x=1, y=1, label="无符合条件的关键词（可降低最小频次）"), size=10) + 
        theme_void()
      return()
    }
    
    ggplot(data, aes(x = first_active, y = mean_freq, 
                     size = total_count, color = trend_category)) +
      geom_point(alpha = 0.7) +
      geom_text_repel(aes(label = word), size = 3, max.overlaps = 15) +
      scale_size_continuous(range = c(3, 15)) +
      scale_color_manual(values = c("上升趋势" = "#E74C3C", 
                                    "下降趋势" = "#3498DB", 
                                    "平稳趋势" = "#2ECC71")) +
      labs(title = "关键词趋势气泡图",
           x = "首次出现年份",
           y = "平均频次",
           size = "总频次",
           color = "趋势类别") +
      theme_minimal()
  })
  
  output$trend_metrics_table <- renderDT({
    req(rv$trend_metrics)
    datatable(rv$trend_metrics, options = list(pageLength = 10))
  })
  
  output$download_trend_metrics <- downloadHandler(
    filename = function() {
      paste0("趋势统计数据_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$trend_metrics)
      write_xlsx(rv$trend_metrics, file)
      show_notification("趋势统计数据导出成功！", "message")
    }
  )
  
  output$download_bubble_plot <- downloadHandler(
    filename = function() {
      paste0("气泡图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$bubble_plot_format)
    },
    content = function(file) {
      req(rv$trend_metrics)
      
      plot_data <- rv$trend_metrics %>%
        filter(total_count >= input$min_total_count)
      
      p <- ggplot(plot_data, aes(x = first_active, y = mean_freq, 
                                 size = total_count, color = trend_category)) +
        geom_point(alpha = 0.7) +
        geom_text_repel(aes(label = word), size = 3, max.overlaps = 15) +
        scale_size_continuous(range = c(3, 15)) +
        scale_color_manual(values = c("上升趋势" = "#E74C3C", 
                                      "下降趋势" = "#3498DB", 
                                      "平稳趋势" = "#2ECC71")) +
        labs(title = "关键词趋势气泡图",
             x = "首次出现年份",
             y = "平均频次",
             size = "总频次",
             color = "趋势类别") +
        theme_minimal()
      
      if (input$bubble_plot_format == "pdf") {
        ggsave(file, plot = p, device = "pdf", width = 12, height = 8, dpi = 300)
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 12, height = 8, dpi = 300)
      }
      show_notification("气泡图导出成功！", "message")
    }
  )
  
  # ==================== 词云图模块（修复版）====================
  # 8. 词云图（修复下载和显示位置问题）
  output$wordcloud_plot <- renderWordcloud2({
    req(rv$word_freq)
    input$generate_cloud # 触发刷新
    
    data <- head(rv$word_freq, input$top_n_words)
    
    # 防崩溃：无数据时返回空
    if (nrow(data) == 0) {
      return(wordcloud2(data.frame(word="无数据", freq=1), size=1))
    }
    
    # 生成词云对象并存储
    wc_obj <- wordcloud2(
      data = data,
      size = input$wordcloud_size,
      minSize = 5,
      gridSize = 10,
      fontFamily = "Microsoft YaHei",
      fontWeight = "bold",
      color = colorRampPalette(brewer.pal(8, "Dark2"))(nrow(data)),
      backgroundColor = input$bg_color,
      rotateRatio = input$wordcloud_rotate,
      shape = switch(input$wordcloud_shape,
                     "圆形" = "circle",
                     "cardioid" = "cardioid",
                     "菱形" = "diamond",
                     "三角形" = "triangle")
    )
    
    # 保存词云对象到反应式值
    rv$wordcloud_obj <- wc_obj
    
    return(wc_obj)
  })
  
  # 添加CSS样式来居中词云图
  observe({
    runjs('
    setTimeout(function() {
      // 找到词云图的容器并添加居中样式
      var wordcloudDiv = document.querySelector("#wordcloud_plot");
      if (wordcloudDiv) {
        wordcloudDiv.style.display = "flex";
        wordcloudDiv.style.justifyContent = "center";
        wordcloudDiv.style.alignItems = "center";
        wordcloudDiv.style.width = "100%";
        
        // 找到canvas并设置样式
        var canvas = wordcloudDiv.querySelector("canvas");
        if (canvas) {
          canvas.style.margin = "0 auto";
          canvas.style.display = "block";
        }
      }
    }, 500);
  ')
  })
  
  # 修复下载JPG功能
  output$download_cloud_jpg <- downloadHandler(
    filename = function() {
      paste0("词云图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    content = function(file) {
      req(rv$wordcloud_obj)
      
      # 创建临时HTML文件
      temp_html <- tempfile(fileext = ".html")
      
      # 保存词云对象到HTML文件
      saveWidget(
        rv$wordcloud_obj, 
        temp_html, 
        selfcontained = TRUE,
        libdir = NULL,
        background = input$bg_color
      )
      
      # 使用webshot2将HTML转为JPG
      tryCatch({
        webshot2::webshot(
          url = temp_html,
          file = file,
          vwidth = 1200,
          vheight = 800,
          selector = "#wordcloud_plot",
          delay = 3,
          zoom = 2
        )
        
        # 清理临时文件
        unlink(temp_html)
        
        show_notification("词云图已导出为JPG格式！", "message")
      }, error = function(e) {
        show_notification(paste("导出失败:", e$message), "error")
      })
    }
  )
  
  # 修复下载PDF功能
  output$download_cloud_pdf <- downloadHandler(
    filename = function() {
      paste0("词云图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(rv$wordcloud_obj)
      
      # 创建临时HTML文件
      temp_html <- tempfile(fileext = ".html")
      
      # 保存词云对象到HTML文件
      saveWidget(
        rv$wordcloud_obj, 
        temp_html, 
        selfcontained = TRUE,
        libdir = NULL,
        background = input$bg_color
      )
      
      # 使用webshot2将HTML转为PDF
      tryCatch({
        webshot2::webshot(
          url = temp_html,
          file = file,
          vwidth = 1200,
          vheight = 800,
          selector = "#wordcloud_plot",
          delay = 3,
          zoom = 2
        )
        
        # 清理临时文件
        unlink(temp_html)
        
        show_notification("词云图已导出为PDF格式！", "message")
      }, error = function(e) {
        show_notification(paste("导出失败:", e$message), "error")
      })
    }
  )
  
  # 添加一个观察器来重新居中词云图（当生成词云或窗口大小改变时）
  observeEvent(input$generate_cloud, {
    runjs('
    setTimeout(function() {
      var wordcloudDiv = document.querySelector("#wordcloud_plot");
      if (wordcloudDiv) {
        wordcloudDiv.style.display = "flex";
        wordcloudDiv.style.justifyContent = "center";
        wordcloudDiv.style.alignItems = "center";
        wordcloudDiv.style.width = "100%";
        
        var canvas = wordcloudDiv.querySelector("canvas");
        if (canvas) {
          canvas.style.margin = "0 auto";
          canvas.style.display = "block";
        }
      }
    }, 800);
  ')
  })
  
  # ==================== 10. 文献综述模块 ====================
  
  # 准备综述数据
  observe({
    req(input$review_data_source)
    
    if (input$review_data_source == "current") {
      # 使用当前分析数据
      if (!is.null(rv$processed_data)) {
        rv$review_data <- rv$processed_data
      } else if (!is.null(rv$translated_processed_data)) {
        rv$review_data <- rv$translated_processed_data
      }
    } else if (input$review_data_source == "filtered") {
      # 使用筛选结果
      rv$review_data <- rv$filtered_search_data
    }
  })
  
  # 上传文件处理
  observeEvent(input$review_file, {
    tryCatch({
      df <- read_excel(input$review_file$datapath, sheet = 1)
      rv$review_data <- df
      show_notification("文献数据加载成功！", "message")
    }, error = function(e) {
      show_notification(paste("加载失败:", e$message), "error")
    })
  })
  
  # 文献统计信息
  output$review_total_papers <- renderValueBox({
    req(rv$review_data)
    valueBox(
      nrow(rv$review_data),
      "文献总数",
      icon = icon("file-alt"),
      color = "aqua"
    )
  })
  
  output$review_year_range_box <- renderValueBox({
    req(rv$review_data)
    if ("PY" %in% names(rv$review_data)) {
      years <- range(as.numeric(rv$review_data$PY), na.rm = TRUE)
      valueBox(
        paste(years[1], "-", years[2]),
        "年份范围",
        icon = icon("calendar"),
        color = "blue"
      )
    } else {
      valueBox("未知", "年份范围", icon = icon("calendar"), color = "blue")
    }
  })
  
  output$review_avg_citations <- renderValueBox({
    req(rv$review_data)
    if ("TC" %in% names(rv$review_data)) {
      avg_cite <- mean(as.numeric(rv$review_data$TC), na.rm = TRUE)
      valueBox(
        round(avg_cite, 1),
        "平均被引频次",
        icon = icon("quote-right"),
        color = "green"
      )
    } else {
      valueBox("N/A", "平均被引频次", icon = icon("quote-right"), color = "green")
    }
  })
  
  output$review_keywords_count <- renderValueBox({
    req(rv$review_data)
    if ("DE" %in% names(rv$review_data)) {
      keywords <- unlist(strsplit(paste(rv$review_data$DE, collapse = ";"), ";"))
      keywords <- trimws(keywords[keywords != ""])
      valueBox(
        length(unique(keywords)),
        "唯一关键词数",
        icon = icon("tags"),
        color = "yellow"
      )
    } else {
      valueBox("N/A", "唯一关键词数", icon = icon("tags"), color = "yellow")
    }
  })
  
  # 生成综述的主函数
  observeEvent(input$generate_review, {
    req(rv$review_data)
    
    # 检查API密钥
    api_key <- if (input$api_key_review != "") input$api_key_review else input$api_key_config
    if (api_key == "" || !grepl("^sk-", api_key)) {
      show_notification("请输入有效的DeepSeek API密钥！", "error")
      return()
    }
    
    # 准备文献数据
    review_data <- rv$review_data
    
    # 按年份筛选
    if ("PY" %in% names(review_data)) {
      start_year <- year(input$review_year_range[1])
      end_year <- year(input$review_year_range[2])
      review_data <- review_data %>%
        filter(PY >= start_year, PY <= end_year)
    }
    
    if (nrow(review_data) == 0) {
      show_notification("所选时间范围内没有文献！", "warning")
      return()
    }
    
    # 随机选择最多50篇代表性文献（避免token超限）
    if (nrow(review_data) > 50) {
      set.seed(123)
      review_data <- review_data[sample(nrow(review_data), 50), ]
    }
    
    rv$is_review_generating <- TRUE
    rv$is_review_canceled <- FALSE
    rv$review_progress_log <- "开始生成文献综述...\n"
    
    # 构建文献摘要文本
    paper_summaries <- c()
    for (i in 1:nrow(review_data)) {
      paper_info <- sprintf(
        "[文献%d] 标题: %s | 作者: %s | 年份: %s | 关键词: %s | 摘要: %s",
        i,
        review_data$TI[i] %||% "未知标题",
        review_data$AU[i] %||% "未知作者",
        review_data$PY[i] %||% "未知年份",
        review_data$DE[i] %||% "无关键词",
        substr(review_data$AB[i] %||% "无摘要", 1, 300)
      )
      paper_summaries <- c(paper_summaries, paper_info)
    }
    
    papers_text <- paste(paper_summaries, collapse = "\n\n")
    
    # 构建系统提示词
    system_prompt <- "你是一位学术文献综述专家，擅长撰写高质量的研究现状综述。"
    
    # 构建用户提示词
    review_type_text <- switch(input$review_type,
                               "domestic_international" = "国内外研究现状",
                               "domestic_only" = "国内研究现状",
                               "international_only" = "国外研究现状"
    )
    
    style_text <- switch(input$review_style,
                         "academic" = "采用学术严谨型风格，客观陈述研究进展，使用正式学术语言。",
                         "review" = "采用综述型风格，按照研究主题或方法分类梳理文献。",
                         "critical" = "采用批判型风格，指出研究不足和未来方向。",
                         "comprehensive" = "采用综合性风格，兼顾客观陈述和批判性思考。"
    )
    
    user_prompt <- sprintf(
      "请根据以下%d篇文献信息，撰写一份关于%s的%s。
    
写作要求：
1. 字数控制在%d字左右
2. %s
3. 请按照逻辑结构组织内容（如：按主题、方法或时间顺序）
4. 在引用具体文献时，请用[文献编号]的方式标注，例如：[文献1]、[文献2]
5. 写作建议：%s
6. 最后请列出完整的参考文献列表，包括编号、作者、年份、标题、期刊/来源

文献信息如下：
%s

请先撰写综述内容，然后在最后单独列出参考文献列表。",
      nrow(review_data),
      review_type_text,
      style_text,
      input$review_word_count,
      style_text,
      input$review_suggestions %||% "无特殊要求",
      papers_text
    )
    
    # API调用
    rv$review_progress_log <- paste(rv$review_progress_log, "正在调用DeepSeek API...\n")
    
    tryCatch({
      api_url <- "https://api.deepseek.com/chat/completions"
      
      request_body <- list(
        model = "deepseek-chat",
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = user_prompt)
        ),
        max_tokens = 4000,
        temperature = 0.7
      )
      
      headers <- add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      )
      
      withProgress(message = '正在生成综述...', value = 0.5, {
        response <- POST(
          url = api_url,
          headers,
          body = toJSON(request_body, auto_unbox = TRUE),
          timeout(120)
        )
      })
      
      if (status_code(response) == 200) {
        content <- content(response, "parsed")
        full_response <- content$choices[[1]]$message$content
        
        # 分离综述内容和参考文献
        parts <- strsplit(full_response, "参考文献|References", fixed = FALSE)[[1]]
        
        if (length(parts) >= 2) {
          rv$review_result <- parts[1]
          rv$reference_list <- parts[2]
        } else {
          rv$review_result <- full_response
          rv$reference_list <- "无独立参考文献列表"
        }
        
        rv$review_progress_log <- paste(rv$review_progress_log, 
                                        sprintf("✅ 综述生成成功！共处理%d篇文献。\n", nrow(review_data)))
        
        show_notification("文献综述生成完成！", "message")
      } else {
        error_msg <- content(response, "text")
        rv$review_progress_log <- paste(rv$review_progress_log, 
                                        sprintf("❌ API调用失败: HTTP %d - %s\n", 
                                                status_code(response), error_msg))
        show_notification("API调用失败，请检查密钥和网络", "error")
      }
      
    }, error = function(e) {
      rv$review_progress_log <- paste(rv$review_progress_log, 
                                      sprintf("❌ 生成过程出错: %s\n", e$message))
      show_notification(paste("生成失败:", e$message), "error")
    })
    
    rv$is_review_generating <- FALSE
  })
  
  # 停止生成
  observeEvent(input$stop_review, {
    rv$is_review_canceled <- TRUE
    rv$is_review_generating <- FALSE
    rv$review_progress_log <- paste(rv$review_progress_log, "⛔ 用户手动停止生成\n")
    show_notification("已停止生成", "warning")
  })
  
  # 显示综述内容
  output$review_content <- renderUI({
    req(rv$review_result)
    
    # 将文本转换为HTML格式，保留换行
    html_content <- gsub("\n", "<br>", rv$review_result)
    
    # 高亮文献引用
    html_content <- gsub("\\[文献(\\d+)\\]", '<span style="background-color: #e7f3ff; padding: 2px 5px; border-radius: 3px; font-weight: bold;">[文献\\1]</span>', html_content)
    
    HTML(paste0(
      '<div style="font-family: \'Microsoft YaHei\', Arial; line-height: 1.8; text-align: justify;">',
      html_content,
      '</div>'
    ))
  })
  
  # 显示参考文献
  output$reference_list <- renderUI({
    req(rv$reference_list)
    
    # 将文本转换为HTML格式
    html_ref <- gsub("\n", "<br>", rv$reference_list)
    html_ref <- gsub("(\\[\\d+\\])", '<strong>\\1</strong>', html_ref)
    
    HTML(paste0(
      '<div style="font-family: \'Microsoft YaHei\', Arial; line-height: 1.8; padding-left: 20px;">',
      '<h4>参考文献</h4>',
      html_ref,
      '</div>'
    ))
  })
  
  # 显示生成过程
  output$review_progress <- renderText({
    rv$review_progress_log
  })
  
  # 下载按钮
  output$download_review_btns <- renderUI({
    req(rv$review_result)
    tagList(
      downloadButton("download_review_txt", "下载综述(TXT)", 
                     class = "btn-success", style = "width: 100%; margin-bottom: 5px;"),
      downloadButton("download_review_docx", "下载综述(DOCX)", 
                     class = "btn-info", style = "width: 100%; margin-bottom: 5px;"),
      downloadButton("download_review_refs", "下载参考文献", 
                     class = "btn-warning", style = "width: 100%;")
    )
  })
  
  # 下载TXT
  output$download_review_txt <- downloadHandler(
    filename = function() {
      paste0("文献综述_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      content <- paste(
        "=== 文献综述 ===\n\n",
        rv$review_result,
        "\n\n=== 参考文献 ===\n",
        rv$reference_list,
        sep = ""
      )
      writeLines(content, file, useBytes = TRUE)
      show_notification("综述下载成功！", "message")
    }
  )
  
  # 下载DOCX (使用writexl作为替代，实际DOCX需要更复杂的处理)
  output$download_review_docx <- downloadHandler(
    filename = function() {
      paste0("文献综述_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      # 创建简单的DOCX格式（实际为RTF）
      content <- paste0(
        "{\\rtf1\\ansi\\deff0\n",
        "{\\fonttbl{\\f0\\fnil\\fcharset134 \\'cb\\'ce\\'cc\\'e5;}}\n",
        "\\viewkind4\\uc1\\pard\\lang2052\\f0\\fs24\n",
        gsub("\n", "\\par\n", paste("文献综述\n\n", rv$review_result, "\n\n参考文献\n", rv$reference_list)),
        "\n}"
      )
      writeLines(content, file, useBytes = TRUE)
      show_notification("综述下载成功！", "message")
    }
  )
  
  # 下载参考文献
  output$download_review_refs <- downloadHandler(
    filename = function() {
      paste0("参考文献_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      content <- paste(
        "=== 参考文献列表 ===\n\n",
        rv$reference_list,
        sep = ""
      )
      writeLines(content, file, useBytes = TRUE)
      show_notification("参考文献下载成功！", "message")
    }
  )
  
  
  
  
  
  
  # ==================== 重置功能 ====================
  
  observeEvent(input$reset_all, {
    rv$raw_data_search <- NULL
    rv$filtered_search_data <- NULL
    rv$raw_data_trans <- NULL
    rv$translated_data <- NULL
    rv$processed_data <- NULL
    rv$translated_processed_data <- NULL  # 新增：重置翻译处理数据
    rv$segmented_data <- NULL
    rv$word_freq <- NULL
    rv$keyword_trend <- NULL
    rv$trend_metrics <- NULL
    rv$segment_log <- ""
    rv$wordcloud_obj <- NULL
    show_notification("已重置所有数据", "message")
  })
}

  # ==========================================
  # 3. 启动应用
  # ==========================================
  shinyApp(ui = ui, server = server)
}