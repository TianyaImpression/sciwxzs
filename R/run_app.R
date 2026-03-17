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
#' @importFrom dplyr mutate filter select summarise group_by ungroup arrange pull n row_number case_when any_of rename bind_rows across everything count top_n left_join slice slice_max
#' @importFrom tibble tibble
#' @importFrom tidyr complete unnest
#' @importFrom purrr map_int map_chr
#' @importFrom lubridate year
#' @importFrom scales percent_format
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#' @importFrom readr write_csv write_rds
#' @importFrom stringr str_split
#' @export
run_sciwxzs <- function() {

  # 尝试加载 PDF 字体，以防用户导出词云图或趋势图时字体缺失
  tryCatch({
    extrafont::loadfonts(device = "pdf", quiet = TRUE)
  }, error = function(e) {
    warning("字体加载失败，PDF 图表可能无法正确显示中文字体。您可以尝试运行 init_sciwxzs_fonts()。")
  })
  # ==========================================
  # 1. UI 定义
  # ==========================================
# UI定义
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SCI文献分析与综述系统", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("数据上传与筛选", tabName = "search", icon = icon("search")),
      menuItem("摘要翻译", tabName = "translate", icon = icon("language")),
      menuItem("API配置", tabName = "api", icon = icon("key"), selected = TRUE),
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
      numericInput("max_tokens", "Max Tokens:", 8000, min = 100, max = 8000),
      numericInput("delay_seconds", "API延迟(秒):", 0.1, min = 0.1, max = 5, step = 0.1),
      numericInput("top_n_words", "显示词数:", 100, min = 10, max = 500),
      checkboxInput("test_mode", "测试模式(仅前10条)", FALSE),
      actionButton("reset_all", "重置所有", class = "btn-warning", style = "width: 80%; margin-top: 10px;")
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
        .api-status-box {
          background-color: #f8f9fa;
          border-left: 4px solid #3c8dbc;
          padding: 10px;
          margin-bottom: 15px;
        }
        .api-status-valid {
          color: #00a65a;
          font-weight: bold;
        }
        .api-status-invalid {
          color: #dd4b39;
          font-weight: bold;
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
                  # API状态提示
                  uiOutput("api_status_indicator_translate"),
                  hr(),
                  
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
                  numericInput(
                    inputId = "max_tokens_trans",
                    label = "最大Token数",
                    value = 8000,
                    min = 500,
                    max = 8192,
                    step = 500
                  ),
                  numericInput(
                    inputId = "delay_seconds_trans",
                    label = "请求间隔(秒)",
                    value = 0.2,
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
                             uiOutput("progress_script_ui"),
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
                  passwordInput("api_key_config", "API密钥:", value = "",
                                placeholder = "输入sk-开头的DeepSeek API密钥"),
                  helpText("请输入您的DeepSeek API密钥。密钥仅在当前会话中保存，所有模块将共用此密钥。"),
                  hr(),
                  selectInput("model", "选择模型:", 
                              choices = c("deepseek-chat", "deepseek-reasoner"),
                              selected = "deepseek-chat"),
                  numericInput("timeout", "请求超时(秒):", 30, min = 10, max = 120),
                  actionButton("save_api_key", "保存API密钥", class = "btn-success", style = "width: 100%;"),
                  actionButton("test_api", "测试连接", class = "btn-info", style = "width: 100%; margin-top: 10px;"),
                  hr(),
                  h4("API状态"),
                  uiOutput("api_status_detailed")
                ),
                box(
                  title = "使用说明", status = "info", solidHeader = TRUE, width = 6,
                  HTML("
        <h4>API配置说明</h4>
        <ul>
          <li><b>API密钥获取：</b> 访问 <a href='https://platform.deepseek.com/' target='_blank'>DeepSeek官网</a> 注册获取</li>
          <li><b>密钥格式：</b> 以 <code>sk-</code> 开头</li>
          <li><b>安全性：</b> 密钥仅保存在当前会话内存中，不会存储到磁盘</li>
          <li><b>模型选择：</b> deepseek-chat 适合翻译和分词，deepseek-reasoner 适合综述、技术分析</li>
        </ul>
        <h4>模块使用说明</h4>
        <ul>
          <li><b>摘要翻译：</b> 使用配置的API密钥进行批量翻译</li>
          <li><b>分词处理：</b> 使用API进行中文分词</li>
          <li><b>文献综述：</b> 基于API生成学术综述</li>
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
                  # API状态提示
                  uiOutput("api_status_indicator_segment"),
                  hr(),
                  
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
      
      # ==================== 5. 词频分析页面（优化PDF导出）====================
      tabItem(tabName = "wordfreq",
              fluidRow(
                box(
                  title = "数据源选择", status = "primary", solidHeader = TRUE, width = 12,
                  column(6,
                         radioButtons("freq_data_source", "选择数据来源:",
                                      choices = list(
                                        "使用分词模块结果" = "segment",
                                        "上传新的分词结果" = "upload"
                                      ),
                                      selected = "segment",
                                      inline = TRUE)
                  ),
                  column(6,
                         conditionalPanel(
                           condition = "input.freq_data_source == 'upload'",
                           fileInput("freq_upload_file", "上传分词结果Excel文件",
                                     accept = c(".xlsx", ".xls"),
                                     width = "100%"),
                           tags$small("文件需包含: doc_id, PY, segmented_words, word_count 列")
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "词频统计设置", status = "primary", solidHeader = TRUE, width = 3,
                  numericInput("min_freq", "最小词频:", 2, min = 1),
                  numericInput("min_word_length", "最小词长:", 2, min = 1),
                  selectInput("stopwords_lang", "停用词库:", 
                              choices = c("中文", "英文", "无"),
                              selected = "中文"),
                  hr(),
                  h4("自定义停用词"),
                  textAreaInput("custom_stopwords", "输入自定义停用词（用逗号分隔）",
                                rows = 3,
                                placeholder = "例如：研究,分析,方法,结果"),
                  actionButton("refresh_freq", "刷新统计", class = "btn-primary", 
                               style = "width: 100%;"),
                  br(), br(),
                  actionButton("sync_to_other_modules", "同步数据到其他分析模块", 
                               class = "btn-info", style = "width: 100%;"),
                  hr(),
                  h4("数据导出"),
                  downloadButton("download_freq_data", "下载高频词汇表(Excel)", 
                                 class = "btn-info", style = "width: 100%; margin-bottom: 5px;"),
                  tags$div(class = "custom-select",
                           selectInput("freq_plot_format", "词频图导出格式:", 
                                       choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png"),
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
                  title = "数据统计", status = "warning", solidHeader = TRUE, width = 12,
                  column(3, valueBoxOutput("freq_total_docs", width = 12)),
                  column(3, valueBoxOutput("freq_total_words", width = 12)),
                  column(3, valueBoxOutput("freq_unique_words", width = 12)),
                  column(3, valueBoxOutput("freq_year_range", width = 12))
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

      # ==================== 7. 热力图页面（优化PDF导出）====================
      tabItem(tabName = "heatmap",
              fluidRow(
                box(
                  title = "热力图设置", status = "primary", solidHeader = TRUE, width = 3,
                  numericInput("heatmap_words", "显示词数:", 30, min = 5, max = 100),
                  selectInput("color_scale", "配色方案:",
                              choices = c(
                                "红-绿" = "red-green",
                                "蓝-黄-红" = "blue-yellow-red",
                                "紫-黄" = "purple-yellow",
                                "热力图" = "heat"
                              ),
                              selected = "red-green"),
                  checkboxInput("cluster_rows", "对行聚类", FALSE),
                  checkboxInput("show_numbers", "显示数值", TRUE),
                  actionButton("plot_heatmap", "生成热力图", class = "btn-primary"),
                  hr(),
                  h4("图表导出"),
                  tags$div(class = "custom-select",
                           selectInput("heatmap_plot_format", "热力图导出格式:", 
                                       choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png"),
                                       selected = "pdf")
                  ),
                  downloadButton("download_heatmap_plot", "下载时间分布热力图", 
                                 class = "btn-info", style = "width: 100%;")
                ),
                box(
                  title = "时间分布热力图", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(plotOutput("heatmap_plot", height = 650), type = 4)
                )
              )
      ),
      
      # ==================== 8. 气泡图页面（优化PDF导出，优化趋势分析）====================
      tabItem(tabName = "bubble",
              fluidRow(
                box(
                  title = "气泡图设置", status = "primary", solidHeader = TRUE, width = 3,
                  numericInput("bubble_top_n", "分析关键词数量:", 450, min = 10, max = 1000),
                  numericInput("bubble_min_freq", "最小出现次数:", 5, min = 1),
                  numericInput("bubble_min_total", "最小总频次:", 5, min = 1),
                  selectInput("bubble_color", "配色:",
                              choices = c("Set1", "Set2", "Set3", "Dark2"),
                              selected = "Set2"),
                  sliderInput("bubble_alpha", "透明度:", 0.7, min = 0.3, max = 1, step = 0.1),
                  sliderInput("bubble_label_size", "标签大小:", 3.5, min = 2, max = 6, step = 0.1),
                  checkboxInput("bubble_show_all", "显示所有关键词（不设限）", FALSE),
                  actionButton("plot_bubble", "生成气泡图", class = "btn-primary"),
                  hr(),
                  h4("X轴刻度设置"),
                  radioButtons("bubble_x_axis", "X轴刻度间隔:",
                               choices = list(
                                 "每年" = "1",
                                 "每2年" = "2",
                                 "每3年" = "3",
                                 "每5年" = "5",
                                 "自动" = "auto"
                               ),
                               selected = "1"),
                  hr(),
                  h4("数据导出"),
                  downloadButton("download_trend_metrics", "下载趋势统计数据(Excel)", 
                                 class = "btn-info", style = "width: 100%; margin-bottom: 5px;"),
                  tags$div(class = "custom-select",
                           selectInput("bubble_plot_format", "气泡图导出格式:", 
                                       choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png"),
                                       selected = "pdf")
                  ),
                  downloadButton("download_bubble_plot", "下载关键词趋势气泡图", 
                                 class = "btn-info", style = "width: 100%;")
                ),
                box(
                  title = "关键词趋势气泡图", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(plotOutput("bubble_plot", height = 600), type = 4)
                )
              ),
              fluidRow(
                box(
                  title = "趋势统计", status = "success", solidHeader = TRUE, width = 12,
                  withSpinner(DTOutput("bubble_metrics_table"), type = 4)
                )
              ),
              fluidRow(
                box(
                  title = "统计信息", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("bubble_stats")
                )
              )
      ),
      
      # ==================== 9. 词云图页面（优化PDF导出）====================
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
                  
                  # API状态提示
                  uiOutput("api_status_indicator_review"),
                  hr(),
                  
                  # 数据来源选择
                  radioButtons(
                    inputId = "review_data_source",
                    label = "数据来源",
                    choices = list(
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
                      "国内外研究现状" = "domestic_international"
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

# Server逻辑
server <- function(input, output, session) {
  
# 反应式值存储
  rv <- reactiveValues(
    # API配置
    api_key = NULL,                     # 全局API密钥
    api_valid = FALSE,                   # API是否有效
    
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
    review_progress_log = "",
    
    # 新增：存储当前使用的分词数据源
    current_freq_data = NULL
  )
  
  # 修复点1：重写通知函数，仅使用合法的type参数
  show_notification <- function(message, type = "message") {
    valid_types <- c("message", "warning", "error")
    type <- ifelse(type %in% valid_types, type, "message")
    showNotification(message, type = type, duration = 3)
  }
  
  # ========== API配置模块：保存和验证API密钥 ==========
  
  # 保存API密钥
  observeEvent(input$save_api_key, {
    api_key <- trimws(input$api_key_config)
    
    if (api_key == "") {
      show_notification("请输入API密钥！", "warning")
      rv$api_valid <- FALSE
      return()
    }
    
    if (!grepl("^sk-", api_key)) {
      show_notification("API密钥格式错误：应以'sk-'开头！", "error")
      rv$api_valid <- FALSE
      return()
    }
    
    rv$api_key <- api_key
    rv$api_valid <- TRUE
    show_notification("API密钥已保存！", "message")
  })
  
  # 测试API连接
  observeEvent(input$test_api, {
    if (is.null(rv$api_key)) {
      show_notification("请先保存API密钥！", "warning")
      return()
    }
    
    rv$api_test_result <- "正在测试连接..."
    
    # 简单的测试调用
    test_result <- tryCatch({
      api_url <- "https://api.deepseek.com/chat/completions"
      
      request_body <- list(
        model = "deepseek-chat",
        messages = list(
          list(role = "user", content = "Hello")
        ),
        max_tokens = 5
      )
      
      headers <- add_headers(
        "Authorization" = paste("Bearer", rv$api_key),
        "Content-Type" = "application/json"
      )
      
      response <- POST(
        url = api_url,
        headers,
        body = toJSON(request_body, auto_unbox = TRUE),
        timeout(10)
      )
      
      if (status_code(response) == 200) {
        "连接成功！API密钥有效。"
      } else {
        paste("连接失败：HTTP", status_code(response))
      }
    }, error = function(e) {
      paste("连接错误：", e$message)
    })
    
    rv$api_test_result <- test_result
    rv$api_valid <- grepl("成功", test_result)
    
    if (rv$api_valid) {
      show_notification("API连接测试成功！", "success")
    } else {
      show_notification("API连接测试失败，请检查密钥和网络", "error")
    }
  })
  
  # 显示API状态（详细版）
  output$api_status_detailed <- renderUI({
    if (is.null(rv$api_key)) {
      div(class = "api-status-box",
          h5("API状态：", span("未配置", class = "api-status-invalid")),
          p("请在上方输入并保存您的DeepSeek API密钥")
      )
    } else if (!rv$api_valid) {
      div(class = "api-status-box",
          h5("API状态：", span("无效", class = "api-status-invalid")),
          p("密钥格式可能不正确或已失效，请重新配置")
      )
    } else {
      div(class = "api-status-box",
          h5("API状态：", span("已配置并有效 ✓", class = "api-status-valid")),
          p("模型：", input$model),
          p("超时：", input$timeout, "秒"),
          p("最后测试：", if (!is.null(rv$api_test_result) && !grepl("测试", rv$api_test_result)) {
            format(Sys.time(), "%H:%M:%S")
          } else {
            "未测试"
          }),
          p("测试结果：", rv$api_test_result %||% "未测试")
      )
    }
  })
  
  # API状态指示器 - 翻译模块
  output$api_status_indicator_translate <- renderUI({
    if (is.null(rv$api_key)) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"), 
          "请先在", 
          actionLink("go_to_api", "API配置页面", onclick = "Shiny.setInputValue('tab_selected', 'api', {priority: 'event'})"),
          "配置API密钥"
      )
    } else if (!rv$api_valid) {
      div(class = "alert alert-danger",
          icon("times-circle"), 
          "API密钥无效，请检查并重新配置"
      )
    } else {
      div(class = "alert alert-success",
          icon("check-circle"), 
          "API已就绪"
      )
    }
  })
  
  # API状态指示器 - 分词模块
  output$api_status_indicator_segment <- renderUI({
    if (is.null(rv$api_key)) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"), 
          "请先在API配置页面设置密钥"
      )
    } else if (!rv$api_valid) {
      div(class = "alert alert-danger",
          icon("times-circle"), 
          "API密钥无效"
      )
    } else {
      div(class = "alert alert-success",
          icon("check-circle"), 
          "API已就绪"
      )
    }
  })
  
  # API状态指示器 - 综述模块
  output$api_status_indicator_review <- renderUI({
    if (is.null(rv$api_key)) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"), 
          "请先在API配置页面设置密钥"
      )
    } else if (!rv$api_valid) {
      div(class = "alert alert-danger",
          icon("times-circle"), 
          "API密钥无效"
      )
    } else {
      div(class = "alert alert-success",
          icon("check-circle"), 
          "API已就绪"
      )
    }
  })
  
  # 标签页切换处理
  observeEvent(input$go_to_api, {
    updateTabItems(session, "sidebarMenu", "api")
  })
  
  observeEvent(input$tab_selected, {
    if (input$tab_selected == "api") {
      updateTabItems(session, "sidebarMenu", "api")
    }
  })
  
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
  
  # 读取翻译文件 - 修复版本
  observeEvent(input$file_translate, {
    if (input$data_source == "upload") {
      req(input$file_translate)
      tryCatch({
        # 读取Excel文件
        raw_data <- read_excel(input$file_translate$datapath, sheet = 1, col_names = TRUE)
        
        # 将所有列转换为字符类型
        for (i in 1:ncol(raw_data)) {
          raw_data[[i]] <- as.character(raw_data[[i]])
        }
        raw_data[is.na(raw_data)] <- ""
        
        # 检查是否包含必要的列
        required_cols <- c("AB", "PY")  # 至少需要摘要和年份
        if (!all(required_cols %in% names(raw_data))) {
          missing_cols <- required_cols[!required_cols %in% names(raw_data)]
          show_notification(paste("上传的文件中缺少以下必要列：", 
                                  paste(missing_cols, collapse = ", ")), "error")
          rv$raw_data_trans <- NULL
          return()
        }
        
        # 保存原始翻译数据
        rv$raw_data_trans <- raw_data
        
        # 创建用于分词处理的数据结构
        processed_df <- raw_data %>%
          mutate(
            doc_id = row_number(),
            PY = as.numeric(PY),
            # 如果存在TI列则保留，否则创建空列
            TI = if ("TI" %in% names(.)) as.character(TI) else "",
            # 如果存在DE列则保留，否则创建空列
            DE = if ("DE" %in% names(.)) as.character(DE) else "",
            # 如果存在AU列则保留，否则创建空列
            AU = if ("AU" %in% names(.)) as.character(AU) else "",
            # 初始化中文摘要列为空
            ABCN = NA_character_
          ) %>%
          filter(!is.na(PY), PY >= 1900, PY <= year(Sys.Date())) %>%
          select(doc_id, TI, AU, PY, DE, AB, ABCN, everything())
        
        # 保存到processed_data
        if (is.null(rv$processed_data)) {
          rv$processed_data <- processed_df
        } else {
          # 如果已有数据，追加新数据
          max_doc_id <- max(rv$processed_data$doc_id, na.rm = TRUE)
          processed_df$doc_id <- processed_df$doc_id + max_doc_id
          rv$processed_data <- bind_rows(rv$processed_data, processed_df)
        }
        
        # 初始化翻译后的处理数据
        rv$translated_processed_data <- processed_df
        
        show_notification(paste("成功加载数据：共", nrow(raw_data), "条记录"), "message")
        
      }, error = function(e) {
        show_notification(paste("读取文件出错：", e$message), "error")
        rv$raw_data_trans <- NULL
      })
    }
  })
  
  # 获取翻译数据 - 修复版本
  get_translation_data <- reactive({
    if (is.null(input$data_source)) return(NULL)
    
    if (input$data_source == "search_result") {
      req(rv$filtered_search_data)
      if (!"AB" %in% colnames(rv$filtered_search_data)) {
        show_notification("检索结果中未找到'AB'列，无法翻译！", "error")
        return(NULL)
      }
      data <- rv$filtered_search_data
      # 确保所有列为字符类型
      data <- data %>%
        mutate(across(everything(), as.character))
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
  
  # 核心翻译逻辑 - 修复版本
  observeEvent(input$run_translate, {
    # 检查API密钥
    if (is.null(rv$api_key) || !rv$api_valid) {
      show_notification("请先在API配置页面设置有效的API密钥！", "error")
      return()
    }
    
    translation_data <- get_translation_data()
    if (is.null(translation_data)) {
      show_notification("没有可翻译的数据，请先上传文件或进行检索筛选", "warning")
      return()
    }
    
    # 检查AB列是否有内容
    if (sum(!is.na(translation_data$AB) & trimws(translation_data$AB) != "") == 0) {
      show_notification("没有找到非空的英文摘要（AB列）", "warning")
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
      
      # 获取当前摘要文本
      current_ab <- translation_data$AB[i]
      
      # 检查是否为空摘要
      if (is.na(current_ab) || trimws(current_ab) == "") {
        translations[i] <- ""
        # 更新进度
        rv$current_progress <- round((i / total_records) * 100, 1)
        session$sendCustomMessage("updateProgress", list(progress = rv$current_progress))
        
        # 更新状态显示
        if (i %% 5 == 0 || i == total_records) {
          output$trans_status <- renderPrint({
            cat(sprintf("正在处理：%d/%d 条 (%.1f%%) - 跳过空摘要\n", 
                        i, total_records, rv$current_progress))
            success_sofar <- sum(!grepl("失败", translations[1:i]) & translations[1:i] != "", na.rm = TRUE)
            empty_sofar <- sum(translations[1:i] == "", na.rm = TRUE)
            cat(sprintf("当前成功：%d 条，跳过空摘要：%d 条\n", success_sofar, empty_sofar))
          })
        }
        
        Sys.sleep(delay_seconds/2)  # 空摘要时等待时间减半
        next
      }
      
      # 调用翻译函数
      translations[i] <- translate_with_deepseek(
        text = current_ab,
        api_key = rv$api_key,
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
      if (i %% 5 == 0 || i == total_records) {
        output$trans_status <- renderPrint({
          cat(sprintf("正在处理：%d/%d 条 (%.1f%%)\n", i, total_records, rv$current_progress))
          success_sofar <- sum(!grepl("失败", translations[1:i]) & translations[1:i] != "", na.rm = TRUE)
          fail_sofar <- sum(grepl("失败", translations[1:i]), na.rm = TRUE)
          empty_sofar <- sum(translations[1:i] == "", na.rm = TRUE)
          cat(sprintf("当前成功：%d 条，失败：%d 条，空摘要：%d 条\n", 
                      success_sofar, fail_sofar, empty_sofar))
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
          cn_length = nchar(trimws(as.character(ABCN_trans))),
          translation_status = case_when(
            is.na(AB) | trimws(AB) == "" ~ "空摘要",
            grepl("失败", ABCN_trans) ~ "翻译失败",
            ABCN_trans != "" ~ "成功",
            TRUE ~ "未知"
          )
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
          if (i <= nrow(rv$processed_data)) {
            rv$processed_data$ABCN[i] <- translations[i]
          }
        }
      }
      
      # 最终状态显示
      output$trans_status <- renderPrint({
        translations_char <- as.character(translations)
        success_count <- sum(!grepl("失败", translations_char) & translations_char != "", na.rm = TRUE)
        fail_count <- sum(grepl("失败", translations_char), na.rm = TRUE)
        empty_count <- sum(is.na(translation_data$AB) | trimws(translation_data$AB) == "", na.rm = TRUE)
        
        cat("翻译任务完成！\n")
        cat(sprintf("总记录数：%d 条\n", total_records))
        cat(sprintf("成功翻译：%d 条\n", success_count))
        cat(sprintf("翻译失败：%d 条\n", fail_count))
        cat(sprintf("空摘要：%d 条\n", empty_count))
      })
      
      session$sendCustomMessage("updateProgress", list(progress = 100))
      
      output$preview_table <- renderTable({
        req(rv$translated_data)
        rv$translated_data %>%
          select(AB, ABCN_trans, translation_status) %>%
          head(5) %>%
          mutate(
            AB = ifelse(nchar(as.character(AB)) > 80, 
                        paste0(substr(as.character(AB), 1, 80), "..."), 
                        as.character(AB)),
            ABCN_trans = ifelse(nchar(as.character(ABCN_trans)) > 80, 
                                paste0(substr(as.character(ABCN_trans), 1, 80), "..."), 
                                as.character(ABCN_trans))
          ) %>%
          rename("英文摘要" = AB, "中文翻译" = ABCN_trans, "状态" = translation_status)
      }, striped = TRUE, hover = TRUE)
      
      output$preview_table_full <- renderTable({
        req(rv$translated_data)
        rv$translated_data %>%
          select(AB, ABCN_trans, translation_status) %>%
          head(10) %>%
          mutate(
            AB = ifelse(nchar(as.character(AB)) > 100, 
                        paste0(substr(as.character(AB), 1, 100), "..."), 
                        as.character(AB)),
            ABCN_trans = ifelse(nchar(as.character(ABCN_trans)) > 100, 
                                paste0(substr(as.character(ABCN_trans), 1, 100), "..."), 
                                as.character(ABCN_trans))
          ) %>%
          rename("英文摘要" = AB, "中文翻译" = ABCN_trans, "状态" = translation_status)
      }, striped = TRUE, hover = TRUE)
      
      output$trans_stats <- renderPrint({
        req(rv$translated_data)
        stats_data <- rv$translated_data %>%
          summarise(
            数据来源 = ifelse(input$data_source == "search_result", "检索模块结果", "上传文件"),
            总记录数 = n(),
            空英文摘要数 = sum(is.na(AB) | trimws(AB) == "", na.rm = TRUE),
            成功翻译数 = sum(translation_status == "成功", na.rm = TRUE),
            翻译失败数 = sum(translation_status == "翻译失败", na.rm = TRUE),
            翻译成功率 = ifelse((总记录数 - 空英文摘要数) > 0, 
                           round(成功翻译数 / (总记录数 - 空英文摘要数) * 100, 1), 
                           0),
            平均英文摘要长度 = round(mean(en_length, na.rm = TRUE), 1),
            平均中文摘要长度 = round(mean(cn_length[cn_length > 0], na.rm = TRUE), 1)
          )
        print(stats_data)
      })
      
      output$download_trans_btn <- renderUI({
        downloadButton("download_trans_result", "下载翻译后的数据", class = "btn-success", style = "width: 100%;")
      })
      
      show_notification("翻译完成！", "message")
    }
  })
  
  # 下载翻译结果 - 修复版本
  output$download_trans_result <- downloadHandler(
    filename = function() {
      paste0("翻译结果_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$translated_data)
      output_data <- rv$translated_data %>%
        select(-any_of(c("en_length", "cn_length", "translation_status")))
      write_xlsx(output_data, file)
      show_notification("翻译结果导出成功！", "message")
    }
  )
  
  # ==================== 分词处理模块 ====================
  
  observeEvent(input$start_segment, {
    # 检查API密钥
    if (is.null(rv$api_key) || !rv$api_valid) {
      show_notification("请先在API配置页面设置有效的API密钥！", "error")
      return()
    }
    
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
          rv$api_key,  # 使用全局API密钥
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
      
      # 保存当前数据源
      rv$current_freq_data <- rv$segmented_data
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
  
  # ==================== 词频分析模块 - 数据上传和处理（同步到其他模块）====================
  
  # 存储上传的词频数据
  freq_uploaded_data <- reactiveVal(NULL)
  
  # 处理上传的文件
  observeEvent(input$freq_upload_file, {
    req(input$freq_upload_file)
    
    tryCatch({
      # 读取上传的Excel文件
      uploaded <- read_excel(input$freq_upload_file$datapath, sheet = 1)
      
      # 检查必要的列
      required_cols <- c("doc_id", "PY", "segmented_words", "word_count")
      missing_cols <- required_cols[!required_cols %in% names(uploaded)]
      
      if (length(missing_cols) > 0) {
        show_notification(paste("上传的文件缺少以下必要列:", 
                                paste(missing_cols, collapse = ", ")), "error")
        return()
      }
      
      # 数据格式转换
      uploaded <- uploaded %>%
        mutate(
          doc_id = as.character(doc_id),
          PY = as.numeric(PY),
          word_count = as.numeric(word_count),
          # 将逗号分隔的字符串转换为列表
          segmented_words = str_split(segmented_words, ",\\s*|;\\s*|\\s+")
        ) %>%
        filter(!is.na(PY), PY >= 1900, PY <= year(Sys.Date()), word_count > 0)
      
      freq_uploaded_data(uploaded)
      rv$current_freq_data <- uploaded
      
      show_notification(paste("成功加载", nrow(uploaded), "条分词记录"), "success")
      
      # 自动刷新统计
      delay(500, {
        session$sendCustomMessage("refresh_freq", list())
      })
      
    }, error = function(e) {
      show_notification(paste("文件读取失败:", e$message), "error")
      freq_uploaded_data(NULL)
      rv$current_freq_data <- NULL
    })
  })
  
  # 获取当前词频分析的数据源
  get_freq_data <- reactive({
    if (input$freq_data_source == "upload") {
      req(freq_uploaded_data())
      return(freq_uploaded_data())
    } else {
      req(rv$segmented_data)
      return(rv$segmented_data)
    }
  })
  
  # 更新词频统计数据并同步到其他模块
  observeEvent(input$refresh_freq, {
    data <- get_freq_data()
    req(data)
    
    withProgress(message = '正在统计词频...', value = 0.5, {
      
      # 获取所有词汇
      all_words <- unlist(data$segmented_words)
      
      if (length(all_words) == 0) {
        show_notification("未找到任何词汇", "warning")
        return()
      }
      
      # 应用最小词长过滤
      all_words <- all_words[nchar(all_words) >= input$min_word_length]
      
      # 停用词处理
      stopwords_combined <- character(0)
      
      # 添加内置停用词
      if (input$stopwords_lang == "中文") {
        stopwords_combined <- c(stopwords_combined, 
                                c("的", "和", "与", "及", "在", "是", "了", "对", "于", "中",
                                  "地", "得", "有", "用", "以", "为", "并", "或", "等", "将",
                                  "从", "到", "通过", "使用", "可以", "这种", "一个", "一种",
                                  "包括", "具有", "进行", "研究", "分析", "基于", "方法", "数据",
                                  "结果", "表明", "显示", "我们", "他们", "它们", "这些",
                                  "那些", "这个", "那个"))
      } else if (input$stopwords_lang == "英文") {
        stopwords_combined <- c(stopwords_combined,
                                c("the", "and", "for", "with", "this", "that", "from",
                                  "were", "have", "been", "was", "are", "has", "had",
                                  "will", "can", "may", "also", "than", "then", "thus"))
      }
      
      # 添加自定义停用词
      if (input$custom_stopwords != "") {
        custom <- strsplit(input$custom_stopwords, ",")[[1]]
        custom <- trimws(custom)
        custom <- custom[custom != ""]
        stopwords_combined <- c(stopwords_combined, custom)
      }
      
      # 去除停用词
      all_words <- all_words[!all_words %in% stopwords_combined]
      
      # 统计词频
      word_freq <- tibble(word = all_words) %>%
        count(word, sort = TRUE) %>%
        mutate(
          weight = log(n + 1),
          rank = row_number()
        ) %>%
        filter(n >= input$min_freq)
      
      # 更新反应值 - 这些是其他模块依赖的数据
      rv$word_freq <- word_freq
      rv$current_freq_data <- data
      
      # 更新 keyword_trend 用于其他模块
      rv$keyword_trend <- data %>%
        select(doc_id, PY, segmented_words) %>%
        unnest(segmented_words) %>%
        rename(word = segmented_words) %>%
        group_by(PY, word) %>%
        summarise(count = n(), .groups = "drop")
      
      # 更新趋势指标用于气泡图
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
      
      # 更新时间趋势模块的关键词选择器
      updateSelectizeInput(session, "trend_keywords", 
                           choices = rv$word_freq$word,
                           selected = head(rv$word_freq$word, 5))
      
      show_notification("词频统计完成！数据已同步到其他分析模块", "success")
    })
  })
  
  # 手动同步数据到其他模块的按钮
  observeEvent(input$sync_to_other_modules, {
    if (!is.null(rv$current_freq_data) && !is.null(rv$word_freq)) {
      # 重新计算 keyword_trend
      rv$keyword_trend <- rv$current_freq_data %>%
        select(doc_id, PY, segmented_words) %>%
        unnest(segmented_words) %>%
        rename(word = segmented_words) %>%
        group_by(PY, word) %>%
        summarise(count = n(), .groups = "drop")
      
      # 重新计算趋势指标
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
      
      # 更新时间趋势模块的关键词选择器
      updateSelectizeInput(session, "trend_keywords", 
                           choices = rv$word_freq$word,
                           selected = head(rv$word_freq$word, 5))
      
      show_notification("数据已成功同步到时间趋势、热力图、气泡图、词云图模块！", "success")
    } else {
      show_notification("请先刷新词频统计后再同步数据！", "warning")
    }
  })
  
  # 数据统计信息
  output$freq_total_docs <- renderValueBox({
    data <- get_freq_data()
    valueBox(
      nrow(data) %||% 0,
      "文档总数",
      icon = icon("file-alt"),
      color = "blue"
    )
  })
  
  output$freq_total_words <- renderValueBox({
    data <- get_freq_data()
    total <- sum(data$word_count) %||% 0
    valueBox(
      total,
      "总词汇数",
      icon = icon("font"),
      color = "green"
    )
  })
  
  output$freq_unique_words <- renderValueBox({
    req(rv$word_freq)
    valueBox(
      nrow(rv$word_freq),
      "唯一词汇数",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$freq_year_range <- renderValueBox({
    data <- get_freq_data()
    years <- range(data$PY, na.rm = TRUE)
    valueBox(
      paste(years[1], "-", years[2]),
      "年份范围",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  # 词频分布图
  output$freq_plot <- renderPlot({
    req(rv$word_freq)
    input$refresh_freq
    
    data <- rv$word_freq %>%
      head(input$top_n_words)
    
    if (nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "无符合条件的词汇"), 
                  size = 12, family = "FangSong") + 
        theme_void()
    } else {
      ggplot(data, aes(x = reorder(word, n), y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
        labs(x = "词汇", y = "频次", title = "高频词汇分布") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "FangSong"),
          axis.text = element_text(family = "FangSong", size = 10),
          axis.title = element_text(family = "FangSong", size = 12),
          legend.position = "none"
        )
    }
  })
  
  # 高频词汇表
  output$freq_table <- renderDT({
    req(rv$word_freq)
    
    datatable(
      rv$word_freq %>%
        head(500) %>%
        mutate(
          word = word,
          n = n,
          累计频率 = cumsum(n) / sum(n) * 100,
          累计频率 = round(累计频率, 2)
        ) %>%
        rename(
          "关键词" = word,
          "频次" = n,
          "累计频率(%)" = 累计频率
        ),
      options = list(
        pageLength = 20,
        order = list(list(1, 'desc')),
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "频次",
        background = styleColorBar(rv$word_freq$n[1:500], "lightblue"),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # 下载高频词汇表
  output$download_freq_data <- downloadHandler(
    filename = function() {
      paste0("高频词汇表_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$word_freq)
      
      export_data <- rv$word_freq %>%
        head(1000) %>%
        mutate(
          累计频率 = cumsum(n) / sum(n) * 100,
          累计频率 = round(累计频率, 2)
        ) %>%
        rename(
          词汇 = word,
          频次 = n,
          权重 = weight,
          排名 = rank,
          累计频率百分比 = 累计频率
        )
      
      write_xlsx(export_data, file)
      show_notification("高频词汇表导出成功！", "message")
    }
  )
  
  # 下载词频分布图
  output$download_freq_plot <- downloadHandler(
    filename = function() {
      paste0("词频分布图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$freq_plot_format)
    },
    content = function(file) {
      req(rv$word_freq)
      
      plot_data <- rv$word_freq %>%
        head(input$top_n_words)
      
      p <- ggplot(plot_data, aes(x = reorder(word, n), y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
        labs(x = "词汇", y = "频次", title = "高频词汇分布") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "FangSong"),
          axis.text = element_text(family = "FangSong", size = 10),
          axis.title = element_text(family = "FangSong", size = 12),
          legend.position = "none"
        )
      
      # 根据格式保存
      if (input$freq_plot_format == "pdf") {
        ggsave(file, plot = p, device = cairo_pdf, width = 12, height = 8, dpi = 300, bg = "white")
      } else if (input$freq_plot_format == "png") {
        ggsave(file, plot = p, device = "png", width = 12, height = 8, dpi = 300, bg = "white")
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 12, height = 8, dpi = 300, bg = "white")
      }
      
      show_notification("词频分布图导出成功！", "message")
    }
  )
  
  # 重置上传数据当切换数据源时
  observeEvent(input$freq_data_source, {
    if (input$freq_data_source == "segment") {
      # 切换回分词模块数据时，如果segmented_data存在，自动刷新统计
      if (!is.null(rv$segmented_data)) {
        rv$current_freq_data <- rv$segmented_data
        delay(500, {
          session$sendCustomMessage("refresh_freq", list())
        })
      }
    }
  })
  
  # ==================== 时间趋势模块 ====================
  
  output$trend_plot <- renderPlot({
    req(rv$keyword_trend)
    input$plot_trend
    
    if (is.null(input$trend_keywords) || length(input$trend_keywords) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "请先选择至少一个关键词"), 
                  size = 12, family = "FangSong") + 
        theme_void()
      return()
    }
    
    plot_data <- rv$keyword_trend %>%
      filter(word %in% input$trend_keywords) %>%
      group_by(PY) %>%
      mutate(rel_freq = count / sum(count) * 100) %>%
      ungroup()
    
    p <- ggplot(plot_data, aes(x = PY, y = rel_freq, color = word, group = word)) +
      geom_line(size = 1.2) +
      labs(
        title = "关键词时间演变趋势",
        x = "年份",
        y = "相对频率 (%)",
        color = "关键词"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "FangSong"),
        axis.text = element_text(family = "FangSong", size = 10),
        axis.title = element_text(family = "FangSong", size = 12),
        legend.text = element_text(family = "FangSong", size = 10),
        legend.title = element_text(family = "FangSong", size = 11, face = "bold"),
        legend.position = "right"
      )
    
    if (input$show_points) {
      p <- p + geom_point(size = 2, alpha = 0.7)
    }
    
    if (input$show_smooth) {
      p <- p + geom_smooth(method = "loess", se = FALSE, span = input$smooth_span)
    }
    
    p <- p + scale_x_continuous(breaks = pretty(plot_data$PY, n = 8)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1))
    
    print(p)
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
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "FangSong"),
          axis.text = element_text(family = "FangSong", size = 10),
          axis.title = element_text(family = "FangSong", size = 12),
          legend.text = element_text(family = "FangSong", size = 10),
          legend.title = element_text(family = "FangSong", size = 11, face = "bold"),
          legend.position = "right"
        ) +
        scale_x_continuous(breaks = pretty(plot_data$PY, n = 8)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1))
      
      if (input$trend_plot_format == "pdf") {
        ggsave(file, plot = p, device = cairo_pdf, width = 12, height = 8, dpi = 300, bg = "white")
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 12, height = 8, dpi = 300, bg = "white")
      }
      show_notification("关键词趋势图导出成功！", "message")
    }
  )
  
  # ==================== 7. 热力图模块（优化版，使用词内年度占比归一化）====================
  
  output$heatmap_plot <- renderPlot({
    req(rv$keyword_trend, rv$word_freq)
    input$plot_heatmap
    
    top_words <- rv$word_freq %>%
      head(input$heatmap_words) %>%
      pull(word)
    
    # 1. 统一归一化逻辑：按词内年度占比（百分比）
    data <- rv$keyword_trend %>%
      filter(word %in% top_words) %>%
      group_by(word) %>%
      mutate(
        year_total = sum(count),
        freq_scaled = count / year_total * 100  # 使用百分比归一化
      ) %>%
      ungroup()
    
    if (nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "无足够数据生成热力图"), 
                  size = 12, family = "FangSong") + 
        theme_void()
      return()
    }
    
    # 2. 配色方案
    colors <- switch(input$color_scale,
                     "red-green" = scale_fill_gradient2(low = "#2E8B57", mid = "#F5F5DC", high = "#8B0000", 
                                                        name = "百分比 (%)"),
                     "blue-yellow-red" = scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026",
                                                              name = "百分比 (%)"),
                     "purple-yellow" = scale_fill_gradient(low = "#FDE725", high = "#440154",
                                                           name = "百分比 (%)"),
                     "heat" = scale_fill_gradient(low = "#FFFFD9", high = "#B10026",
                                                  name = "百分比 (%)"))
    
    # 3. 基础绘图
    p <- ggplot(data, aes(x = PY, y = reorder(word, count), fill = freq_scaled)) +
      geom_tile(color = "white") +
      colors +
      labs(
        title = paste("关键词时间分布热力图 (Top", input$heatmap_words, ")"),
        x = "年份",
        y = "关键词",
        fill = "百分比 (%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "FangSong"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, family = "FangSong"),
        axis.text.y = element_text(size = 10, family = "FangSong"),
        axis.title = element_text(family = "FangSong", size = 12, face = "bold"),
        legend.text = element_text(family = "FangSong", size = 9),
        legend.title = element_text(family = "FangSong", size = 10, face = "bold"),
        legend.position = "right"
      ) +
      scale_x_continuous(breaks = pretty(data$PY, n = 8))
    
    # 4. 显示数值标签（如果勾选）
    if (input$show_numbers) {
      p <- p + geom_text(aes(label = ifelse(count > 0, sprintf("%.1f%%", freq_scaled), "")),
                         size = 3, color = "black", alpha = 0.7, family = "FangSong")
    }
    
    # 5. 行聚类（如果勾选）
    if (input$cluster_rows) {
      word_order <- data %>%
        group_by(word) %>%
        summarise(pattern = paste(count, collapse = "")) %>%
        arrange(pattern) %>%
        pull(word)
      
      p <- p + scale_y_discrete(limits = word_order)
    }
    
    print(p)
  })
  
  # 热力图导出函数（优化版）
  output$download_heatmap_plot <- downloadHandler(
    filename = function() {
      paste0("热力图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$heatmap_plot_format)
    },
    content = function(file) {
      req(rv$keyword_trend, rv$word_freq)
      
      top_words <- rv$word_freq %>%
        head(input$heatmap_words) %>%
        pull(word)
      
      # 1. 统一归一化逻辑：按词内年度占比（百分比）
      plot_data <- rv$keyword_trend %>%
        filter(word %in% top_words) %>%
        group_by(word) %>%
        mutate(
          year_total = sum(count),
          freq_scaled = count / year_total * 100  # 和显示逻辑统一
        ) %>%
        ungroup()
      
      # 2. 统一配色方案
      colors <- switch(input$color_scale,
                       "red-green" = scale_fill_gradient2(low = "#2E8B57", mid = "#F5F5DC", high = "#8B0000",
                                                          name = "百分比 (%)"),
                       "blue-yellow-red" = scale_fill_gradient2(low = "#313695", mid = "#FFFFBF", high = "#A50026",
                                                                name = "百分比 (%)"),
                       "purple-yellow" = scale_fill_gradient(low = "#FDE725", high = "#440154",
                                                             name = "百分比 (%)"),
                       "heat" = scale_fill_gradient(low = "#FFFFD9", high = "#B10026",
                                                    name = "百分比 (%)"))
      
      # 3. 统一绘图逻辑
      p <- ggplot(plot_data, aes(x = PY, y = reorder(word, count), fill = freq_scaled)) +
        geom_tile(color = "white") +
        colors +
        labs(
          title = paste("关键词时间分布热力图 (Top", input$heatmap_words, ")"),
          x = "年份",
          y = "关键词",
          fill = "百分比 (%)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "FangSong"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10, family = "FangSong"),
          axis.text.y = element_text(size = 10, family = "FangSong"),
          axis.title = element_text(family = "FangSong", size = 12, face = "bold"),
          legend.text = element_text(family = "FangSong", size = 9),
          legend.title = element_text(family = "FangSong", size = 10, face = "bold"),
          legend.position = "right"
        ) +
        scale_x_continuous(breaks = pretty(plot_data$PY, n = 8))
      
      # 4. 显示数值标签（如果勾选）
      if (input$show_numbers) {
        p <- p + geom_text(aes(label = ifelse(count > 0, sprintf("%.1f%%", freq_scaled), "")),
                           size = 3, color = "black", alpha = 0.7, family = "FangSong")
      }
      
      # 5. 行聚类（如果勾选）
      if (input$cluster_rows) {
        word_order <- plot_data %>%
          group_by(word) %>%
          summarise(pattern = paste(count, collapse = "")) %>%
          arrange(pattern) %>%
          pull(word)
        
        p <- p + scale_y_discrete(limits = word_order)
      }
      
      # 6. 优化导出格式（PDF使用cairo_pdf避免中文乱码）
      if (input$heatmap_plot_format == "pdf") {
        ggsave(file, plot = p, device = cairo_pdf, width = 14, height = 10, dpi = 300, bg = "white")
      } else if (input$heatmap_plot_format == "png") {
        ggsave(file, plot = p, device = "png", width = 14, height = 10, dpi = 300, bg = "white")
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 14, height = 10, dpi = 300, bg = "white")
      }
      
      show_notification("热力图导出成功！", "message")
    }
  )
  
  # ==================== 8. 气泡图模块 Server 逻辑（使用同步的数据）====================
  
  # 气泡图数据处理函数
  prepare_bubble_data <- reactive({
    req(rv$word_freq, rv$keyword_trend)
    
    # 获取参数
    top_n <- if(input$bubble_show_all) nrow(rv$word_freq) else input$bubble_top_n
    min_freq <- input$bubble_min_freq
    
    # 选择高频关键词
    top_keywords <- rv$word_freq %>%
      filter(n >= min_freq) %>%
      slice_max(n, n = top_n) %>%
      pull(word)
    
    # 计算趋势指标
    trend_metrics <- rv$keyword_trend %>%
      filter(word %in% top_keywords) %>%
      group_by(word) %>%
      summarise(
        total_count = sum(count),
        mean_freq = mean(count, na.rm = TRUE),
        max_freq = max(count, na.rm = TRUE),
        # 计算Spearman相关系数衡量趋势
        trend = ifelse(
          n() > 2, 
          suppressWarnings(cor(PY, count, use = "complete.obs", method = "spearman")), 
          0
        ),
        # 首次出现年份
        first_active = min(PY[count > 0], na.rm = TRUE),
        # 最后出现年份
        last_active = max(PY[count > 0], na.rm = TRUE),
        # 活跃年限
        active_years = last_active - first_active + 1,
        # 频率标准差
        freq_sd = sd(count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        # 趋势分类
        trend_category = case_when(
          trend > 0.3 ~ "上升趋势",
          trend < -0.3 ~ "下降趋势",
          TRUE ~ "平稳趋势"
        ),
        trend_strength = abs(trend),
        # 变异系数
        cv = ifelse(mean_freq > 0, freq_sd / mean_freq, 0)
      ) %>%
      filter(!is.na(first_active), !is.na(last_active))
    
    # 合并词频数据
    bubble_data <- trend_metrics %>%
      left_join(rv$word_freq, by = "word") %>%
      filter(total_count >= input$bubble_min_total) %>%
      arrange(desc(total_count))
    
    return(bubble_data)
  })
  
  # 绘制气泡图
  output$bubble_plot <- renderPlot({
    req(rv$word_freq, rv$keyword_trend)
    input$plot_bubble
    
    bubble_data <- prepare_bubble_data()
    
    if (nrow(bubble_data) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, 
                      label = "无符合条件的关键词（请降低最小频次或增加关键词数量）"), 
                  size = 8, family = "FangSong") + 
        theme_void()
      return()
    }
    
    # 设置颜色方案
    trend_colors <- c(
      "上升趋势" = "#E74C3C",  # 红色
      "下降趋势" = "#3498DB",  # 蓝色
      "平稳趋势" = "#2ECC71"   # 绿色
    )
    
    # 计算X轴刻度
    min_year <- floor(min(bubble_data$first_active))
    max_year <- ceiling(max(bubble_data$first_active))
    
    # 根据用户选择设置X轴间隔
    x_breaks <- if(input$bubble_x_axis == "auto") {
      pretty(bubble_data$first_active, n = 8)
    } else {
      by_val <- as.numeric(input$bubble_x_axis)
      seq(floor(min_year/by_val)*by_val, ceiling(max_year/by_val)*by_val, by = by_val)
    }
    
    p <- ggplot(bubble_data,
                aes(x = first_active, 
                    y = mean_freq,
                    size = total_count, 
                    color = trend_category,
                    alpha = trend_strength)) +
      # 添加气泡
      geom_point(shape = 16, alpha = input$bubble_alpha) +
      # 添加关键词标签
      geom_text_repel(
        aes(label = word),
        size = input$bubble_label_size,
        max.overlaps = 20,
        box.padding = 0.6,
        point.padding = 0.3,
        segment.color = "grey50",
        segment.size = 0.3,
        min.segment.length = 0.2,
        force = 2,
        fontface = "bold",
        family = "FangSong"
      ) +
      # 设置气泡大小
      scale_size_continuous(
        range = c(3, 15),
        name = "总出现次数",
        breaks = c(10, 50, 100, 200, 500, 1000),
        labels = c("10", "50", "100", "200", "500", "1000+")
      ) +
      # 设置颜色
      scale_color_manual(
        values = trend_colors,
        name = "趋势类别"
      ) +
      # 设置透明度
      scale_alpha_continuous(
        range = c(0.5, 1), 
        guide = "none"
      ) +
      # 坐标轴设置
      scale_x_continuous(
        breaks = x_breaks,
        limits = c(min_year - 1, max_year + 1),
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      scale_y_continuous(
        labels = scales::percent_format(scale = 1, accuracy = 0.1),
        breaks = scales::pretty_breaks(n = 8)
      ) +
      # 添加标题和标签
      labs(
        title = "文献关键词趋势分析气泡图",
        subtitle = sprintf("基于分词数据分析（共%d个高频关键词）", nrow(bubble_data)),
        x = "首次出现年份",
        y = "平均相对频率 (%)",
        caption = paste(
          "气泡大小：关键词总出现次数\n",
          "颜色：趋势方向（Spearman相关系数）\n",
          "透明度：趋势强度\n",
          "数据来源：", min(rv$keyword_trend$PY), "-", max(rv$keyword_trend$PY), "年 共", 
          nrow(rv$current_freq_data %||% rv$segmented_data), "篇文献"
        )
      ) +
      # 主题设置
      theme_minimal(base_size = 14, base_family = "FangSong") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "FangSong"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10, family = "FangSong"),
          axis.text.y = element_text(size = 10, family = "FangSong"),
          axis.title = element_text(family = "FangSong", size = 12, face = "bold"),
          legend.text = element_text(family = "FangSong", size = 9),
          legend.title = element_text(family = "FangSong", size = 10, face = "bold"),
          legend.position = "right"
      )
    
    print(p)
  })
  
  # 显示趋势统计数据表
  output$bubble_metrics_table <- renderDT({
    req(rv$trend_metrics)
    
    bubble_data <- prepare_bubble_data()
    
    datatable(
      bubble_data %>%
        select(word, total_count, mean_freq, trend, trend_category, 
               first_active, last_active, active_years) %>%
        mutate(
          mean_freq = round(mean_freq, 2),
          trend = round(trend, 3)
        ) %>%
        rename(
          "关键词" = word,
          "总频次" = total_count,
          "平均频率" = mean_freq,
          "趋势系数" = trend,
          "趋势类别" = trend_category,
          "首次出现" = first_active,
          "最后出现" = last_active,
          "活跃年限" = active_years
        ),
      options = list(
        pageLength = 10,
        order = list(list(2, 'desc')),
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # 统计信息
  output$bubble_stats <- renderPrint({
    req(rv$trend_metrics)
    
    bubble_data <- prepare_bubble_data()
    
    cat("=== 气泡图统计信息 ===\n\n")
    cat("总关键词数:", nrow(bubble_data), "\n")
    cat("总出现频次:", sum(bubble_data$total_count), "\n\n")
    
    # 按趋势类别统计
    cat("按趋势类别统计:\n")
    trend_summary <- bubble_data %>%
      group_by(trend_category) %>%
      summarise(
        关键词数量 = n(),
        平均出现次数 = round(mean(total_count), 1),
        平均趋势强度 = round(mean(trend_strength), 3),
        平均首次出现年份 = round(mean(first_active), 1),
        占比 = paste0(round(n()/nrow(bubble_data)*100, 1), "%")
      )
    
    print(trend_summary)
    
    cat("\n上升趋势最强的关键词（Top 5）:\n")
    rising <- bubble_data %>%
      filter(trend_category == "上升趋势") %>%
      arrange(desc(trend)) %>%
      select(word, trend, total_count, first_active) %>%
      head(5)
    print(rising)
    
    cat("\n下降趋势最强的关键词（Top 5）:\n")
    falling <- bubble_data %>%
      filter(trend_category == "下降趋势") %>%
      arrange(trend) %>%
      select(word, trend, total_count, first_active) %>%
      head(5)
    print(falling)
  })
  
  # 下载趋势统计数据
  output$download_trend_metrics <- downloadHandler(
    filename = function() {
      paste0("趋势统计数据_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$trend_metrics)
      
      bubble_data <- prepare_bubble_data() %>%
        mutate(
          mean_freq = round(mean_freq, 2),
          trend = round(trend, 3)
        )
      
      write_xlsx(bubble_data, file)
      show_notification("趋势统计数据导出成功！", "message")
    }
  )
  
  # 下载气泡图
  output$download_bubble_plot <- downloadHandler(
    filename = function() {
      paste0("气泡图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$bubble_plot_format)
    },
    content = function(file) {
      req(rv$word_freq, rv$keyword_trend)
      
      bubble_data <- prepare_bubble_data()
      
      trend_colors <- c(
        "上升趋势" = "#E74C3C",
        "下降趋势" = "#3498DB",
        "平稳趋势" = "#2ECC71"
      )
      
      min_year <- floor(min(bubble_data$first_active))
      max_year <- ceiling(max(bubble_data$first_active))
      
      x_breaks <- if(input$bubble_x_axis == "auto") {
        pretty(bubble_data$first_active, n = 8)
      } else {
        by_val <- as.numeric(input$bubble_x_axis)
        seq(floor(min_year/by_val)*by_val, ceiling(max_year/by_val)*by_val, by = by_val)
      }
      
      p <- ggplot(bubble_data,
                  aes(x = first_active, 
                      y = mean_freq,
                      size = total_count, 
                      color = trend_category,
                      alpha = trend_strength)) +
        geom_point(shape = 16, alpha = input$bubble_alpha) +
        geom_text_repel(
          aes(label = word),
          size = input$bubble_label_size,
          max.overlaps = 20,
          box.padding = 0.6,
          point.padding = 0.3,
          segment.color = "grey50",
          segment.size = 0.3,
          min.segment.length = 0.2,
          force = 2,
          fontface = "bold",
          family = "FangSong"
        ) +
        scale_size_continuous(range = c(3, 15), name = "总出现次数") +
        scale_color_manual(values = trend_colors, name = "趋势类别") +
        scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
        scale_x_continuous(breaks = x_breaks, limits = c(min_year - 1, max_year + 1)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1)) +
        labs(
          title = "文献关键词趋势分析气泡图",
          subtitle = sprintf("基于分词数据分析（共%d个高频关键词）", nrow(bubble_data)),
          x = "首次出现年份",
          y = "平均相对频率 (%)",
          caption = paste("数据来源：", min(rv$keyword_trend$PY), "-", max(rv$keyword_trend$PY), "年")
        ) +
        theme_minimal(base_family = "FangSong", base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18, family = "FangSong"),
          plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40", family = "FangSong"),
          plot.caption = element_text(size = 10, color = "gray60", hjust = 0, family = "FangSong"),
          legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11, family = "FangSong"),
          axis.text.y = element_text(size = 11, family = "FangSong")
        )
      
      # 根据格式保存
      if (input$bubble_plot_format == "pdf") {
        ggsave(file, plot = p, device = cairo_pdf, width = 14, height = 9, dpi = 300, bg = "white")
      } else if (input$bubble_plot_format == "png") {
        ggsave(file, plot = p, device = "png", width = 14, height = 9, dpi = 300, bg = "white")
      } else {
        ggsave(file, plot = p, device = "jpeg", width = 14, height = 9, dpi = 300, bg = "white")
      }
      
      show_notification("气泡图导出成功！", "message")
    }
  )
  
  # ==================== 9. 词云图模块（使用同步的数据）====================
  
  output$wordcloud_plot <- renderWordcloud2({
    req(rv$word_freq)
    input$generate_cloud
    
    data <- head(rv$word_freq, input$top_n_words)
    
    if (nrow(data) == 0) {
      return(wordcloud2(data.frame(word = "无数据", freq = 1), size = 1))
    }
    
    wc_obj <- wordcloud2(
      data = data,
      size = input$wordcloud_size,
      minSize = 5,
      gridSize = 10,
      fontFamily = "FangSong, Microsoft YaHei, SimHei",
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
    
    rv$wordcloud_obj <- wc_obj
    return(wc_obj)
  })
  
  # 添加CSS样式来居中词云图
  observe({
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
    }, 500);
  ')
  })
  
  output$download_cloud_jpg <- downloadHandler(
    filename = function() {
      paste0("词云图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    content = function(file) {
      req(rv$wordcloud_obj)
      
      temp_html <- tempfile(fileext = ".html")
      
      saveWidget(rv$wordcloud_obj, temp_html, selfcontained = TRUE, libdir = NULL, background = input$bg_color)
      
      html_lines <- readLines(temp_html, warn = FALSE)
      font_style <- '<style> * { font-family: "FangSong", "Microsoft YaHei", "SimHei", sans-serif; } </style>'
      html_lines <- sub('(<head>)', paste0('\\1\n', font_style), html_lines)
      writeLines(html_lines, temp_html)
      
      tryCatch({
        webshot2::webshot(
          url = paste0("file://", normalizePath(temp_html)),
          file = file,
          vwidth = 1200,
          vheight = 800,
          selector = "#wordcloud_plot",
          delay = 3,
          zoom = 2
        )
        unlink(temp_html)
        show_notification("词云图已导出为JPG格式！", "message")
      }, error = function(e) {
        show_notification(paste("导出失败:", e$message), "error")
      })
    }
  )
  
  output$download_cloud_pdf <- downloadHandler(
    filename = function() {
      paste0("词云图_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(rv$wordcloud_obj)
      
      temp_html <- tempfile(fileext = ".html")
      
      saveWidget(rv$wordcloud_obj, temp_html, selfcontained = TRUE, libdir = NULL, background = input$bg_color)
      
      html_lines <- readLines(temp_html, warn = FALSE)
      font_style <- '<style> * { font-family: "FangSong", "Microsoft YaHei", "SimHei", sans-serif; } </style>'
      html_lines <- sub('(<head>)', paste0('\\1\n', font_style), html_lines)
      writeLines(html_lines, temp_html)
      
      tryCatch({
        webshot2::webshot(
          url = paste0("file://", normalizePath(temp_html)),
          file = file,
          vwidth = 1200,
          vheight = 800,
          selector = "#wordcloud_plot",
          delay = 3,
          zoom = 2
        )
        unlink(temp_html)
        show_notification("词云图已导出为PDF格式！", "message")
      }, error = function(e) {
        show_notification(paste("导出失败:", e$message), "error")
      })
    }
  )
  
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
    if (is.null(rv$api_key) || !rv$api_valid) {
      show_notification("请先在API配置页面设置有效的API密钥！", "error")
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
        "Authorization" = paste("Bearer", rv$api_key),  # 使用全局API密钥
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