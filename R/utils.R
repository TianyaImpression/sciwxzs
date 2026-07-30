#' 默认值替换符 (Null coalescing operator)
#' @name null_coalescing
#' @param x 左侧变量
#' @param y 右侧默认值
#' @return 如果 x 为 NULL 则返回 y，否则返回 x
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' 初始化系统字体库 (仅需运行一次)
#' 
#' @description 扫描并导入系统字体，用于生成带有特殊字体的 PDF 图表。
#' @importFrom extrafont font_import loadfonts
#' @export
init_sciwxzs_fonts <- function() {
  message("正在导入系统字体，这可能需要几分钟时间，请耐心等待...")
  extrafont::font_import(prompt = FALSE)
  extrafont::loadfonts(device = "pdf", quiet = TRUE)
  message("字体导入完成！")
}


# ===========================================================================
# 统一 AI API 调用基础设施
# ===========================================================================

#' 构建标准 API 请求头
#' @param api_key API 密钥（可选，本地大模型可留空）
#' @return httr add_headers 对象
#' @keywords internal
build_api_headers <- function(api_key = "") {
  if (!is.null(api_key) && nzchar(trimws(api_key))) {
    httr::add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    )
  } else {
    httr::add_headers(
      "Content-Type" = "application/json"
    )
  }
}

#' 获取 API 端点 URL
#' @param provider API 提供商类型
#' @param custom_url 自定义 API URL（仅当 provider = "custom" 时使用）
#' @return API 端点 URL 字符串
#' @keywords internal
get_api_url <- function(provider = "deepseek", custom_url = NULL) {
  if (provider == "deepseek") {
    "https://api.deepseek.com/v1/chat/completions"
  } else if (provider == "custom") {
    if (is.null(custom_url) || trimws(custom_url) == "") {
      stop("自定义 API 模式下必须提供 API 端点 URL")
    }
    url <- trimws(custom_url)
    if (!grepl("/chat/completions$", url)) {
      url <- paste0(url, "/v1/chat/completions")
    }
    url
  } else {
    stop("不支持的 API 提供商类型: ", provider)
  }
}

#' 获取默认模型名称
#' @param provider API 提供商类型
#' @param custom_model 自定义模型名称
#' @return 模型名称字符串
#' @keywords internal
get_default_model <- function(provider = "deepseek", custom_model = NULL) {
  if (provider == "deepseek") {
    "deepseek-chat"
  } else if (provider == "custom") {
    custom_model %||% "gpt-3.5-turbo"
  } else {
    stop("不支持的 API 提供商类型: ", provider)
  }
}

#' 统一 AI API 调用函数
#'
#' @description 支持 DeepSeek V4 和用户自定义 OpenAI 兼容 API 的统一调用接口。
#'   本地大模型无需 API 密钥，传入空字符串即可。
#'
#' @param messages 消息列表，每项包含 role 和 content
#' @param api_key API 密钥（DeepSeek 必填，自定义/本地可选）
#' @param provider API 提供商类型，"deepseek" 或 "custom"
#' @param custom_url 自定义 API 端点 URL（仅 provider = "custom" 时有效）
#' @param model 模型名称，若为 NULL 则使用默认模型
#' @param max_tokens 最大 Token 数
#' @param temperature 温度参数 (0-2)
#' @param timeout_sec 超时时间（秒）
#' @param response_format 响应格式，如 list(type = "text") 或 list(type = "json_object")
#'
#' @return 成功时返回 API 响应中的文本内容；失败时返回以 "失败：" 开头的错误信息
#'
#' @importFrom httr add_headers POST timeout status_code content
#' @importFrom jsonlite toJSON
#' @export
call_ai_api <- function(messages,
                        api_key = "",
                        provider = "deepseek",
                        custom_url = NULL,
                        model = NULL,
                        max_tokens = 8000,
                        temperature = 0.3,
                        timeout_sec = 30,
                        response_format = NULL) {

  # 仅 DeepSeek 模式强制要求 API 密钥；自定义/本地 LLM 可选
  if (provider == "deepseek" && (is.null(api_key) || !nzchar(trimws(api_key)))) {
    stop("DeepSeek API 密钥不能为空")
  }

  api_url <- get_api_url(provider, custom_url)
  model <- model %||% get_default_model(provider, custom_model)

  request_body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    temperature = temperature
  )

  if (!is.null(response_format)) {
    request_body$response_format <- response_format
  }

  headers <- build_api_headers(api_key)

  tryCatch({
    response <- httr::POST(
      url = api_url,
      headers,
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      httr::timeout(timeout_sec)
    )

    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "parsed")
      result <- content$choices[[1]]$message$content
      # 将 token 用量信息附加为属性，调用方可透过 attr(, "usage") 获取
      if (!is.null(content$usage)) {
        usage <- list(
          prompt_tokens     = content$usage$prompt_tokens     %||% 0,
          completion_tokens = content$usage$completion_tokens %||% 0,
          total_tokens      = content$usage$total_tokens      %||% 0
        )
        attr(result, "usage") <- usage
      }
      return(result)
    } else {
      error_body <- tryCatch(
        httr::content(response, "text"),
        error = function(e) "无法解析错误响应"
      )
      return(paste("失败：API状态码", httr::status_code(response), "-", error_body))
    }
  }, error = function(e) {
    return(paste("失败：", as.character(e$message)))
  })
}


# ===========================================================================
# 业务功能函数
# ===========================================================================

#' 使用 AI API 进行摘要翻译
#'
#' @param text 需要翻译的英文文本
#' @param api_key API 密钥
#' @param provider API 提供商类型，"deepseek" 或 "custom"
#' @param custom_url 自定义 API URL（仅 provider = "custom" 时有效）
#' @param model 模型名称，默认使用 deepseek-chat
#' @param max_tokens 最大 Token 数
#' @param timeout_sec 超时时间（秒）
#'
#' @importFrom httr add_headers POST timeout status_code content
#' @importFrom jsonlite toJSON
#' @export
translate_with_deepseek <- function(text,
                                     api_key,
                                     provider = "deepseek",
                                     custom_url = NULL,
                                     model = NULL,
                                     max_tokens = 8000,
                                     timeout_sec = 30) {
  if (is.na(text) || is.null(text) || trimws(text) == "") {
    return("")
  }

  messages <- list(
    list(role = "system", content = "专业科学文献翻译，准确将英文摘要译为中文，保留学术严谨性，不添加额外说明"),
    list(role = "user", content = trimws(text))
  )

  result <- call_ai_api(
    messages = messages,
    api_key = api_key,
    provider = provider,
    custom_url = custom_url,
    model = model,
    max_tokens = max_tokens,
    temperature = 0.3,
    timeout_sec = timeout_sec
  )

  # 提取并保留 token 用量属性
  usage_attr <- attr(result, "usage")

  if (grepl("^失败", result)) {
    return(result)
  }

  result <- trimws(gsub("^翻译[:：]\\s*", "", result))
  attr(result, "usage") <- usage_attr
  result
}

#' 使用 AI API 进行中文分词
#'
#' @param text 中文文本
#' @param api_key API密钥
#' @param provider API 提供商类型，"deepseek" 或 "custom"
#' @param custom_url 自定义 API URL（仅 provider = "custom" 时有效）
#' @param doc_id 文档ID
#' @param max_tokens 最大Token数
#' @param model 模型名称
#' @param timeout_sec 超时时间
#'
#' @importFrom httr add_headers POST timeout status_code content
#' @importFrom jsonlite toJSON
#' @export
segment_chinese_with_deepseek <- function(text,
                                           api_key,
                                           provider = "deepseek",
                                           custom_url = NULL,
                                           doc_id = NULL,
                                           max_tokens = 1000,
                                           model = NULL,
                                           timeout_sec = 30) {
  if (is.na(text) || is.null(text) || text == "") {
    return(list(words = character(0), success = FALSE))
  }

  if (nchar(text) > 2000) {
    text <- substr(text, 1, 2000)
  }

  system_prompt <- paste0(
    "你是一个专业的中文自然语言处理专家，擅长进行科学文献分析。",
    "请对以下中文文本进行分词处理。分词要求：",
    "1. 识别专业术语、科技术语和专有名词，保持其完整性；",
    "2. 去除常见的停用词（如'的'、'和'、'在'、'是'等）；",
    "3. 只保留有意义的名词、动词、形容词等实词；",
    "4. 过滤掉标点符号和数字，数学公式，百分数，年份等数学式；",
    "5. 每个词语之间用逗号分隔；",
    "请严格按照'词语1,词语2,词语3,...'的格式返回分词结果，不要添加任何解释或额外文本。"
  )

  messages <- list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste("请对以下文本进行分词：\n\n", text))
  )

  result <- call_ai_api(
    messages = messages,
    api_key = api_key,
    provider = provider,
    custom_url = custom_url,
    model = model,
    max_tokens = max_tokens,
    temperature = 0.1,
    timeout_sec = timeout_sec,
    response_format = list(type = "text")
  )

  # 提取并保留 token 用量属性
  usage_attr <- attr(result, "usage")

  if (grepl("^失败", result)) {
    return(list(words = character(0), success = FALSE, error = result))
  }

  segmented_text <- gsub("\n|\\s+", "", result)

  words <- strsplit(segmented_text, ",")[[1]]
  words <- trimws(words[words != ""])
  words <- words[nchar(words) > 1]

  stopwords_custom <- c("的", "和", "与", "及", "在", "是", "了", "对", "于", "中",
                        "使用", "可以", "研究", "分析", "基于", "方法", "数据",
                        "结果", "表明", "显示", "我们", "他们", "它们", "这些",
                        "那些", "这个", "那个")
  words <- words[!words %in% stopwords_custom]

  list(words = words, success = TRUE, usage = usage_attr)
}

