#' 默认值替换符 (Null coalescing operator)
#' @name null_coalescing
#' @param x 左侧变量
#' @param y 右侧默认值
#' @return 如果 x 为 NULL 则返回 y，否则返回 x
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' 使用 DeepSeek API 进行摘要翻译
#' @param text 需要翻译的英文文本
#' @param api_key DeepSeek API 密钥
#' @param max_tokens 最大 Token 数
#' @importFrom httr add_headers POST timeout status_code content
#' @importFrom jsonlite toJSON
#' @export
translate_with_deepseek <- function(text, api_key, max_tokens = 8000) {
  if (is.na(text) || is.null(text) || trimws(text) == "") {
    return("")
  }

  api_url <- "https://api.deepseek.com/chat/completions"
  request_body <- list(
    model = "deepseek-chat",
    messages = list(
      list(role = "system", content = "专业科学文献翻译，准确将英文摘要译为中文，保留学术严谨性，不添加额外说明"),
      list(role = "user", content = trimws(text))
    ),
    max_tokens = max_tokens,
    temperature = 0.3
  )

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )

  tryCatch({
    response <- httr::POST(
      url = api_url,
      headers,
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      httr::timeout(30)
    )
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "parsed")
      translation <- content$choices[[1]]$message$content
      return(trimws(gsub("^翻译[:：]\\s*", "", translation)))
    } else {
      return(paste("失败：API状态码", httr::status_code(response)))
    }
  }, error = function(e) {
    return(paste("失败：", as.character(e$message)))
  })
}

#' 使用 DeepSeek API 进行中文分词
#' @param text 中文文本
#' @param api_key API密钥
#' @param doc_id 文档ID
#' @param max_tokens 最大Token数
#' @param model 模型名称
#' @param timeout_sec 超时时间
#' @importFrom httr add_headers POST timeout status_code content
#' @importFrom jsonlite toJSON
#' @export
segment_chinese_with_deepseek <- function(text, api_key, doc_id = NULL,
                                          max_tokens = 1000,
                                          model = "deepseek-chat",
                                          timeout_sec = 30) {
  if (is.na(text) || is.null(text) || text == "") {
    return(list(words = character(0), success = FALSE))
  }

  if (nchar(text) > 2000) {
    text <- substr(text, 1, 2000)
  }

  api_url <- "https://api.deepseek.com/chat/completions"
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

  request_body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = paste("请对以下文本进行分词：\n\n", text))
    ),
    max_tokens = max_tokens,
    temperature = 0.1,
    response_format = list(type = "text")
  )

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )

  tryCatch({
    response <- httr::POST(
      url = api_url,
      headers,
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      httr::timeout(timeout_sec)
    )

    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "parsed")
      segmented_text <- content$choices[[1]]$message$content
      segmented_text <- gsub("\n|\\s+", "", segmented_text)

      words <- strsplit(segmented_text, ",")[[1]]
      words <- trimws(words[words != ""])
      words <- words[nchar(words) > 1]

      stopwords_custom <- c("的", "和", "与", "及", "在", "是", "了", "对", "于", "中", "使用", "可以", "研究", "分析", "基于", "方法", "数据", "结果", "表明", "显示", "我们", "他们", "它们", "这些", "那些", "这个", "那个")
      words <- words[!words %in% stopwords_custom]

      return(list(words = words, success = TRUE))
    } else {
      return(list(words = character(0), success = FALSE, error = paste("HTTP", httr::status_code(response))))
    }
  }, error = function(e) {
    return(list(words = character(0), success = FALSE, error = e$message))
  })
}