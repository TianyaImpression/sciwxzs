# ===========================================================================
# sciwxzs 运行日志模块
# ===========================================================================
# 负责记录软件运行时长、分析文献数量、API Token 消耗、功能使用频次。
# 日志写入运行根目录下的 sciwxzs_log 文件。
# ===========================================================================

#' 初始化运行日志
#'
#' 在运行根目录创建或追加日志文件，记录会话的开始时间。
#' 返回一个日志环境对象，用于在会话中累积日志数据。
#'
#' @param log_file 日志文件完整路径；若为 NULL 则根据 log_dir 自动生成
#' @param log_dir 日志目录，仅在 log_file 为 NULL 时使用，默认 getwd()
#' @return 日志环境对象
#' @keywords internal
init_sciwxzs_logger <- function(log_file = NULL, log_dir = getwd()) {

  log_env <- new.env(parent = emptyenv())

  log_env$start_time  <- Sys.time()
  log_env$last_update <- Sys.time()
  log_env$paper_count <- 0

  log_env$token_usage <- list(
    translation = list(prompt = 0, completion = 0),
    segmentation = list(prompt = 0, completion = 0),
    review       = list(prompt = 0, completion = 0)
  )

  log_env$feature_count <- list(
    data_upload   = 0,
    translation   = 0,
    segmentation  = 0,
    word_freq     = 0,
    time_trend    = 0,
    heatmap       = 0,
    bubble_chart  = 0,
    word_cloud    = 0,
    review        = 0
  )

  if (is.null(log_file)) {
    log_file <- file.path(log_dir, "sciwxzs_log")
  }
  log_file <- normalizePath(log_file, mustWork = FALSE)
  log_env$log_file <- log_file

  # 写入初始日志头
  write_sciwxzs_log(log_env)

  log_env
}


#' 写入日志到磁盘
#'
#' 将当前累积的日志数据格式化写入 sciwxzs_log 文件。
#'
#' @param log_env 日志环境对象
#' @keywords internal
write_sciwxzs_log <- function(log_env) {
  now <- Sys.time()
  log_env$last_update <- now

  duration <- difftime(now, log_env$start_time, units = "auto")

  # 计算格式化运行时长
  duration_str <- format_time_duration(duration)

  # 计算 token 汇总
  total_prompt     <- 0
  total_completion <- 0
  for (feature in names(log_env$token_usage)) {
    total_prompt     <- total_prompt     + (log_env$token_usage[[feature]]$prompt     %||% 0)
    total_completion <- total_completion + (log_env$token_usage[[feature]]$completion %||% 0)
  }

  lines <- c(
    "========================================",
    sprintf("  会话开始 : %s", format(log_env$start_time, "%Y-%m-%d %H:%M:%S")),
    sprintf("  当前时间 : %s", format(now, "%Y-%m-%d %H:%M:%S")),
    sprintf("  运行时长 : %s", duration_str),
    "----------------------------------------",
    sprintf("  文献分析数量 : %d 篇", log_env$paper_count),
    "----------------------------------------",
    "  API Token 消耗 :",
    sprintf("    翻译     : 输入 %d  | 输出 %d",
            log_env$token_usage$translation$prompt,
            log_env$token_usage$translation$completion),
    sprintf("    分词     : 输入 %d  | 输出 %d",
            log_env$token_usage$segmentation$prompt,
            log_env$token_usage$segmentation$completion),
    sprintf("    文献综述 : 输入 %d  | 输出 %d",
            log_env$token_usage$review$prompt,
            log_env$token_usage$review$completion),
    sprintf("    合计     : 输入 %d  | 输出 %d", total_prompt, total_completion),
    "----------------------------------------",
    "  功能使用频次 :",
    sprintf("    数据上传与筛选 : %d 次", log_env$feature_count$data_upload),
    sprintf("    摘要翻译       : %d 次", log_env$feature_count$translation),
    sprintf("    分词处理       : %d 次", log_env$feature_count$segmentation),
    sprintf("    词频分析       : %d 次", log_env$feature_count$word_freq),
    sprintf("    时间趋势       : %d 次", log_env$feature_count$time_trend),
    sprintf("    热力图         : %d 次", log_env$feature_count$heatmap),
    sprintf("    气泡图         : %d 次", log_env$feature_count$bubble_chart),
    sprintf("    词云图         : %d 次", log_env$feature_count$word_cloud),
    sprintf("    文献综述       : %d 次", log_env$feature_count$review),
    "========================================",
    ""
  )

  tryCatch({
    writeLines(lines, log_env$log_file)
  }, error = function(e) {
    warning("无法写入 sciwxzs_log 文件: ", e$message)
  })
}


#' 格式化时间间隔为易读字符串
#' @param duration difftime 对象
#' @return 格式化字符串，如 "2小时15分30秒"
#' @keywords internal
format_time_duration <- function(duration) {
  seconds <- as.numeric(duration, units = "secs")
  if (is.na(seconds) || seconds < 0) return("未知")

  days    <- floor(seconds / 86400)
  seconds <- seconds - days * 86400
  hours   <- floor(seconds / 3600)
  seconds <- seconds - hours * 3600
  minutes <- floor(seconds / 60)
  secs    <- round(seconds - minutes * 60)

  parts <- c()
  if (days > 0)    parts <- c(parts, sprintf("%d天", days))
  if (hours > 0)   parts <- c(parts, sprintf("%d小时", hours))
  if (minutes > 0) parts <- c(parts, sprintf("%d分钟", minutes))
  if (secs > 0 || length(parts) == 0) parts <- c(parts, sprintf("%d秒", secs))

  paste(parts, collapse = "")
}


#' 记录 Token 使用量
#' @param log_env 日志环境对象
#' @param feature 功能名称 ("translation" / "segmentation" / "review")
#' @param prompt_tokens 输入 token 数
#' @param completion_tokens 输出 token 数
#' @keywords internal
log_tokens <- function(log_env, feature, prompt_tokens, completion_tokens) {
  if (!feature %in% names(log_env$token_usage)) {
    warning("未知功能类型: ", feature)
    return(invisible())
  }
  log_env$token_usage[[feature]]$prompt     <- log_env$token_usage[[feature]]$prompt     + (prompt_tokens     %||% 0)
  log_env$token_usage[[feature]]$completion <- log_env$token_usage[[feature]]$completion + (completion_tokens %||% 0)
}


#' 记录功能使用频次
#' @param log_env 日志环境对象
#' @param feature 功能名称
#' @keywords internal
log_feature_use <- function(log_env, feature) {
  if (!feature %in% names(log_env$feature_count)) {
    warning("未知功能: ", feature)
    return(invisible())
  }
  log_env$feature_count[[feature]] <- log_env$feature_count[[feature]] + 1
}


#' 记录分析的文献数量
#' @param log_env 日志环境对象
#' @param count 文献数量
#' @keywords internal
log_paper_count <- function(log_env, count) {
  log_env$paper_count <- max(log_env$paper_count, count %||% 0)
}
