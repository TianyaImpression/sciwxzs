```markdown

# sciwxzs: 基于 DeepSeek 的 SCI 文献智能分析与综述生成系统 🚀

[](https://www.r-project.org/)
**`sciwxzs`** 是一款专为科研工作者打造的 R 语言工具包。它将强大的 **DeepSeek 大语言模型能力** 与传统的文献计量学方法深度融合，提供了一个高度集成的交互式 Web 分析系统（基于 Shiny）。

本工具旨在解决海量英文 SCI 论文在阅读、信息提取和总结阶段的痛点，帮助研究人员快速把握领域前沿、厘清脉络，并一键生成高质量的文献综述。

## ✨ 核心功能 (Features)

* 🤖 **AI 智能翻译与分词**：调用 DeepSeek API，对导入的 SCI 英文摘要进行符合学术语境的高质量翻译，并进行精准的中文专业术语分词。
* 📊 **多维度词频分析**：自动清洗停用词，统计并提取高频核心关键词。
* 📈 **时间演变与可视化趋势**：内置多种发表级别的可视化图表，包括：
* 学术风向词云图 (Wordcloud)
* 关键词时间演变趋势线图 (Trend Line)
* 热力分布图 (Heatmap)
* 趋势气泡图 (Bubble Plot)


* ✍️ **一键综述生成**：依托 LLM 强大的归纳推理能力，对选定范围内的文献进行深度总结，自动化撰写结构严谨的**国内外研究现状综述**，并附带规范的参考文献列表。

## 📥 安装指南 (Installation)

你可以使用 `remotes` 包直接从 GitHub 安装开发版本的 `sciwxzs`：

```{r, eval = FALSE}
# 如果你尚未安装 remotes，请先运行：
# install.packages("remotes")

# 从 GitHub 安装 sciwxzs 
remotes::install_github("TianyaImpression/sciwxzs")

```

*(注意：安装前请确保系统中已安装 R 和 RStudio/Positron IDE)*

## 🚀 快速入门 (Quick Start)

安装完成后，只需两行代码即可启动交互式分析系统：

```{r, eval = FALSE}
library(sciwxzs)

# 启动本地文献分析与翻译系统
run_sciwxzs()

```

### 准备工作 (Prerequisites)

1. **数据准备**：请准备包含 SCI 文献元数据的 Excel 文件（需包含 `TI`标题, `AB`摘要, `PY`年份, `DE`关键词 等标准字段）。
2. **API 密钥**：获取一个有效的 DeepSeek API Key（以 `sk-` 开头），用于在系统中激活翻译、分词和综述撰写功能。

## 👥 团队与致谢 (Authors & Acknowledgements)

* **作者 (Author)**：张凯
* **指导老师 (Advisor)**：徐洋（西南林业大学）

💡 **交流与探讨**
欢迎关注微信公众号 **“走天涯徐小洋地理数据科学”** 获取更多关于空间数据分析、R 语言开发及地理信息科学的前沿技术分享。

```

