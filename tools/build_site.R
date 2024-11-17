# カレントディレクトリを保存
old_dir <- getwd()
on.exit(setwd(old_dir))

message("Building documentation...")
# まずdocsディレクトリのファイルをビルド
message("Building docs site...")
setwd("docs")

# 画像ファイルのディレクトリを作成
dir.create("figures", showWarnings = FALSE)

# 英語版をレンダリングして図を生成
message("Rendering English version...")
rmarkdown::render("index.Rmd",
  output_format = "html_document",
  output_options = list(
    self_contained = FALSE,
    lib_dir = "libs",
    fig_path = "figures/"
  ),
  output_file = "index.html"
)

# 生成された図のファイル名を取得
fig_files <- list.files("figures", pattern = "\\.(png|jpg|jpeg|svg)$", full.names = TRUE)
message("Generated figures: ", paste(basename(fig_files), collapse = ", "))

# 日本語版のRmdを読み込んで修正
message("Modifying Japanese version...")
ja_content <- readLines("ja.Rmd")

# setup chunkを探して修正
setup_start <- which(grepl("^```\\{r setup,", ja_content))
if (length(setup_start) > 0) {
  setup_end <- setup_start + which(grepl("^```$", ja_content[setup_start:length(ja_content)]))[1] - 1

  # 新しいsetup chunk（キャッシュを無効化し、既存の図を使用）
  new_setup <- c(
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(",
    "  cache = FALSE,", # キャッシュを無効化
    "  fig.path = 'figures/',", # 図のパスは共通
    "  eval = FALSE     # 図を生成せず表示のみ",
    ")",
    "```"
  )

  # setup chunkを置換
  ja_content <- c(
    ja_content[1:(setup_start - 1)],
    new_setup,
    ja_content[(setup_end + 1):length(ja_content)]
  )
}

# 修正した日本語版を一時ファイルに保存
temp_ja <- tempfile(fileext = ".Rmd")
writeLines(ja_content, temp_ja)

# 修正した日本語版をレンダリング
message("Rendering Japanese version...")
rmarkdown::render(temp_ja,
  output_format = "html_document",
  output_options = list(
    self_contained = FALSE,
    lib_dir = "libs",
    fig_path = "figures/"
  ),
  output_file = "ja.html",
  knit_root_dir = getwd()
) # 重要：元のディレクトリでknitする

unlink(temp_ja) # 一時ファイルを削除

# ルートディレクトリに戻る
setwd(old_dir)

# README.md作成
message("Creating README.md...")
content <- readLines("docs/index.Rmd")

# YAMLヘッダーを削除
yaml_end <- which(content == "---")[2]
content <- content[(yaml_end + 1):length(content)]

# setup chunkを削除（あれば）
setup_start <- which(grepl("^```\\{r setup,", content))
if (length(setup_start) > 0) {
  setup_end <- setup_start + which(grepl("^```$", content[setup_start:length(content)]))[1] - 1
  content <- content[-(setup_start:setup_end)]
}

# 言語切り替えリンクを修正
lang_link_idx <- grep("\\[English\\].*\\[日本語\\]", content)
if (length(lang_link_idx) > 0) {
  content[lang_link_idx] <- "[English](docs/index.html) | [日本語](docs/ja.html)"
}

# README.mdとして書き出し
writeLines(content, "README.md")

message("Documentation built successfully!")
