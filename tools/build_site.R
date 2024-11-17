# カレントディレクトリを保存
old_dir <- getwd()
on.exit(setwd(old_dir))

message("Building documentation...")
# まずdocsディレクトリのファイルをビルド
message("Building docs site...")
setwd("docs")

# 画像ファイルのディレクトリを作成
dir.create("figures", showWarnings = FALSE)

# 英語版
rmarkdown::render("index.Rmd",
  output_format = "html_document",
  output_options = list(
    self_contained = FALSE,
    lib_dir = "libs",
    fig_path = "figures/"
  ),
  output_file = "index.html"
)

# 日本語版
rmarkdown::render("ja.Rmd",
  output_format = "html_document",
  output_options = list(
    self_contained = FALSE,
    lib_dir = "libs",
    fig_path = "figures/"
  ),
  output_file = "ja.html"
)

# ルートディレクトリに戻る
setwd(old_dir)

# README.md生成のための一時Rmdファイルを作成
message("Creating README.md...")
# index.Rmdの内容を読み込み
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
