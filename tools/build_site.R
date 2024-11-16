# tools/build.R
build_site <- function() {
  # カレントディレクトリを保存
  old_dir <- getwd()
  on.exit(setwd(old_dir))

  message("Building documentation...")

  # まずdocsディレクトリのファイルをビルド
  message("Building docs site...")
  setwd("docs")

  # 英語版
  rmarkdown::render("index.Rmd",
                    output_format = "html_document",
                    output_file = "index.html")

  # 日本語版
  rmarkdown::render("ja.Rmd",
                    output_format = "html_document",
                    output_file = "ja.html")

  # ルートディレクトリに戻る
  setwd(old_dir)

  # index.RmdをREADME.Rmdとしてコピー
  message("Creating README.md...")
  file.copy("docs/index.Rmd", "README.Rmd", overwrite = TRUE)

  # README.RmdのYAMLヘッダーを修正
  content <- readLines("README.Rmd")
  yaml_end <- which(content == "---")[2]
  new_content <- c(
    "---",
    "output: github_document",
    "---",
    content[(yaml_end + 1):length(content)]
  )
  writeLines(new_content, "README.Rmd")

  # README.mdを生成
  rmarkdown::render("README.Rmd",
                    output_format = "github_document",
                    output_file = "README.md")

  message("Documentation built successfully!")
}

# スクリプトとして実行された場合は自動的にビルド実行
if (!interactive()) {
  build_site()
}
