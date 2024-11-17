build_site <- function() {
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
                    output_file = "index.html")

  # 日本語版
  rmarkdown::render("ja.Rmd",
                    output_format = "html_document",
                    output_options = list(
                      self_contained = FALSE,
                      lib_dir = "libs",
                      fig_path = "figures/"
                    ),
                    output_file = "ja.html")

  # ルートディレクトリに戻る
  setwd(old_dir)

  # README.md 生成のための準備
  message("Creating README.md...")

  # 一時的にREADME.Rmdを作成
  temp_rmd <- tempfile(fileext = ".Rmd")
  on.exit(unlink(temp_rmd), add = TRUE)

  # index.Rmdをコピーして内容を修正
  file.copy("docs/index.Rmd", temp_rmd)
  content <- readLines(temp_rmd)

  # YAMLヘッダーを修正し、画像パスを docs/figures に設定
  yaml_end <- which(content == "---")[2]
  new_content <- c(
    "---",
    "output: github_document",
    "---",
    content[(yaml_end + 1):length(content)]
  )

  # figures/ を docs/figures/ に置換（GitHubでの相対パス参照用）
  new_content <- gsub("figures/", "docs/figures/", new_content)
  writeLines(new_content, temp_rmd)

  # README.mdを生成
  rmarkdown::render(temp_rmd,
                    output_format = "github_document",
                    output_file = "README.md",
                    output_dir = getwd(),
                    knit_root_dir = getwd())

  message("Documentation built successfully!")
}
