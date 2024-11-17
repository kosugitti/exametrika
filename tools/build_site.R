# tools/build.R
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

  # 一時的にREADME.Rmdを作成
  message("Creating README.md...")
  temp_rmd <- tempfile(fileext = ".Rmd")
  on.exit(unlink(temp_rmd), add = TRUE)

  file.copy("docs/index.Rmd", temp_rmd)

  # 一時ファイルのYAMLヘッダーを修正
  content <- readLines(temp_rmd)
  # docs/figures/ を figures/ に置換
  content <- gsub("docs/figures/", "figures/", content)
  writeLines(content, temp_rmd)
  yaml_end <- which(content == "---")[2]
  new_content <- c(
    "---",
    "output:",
    "  github_document",
    "---",
    content[(yaml_end + 1):length(content)]
  )
  writeLines(new_content, temp_rmd)

  # README.mdを生成
  rmarkdown::render(temp_rmd,
                    output_format = "github_document",
                    output_file = "README.md",
                    output_dir = getwd(),     # 出力ディレクトリを明示的に指定
                    knit_root_dir = getwd())  # knit実行のルートディレクトリを指定


  message("Documentation built successfully!")
}

build_site()
