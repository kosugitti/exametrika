build_site <- function() {
  # カレントディレクトリを保存
  old_dir <- getwd()
  on.exit(setwd(old_dir))

  message("Building documentation...")

  # docsディレクトリに移動
  setwd("docs")

  # setup chunkの内容を確認
  message("\nChecking Rmd files setup chunks:")

  # index.Rmdのsetup確認
  message("\nindex.Rmd setup:")
  index_content <- readLines("index.Rmd")
  setup_start <- which(grepl("^```\\{r setup,", index_content))
  if (length(setup_start) > 0) {
    setup_end <- setup_start + which(grepl("^```$", index_content[setup_start:length(index_content)]))[1] - 1
    writeLines(index_content[setup_start:setup_end])
  }

  # ja.Rmdのsetup確認
  message("\nja.Rmd setup:")
  ja_content <- readLines("ja.Rmd")
  setup_start <- which(grepl("^```\\{r setup,", ja_content))
  if (length(setup_start) > 0) {
    setup_end <- setup_start + which(grepl("^```$", ja_content[setup_start:length(ja_content)]))[1] - 1
    writeLines(ja_content[setup_start:setup_end])
  }

  # HTML生成
  message("\nGenerating HTML files...")

  # 英語版
  rmarkdown::render("index.Rmd",
                    output_format = "html_document",
                    output_file = "index.html")

  # 日本語版
  rmarkdown::render("ja.Rmd",
                    output_format = "html_document",
                    output_file = "ja.html")

  # 生成された図を確認
  message("\nChecking generated figures:")
  if (dir.exists("figures")) {
    fig_files <- list.files("figures", full.names = TRUE)
    message("Found figures: ", paste(basename(fig_files), collapse = ", "))
  } else {
    message("No figures directory found")
  }

  # READMEの生成
  setwd(old_dir)
  message("\nCreating README.md...")

  # index.Rmdから必要な部分を抽出
  content <- readLines("docs/index.Rmd")
  yaml_end <- which(content == "---")[2]
  content <- content[(yaml_end + 1):length(content)]

  # setup chunkを削除
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

  writeLines(content, "README.md")

  message("\nDocumentation built successfully!")
}

build_site()
