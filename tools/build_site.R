build_site <- function() {
  # カレントディレクトリを保存
  old_dir <- getwd()
  on.exit(setwd(old_dir))

  message("Building documentation...")

  # docsディレクトリに移動
  setwd("docs")

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

  # READMEの生成
  setwd(old_dir)
  message("\nCreating README.md...")
  # index.Rmdから必要な部分を抽出
  content <- readLines("docs/index.Rmd")
  yaml_end <- which(content == "---")[2]
  content <- content[(yaml_end + 1):length(content)]

  # setup chunkを削除
  setup_start <- which(grepl("^```\\{r setup,|^```\\{r setup\\}", content))
  if (length(setup_start) > 0) {
    setup_end <- setup_start + which(grepl("^```$", content[setup_start:length(content)]))[1] - 1
    content <- content[-(setup_start:setup_end)]
  }

  # Stickerの画像行を修正
  img_line <- grep("^\\!\\[\\]\\(", content)
  if (length(img_line) > 0) {
    # GitHubスタイルの画像タグに変換
    content[img_line] <- "# exametrika <img src=\"man/figures/sticker.png\" align=\"right\" height=\"139\" />"
    # スタイル行を削除
    style_line <- grep("\\{style=", content)
    if (length(style_line) > 0) {
      content <- content[-style_line]
    }
    # 元のタイトル行を削除
    title_line <- grep("^# exametrika$", content)
    if (length(title_line) > 0) {
      content <- content[-title_line]
    }
  }

  # 言語切り替えリンクと前の空行を削除
  lang_link_idx <- grep("\\[English\\].*\\[日本語\\]", content)
  if (length(lang_link_idx) > 0) {
    # 言語切り替えリンクの前の空行を探す（最大2行まで）
    empty_lines <- numeric(0)
    for (i in 1:2) {
      if (lang_link_idx - i > 0 && content[lang_link_idx - i] == "") {
        empty_lines <- c(empty_lines, lang_link_idx - i)
      }
    }
    # 空行と言語切り替えリンクを削除
    lines_to_remove <- c(empty_lines, lang_link_idx)
    content <- content[-lines_to_remove]
  }

  writeLines(content, "README.md")
  message("\nDocumentation built successfully!")
}
build_site()
