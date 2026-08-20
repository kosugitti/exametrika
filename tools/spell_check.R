# 綴り検査。devtools::spell_check() の代わりにこれを source() する。
#
# 素の spell_check() は guide-ja.Rmd(日本語 vignette。hunspell が形態素を
# 単語として拾うだけで永久にノイズ)と NEWS.md(履歴。過去の誤字は直さない
# 方針 2026-08-19)も検査して 300 件超を報告し，本物の誤字が埋もれて
# Biclustering.Rd の wheter/iterasions を長く見逃した。ここでは両者を
# 対象から外す。指摘ゼロが正常で，何か出たらそれは本物。
#
# 新出の正当な用語は inst/WORDLIST に登録する(リリースごとに見直す)。
pacman::p_load(spelling)

files <- c(
  "README.md",
  list.files("man", pattern = "[.]Rd$", full.names = TRUE),
  setdiff(
    list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE),
    "vignettes/guide-ja.Rmd"
  )
)
result <- spelling::spell_check_files(
  files,
  ignore = readLines("inst/WORDLIST"),
  lang = "en-US"
)
# DESCRIPTION は生テキストで検査するとフィールド名(LinkingTo 等)まで拾うので，
# 利用者が読む Title と Description の本文だけを見る。
desc <- read.dcf("DESCRIPTION", fields = c("Title", "Description"))
desc_bad <- hunspell::hunspell(
  paste(desc, collapse = "\n"),
  dict = "en_US",
  ignore = readLines("inst/WORDLIST")
)[[1]]

print(result)
if (length(desc_bad) > 0) cat("DESCRIPTION(Title/Description):", desc_bad, "\n")
if (nrow(result) == 0 && length(desc_bad) == 0) message("綴り検査: 指摘なし")
