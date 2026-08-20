# リリース手順。上から順に手で実行する(source() で一括実行しない)。
# 最後の submit_cran() は提出日に本人が押す。
#
# == なぜ git archive を挟むか (2026-08-20 の実害) ==
# このリポジトリは Dropbox 同期下にあり，Dropbox はもう一台のマシンから
# 「git 上は削除済みのファイル」を働き木に復活させることがある。
# check_win_devel() を働き木から直接使ったところ，削除済みの旧テスト
# ファイル 5 本が tarball に混入し，win-builder で 9 件の偽の失敗が出た
# (旧テスト×再生成済み参照値の齟齬。https://win-builder.r-project.org/OF5w3t5GQ3MP)。
# 検査・提出に使う木は必ず git archive で Dropbox の外に書き出して作る。

pacman::p_load(styler, devtools, rhub)

## --- A. 開発側(働き木)。差分が出たらコミットしてから B へ進む ---
styler::style_pkg()
devtools::document()
source("tools/spell_check.R")

## --- B. 検査・提出側(コミット済みの木だけを使う) ---
dirty <- system2("git", c("status", "--porcelain"), stdout = TRUE)
if (length(dirty) > 0) {
  stop("働き木がクリーンではない(未追跡を含む):\n", paste(dirty, collapse = "\n"))
}
src <- file.path(path.expand("~/.local/tmp"), "exametrika-release")
unlink(src, recursive = TRUE)
dir.create(src, recursive = TRUE, showWarnings = FALSE)
system(sprintf("git archive HEAD | tar -x -C %s", shQuote(src)))

devtools::check(src, cran = TRUE)
# rhub は GitHub のコミットを検査するので働き木に依存しない
rhub::rhub_check(platforms = c("linux", "macos-arm64", "windows"))
devtools::check_win_devel(src, email = "kosugitti@gmail.com")

devtools::submit_cran(src)
