pacman::p_load(styler, devtools, rhub)
styler::style_pkg()
devtools::document()
devtools::spell_check()
devtools::check()
# rhub::rhub_doctor()
# Note: "macos" (Intel macos-13) removed because rhub resolves it to
# "macos-13-us-default" which is no longer available on GitHub Actions.
rhub::rhub_check(platforms = c("linux", "macos-arm64", "windows"))
devtools::check_win_devel(email = "kosugitti@gmail.com")

devtools::release()
