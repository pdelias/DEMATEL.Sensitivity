# A smoke test for the application, driven through a real browser.
#
# Why a browser and not shiny::testServer(): the two faults this exists to catch
# were both invisible to the server. `req()` stopped an observer silently, and
# shinyjs::click() sent a message to a client handler that was never registered
# because useShinyjs() was missing from the UI. Neither logged anything, on
# either side. Only pressing the buttons and looking at what changed finds them.
#
# It lives under .github/ on purpose. shinylive bundles whatever
# renv::dependencies() finds in the app directory, and a tests/ folder here
# puts testthat and chromote into the WebAssembly build -- measured, not
# assumed. renv skips dot-directories, so this file costs the bundle nothing.
#
# Run it locally exactly as CI does:
#   Rscript .github/smoke/smoke-test.R

library(chromote)

APP_PORT <- as.integer(Sys.getenv("SMOKE_PORT", "7801"))
APP_URL  <- sprintf("http://127.0.0.1:%d/", APP_PORT)

failures <- character()
passes   <- 0L

report <- function(ok, what, detail = "") {
  if (isTRUE(ok)) {
    passes <<- passes + 1L
    cat(sprintf("  ok    %s\n", what))
  } else {
    failures <<- c(failures, what)
    cat(sprintf("  FAIL  %s%s\n", what, if (nzchar(detail)) paste0("  [", detail, "]") else ""))
  }
}

# ---------------------------------------------------------------- the app ----

app <- processx::process$new(
  "Rscript",
  c("-e", sprintf(
    "shiny::runApp('.', port = %d, host = '127.0.0.1', launch.browser = FALSE)",
    APP_PORT)),
  stdout = "|", stderr = "2>&1"
)

reachable <- function(u) {
  con <- NULL
  ok <- tryCatch({
    con <- url(u, open = "rb")
    length(readBin(con, "raw", 1L)) > 0
  }, error = function(e) FALSE)
  if (!is.null(con)) try(close(con), silent = TRUE)
  isTRUE(ok)
}

cat("waiting for the app to listen ...\n")
up <- FALSE
for (i in 1:60) {
  if (reachable(APP_URL)) { up <- TRUE; break }
  Sys.sleep(1)
}
if (!up) {
  cat("app never started. Its output:\n")
  cat(app$read_all_output_lines(), sep = "\n")
  quit(status = 1)
}
cat("app is up on", APP_URL, "\n\n")

# ------------------------------------------------------------- the browser ---

b <- ChromoteSession$new()
js <- function(expr) {
  out <- b$Runtime$evaluate(expr, returnByValue = TRUE)
  if (!is.null(out$exceptionDetails)) {
    stop("JS error: ", out$exceptionDetails$text, " in: ", substr(expr, 1, 80))
  }
  out$result$value
}
# Poll until `expr` is truthy. Shiny is asynchronous and every assertion here
# is about a round trip, so a bare check would race the thing it is checking.
wait_for <- function(expr, timeout = 60, what = expr) {
  deadline <- Sys.time() + timeout
  repeat {
    val <- tryCatch(js(expr), error = function(e) NULL)
    if (isTRUE(val)) return(TRUE)
    if (Sys.time() > deadline) return(FALSE)
    Sys.sleep(0.5)
  }
}

b$Page$navigate(APP_URL)

cat("boot\n")
report(wait_for("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())",
                timeout = 90),
       "Shiny connects")

report(wait_for("!!document.getElementById('process_matrix')", timeout = 60),
       "the UI renders")

# The five R files announce themselves on load; a silent failure to source one
# would leave the app half-built and looking fine.
report(isTRUE(js("!!document.querySelector('.sidebar-menu')")),
       "the sidebar is present")

report(identical(js("document.title"), "Spectral DEMATEL"),
       "the page is titled Spectral DEMATEL",
       js("document.title"))

# ------------------------------------------------------ the working banner ---
#
# Driven by shiny:busy / shiny:idle, because under webR R blocks and nothing
# server-driven can repaint. Asserted by firing those events rather than by
# finding a slow computation: locally the work takes 30 ms and the banner is
# deliberately suppressed below 400 ms.
#
# It is asserted at all because a stray edit once left the CSS comment above
# these rules unterminated, which silently dropped `cursor: progress` -- a whole
# rule gone with nothing to show for it.

cat("\nthe working banner\n")

banner <- function() {
  js(paste0("(function(){return JSON.stringify({",
            "cls: document.body.classList.contains('sd-working'),",
            "pill: (getComputedStyle(document.body,'::after').content||'none') !== 'none',",
            "bar: getComputedStyle(document.body,'::before').height,",
            "cursor: getComputedStyle(document.body).cursor});})()"))
}

report(!grepl('"cls":true', banner()), "the banner is absent when idle", banner())

# The CSS half. This reads the rules out of the stylesheet rather than applying
# the class to a live element, because applying it is not deterministic either:
# the application's own shiny:idle handler removes the class, and on a fast
# runner idle fires often enough during startup to strip it between the add and
# the read. That failed this assertion once, after a previous version had
# already failed for the mirror-image reason.
#
# Reading the rules is what the assertion was for in any case. It exists because
# an unterminated comment once silently dropped `cursor: progress`, and a
# missing rule is missing whether or not anything is currently busy.
css <- js(paste0(
  "(function(){var out={};",
  "for (var i=0;i<document.styleSheets.length;i++){var rs;",
  "  try { rs = document.styleSheets[i].cssRules } catch(e) { continue }",
  "  for (var j=0;j<rs.length;j++){ var r=rs[j];",
  "    if(!r.selectorText) continue;",
  "    if(r.selectorText.indexOf('sd-working')===-1) continue;",
  "    out[r.selectorText] = r.style.cssText;",
  "  }}",
  "return JSON.stringify(out);})()"))

report(grepl("sd-working::before", css) && grepl("height: 3px", css),
       "the stylesheet defines the working bar", css)
report(grepl("sd-working::after", css) && grepl("content:", css),
       "the stylesheet defines the working message")
report(grepl("cursor: progress", css),
       "the stylesheet defines the busy cursor")

# The wiring half. This one is genuinely racy and the race is the feature: the
# handler waits 400 ms before showing anything, so a fast operation never
# flickers. On a CI runner under native R the application goes idle inside that
# window and cancels the pending timer -- which is correct behaviour, and which
# failed this assertion when it was written as a single attempt. Retry, and
# treat repeated interruption as a pass with a note rather than a failure,
# because "the work finished in under 400 ms" is not a defect.
saw_banner <- FALSE
for (attempt in 1:5) {
  invisible(js("$(document).trigger('shiny:busy')"))
  if (wait_for("document.body.classList.contains('sd-working')", timeout = 3)) {
    saw_banner <- TRUE
    break
  }
}
report(saw_banner, "a busy event raises the banner",
       "idle kept arriving inside the 400 ms delay")

if (saw_banner) {
  invisible(js("$(document).trigger('shiny:idle')"))
  report(wait_for("!document.body.classList.contains('sd-working')", timeout = 5),
         "the banner clears when the app goes idle")
} else {
  report(TRUE, "the banner clears when the app goes idle (not raised, nothing to clear)")
}

# ------------------------------------------- a button pressed with nothing ---
#
# Regression test: run_sensitivity used req(), which stops the observer in
# silence. The user saw no spinner, no message and no error, and concluded the
# tool was broken.

cat("\nguards\n")
invisible(js("[...document.querySelectorAll('.sidebar-menu a')].find(x => x.innerText.includes('Where to act')).click()"))
report(wait_for("!!document.getElementById('run_sensitivity')"),
       "the sensitivity tab renders")

invisible(js("document.getElementById('run_sensitivity').click()"))
report(wait_for("document.querySelectorAll('.shiny-notification').length > 0", timeout = 45),
       "pressing Run Sensitivity with no matrix says so")

# --------------------------------------------------- the example button -----
#
# Regression test: this called shinyjs::click() while shinyjs was neither
# loaded nor initialised, so it selected the example, switched tabs, and never
# started the analysis. Nothing was logged anywhere.

cat("\nthe example button\n")
invisible(js("[...document.querySelectorAll('.sidebar-menu a')].find(x => x.innerText.includes('Help')).click()"))
report(wait_for("!!document.getElementById('load_example_now')"),
       "the help tab renders")

invisible(js("document.getElementById('load_example_now').click()"))

report(wait_for("Shiny.shinyapp.$inputValues['process_matrix:shiny.action'] >= 1",
                timeout = 60),
       "the example button starts the analysis",
       paste("process_matrix =",
             js("String(Shiny.shinyapp.$inputValues['process_matrix:shiny.action'])")))

report(wait_for("Shiny.shinyapp.$inputValues['input_method'] === 'example'"),
       "the example button selects the example")

# --------------------------------------------------------- it computes ------

cat("\nthe diagnosis\n")
invisible(js("[...document.querySelectorAll('.sidebar-menu a')].find(x => x.innerText.includes('The diagnosis')).click()"))

report(wait_for(paste0("(function(){var e=document.getElementById('structure_map');",
                       "var i=e?e.querySelector('img'):null;",
                       "return !!i && i.naturalWidth > 100;})()"), timeout = 120),
       "the structure map renders from one press of the example button")

report(isTRUE(js("/[Cc]oupling/.test(document.body.innerText)")),
       "a verdict is on the page")

# No output anywhere may be in an error state, on any tab visited.
errs <- js("[...document.querySelectorAll('.shiny-output-error')].map(e => e.innerText.slice(0,120)).join(' | ')")
if (is.null(errs)) errs <- ""
report(!nzchar(errs), "no output is in an error state", errs)

# ------------------------------------------------------------------ done ----

invisible(b$close())
if (app$is_alive()) app$kill()

cat(sprintf("\n%d passed, %d failed\n", passes, length(failures)))
if (length(failures)) {
  cat("failed:\n"); cat(paste0("  - ", failures, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("smoke test passed\n")
