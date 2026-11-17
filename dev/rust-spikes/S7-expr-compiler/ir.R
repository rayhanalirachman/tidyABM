# Spike S7: compile a rule's expression (R AST) into the IR the Rust evaluator runs.
# Anything not covered raises a classed condition `ir_fallback` with the reason, so
# the rule takes the R path. A function name is accepted only if, in the quosure's
# environment, it is the very base / dplyr function (a user's own `pmin` is not).

ir_covered <- list(
  "+" = base::`+`, "-" = base::`-`, "*" = base::`*`, "/" = base::`/`,
  "<" = base::`<`, ">" = base::`>`, "<=" = base::`<=`, ">=" = base::`>=`,
  "==" = base::`==`, "!=" = base::`!=`, "&" = base::`&`, "|" = base::`|`, "!" = base::`!`,
  "(" = base::`(`, exp = base::exp, log = base::log, pmin = base::pmin, pmax = base::pmax,
  if_else = dplyr::if_else, case_when = dplyr::case_when, runif = stats::runif, n = dplyr::n)

ir_fallback <- function(reason) stop(structure(class = c("ir_fallback", "error", "condition"),
                                               list(message = reason, call = NULL)))

ir_node <- function(op, ...) list(op = op, ...)

compile_ir <- function(expr, env = globalenv()) {
  if (is.numeric(expr) && length(expr) == 1L && is.null(attributes(expr))) {
    if (is.na(expr)) ir_fallback("NA literal")
    return(ir_node("num", value = as.double(expr)))
  }
  if (is.logical(expr) && length(expr) == 1L) return(ir_node("lgl", value = expr))
  if (is.character(expr) && length(expr) == 1L && !is.na(expr)) return(ir_node("str", value = expr))
  if (is.symbol(expr)) return(ir_node("var", name = as.character(expr)))
  if (!is.call(expr) || !is.symbol(expr[[1]])) ir_fallback("not a plain call")
  fn <- as.character(expr[[1]])
  if (!fn %in% names(ir_covered)) ir_fallback(paste0("function `", fn, "` not covered"))
  # dplyr:: functions are reached through the mask's search path in the engine;
  # base ones through the quosure env. Either way it must be the real thing.
  found <- get0(fn, envir = env, mode = "function")
  if (!is.null(found) && !identical(found, ir_covered[[fn]])) ir_fallback(paste0("`", fn, "` is masked"))
  args <- as.list(expr)[-1]
  if (fn == "(") return(compile_ir(args[[1]], env))
  if (fn == "-" && length(args) == 1L) return(ir_node("call", fn = "neg", args = list(compile_ir(args[[1]], env))))
  if (fn == "case_when") {
    nms <- names(args) %||% rep("", length(args))
    if (any(!nms %in% c("", ".default"))) ir_fallback("case_when argument other than .default")
    default <- if (".default" %in% nms) compile_ir(args[[which(nms == ".default")]], env)
    pairs <- lapply(args[nms == ""], function(f) {
      if (!rlang::is_formula(f, lhs = TRUE)) ir_fallback("case_when argument is not a two-sided formula")
      list(cond = compile_ir(f[[2]], env), value = compile_ir(f[[3]], env))
    })
    return(ir_node("case_when", pairs = pairs, default = default))
  }
  if (!is.null(names(args)) && any(names(args) != "")) ir_fallback(paste0("named arguments to `", fn, "`"))
  if (fn == "runif" && !(length(args) == 1L && identical(args[[1]], quote(n())))) ir_fallback("runif other than runif(n())")
  if (fn == "log" && length(args) != 1L) ir_fallback("log with a base")
  ir_node("call", fn = fn, args = lapply(args, compile_ir, env = env))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
