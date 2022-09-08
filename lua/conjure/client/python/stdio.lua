local _2afile_2a = "fnl/conjure/client/python/stdio.fnl"
local _2amodule_name_2a = "conjure.client.python.stdio"
local _2amodule_2a
do
  package.loaded[_2amodule_name_2a] = {}
  _2amodule_2a = package.loaded[_2amodule_name_2a]
end
local _2amodule_locals_2a
do
  _2amodule_2a["aniseed/locals"] = {}
  _2amodule_locals_2a = (_2amodule_2a)["aniseed/locals"]
end
local autoload = (require("conjure.aniseed.autoload")).autoload
local a, client, config, extract, log, mapping, nvim, stdio, str, text, ts, _ = autoload("conjure.aniseed.core"), autoload("conjure.client"), autoload("conjure.config"), autoload("conjure.extract"), autoload("conjure.log"), autoload("conjure.mapping"), autoload("conjure.aniseed.nvim"), autoload("conjure.remote.stdio"), autoload("conjure.aniseed.string"), autoload("conjure.text"), autoload("conjure.tree-sitter"), nil
_2amodule_locals_2a["a"] = a
_2amodule_locals_2a["client"] = client
_2amodule_locals_2a["config"] = config
_2amodule_locals_2a["extract"] = extract
_2amodule_locals_2a["log"] = log
_2amodule_locals_2a["mapping"] = mapping
_2amodule_locals_2a["nvim"] = nvim
_2amodule_locals_2a["stdio"] = stdio
_2amodule_locals_2a["str"] = str
_2amodule_locals_2a["text"] = text
_2amodule_locals_2a["ts"] = ts
_2amodule_locals_2a["_"] = _
config.merge({client = {python = {stdio = {mapping = {start = "cs", stop = "cS", interrupt = "ei"}, command = "ipython -i --colors=NoColor", prompt_pattern = "In%s%[%d+%]:%s"}}}})
local cfg = config["get-in-fn"]({"client", "python", "stdio"})
do end (_2amodule_locals_2a)["cfg"] = cfg
local state
local function _1_()
  return {repl = nil}
end
state = ((_2amodule_2a).state or client["new-state"](_1_))
do end (_2amodule_locals_2a)["state"] = state
local buf_suffix = ".py"
_2amodule_2a["buf-suffix"] = buf_suffix
local comment_prefix = "# "
_2amodule_2a["comment-prefix"] = comment_prefix
local expression_output_pattern = "^Out%[%d+%]:%s"
_2amodule_2a["expression-output-pattern"] = expression_output_pattern
local function with_repl_or_warn(f, opts)
  local repl = state("repl")
  if repl then
    return f(repl)
  else
    return log.append({(comment_prefix .. "No REPL running"), (comment_prefix .. "Start REPL with " .. config["get-in"]({"mapping", "prefix"}) .. cfg({"mapping", "start"}))})
  end
end
_2amodule_locals_2a["with-repl-or-warn"] = with_repl_or_warn
local function prep_code(s)
  return (s .. "\n\n")
end
_2amodule_locals_2a["prep-code"] = prep_code
local function unbatch(msgs)
  local function _3_(_241)
    return (a.get(_241, "out") or a.get(_241, "err"))
  end
  return str.join("", a.map(_3_, msgs))
end
_2amodule_2a["unbatch"] = unbatch
local function format_msg(msg)
  local function _4_(_241)
    return ("" ~= _241)
  end
  return a.filter(_4_, str.split(msg, "\n"))
end
_2amodule_2a["format-msg"] = format_msg
local function is_expression_output_3f(m)
  if m:find(expression_output_pattern) then
    return true
  else
    return false
  end
end
_2amodule_locals_2a["is-expression-output?"] = is_expression_output_3f
local function remove_secondary_prompt(s)
  local function _7_()
    local _6_ = s:gsub("%.%.%.:", "")
    return _6_
  end
  return str.trim(_7_())
end
_2amodule_locals_2a["remove-secondary-prompt"] = remove_secondary_prompt
local function get_console_output_msgs(msgs)
  local function _8_(_241)
    return (comment_prefix .. "(out) " .. _241)
  end
  local function _9_(_241)
    return not is_expression_output_3f(_241)
  end
  local function _10_(_241)
    return not a["empty?"](_241)
  end
  return a.map(_8_, a.filter(_9_, a.filter(_10_, a.map(remove_secondary_prompt, msgs))))
end
_2amodule_locals_2a["get-console-output-msgs"] = get_console_output_msgs
local function extract_expression_output(m)
  local _11_ = m:gsub(expression_output_pattern, "")
  return _11_
end
_2amodule_locals_2a["extract-expression-output"] = extract_expression_output
local function get_expression_output(msgs)
  local results = a.filter(is_expression_output_3f, msgs)
  return a.map(extract_expression_output, results)
end
_2amodule_locals_2a["get-expression-output"] = get_expression_output
local function log_repl_output(msgs)
  local msgs0 = format_msg(unbatch(msgs))
  local console_outputs = get_console_output_msgs(msgs0)
  local expression_outputs = get_expression_output(msgs0)
  if not a["empty?"](console_outputs) then
    log.append(console_outputs)
  else
  end
  if not a["empty?"](expression_outputs) then
    return log.append(expression_outputs)
  else
    return nil
  end
end
_2amodule_locals_2a["log-repl-output"] = log_repl_output
local function eval_str(opts)
  local function _14_(repl)
    local function _15_(msgs)
      log_repl_output(msgs)
      if opts["on-result"] then
        return opts["on-result"](str.join(" ", msgs))
      else
        return nil
      end
    end
    return repl.send(prep_code(opts.code), _15_, {["batch?"] = true})
  end
  return with_repl_or_warn(_14_)
end
_2amodule_2a["eval-str"] = eval_str
local function eval_file(opts)
  return eval_str(a.assoc(opts, "code", a.slurp(opts["file-path"])))
end
_2amodule_2a["eval-file"] = eval_file
local function display_repl_status(status)
  local repl = state("repl")
  if repl then
    return log.append({(comment_prefix .. a["pr-str"](a["get-in"](repl, {"opts", "cmd"})) .. " (" .. status .. ")")}, {["break?"] = true})
  else
    return nil
  end
end
_2amodule_locals_2a["display-repl-status"] = display_repl_status
local function stop()
  local repl = state("repl")
  if repl then
    repl.destroy()
    display_repl_status("stopped")
    return a.assoc(state(), "repl", nil)
  else
    return nil
  end
end
_2amodule_2a["stop"] = stop
local function start()
  if state("repl") then
    return log.append({(comment_prefix .. "Can't start, REPL is already running."), (comment_prefix .. "Stop the REPL with " .. config["get-in"]({"mapping", "prefix"}) .. cfg({"mapping", "stop"}))}, {["break?"] = true})
  else
    local function _19_()
      return display_repl_status("started")
    end
    local function _20_(err)
      return display_repl_status(err)
    end
    local function _21_(code, signal)
      if (("number" == type(code)) and (code > 0)) then
        log.append({(comment_prefix .. "process exited with code " .. code)})
      else
      end
      if (("number" == type(signal)) and (signal > 0)) then
        log.append({(comment_prefix .. "process exited with signal " .. signal)})
      else
      end
      return stop()
    end
    local function _24_(msg)
      return log.dbg(format_msg(unbatch({msg})), {["join-first?"] = true})
    end
    return a.assoc(state(), "repl", stdio.start({["prompt-pattern"] = cfg({"prompt_pattern"}), cmd = cfg({"command"}), ["on-success"] = _19_, ["on-error"] = _20_, ["on-exit"] = _21_, ["on-stray-output"] = _24_}))
  end
end
_2amodule_2a["start"] = start
local function on_load()
  return start()
end
_2amodule_2a["on-load"] = on_load
local function on_exit()
  return stop()
end
_2amodule_2a["on-exit"] = on_exit
local function interrupt()
  local function _26_(repl)
    local uv = vim.loop
    return uv.kill(repl.pid, uv.constants.SIGINT)
  end
  return with_repl_or_warn(_26_)
end
_2amodule_2a["interrupt"] = interrupt
local function on_filetype()
  mapping.buf("n", "PythonStart", cfg({"mapping", "start"}), _2amodule_name_2a, "start")
  mapping.buf("n", "PythonStop", cfg({"mapping", "stop"}), _2amodule_name_2a, "stop")
  return mapping.buf("n", "PythonInterrupt", cfg({"mapping", "interrupt"}), _2amodule_name_2a, "interrupt")
end
_2amodule_2a["on-filetype"] = on_filetype
return _2amodule_2a