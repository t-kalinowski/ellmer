devtools::load_all(".")
client <- chat_codex()
client$chat("What is this package")

client$chat("what is the current working directory?")

client <- chat_codex()
# Register a simple demo tool.
client$register_tool(tool(
  function() {
    "72 F"
  },
  name = "getTemperature",
  description = "Return the current temperature for demo purposes."
))

# Force a tool-use turn.
client$chat("Call the getTemperature tool and report the value.")

.Last.value
