# Config from environment

Read configuration from environment variables, falling back to
defaults. A common pattern for twelve-factor-style services.

```kl
def env_or(name: String, fallback: String): String =
  if (Environment#exists(name)) Environment#get(name)
  else fallback

val host = env_or("APP_HOST", "0.0.0.0")
val port = env_or("APP_PORT", "8080")
val log_level = env_or("LOG_LEVEL", "info")

println("host = " + host)
println("port = " + port)
println("log_level = " + log_level)
```

Try it:

```bash
klassic build config.kl -o config
./config
# host = 0.0.0.0
# port = 8080
# log_level = info

APP_PORT=4000 LOG_LEVEL=debug ./config
# host = 0.0.0.0
# port = 4000
# log_level = debug
```

## Aliases

`Environment#get` / `Environment#exists` / `Environment#vars` all
have short aliases (`getEnv`, `hasEnv`, `env`):

```kl
def env_or(name: String, fallback: String): String =
  if (hasEnv(name)) getEnv(name) else fallback

println(env_or("USER", "anonymous"))
```

## Listing every variable

```kl
foreach (entry in env()) {
  println(entry)        // KEY=VALUE format
}
```
