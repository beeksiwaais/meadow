import dot_env
import dot_env/env
import gleam/result

pub type Configuration {
  Configuration(port: Int, host: String, home: String, col: Int)
}

pub fn load_from_dotenv() -> Configuration {
  dot_env.load()
  Configuration(
    port: env.get_int("PORT")
      |> result.unwrap(70),
    host: env.get("HOST")
      |> result.unwrap("HOST is not set"),
    home: env.get("ROOT_DIR")
      |> result.unwrap("ROOT_DIR is not set"),
    col: env.get_int("COL")
      |> result.unwrap(80),
  )
}
