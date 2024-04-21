import dot_env
import dot_env/env
import gleam/result
import gleam/io
import gleam/bit_array

pub type Configuration {
  Configuration(port: Int, host: String, root: String, col: Int)
}

pub fn load_from_dotenv() -> Configuration {
  dot_env.load()
  io.debug(get_cwd())

  Configuration(
    port: env.get_int("PORT")
      |> result.unwrap(70),
    host: env.get("HOST")
      |> result.unwrap("HOST is not set"),
    root: env.get("ROOT_DIR")
      |> result.unwrap(
        bit_array.to_string(get_cwd())
        |> result.unwrap(""),
      ),
    col: env.get_int("COL")
      |> result.unwrap(80),
  )
}

@external(erlang, "erlfile_ffi", "cwd")
fn get_cwd() -> BitArray
