pub type Configuration {
  Configuration(port: Int, fqn: String, home: String, col: Int)
}

@external(erlang, "Elixir.dotenv", "hex")
pub fn random_color() -> String

pub fn config() -> Configuration {
  todo
}
