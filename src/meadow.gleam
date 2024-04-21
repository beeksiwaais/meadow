import gleam/bytes_builder
import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import glisten.{Packet}
import glisten/transport
import gleam/string
import gleam/list
import gleam/int
import gleam/bit_array
import gleam/result
import meadow/config
import gleam/io

type Entry {
  Entry(id: Int, message: String, path: String, host: String, port: Int)
}

pub type FileDescriptor {
  FileDescriptor(Int)
}

pub type FileError {
  IsDir
  NoAccess
  NoEntry
  UnknownFileError
}

pub fn string_or_empty(string: BitArray) -> String {
  bit_array.to_string(string)
  |> result.unwrap("")
}

pub fn main() {
  io.println("Starting Meadow")
  let config = config.load_from_dotenv()
  io.println("Root: " <> config.root)

  let assert Ok(_) =
    glisten.handler(fn(_conn) { #(Nil, None) }, fn(msg, state, conn) {
      let assert Packet(msg) = msg

      let assert Ok(_) =
        glisten.send(
          conn,
          bytes_builder.from_string(format_response(
            string_or_empty(msg),
            config,
          )),
        )

      let _ = transport.close(conn.transport, conn.socket)
      actor.continue(state)
    })
    |> glisten.serve(config.port)

  process.sleep_forever()
}

fn apply_gopher_format(entry: Entry) -> String {
  string.concat([
    int.to_string(entry.id),
    entry.message,
    "\t",
    entry.path,
    "\t",
    entry.host,
    "\t",
    int.to_string(entry.port),
    "\r\n",
  ])
}

fn retries_entries(path: String, root: String) -> List(Entry) {
  let gopher_path = string.replace(path, root, "")

  list_files(bit_array.from_string(path))
  |> result.unwrap(list.new())
  |> list.map(fn(file) {
    case dir_exists(bit_array.from_string(string.concat([path, file]))) {
      True ->
        Entry(1, file, string.concat([gopher_path, file]), "localhost", 70)
      False ->
        Entry(0, file, string.concat([gopher_path, file]), "localhost", 70)
    }
  })
}

fn format_response(msg: String, config: config.Configuration) -> String {
  let msg = string.replace(string.trim(msg), "..", "")
  let root = config.root
  let path = root <> msg <> "/"

  let formatted = case dir_exists(bit_array.from_string(path)) {
    True -> {
      retries_entries(path, config.root)
      |> list.map(fn(entry) { apply_gopher_format(entry) })
    }
    False -> {
      // if file render/send it
      [apply_gopher_format(Entry(3, "", "", "", 0))]
    }
  }

  string.concat(formatted) <> ".\r\n"
}

@external(erlang, "erlfile_ffi", "file_open")
pub fn open_file(file: BitArray) -> Result(FileDescriptor, FileError)

@external(erlang, "erlfile_ffi", "list_files")
pub fn list_files(path: BitArray) -> Result(List(String), FileError)

@external(erlang, "erlfile_ffi", "dir_exists")
pub fn dir_exists(path: BitArray) -> Bool
