import gleam/io
import gleam/bytes_builder
import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import glisten.{Packet}
import glisten/transport.{type Transport}
import gleam/string
import gleam/list
import gleam/int
import gleam/bit_array
import gleam/result

pub type Configuration {
  Configuration(port: Int, fqn: String)
}

type Entry {
  Entry(id: Int, message: String, path: String, host: String, port: Int)
}

pub type FileDescriptor

pub type FileError {
  IsDir
  NoAccess
  NoEntry
  UnknownFileError
}

pub fn main() {
  let assert Ok(_) =
    glisten.handler(fn(_conn) { #(Nil, None) }, fn(msg, state, conn) {
      let assert Packet(msg) = msg
      let lmsg =
        bit_array.to_string(msg)
        |> result.unwrap("")
      let assert Ok(_) =
        glisten.send(
          conn,
          bytes_builder.from_string(string.concat([render(lmsg), "\r\n."])),
        )

      let _ = transport.close(conn.transport, conn.socket)
      actor.continue(state)
    })
    |> glisten.serve(70)

  process.sleep_forever()
}

fn render(msg: String) -> String {
  let msg = string.trim(msg)
  let root = "/"
  let ignore_list = ["node_modules", ".git", ".DS_Store"]

  let entries =
    list_files(bit_array.from_string(string.concat([root, msg])))
    |> result.unwrap(list.new())
    |> list.filter(fn(file) { !list.contains(ignore_list, file) })
    |> list.map(fn(file) { Entry(1, file, file, "localhost", 70) })

  string.concat(
    entries
    |> list.map(fn(entry) {
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
    }),
  )
}

@external(erlang, "meadow_ffi", "file_open")
pub fn open_file(file: BitArray) -> Result(FileDescriptor, FileError)

@external(erlang, "meadow_ffi", "list_files")
pub fn list_files(path: BitArray) -> Result(List(String), FileError)