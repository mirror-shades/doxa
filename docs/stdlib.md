# Standard Library

## How to use
The standard library is imported like any other module. `@std()` is its specifier, the string `"std//std.doxa"` (the file `std.doxa` under the `std` root; see [Modules](modules.md#specifiers)).
```
module std from @std()
```

## Zig errors

`std.zerr.ZigError` is an ordinary enum holding every error name in the bundled
Zig standard library. It is opt-in:

```doxa
import zerr from @std()

const e is zerr.ZigError.AccessDenied
```

Like any enum, a `match` on it needs an `else` (or every variant). The enum is
generated from the bundled Zig sources by `zig build zerr`; `zig build test`
runs `check zerr` and fails when the committed `std/zerr/zerr.doxa` is stale.

A std function that can fail returns `T | zerr.ZigError`, the error named as
Zig names it (`ZigError.FileNotFound`, `ZigError.AccessDenied`). Nothing maps
Zig's errors onto a coarser enum. `zerr.StdError` is an ordinary group of
`ZigError` and Doxa's own runtime errors, `error.Method`, for code that
handles both:

```doxa
import zerr, error from @std()

function describe(e :: zerr.StdError) {
    match e {
        zerr.ZigError then @print("Zig: {e}\n"),
        error.Method then @print("Doxa: {e}\n"),
    }
}
```

No std function reports a result through state a later call reads: a second
result is a struct field, and an error is the returned union.

## Build

`std.build` drives the compiler from a build script. `execute(c)` compiles
every artifact in a `Context` and ends the script: it exits 0 when all compile,
with the compiler's status when one fails, and with 1 (after printing why)
when the compiler could not run or ended by a signal. `run(c)` does the same
work and returns the result instead, as `process.Term | zerr.ZigError`: how
the first failing compile ended, or the last one. A cross target with no OS is
`ZigError.MissingOperatingSystem`.

## File

`std.file` reads, writes and lists files and directories relative to the
working directory. A failing call returns `zerr.ZigError`, named as Zig names
it: `FileNotFound`, `PathAlreadyExists`, `NotDir`, `DirNotEmpty` (deleting a
non-empty directory without `recursive`), `AccessDenied`. An empty path is
`BadPathName`. `isDir`, `isFile` and `isPresent` answer a question and never
fail.

## Host


## HTTP

`std.http` provides a buffered client plus the loopback socket primitives that
the sequential server layer builds on.

```doxa
module std from @std()

function printResponse(response :: Response) {
    @print("status {response.statusCode()}\n")
    @print("{response.body()}\n")
    const content_types is response.header("content-type")
    if @length(content_types) > 0 then {
        @print("content type {content_types[0]}\n")
    }
}

const response is std.http.get("https://example.com/")
response as Response then {
    const value is response
    printResponse(value)
}
else {
    @print("request failed\n")
}
```

- `get(url)` returns `Response | zerr.ZigError`. HTTP status is data; a `404`
  still returns a response, and `getText(url)` returns the body regardless of
  status. A request that gets no response fails with the name Zig's client
  gives it: `ConnectionRefused`, `UnknownHostName`, `Timeout` when the deadline
  passes, `HttpConnectionClosing` when the server closes without answering,
  `TooManyHttpRedirects`, `HttpHeadersInvalid` for a header line that could
  break framing.
- `Request.new()` creates request settings with an empty `headers` array and
  defaults of a 30-second response-read timeout and three followed redirects.
  Each header entry is a `"Name: value"` string. Set `body`, `timeout_ms`, or
  `max_redirects` on the request before calling
  `Response.send(method, url, req)`. `post(url, ^req)`, `put(url, ^req)`,
  `delete(url, ^req)`, and `head(url, ^req)` are thin method-specific wrappers
  over it.

  ```doxa
  var req is std.http.Request.new()
  req.body is "hello"
  @push(req.headers, "Content-Type: text/plain")
  @push(req.headers, "X-Trace: demo")
  const response is std.http.post("https://example.com/submit", ^req)
  ```
- `Response.statusCode()`, `body()`, `header(name)`, and `hasHeader(name)` are
  accessors. `header` returns every matching value in order and compares names
  case-insensitively.
- `timeout_ms` and `getWithTimeout(url, timeout_ms)` bound the whole exchange:
  name resolution, connecting, the TLS handshake, sending, and receiving. A
  value of `0` disables the deadline.
- Redirects are followed up to `max_redirects` times (three by default); an
  exhausted limit is `ZigError.TooManyHttpRedirects`. `Authorization`, `Proxy-Authorization`,
  `Cookie`, and `Cookie2` are stripped when the redirect changes the strict
  origin (scheme, host, or effective port). Other custom headers continue to
  the destination.
- The server primitives bind only to loopback: `listen(port)` (use `0` for an
  ephemeral port), `localAddr(listener)`, `accept(listener)`, `close(handle)`,
  and the request/response pair `readRequest(connection)` /
  `ServerRequest.take(connection)` and
  `respond(connection, status, headers, body)`. `readRequest` is
  **non-blocking**: it returns `1` when a request is ready (read it with
  `ServerRequest.take`, a `ServerRequest` whose `.verb`, `.target`, and
  `.body_text` hold the parsed fields, with `header(name)` / `hasHeader(name)`
  reading repeated request headers case-insensitively), `0` when none has
  arrived yet (retry on the next `Readable` event), and `-1` once the
  connection has ended; it returns `int | zerr.ZigError`, the error being
  `InvalidHandle` for a handle the library does not know. A malformed or oversized request never reaches the
  program: the library answers it (400, 413, 417, or 431) and reports the
  connection `Closed`. `respond` must follow a successfully parsed request
  (otherwise it is `ZigError.NotOpenForWriting`); its
  `headers` argument is a CRLF-separated blob, and the library supplies
  `Content-Length` and connection framing. A client that asks for
  `Connection: close` gets it back, and `keepAlive(connection)` reports whether
  the request may reuse the connection. `accept(listener)` is the low-level
  primitive; a poll loop reads requests from `poll` events instead.

- `poll(listener, timeout_ms)` waits across the listener and every live
  connection at once and returns `Event[] | zerr.ZigError` (`InvalidHandle`
  for a listener that is not open). Each `Event` carries a connection
  `handle` and a `kind :: EventKind`: `Accepted` for a freshly accepted
  connection, `Readable` for a complete request or message waiting, and
  `Closed` once a connection has ended (`closeReason(connection)` says why).
  One `poll` reports a connection at most once, and keeps reporting it while
  messages remain unread. `timeout_ms == 0` polls immediately and a negative
  value blocks until an event; closing the listener closes its connections and
  is the way to stop a loop. Connections are served by background tasks, so a
  slow peer never holds up another; `setLimit(limit, value)` adjusts the caps
  and deadlines (open connections, header/body/message sizes, idle and read
  deadlines) for connections opened afterwards. A server serves many
  keep-alive clients without
  blocking on any one connection by polling instead of calling `accept`
  directly:

  ```doxa
  const listener is std.http.listen(0) as int else 0
  while true {
      const events is std.http.poll(listener, 1000) as std.http.Event[] else @panic("the listener closed")
      for i while i < @length(events) do i += 1 {
          const event is events[i]
          if event.kind == std.http.EventKind.Closed then {
              std.http.close(event.handle)
          } else {
              # 1 = request ready, 0 = wait for more bytes, -1 = ended.
              const status is std.http.readRequest(event.handle) as int else -1
              if status == 1 then {
                  const value is std.http.ServerRequest.take(event.handle)
                  std.http.respond(event.handle, 200, "Content-Type: text/plain\r\n", "hello {value.target}")
              } else if status < 0 then {
                  std.http.close(event.handle)
              }
          }
      }
  }
  ```

- `Router` matches request paths to integer route ids as data:
  `Router.new()`, `add(verb, pattern, id)`, and
  `router.find(verb, path) → Route | nothing`. A pattern segment written
  `:name` captures the matching path segment, and `Route.param(name)` reads the
  capture. Matching is exact per segment and verb, so `/users/:id` matches
  `/users/42` with `param("id") == "42"`, but not `/users/42/posts` or a
  different verb. (`{}` placeholders are not usable: Doxa string literals
  interpolate on `{`.)

  ```doxa
  var router is std.http.Router.new()
  router.add("GET", "/users/:id", 1)

  const matched is router.find("GET", "/users/42")
  matched as Route then {
      const id is matched.param("id") as string else ""
      @print("user {id}\n")
  }
  ```

- `download(url)` / `downloadUrl(url, destination)` use the same redirect,
  timeout, and status handling as requests, but stream response bytes directly
  to the destination file, and return the response status as
  `int | zerr.ZigError`. A non-2xx status is the server's answer, not a
  failure: its body is written like any other.
- WebSockets upgrade a connection after its handshake request has been read:
  `isWebSocket(connection)` reports whether the handshake has completed, and
  `upgradeWebSocket(connection)` completes the RFC 6455 handshake on a buffered
  request that asked to switch protocols. `wsSend(connection, op, data)` sends
  one frame; `wsNext(connection)` advances the decoder and returns `1` when a
  message is ready (read it with `wsMessage`, whose `op` is `WsOp.Text`,
  `Binary`, `Ping`, `Pong`, or `Close` and whose `data` is the payload), `0` when
  none is waiting (retry on the next `Readable` event), `2` when the peer
  closed, and `ZigError.ProtocolError` on a protocol or size violation. A
  frame sent to a connection that has ended is `ZigError.Closed`. Fragmented
  messages are reassembled before they are surfaced, and control frames are
  surfaced rather than handled silently, so the caller owns ping/pong/close
  policy. Frames are read in the background, so a partial frame from one peer
  cannot stall other connections. A WebSocket with no incoming frame for
  `Limit.IdleMs` (120 s by default) is closed as `CloseReason.IdleTimeout`;
  until heartbeats land, a peer stays open by sending something, a ping
  included, within it. A text message that is not valid UTF-8, a malformed close
  frame, or a frame larger than 16 MiB fails the connection with the
  corresponding Close code (`1007` / `1002` / `1009`).

  ```doxa
  const listener is std.http.listen(0) as int else 0
  while true {
      const events is std.http.poll(listener, 1000) as std.http.Event[] else @panic("the listener closed")
      for i while i < @length(events) do i += 1 {
          const event is events[i]
          if event.kind == std.http.EventKind.Closed then {
              std.http.close(event.handle)
          } else {
              if not std.http.isWebSocket(event.handle) then {
                  const status is std.http.readRequest(event.handle) as int else -1
                  if status == 1 then {
                      std.http.upgradeWebSocket(event.handle)
                  } else if status < 0 then {
                      std.http.close(event.handle)
                  }
              }
              if std.http.isWebSocket(event.handle) then {
                  var reading is true
                  while reading {
                      const ws is std.http.wsNext(event.handle) as int else -1
                      if ws == 1 then {
                          const message is std.http.wsMessage(event.handle)
                          if message.op == std.http.WsOp.Text then {
                              std.http.wsSend(event.handle, std.http.WsOp.Text, message.data)
                          } else if message.op == std.http.WsOp.Close then {
                              std.http.wsSend(event.handle, std.http.WsOp.Close, message.data)
                              std.http.close(event.handle)
                              reading is false
                          }
                      } else if ws == 2 or ws < 0 then {
                          std.http.close(event.handle)
                          reading is false
                      } else {
                          reading is false
                      }
                  }
              }
          }
      }
  }
  ```

  Most servers only need the Go-flavoured sugar; the primitives above stay
  public for full control:

  - `wsAccept(connection)` completes the handshake once the request is buffered
    and returns `true` once the connection is a WebSocket (it is a no-op after).
    It returns `false` while no request has arrived. A request that is not an
    upgrade is answered `426 Upgrade Required`.
  - `wsRead(connection)` returns the next `Message`, `nothing` when none is
    buffered yet or the peer closed, or an error on failure.
  - `wsWrite(connection, op, data)` sends a frame with an explicit opcode;
    `wsText`, `wsBinary`, `wsPing`, `wsPong` name the common kinds, and
    `wsClose(connection)` sends a Close frame and drops the connection.

  ```doxa
  const listener is std.http.listen(0) as int else 0
  while true {
      const events is std.http.poll(listener, 1000)
      for i while i < @length(events) do i += 1 {
          const event is events[i]
          if event.kind == std.http.EventKind.Closed then {
              std.http.close(event.handle)
          } else if std.http.wsAccept(event.handle) then {
              var reading is true
              while reading {
                  const m is std.http.wsRead(event.handle)
                  m as Message then {
                      const message is m
                      if message.op == std.http.WsOp.Text then {
                          std.http.wsText(event.handle, "echo: {message.data}")
                      } else if message.op == std.http.WsOp.Close then {
                          std.http.wsClose(event.handle)
                          reading is false
                      }
                  } else {
                      reading is false
                  }
              }
          }
      }
  }
  ```

- Client WebSockets (dialing out to `ws://` / `wss://`) are not available yet;
  see phase R4 of `plan/http-robustness.md`.


## IO

`std.io` writes to standard output and standard error and reads standard
input.

- `print(text)` and `println(text)` write to standard output, `eprint` and
  `eprintln` to standard error; the `ln` forms add the platform's line ending
  (`\r\n` on Windows). Each returns `nothing | zerr.ZigError`
  (`ZigError.WriteFailed` when the stream cannot be written).
- `input()` returns the next line without its `\n` or `\r\n`,
  `inputByte()` the next byte, and `inputPrompt(prompt)` prints `prompt` and
  then reads a line. At the end of input each returns `nothing`: running out of
  input is an outcome, not an error. A failed read is a `zerr.ZigError`
  (`ReadFailed`; `StreamTooLong` for a line longer than the 4 KiB input
  buffer).
- Every read goes through one buffered reader, so a byte read after a line
  sees what the line read buffered past it.

## JSON

`std.json` parses a document into a handle-based tree and writes one back out.
A `Node` is a small generation-checked handle into a single document slot, so
retaining one past the next `parse` is safe: it reads as `Kind.Invalid` rather
than a dangling value.

```doxa
module std from @std()

const result is std.json.Node.parse("{\"name\":\"doxa\",\"stars\":5}")
match result {
    std.json.Node then {
        const name is result.field("name")
        match name {
            std.json.Node then @print("{name.text()}\n")
            else @print("no name\n")
        }
    }
    else {
        @print("malformed\n")
    }
}
```

- `parse(text)` returns `Node | zerr.ZigError`, the error named as Zig's JSON
  parser names it: `SyntaxError` for a malformed document,
  `UnexpectedEndOfInput` for one that stops early, `DuplicateField` for a
  repeated key. Objects keep their key order.
- The writer refuses what that parser would reject, by the same name: a write
  the grammar does not allow (a value where a key was expected, an unbalanced
  `end()`, a second root value, NaN or infinity, a tetra's `both` or
  `neither`) is `SyntaxError`, and `finish()` with a container still open or
  nothing written is `UnexpectedEndOfInput`. Text that is not UTF-8 is
  `InvalidUtf8`.
- `kind()` never fails and returns a `Kind`: `Invalid`, `Null`, `Boolean`,
  `Integer`, `Number`, `Text`, `Array`, or `Object`. An integer that the runtime
  could not fit in `i64` or an exponent beyond the float range arrives as
  `Number` (`1e400` reads as infinity from `floatValue()`).
- `count()`, `element(index)`, `field(name)`, `key()`, `text()`, `intValue()`,
  `floatValue()`, and `booleanValue()` are strict: a wrong kind, a missing
  field, or an out-of-range index yields `nothing`. `count` works on arrays and
  objects; `field` only on objects; the value accessors only on the matching
  scalar kind.
- Writing mirrors the document shape: `beginObject()` / `beginArray()` open a
  container, `writeKey(name)` precedes a member value, and `end()` closes it.
  `finish()` returns the escaped text as `string | zerr.ZigError` and releases
  the writer; the next writer call starts a fresh document. A refused write
  refuses the rest of its document, and `finish()` reports it; the document
  after it starts clean.
- The writer emits `\b`, `\f`, `\n`, `\r`, `\t` as their two-character escapes
  and any other control byte below `0x20` as a lowercase `\u00xx` sequence. It
  validates UTF-8 before quoting and refuses NaN or infinity.

## Math

`std.math` holds the numeric limits. Each is a constant expression, so a call
folds to its value. Wrapping arithmetic is the `+%`, `-%` and `*%` operators,
not a library call ([Overflow and wrapping](math.md#overflow-and-wrapping)).

```doxa
module std from @std()

function inRange(x :: int, delta :: int) returns tetra {
    return delta <= 0 or x <= std.math.maxInt() - delta
}
```

- Limits: `maxInt()` and `minInt()` (`i64`), `maxByte()` and `minByte()`
  (`0xFF`, `0x00`), `maxFloat()` (the largest finite `float`), `minFloat()`
  (`-maxFloat()`, the lowest finite `float`), and `minNormalFloat()` (the
  smallest positive normal `float`).

## Methods



## Process

`std.process` runs other programs and reads the program's own arguments and
environment. Running a program comes in two forms: one passes a command line
to the platform shell, the other starts a program directly with its arguments.

```doxa
import process from @std()

# Portable: no shell, so the path and arguments arrive as written.
const run is process.execCaptureArgs("bin/tool", ["--version"]) as process.Run else {
    return
}
if run.term.succeeded() then @print(run.stdout)

# Shell: the command line is cmd.exe's on Windows, `sh`'s elsewhere.
const built is process.execOk("make all")
```

- **Shell commands.** `exec(command)` runs `command` and waits for it;
  `execCapture(command)` also captures what it writes; `execOk(command)` says
  whether it ran and exited 0. The text is handed to the platform shell
  unchanged: `cmd.exe` on Windows, `sh -c` elsewhere. Its grammar is that
  shell's: quoting, pipes, `%VAR%` or `$VAR`. On Windows `/` begins a switch,
  so an unquoted relative program path must use `\` (`bin\tool`) or be quoted
  (`"bin/tool"`); `bin/tool` runs `bin` with the switch `/tool`.
- **Programs.** `execArgs(program, arguments)` runs `program` and waits for
  it; `execCaptureArgs(program, arguments)` also captures what it writes. No
  shell is involved: `program` is a path (`/` or `\` on Windows) or a name
  looked up on `PATH`, and each argument arrives as one argument. These are
  the form to use for a path a program computed, such as a build script
  running what it just compiled. `execArgsEnv` and `execCaptureEnv` take two
  more arrays, `env_keys` and `env_values`, set in the child's environment on
  top of the parent's.
- **Results.** The `exec` forms return a `Term`, how the child ended: `kind`
  (`TermKind.Exited`, `Signal`, `Stopped` or `Unknown`, as Zig reports it) and
  `code`, the exit status or signal number. `term.succeeded()` is "exited with
  0". The capture forms return a `Run`: its `term`, and the child's `stdout`
  and `stderr`. A non-zero exit is part of the result, not an error.
- **Failures** are `zerr.ZigError` values, named as Zig names them: a program
  that does not exist is `ZigError.FileNotFound`, one that cannot run
  `ZigError.InvalidExe` or `ZigError.AccessDenied`. A failure means the
  program never ran (or could not be waited for); nothing of the result is
  built.
- **The program itself.** `args()` returns every argument, the program's own
  path first; `argc()` is how many, and `argv(index)` one of them, or
  `error.Method.OutOfBounds`. `cwd()` is the working directory (or a
  `ZigError`). `getenv(name)` is an environment variable as the program
  started with it, or `nothing` when it is not set. `exit(code)` ends the
  program with `code`; a POSIX parent sees its low byte.

## Random


## Time

