# Standard Library API

> **Generated file.** Built from the Doxa standard library sources by
> `scripts/gen_stdlib_docs.zig`. Do not edit by hand; run `zig build docs`
> to regenerate it after changing anything under `std/`.

Each section is one standard-library module. Every entry shows its declared
signature, the description comment that precedes it in the source (when one
exists), and a collapsible copy of the full source declaration.

## `std.io`

### `print`

```doxa
public function print(text :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function print(text :: string) returns nothing | zerr.ZigError {
    return IO.print(text)
}
```

</details>

### `println`

```doxa
public function println(text :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function println(text :: string) returns nothing | zerr.ZigError {
    return IO.println(text)
}
```

</details>

### `eprint`

```doxa
public function eprint(text :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function eprint(text :: string) returns nothing | zerr.ZigError {
    return IO.eprint(text)
}
```

</details>

### `eprintln`

```doxa
public function eprintln(text :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function eprintln(text :: string) returns nothing | zerr.ZigError {
    return IO.eprintln(text)
}
```

</details>

### `input`

```doxa
public function input() returns string | nothing | zerr.ZigError
```

The next line of standard input, without its line ending, or `nothing` at
the end of input.

<details>
<summary>Source</summary>

```doxa
public function input() returns string | nothing | zerr.ZigError {
    const line is IO.input() as Line else {
        return line
    }
    if line.ended then return nothing
    return line.text
}
```

</details>

### `inputByte`

```doxa
public function inputByte() returns byte | nothing | zerr.ZigError
```

The next byte of standard input, or `nothing` at the end of input.

<details>
<summary>Source</summary>

```doxa
public function inputByte() returns byte | nothing | zerr.ZigError {
    const value is IO.inputByte() as int else {
        return value
    }
    if value < 0 then return nothing
    return @byte(value)
}
```

</details>

### `inputPrompt`

```doxa
public function inputPrompt(prompt :: string) returns string | nothing | zerr.ZigError
```

`print(prompt)`, then `input()`.

<details>
<summary>Source</summary>

```doxa
public function inputPrompt(prompt :: string) returns string | nothing | zerr.ZigError {
    const printed is print(prompt)
    printed as zerr.ZigError then return printed else {}
    return input()
}
```

</details>

## `std.process`

### `TermKind`

```doxa
public enum TermKind
```

How a child process ended, as Zig's `std.process.Child.Term` reports it.

<details>
<summary>Source</summary>

```doxa
public enum TermKind {
    Exited,
    Signal,
    Stopped,
    Unknown,
}
```

</details>

### `Term`

```doxa
public struct Term
```

How a child process ended. `code` is the exit status for `Exited`, the signal
number for `Signal` and `Stopped`, and the raw status for `Unknown`.

<details>
<summary>Source</summary>

```doxa
public struct Term {
    public kind :: TermKind,
    public code :: int,

    # Whether the child exited with status 0.
    public method succeeded() returns tetra {
        return this.kind == TermKind.Exited and this.code == 0
    }
}
```

</details>

#### `Term.succeeded`

```doxa
public method succeeded() returns tetra
```

Whether the child exited with status 0.

<details>
<summary>Source</summary>

```doxa
public method succeeded() returns tetra {
        return this.kind == TermKind.Exited and this.code == 0
    }
```

</details>

### `Run`

```doxa
public struct Run
```

A captured run: how the child ended and everything it wrote. A non-zero exit
is part of the result, not an error.

<details>
<summary>Source</summary>

```doxa
public struct Run {
    public term :: Term,
    public stdout :: string,
    public stderr :: string,
}
```

</details>

### `exec`

```doxa
public function exec(command :: string) returns Term | zerr.ZigError
```

Run `command` through the system shell (`sh -c`, or `cmd /c` on Windows).
The child shares this process's stdout and stderr.

<details>
<summary>Source</summary>

```doxa
public function exec(command :: string) returns Term | zerr.ZigError {
    return Process.exec(command)
}
```

</details>

### `execArgs`

```doxa
public function execArgs(program :: string, arguments :: string[]) returns Term | zerr.ZigError
```

Run `program` with `arguments`, no shell. The child shares this process's
stdout and stderr.

<details>
<summary>Source</summary>

```doxa
public function execArgs(program :: string, arguments :: string[]) returns Term | zerr.ZigError {
    return Process.execArgs(program, arguments, [], [])
}
```

</details>

### `execArgsEnv`

```doxa
public function execArgsEnv(program :: string, arguments :: string[], env_keys :: string[], env_values :: string[]) returns Term | zerr.ZigError
```

`execArgs` with `env_keys[i]` set to `env_values[i]` in the child's
environment, over this process's own.

<details>
<summary>Source</summary>

```doxa
public function execArgsEnv(program :: string, arguments :: string[], env_keys :: string[], env_values :: string[]) returns Term | zerr.ZigError {
    return Process.execArgs(program, arguments, env_keys, env_values)
}
```

</details>

### `execOk`

```doxa
public function execOk(command :: string) returns tetra
```

Whether `command` ran and exited with status 0.

<details>
<summary>Source</summary>

```doxa
public function execOk(command :: string) returns tetra {
    const term is exec(command) as Term else {
        return false
    }
    return term.succeeded()
}
```

</details>

### `execCapture`

```doxa
public function execCapture(command :: string) returns Run | zerr.ZigError
```

Run `command` through the system shell and capture what it writes.

<details>
<summary>Source</summary>

```doxa
public function execCapture(command :: string) returns Run | zerr.ZigError {
    return Process.capture(command)
}
```

</details>

### `execCaptureArgs`

```doxa
public function execCaptureArgs(program :: string, arguments :: string[]) returns Run | zerr.ZigError
```

Run `program` with `arguments`, no shell, and capture what it writes.

<details>
<summary>Source</summary>

```doxa
public function execCaptureArgs(program :: string, arguments :: string[]) returns Run | zerr.ZigError {
    return Process.captureArgs(program, arguments, [], [])
}
```

</details>

### `execCaptureEnv`

```doxa
public function execCaptureEnv(program :: string, arguments :: string[], env_keys :: string[], env_values :: string[]) returns Run | zerr.ZigError
```

`execCaptureArgs` with `env_keys[i]` set to `env_values[i]` in the child's
environment, over this process's own.

<details>
<summary>Source</summary>

```doxa
public function execCaptureEnv(program :: string, arguments :: string[], env_keys :: string[], env_values :: string[]) returns Run | zerr.ZigError {
    return Process.captureArgs(program, arguments, env_keys, env_values)
}
```

</details>

### `args`

```doxa
public function args() returns string[]
```

The program's command-line arguments, the program's own path first.

<details>
<summary>Source</summary>

```doxa
public function args() returns string[] {
    var result :: string[]
    const count is argc()
    for i while i < count do i += 1 {
        @push(result, Process.argAt(i))
    }
    return result
}
```

</details>

### `argc`

```doxa
public function argc() returns int
```

<details>
<summary>Source</summary>

```doxa
public function argc() returns int {
    return Process.argCount()
}
```

</details>

### `argv`

```doxa
public function argv(index :: int) returns string | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function argv(index :: int) returns string | error.Method {
    if index < 0 or index >= argc() then return error.Method.OutOfBounds
    return Process.argAt(index)
}
```

</details>

### `exit`

```doxa
public function exit(code :: int)
```

End the program with `code`; a POSIX parent sees its low byte.

<details>
<summary>Source</summary>

```doxa
public function exit(code :: int) {
    Process.exit(code)
}
```

</details>

### `cwd`

```doxa
public function cwd() returns string | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function cwd() returns string | zerr.ZigError {
    return Process.cwd()
}
```

</details>

### `getenv`

```doxa
public function getenv(name :: string) returns string | nothing
```

The variable `name` in the environment the program started with, or
`nothing` when it is not set.

<details>
<summary>Source</summary>

```doxa
public function getenv(name :: string) returns string | nothing {
    const env is Process.getEnv(name)
    if env.found then return env.value
    return nothing
}
```

</details>

## `std.file`

### `makeDir`

```doxa
public function makeDir(path :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function makeDir(path :: string) returns nothing | zerr.ZigError {
    return File.makeDir(path)
}
```

</details>

### `makePath`

```doxa
public function makePath(path :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function makePath(path :: string) returns nothing | zerr.ZigError {
    return File.makePath(path)
}
```

</details>

### `copyFile`

```doxa
public function copyFile(source :: string, destination :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function copyFile(source :: string, destination :: string) returns nothing | zerr.ZigError {
    return File.copyFile(source, destination)
}
```

</details>

### `delete`

```doxa
public function delete(path :: string, recursive :: tetra) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function delete(path :: string, recursive :: tetra) returns nothing | zerr.ZigError {
    return File.delete(path, recursive)
}
```

</details>

### `copyDir`

```doxa
public function copyDir(source :: string, destination :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function copyDir(source :: string, destination :: string) returns nothing | zerr.ZigError {
    return File.copyDir(source, destination)
}
```

</details>

### `unzipFile`

```doxa
public function unzipFile(zip_path :: string, unzip_path :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function unzipFile(zip_path :: string, unzip_path :: string) returns nothing | zerr.ZigError {
    return File.unzipFile(zip_path, unzip_path)
}
```

</details>

### `isDir`

```doxa
public function isDir(path :: string) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function isDir(path :: string) returns tetra {
    return File.isDir(path)
}
```

</details>

### `renameDir`

```doxa
public function renameDir(old_folder :: string, new_folder :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function renameDir(old_folder :: string, new_folder :: string) returns nothing | zerr.ZigError {
    return File.rename(old_folder, new_folder)
}
```

</details>

### `rename`

```doxa
public function rename(old_path :: string, new_path :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function rename(old_path :: string, new_path :: string) returns nothing | zerr.ZigError {
    return File.rename(old_path, new_path)
}
```

</details>

### `read`

```doxa
public function read(path :: string) returns string | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function read(path :: string) returns string | zerr.ZigError {
    return File.read(path)
}
```

</details>

### `write`

```doxa
public function write(path :: string, content :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function write(path :: string, content :: string) returns nothing | zerr.ZigError {
    return File.write(path, content)
}
```

</details>

### `append`

```doxa
public function append(path :: string, content :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function append(path :: string, content :: string) returns nothing | zerr.ZigError {
    return File.append(path, content)
}
```

</details>

### `isPresent`

```doxa
public function isPresent(path :: string) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function isPresent(path :: string) returns tetra {
    return File.isPresent(path)
}
```

</details>

### `isFile`

```doxa
public function isFile(path :: string) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function isFile(path :: string) returns tetra {
    return File.isFile(path)
}
```

</details>

### `list`

```doxa
public function list(path :: string) returns string[] | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function list(path :: string) returns string[] | zerr.ZigError {
    return File.list(path)
}
```

</details>

### `touch`

```doxa
public function touch(path :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function touch(path :: string) returns nothing | zerr.ZigError {
    return File.touch(path)
}
```

</details>

## `std.http`

### `downloadUrl`

```doxa
public function downloadUrl(url :: string, destination :: string) returns int | zerr.ZigError
```

Stream `url`'s body into the file `destination` and return the response
status. A non-2xx status is the server's answer, not a failure: its body is
written like any other.

<details>
<summary>Source</summary>

```doxa
public function downloadUrl(url :: string, destination :: string) returns int | zerr.ZigError {
    return HTTP.downloadUrl(url, destination)
}
```

</details>

### `download`

```doxa
public function download(url :: string) returns int | zerr.ZigError
```

`downloadUrl` into the file `url` names, in the working directory.

<details>
<summary>Source</summary>

```doxa
public function download(url :: string) returns int | zerr.ZigError {
    return HTTP.download(url)
}
```

</details>

### `Response`

```doxa
public struct Response
```

<details>
<summary>Source</summary>

```doxa
public struct Response {
    status_code :: int,
    raw_headers :: string,
    body_text :: string,

    /* Perform one request. The error arm means no response arrived; a non-2xx
       status is still a `Response`. */
    public function send(verb :: string, url :: string, req :: Request) returns Response | zerr.ZigError {
        var header_blob is ""
        for i while i < @length(req.headers) do i += 1 {
            header_blob is header_blob + req.headers[i] + "\u{d}\u{a}"
        }
        return HTTP.performRequest(verb, url, header_blob, req.body, req.timeout_ms, req.max_redirects)
    }

    public method statusCode() returns int {
        return this.status_code
    }

    public method body() returns string {
        return this.body_text
    }

    public method header(name :: string) returns string[] {
        var values :: string[]
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then {
                @push(values, HTTP.headerValueAt(this.raw_headers, i))
            }
        }
        return values
    }

    public method hasHeader(name :: string) returns tetra {
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then return true
        }
        return false
    }
}
```

</details>

#### `Response.send`

```doxa
public function send(verb :: string, url :: string, req :: Request) returns Response | zerr.ZigError
```

Perform one request. The error arm means no response arrived; a non-2xx
status is still a `Response`.

<details>
<summary>Source</summary>

```doxa
public function send(verb :: string, url :: string, req :: Request) returns Response | zerr.ZigError {
        var header_blob is ""
        for i while i < @length(req.headers) do i += 1 {
            header_blob is header_blob + req.headers[i] + "\u{d}\u{a}"
        }
        return HTTP.performRequest(verb, url, header_blob, req.body, req.timeout_ms, req.max_redirects)
    }
```

</details>

#### `Response.statusCode`

```doxa
public method statusCode() returns int
```

<details>
<summary>Source</summary>

```doxa
public method statusCode() returns int {
        return this.status_code
    }
```

</details>

#### `Response.body`

```doxa
public method body() returns string
```

<details>
<summary>Source</summary>

```doxa
public method body() returns string {
        return this.body_text
    }
```

</details>

#### `Response.header`

```doxa
public method header(name :: string) returns string[]
```

<details>
<summary>Source</summary>

```doxa
public method header(name :: string) returns string[] {
        var values :: string[]
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then {
                @push(values, HTTP.headerValueAt(this.raw_headers, i))
            }
        }
        return values
    }
```

</details>

#### `Response.hasHeader`

```doxa
public method hasHeader(name :: string) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public method hasHeader(name :: string) returns tetra {
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then return true
        }
        return false
    }
```

</details>

### `listen`

```doxa
public function listen(port :: int) returns int | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function listen(port :: int) returns int | zerr.ZigError {
    return HTTP.listen(port)
}
```

</details>

### `localAddr`

```doxa
public function localAddr(handle :: int) returns int | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function localAddr(handle :: int) returns int | zerr.ZigError {
    return HTTP.localAddr(handle)
}
```

</details>

### `accept`

```doxa
public function accept(handle :: int) returns int | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function accept(handle :: int) returns int | zerr.ZigError {
    return HTTP.accept(handle)
}
```

</details>

### `ServerRequest`

```doxa
public struct ServerRequest
```

<details>
<summary>Source</summary>

```doxa
public struct ServerRequest {
    public verb :: string,
    public target :: string,
    public body_text :: string,
    raw_headers :: string,

    /* The request `readRequest` reported. It stays available until `respond`. */
    public function take(connection :: int) returns ServerRequest {
        return $ServerRequest {
            verb is HTTP.requestMethod(connection),
            target is HTTP.requestTarget(connection),
            body_text is HTTP.requestBody(connection),
            raw_headers is HTTP.requestHead(connection),
        }
    }

    public method path() returns string {
        return this.target
    }

    public method body() returns string {
        return this.body_text
    }

    public method header(name :: string) returns string[] {
        var values :: string[]
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then {
                @push(values, HTTP.headerValueAt(this.raw_headers, i))
            }
        }
        return values
    }

    public method hasHeader(name :: string) returns tetra {
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then return true
        }
        return false
    }
}
```

</details>

#### `ServerRequest.take`

```doxa
public function take(connection :: int) returns ServerRequest
```

The request `readRequest` reported. It stays available until `respond`.

<details>
<summary>Source</summary>

```doxa
public function take(connection :: int) returns ServerRequest {
        return $ServerRequest {
            verb is HTTP.requestMethod(connection),
            target is HTTP.requestTarget(connection),
            body_text is HTTP.requestBody(connection),
            raw_headers is HTTP.requestHead(connection),
        }
    }
```

</details>

#### `ServerRequest.path`

```doxa
public method path() returns string
```

<details>
<summary>Source</summary>

```doxa
public method path() returns string {
        return this.target
    }
```

</details>

#### `ServerRequest.body`

```doxa
public method body() returns string
```

<details>
<summary>Source</summary>

```doxa
public method body() returns string {
        return this.body_text
    }
```

</details>

#### `ServerRequest.header`

```doxa
public method header(name :: string) returns string[]
```

<details>
<summary>Source</summary>

```doxa
public method header(name :: string) returns string[] {
        var values :: string[]
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then {
                @push(values, HTTP.headerValueAt(this.raw_headers, i))
            }
        }
        return values
    }
```

</details>

#### `ServerRequest.hasHeader`

```doxa
public method hasHeader(name :: string) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public method hasHeader(name :: string) returns tetra {
        const count is HTTP.headerCount(this.raw_headers)
        for i while i < count do i += 1 {
            if HTTP.headerNameMatches(this.raw_headers, i, name) then return true
        }
        return false
    }
```

</details>

### `readRequest`

```doxa
public function readRequest(connection :: int) returns int | zerr.ZigError
```

Whether a request is waiting on the connection. Returns `1` when one is
(read it with `ServerRequest.take`), `0` when none has arrived yet (retry on the
next `Readable` event), and a negative status once the connection has ended.
A request only appears here complete and valid: a malformed or oversized one
is answered by the library and the connection reports `Closed`. An unknown
handle is `ZigError.InvalidHandle`.

<details>
<summary>Source</summary>

```doxa
public function readRequest(connection :: int) returns int | zerr.ZigError {
    return HTTP.readRequest(connection)
}
```

</details>

### `isWebSocket`

```doxa
public function isWebSocket(connection :: int) returns tetra
```

Whether the connection has completed the WebSocket handshake.

<details>
<summary>Source</summary>

```doxa
public function isWebSocket(connection :: int) returns tetra {
    return HTTP.isWebSocket(connection)
}
```

</details>

### `keepAlive`

```doxa
public function keepAlive(connection :: int) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function keepAlive(connection :: int) returns tetra {
    return HTTP.keepAlive(connection)
}
```

</details>

### `WsOp`

```doxa
public enum WsOp
```

WebSocket frame kinds. The inline-Zig boundary only carries scalars, so the
codes crossing it are the RFC 6455 opcodes and `wsOpFromCode`/`wsOpCode`
translate to and from this enum.

<details>
<summary>Source</summary>

```doxa
public enum WsOp {
    Text,
    Binary,
    Ping,
    Pong,
    Close,
}
```

</details>

### `Message`

```doxa
public struct Message
```

One decoded WebSocket message. Data frames carry their payload; control
frames (ping/pong/close) are surfaced so the caller owns the policy.

<details>
<summary>Source</summary>

```doxa
public struct Message {
    public op :: WsOp,
    public data :: string,
}
```

</details>

### `upgradeWebSocket`

```doxa
public function upgradeWebSocket(connection :: int) returns nothing | zerr.ZigError
```

Accept the pending request's upgrade to a WebSocket (RFC 6455). Call after
`readRequest`. The handshake is written in the background; `wsSend` may be
called at once and its frames follow the handshake.

<details>
<summary>Source</summary>

```doxa
public function upgradeWebSocket(connection :: int) returns nothing | zerr.ZigError {
    return HTTP.upgradeWebSocket(connection)
}
```

</details>

### `wsSend`

```doxa
public function wsSend(connection :: int, op :: WsOp, data :: string) returns nothing | zerr.ZigError
```

Queue one WebSocket frame with `data` as its payload. Returns at once: a
peer that is slow to read never blocks the caller. A peer that falls further
behind than `Limit.OutboundBytes` is closed with `CloseReason.SlowConsumer`
and the call returns `ZigError.WriteFailed`. A connection that has ended is
`ZigError.Closed`.

<details>
<summary>Source</summary>

```doxa
public function wsSend(connection :: int, op :: WsOp, data :: string) returns nothing | zerr.ZigError {
    return HTTP.wsSend(connection, wsOpCode(op), data)
}
```

</details>

### `wsNext`

```doxa
public function wsNext(connection :: int) returns int | zerr.ZigError
```

Take the connection's next message. Returns `1` when one is ready (read it
with `wsMessage`), `0` when none is waiting (retry on the next `Readable`
event), and `2` once the peer has closed. A message the peer broke (the
protocol or a size limit) is `ZigError.ProtocolError`. Messages arrive
whole: fragments are reassembled and text is validated before they are
surfaced.

<details>
<summary>Source</summary>

```doxa
public function wsNext(connection :: int) returns int | zerr.ZigError {
    return HTTP.wsNext(connection)
}
```

</details>

### `wsMessage`

```doxa
public function wsMessage(connection :: int) returns Message
```

The message decoded by the last successful `wsNext`.

<details>
<summary>Source</summary>

```doxa
public function wsMessage(connection :: int) returns Message {
    return $Message {
        op is wsOpFromCode(HTTP.wsMessageOp(connection)),
        data is HTTP.wsMessageData(connection),
    }
}
```

</details>

### `wsPending`

```doxa
public function wsPending(connection :: int) returns int
```

How many messages are waiting for `wsNext`. The library stops reading from
a connection once `Limit.InboxMessages` are waiting, so this never grows
past it.

<details>
<summary>Source</summary>

```doxa
public function wsPending(connection :: int) returns int {
    return HTTP.wsPending(connection)
}
```

</details>

### `wsBuffered`

```doxa
public function wsBuffered(connection :: int) returns tetra
```

Whether messages are waiting for `wsNext`.

<details>
<summary>Source</summary>

```doxa
public function wsBuffered(connection :: int) returns tetra {
    return HTTP.wsPending(connection) > 0
}
```

</details>

### `EventKind`

```doxa
public enum EventKind
```

Why a `poll` event fired. `Accepted` is a fresh connection. `Readable` is a
connection with a complete request or message waiting. `Closed` is a
connection that has ended: it is reported once, after everything else from
that connection, and `closeReason` says why. The library has already cleaned
up; calling `close` afterwards is allowed and does nothing.

<details>
<summary>Source</summary>

```doxa
public enum EventKind {
    Accepted,
    Readable,
    Closed,
}
```

</details>

### `Event`

```doxa
public struct Event
```

One readiness event from `poll`. `handle` is the connection (or listener)
handle.

<details>
<summary>Source</summary>

```doxa
public struct Event {
    public handle :: int,
    public kind :: EventKind,
}
```

</details>

### `poll`

```doxa
public function poll(listener :: int, timeout_ms :: int) returns Event[] | zerr.ZigError
```

Wait for events on the listener's connections. Blocks up to `timeout_ms`
(0 does not wait, negative waits indefinitely) and returns every event that
is ready. Connections are read in the background whether or not the program
is polling; `poll` only collects what is finished. An unknown listener is
`ZigError.InvalidHandle`.

<details>
<summary>Source</summary>

```doxa
public function poll(listener :: int, timeout_ms :: int) returns Event[] | zerr.ZigError {
    const polled is HTTP.poll(listener, timeout_ms) as Polled else {
        return polled
    }
    var events :: Event[]
    each handle at i in polled.handles {
        @push(events, $Event {
            handle is handle,
            kind is eventKindFromCode(polled.kinds[i]),
        })
    }
    return events
}
```

</details>

### `CloseReason`

```doxa
public enum CloseReason
```

Why a connection ended.

<details>
<summary>Source</summary>

```doxa
public enum CloseReason {
    Open,
    PeerClosed,
    Reset,
    LocalClose,
    IdleTimeout,
    ReadTimeout,
    WriteTimeout,
    HandlerTimeout,
    SlowConsumer,
    TooLarge,
    ProtocolError,
    HandshakeFailed,
    ConnectFailed,
    TlsError,
}
```

</details>

### `closeReason`

```doxa
public function closeReason(connection :: int) returns CloseReason
```

Why `connection` ended; `CloseReason.Open` while it has not. Valid from the
`Closed` event until the next `poll`. A connection the program closed itself
reads as `LocalClose`.

<details>
<summary>Source</summary>

```doxa
public function closeReason(connection :: int) returns CloseReason {
    return closeReasonFromCode(HTTP.closeReason(connection))
}
```

</details>

### `Limit`

```doxa
public enum Limit
```

The caps and deadlines every connection runs under. Sizes are in bytes and
deadlines in milliseconds; a deadline of 0 is disabled.

- `HeadBytes`: a request's headers. Over it: `431`, then `TooLarge`.
- `BodyBytes`: a request's body. Over it: `413`, then `TooLarge`.
- `MessageBytes`: one reassembled WebSocket message. Over it: `TooLarge`.
- `OutboundBytes`: frames queued for one peer. Over it: `SlowConsumer`.
- `InboxMessages`: messages one connection may have waiting for the program
before the library stops reading from it.
- `PendingBytes`: bytes waiting for the program across all connections
before the library stops reading.
- `Connections`: open connections; further ones wait to be accepted.
- `HandshakeMs`: opening a client connection.
- `HeadMs`: the rest of a request's headers once they have started.
- `ReadMs`: a body or message making no progress.
- `WriteMs`: a send making no progress.
- `IdleMs`: a kept-alive connection with no request, or a WebSocket with
no incoming frame. Until heartbeats land, a WebSocket peer stays open by
sending something, a ping included, within it.
- `HandlerMs`: the program answering a request it was given.
- `LingerMs`: the peer acknowledging a close the library started.

<details>
<summary>Source</summary>

```doxa
public enum Limit {
    HeadBytes,
    BodyBytes,
    MessageBytes,
    OutboundBytes,
    InboxMessages,
    PendingBytes,
    Connections,
    HandshakeMs,
    HeadMs,
    ReadMs,
    WriteMs,
    IdleMs,
    HandlerMs,
    LingerMs,
}
```

</details>

### `setLimit`

```doxa
public function setLimit(which :: Limit, value :: int) returns nothing | zerr.ZigError
```

Set one limit for connections opened from now on. A negative value is
`ZigError.Overflow`: no size or deadline is below zero. `Limit`'s order is
the engine's `LimitKey`.

<details>
<summary>Source</summary>

```doxa
public function setLimit(which :: Limit, value :: int) returns nothing | zerr.ZigError {
    return HTTP.setLimit(which, value)
}
```

</details>

### `Route`

```doxa
public struct Route
```

<details>
<summary>Source</summary>

```doxa
public struct Route {
    public id :: int,
    public pattern :: string,
    param_names :: string[],
    param_values :: string[],

    /* `pattern` matched against `path`, segment by segment: a `:name` segment
       captures the path's segment as `name`, any other must be equal, and the
       two must have as many segments. `nothing` when they do not match. */
    public function matching(id :: int, pattern :: string, path :: string) returns Route | nothing {
        const wanted is segments(pattern)
        const given is segments(path)
        if @length(wanted) != @length(given) then return nothing
        var names :: string[]
        var values :: string[]
        each part at i in wanted {
            if @length(part) >= 2 and @slice(part, 0, 1) == ":" then {
                @push(names, @slice(part, 1, @length(part) - 1))
                @push(values, given[i])
            } else if part != given[i] then {
                return nothing
            }
        }
        return $Route {
            id is id,
            pattern is pattern,
            param_names is names,
            param_values is values,
        }
    }

    public method param(name :: string) returns string | nothing {
        for i while i < @length(this.param_names) do i += 1 {
            if this.param_names[i] == name then {
                const found is this.param_values[i]
                return found
            }
        }
        return nothing
    }
}
```

</details>

#### `Route.matching`

```doxa
public function matching(id :: int, pattern :: string, path :: string) returns Route | nothing
```

`pattern` matched against `path`, segment by segment: a `:name` segment
captures the path's segment as `name`, any other must be equal, and the
two must have as many segments. `nothing` when they do not match.

<details>
<summary>Source</summary>

```doxa
public function matching(id :: int, pattern :: string, path :: string) returns Route | nothing {
        const wanted is segments(pattern)
        const given is segments(path)
        if @length(wanted) != @length(given) then return nothing
        var names :: string[]
        var values :: string[]
        each part at i in wanted {
            if @length(part) >= 2 and @slice(part, 0, 1) == ":" then {
                @push(names, @slice(part, 1, @length(part) - 1))
                @push(values, given[i])
            } else if part != given[i] then {
                return nothing
            }
        }
        return $Route {
            id is id,
            pattern is pattern,
            param_names is names,
            param_values is values,
        }
    }
```

</details>

#### `Route.param`

```doxa
public method param(name :: string) returns string | nothing
```

<details>
<summary>Source</summary>

```doxa
public method param(name :: string) returns string | nothing {
        for i while i < @length(this.param_names) do i += 1 {
            if this.param_names[i] == name then {
                const found is this.param_values[i]
                return found
            }
        }
        return nothing
    }
```

</details>

### `Router`

```doxa
public struct Router
```

<details>
<summary>Source</summary>

```doxa
public struct Router {
    verbs :: string[],
    patterns :: string[],
    ids :: int[],

    public function new() returns Router {
        return $Router {
            verbs is [],
            patterns is [],
            ids is [],
        }
    }

    public method add(verb :: string, pattern :: string, id :: int) {
        @push(this.verbs, verb)
        @push(this.patterns, pattern)
        @push(this.ids, id)
    }

    /* The first route added for `verb` whose pattern matches `path`, or
       `nothing`. A `:name` pattern segment captures that path segment. */
    public method find(verb :: string, path :: string) returns Route | nothing {
        for i while i < @length(this.patterns) do i += 1 {
            if this.verbs[i] == verb then {
                const route is Route.matching(this.ids[i], this.patterns[i], path)
                route as Route then return route else {}
            }
        }
        return nothing
    }
}
```

</details>

#### `Router.new`

```doxa
public function new() returns Router
```

<details>
<summary>Source</summary>

```doxa
public function new() returns Router {
        return $Router {
            verbs is [],
            patterns is [],
            ids is [],
        }
    }
```

</details>

#### `Router.add`

```doxa
public method add(verb :: string, pattern :: string, id :: int)
```

<details>
<summary>Source</summary>

```doxa
public method add(verb :: string, pattern :: string, id :: int) {
        @push(this.verbs, verb)
        @push(this.patterns, pattern)
        @push(this.ids, id)
    }
```

</details>

#### `Router.find`

```doxa
public method find(verb :: string, path :: string) returns Route | nothing
```

The first route added for `verb` whose pattern matches `path`, or
`nothing`. A `:name` pattern segment captures that path segment.

<details>
<summary>Source</summary>

```doxa
public method find(verb :: string, path :: string) returns Route | nothing {
        for i while i < @length(this.patterns) do i += 1 {
            if this.verbs[i] == verb then {
                const route is Route.matching(this.ids[i], this.patterns[i], path)
                route as Route then return route else {}
            }
        }
        return nothing
    }
```

</details>

### `respond`

```doxa
public function respond(connection :: int, status :: int, headers :: string, body :: string) returns nothing | zerr.ZigError
```

Answer the pending request. With no request waiting, or one already
answered, the call is `ZigError.NotOpenForWriting`; a status that is not
three digits, or a header line that could break framing, is
`ZigError.HttpHeadersInvalid`.

<details>
<summary>Source</summary>

```doxa
public function respond(connection :: int, status :: int, headers :: string, body :: string) returns nothing | zerr.ZigError {
    return HTTP.respond(connection, status, headers, body)
}
```

</details>

### `close`

```doxa
public function close(handle :: int) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function close(handle :: int) returns nothing | zerr.ZigError {
    return HTTP.close(handle)
}
```

</details>

### `Request`

```doxa
public struct Request
```

<details>
<summary>Source</summary>

```doxa
public struct Request {
    public headers :: string[],
    public body :: string,
    public timeout_ms :: int,
    public max_redirects :: int,

    public function new() returns Request {
        return $Request {
            headers is [],
            body is "",
            timeout_ms is 30000,
            max_redirects is 3,
        }
    }
}
```

</details>

#### `Request.new`

```doxa
public function new() returns Request
```

<details>
<summary>Source</summary>

```doxa
public function new() returns Request {
        return $Request {
            headers is [],
            body is "",
            timeout_ms is 30000,
            max_redirects is 3,
        }
    }
```

</details>

### `get`

```doxa
public function get(url :: string) returns Response | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function get(url :: string) returns Response | zerr.ZigError {
    var req is Request.new()
    return Response.send("GET", url, req)
}
```

</details>

### `getWithTimeout`

```doxa
public function getWithTimeout(url :: string, timeout_ms :: int) returns Response | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function getWithTimeout(url :: string, timeout_ms :: int) returns Response | zerr.ZigError {
    var req is Request.new()
    req.timeout_ms is timeout_ms
    return Response.send("GET", url, req)
}
```

</details>

### `post`

```doxa
public function post(url :: string, ^req :: Request) returns Response | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function post(url :: string, ^req :: Request) returns Response | zerr.ZigError {
    return Response.send("POST", url, req)
}
```

</details>

### `put`

```doxa
public function put(url :: string, ^req :: Request) returns Response | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function put(url :: string, ^req :: Request) returns Response | zerr.ZigError {
    return Response.send("PUT", url, req)
}
```

</details>

### `delete`

```doxa
public function delete(url :: string, ^req :: Request) returns Response | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function delete(url :: string, ^req :: Request) returns Response | zerr.ZigError {
    return Response.send("DELETE", url, req)
}
```

</details>

### `head`

```doxa
public function head(url :: string, ^req :: Request) returns Response | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function head(url :: string, ^req :: Request) returns Response | zerr.ZigError {
    return Response.send("HEAD", url, req)
}
```

</details>

### `getText`

```doxa
public function getText(url :: string) returns string | zerr.ZigError
```

The body of a GET, whatever its status.

<details>
<summary>Source</summary>

```doxa
public function getText(url :: string) returns string | zerr.ZigError {
    const response is get(url)
    response as Response then return response.body()
    else return response
}
```

</details>

### `wsAccept`

```doxa
public function wsAccept(connection :: int) returns tetra
```

Complete the handshake if `connection`'s buffered request is a WebSocket
upgrade. Returns `true` once it is a WebSocket; while it is still `false`,
call again after the next readable event. A request that is not an upgrade
is answered `426 Upgrade Required`, so its client is never left waiting.

<details>
<summary>Source</summary>

```doxa
public function wsAccept(connection :: int) returns tetra {
    if isWebSocket(connection) then return true
    const status is readRequest(connection) as int else {
        return false
    }
    if status != 1 then return false
    const upgraded is upgradeWebSocket(connection)
    upgraded as zerr.ZigError then {} else return true
    respond(connection, 426, "Upgrade: websocket\u{d}\u{a}Sec-WebSocket-Version: 13", "")
    return false
}
```

</details>

### `wsRead`

```doxa
public function wsRead(connection :: int) returns Message | nothing | zerr.ZigError
```

The next complete message, `nothing` when none is buffered yet or the peer
closed, or an error on a protocol/transport failure. Fragmented messages are
reassembled before they are surfaced.

<details>
<summary>Source</summary>

```doxa
public function wsRead(connection :: int) returns Message | nothing | zerr.ZigError {
    const status is wsNext(connection) as int else {
        return status
    }
    if status == 1 then return wsMessage(connection)
    return nothing
}
```

</details>

### `wsWrite`

```doxa
public function wsWrite(connection :: int, op :: WsOp, data :: string) returns nothing | zerr.ZigError
```

Send one frame with an explicit opcode.

<details>
<summary>Source</summary>

```doxa
public function wsWrite(connection :: int, op :: WsOp, data :: string) returns nothing | zerr.ZigError {
    return wsSend(connection, op, data)
}
```

</details>

### `wsText`

```doxa
public function wsText(connection :: int, data :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function wsText(connection :: int, data :: string) returns nothing | zerr.ZigError {
    return wsSend(connection, WsOp.Text, data)
}
```

</details>

### `wsBinary`

```doxa
public function wsBinary(connection :: int, data :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function wsBinary(connection :: int, data :: string) returns nothing | zerr.ZigError {
    return wsSend(connection, WsOp.Binary, data)
}
```

</details>

### `wsPing`

```doxa
public function wsPing(connection :: int, data :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function wsPing(connection :: int, data :: string) returns nothing | zerr.ZigError {
    return wsSend(connection, WsOp.Ping, data)
}
```

</details>

### `wsPong`

```doxa
public function wsPong(connection :: int, data :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function wsPong(connection :: int, data :: string) returns nothing | zerr.ZigError {
    return wsSend(connection, WsOp.Pong, data)
}
```

</details>

### `wsClose`

```doxa
public function wsClose(connection :: int) returns nothing | zerr.ZigError
```

Send a Close frame (empty payload) and drop the connection.

<details>
<summary>Source</summary>

```doxa
public function wsClose(connection :: int) returns nothing | zerr.ZigError {
    wsSend(connection, WsOp.Close, "")
    return close(connection)
}
```

</details>

## `std.host`

### `os`

```doxa
public function os() returns string
```

<details>
<summary>Source</summary>

```doxa
public function os() returns string {
    return Host.osName();
}
```

</details>

### `arch`

```doxa
public function arch() returns string
```

<details>
<summary>Source</summary>

```doxa
public function arch() returns string {
    return Host.archName();
}
```

</details>

### `abi`

```doxa
public function abi() returns string
```

<details>
<summary>Source</summary>

```doxa
public function abi() returns string {
    return Host.abiName();
}
```

</details>

### `cpu`

```doxa
public function cpu() returns string | zerr.ZigError
```

The model of the CPU running the program (e.g. `"znver4"`), detected when
called rather than fixed when compiled; the `ZigError` Zig's target
detection reports when the host cannot be probed.

<details>
<summary>Source</summary>

```doxa
public function cpu() returns string | zerr.ZigError {
    return Host.cpuName()
}
```

</details>

### `isWindows`

```doxa
public function isWindows() returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function isWindows() returns tetra {
    return Host.isWindows();
}
```

</details>

### `isLinux`

```doxa
public function isLinux() returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function isLinux() returns tetra {
    return Host.isLinux();
}
```

</details>

### `isMac`

```doxa
public function isMac() returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function isMac() returns tetra {
    return Host.isMac();
}
```

</details>

### `pathFor`

```doxa
public function pathFor(platform :: string, parts :: string[]) returns string
```

Joins `parts` with the separator for `platform` (one of the names returned by
`os()`, e.g. `"windows"`, `"linux"`, `"macos"`). Empty segments are skipped,
an absolute segment resets the result, and a separator already at the join
point is not doubled.

<details>
<summary>Source</summary>

```doxa
public function pathFor(platform :: string, parts :: string[]) returns string {
    const sep is sepFor(platform)
    var result is ""
    var i is 0
    while i < @length(parts) {
        const part is parts[i]
        if @length(part) == 0 then {
            i is i + 1
            continue
        }

        if isAbsolute(part, platform) then {
            result is part
        } else if @length(result) == 0 then {
            result is part
        } else if isSep(result[@length(result) - 1], platform) then {
            result is result + part
        } else {
            result is result + sep + part
        }
        i is i + 1
    }
    return result
}
```

</details>

### `path`

```doxa
public function path(parts :: string[]) returns string
```

Joins `parts` with the host OS's separator. See `pathFor`.

<details>
<summary>Source</summary>

```doxa
public function path(parts :: string[]) returns string {
    return pathFor(os(), parts)
}
```

</details>

## `std.time`

### `unix`

```doxa
public function unix() returns int
```

<details>
<summary>Source</summary>

```doxa
public function unix() returns int {
    return Time.unixSeconds();
}
```

</details>

### `tick`

```doxa
public function tick() returns int
```

<details>
<summary>Source</summary>

```doxa
public function tick() returns int {
    return Time.tickNs();
}
```

</details>

### `monotonic`

```doxa
public function monotonic() returns int
```

<details>
<summary>Source</summary>

```doxa
public function monotonic() returns int {
    return Time.monotonicNs();
}
```

</details>

### `unixMs`

```doxa
public function unixMs() returns int
```

<details>
<summary>Source</summary>

```doxa
public function unixMs() returns int {
    return Time.unixMillis();
}
```

</details>

### `sleep`

```doxa
public function sleep(ms :: int)
```

<details>
<summary>Source</summary>

```doxa
public function sleep(ms :: int) {
    Time.sleepMs(@string(ms));
}
```

</details>

### `sleepSeconds`

```doxa
public function sleepSeconds(s :: int)
```

<details>
<summary>Source</summary>

```doxa
public function sleepSeconds(s :: int) {
    Time.sleepSeconds(@string(s));
}
```

</details>

## `std.random`

### `random`

```doxa
public function random() returns float
```

<details>
<summary>Source</summary>

```doxa
public function random() returns float {
    return Random.floatUnit();
}
```

</details>

## `std.build`

### `Optimization`

```doxa
public enum Optimization
```

<details>
<summary>Source</summary>

```doxa
public enum Optimization {
    None,
    Some,
    Speed,
    Size,
}
```

</details>

### `optModeName`

```doxa
public function optModeName(o :: Optimization) returns string
```

Maps the four-cornered Optimization to the backend's release modes, mirrored by
the compiler's `--opt-mode` flag (debug | safe | fast | small). Written with
enum equality rather than `match`: a `match` expression on an enum currently
does not produce its arm value (a compiler bug; `==` is correct).

<details>
<summary>Source</summary>

```doxa
public function optModeName(o :: Optimization) returns string {
    if o == Optimization.None then return "debug"
    if o == Optimization.Some then return "safe"
    if o == Optimization.Speed then return "fast"
    return "small"
}
```

</details>

### `Target`

```doxa
public struct Target
```

<details>
<summary>Source</summary>

```doxa
public struct Target {
    public arch :: string,
    public os :: string,
    public abi :: string,
    # The machine running the build, compiled for its own CPU. A cross target
    # naming the host's triple is still a cross target: it compiles for the
    # triple's baseline CPU, so its binary runs on any machine of that triple.
    public native :: tetra,

    public function host() returns Target {
        return $Target {
            arch is Build.hostArch(),
            os is Build.hostOs(),
            abi is Build.hostAbi(),
            native is true,
        }
    }

    # Cross-compilation target. `os` is the raw OS name passed to `--os=`; an
    # empty OS is rejected at build time (`run`), never silently collapsed to the
    # host.
    public function cross(arch :: string, os :: string, abi :: string) returns Target {
        return $Target {
            arch is arch,
            os is os,
            abi is abi,
            native is false,
        }
    }

    public method isWindows() returns tetra {
        return this.os == "windows"
    }

    public method isLinux() returns tetra {
        return this.os == "linux"
    }

    public method isMac() returns tetra {
        return this.os == "macos"
    }
}
```

</details>

#### `Target.host`

```doxa
public function host() returns Target
```

<details>
<summary>Source</summary>

```doxa
public function host() returns Target {
        return $Target {
            arch is Build.hostArch(),
            os is Build.hostOs(),
            abi is Build.hostAbi(),
            native is true,
        }
    }
```

</details>

#### `Target.cross`

```doxa
public function cross(arch :: string, os :: string, abi :: string) returns Target
```

Cross-compilation target. `os` is the raw OS name passed to `--os=`; an
empty OS is rejected at build time (`run`), never silently collapsed to the
host.

<details>
<summary>Source</summary>

```doxa
public function cross(arch :: string, os :: string, abi :: string) returns Target {
        return $Target {
            arch is arch,
            os is os,
            abi is abi,
            native is false,
        }
    }
```

</details>

#### `Target.isWindows`

```doxa
public method isWindows() returns tetra
```

<details>
<summary>Source</summary>

```doxa
public method isWindows() returns tetra {
        return this.os == "windows"
    }
```

</details>

#### `Target.isLinux`

```doxa
public method isLinux() returns tetra
```

<details>
<summary>Source</summary>

```doxa
public method isLinux() returns tetra {
        return this.os == "linux"
    }
```

</details>

#### `Target.isMac`

```doxa
public method isMac() returns tetra
```

<details>
<summary>Source</summary>

```doxa
public method isMac() returns tetra {
        return this.os == "macos"
    }
```

</details>

### `Executable`

```doxa
public struct Executable
```

<details>
<summary>Source</summary>

```doxa
public struct Executable {
    public name :: string,
    public entry_point :: string,
    public output :: string,
    public includes :: string[],
    public libdirs :: string[],
    public links :: string[],
    public frameworks :: string[],

    public function new(name :: string, entry_point :: string, output :: string) returns Executable {
        return $Executable {
            name is name,
            entry_point is entry_point,
            output is output,
            includes is [],
            libdirs is [],
            links is [],
            frameworks is [],
        }
    }

    public method include(x :: string | string[]) {
        x as string then @push(this.includes, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.includes, x[i])
                i is i + 1
            }
        }
    }

    public method libdir(x :: string | string[]) {
        x as string then @push(this.libdirs, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.libdirs, x[i])
                i is i + 1
            }
        }
    }

    public method link(x :: string | string[]) {
        x as string then @push(this.links, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.links, x[i])
                i is i + 1
            }
        }
    }

    public method framework(x :: string | string[]) {
        x as string then @push(this.frameworks, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.frameworks, x[i])
                i is i + 1
            }
        }
    }
}
```

</details>

#### `Executable.new`

```doxa
public function new(name :: string, entry_point :: string, output :: string) returns Executable
```

<details>
<summary>Source</summary>

```doxa
public function new(name :: string, entry_point :: string, output :: string) returns Executable {
        return $Executable {
            name is name,
            entry_point is entry_point,
            output is output,
            includes is [],
            libdirs is [],
            links is [],
            frameworks is [],
        }
    }
```

</details>

#### `Executable.include`

```doxa
public method include(x :: string | string[])
```

<details>
<summary>Source</summary>

```doxa
public method include(x :: string | string[]) {
        x as string then @push(this.includes, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.includes, x[i])
                i is i + 1
            }
        }
    }
```

</details>

#### `Executable.libdir`

```doxa
public method libdir(x :: string | string[])
```

<details>
<summary>Source</summary>

```doxa
public method libdir(x :: string | string[]) {
        x as string then @push(this.libdirs, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.libdirs, x[i])
                i is i + 1
            }
        }
    }
```

</details>

#### `Executable.link`

```doxa
public method link(x :: string | string[])
```

<details>
<summary>Source</summary>

```doxa
public method link(x :: string | string[]) {
        x as string then @push(this.links, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.links, x[i])
                i is i + 1
            }
        }
    }
```

</details>

#### `Executable.framework`

```doxa
public method framework(x :: string | string[])
```

<details>
<summary>Source</summary>

```doxa
public method framework(x :: string | string[]) {
        x as string then @push(this.frameworks, x) else {
            var i is 0
            while i < @length(x) {
                @push(this.frameworks, x[i])
                i is i + 1
            }
        }
    }
```

</details>

### `Context`

```doxa
public struct Context
```

<details>
<summary>Source</summary>

```doxa
public struct Context {
    public target :: Target,
    public debug :: tetra,
    public optimization :: Optimization,
    public artifacts :: Executable[],

    public function new(target :: Target) returns Context {
        return $Context {
            target is target,
            debug is false,
            optimization is Optimization.None,
            artifacts is [],
        }
    }

    public function host() returns Context {
        return Context.new(Target.host())
    }

    public method addArtifact(b :: Executable) {
        @push(this.artifacts, b)
    }
}
```

</details>

#### `Context.new`

```doxa
public function new(target :: Target) returns Context
```

<details>
<summary>Source</summary>

```doxa
public function new(target :: Target) returns Context {
        return $Context {
            target is target,
            debug is false,
            optimization is Optimization.None,
            artifacts is [],
        }
    }
```

</details>

#### `Context.host`

```doxa
public function host() returns Context
```

<details>
<summary>Source</summary>

```doxa
public function host() returns Context {
        return Context.new(Target.host())
    }
```

</details>

#### `Context.addArtifact`

```doxa
public method addArtifact(b :: Executable)
```

<details>
<summary>Source</summary>

```doxa
public method addArtifact(b :: Executable) {
        @push(this.artifacts, b)
    }
```

</details>

### `run`

```doxa
public function run(c :: Context) returns process.Term | zerr.ZigError
```

Drive the compiler over every artifact in the context, stopping at the first
compile that does not succeed. Returns how that compile ended, or how the
last one did; with no artifacts, an exit of 0.

There is deliberately no up-to-date skip here. An earlier version compared the
output's mtime against the entry source alone and skipped the compile when the
output looked newer, which silently ignored every change in an imported module
and reported success while leaving a stale binary in place — including when the
program no longer compiled at all. Re-deriving the module closure in this layer
would mean duplicating the compiler's import resolution, so the check is the
driver's alone; `plan/incremental-builds.md` tracks the whole-unit manifest
that will make the driver able to answer this content-accurately.

<details>
<summary>Source</summary>

```doxa
public function run(c :: Context) returns process.Term | zerr.ZigError {
    # A cross target must name its OS; an empty OS is never silently resolved to
    # the host.
    if not c.target.native and @length(c.target.os) == 0 then return zerr.ZigError.MissingOperatingSystem
    const compiler is Build.compilerPath() as string else {
        return compiler
    }
    var last is $process.Term { kind is process.TermKind.Exited, code is 0 }
    each art in c.artifacts {
        const term is compileArtifact(compiler, c, art) as process.Term else {
            return term
        }
        if not term.succeeded() then return term
        last is term
    }
    return last
}
```

</details>

### `execute`

```doxa
public function execute(c :: Context)
```

Drive the compiler over every artifact and propagate the outcome through the
process exit code: a successful build exits 0, a failed compile exits with
the compiler's status, and a compiler that could not run, or ended by a
signal, prints why to stderr and exits 1. `execute` is the terminal statement
of a build script; it never returns.

<details>
<summary>Source</summary>

```doxa
public function execute(c :: Context) {
    const outcome is run(c) as process.Term else {
        io.eprintln("Build failed: {outcome}")
        @exit(1)
    }
    if outcome.kind != process.TermKind.Exited then {
        io.eprintln("Build failed: the compiler ended by {outcome.kind} {outcome.code}")
        @exit(1)
    }
    # The compiler's own diagnostics already describe a failing artifact.
    @print("build exit code: {outcome.code}\n")
    if outcome.code != 0 then @exit(outcome.code)
}
```

</details>

### `compile`

```doxa
public function compile(src :: string, out :: string, arch :: string, os :: string, abi :: string, debug :: tetra) returns process.Term | zerr.ZigError
```

Low-level single-file compile, retained for direct use and the build test
harness. `debug` forces the debug optimization mode.

<details>
<summary>Source</summary>

```doxa
public function compile(src :: string, out :: string, arch :: string, os :: string, abi :: string, debug :: tetra) returns process.Term | zerr.ZigError {
    var c is Context.new(Target.cross(arch, os, abi))
    c.debug is debug
    c.optimization is Optimization.Speed
    c.addArtifact(Executable.new(src, src, out))
    return run(c)
}
```

</details>

### `compileHost`

```doxa
public function compileHost(src :: string, out :: string, debug :: tetra) returns process.Term | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function compileHost(src :: string, out :: string, debug :: tetra) returns process.Term | zerr.ZigError {
    return compile(src, out, Build.hostArch(), Build.hostOs(), Build.hostAbi(), debug)
}
```

</details>

## `std.error`

### `Method`

```doxa
public enum Method
```

Doxa's own runtime errors, raised by the intrinsics.

<details>
<summary>Source</summary>

```doxa
public enum Method {
    OutOfBounds,
    EmptyCollection,
    InvalidNumber,
    Overflow,
    Unexpected,
}
```

</details>

## `std.methods`

### `push`

```doxa
public function push(^collection :: string | int[] | float[] | byte[] | tetra[] | string[], value :: string | int | float | byte | tetra)
```

The collection families that do not return the collection's own type collapse
to one union-typed entry point. The element type is checked at run time: a
value that does not match the collection's element type panics rather than
silently doing nothing.

<details>
<summary>Source</summary>

```doxa
public function push(^collection :: string | int[] | float[] | byte[] | tetra[] | string[], value :: string | int | float | byte | tetra) {
    match collection {
        string then {
            const element is value as string else { @panic("methods.push: value type does not match the collection element type") }
            @push(collection, element)
        }
        int[] then {
            const element is value as int else { @panic("methods.push: value type does not match the collection element type") }
            @push(collection, element)
        }
        float[] then {
            const element is value as float else { @panic("methods.push: value type does not match the collection element type") }
            @push(collection, element)
        }
        byte[] then {
            const element is value as byte else { @panic("methods.push: value type does not match the collection element type") }
            @push(collection, element)
        }
        tetra[] then {
            const element is value as tetra else { @panic("methods.push: value type does not match the collection element type") }
            @push(collection, element)
        }
        string[] then {
            const element is value as string else { @panic("methods.push: value type does not match the collection element type") }
            @push(collection, element)
        }
    }
}
```

</details>

### `popString`

```doxa
public function popString(^collection :: string) returns string | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function popString(^collection :: string) returns string | error.Method {
    if @length(collection) == 0 then return error.Method.EmptyCollection
    return @pop(collection)
}
```

</details>

### `popInt`

```doxa
public function popInt(^collection :: int[]) returns int | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function popInt(^collection :: int[]) returns int | error.Method {
    if @length(collection) == 0 then return error.Method.EmptyCollection
    return @pop(collection)
}
```

</details>

### `popFloat`

```doxa
public function popFloat(^collection :: float[]) returns float | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function popFloat(^collection :: float[]) returns float | error.Method {
    if @length(collection) == 0 then return error.Method.EmptyCollection
    return @pop(collection)
}
```

</details>

### `popByte`

```doxa
public function popByte(^collection :: byte[]) returns byte | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function popByte(^collection :: byte[]) returns byte | error.Method {
    if @length(collection) == 0 then return error.Method.EmptyCollection
    return @pop(collection)
}
```

</details>

### `popTetra`

```doxa
public function popTetra(^collection :: tetra[]) returns tetra | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function popTetra(^collection :: tetra[]) returns tetra | error.Method {
    if @length(collection) == 0 then return error.Method.EmptyCollection
    return @pop(collection)
}
```

</details>

### `popStringArray`

```doxa
public function popStringArray(^collection :: string[]) returns string | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function popStringArray(^collection :: string[]) returns string | error.Method {
    if @length(collection) == 0 then return error.Method.EmptyCollection
    return @pop(collection)
}
```

</details>

### `insert`

```doxa
public function insert(^collection :: string | int[] | float[] | byte[] | tetra[] | string[], index :: int, value :: string | int | float | byte | tetra) returns nothing | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function insert(^collection :: string | int[] | float[] | byte[] | tetra[] | string[], index :: int, value :: string | int | float | byte | tetra) returns nothing | error.Method {
    match collection {
        string then {
            if index < 0 then return error.Method.OutOfBounds
            if index > @length(collection) then return error.Method.OutOfBounds
            const element is value as string else { return error.Method.Unexpected }
            @insert(collection, index, element)
        }
        int[] then {
            if index < 0 then return error.Method.OutOfBounds
            if index > @length(collection) then return error.Method.OutOfBounds
            const element is value as int else { return error.Method.Unexpected }
            @insert(collection, index, element)
        }
        float[] then {
            if index < 0 then return error.Method.OutOfBounds
            if index > @length(collection) then return error.Method.OutOfBounds
            const element is value as float else { return error.Method.Unexpected }
            @insert(collection, index, element)
        }
        byte[] then {
            if index < 0 then return error.Method.OutOfBounds
            if index > @length(collection) then return error.Method.OutOfBounds
            const element is value as byte else { return error.Method.Unexpected }
            @insert(collection, index, element)
        }
        tetra[] then {
            if index < 0 then return error.Method.OutOfBounds
            if index > @length(collection) then return error.Method.OutOfBounds
            const element is value as tetra else { return error.Method.Unexpected }
            @insert(collection, index, element)
        }
        string[] then {
            if index < 0 then return error.Method.OutOfBounds
            if index > @length(collection) then return error.Method.OutOfBounds
            const element is value as string else { return error.Method.Unexpected }
            @insert(collection, index, element)
        }
    }
}
```

</details>

### `removeString`

```doxa
public function removeString(^collection :: string, index :: int) returns string | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function removeString(^collection :: string, index :: int) returns string | error.Method {
    const len is @length(collection)
    if outOfBounds(index, len) then return error.Method.OutOfBounds
    return @remove(collection, index)
}
```

</details>

### `removeInt`

```doxa
public function removeInt(^collection :: int[], index :: int) returns int | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function removeInt(^collection :: int[], index :: int) returns int | error.Method {
    const len is @length(collection)
    if outOfBounds(index, len) then return error.Method.OutOfBounds
    return @remove(collection, index)
}
```

</details>

### `removeFloat`

```doxa
public function removeFloat(^collection :: float[], index :: int) returns float | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function removeFloat(^collection :: float[], index :: int) returns float | error.Method {
    const len is @length(collection)
    if outOfBounds(index, len) then return error.Method.OutOfBounds
    return @remove(collection, index)
}
```

</details>

### `removeByte`

```doxa
public function removeByte(^collection :: byte[], index :: int) returns byte | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function removeByte(^collection :: byte[], index :: int) returns byte | error.Method {
    const len is @length(collection)
    if outOfBounds(index, len) then return error.Method.OutOfBounds
    return @remove(collection, index)
}
```

</details>

### `removeTetra`

```doxa
public function removeTetra(^collection :: tetra[], index :: int) returns tetra | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function removeTetra(^collection :: tetra[], index :: int) returns tetra | error.Method {
    const len is @length(collection)
    if outOfBounds(index, len) then return error.Method.OutOfBounds
    return @remove(collection, index)
}
```

</details>

### `removeStringArray`

```doxa
public function removeStringArray(^collection :: string[], index :: int) returns string | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function removeStringArray(^collection :: string[], index :: int) returns string | error.Method {
    const len is @length(collection)
    if outOfBounds(index, len) then return error.Method.OutOfBounds
    return @remove(collection, index)
}
```

</details>

### `clear`

```doxa
public function clear(^collection :: string | int[] | float[] | byte[] | tetra[] | string[])
```

<details>
<summary>Source</summary>

```doxa
public function clear(^collection :: string | int[] | float[] | byte[] | tetra[] | string[]) {
    match collection {
        string then { @clear(collection) }
        int[] then { @clear(collection) }
        float[] then { @clear(collection) }
        byte[] then { @clear(collection) }
        tetra[] then { @clear(collection) }
        string[] then { @clear(collection) }
    }
}
```

</details>

### `find`

```doxa
public function find(collection :: string | int[] | float[] | byte[] | tetra[] | string[], value :: string | int | float | byte | tetra) returns int
```

<details>
<summary>Source</summary>

```doxa
public function find(collection :: string | int[] | float[] | byte[] | tetra[] | string[], value :: string | int | float | byte | tetra) returns int {
    match collection {
        string then {
            const needle is value as string else { return -1 }
            return @find(collection, needle)
        }
        int[] then {
            const needle is value as int else { return -1 }
            return @find(collection, needle)
        }
        float[] then {
            const needle is value as float else { return -1 }
            return @find(collection, needle)
        }
        byte[] then {
            const needle is value as byte else { return -1 }
            return @find(collection, needle)
        }
        tetra[] then {
            const needle is value as tetra else { return -1 }
            return @find(collection, needle)
        }
        string[] then {
            const needle is value as string else { return -1 }
            return @find(collection, needle)
        }
    }
    return -1
}
```

</details>

### `sliceString`

```doxa
public function sliceString(collection :: string, start :: int, length :: int) returns string | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function sliceString(collection :: string, start :: int, length :: int) returns string | error.Method {
    if sliceOutOfBounds(start, length, @length(collection)) then return error.Method.OutOfBounds
    return @slice(collection, start, length)
}
```

</details>

### `sliceInt`

```doxa
public function sliceInt(collection :: int[], start :: int, length :: int) returns int[] | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function sliceInt(collection :: int[], start :: int, length :: int) returns int[] | error.Method {
    if sliceOutOfBounds(start, length, @length(collection)) then return error.Method.OutOfBounds
    return @slice(collection, start, length)
}
```

</details>

### `sliceFloat`

```doxa
public function sliceFloat(collection :: float[], start :: int, length :: int) returns float[] | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function sliceFloat(collection :: float[], start :: int, length :: int) returns float[] | error.Method {
    if sliceOutOfBounds(start, length, @length(collection)) then return error.Method.OutOfBounds
    return @slice(collection, start, length)
}
```

</details>

### `sliceByte`

```doxa
public function sliceByte(collection :: byte[], start :: int, length :: int) returns byte[] | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function sliceByte(collection :: byte[], start :: int, length :: int) returns byte[] | error.Method {
    if sliceOutOfBounds(start, length, @length(collection)) then return error.Method.OutOfBounds
    return @slice(collection, start, length)
}
```

</details>

### `sliceTetra`

```doxa
public function sliceTetra(collection :: tetra[], start :: int, length :: int) returns tetra[] | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function sliceTetra(collection :: tetra[], start :: int, length :: int) returns tetra[] | error.Method {
    if sliceOutOfBounds(start, length, @length(collection)) then return error.Method.OutOfBounds
    return @slice(collection, start, length)
}
```

</details>

### `sliceStringArray`

```doxa
public function sliceStringArray(collection :: string[], start :: int, length :: int) returns string[] | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function sliceStringArray(collection :: string[], start :: int, length :: int) returns string[] | error.Method {
    if sliceOutOfBounds(start, length, @length(collection)) then return error.Method.OutOfBounds
    return @slice(collection, start, length)
}
```

</details>

### `toInt`

```doxa
public function toInt(value :: int | float | byte | string) returns int | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function toInt(value :: int | float | byte | string) returns int | error.Method {
    match value {
        int then {
            return value
        } 
        float then {
        if value != value then return error.Method.InvalidNumber
        if value > 9223372036854775807.0 then return error.Method.Overflow
        if value < -9223372036854775808.0 then return error.Method.Overflow
        return @int(value)
        }
        byte then {
            if value < 0 or value > 255 then return error.Method.Overflow
            return @int(value)
        }
        string then {
            return Parse.parseIntSafe(value)
        }
    }
}
```

</details>

### `toFloat`

```doxa
public function toFloat(value :: int | float | byte | string) returns float | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function toFloat(value :: int | float | byte | string) returns float | error.Method {
    match value {
        float then {
            if value != value then return error.Method.InvalidNumber
            return value
        }
        int then {
            return @float(value)
        }
        byte then {
            return @float(value)
        }
        string then {
            if Parse.isFloat(value) then return Parse.parseFloatSafe(value)
            return error.Method.InvalidNumber
        }
    }
}
```

</details>

### `toByte`

```doxa
public function toByte(value :: int | float | byte | string) returns byte | error.Method
```

<details>
<summary>Source</summary>

```doxa
public function toByte(value :: int | float | byte | string) returns byte | error.Method {
    match value {
        byte then {
            return value
        }
        int then {
            if value < 0 or value > 255 then return error.Method.Overflow
            return @byte(value)
        }
        float then {
            if value != value then return error.Method.InvalidNumber
            if value < 0.0 or value > 255.0 then return error.Method.Overflow
            return @byte(value)
        }
        string then {
            if Parse.isByte(value) then return Parse.parseByteSafe(value)
            return error.Method.InvalidNumber
        }
    }
}
```

</details>

## `std.json`

### `Kind`

```doxa
public enum Kind
```

<details>
<summary>Source</summary>

```doxa
public enum Kind {
    Invalid,
    Null,
    Boolean,
    Integer,
    Number,
    Text,
    Array,
    Object,
}
```

</details>

### `Node`

```doxa
public struct Node
```

<details>
<summary>Source</summary>

```doxa
public struct Node {
    handle :: int,

    function new(h :: int) returns Node {
        return $Node {
            handle is h
        }
    }

    # Parse `text` as one JSON document. A node of an earlier document reads
    # as `Kind.Invalid` once another is parsed.
    public function parse(text :: string) returns Node | zerr.ZigError {
        const root is JSON.parseDocument(text) as int else {
            return root
        }
        return Node.new(root)
    }

    public method kind() returns Kind {
        const code is JSON.nodeKind(this.handle)
        return match code {
            1 then Kind.Null,
            2 then Kind.Boolean,
            3 then Kind.Integer,
            4 then Kind.Number,
            5 then Kind.Text,
            6 then Kind.Array,
            7 then Kind.Object,
            else Kind.Invalid,
        }
    }

    method holds(kind :: Kind) returns tetra {
        return this.kind() == kind
    }

    # How many elements or fields, or `nothing` for a node that is neither an
    # array nor an object.
    public method count() returns int | nothing {
        if not this.holds(Kind.Array) and not this.holds(Kind.Object) then return nothing
        return JSON.nodeCount(this.handle)
    }

    public method element(index :: int) returns Node | nothing {
        const child is JSON.nodeChild(this.handle, index)
        if child == 0 then return nothing
        return Node.new(child)
    }

    public method field(name :: string) returns Node | nothing {
        const child is JSON.nodeField(this.handle, name)
        if child == 0 then return nothing
        return Node.new(child)
    }

    # The field name this node is stored under, or `nothing` for a node that
    # is not an object's field.
    public method key() returns string | nothing {
        if not JSON.nodeHasKey(this.handle) then return nothing
        return JSON.nodeKey(this.handle)
    }

    public method text() returns string | nothing {
        if not this.holds(Kind.Text) then return nothing
        return JSON.nodeText(this.handle)
    }

    public method intValue() returns int | nothing {
        if not this.holds(Kind.Integer) then return nothing
        return JSON.nodeIntValue(this.handle)
    }

    public method floatValue() returns float | nothing {
        if not this.holds(Kind.Number) then return nothing
        return JSON.nodeFloatValue(this.handle)
    }

    public method booleanValue() returns tetra | nothing {
        if not this.holds(Kind.Boolean) then return nothing
        return JSON.nodeBooleanValue(this.handle)
    }
}
```

</details>

#### `Node.parse`

```doxa
public function parse(text :: string) returns Node | zerr.ZigError
```

Parse `text` as one JSON document. A node of an earlier document reads
as `Kind.Invalid` once another is parsed.

<details>
<summary>Source</summary>

```doxa
public function parse(text :: string) returns Node | zerr.ZigError {
        const root is JSON.parseDocument(text) as int else {
            return root
        }
        return Node.new(root)
    }
```

</details>

#### `Node.kind`

```doxa
public method kind() returns Kind
```

<details>
<summary>Source</summary>

```doxa
public method kind() returns Kind {
        const code is JSON.nodeKind(this.handle)
        return match code {
            1 then Kind.Null,
            2 then Kind.Boolean,
            3 then Kind.Integer,
            4 then Kind.Number,
            5 then Kind.Text,
            6 then Kind.Array,
            7 then Kind.Object,
            else Kind.Invalid,
        }
    }
```

</details>

#### `Node.count`

```doxa
public method count() returns int | nothing
```

How many elements or fields, or `nothing` for a node that is neither an
array nor an object.

<details>
<summary>Source</summary>

```doxa
public method count() returns int | nothing {
        if not this.holds(Kind.Array) and not this.holds(Kind.Object) then return nothing
        return JSON.nodeCount(this.handle)
    }
```

</details>

#### `Node.element`

```doxa
public method element(index :: int) returns Node | nothing
```

<details>
<summary>Source</summary>

```doxa
public method element(index :: int) returns Node | nothing {
        const child is JSON.nodeChild(this.handle, index)
        if child == 0 then return nothing
        return Node.new(child)
    }
```

</details>

#### `Node.field`

```doxa
public method field(name :: string) returns Node | nothing
```

<details>
<summary>Source</summary>

```doxa
public method field(name :: string) returns Node | nothing {
        const child is JSON.nodeField(this.handle, name)
        if child == 0 then return nothing
        return Node.new(child)
    }
```

</details>

#### `Node.key`

```doxa
public method key() returns string | nothing
```

The field name this node is stored under, or `nothing` for a node that
is not an object's field.

<details>
<summary>Source</summary>

```doxa
public method key() returns string | nothing {
        if not JSON.nodeHasKey(this.handle) then return nothing
        return JSON.nodeKey(this.handle)
    }
```

</details>

#### `Node.text`

```doxa
public method text() returns string | nothing
```

<details>
<summary>Source</summary>

```doxa
public method text() returns string | nothing {
        if not this.holds(Kind.Text) then return nothing
        return JSON.nodeText(this.handle)
    }
```

</details>

#### `Node.intValue`

```doxa
public method intValue() returns int | nothing
```

<details>
<summary>Source</summary>

```doxa
public method intValue() returns int | nothing {
        if not this.holds(Kind.Integer) then return nothing
        return JSON.nodeIntValue(this.handle)
    }
```

</details>

#### `Node.floatValue`

```doxa
public method floatValue() returns float | nothing
```

<details>
<summary>Source</summary>

```doxa
public method floatValue() returns float | nothing {
        if not this.holds(Kind.Number) then return nothing
        return JSON.nodeFloatValue(this.handle)
    }
```

</details>

#### `Node.booleanValue`

```doxa
public method booleanValue() returns tetra | nothing
```

<details>
<summary>Source</summary>

```doxa
public method booleanValue() returns tetra | nothing {
        if not this.holds(Kind.Boolean) then return nothing
        return JSON.nodeBooleanValue(this.handle)
    }
```

</details>

### `beginObject`

```doxa
public function beginObject() returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function beginObject() returns nothing | zerr.ZigError {
    return JSON.writerBeginObject()
}
```

</details>

### `beginArray`

```doxa
public function beginArray() returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function beginArray() returns nothing | zerr.ZigError {
    return JSON.writerBeginArray()
}
```

</details>

### `end`

```doxa
public function end() returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function end() returns nothing | zerr.ZigError {
    return JSON.writerEnd()
}
```

</details>

### `writeKey`

```doxa
public function writeKey(name :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function writeKey(name :: string) returns nothing | zerr.ZigError {
    return JSON.writerKey(name)
}
```

</details>

### `writeText`

```doxa
public function writeText(text :: string) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function writeText(text :: string) returns nothing | zerr.ZigError {
    return JSON.writerText(text)
}
```

</details>

### `writeInt`

```doxa
public function writeInt(value :: int) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function writeInt(value :: int) returns nothing | zerr.ZigError {
    return JSON.writerInt(value)
}
```

</details>

### `writeFloat`

```doxa
public function writeFloat(value :: float) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function writeFloat(value :: float) returns nothing | zerr.ZigError {
    return JSON.writerFloat(value)
}
```

</details>

### `writeBoolean`

```doxa
public function writeBoolean(value :: tetra) returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function writeBoolean(value :: tetra) returns nothing | zerr.ZigError {
    if value == both or value == neither then return JSON.writerRefuse()
    return JSON.writerBoolean(value == true)
}
```

</details>

### `writeNull`

```doxa
public function writeNull() returns nothing | zerr.ZigError
```

<details>
<summary>Source</summary>

```doxa
public function writeNull() returns nothing | zerr.ZigError {
    return JSON.writerNull()
}
```

</details>

### `finish`

```doxa
public function finish() returns string | zerr.ZigError
```

End the document: its text, or the first refusal it met. The next write
starts a new document either way.

<details>
<summary>Source</summary>

```doxa
public function finish() returns string | zerr.ZigError {
    return JSON.writerFinish()
}
```

</details>

## `std.math`

### `maxInt`

```doxa
public function maxInt() returns int
```

The largest `int`, 9223372036854775807.

<details>
<summary>Source</summary>

```doxa
public function maxInt() returns int {
    return 9223372036854775807
}
```

</details>

### `minInt`

```doxa
public function minInt() returns int
```

The smallest `int`, -9223372036854775808.

<details>
<summary>Source</summary>

```doxa
public function minInt() returns int {
    return -9223372036854775807 - 1
}
```

</details>

### `maxFloat`

```doxa
public function maxFloat() returns float
```

The largest finite `float`, about 1.7976931348623157e308.

<details>
<summary>Source</summary>

```doxa
public function maxFloat() returns float {
    return 1.7976931348623157e308
}
```

</details>

### `minFloat`

```doxa
public function minFloat() returns float
```

The smallest finite `float`: the negation of `maxFloat()`.

<details>
<summary>Source</summary>

```doxa
public function minFloat() returns float {
    return -1.7976931348623157e308
}
```

</details>

### `minNormalFloat`

```doxa
public function minNormalFloat() returns float
```

The smallest positive normal `float`, about 2.2250738585072014e-308.

<details>
<summary>Source</summary>

```doxa
public function minNormalFloat() returns float {
    return 2.2250738585072014e-308
}
```

</details>

### `maxByte`

```doxa
public function maxByte() returns byte
```

The largest `byte`, 0xFF.

<details>
<summary>Source</summary>

```doxa
public function maxByte() returns byte {
    return 0xFF
}
```

</details>

### `minByte`

```doxa
public function minByte() returns byte
```

The smallest `byte`, 0x00.

<details>
<summary>Source</summary>

```doxa
public function minByte() returns byte {
    return 0x00
}
```

</details>

## `std.zerr`

### `StdError`

```doxa
public group StdError
```

Every failure a std function reports: an error Zig raised, named as Zig
names it, or one of Doxa's own runtime errors.

<details>
<summary>Source</summary>

```doxa
public group StdError {
    ZigError,
    error.Method
}
```

</details>

### `ZigError`

```doxa
public enum ZigError
```

Every error name the bundled Zig standard library defines.

<details>
<summary>Source</summary>

```doxa
public enum ZigError {
    Aborted,
    AccessDenied,
    AddressFamilyUnsupported,
    AddressInUse,
    AddressUnavailable,
    AllocatorRequired,
    AlreadyConnected,
    AlreadyReported,
    AlreadyStarted,
    AlreadyTerminated,
    AnalysisFail,
    AntivirusInterference,
    ApiLevelQueryFailed,
    ArgumentsInvalid,
    AuthenticationFailed,
    BadBufferSize,
    BadFd,
    BadGzipHeader,
    BadHeader,
    BadMagic,
    BadPathName,
    BadString,
    BadSubsectionSize,
    BadSymtab,
    BadToken,
    BadVersion,
    BadZlibHeader,
    BlockOversize,
    BlockedByFirewall,
    BrokenPipe,
    BufferInvalid,
    BufferTooSmall,
    BufferUnderrun,
    BuffersNotRegistered,
    CCompilerCannotFindHeaders,
    CCompilerCrashed,
    CCompilerExitCode,
    CacheCheckFailed,
    CancelRequested,
    Canceled,
    CertificateAuthorityBundleTooBig,
    CertificateBundleLoadFailure,
    CertificateExpired,
    CertificateFieldHasInvalidLength,
    CertificateFieldHasWrongDataType,
    CertificateHasInvalidBitString,
    CertificateHasUnrecognizedObjectId,
    CertificateHostMismatch,
    CertificateIssuerMismatch,
    CertificateIssuerNotFound,
    CertificateNotYetValid,
    CertificatePublicKeyInvalid,
    CertificateSignatureAlgorithmMismatch,
    CertificateSignatureAlgorithmUnsupported,
    CertificateSignatureInvalid,
    CertificateSignatureInvalidLength,
    CertificateSignatureNamedCurveUnsupported,
    CertificateSignatureUnsupportedBitCount,
    CertificateTimeInvalid,
    ChecksumFailure,
    ClockUnavailable,
    Closed,
    ClosingBraceNotLast,
    CodepointTooLarge,
    CompletionQueueOvercommitted,
    CompressedSizeMismatch,
    CompressionUnsupported,
    CompromisedData,
    ConcurrencyUnavailable,
    ConnectionAborted,
    ConnectionClose,
    ConnectionFin,
    ConnectionPending,
    ConnectionRefused,
    ConnectionReset,
    ConnectionResetByPeer,
    ContentOversize,
    ContextTooLong,
    CorruptInput,
    CorruptPasswordFile,
    CorruptedData,
    CrcError,
    CrcMismatch,
    CrossDevice,
    CurrentDirUnlinked,
    DanglingSurrogateHalf,
    DarwinSdkNotFound,
    DeadLock,
    DecompressedSizeMismatch,
    DetectingNetworkConfigurationFailed,
    DeviceBusy,
    DeviceError,
    DictionaryIdFlagUnsupported,
    DirNotEmpty,
    DiskQuota,
    DivisionByZero,
    DuplicateField,
    DwordNotFound,
    EOF,
    ElementNotFound,
    ElfHashTableNotFound,
    ElfStringSectionNotFound,
    ElfSymSectionNotFound,
    EndOfFile,
    EndOfMedia,
    EndOfStream,
    EntriesNotInRange,
    EntriesNotPowerOfTwo,
    EntriesZero,
    EntropyUnavailable,
    EnvironmentVariableMissing,
    EvenModulus,
    EventNotFound,
    EventNotSupported,
    EventRequiresUnsupportedCpuFeature,
    ExceededMaxDepth,
    ExecNotSupported,
    ExitCodeFailure,
    ExpectedError,
    ExpectedSecondSurrogateHalf,
    FailedTest,
    FastOpenAlreadyInProgress,
    FieldInAttrNeedsZeroing,
    FileBusy,
    FileDescriptorInBadState,
    FileDescriptorInvalid,
    FileDescriptorNotASocket,
    FileLocksUnsupported,
    FileNotFound,
    FileSystem,
    FileTooBig,
    FilesAlreadyRegistered,
    FilesEmpty,
    FilesNotRegistered,
    GLibCNotFound,
    HardwareFailure,
    HeaderConfigFailed,
    HostUnreachable,
    HttpChunkInvalid,
    HttpChunkTruncated,
    HttpConnectionClosing,
    HttpConnectionHeaderUnsupported,
    HttpContentEncodingUnsupported,
    HttpError,
    HttpExpectationFailed,
    HttpHeaderContinuationsUnsupported,
    HttpHeadersInvalid,
    HttpHeadersOversize,
    HttpRedirectLocationInvalid,
    HttpRedirectLocationMissing,
    HttpRedirectLocationOversize,
    HttpRequestTruncated,
    HttpTransferEncodingUnsupported,
    HuffmanTreeIncomplete,
    IcmpError,
    IdentityElement,
    IncompatibleRegisterSize,
    IncompatibleVersion,
    Incomplete,
    IncompleteExpressionContext,
    IncompleteHuffmanTree,
    InnerCarriageReturn,
    InodeMountFail,
    InputBufferUndersize,
    InputOutput,
    InstallationNotFound,
    InsufficientEntropy,
    InsufficientRandomness,
    InsufficientSeed,
    InterfaceNotFound,
    Invalid,
    InvalidAbbrevId,
    InvalidAbiVersion,
    InvalidAddress,
    InvalidArchiveHeaderMagicNumber,
    InvalidArchiveMagicNumber,
    InvalidArchivePadding,
    InvalidArg0,
    InvalidArrayElement,
    InvalidBase,
    InvalidBatchScriptArg,
    InvalidBitStream,
    InvalidBitString,
    InvalidBlockIndex,
    InvalidBlockType,
    InvalidBool,
    InvalidBuildIdStyle,
    InvalidCFAOpcode,
    InvalidCharacter,
    InvalidCode,
    InvalidCodepoint,
    InvalidCompressedSection,
    InvalidContentLength,
    InvalidDebugDirectory,
    InvalidDebugInfo,
    InvalidDepFile,
    InvalidDnsAAAARecord,
    InvalidDnsARecord,
    InvalidDnsCnameRecord,
    InvalidDnsPacket,
    InvalidDwarf,
    InvalidDynamicBlockHeader,
    InvalidElfClass,
    InvalidElfEndian,
    InvalidElfFile,
    InvalidElfMagic,
    InvalidElfVersion,
    InvalidEncoding,
    InvalidEnd,
    InvalidEndBlock,
    InvalidEnumTag,
    InvalidExe,
    InvalidExpression,
    InvalidFileDescriptor,
    InvalidFileName,
    InvalidFormat,
    InvalidFrameBase,
    InvalidGnuLibCVersion,
    InvalidHandle,
    InvalidHostName,
    InvalidInput,
    InvalidIntegralTypeSize,
    InvalidLanguage,
    InvalidLength,
    InvalidLiteral,
    InvalidMachO,
    InvalidMachine,
    InvalidMagic,
    InvalidMagicByte,
    InvalidMagicNumber,
    InvalidMatch,
    InvalidName,
    InvalidNumber,
    InvalidOpcode,
    InvalidOperand,
    InvalidOperatingSystemVersion,
    InvalidOperation,
    InvalidPEHeader,
    InvalidPEMagic,
    InvalidPadding,
    InvalidParameter,
    InvalidPort,
    InvalidProcessGroupId,
    InvalidProgram,
    InvalidProtocolOption,
    InvalidRangeCode,
    InvalidRecordId,
    InvalidRegister,
    InvalidSignature,
    InvalidStreamDirectory,
    InvalidStrtabSize,
    InvalidSubExpression,
    InvalidSyscall,
    InvalidSyscallParameters,
    InvalidThread,
    InvalidToken,
    InvalidTypeLength,
    InvalidUserId,
    InvalidUtf8,
    InvalidVersion,
    InvalidWtf8,
    IpAddressConflict,
    IsDir,
    IterativeHashDidNotMatchDirect,
    IterativeHashWasNotIdempotent,
    KeyMismatch,
    KeyNotFound,
    LargeValue,
    LengthMismatch,
    LibCInstallationMissingCrtDir,
    LibCKernel32LibNotFound,
    LibCRuntimeNotFound,
    LibCStdLibHeaderNotFound,
    LimitTooBig,
    LinkQuotaExceeded,
    LoadError,
    LockViolation,
    LockedMemoryLimitExceeded,
    MadviseUnavailable,
    MakeFailed,
    MakeSkipped,
    Malformed,
    MalformedAccuracyLog,
    MalformedBlock,
    MalformedCompressedBlock,
    MalformedFrame,
    MalformedFseBits,
    MalformedFseTable,
    MalformedHuffmanTree,
    MalformedLiteralsHeader,
    MalformedLiteralsLength,
    MalformedLiteralsSection,
    MalformedSequence,
    MapTypeOrAttrInvalid,
    MappingAlreadyExists,
    MarkAlreadyExists,
    MediaChanged,
    MemoryLeakDetected,
    MemoryMappingNotSupported,
    MessageOversize,
    MessageTooLong,
    MincoreUnavailable,
    MissingBraceValue,
    MissingClosingBrace,
    MissingCoffSection,
    MissingCoreServicesSymbol,
    MissingDebugInfo,
    MissingDynamicLinkingInformation,
    MissingEndCertificateMarker,
    MissingEndOfBlockCode,
    MissingField,
    MissingFinalNewline,
    MissingKey,
    MissingMaskBit,
    MissingOperatingSystem,
    MissingPEHeader,
    MissingPrefix,
    MissingStartBit,
    MissingStringTable,
    MissingValue,
    ModulusTooSmall,
    MsvcLibDirNotFound,
    NameServerFailure,
    NameTooLong,
    NeedCompileErrorCheck,
    NegativeDenominator,
    NegativeIntoUnsigned,
    NetworkDown,
    NetworkNotFound,
    NetworkUnreachable,
    NoAddressReturned,
    NoBufferSelected,
    NoDevice,
    NoMapping,
    NoMedia,
    NoResponse,
    NoSpaceLeft,
    NoStrtab,
    NoSymtab,
    NonCanonical,
    NonResizable,
    NondeterministicMemoryUsage,
    NotADword,
    NotAPgrpMember,
    NotAString,
    NotASubPath,
    NotATerminal,
    NotAssociatedWithFileSystem,
    NotDir,
    NotDynamicLibrary,
    NotElfFile,
    NotFound,
    NotLink,
    NotNtPath,
    NotOpenForReading,
    NotOpenForWriting,
    NotReady,
    NotSquare,
    NotStarted,
    NotTerminalDevice,
    NotXzStream,
    NullExponent,
    OSVersionDetectionFail,
    OctalOverflow,
    OpcodeNotSupported,
    OpenFrameworkFailed,
    OperationUnsupported,
    OptionUnsupported,
    OutOfMemory,
    OutOfResources,
    OutputBufferUndersize,
    OutputTooLong,
    Overflow,
    OverheadTooLarge,
    OverlargeFooter,
    OversubscribedHuffmanTree,
    PackageNotFound,
    ParamsOutsideAccessibleAddressSpace,
    ParseError,
    ParseFailed,
    ParseZon,
    PasswordVerificationFailed,
    PathAlreadyExists,
    PathNotFound,
    PathTooLong,
    PaxInvalidAttributeEnd,
    PaxNullInKeyword,
    PaxNullInValue,
    PaxSizeAttrOverflow,
    PermissionDenied,
    PipeBusy,
    PkgConfigCrashed,
    PkgConfigFailed,
    PkgConfigInvalidOutput,
    PkgConfigNotInstalled,
    PortUnreachable,
    ProcessAlreadyExec,
    ProcessFdQuotaExceeded,
    ProcessNotFound,
    ProcessOrphaned,
    ProcessResources,
    ProcessTerminated,
    ProtocolError,
    ProtocolFailure,
    ProtocolUnreachable,
    ProtocolUnsupportedByAddressFamily,
    ProtocolUnsupportedBySystem,
    ReachedMaxEntries,
    ReadDirectoryChangesUnsupported,
    ReadFailed,
    ReadFailure,
    ReadOnlyFileSystem,
    RedirectRequiresResend,
    RejectionSamplingFailed,
    RelativeShebang,
    RepeatModeFirst,
    ReservedBitSet,
    ReservedBlock,
    ResolvConfParseFailed,
    ResourceLimitReached,
    RingShuttingDown,
    RingShuttingDownOrAlreadyRegisteringFiles,
    SampleMaxStackOverflow,
    SampleStackNotSupported,
    SectionOversize,
    SecurityViolation,
    SequenceBufferUndersize,
    ServerError,
    SignalInterrupt,
    SignatureVerificationFailed,
    SizeMismatch,
    SizeTooSmall,
    SkipZigTest,
    SocketModeUnsupported,
    SocketNotBound,
    SocketNotListening,
    SocketUnconnected,
    SqrtOfNegativeNumber,
    StartFailed,
    StaticElfFile,
    StderrStreamTooLong,
    StdoutStreamTooLong,
    StreamTooLong,
    Streaming,
    StringNotFound,
    SubmissionQueueEntryInvalid,
    SubmissionQueueFull,
    SuccessfulEndOfStream,
    SwallowedOutOfMemoryError,
    SymLinkLoop,
    SyntaxError,
    SystemCannotYield,
    SystemFdQuotaExceeded,
    SystemOutdated,
    SystemResources,
    TagNotFound,
    TarComponentsOutsideStrippedPrefix,
    TarHeader,
    TarHeaderChksum,
    TarHeadersTooBig,
    TarInsufficientBuffer,
    TarNumericValueNegative,
    TarNumericValueTooBig,
    TarUnsupportedHeader,
    TargetTooSmall,
    TestExpectedApproxEqAbs,
    TestExpectedApproxEqRel,
    TestExpectedEndsWith,
    TestExpectedEqual,
    TestExpectedError,
    TestExpectedStartsWith,
    TestFailed,
    TestUnexpectedError,
    TestUnexpectedResult,
    TftpError,
    ThreadQuotaExceeded,
    Timeout,
    TimeoutTooBig,
    TlsAlert,
    TlsAlertAccessDenied,
    TlsAlertBadCertificate,
    TlsAlertBadCertificateStatusResponse,
    TlsAlertBadRecordMac,
    TlsAlertCertificateExpired,
    TlsAlertCertificateRequired,
    TlsAlertCertificateRevoked,
    TlsAlertCertificateUnknown,
    TlsAlertDecodeError,
    TlsAlertDecryptError,
    TlsAlertHandshakeFailure,
    TlsAlertIllegalParameter,
    TlsAlertInappropriateFallback,
    TlsAlertInsufficientSecurity,
    TlsAlertInternalError,
    TlsAlertMissingExtension,
    TlsAlertNoApplicationProtocol,
    TlsAlertProtocolVersion,
    TlsAlertRecordOverflow,
    TlsAlertUnexpectedMessage,
    TlsAlertUnknown,
    TlsAlertUnknownCa,
    TlsAlertUnknownPskIdentity,
    TlsAlertUnrecognizedName,
    TlsAlertUnsupportedCertificate,
    TlsAlertUnsupportedExtension,
    TlsBadEndValue,
    TlsBadLength,
    TlsBadRecordMac,
    TlsBadRsaSignatureBitCount,
    TlsBadSignatureScheme,
    TlsBadStartValue,
    TlsCertificateNotVerified,
    TlsConnectionTruncated,
    TlsDecodeError,
    TlsDecryptError,
    TlsDecryptFailure,
    TlsIllegalParameter,
    TlsInitializationFailed,
    TlsRecordOverflow,
    TlsSequenceOverflow,
    TlsUnexpectedMessage,
    TooBig,
    TooManyBreakpoints,
    TooManyHttpRedirects,
    TooManyParentDirs,
    TreelessLiteralsFirst,
    TruncatedElfFile,
    TruncatedInput,
    TruncatedIntegralType,
    TunnelNotSupported,
    UnableToCreateSymLink,
    UnableToOpenElfFile,
    UnableToSpawnCCompiler,
    UnalignedMemory,
    Underflow,
    Unexpected,
    UnexpectedCharacter,
    UnexpectedElement,
    UnexpectedEndOfInput,
    UnexpectedEndOfLiteralStream,
    UnexpectedEndOfStream,
    UnexpectedEof,
    UnexpectedExtraField,
    UnexpectedOpCode,
    UnexpectedRemainder,
    UnexpectedRepresentation,
    UnexpectedSecondSurrogateHalf,
    UnexpectedSubgroup,
    UnexpectedToken,
    UnhandledBigDirectoryStream,
    UnhelpfulFile,
    Unimplemented,
    UnimplementedExpressionCall,
    UnimplementedOpcode,
    UnimplementedTypeConversion,
    UnimplementedTypedComparison,
    UnimplementedUserOpcode,
    UnknownApplicationBinaryInterface,
    UnknownArchitecture,
    UnknownCpuFeature,
    UnknownCpuModel,
    UnknownExpressionOpcode,
    UnknownField,
    UnknownHostName,
    UnknownHttpMethod,
    UnknownName,
    UnknownObjectFormat,
    UnknownOid,
    UnknownOperatingSystem,
    UnknownPDBVersion,
    UnknownTotalSystemMemory,
    UnknownVariable,
    UnmappedMemory,
    UnrecognizedFormat,
    UnrecognizedGnuLibCFileName,
    UnrecognizedVolume,
    UnrecoverableInvalidExe,
    UnresolvedScope,
    UnsafeProgram,
    Unseekable,
    UnspecifiedBlockId,
    UnsuportedAbbrevEncoding,
    Unsupported,
    UnsupportedAddrSize,
    UnsupportedArchitecture,
    UnsupportedArrayElement,
    UnsupportedBlockId,
    UnsupportedBlockInfoRecord,
    UnsupportedCertificateVersion,
    UnsupportedCompressionMethod,
    UnsupportedDebugInfo,
    UnsupportedDwarfVersion,
    UnsupportedEncoding,
    UnsupportedFeature,
    UnsupportedFlags,
    UnsupportedOperation,
    UnsupportedRegister,
    UnsupportedReparsePointType,
    UnsupportedTarget,
    UnsupportedUriScheme,
    UnsupportedWasmVersion,
    UnusedValue,
    UriMissingHost,
    UserFdQuotaExceeded,
    UserMarkQuotaExceeded,
    UserNotFound,
    Utf16InvalidStartCodeUnit,
    Utf8CannotEncodeSurrogateHalf,
    Utf8CodepointTooLarge,
    Utf8EncodesSurrogateHalf,
    Utf8ExpectedContinuation,
    Utf8InvalidStartByte,
    Utf8OverlongEncoding,
    ValueNameNotFound,
    ValueTooLong,
    VersionTooLong,
    VolumeCorrupted,
    VolumeFull,
    WasmCompilationFailed,
    WeakParameters,
    WeakPublicKey,
    WindowOversize,
    WindowSizeUnknown,
    WindowTooSmall,
    WindowsSdkNotFound,
    WouldBlock,
    WouldExceedMaximumResidentSetSize,
    WriteFailed,
    WriteProtected,
    WrongChecksum,
    WrongGzipChecksum,
    WrongGzipSize,
    WrongStoredBlockNlen,
    WrongType,
    WrongZlibChecksum,
    ZigIsTheCCompiler,
    ZigProtocolVersionMismatch,
    Zip64CentralDirectoryOffsetMismatch,
    Zip64CentralDirectorySizeMismatch,
    Zip64RecordCountTotalMismatch,
    ZipBadCd64Size,
    ZipBadCdOffset,
    ZipBadDirectorySize,
    ZipBadEndRecord64Sig,
    ZipBadExtraFieldSize,
    ZipBadFileOffset,
    ZipBadFilename,
    ZipBadLocatorSig,
    ZipCdOversized,
    ZipCdUndersized,
    ZipDecompressTruncated,
    ZipDiskRecordCountTooLarge,
    ZipEncryptionUnsupported,
    ZipEndRecord64SizeTooSmall,
    ZipEndRecord64UnhandledExtraData,
    ZipFilenameHasBackslash,
    ZipInsufficientBuffer,
    ZipInvalid,
    ZipMismatchCompLen,
    ZipMismatchCrc32,
    ZipMismatchFilenameLen,
    ZipMismatchFlags,
    ZipMismatchModDate,
    ZipMismatchModTime,
    ZipMismatchUncompLen,
    ZipMismatchVersionNeeded,
    ZipMultiDiskUnsupported,
    ZipNoEndRecord,
    ZipTruncated,
    ZipUnsupportedVersion,
    ZipUnsupportedZip64DiskCount
}
```

</details>

