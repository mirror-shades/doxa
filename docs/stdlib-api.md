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
public function print(input :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function print(input :: string) returns error.StdError {
    IO.print(input);
    const err is IO.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `println`

```doxa
public function println(input :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function println(input :: string) returns error.StdError {
    IO.printlnStdout(input);
    const err is IO.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `eprint`

```doxa
public function eprint(input :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function eprint(input :: string) returns error.StdError {
    IO.eprint(input);
    const err is IO.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `eprintln`

```doxa
public function eprintln(input :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function eprintln(input :: string) returns error.StdError {
    IO.eprintlnStderr(input);
    const err is IO.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `input`

```doxa
public function input() returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function input() returns string | error.StdError {
    const value is IO.input();
    const err is IO.takeLastErrorCode()
    if err == 0 then return value
    return mapIOError(err)
}
```

</details>

### `inputByte`

```doxa
public function inputByte() returns byte | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function inputByte() returns byte | error.StdError {
    const value is IO.inputByte()
    const err is IO.takeLastErrorCode()
    if err == 0 then return @byte(value)
    return mapIOError(err)
}
```

</details>

### `inputPrompt`

```doxa
public function inputPrompt(prompt :: string) returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function inputPrompt(prompt :: string) returns string | error.StdError {
    const value is IO.inputPrompt(prompt);
    const err is IO.takeLastErrorCode()
    if err == 0 then return value
    return mapIOError(err)
}
```

</details>

## `std.process`

### `exec`

```doxa
public function exec(input :: string) returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function exec(input :: string) returns int | error.StdError {
    const code is Process.exec_cmd(input)
    const err is Process.takeLastErrorCode()
    if err == 0 then return code
    return mapProcessError(err)
}
```

</details>

### `execCapture`

```doxa
public function execCapture(input :: string) returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function execCapture(input :: string) returns string | error.StdError {
    const output is Process.exec_capture_stdout(input)
    const err is Process.takeLastErrorCode()
    if err == 0 then return output
    return mapProcessError(err)
}
```

</details>

### `execCaptureEnv`

```doxa
public function execCaptureEnv(program :: string, args :: string[], env_keys :: string[], env_values :: string[]) returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function execCaptureEnv(program :: string, args :: string[], env_keys :: string[], env_values :: string[]) returns string | error.StdError {
    const output is Process.exec_capture_stdout_env(program, args, env_keys, env_values)
    const err is Process.takeLastErrorCode()
    if err == 0 then return output
    return mapProcessError(err)
}
```

</details>

### `execArgs`

```doxa
public function execArgs(program :: string, args :: string[]) returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function execArgs(program :: string, args :: string[]) returns int | error.StdError {
    const code is Process.exec_args(program, args)
    const err is Process.takeLastErrorCode()
    if err == 0 then return code
    return mapProcessError(err)
}
```

</details>

### `execArgsEnv`

```doxa
public function execArgsEnv(program :: string, args :: string[], env_keys :: string[], env_values :: string[]) returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function execArgsEnv(program :: string, args :: string[], env_keys :: string[], env_values :: string[]) returns int | error.StdError {
    const code is Process.exec_args_env(program, args, env_keys, env_values)
    const err is Process.takeLastErrorCode()
    if err == 0 then return code
    return mapProcessError(err)
}
```

</details>

### `execOk`

```doxa
public function execOk(input :: string) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function execOk(input :: string) returns tetra {
    const result is exec(input)
    result as int then {
        return true
    }
    else {
        return false
    }
}
```

</details>

### `args`

```doxa
public function args() returns string[] | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function args() returns string[] | error.StdError {
    const _argc is argc() as int else {
        return mapProcessError(Process.takeLastErrorCode())
    }
    var result :: string[]
    for i while i < _argc do i += 1 {
        const arg is argv(i) as string else {
            return mapProcessError(Process.takeLastErrorCode())
        }
        @push(result, arg)
    }
    const err is Process.takeLastErrorCode()
    if err == 0 then return result
    return mapProcessError(err)
}
```

</details>

### `argc`

```doxa
public function argc() returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function argc() returns int | error.StdError {
    const count is Process.argCount();
    const err is Process.takeLastErrorCode()
    if err == 0 then return count
    return mapProcessError(err)
}
```

</details>

### `argv`

```doxa
public function argv(index :: int) returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function argv(index :: int) returns string | error.StdError {
    const value is Process.argAt(@string(index));
    const err is Process.takeLastErrorCode()
    if err == 0 then return value
    return mapProcessError(err)
}
```

</details>

### `exit`

```doxa
public function exit(code :: int)
```

<details>
<summary>Source</summary>

```doxa
public function exit(code :: int) {
    Process.exitCode(@string(code));
}
```

</details>

### `cwd`

```doxa
public function cwd() returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function cwd() returns string | error.StdError {
    const value is Process.cwd();
    const err is Process.takeLastErrorCode()
    if err == 0 then return value
    return mapProcessError(err)
}
```

</details>

### `getenv`

```doxa
public function getenv(name :: string) returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function getenv(name :: string) returns string | error.StdError {
    const value is Process.getEnv(name);
    const err is Process.takeLastErrorCode()
    if err == 0 then return value
    return mapProcessError(err)
}
```

</details>

## `std.file`

### `makeDir`

```doxa
public function makeDir(path :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function makeDir(path :: string) returns nothing | error.StdError {
    File.makeDir(path)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `makePath`

```doxa
public function makePath(path :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function makePath(path :: string) returns nothing | error.StdError {
    File.makePath(path)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `copyFile`

```doxa
public function copyFile(source :: string, destination :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function copyFile(source :: string, destination :: string) returns nothing | error.StdError {
    File.copyFile(source, destination)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `delete`

```doxa
public function delete(path :: string, recursive :: tetra) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function delete(path :: string, recursive :: tetra) returns nothing | error.StdError {
    File.delete(path, recursive)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `copyDir`

```doxa
public function copyDir(source :: string, destination :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function copyDir(source :: string, destination :: string) returns nothing | error.StdError {
    File.copyDir(source, destination)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `unzipFile`

```doxa
public function unzipFile(zip_path :: string, unzip_path :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function unzipFile(zip_path :: string, unzip_path :: string) returns nothing | error.StdError {
    File.unzipFile(zip_path, unzip_path)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
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
    return File.IsDir(path)
}
```

</details>

### `renameDir`

```doxa
public function renameDir(old_folder :: string, new_folder :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function renameDir(old_folder :: string, new_folder :: string) returns nothing | error.StdError {
    File.renameDir(old_folder, new_folder)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `rename`

```doxa
public function rename(old_path :: string, new_path :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function rename(old_path :: string, new_path :: string) returns nothing | error.StdError {
    File.rename(old_path, new_path)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `read`

```doxa
public function read(path :: string) returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function read(path :: string) returns string | error.StdError {
    const value is File.read(path)
    const err is File.takeLastErrorCode()
    if err == 0 then return value
    return mapFileError(err)
}
```

</details>

### `write`

```doxa
public function write(path :: string, content :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function write(path :: string, content :: string) returns nothing | error.StdError {
    File.write(path, content)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `append`

```doxa
public function append(path :: string, content :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function append(path :: string, content :: string) returns nothing | error.StdError {
    File.append(path, content)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

### `exists`

```doxa
public function exists(path :: string) returns tetra
```

<details>
<summary>Source</summary>

```doxa
public function exists(path :: string) returns tetra {
    return File.exists(path)
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
public function list(path :: string) returns string[] | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function list(path :: string) returns string[] | error.StdError {
    const value is File.list(path)
    const err is File.takeLastErrorCode()
    if err == 0 then return value
    return mapFileError(err)
}
```

</details>

### `touch`

```doxa
public function touch(path :: string) returns nothing | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function touch(path :: string) returns nothing | error.StdError {
    File.touch(path)
    const err is File.takeLastErrorCode()
    if err == 0 then return
    return mapFileError(err)
}
```

</details>

## `std.http`

### `mapIOError`

```doxa
public function mapIOError(code :: int) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function mapIOError(code :: int) returns error.StdError {
    return match code {
        1 then error.Common.InvalidArgument,
        2 then error.Common.OutOfMemory,
        3 then error.Common.PermissionDenied,
        4 then error.Common.NotSupported,
        100 then error.IO.NotFound,
        101 then error.IO.AlreadyExists,
        102 then error.IO.NotDirectory,
        103 then error.IO.IsDirectory,
        104 then error.IO.InvalidPath,
        105 then error.IO.OpenFailed,
        106 then error.IO.ReadFailed,
        107 then error.IO.WriteFailed,
        108 then error.IO.CreateFailed,
        109 then error.IO.DeleteFailed,
        110 then error.IO.RenameFailed,
        111 then error.IO.CopyFailed,
        112 then error.IO.IterateFailed,
        113 then error.IO.InvalidData,
        114 then error.IO.ConnectionFailed,
        115 then error.IO.Timeout,
        116 then error.IO.HttpStatus,
        117 then error.IO.Unexpected,
        else error.Common.Unexpected,
    }
}
```

</details>

### `downloadUrl`

```doxa
public function downloadUrl(url :: string, destination :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function downloadUrl(url :: string, destination :: string) returns error.StdError {
    HTTP.downloadUrl(url, destination)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `download`

```doxa
public function download(url :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function download(url :: string) returns error.StdError {
    HTTP.download(url)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
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
public function listen(port :: int) returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function listen(port :: int) returns int | error.StdError {
    const handle is HTTP.listen(port)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return handle
    return mapIOError(err)
}
```

</details>

### `localAddr`

```doxa
public function localAddr(handle :: int) returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function localAddr(handle :: int) returns int | error.StdError {
    const port is HTTP.localAddr(handle)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return port
    return mapIOError(err)
}
```

</details>

### `accept`

```doxa
public function accept(handle :: int) returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function accept(handle :: int) returns int | error.StdError {
    const connection is HTTP.accept(handle)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return connection
    return mapIOError(err)
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
public function readRequest(connection :: int) returns int
```

Parse the next request from the connection's buffer. Returns `1` when a
request is ready (read it with `takeRequest`), `0` when more bytes are needed
(retry on the next `Readable` event), and a negative status on a connection
error. Decoding is non-blocking, so a slow or partial sender cannot stall the
loop.

<details>
<summary>Source</summary>

```doxa
public function readRequest(connection :: int) returns int {
    return HTTP.readRequest(connection)
}
```

</details>

### `takeRequest`

```doxa
public function takeRequest(connection :: int) returns ServerRequest
```

The request parsed by the last successful `readRequest`.

<details>
<summary>Source</summary>

```doxa
public function takeRequest(connection :: int) returns ServerRequest {
    return $ServerRequest {
        verb is HTTP.requestMethod(connection),
        target is HTTP.requestTarget(connection),
        body_text is HTTP.requestBody(connection),
        raw_headers is HTTP.requestHead(connection),
    }
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
public function upgradeWebSocket(connection :: int) returns error.StdError
```

Complete the RFC 6455 handshake on an accepted connection whose pending
request asked to upgrade. Call after `readRequest`, before `wsSend`/`wsNext`.

<details>
<summary>Source</summary>

```doxa
public function upgradeWebSocket(connection :: int) returns error.StdError {
    HTTP.upgradeWebSocket(connection)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `wsSend`

```doxa
public function wsSend(connection :: int, op :: WsOp, data :: string) returns error.StdError
```

Send one WebSocket frame with `data` as its payload.

<details>
<summary>Source</summary>

```doxa
public function wsSend(connection :: int, op :: WsOp, data :: string) returns error.StdError {
    HTTP.wsSend(connection, wsOpCode(op), data)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `wsNext`

```doxa
public function wsNext(connection :: int) returns int
```

Advance the connection's frame decoder. Returns `1` when a message is ready
(read it with `wsMessage`), `0` when more bytes are needed (retry on the next
`Readable` event), `2` when the peer closed, and a negative status on a
protocol or transport error. Fragmented messages are reassembled before they
are surfaced. Decoding is non-blocking.

<details>
<summary>Source</summary>

```doxa
public function wsNext(connection :: int) returns int {
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

### `wsBuffered`

```doxa
public function wsBuffered(connection :: int) returns tetra
```

Whether undecoded bytes are already buffered for the connection. A poll loop
uses `while std.http.wsBuffered(handle) { ... wsNext ... }` to drain frames
that arrived with (or pipelined behind) the handshake before waiting on
another readiness event.

<details>
<summary>Source</summary>

```doxa
public function wsBuffered(connection :: int) returns tetra {
    return HTTP.wsBuffered(connection)
}
```

</details>

### `takeLastErrorCode`

```doxa
public function takeLastErrorCode() returns int
```

The last error code left by `readRequest`/`ws*`, taken and cleared.

<details>
<summary>Source</summary>

```doxa
public function takeLastErrorCode() returns int {
    return HTTP.takeLastErrorCode()
}
```

</details>

### `EventKind`

```doxa
public enum EventKind
```

Why a `poll` event fired. `Accepted` is a fresh connection, `Readable` is a
connection with bytes to read, and `Closed` is a peer that closed or errored.
The inline-Zig boundary only carries scalars, so the codes crossing it are
mapped by `eventKindFromCode`.

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
public function poll(listener :: int, timeout_ms :: int) returns Event[]
```

Wait for readiness across the listener and every live connection. Blocks up
to `timeout_ms` (0 polls immediately, negative waits indefinitely) and
returns the events that fired.

<details>
<summary>Source</summary>

```doxa
public function poll(listener :: int, timeout_ms :: int) returns Event[] {
    HTTP.poll(listener, timeout_ms)
    var events :: Event[]
    const count is HTTP.eventCount()
    for i while i < count do i += 1 {
        @push(events, $Event {
            handle is HTTP.eventHandle(i),
            kind is eventKindFromCode(HTTP.eventKind(i)),
        })
    }
    return events
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

### `route`

```doxa
public function route(^router :: Router, verb :: string, path :: string) returns Route | nothing
```

`^router` borrows the caller's Router instead of snapshotting its three
arrays on every call, which the arena model would otherwise deep-copy per
request (docs/memory.md by-value parameters snapshot).

<details>
<summary>Source</summary>

```doxa
public function route(^router :: Router, verb :: string, path :: string) returns Route | nothing {
    for i while i < @length(router.patterns) do i += 1 {
        if router.verbs[i] == verb then {
            if HTTP.matchPattern(router.patterns[i], path) == 1 then {
                var names :: string[]
                var values :: string[]
                const count is HTTP.paramCount()
                for j while j < count do j += 1 {
                    @push(names, HTTP.paramNameAt(j))
                    @push(values, HTTP.paramValueAt(j))
                }
                return $Route {
                    id is router.ids[i],
                    pattern is router.patterns[i],
                    param_names is names,
                    param_values is values,
                }
            }
        }
    }
    return nothing
}
```

</details>

### `respond`

```doxa
public function respond(connection :: int, status :: int, headers :: string, body :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function respond(connection :: int, status :: int, headers :: string, body :: string) returns error.StdError {
    HTTP.respond(connection, status, headers, body)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `close`

```doxa
public function close(handle :: int) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function close(handle :: int) returns error.StdError {
    HTTP.close(handle)
    const err is HTTP.takeLastErrorCode()
    if err == 0 then return
    return mapIOError(err)
}
```

</details>

### `get`

```doxa
public function get(url :: string) returns Response | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function get(url :: string) returns Response | error.StdError {
    return performGet(url, 30000)
}
```

</details>

### `getWithTimeout`

```doxa
public function getWithTimeout(url :: string, timeout_ms :: int) returns Response | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function getWithTimeout(url :: string, timeout_ms :: int) returns Response | error.StdError {
    return performGet(url, timeout_ms)
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

### `request`

```doxa
public function request(verb :: string, url :: string, ^req :: Request) returns Response | error.StdError
```

`^req` borrows the caller's Request so `req.headers` is not deep-copied.

<details>
<summary>Source</summary>

```doxa
public function request(verb :: string, url :: string, ^req :: Request) returns Response | error.StdError {
    var header_blob is ""
    for i while i < @length(req.headers) do i += 1 {
        header_blob is header_blob + req.headers[i] + "\u{d}\u{a}"
    }
    const value is HTTP.performRequest(verb, url, header_blob, req.body, req.timeout_ms, req.max_redirects)
    const status is HTTP.takeStatusCode()
    const headers is HTTP.takeResponseHeaders()
    const err is HTTP.takeLastErrorCode()
    HTTP.releaseResponse()
    if err == 0 then return $Response {
        status_code is status,
        raw_headers is headers,
        body_text is value,
    }
    return mapIOError(err)
}
```

</details>

### `post`

```doxa
public function post(url :: string, ^req :: Request) returns Response | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function post(url :: string, ^req :: Request) returns Response | error.StdError {
    return request("POST", url, ^req)
}
```

</details>

### `put`

```doxa
public function put(url :: string, ^req :: Request) returns Response | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function put(url :: string, ^req :: Request) returns Response | error.StdError {
    return request("PUT", url, ^req)
}
```

</details>

### `delete`

```doxa
public function delete(url :: string, ^req :: Request) returns Response | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function delete(url :: string, ^req :: Request) returns Response | error.StdError {
    return request("DELETE", url, ^req)
}
```

</details>

### `head`

```doxa
public function head(url :: string, ^req :: Request) returns Response | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function head(url :: string, ^req :: Request) returns Response | error.StdError {
    return request("HEAD", url, ^req)
}
```

</details>

### `getText`

```doxa
public function getText(url :: string) returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function getText(url :: string) returns string | error.StdError {
    const value is HTTP.getText(url, 30000)
    const _status is HTTP.takeStatusCode()
    const _headers is HTTP.takeResponseHeaders()
    const err is HTTP.takeLastErrorCode()
    HTTP.releaseResponse()
    if err == 0 then return value
    return mapIOError(err)
}
```

</details>

### `checkStatus`

```doxa
public function checkStatus(response :: Response) returns Response | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function checkStatus(response :: Response) returns Response | error.StdError {
    const status is response.statusCode()
    if status < 200 or status >= 300 then return error.IO.HttpStatus
    return response
}
```

</details>

### `wsAccept`

```doxa
public function wsAccept(connection :: int) returns tetra
```

Complete the handshake if `connection`'s buffered request is a WebSocket
upgrade. Returns `true` once it is a WebSocket; while it is still `false`,
call again after the next readable event.

<details>
<summary>Source</summary>

```doxa
public function wsAccept(connection :: int) returns tetra {
    if isWebSocket(connection) then return true
    const status is readRequest(connection)
    if status != 1 then return false
    upgradeWebSocket(connection)
    return takeLastErrorCode() == 0
}
```

</details>

### `wsRead`

```doxa
public function wsRead(connection :: int) returns Message | nothing | error.StdError
```

The next complete message, `nothing` when none is buffered yet or the peer
closed, or an error on a protocol/transport failure. Fragmented messages are
reassembled before they are surfaced.

<details>
<summary>Source</summary>

```doxa
public function wsRead(connection :: int) returns Message | nothing | error.StdError {
    const status is wsNext(connection)
    if status == 1 then return wsMessage(connection)
    const err is takeLastErrorCode()
    if err != 0 then return mapIOError(err)
    return nothing
}
```

</details>

### `wsWrite`

```doxa
public function wsWrite(connection :: int, op :: WsOp, data :: string) returns error.StdError
```

Send one frame with an explicit opcode.

<details>
<summary>Source</summary>

```doxa
public function wsWrite(connection :: int, op :: WsOp, data :: string) returns error.StdError {
    return wsSend(connection, op, data)
}
```

</details>

### `wsText`

```doxa
public function wsText(connection :: int, data :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function wsText(connection :: int, data :: string) returns error.StdError {
    return wsSend(connection, WsOp.Text, data)
}
```

</details>

### `wsBinary`

```doxa
public function wsBinary(connection :: int, data :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function wsBinary(connection :: int, data :: string) returns error.StdError {
    return wsSend(connection, WsOp.Binary, data)
}
```

</details>

### `wsPing`

```doxa
public function wsPing(connection :: int, data :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function wsPing(connection :: int, data :: string) returns error.StdError {
    return wsSend(connection, WsOp.Ping, data)
}
```

</details>

### `wsPong`

```doxa
public function wsPong(connection :: int, data :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function wsPong(connection :: int, data :: string) returns error.StdError {
    return wsSend(connection, WsOp.Pong, data)
}
```

</details>

### `wsClose`

```doxa
public function wsClose(connection :: int) returns error.StdError
```

Send a Close frame (empty payload) and drop the connection.

<details>
<summary>Source</summary>

```doxa
public function wsClose(connection :: int) returns error.StdError {
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
public function pathFor(os :: string, parts :: string[]) returns string
```

Joins `parts` with the separator for `os` (one of the names returned by
`os()`, e.g. `"windows"`, `"linux"`, `"macos"`). Empty segments are skipped,
an absolute segment resets the result, and a separator already at the join
point is not doubled.

<details>
<summary>Source</summary>

```doxa
public function pathFor(os :: string, parts :: string[]) returns string {
    const sep is sepFor(os)
    var result is ""
    var i is 0
    while i < @length(parts) {
        const part is parts[i]
        if @length(part) == 0 then {
            i is i + 1
            continue
        }

        if isAbsolute(part, os) then {
            result is part
        } else if @length(result) == 0 then {
            result is part
        } else if isSep(result[@length(result) - 1], os) then {
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

### `joinLines`

```doxa
public function joinLines(parts :: string[]) returns string
```

The inline-Zig ABI is scalar-only, so array-valued directives cross into the
compiler shim as newline-joined strings.

<details>
<summary>Source</summary>

```doxa
public function joinLines(parts :: string[]) returns string {
    var out is ""
    var i is 0
    while i < @length(parts) {
        if i > 0 then out is out + "\n"
        out is out + parts[i]
        i is i + 1
    }
    return out
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

    public function host() returns Target {
        return $Target {
            arch is Build.hostArch(),
            os is Build.hostOs(),
            abi is Build.hostAbi(),
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
public function run(c :: Context) returns int | error.StdError
```

Drive the compiler over every artifact in the context. Returns the first
non-zero exit code or mapped error encountered, or 0 on success.

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
public function run(c :: Context) returns int | error.StdError {
    # A cross target must name its OS; an empty OS is never silently resolved to
    # the host.
    if @length(c.target.os) == 0 then return error.Common.InvalidArgument
    # Written as an assignment rather than an `if` expression: a runtime `if`
    # expression that produces a string currently yields a corrupted value.
    var opt is "debug"
    if not c.debug then opt is optModeName(c.optimization)
    var i is 0
    while i < @length(c.artifacts) {
        const art is c.artifacts[i]

        const links is joinLines(art.links)
        const libdirs is joinLines(art.libdirs)
        const frameworks is joinLines(art.frameworks)
        const includes is joinLines(art.includes)
        const code is Build.compileArtifact(art.entry_point, art.output, c.target.arch, c.target.os, c.target.abi, opt, links, libdirs, frameworks, includes)
        const err is Build.takeLastErrorCode()
        if err != 0 then return mapBuildError(err)
        if code != 0 then return code

        i is i + 1
    }
    return 0
}
```

</details>

### `execute`

```doxa
public function execute(c :: Context)
```

Drive the compiler over every artifact and propagate the outcome through the
process exit code: a successful build exits 0, an `int` result exits with that
code, and an `error.StdError` prints a mapped message to stderr and exits 1.
`execute` is the terminal statement of a build script; it never returns.

<details>
<summary>Source</summary>

```doxa
public function execute(c :: Context) {
    const outcome is run(c)
    outcome as int then {
        @print("build exit code: {@string(outcome)}\n")
        if outcome != 0 then @exit(outcome)
    } else {
        # The compiler's own diagnostics already describe the failing artifact;
        # here we only need to signal the failure on the standard channel.
        io.eprintln("Build failed")
        @exit(1)
    }
}
```

</details>

### `compile`

```doxa
public function compile(src :: string, out :: string, arch :: string, os :: string, abi :: string, debug :: tetra) returns int | error.StdError
```

Low-level single-file compile, retained for direct use and the build test
harness. `debug` forces the debug optimization mode.

<details>
<summary>Source</summary>

```doxa
public function compile(src :: string, out :: string, arch :: string, os :: string, abi :: string, debug :: tetra) returns int | error.StdError {
    # Assignment form: a runtime `if` expression producing a string is corrupt.
    var opt is "debug"
    if not debug then opt is "fast"
    const code is Build.compileArtifact(src, out, arch, os, abi, opt, "", "", "", "")
    const err is Build.takeLastErrorCode()
    if err == 0 then return code
    return mapBuildError(err)
}
```

</details>

### `compileHost`

```doxa
public function compileHost(src :: string, out :: string, debug :: tetra) returns int | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function compileHost(src :: string, out :: string, debug :: tetra) returns int | error.StdError {
    return compile(src, out, Build.hostArch(), Build.hostOs(), Build.hostAbi(), debug)
}
```

</details>

## `std.error`

### `Common`

```doxa
public enum Common
```

<details>
<summary>Source</summary>

```doxa
public enum Common {
    InvalidArgument,
    OutOfMemory,
    PermissionDenied,
    NotSupported,
    Unexpected,
}
```

</details>

### `IO`

```doxa
public enum IO
```

<details>
<summary>Source</summary>

```doxa
public enum IO {
    NotFound,
    AlreadyExists,
    NotDirectory,
    IsDirectory,
    InvalidPath,
    OpenFailed,
    ReadFailed,
    WriteFailed,
    CreateFailed,
    DeleteFailed,
    RenameFailed,
    CopyFailed,
    IterateFailed,
    InvalidData,
    ConnectionFailed,
    Timeout,
    HttpStatus,
    Unexpected,
}
```

</details>

### `Process`

```doxa
public enum Process
```

<details>
<summary>Source</summary>

```doxa
public enum Process {
    CommandNotFound,
    NotExecutable,
    SpawnFailed,
    WaitFailed,
    KillFailed,
    CaptureFailed,
    OutputTooLarge,
    NonZeroExit,
    Unexpected,
}
```

</details>

### `Method`

```doxa
public enum Method
```

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

### `StdError`

```doxa
public group StdError
```

<details>
<summary>Source</summary>

```doxa
public group StdError {
    Common,
    IO,
    Process,
    Method
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
public function insert(^collection :: string | int[] | float[] | byte[] | tetra[] | string[], index :: int, value :: string | int | float | byte | tetra) returns error.Method
```

<details>
<summary>Source</summary>

```doxa
public function insert(^collection :: string | int[] | float[] | byte[] | tetra[] | string[], index :: int, value :: string | int | float | byte | tetra) returns error.Method {
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

    public method count() returns int | nothing {
        const value is JSON.nodeCount(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
    }

    public method element(index :: int) returns Node | nothing {
        const child is JSON.nodeChild(this.handle, index)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return Node.new(child)
    }

    public method field(name :: string) returns Node | nothing {
        const child is JSON.nodeField(this.handle, name)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return Node.new(child)
    }

    public method key() returns string | nothing {
        const value is JSON.nodeKey(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
    }

    public method text() returns string | nothing {
        const value is JSON.nodeText(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
    }

    public method intValue() returns int | nothing {
        const value is JSON.nodeIntValue(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
    }

    public method floatValue() returns float | nothing {
        const value is JSON.nodeFloatValue(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
    }

    public method booleanValue() returns tetra | nothing {
        const value is JSON.nodeBooleanValue(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
    }
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

<details>
<summary>Source</summary>

```doxa
public method count() returns int | nothing {
        const value is JSON.nodeCount(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
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
        if JSON.takeLastErrorCode() != 0 then return nothing
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
        if JSON.takeLastErrorCode() != 0 then return nothing
        return Node.new(child)
    }
```

</details>

#### `Node.key`

```doxa
public method key() returns string | nothing
```

<details>
<summary>Source</summary>

```doxa
public method key() returns string | nothing {
        const value is JSON.nodeKey(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
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
        const value is JSON.nodeText(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
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
        const value is JSON.nodeIntValue(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
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
        const value is JSON.nodeFloatValue(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
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
        const value is JSON.nodeBooleanValue(this.handle)
        if JSON.takeLastErrorCode() != 0 then return nothing
        return value
    }
```

</details>

### `parse`

```doxa
public function parse(text :: string) returns Node | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function parse(text :: string) returns Node | error.StdError {
    const root is JSON.parseDocument(text)
    const code is JSON.takeLastErrorCode()
    if code == 0 then return Node.new(root)
    return mapJSONError(code)
}
```

</details>

### `beginObject`

```doxa
public function beginObject() returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function beginObject() returns error.StdError {
    JSON.writerBeginObject()
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `beginArray`

```doxa
public function beginArray() returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function beginArray() returns error.StdError {
    JSON.writerBeginArray()
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `end`

```doxa
public function end() returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function end() returns error.StdError {
    JSON.writerEnd()
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `writeKey`

```doxa
public function writeKey(name :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function writeKey(name :: string) returns error.StdError {
    JSON.writerKey(name)
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `writeText`

```doxa
public function writeText(text :: string) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function writeText(text :: string) returns error.StdError {
    JSON.writerText(text)
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `writeInt`

```doxa
public function writeInt(value :: int) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function writeInt(value :: int) returns error.StdError {
    JSON.writerInt(value)
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `writeFloat`

```doxa
public function writeFloat(value :: float) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function writeFloat(value :: float) returns error.StdError {
    JSON.writerFloat(value)
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `writeBoolean`

```doxa
public function writeBoolean(value :: tetra) returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function writeBoolean(value :: tetra) returns error.StdError {
    if value == both or value == neither then {
        JSON.writerReject(1)
        return error.Common.InvalidArgument
    }
    JSON.writerBoolean(value == true)
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `writeNull`

```doxa
public function writeNull() returns error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function writeNull() returns error.StdError {
    JSON.writerNull()
    const code is JSON.takeLastErrorCode()
    if code == 0 then return
    return mapJSONError(code)
}
```

</details>

### `finish`

```doxa
public function finish() returns string | error.StdError
```

<details>
<summary>Source</summary>

```doxa
public function finish() returns string | error.StdError {
    const value is JSON.writerFinish()
    const code is JSON.takeLastErrorCode()
    if code == 0 then return value
    return mapJSONError(code)
}
```

</details>

