# Standard Library

## How to use
The standard library is imported like any other module. `@std()` is its specifier, the string `"std//std.doxa"` (the file `std.doxa` under the `std` root; see [Modules](modules.md#specifiers)).
```
module std from @std()
```

## Build


## File


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

- `get(url)` returns `Response | error.StdError`. HTTP status is data; a `404`
  still returns a response. `getText(url)` returns the body regardless of
  status, while `checkStatus(response)` opts into `error.IO.HttpStatus` for
  non-2xx responses.
- `Request.new()` creates request settings with an empty `headers` array and
  defaults of a 30-second response-read timeout and three followed redirects.
  Each header entry is a `"Name: value"` string. Set `body`, `timeout_ms`, or
  `max_redirects` on the request before calling `request(method, url, ^req)`.
  `post(url, ^req)`, `put(url, ^req)`, `delete(url, ^req)`, and `head(url, ^req)`
  are thin method-specific wrappers; `request` also accepts `"GET"`. The request
  is borrowed with `^` so its `headers` array is not deep-copied per call.

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
- `timeout_ms` and `getWithTimeout(url, timeout_ms)` bound response-head and body
  reads. They do not interrupt DNS lookup, connection establishment, or request
  writes. A value of `0` disables the read deadline.
- Redirects are followed up to `max_redirects` times (three by default); an
  exhausted limit is a request error. `Authorization`, `Proxy-Authorization`,
  `Cookie`, and `Cookie2` are stripped when the redirect changes the strict
  origin (scheme, host, or effective port). Other custom headers continue to
  the destination.
- The server primitives bind only to loopback: `listen(port)` (use `0` for an
  ephemeral port), `localAddr(listener)`, `accept(listener)`, `close(handle)`,
  and the request/response pair `readRequest(connection)` /
  `takeRequest(connection)` and `respond(connection, status, headers, body)`.
  `readRequest` is **non-blocking**: it returns `1` when a request is ready
  (read it with `takeRequest`, a `ServerRequest` whose `.verb`, `.target`, and
  `.body_text` hold the parsed fields, with `header(name)` / `hasHeader(name)`
  reading repeated request headers case-insensitively), `0` when more bytes are
  needed (retry on the next `Readable` event), and a negative status on a
  connection error. `respond` must follow a successfully parsed request; its
  `headers` argument is a CRLF-separated blob, and the library supplies
  `Content-Length` and connection framing. A client that asks for
  `Connection: close` gets it back, and `keepAlive(connection)` reports whether
  the request may reuse the connection. `accept(listener)` is the low-level
  primitive; a poll loop reads requests from `poll` events instead.

- `poll(listener, timeout_ms)` waits across the listener and every live
  connection at once and returns `Event[]`. Each `Event` carries a connection
  `handle` and a `kind :: EventKind`: `Accepted` for a freshly accepted
  connection, `Readable` for data to read, and `Closed` for a peer that closed
  or errored. `timeout_ms == 0` polls immediately and a negative value blocks
  until an event; closing the listener is the way to stop a loop. There is no
  fixed connection ceiling. A server serves many keep-alive clients without
  blocking on any one connection by polling instead of calling `accept`
  directly:

  ```doxa
  const listener is std.http.listen(0) as int else 0
  while true {
      const events is std.http.poll(listener, 1000)
      for i while i < @length(events) do i += 1 {
          const event is events[i]
          if event.kind == std.http.EventKind.Closed then {
              std.http.close(event.handle)
          } else {
              # 1 = request ready, 0 = wait for more bytes, negative = error.
              const status is std.http.readRequest(event.handle)
              if status == 1 then {
                  const value is std.http.takeRequest(event.handle)
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
  `route(^router, verb, path) → Route | nothing`. A pattern segment written
  `:name` captures the matching path segment, and `Route.param(name)` reads the
  capture. Matching is exact per segment and verb, so `/users/:id` matches
  `/users/42` with `param("id") == "42"`, but not `/users/42/posts` or a
  different verb. (`{}` placeholders are not usable: Doxa string literals
  interpolate on `{`.)

  ```doxa
  var router is std.http.Router.new()
  router.add("GET", "/users/:id", 1)

  const matched is std.http.route(^router, "GET", "/users/42")
  matched as Route then {
      const id is matched.param("id") as string else ""
      @print("user {id}\n")
  }
  ```

- `download(url)` / `downloadUrl(url, destination)` use the same redirect,
  timeout, and status handling as requests, but stream response bytes directly
  to the destination file. Non-2xx responses still write their body and then
  return `error.IO.HttpStatus`.
- WebSockets upgrade a connection after its handshake request has been read:
  `isWebSocket(connection)` reports whether the handshake has completed, and
  `upgradeWebSocket(connection)` completes the RFC 6455 handshake on a buffered
  request that asked to switch protocols. `wsSend(connection, op, data)` sends
  one frame; `wsNext(connection)` advances the decoder and returns `1` when a
  message is ready (read it with `wsMessage`, whose `op` is `WsOp.Text`,
  `Binary`, `Ping`, `Pong`, or `Close` and whose `data` is the payload), `0` when
  more bytes are needed (retry on the next `Readable` event), `2` when the peer
  closed, and a negative status on a protocol or transport error. Fragmented
  messages are reassembled before they are surfaced, and control frames are
  surfaced rather than handled silently, so the caller owns ping/pong/close
  policy. Decoding is non-blocking, so a partial frame from one peer cannot stall
  other connections. A text message that is not valid UTF-8, a malformed close
  frame, or a frame larger than 16 MiB fails the connection with the
  corresponding Close code (`1007` / `1002` / `1009`).

  ```doxa
  const listener is std.http.listen(0) as int else 0
  while true {
      const events is std.http.poll(listener, 1000)
      for i while i < @length(events) do i += 1 {
          const event is events[i]
          if event.kind == std.http.EventKind.Closed then {
              std.http.close(event.handle)
          } else {
              if not std.http.isWebSocket(event.handle) then {
                  const status is std.http.readRequest(event.handle)
                  if status == 1 then {
                      std.http.upgradeWebSocket(event.handle)
                  } else if status < 0 then {
                      std.http.close(event.handle)
                  }
              }
              if std.http.isWebSocket(event.handle) then {
                  var reading is true
                  while reading {
                      const ws is std.http.wsNext(event.handle)
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
    It returns `false` while the request is still incomplete.
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

- Client WebSockets connect out to a `ws://` or `wss://` endpoint with
  `wsConnect(url, headers)`, where `headers` is a preformatted `Name: value\r\n`
  blob added to the handshake (TLS and certificate validation come from the
  pooled HTTP client). It returns a connection handle or an error. Client frames
  are masked automatically. `wsClientSend(connection, op, data)` sends one
  frame; `wsClientNext(connection)` blocks until a message is ready and returns
  `1` (read it with `wsClientMessage`), `2` when the peer closed, or `-1` on
  error. The codec answers inbound pings automatically and surfaces a close as
  `WsOp.Close`. Call `wsClientClose(connection)` for the close handshake, then
  `wsClientRelease(connection)` to free the connection.

  ```doxa
  const ws is std.http.wsConnect("wss://example.com/socket", "") as int else 0
  if ws > 0 then {
      std.http.wsClientSend(ws, std.http.WsOp.Text, "hello")
      const status is std.http.wsClientNext(ws)
      if status == 1 then {
          const m is std.http.wsClientMessage(ws)
          @print("{m.data}\n")
      }
      std.http.wsClientClose(ws)
      std.http.wsClientRelease(ws)
  }
  ```


## IO


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

- `parse(text)` returns `Node | error.StdError`. Objects keep their key order.
  Duplicate keys are rejected. A malformed document, invalid UTF-8, and a
  `number_string` that is not finite all surface as `error.IO.InvalidData`;
  the writer returns `error.Common.InvalidArgument` for misuse (a value where a
  key was expected, an unbalanced end, a second root value, and so on).
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
  `finish()` returns the escaped text as `string | error.StdError` and releases
  the writer; the next writer call starts a fresh document.
- The writer emits `\b`, `\f`, `\n`, `\r`, `\t` as their two-character escapes
  and any other control byte below `0x20` as a lowercase `\u00xx` sequence. It
  validates UTF-8 before quoting and refuses NaN or infinity.

## Methods



## Process


## Random


## Time

