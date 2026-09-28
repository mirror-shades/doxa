# Standard Library

## How to use
The standard library can be included as a module using the `@std()` method which returns a string of the path to the library.
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
  `max_redirects` on the request before calling `request(method, url, req)`.
  `post(url, req)`, `put(url, req)`, `delete(url, req)`, and `head(url, req)` are
  thin method-specific wrappers; `request` also accepts `"GET"`.

  ```doxa
  var req is std.http.Request.new()
  req.body is "hello"
  @push(req.headers, "Content-Type: text/plain")
  @push(req.headers, "X-Trace: demo")
  const response is std.http.post("https://example.com/submit", req)
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
- The sequential server primitives bind only to loopback: `listen(port)` (use
  `0` for an ephemeral port), `localAddr(listener)`, `accept(listener)`,
  `respond(connection, status, headers, body)`, and `close(handle)`. The
  `respond` header argument is a CRLF-separated blob; `Content-Length`,
  `Transfer-Encoding`, and `Connection` are supplied by the library.
- `download(url)` / `downloadUrl(url, destination)` use the same redirect,
  timeout, and status handling as requests, but stream response bytes directly
  to the destination file. Non-2xx responses still write their body and then
  return `error.IO.HttpStatus`.

## IO


## JSON

`std.json` parses a document into a handle-based tree and writes one back out.
A `Node` is a small generation-checked handle into a single document slot, so
retaining one past the next `parse` is safe: it reads as `Kind.Invalid` rather
than a dangling value.

```doxa
module std from @std()

const result is std.json.parse("{\"name\":\"doxa\",\"stars\":5}")
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

