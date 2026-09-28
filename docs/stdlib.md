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


## Methods


## Process


## Random


## Time

