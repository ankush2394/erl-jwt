# JSON Web Token (jwt)

[![Hex.pm](https://img.shields.io/badge/hex-0.1.8-aa66cc.svg)](https://hex.pdmbuilds.proximetry.com/packages/jwt/0.1.8) [![Coverage Status](https://coveralls.io/repos/github/relayr/erl-jwt/badge.svg?branch=master)](https://coveralls.io/github/relayr/erl-jwt?branch=master)

Erlang functions for decoding and validation of JSON Web Tokens (JWT). JWT standard is defined in [RFC 7519](https://tools.ietf.org/html/rfc7519).

## Examples

#### jwt:decode_no_verify/1
Decode JSON Web Token without validating of token's signature and expiration time.
```
1> jwt:decode_no_verify(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsb2dnZWRJbkFzIjoiYWRtaW4iLCJpYXQiOjE0MjI3Nzk2Mzh9.gzSraSYS8EXBxLN_oWnFSRgCzcmJmMjLiuyu5CSpyHI">>).
{ok,[{<<"loggedInAs">>,<<"admin">>},{<<"iat">>,1422779638}]}
2> jwt:decode_no_verify(<<"eyJsb2dnZWRJbkFzIjoiYWRtaW4iLCJpYXQiOjE0MjI3Nzk2Mzh9">>).
{error,bad_token_format}
```

#### jwt:decode/2
Decode JSON Web Token and verify:
 * signature match using provided RSA key
 * token issuance time
 * token expiration time
```
1> jwt:decode(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsb2dnZWRJbkFzIjoiYWRtaW4iLCJpYXQiOjE0MjI3Nzk2Mzh9.gzSraSYS8EXBxLN_oWnFSRgCzcmJmMjLiuyu5CSpyHI">>, <<"MIIBIjANBgkqhki...">>).
{ok,[{<<"loggedInAs">>,<<"admin">>},{<<"iat">>,1422779638}]}
2> jwt:decode(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsb2dnZWRJbkFzIjoiYWRtaW4iLCJpYXQiOjB9.gzSraSYS8EXBxLN_oWnFSRgCzcmJmMjLiuyu5CSpyHI">>, <<"MIeBkNAgjkhBisq...">>).
{error, token_expired}
3> jwt:decode(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsb2dnZWRJbkFzIjoiYWRtaW4iLCJpYXQiOjE0MjI3Nzk2Mzh9.gzSraSYS8EXBxLN_oWnFSRgCzcmJmMjLiuyu5CSpyHI">>, <<"MIeBkNAgjkhBisq...">>).
{error, bad_signature}
4> jwt:decode(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsb2dnZWRJbkFzIjoiYWRtaW4iLCJpYXQiOjE0MjI3Nzk2Mzh9.gzSraSYS8EXBxLN_oWnFSRgCzcmJmMjLiuyu5CSpyHI">>, <<>>).
{error,{asn1,...}}
```
