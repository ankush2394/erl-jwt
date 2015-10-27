-module(jwt).
-export([decode/2]).

-define(BEGIN_KEY, "-----BEGIN PUBLIC KEY-----\n").
-define(END_KEY, "\n-----END PUBLIC KEY-----").

-include_lib("logger/include/logger.hrl").

-type token() :: {H :: binary(), P :: binary(), S :: binary(), SignIn :: binary()}.


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
-spec decode(binary(), binary()) -> {ok, jsx:json_term()} | {error, any()}.
decode(Token, RSAKey) when is_binary(Token) and is_binary(RSAKey) ->
    try
        {ok, Key} = parse_public_key(RSAKey),
        {ok, TokenParts} = parse_token(Token),
        ok = verify_signature(TokenParts, Key),
        {ok, Decoded} = decode_token(TokenParts),
        ok = verify_token(Decoded),
        {ok, Decoded}
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end.

%%%-------------------------------------------------------------------
%%% Local
%%%-------------------------------------------------------------------
-spec parse_public_key(binary()) -> {ok, public_key:public_key()}.
parse_public_key(BinKey) ->
    FullKey = <<?BEGIN_KEY, BinKey/binary, ?END_KEY>>,
    [Entry] = public_key:pem_decode(FullKey),
    {ok, public_key:pem_entry_decode(Entry)}.

-spec parse_token(binary()) -> {ok, token()}.
parse_token(Token) ->
    Parts = binary:split(Token, <<".">>, [global]),
    DecodedParts = [base64url:decode(Part) || Part <- Parts],
    [EncHeader, EncPayload, _EncSignature] = Parts,
    [Header, Payload, Signature] = DecodedParts,
    SignIn = <<EncHeader/binary, ".", EncPayload/binary>>,
    {ok, {Header, Payload, Signature, SignIn}}.

-spec verify_signature(token(), public_key:public_key()) -> ok | {error, bad_signature}.
verify_signature({_, _, Signature, SignIn}, Key) ->
    case public_key:verify(SignIn, sha256, Signature, Key) of
        true ->
            ok;
        false ->
            {error, bad_signature}
    end.

-spec decode_token(token()) -> {ok, jsx:json_term()}.
decode_token({_, Payload, _, _}) ->
    {ok, jsx:decode(Payload)}.

-spec verify_token(jsx:json_term()) -> ok.
verify_token(Payload) ->
    Now = utils:get_unix_timestamp(),
    ok = verify_issued_at(Payload, Now),
    ok = verify_expiration_date(Payload, Now),
    ok = verify_not_before(Payload, Now).

-spec verify_expiration_date(jsx:json_term(), integer()) -> ok.
verify_expiration_date(Payload, Now) ->
    {_, Exp} = lists:keyfind(<<"exp">>, 1, Payload),
    if
        Exp > Now -> ok;
        true -> {error, token_expired}
    end.

-spec verify_issued_at(jsx:json_term(), integer()) -> ok.
verify_issued_at(Payload, Now) ->
    {_, Iat} = lists:keyfind(<<"iat">>, 1, Payload),
    if
        Iat < Now -> ok;
        true -> {error, not_issued_yet}
    end.

-spec verify_not_before(jsx:json_term(), integer()) -> ok.
verify_not_before(Payload, Now) ->
    case lists:keyfind(<<"nbf">>, 1, Payload) of
        false -> ok;
        {_, Nbf} when Nbf < Now -> ok;
        _ -> {error, not_valid_yet}
    end.

