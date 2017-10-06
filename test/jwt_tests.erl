%%%-------------------------------------------------------------------
%%% @author sjanota
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2015 12:43
%%%-------------------------------------------------------------------
-module(jwt_tests).
-author("sjanota").

%% API
-export([]).

-define(BASE_TOKEN, [
    {<<"iss">>, <<"me">>},
    {<<"sub">>, <<"sj">>},
    {<<"jti">>, <<"12345">>},
    {<<"typ">>, <<"my_token">>}
]).
-define(PUBLIC_KEY, <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA33TqqLR3eeUmDtHS89qF\n
3p4MP7Wfqt2Zjj3lZjLjjCGDvwr9cJNlNDiuKboODgUiT4ZdPWbOiMAfDcDzlOxA\n
04DDnEFGAf+kDQiNSe2ZtqC7bnIc8+KSG/qOGQIVaay4Ucr6ovDkykO5Hxn7OU7s\n
Jp9TP9H0JH8zMQA6YzijYH9LsupTerrY3U6zyihVEDXXOv08vBHk50BMFJbE9iwF\n
wnxCsU5+UZUZYw87Uu0n4LPFS9BT8tUIvAfnRXIEWCha3KbFWmdZQZlyrFw0buUE\n
f0YN3/Q0auBkdbDR/ES2PbgKTJdkjc/rEeM0TxvOUf7HuUNOhrtAVEN1D5uuxE1W\n
SwIDAQAB">>).

-define(NOW, 123456789).
-define(BEFORE, 123456788).
-define(AFTER, 123456790).

-define(MALFORMED_TOKEN, <<"InVaLiD">>).

-define(VALID_TOKEN, ?BASE_TOKEN ++ [{<<"nbf">>, ?BEFORE}, {<<"exp">>, ?AFTER}, {<<"iat">>, ?BEFORE}]).
-define(VALID_TOKEN_DEC, <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJtZSIsInN1YiI6InNqIiwibmJmIjoxMjM0NTY3ODgsImV4cCI6MTIzNDU2NzkwLCJpYXQiOjEyMzQ1Njc4OCwianRpIjoiMTIzNDUiLCJ0eXAiOiJteV90b2tlbiJ9.BmJ8GbYK1a_a__wofSnoK3sfo0IzL8Mvv6bJQCjHimuxc_jxHKebEAbySCPTj6chHHM20JnMM56gEAezp1Hm0rKnPkhPwuN2aMoKGxWBLt0Py9ATYy8tkFJ7jQK3Jr5zJiTVm2YNfh46XoPGdGFOf-VZwPlp2vu9MpGvV6MOu1NhU9GElY--ADUtaeg1mTN4tcJ1fsTsW0BWqdj4GBLVXU4h8oEEcOd1tFHYmx6aUQ7EIgMBOKQZltoLRZT6TdwMiNJKdgXMXbGPOKM64kYZ0AI2aqVGsPzFtcb4oYMrLtktfu6vLRPry4cy10P9VwcCvVtq1zjrz_FiemAtA6hV1A">>).
-define(NOT_YET_TOKEN_DEC, <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJtZSIsInN1YiI6InNqIiwibmJmIjoxMjM0NTY3OTAsImV4cCI6MTIzNDU2NzkwLCJpYXQiOjEyMzQ1Njc4OCwianRpIjoiMTIzNDUiLCJ0eXAiOiJteV90b2tlbiJ9.Oavd_XM8tUrbHBqgC6wnbJo7g3cSpdC05fzO4WI_N2pzBc7g-z5fVmws8sHHLbML9N80kghZQE8hX5SFdIjUY1Z2_QolbBvk-f1QpuOJ24-mSOYrruHoVnr3c86VYU-Rae0qWF0JhgRAl13iKoNBjf1GBR7aYpPBrFcjkEk8gZ1KA4ivaZ8l_AFGtMNsXLLTwmCjum4yk1ldXr2UfPJSQ274Nm-YSzMEE_beFtk_lVSM9-HYKWxJg4Z6vdjUTuNr4iT1XCWIitmiTzrolpVgcYzF3kvwiyuvoSS7wSL7uPH4852j-zMSQMV8-rNBmKMkkQV_RHYjWmmwx7BKBJdFEA">>).
-define(EXPIRED_TOKEN_DEC, <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJtZSIsInN1YiI6InNqIiwibmJmIjoxMjM0NTY3ODgsImV4cCI6MTIzNDU2Nzg4LCJpYXQiOjEyMzQ1Njc4OCwianRpIjoiMTIzNDUiLCJ0eXAiOiJteV90b2tlbiJ9.B86pig9T-tHBYezRzKH0ql8DD6-9Ue1rz6ocWuHIoz23zjkoMSQ2KcJy8Kg3JcZlru0K0JrwqXvQZh9VTErjNqRQo-4iZbRDsTcc_iwh-94wQPunLK2DlEmMdSSYuwAIdlPl91FMGPWk9RQdy97gKodNWimOkHTWzGBVvOLT-Bhjt7mV14QdsmJOBGKlZbevHY4Tn_xBA9KlbjeSFYK-iB8S7AhnvcezY9RxBFMcG2BfekqeWF9_fdpSOzUi_Q_UigWgcc_dEvx9AcNaN17FqbN-2DN5Yg0WQTuIwhapAxPnZ3p5mwenI2vC-itdZDCtRo6uQWIn__y-NN2IaR40_g">>).
-define(NOT_ISSUED_TOKEN_DEC, <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJtZSIsInN1YiI6InNqIiwibmJmIjoxMjM0NTY3ODgsImV4cCI6MTIzNDU2Nzg4LCJpYXQiOjEyMzQ1Njc5MCwianRpIjoiMTIzNDUiLCJ0eXAiOiJteV90b2tlbiJ9.n3XLmcTlvi4-nHAVzRiYfOTMeG7w7IpVemjuWXmET4L1MLHJMcTi1R0uZo-ADr9p2-dP6f_XoqYMrsH6VV0hFuiqOQTGrP8OWYO4tdtarB7T3yxorjzXXZOzoaU1WXoOsl5l8C_2U5upNiVS8bcXmLW_URwm_lBDEUIvzf8SgjBzToMnHoh3Smr1dq97_ZQj1yZtKKLPXXoZ9HphS31Arz9MIlMSle8xBJHd7NQ6iRbCiKaHF17THULSlRjRdZ2K8xeF9AaLLWFskA5ALznWlxPHwWw9nzRReNCOzCGOZkWDN9NBnBVhE22JoFsi4tA_3BKYOECTlDMGZe0d6MAwDA">>).
-define(INVALID_TOKEN_DEC, <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJtZSIsInN1YiI6InNqIiwibmJmIjoxMjM0NTY3ODgsImV4cCI6MTIzNDU2Nzg4LCJpYXQiOjEyMzQ1Njc5MCwianRpIjoiMTIzNDUiLCJ0eXAiOiJpbnZhbGlkIn0.dP6f_XoqYMrsH6VV0hFuiqOQTGrP8OWYO4tdtarB7T3yxorjzXXZOzoaU1WXoOsl5l8C_2U5upNiVS8bcXmLW_URwm_lBDEUIvzf8SgjBzToMnHoh3Smr1dq97_ZQj1yZtKKLPXXoZ9HphS31Arz9MIlMSle8xBJHd7NQ6iRbCiKaHF17THULSlRjRdZ2K8xeF9AaLLWFskA5ALznWlxPHwWw9nzRReNCOzCGOZkWDN9NBnBVhE22JoFsi4tA_3BKYOECTlDMGZe0d6MAwDA">>).


-ifdef(TEST).

-include_lib("testutils/include/testing.hrl").

?TEST_FUN().
jwt_test_() ->
    {foreach,
        fun() -> ?MECK(jwt, [{system_time, ?NOW}]) end,
        fun(_) -> ok end,
        [
            {"Valid token", fun valid_token/0},
            {"Invalid signature", fun invalid_token/0},
            {"Not before invalid token", fun not_yet_token/0},
            {"Expired token", fun expired_token/0},
            {"Not issued yet token", fun not_issued_token/0},
            {"Malformed token", fun malformed_token/0}
        ]
    }.

valid_token() ->
    {ok, Token} = jwt:decode(?VALID_TOKEN_DEC, ?PUBLIC_KEY),
    ?assertEqual(lists:sort(?VALID_TOKEN), lists:sort(Token)).

not_yet_token() ->
    {error, Reason} = jwt:decode(?NOT_YET_TOKEN_DEC, ?PUBLIC_KEY),
    ?assertEqual(not_valid_yet, Reason).

invalid_token() ->
    {error, Reason} = jwt:decode(?INVALID_TOKEN_DEC, ?PUBLIC_KEY),
    ?assertEqual(bad_signature, Reason).

expired_token() ->
    {error, Reason} = jwt:decode(?EXPIRED_TOKEN_DEC, ?PUBLIC_KEY),
    ?assertEqual(token_expired, Reason).

not_issued_token() ->
    {error, Reason} = jwt:decode(?NOT_ISSUED_TOKEN_DEC, ?PUBLIC_KEY),
    ?assertEqual(not_issued_yet, Reason).

malformed_token() ->
    {error, Reason} = jwt:decode(?MALFORMED_TOKEN, ?PUBLIC_KEY),
    ?assertEqual(bad_token_format, Reason). 

-endif.
