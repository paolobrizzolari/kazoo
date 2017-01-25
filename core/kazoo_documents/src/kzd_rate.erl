%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% Webhook document accessors
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_rate).

-export([surcharge/1, surcharge/2
        ,rate_cost/1, rate_cost/2
        ,private_cost/1, private_cost/2
        ,minimum/1, minimum/2
        ,increment/1, increment/2
        ,no_charge/1, no_charge/2
        ,version/1, version/2
        ,prefix/1, prefix/2
        ,name/1, name/2, set_name/2
        ,ratedeck/1, ratedeck/2
        ,description/1, description/2
        ,weight/1, weight/2
        ,direction/1, direction/2
        ,options/1, options/2
        ,routes/1, routes/2
        ,account_id/1, account_id/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-export_type([doc/0, docs/0]).

-spec minimum(doc()) -> integer().
-spec minimum(doc(), integer()) -> integer().
minimum(Rate) ->
    minimum(Rate, 0).
minimum(Rate, Default) ->
    kz_json:get_integer_value(<<"minimum">>, Rate, Default).

-spec increment(doc()) -> integer().
-spec increment(doc(), integer()) -> integer().
increment(Rate) ->
    increment(Rate, 0).
increment(Rate, Default) ->
    kz_json:get_integer_value(<<"rate_increment">>, Rate, Default).

-spec no_charge(doc()) -> integer().
-spec no_charge(doc(), integer()) -> integer().
no_charge(Rate) ->
    no_charge(Rate, 0).
no_charge(Rate, Default) ->
    kz_json:get_integer_value(<<"rate_nocharge_time">>, Rate, Default).

-spec surcharge(doc()) -> kz_transaction:units().
-spec surcharge(doc(), float()) -> kz_transaction:units().
surcharge(Rate) ->
    surcharge(Rate, 0.0).
surcharge(Rate, Default) ->
    Surcharge = kz_json:get_float_value(<<"rate_surcharge">>, Rate, Default),
    wht_util:dollars_to_units(Surcharge).

-spec rate_cost(doc()) -> kz_transaction:units().
-spec rate_cost(doc(), float()) -> kz_transaction:units().
rate_cost(Rate) ->
    rate_cost(Rate, 0.0).
rate_cost(Rate, Default) ->
    Cost = kz_json:get_float_value(<<"rate_cost">>, Rate, Default),
    wht_util:dollars_to_units(Cost).

-spec private_cost(doc()) -> kz_transaction:units().
-spec private_cost(doc(), float()) -> kz_transaction:units().
private_cost(Rate) ->
    private_cost(Rate, 0.0).
private_cost(Rate, Default) ->
    Cost = kz_json:get_float_value(<<"pvt_internal_rate_cost">>, Rate, Default),
    wht_util:dollars_to_units(Cost).

-spec version(doc()) -> api_ne_binary().
-spec version(doc(), Default) -> ne_binary() | Default.
version(Rate) ->
    version(Rate, 'undefined').
version(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"rate_version">>, Rate, Default).

-spec prefix(doc()) -> api_ne_binary().
-spec prefix(doc(), Default) -> ne_binary() | Default.
prefix(Rate) ->
    prefix(Rate, 'undefined').
prefix(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"prefix">>, Rate, Default).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Rate) ->
    name(Rate, 'undefined').
name(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"rate_name">>, Rate, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Rate, Name) ->
    kz_json:set_value(<<"rate_name">>, Name, Rate).

-spec ratedeck(doc()) -> api_ne_binary().
-spec ratedeck(doc(), Default) -> ne_binary() | Default.
ratedeck(Rate) ->
    ratedeck(Rate, 'undefined').
ratedeck(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"ratedeck_name">>, Rate, Default).

-spec description(doc()) -> api_ne_binary().
-spec description(doc(), Default) -> ne_binary() | Default.
description(Rate) ->
    description(Rate, 'undefined').
description(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"description">>, Rate, Default).

-spec weight(doc()) -> pos_integer().
-spec weight(doc(), pos_integer()) -> pos_integer().
weight(Rate) ->
    weight(Rate, 1).
weight(Rate, Default) ->
    kz_json:get_integer_value(<<"weight">>, Rate, Default).

-spec direction(doc()) -> ne_binaries().
-spec direction(doc(), ne_binaries()) -> ne_binaries().
-define(BOTH_DIRECTIONS, [<<"inbound">>, <<"outbound">>]).

direction(Rate) ->
    direction(Rate, ?BOTH_DIRECTIONS).
direction(Rate, Default) ->
    kz_json:get_list_value(<<"direction">>, Rate, Default).

-spec options(doc()) -> ne_binaries().
-spec options(doc(), ne_binaries()) -> ne_binaries().
options(Rate) ->
    options(Rate, []).
options(Rate, Default) ->
    kz_json:get_list_value(<<"options">>, Rate, Default).

-spec routes(doc()) -> ne_binaries().
-spec routes(doc(), ne_binaries()) -> ne_binaries().
routes(Rate) ->
    routes(Rate, []).
routes(Rate, Default) ->
    kz_json:get_list_value(<<"routes">>, Rate, Default).

-spec account_id(doc()) -> api_ne_binary().
-spec account_id(doc(), Default) -> ne_binary() | Default.
account_id(Rate) ->
    account_id(Rate, 'undefined').
account_id(Rate, Default) ->
    kz_json:get_ne_binary_value(<<"account_id">>, Rate, Default).
