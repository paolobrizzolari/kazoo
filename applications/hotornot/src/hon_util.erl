%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_util).

-export([candidate_rates/1
        ,matching_rates/2
        ,sort_rates/1
        ]).

-ifdef(TEST).
-export([build_keys/1]).
-endif.

-include("hotornot.hrl").

-define(MIN_PREFIX_LEN, 1). % how many chars to strip off the e164 DID

-spec candidate_rates(ne_binary()) ->
                             {'ok', kz_json:objects()} |
                             {'error', atom()}.
candidate_rates(ToDID) ->
    E164 = knm_converters:normalize(ToDID),
    find_candidate_rates(E164).

-spec find_candidate_rates(ne_binary()) ->
                                  {'ok', kz_json:objects()} |
                                  {'error', atom()}.
find_candidate_rates(E164)
  when byte_size(E164) > ?MIN_PREFIX_LEN ->
    case hotornot_config:use_trie() of
        'false' -> fetch_candidate_rates(E164);
        'true' -> find_trie_rates(E164)
    end;
find_candidate_rates(DID) ->
    lager:debug("DID ~s is too short", [DID]),
    {'error', 'did_too_short'}.

-spec find_trie_rates(api_binary()) ->
                             {'ok', kz_json:objects()} |
                             {'error', atom()}.
find_trie_rates(E164) ->
    case hon_trie:match_did(only_numeric(E164)) of
        {'ok', Result} -> {'ok', Result};
        {'error', _E} ->
            lager:warning("got error while searching did in trie, falling back to DB search"),
            fetch_candidate_rates(E164)
    end.

-spec fetch_candidate_rates(ne_binary()) ->
                                   {'ok', kzd_rate:docs()} |
                                   {'error', 'did_too_short'} |
                                   kz_datamgr:data_error().
-spec fetch_candidate_rates(ne_binary(), ne_binaries()) ->
                                   {'ok', kzd_rate:docs()} |
                                   {'error', 'did_too_short'} |
                                   kz_datamgr:data_error().
fetch_candidate_rates(E164) ->
    fetch_candidate_rates(E164, build_keys(E164)).

fetch_candidate_rates(_E164, []) ->
    {'error', 'did_too_short'};
fetch_candidate_rates(E164, Keys) ->
    lager:debug("searching for prefixes for ~s: ~p", [E164, Keys]),
    case kz_datamgr:get_results(?KZ_RATES_DB
                               ,<<"rates/lookup">>
                               ,[{'keys', Keys}
                                ,'include_docs'
                                ]
                               )
    of
        {'ok', []}=OK -> OK;
        {'error', _}=E -> E;
        {'ok', ViewRows} ->
            {'ok'
            ,[kz_json:get_value(<<"doc">>, ViewRow)
              || ViewRow <- ViewRows
             ]
            }
    end.

-spec build_keys(ne_binary()) -> [integer()].
build_keys(Number) ->
    case only_numeric(Number) of
        <<>> -> [];
        <<D:1/binary, Rest/binary>> ->
            build_keys(Rest, D, [kz_term:to_integer(D)])
    end.

-spec only_numeric(binary()) -> binary().
only_numeric(Number) ->
    << <<N>> || <<N>> <= Number, is_numeric(N)>>.

-spec is_numeric(integer()) -> boolean().
is_numeric(N) ->
    N >= $0
        andalso N =< $9.

-spec build_keys(binary(), ne_binary(), [integer()]) -> [integer()].
build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [kz_term:to_integer(<<Prefix/binary, D/binary>>) | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

-spec matching_rates(kzd_rate:docs(), kapi_rate:req()) ->
                            kzd_rate:docs().
matching_rates(Rates, RateReq) ->
    FilterList = hotornot_config:filter_list(),
    lists:foldl(fun(Filter, Acc) ->
                        lists:filter(fun(Rate) -> matching_rate(Rate, Filter, RateReq) end, Acc)
                end
               ,Rates
               ,FilterList
               ).

-spec sort_rates(kzd_rate:docs()) -> kzd_rate:docs().
sort_rates(Rates) ->
    case hotornot_config:should_sort_by_weight() of
        'true' -> lists:usort(fun sort_rate_by_weight/2, Rates);
        'false' -> lists:usort(fun sort_rate_by_cost/2, Rates)
    end.

%% Private helper functions

-spec matching_rate(kzd_rate:doc(), ne_binary(), kapi_rate:req()) -> boolean().
matching_rate(Rate, <<"direction">>, RateReq) ->
    case kz_json:get_value(<<"Direction">>, RateReq) of
        'undefined' -> 'true';
        Direction ->
            lists:member(Direction, kzd_rate:direction(Rate))
    end;

matching_rate(Rate, <<"route_options">>, RateReq) ->
    RouteOptions = kz_json:get_value(<<"Options">>, RateReq, []),
    RouteFlags   = kz_json:get_value(<<"Outbound-Flags">>, RateReq, []),
    ResourceFlag = case kz_json:get_value(<<"Account-ID">>, RateReq) of
                       'undefined' -> [];
                       AccountId -> maybe_add_resource_flag(RateReq, AccountId)
                   end,
    options_match(kzd_rate:options(Rate), RouteOptions++RouteFlags++ResourceFlag);

matching_rate(Rate, <<"routes">>, RateReq) ->
    E164 = knm_converters:normalize(kz_json:get_value(<<"To-DID">>, RateReq)),
    lists:any(fun(Regex) -> re:run(E164, Regex) =/= 'nomatch' end
             ,kzd_rate:routes(Rate, [])
             );

matching_rate(Rate, <<"ratedeck_name">>, RateReq) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, RateReq),
    AccountRatedeck = kz_service_ratedeck_name:get_ratedeck_name(AccountId),
    RatedeckName = kzd_rate:ratedeck(Rate),
    AccountRatedeck =:= RatedeckName;

matching_rate(Rate, <<"reseller">>, RateReq) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, RateReq),
    ResellerId = kz_services:find_reseller_id(AccountId),
    RateAccountId = kzd_rate:account_id(Rate),
    RateAccountId =:= ResellerId;

matching_rate(Rate, <<"version">>, _RateReq) ->
    kzd_rate:version(Rate) =:= hotornot_config:rate_version();

matching_rate(_Rate, _FilterType, _RateReq) -> 'false'.

%% Return true if RateA has lower weight than RateB
-spec sort_rate_by_weight(kzd_rate:doc(), kzd_rate:doc()) -> boolean().
sort_rate_by_weight(RateA, RateB) ->
    PrefixA = byte_size(kzd_rate:prefix(RateA)),
    PrefixB = byte_size(kzd_rate:prefix(RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            kzd_rate:weight(RateA, 100) < kzd_rate:weight(RateB, 100);
        'false' ->
            PrefixA > PrefixB
    end.

-spec sort_rate_by_cost(kzd_rate:doc(), kzd_rate:doc()) -> boolean().
sort_rate_by_cost(RateA, RateB) ->
    PrefixA = byte_size(kzd_rate:prefix(RateA)),
    PrefixB = byte_size(kzd_rate:prefix(RateB)),

    case PrefixA =:= PrefixB of
        'true' ->
            kzd_rate:rate_cost(RateA, 0.0) > kzd_rate:rate_cost(RateB, 0.0);
        'false' ->
            PrefixA > PrefixB
    end.

%% Route options come from the client device
%% Rate options come from the carrier providing the trunk
%% All Route options must exist in a carrier's options to keep the carrier
%% in the list of carriers capable of handling the call
-spec options_match(trunking_options(), trunking_options()) -> boolean().
options_match([], []) -> 'true';
options_match([], _) -> 'true';
options_match(RateOptions, RouteOptions) ->
    lists:all(fun(RouteOption) ->
                      props:get_value(RouteOption, RateOptions, 'false') =/= 'false'
              end
             ,RouteOptions
             ).

-spec maybe_add_resource_flag(kapi_rate:req(), ne_binary()) -> kz_proplist().
maybe_add_resource_flag(RateReq, AccountId) ->
    case hotornot_config:should_account_filter_by_resource(AccountId) of
        'true' ->
            case kz_json:get_ne_binary_value(<<"Resource-ID">>, RateReq) of
                'undefined' -> [];
                ResourceId -> [ResourceId]
            end;
        'false' -> []
    end.
