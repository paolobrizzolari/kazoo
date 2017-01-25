-module(hotornot_config).

-export([default_minimum/0
        ,default_increment/0
        ,default_nocharge/0
        ,default_surcharge/0
        ,default_cost/0
        ,default_internal_cost/0
        ,default_filter_list/0
        ]).

-include("hotornot.hrl").

-define(DEFAULT_MINIMUM, kapps_config:get_integer(?APP_NAME, <<"default_rate_minimum">>, 60)).
-define(DEFAULT_INCREMENT, kapps_config:get_integer(?APP_NAME, <<"default_rate_increment">>, 60)).
-define(DEFAULT_NOCHARGE, kapps_config:get_integer(?APP_NAME, <<"default_rate_nocharge_time">>, 0)).
-define(DEFAULT_SURCHARGE, kapps_config:get_float(?APP_NAME, <<"default_rate_surcharge">>, 0.0)).
-define(DEFAULT_COST, kapps_config:get_float(?APP_NAME, <<"default_rate_cost">>, 0.0)).
-define(DEFAULT_INT_COST, kapps_config:get_float(?APP_NAME, <<"default_rate_internal_cost">>, 0.0)).

-define(DEFAULT_FILTER_LIST, [<<"direction">>
                             ,<<"route_options">>
                             ,<<"routes">>
                             ]).

-spec default_minimum() -> pos_integer().
default_minimum() -> ?DEFAULT_MINIMUM.

-spec default_increment() -> pos_integer().
default_increment() -> ?DEFAULT_INCREMENT.

-spec default_nocharge() -> non_neg_integer().
default_nocharge() -> ?DEFAULT_NOCHARGE.

-spec default_surcharge() -> float().
default_surcharge() -> ?DEFAULT_SURCHARGE.

-spec default_cost() -> float().
default_cost() -> ?DEFAULT_COST.

-spec default_internal_cost() -> float().
default_internal_cost() -> ?DEFAULT_INT_COST.

-spec default_filter_list() -> ne_binaries().
default_filter_list() -> ?DEFAULT_FILTER_LIST.
