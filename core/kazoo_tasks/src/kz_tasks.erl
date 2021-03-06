%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% Utilities for tasks validation & stuff.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_tasks).

-export([all/0
        ,all/1
        ,read/1
        ,new/7
        ]).

-export([mandatory/1
        ,optional/1
        ,input_mime/1
        ]).

-export([from_json/1
        ,to_json/1
        ,to_public_json/1
        ,task_by_id/1
        ]).

-export([is_processing/1
        ]).

-include("kazoo_tasks.hrl").

-define(TASK_ID_SIZE, 15).
-define(A_TASK_ID, kz_binary:rand_hex(?TASK_ID_SIZE)).
-type task_id() :: <<_:(8*2*?TASK_ID_SIZE)>>.

-type task() :: #{worker_pid => api_pid()
                 ,worker_node => api_ne_binary()
                 ,account_id => ne_binary()
                 ,auth_account_id => ne_binary()
                 ,id => task_id()
                 ,category => ne_binary()
                 ,action => ne_binary()
                 ,file_name => api_ne_binary()
                 ,created => gregorian_seconds() %% Time of task creation (PUT)
                 ,started => api_seconds() %% Time of task start (PATCH)
                 ,finished => api_seconds() %% Time of task finish (> started)
                 ,total_rows => api_pos_integer() %% CSV rows (undefined for a noinput task)
                 ,total_rows_failed => api_non_neg_integer() %% Rows that crashed or didn't return ok
                 ,total_rows_succeeded => api_non_neg_integer() %% Rows that returned 'ok'
                 }.

-type input() :: api_ne_binary() | kz_json:objects().

-type help_error() :: {'error', 'unknown_category_action'}.

-export_type([task_id/0
             ,input/0
             ,help_error/0
             ]).

-define(API_MANDATORY, <<"mandatory">>).
-define(API_OPTIONAL, <<"optional">>).
-define(API_INPUT_MIME, <<"expected_content">>).


%%%===================================================================
%%% API
%%%===================================================================

-spec mandatory(kz_json:object()) -> ne_binaries().
mandatory(APIJObj) ->
    kz_json:get_list_value(?API_MANDATORY, APIJObj, []).

-spec optional(kz_json:object()) -> ne_binaries().
optional(APIJObj) ->
    kz_json:get_list_value(?API_OPTIONAL, APIJObj, []).

-spec input_mime(kz_json:object()) -> ne_binary().
input_mime(APIJObj) ->
    kz_json:get_ne_binary_value(?API_INPUT_MIME, APIJObj, ?NIL_MIME).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec all() -> kz_json:objects().
all() ->
    view([]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec all(ne_binary()) -> kz_json:objects().
all(AccountId=?NE_BINARY) ->
    view([{'startkey', [AccountId]}
         ,{'endkey', [AccountId, kz_json:new()]}
         ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read(task_id()) -> {'ok', kz_json:object()} |
                         {'error', 'not_found'}.
read(TaskId=?NE_BINARY) ->
    case task_by_id(TaskId) of
        [Task] -> {'ok', to_public_json(Task)};
        [] -> {'error', 'not_found'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Verify a task previous to its creation in DB.
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary(), ne_binary()
         ,ne_binary(), ne_binary(), api_pos_integer(), input(), api_binary()) ->
                 {'ok', kz_json:object()} |
                 help_error() |
                 {'error', kz_json:object()}.
new(?MATCH_ACCOUNT_RAW(AuthAccountId), ?MATCH_ACCOUNT_RAW(AccountId)
   ,Category=?NE_BINARY, Action=?NE_BINARY, TotalRows, Input, CSVName)
  when is_integer(TotalRows), TotalRows > 0;
       TotalRows =:= 'undefined', Input =:= 'undefined' ->
    case help(Category, Action) of
        {'error', _R}=E ->
            lager:debug("checking task ~s ~s failed: ~p", [Category, Action, _R]),
            E;
        API ->
            lager:debug("task ~s ~s matched api ~s", [Category, Action, kz_json:encode(API)]),
            case find_input_errors(API, Input) of
                Errors when Errors =/= #{} ->
                    JObj = kz_json:from_list(props:filter_empty(maps:to_list(Errors))),
                    {'error', JObj};
                _ ->
                    InputName = case kz_term:is_empty(CSVName) of
                                    'true' -> 'undefined';
                                    'false' -> kz_term:to_binary(CSVName)
                                end,
                    lager:debug("creating ~s.~s task (~p)", [Category, Action, TotalRows]),
                    lager:debug("using auth ~s and account ~s", [AuthAccountId, AccountId]),
                    TaskId = ?A_TASK_ID,
                    Task = #{worker_pid => 'undefined'
                            ,worker_node => 'undefined'
                            ,account_id => AccountId
                            ,auth_account_id => AuthAccountId
                            ,id => TaskId
                            ,category => Category
                            ,action => Action
                            ,file_name => InputName
                            ,created => kz_time:current_tstamp()
                            ,started => 'undefined'
                            ,finished => 'undefined'
                            ,total_rows => TotalRows
                            ,total_rows_failed => 'undefined'
                            ,total_rows_succeeded => 'undefined'
                            },
                    {'ok', _JObj} = Ok = save_new_task(Task),
                    lager:debug("task ~s created, rows: ~p", [TaskId, TotalRows]),
                    Ok
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec help(ne_binary(), ne_binary()) ->
                  kz_json:object() |
                  {'error', 'unknown_category_action'}.
help(Category, Action) ->
    Req = props:filter_undefined(
            [{<<"Category">>, Category}
            ,{<<"Action">>, Action}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case kz_amqp_worker:call(Req
                            ,fun kapi_tasks:publish_lookup_req/1
                            ,fun kapi_tasks:lookup_resp_v/1
                            )
    of
        {'ok', JObj} ->
            Help = kz_json:get_value([<<"Help">>, Category, Action], JObj),
            case kz_term:is_empty(Help) of
                false -> Help;
                true -> {error, unknown_category_action}
            end;
        {'timeout', _Resp} -> {error, unknown_category_action};
        {'error', _E} -> {error, unknown_category_action}
    end.


-spec find_input_errors(kz_json:object(), input()) -> map().
find_input_errors(API, 'undefined') ->
    find_API_errors(API, [], 'false');

find_input_errors(API, Input=?NE_BINARY) ->
    {Fields, InputData} = kz_csv:take_row(Input),
    Errors = find_API_errors(API, Fields, 'true'),
    %% Stop here if there is no Mandatory fields to check against.
    case mandatory(API) of
        [] -> Errors;
        Mandatory ->
            IsMandatory = [lists:member(Field, Mandatory) || Field <- Fields],
            Unsets =
                fun (Row, Es) ->
                        case are_mandatories_unset(IsMandatory, Row) of
                            'false' -> Es;
                            'true' -> [iolist_to_binary(kz_csv:row_to_iolist(Row)) | Es]
                        end
                end,
            case kz_csv:fold(InputData, Unsets, []) of
                [] -> Errors;
                MMVs -> Errors#{?KZ_TASKS_INPUT_ERROR_MMV => lists:reverse(MMVs)}
            end
    end;

find_input_errors(API, InputRecord=[_|_]) ->
    %%NOTE: assumes first record has all the fields that all the other records will ever need set,
    %%NOTE: assumes all records have all the same fields defined.
    Fields = kz_json:get_keys(hd(InputRecord)),
    find_API_errors(API, Fields, 'true').

-spec find_API_errors(kz_json:object(), ne_binaries(), boolean()) -> map().
find_API_errors(API, Fields, HasInputData) ->
    Mandatory = mandatory(API),
    Routines =
        [fun (Errors) ->
                 case Mandatory -- Fields of
                     [] -> Errors;
                     Missing -> Errors#{?KZ_TASKS_INPUT_ERROR_MMF => Missing}
                 end
         end
        ,fun (Errors) ->
                 RequestedMIME = input_mime(API),
                 APIRequiresInputData = ?NIL_MIME /= RequestedMIME,
                 case APIRequiresInputData xor HasInputData of
                     'false' -> Errors;
                     'true' ->  Errors#{?KZ_TASKS_INPUT_ERROR_MIME => RequestedMIME}
                 end
         end
        ],
    lists:foldl(fun (F, Errors) -> F(Errors) end, #{}, Routines).

-spec are_mandatories_unset(nonempty_list(boolean()), nonempty_list(ne_binary())) -> boolean().
are_mandatories_unset(IsMandatory, Row) ->
    MapF = fun (Mandatory, Value) ->
                   Mandatory
                       andalso 'undefined' == Value
           end,
    RedF = fun erlang:'or'/2,
    lists:foldl(RedF, 'false', lists:zipwith(MapF, IsMandatory, Row)).


-spec view(list()) -> kz_json:objects().
view(ViewOptions) ->
    case kz_datamgr:get_results(?KZ_TASKS_DB, ?KZ_TASKS_BY_ACCOUNT, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} ->
            Found = [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
            lists:sort(fun compare_tasks/2, Found);
        {'error', _R} ->
            lager:debug("error viewing tasks (~p): ~p", [ViewOptions, _R]),
            []
    end.

-spec compare_tasks(kz_json:object(), kz_json:object()) -> boolean().
compare_tasks(JObjA, JObjB) ->
    kz_doc:created(JObjA) =< kz_doc:created(JObjB).

-spec save_new_task(task()) -> {'ok', kz_json:object()} |
                               {'error', any()}.
save_new_task(Task = #{id := _TaskId}) ->
    case kz_datamgr:save_doc(?KZ_TASKS_DB, to_json(Task)) of
        {'ok', Doc} -> {'ok', to_public_json(from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to save ~s in ~s: ~p", [_TaskId, ?KZ_TASKS_DB, _R]),
            E
    end.

-spec task_by_id(task_id()) -> [task()].
task_by_id(TaskId) ->
    case kz_datamgr:open_cache_doc(?KZ_TASKS_DB, TaskId) of
        {'ok', JObj} -> [from_json(JObj)];
        {'error', _R} ->
            lager:error("failed to open ~s in ~s: ~p", [TaskId, ?KZ_TASKS_DB, _R]),
            []
    end.


-spec from_json(kz_json:object()) -> task().
from_json(Doc) ->
    #{worker_pid => 'undefined'
     ,worker_node => kzd_task:node(Doc)
     ,account_id => kzd_task:account_id(Doc)
     ,auth_account_id => kzd_task:auth_account_id(Doc)
     ,id => kzd_task:id(Doc)
     ,category => kzd_task:category(Doc)
     ,action => kzd_task:action(Doc)
     ,file_name => kzd_task:file_name(Doc)
     ,created => kz_doc:created(Doc)
     ,started => kzd_task:start_timestamp(Doc)
     ,finished => kzd_task:end_timestamp(Doc)
     ,total_rows => kzd_task:total_count(Doc)
     ,total_rows_failed => kzd_task:failure_count(Doc)
     ,total_rows_succeeded => kzd_task:success_count(Doc)
     }.

-spec to_json(task()) -> kz_json:object().
to_json(#{id := TaskId
         ,worker_node := Node
         ,account_id := AccountId
         ,auth_account_id := AuthAccountId
         ,category := Category
         ,action := Action
         ,file_name := InputName
         ,created := Created
         ,started := Started
         ,finished := Finished
         ,total_rows := TotalRows
         ,total_rows_failed := TotalFailed
         ,total_rows_succeeded := TotalSucceeded
         } = Task) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"_id">>, TaskId}
        ,{?PVT_TYPE, kzd_task:type()}
        ,{?PVT_WORKER_NODE, Node}
        ,{?PVT_ACCOUNT_ID, AccountId}
        ,{?PVT_AUTH_ACCOUNT_ID, AuthAccountId}
        ,{?PVT_CATEGORY, Category}
        ,{?PVT_ACTION, Action}
        ,{?PVT_FILENAME, InputName}
        ,{?PVT_CREATED, Created}
        ,{?PVT_MODIFIED, kz_time:current_tstamp()}
        ,{?PVT_STARTED_AT, Started}
        ,{?PVT_FINISHED_AT, Finished}
        ,{?PVT_TOTAL_ROWS, TotalRows}
        ,{?PVT_TOTAL_ROWS_FAILED, TotalFailed}
        ,{?PVT_TOTAL_ROWS_SUCCEEDED, TotalSucceeded}
        ,{?PVT_STATUS, status(Task)}
        ])).

-spec to_public_json(task()) -> kz_json:object().
to_public_json(Task) ->
    Doc = to_json(Task),
    JObj =
        kz_json:from_list(
          props:filter_undefined(
            [{<<"id">>, kzd_task:id(Doc)}
            ,{<<"node">>, kzd_task:node(Doc)}
            ,{<<"account_id">>, kzd_task:account_id(Doc)}
            ,{<<"auth_account_id">>, kzd_task:auth_account_id(Doc)}
            ,{<<"category">>, kzd_task:category(Doc)}
            ,{<<"action">>, kzd_task:action(Doc)}
            ,{<<"file_name">>, kzd_task:file_name(Doc)}
            ,{<<"created">>, kz_doc:created(Doc)}
            ,{<<"start_timestamp">>, kzd_task:start_timestamp(Doc)}
            ,{<<"end_timestamp">>, kzd_task:end_timestamp(Doc)}
            ,{<<"total_count">>, kzd_task:total_count(Doc)}
            ,{<<"failure_count">>, kzd_task:failure_count(Doc)}
            ,{<<"success_count">>, kzd_task:success_count(Doc)}
            ,{<<"status">>, kzd_task:status(Doc)}
            ])),
    kz_json:set_value(<<"_read_only">>, JObj, kz_json:new()).


%%--------------------------------------------------------------------
%% @public
%% @doc Whether task has been started and is still running.
%% @end
%%--------------------------------------------------------------------
-spec is_processing(task()) -> boolean().
is_processing(#{started := Started
               ,finished := Finished
               })
  when Started  /= 'undefined',
       Finished == 'undefined' ->
    'true';
is_processing(_Task) ->
    'false'.

%% @private
-spec status(task()) -> api_binary().
status(#{started := 'undefined'}) ->
    ?STATUS_PENDING;
status(#{started := Started
        ,finished := 'undefined'
        })
  when Started /= 'undefined' ->
    ?STATUS_EXECUTING;

%% For tasks with CSV input
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := 0
        ,total_rows_succeeded := TotalRows
        })
  when Finished /= 'undefined',
       is_integer(TotalRows), TotalRows > 0 ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := TotalRows
        ,total_rows_succeeded := 0
        })
  when Finished /= 'undefined',
       is_integer(TotalRows), TotalRows > 0 ->
    ?STATUS_FAILURE;
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished /= 'undefined',
       is_integer(TotalRows), is_integer(TotalFailed), is_integer(TotalSucceeded),
       TotalRows > TotalFailed, TotalRows > TotalSucceeded ->
    ?STATUS_PARTIAL;

%% For noinput tasks
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := 0
        ,total_rows_succeeded := 0
        })
  when Finished /= 'undefined' ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := 0
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished /= 'undefined',
       is_integer(TotalSucceeded), TotalSucceeded > 0 ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := 0
        })
  when Finished /= 'undefined',
       is_integer(TotalFailed), TotalFailed > 0 ->
    ?STATUS_FAILURE;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished /= 'undefined',
       is_integer(TotalFailed), is_integer(TotalSucceeded) ->
    ?STATUS_PARTIAL;

status(_Task) ->
    lager:error("impossible task ~p", [_Task]),
    %% Probably due to worker killed (due to e.g. OOM).
    ?STATUS_BAD.
