

%% Type defs

-type merge_actions() :: [{write, any()} | {delete, any()}].

-type merge_strategy() :: same | all_keys | {atom(), atom()}.

-type merge_ret() ::  stop
		    | {ok, any()}
		    | {ok, merge_actions(), any()}
		    | {ok, merge_actions(), merge_strategy(), any()}.

-type log_fun() :: fun((LogType :: log_type(), Message :: string()) -> 'ok').

-type log_type() :: 'error' | 'normal'.

