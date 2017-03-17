-module(riakacl_cth).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	init_config/0,
	riakc_open/1,
	has_group/4,
	has_verified_group/4,
	make_key/0,
	make_group/0
]).

%% =============================================================================
%% API
%% =============================================================================

-spec init_config() -> list().
init_config() ->
	Env =
		case os:getenv("DEVELOP_ENVIRONMENT") of
			false -> error(missing_develop_environment);
			Val   -> {ok, S, _} = erl_scan:string(Val), {ok, Term} = erl_parse:parse_term(S), Term
		end,
	Config = maps:fold(fun(Key, Val, Acc) -> [{Key, Val}|Acc] end, [], Env),
	SubBucket = {<<"riakacl_subject_t">>, <<"riakacl-object">>},
	ObjBucket = {<<"riakacl_object_t">>, <<"riakacl-object">>},
	[{subject_bucket, SubBucket}, {object_bucket, ObjBucket} | Config].

riakc_open(Config) ->
	{_, #{host := Host, port := Port}} = lists:keyfind(kv_protobuf, 1, Config),
	{ok, Pid} = riakc_pb_socket:start_link(Host, Port),
	Pid.

-spec has_group(pid(), bucket_and_type(), binary(), binary()) -> boolean().
has_group(Pid, Bucket, Key, Name) ->
	case riakacl_entry:find_group_dt(Name, riakacl_entry:get(Pid, Bucket, Key)) of
		{ok, _Group} -> true;
		_            -> false
	end.

-spec has_verified_group(pid(), bucket_and_type(), binary(), binary()) -> boolean().
has_verified_group(Pid, Bucket, Key, Name) ->
	case riakacl_entry:find_group_dt(Name, riakacl_entry:get(Pid, Bucket, Key)) of
		{ok, Group} -> riakacl_group:check_rawdt(Group);
		_           -> false
	end.

-spec make_key() -> binary().
make_key() ->
	list_to_binary(vector(128, alphanum_chars())).

-spec make_group() -> binary().
make_group() ->
	list_to_binary(vector(128, alphanum_chars())).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec oneof(list()) -> integer().
oneof(L) ->
	lists:nth(rand:uniform(length(L)), L).

-spec vector(non_neg_integer(), list()) -> list().
vector(MaxSize, L) ->
	vector(0, MaxSize, L, []).

-spec vector(non_neg_integer(), non_neg_integer(), list(), list()) -> list().
vector(Size, MaxSize, L, Acc) when Size < MaxSize ->
	vector(Size +1, MaxSize, L, [oneof(L)|Acc]);
vector(_, _, _, Acc) ->
	Acc.

-spec alphanum_chars() -> list().
alphanum_chars() ->
	"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
