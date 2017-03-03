-module(riakacl_cth).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	init_config/0,
	riakc_open/1,
	await/3,
	has_group/4,
	make_bucket/0,
	make_group/0
]).

%% =============================================================================
%% API
%% =============================================================================

-spec init_config() -> list().
init_config() ->
	{ok, Config} = file:consult(root_path(<<".develop-environment">>)),
	SubBucket = {<<"riakacl_subject_t">>, <<"riakacl-object">>},
	ObjBucket = {<<"riakacl_object_t">>, <<"riakacl-object">>},
	[{subject_bucket, SubBucket}, {object_bucket, ObjBucket} | Config].

-spec root_path(binary()) -> binary().
root_path(Path) ->
	Root = list_to_binary(filename:dirname(filename:join([filename:dirname(code:which(?MODULE))]))),
	<<Root/binary, $/, Path/binary>>.

riakc_open(Config) ->
	{_, #{host := Host, port := Port}} = lists:keyfind(kv_protobuf, 1, Config),
	{ok, Pid} = riakc_pb_socket:start_link(Host, Port),
	Pid.

-spec await(pid(), bucket_and_type(), binary()) -> ok.
await(Pid, Bucket, Key) ->
	riakacl_entry:find(Pid, Bucket, Key, [{pr, quorum}]),
	ok.

-spec has_group(pid(), bucket_and_type(), binary(), binary()) -> boolean().
has_group(Pid, Bucket, Key, Group) ->
	gb_sets:is_member(
		Group,
		riakacl_entry:verified_groupset_dt(
			riakacl_entry:get(Pid, Bucket, Key),
			riakacl:unix_time_us())).

-spec make_bucket() -> binary().
make_bucket() ->
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
