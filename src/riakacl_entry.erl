%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016-2017 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, entry to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(riakacl_entry).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	put_groups/4,
	put_groups/5,
	put_groups/6,
	remove_groups/4,
	remove_groups/5,
	remove_groups/6,
	update_groups/4,
	update_groups/5,
	find/3,
	find/4,
	get/3,
	get/4,
	get/5,
	put/4,
	put/5
]).

%% DataType API
-export([
	new_dt/0,
	new_dt/1,
	update_dt/3,
	update_groups_dt/3,
	group_rawdt/2,
	find_group_rawdt/2,
	fold_groups_dt/3,
	verified_groupset_dt/2
]).

%% Types
-type entry() :: riakc_map:crdt_map().

-export_type([entry/0]).

%% =============================================================================
%% API
%% =============================================================================

-spec put_groups(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}]) -> riakacl_entry:entry().
put_groups(Pid, Bucket, Key, Groups) ->
	put_groups(Pid, Bucket, Key, Groups, []).

-spec put_groups(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}], [proplists:property()]) -> riakacl_entry:entry().
put_groups(Pid, Bucket, Key, Groups, Opts) ->
	update_groups(Pid, Bucket, Key, fun(Gs) -> put_groups_(Groups, Gs) end, Opts).

%% Be careful using this function, in most of the cases you will prefer to pass
%% to it an entity that has been retrieved using strict quorum (pr = quorum).
-spec put_groups(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}], riakacl_entry:entry(), [proplists:property()]) -> riakacl_entry:entry().
put_groups(Pid, Bucket, Key, Groups, E0, Opts) ->
	Now = riakacl:unix_time_us(),
	E1 = update_groups_dt(fun(Gs) -> put_groups_(Groups, Gs) end, Now, E0),
	put(Pid, Bucket, Key, E1, Opts).

-spec remove_groups(pid(), bucket_and_type(), binary(), [binary()]) -> entry().
remove_groups(Pid, Bucket, Key, Names) ->
	update_groups(Pid, Bucket, Key, fun(Gs) -> remove_groups_(Names, Gs) end).

-spec remove_groups(pid(), bucket_and_type(), binary(), [binary()], [proplists:property()]) -> entry().
remove_groups(Pid, Bucket, Key, Names, Opts) ->
	update_groups(Pid, Bucket, Key, fun(Gs) -> remove_groups_(Names, Gs) end, Opts).

%% Be careful using this function, in most of the cases you will prefer to pass
%% to it an entity that has been retrieved using strict quorum (pr = quorum).
-spec remove_groups(pid(), bucket_and_type(), binary(), [binary()], riakacl_entry:entry(), [proplists:property()]) -> entry().
remove_groups(Pid, Bucket, Key, Names, E0, Opts) ->
	Now = riakacl:unix_time_us(),
	E1 = update_groups_dt(fun(Gs) -> remove_groups_(Names, Gs) end, Now, E0),
	put(Pid, Bucket, Key, E1, Opts).

-spec update_groups(pid(), bucket_and_type(), binary(), fun((riakacl_group:group()) -> riakacl_group:group())) -> entry().
update_groups(Pid, Bucket, Key, Handle) ->
	update_groups(Pid, Bucket, Key, Handle, []).

-spec update_groups(pid(), bucket_and_type(), binary(), fun((riakacl_group:group()) -> riakacl_group:group()), [proplists:property()]) -> entry().
update_groups(Pid, Bucket, Key, Handle, Opts) ->
	Now = riakacl:unix_time_us(),
	E0 = get(Pid, Bucket, Key, [{pr, quorum}], new_dt(Now)),
	E1 = update_groups_dt(Handle, Now, E0),
	put(Pid, Bucket, Key, E1, Opts).

-spec get(pid(), bucket_and_type(), binary()) -> entry().
get(Pid, Bucket, Key) ->
	get(Pid, Bucket, Key, []).

-spec get(pid(), bucket_and_type(), binary(), [proplists:property()]) -> entry().
get(Pid, Bucket, Key, Opts) ->
	case find(Pid, Bucket, Key, Opts) of
		{ok, Val} -> Val;
		_         -> error({bad_key, Bucket, Key})
	end.

-spec get(pid(), bucket_and_type(), binary(), [proplists:property()], entry()) -> entry().
get(Pid, Bucket, Key, Opts, Default) ->
	case find(Pid, Bucket, Key, Opts) of
		{ok, Val} -> Val;
		_         -> Default
	end.

-spec find(pid(), bucket_and_type(), binary()) -> {ok, entry()} | error.
find(Pid, Bucket, Key) ->
	find(Pid, Bucket, Key, []).

-spec find(pid(), bucket_and_type(), binary(), [proplists:property()]) -> {ok, entry()} | error.
find(Pid, Bucket, Key, Opts) ->
	case catch riakc_pb_socket:fetch_type(Pid, Bucket, Key, Opts) of
		{ok, Val}                  -> {ok, Val};
		{error, {notfound, _Type}} -> error;
		{error, Reason}            -> exit(Reason);
		{'EXIT', Reason}           -> exit(Reason);
		Else                       -> exit({bad_return_value, Else})
	end.

-spec put(pid(), bucket_and_type(), binary(), entry()) -> entry().
put(Pid, Bucket, Key, E) ->
	put(Pid, Bucket, Key, E, []).

-spec put(pid(), bucket_and_type(), binary(), entry(), [proplists:property()]) -> entry().
put(Pid, Bucket, Key, E, Opts) ->
	case catch riakc_pb_socket:update_type(Pid, Bucket, Key, riakc_map:to_op(E), [{pw, quorum}|Opts]) of
		ok                  -> E;
		{ok, Emodified}     -> Emodified;
		{error, unmodified} -> ok;
		{error, Reason}     -> exit(Reason);
		{'EXIT', Reason}    -> exit(Reason);
		Else                -> exit({bad_return_value, Else})
	end.

%% =============================================================================
%% DataType API
%% =============================================================================

%% It would be slightly more efficient to use `new_dt/1`,
%% creating many entries by once.
-spec new_dt() -> entry().
new_dt() ->
	new_dt(riakacl:unix_time_us()).

-spec new_dt(non_neg_integer()) -> entry().
new_dt(CreatedAt) ->
	update_dt(
		fun(E) ->
			riakc_map:update({<<"cat">>, register}, fun(Obj) -> riakc_register:set(integer_to_binary(CreatedAt), Obj) end, E)
		end, CreatedAt, riakc_map:new()).

-spec update_dt(fun((riakacl_group:group()) -> riakacl_group:group()), non_neg_integer(), entry()) -> entry().
update_dt(Handle, ModifiedAt, E0) ->
	E1 = Handle(E0),
	riakc_map:update({<<"mat">>, register}, fun(Obj) -> riakc_register:set(integer_to_binary(ModifiedAt), Obj) end, E1).
	
-spec update_groups_dt(fun((riakacl_group:group()) -> riakacl_group:group()), non_neg_integer(), entry()) -> entry().
update_groups_dt(HandleGroups, ModifiedAt, E0) ->
	update_dt(
		fun(E1) ->
			riakc_map:update({<<"groups">>, map}, fun(Gs) -> HandleGroups(cleanup_groups_dt_(Gs, ModifiedAt)) end, E1)
		end, ModifiedAt, E0).

-spec group_rawdt(binary(), entry()) -> [riakacl_group:rawdt()].
group_rawdt(Name, E) ->
	case find_group_rawdt(Name, E) of
		{ok, Raw} -> Raw;
		_         -> error({bad_key, Name})
	end.

-spec find_group_rawdt(binary(), entry()) -> {ok, [riakacl_group:rawdt()]} | error.
find_group_rawdt(Name, E) ->
	try riakc_map:fetch({<<"groups">>, map}, E) of
		Input ->
			case lists:keyfind({Name, map}, 1, Input) of
				{_, Group} -> {ok, Group};
				_          -> error
			end
	catch _:_ -> error end.

-spec fold_groups_dt(fun((binary(), any(), any()) -> any()), any(), entry()) -> any().
fold_groups_dt(HandleGroup, AccIn, E) ->
	try riakc_map:fetch({<<"groups">>, map}, E) of
		Input ->
			lists:foldl(
				fun
					({{Name, map}, Raw}, Acc) -> HandleGroup(Name, Raw, Acc);
					(_Key, Acc)               -> Acc
				end, AccIn, Input)
	catch _:_ -> AccIn end.

-spec verified_groupset_dt(entry(), non_neg_integer()) -> gb_sets:set(binary()).
verified_groupset_dt(E, Time) ->
	fold_groups_dt(
		fun(Name, Raw, Acc) ->
			case riakacl_group:check_rawdt(Raw, Time) of
				true -> gb_sets:add_element(Name, Acc);
				_    -> Acc
			end
		end, gb_sets:new(), E).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec put_groups_([{binary(), riakacl_group:group()}], riakacl_group:group()) -> riakacl_group:group().
put_groups_(Groups, Gs) ->
	lists:foldl(
		fun({Name, Group}, Acc) ->
			riakc_map:update({Name, map}, fun(_) -> Group end, Acc)
		end, Gs, Groups).

-spec remove_groups_([binary()], riakacl_group:group()) -> riakacl_group:group().
remove_groups_(Names, Gs) ->
	lists:foldl(
		fun(Name, Acc) ->
			riakc_map:erase({Name, map}, Acc)
		end, Gs, Names).

-spec cleanup_groups_dt_(riakc_map:crdt_map(), non_neg_integer()) -> riakc_map:crdt_map().
cleanup_groups_dt_(Gs, Time) ->
	riakc_map:fold(
		fun
			({_Name, map} =Key, Raw, Acc) ->
				%% Removing obsolete groups
				case catch riakacl_group:check_rawdt(Raw, Time) of
					true -> Acc;
					_    -> riakc_map:erase(Key, Acc)
				end;
			(Key, _Raw, Acc) ->
				%% Removing invalid groups
				riakc_map:erase(Key, Acc)
		end,
		Gs, Gs).
