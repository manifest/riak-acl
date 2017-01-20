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
	update_groups/4,
	find/3,
	find/4,
	get/3,
	get/4,
	get/5,
	put/4
]).

%% DataType API
-export([
	new_dt/1,
	update_dt/3,
	fold_groups_dt/3,
	verified_groupset_dt/2
]).

%% Types
-type entry() :: riakc_map:crdt_map().

-export_type([entry/0]).

%% =============================================================================
%% API
%% =============================================================================

-spec update_groups(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}]) -> entry().
update_groups(Pid, Bucket, Key, Groups) ->
	Now = riakacl:unix_time_us(),
	E0 = get(Pid, Bucket, Key, [{pr, quorum}], new_dt(Now)),
	E1 =
		update_groups_dt(
			fun(Gs) ->
				lists:foldl(
					fun({Name, Group}, Acc) ->
						riakc_map:update({Name, map}, fun(_) -> Group end, Acc)
					end, Gs, Groups)
			end, Now, E0),
	put(Pid, Bucket, Key, E1),
	E1.

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

-spec put(pid(), bucket_and_type(), binary(), entry()) -> ok.
put(Pid, Bucket, Key, E) ->
	case catch riakc_pb_socket:update_type(Pid, Bucket, Key, riakc_map:to_op(E), [{pw, quorum}]) of
		ok                  -> ok;
		{error, unmodified} -> ok;
		{error, Reason}     -> exit(Reason);
		{'EXIT', Reason}    -> exit(Reason);
		Else                -> exit({bad_return_value, Else})
	end.

%% =============================================================================
%% DataType API
%% =============================================================================

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
