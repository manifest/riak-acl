%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
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

-module(riakacl).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	authorize/7,
	authorize/6,
	update_subject_groups/4,
	update_object_acl/4,
	unix_time_us/0,
	unix_time_us/1
]).

%% Callbacks
-callback acl(RawDataList) -> ACL
	when
		RawDataList :: [[riakacl_group:rawdt()]],
		ACL         :: any().	

%% =============================================================================
%% API
%% =============================================================================

-spec authorize(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), module()) -> {ok, any()} | error.
authorize(Pid, Sb, Skey, Ob, Okey, Mod) ->
	authorize(Pid, Sb, Skey, Ob, Okey, Mod, unix_time_us()).

-spec authorize(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), module(), non_neg_integer()) -> {ok, any()} | error.
authorize(Pid, Sb, Skey, Ob, Okey, Mod, Time) ->
	authorize_(
		riakacl_entry:find(Pid, Sb, Skey),
		riakacl_entry:find(Pid, Ob, Okey),
		Mod,
		Time).

-spec update_subject_groups(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}]) -> riakacl_entry:entry().
update_subject_groups(Pid, Bucket, Key, Groups) ->
	riakacl_entry:update_groups(Pid, Bucket, Key, Groups).

-spec update_object_acl(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}]) -> riakacl_entry:entry().
update_object_acl(Pid, Bucket, Key, Groups) ->
	riakacl_entry:update_groups(Pid, Bucket, Key, Groups).

-spec unix_time_us() -> non_neg_integer().
unix_time_us() ->
	unix_time_us(erlang:timestamp()).

-spec unix_time_us(erlang:timestamp()) -> non_neg_integer().
unix_time_us({MS, S, US}) ->
	MS * 1000000000000 + S * 1000000 + US.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec authorize_(MaybeEntry, MaybeEntry, module(), non_neg_integer()) -> {ok, any()} | error when MaybeEntry :: {ok, riakacl_entry:entry()} | error.
authorize_({ok, S}, {ok, O}, Mod, Time) ->
	Groups =
		gb_sets:intersection(
			riakacl_entry:verified_groupset_dt(S, Time),
			riakacl_entry:verified_groupset_dt(O, Time)),
	case gb_sets:is_empty(Groups) of
		true -> error;
		_    -> {ok, Mod:acl(data_rawdt_(O, Groups))}
	end;
authorize_(_MaybeS, _MaybeO, _Mod, _Time) ->
	error.

-spec data_rawdt_(riakacl_entry:entry(), gb_sets:set(binary())) -> [[riakacl_group:rawdt()]].
data_rawdt_(E, Groups) ->
	riakacl_entry:fold_groups_dt(
		fun(Name, Raw, Acc) ->
			case gb_sets:is_element(Name, Groups) of
				true -> [riakacl_group:data_rawdt(Raw)|Acc];
				_    -> Acc
			end
		end, [], E).
