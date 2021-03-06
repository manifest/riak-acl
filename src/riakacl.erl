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
	authorize/6,
	authorize/7,
	authorize/8,
	authorize_predefined_object/5,
	authorize_predefined_object/6,
	authorize_predefined_object/7,
	authorize_predefined_object/8,
	authorize_predefined_object/9,
	authorize_predefined_subject/5,
	authorize_predefined_subject/6,
	authorize_predefined_subject/7,
	authorize_predefined_subject/8,
	authorize_predefined_subject/9,
	put_subject_groups/4,
	remove_subject_groups/4,
	put_object_acl/4,
	remove_object_acl/4,
	unix_time_us/0,
	unix_time_us/1
]).

%% Callbacks
-callback parse_rawdt(RawData) -> Access
	when
		RawData :: [riakacl_group:rawdt()],
		Access  :: any().

-callback max_access(Access, Access) -> Access
	when
		Access :: any().

-callback no_access() -> Access
	when
		Access :: any().

%% =============================================================================
%% API
%% =============================================================================

-spec authorize(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), module()) -> {ok | error, any()}.
authorize(Pid, Sb, Skey, Ob, Okey, Mod) ->
	authorize(Pid, Sb, Skey, Ob, Okey, [], Mod).

-spec authorize(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), [binary()], module()) -> {ok | error, any()}.
authorize(Pid, Sb, Skey, Ob, Okey, DerivedSubjectGroups, Mod) ->
	authorize(Pid, Sb, Skey, Ob, Okey, DerivedSubjectGroups, Mod, unix_time_us()).

-spec authorize(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), [binary()], module(), non_neg_integer()) -> {ok | error, any()}.
authorize(Pid, Sb, Skey, Ob, Okey, DerivedSubjectGroups, Mod, Time) ->
	authorize_(
		riakacl_entry:find(Pid, Sb, Skey),
		riakacl_entry:find(Pid, Ob, Okey),
		DerivedSubjectGroups,
		Mod,
		Time).

-spec authorize_predefined_object(pid(), bucket_and_type(), binary(), [{binary(), any()}], module()) -> {ok | error, any()}.
authorize_predefined_object(Pid, Sb, Skey, PredefinedObjectGroups, Mod) ->
	authorize_predefined_object(Pid, Sb, Skey, PredefinedObjectGroups, Mod, unix_time_us()).

-spec authorize_predefined_object(pid(), bucket_and_type(), binary(), [{binary(), any()}], module(), non_neg_integer()) -> {ok | error, any()}.
authorize_predefined_object(Pid, Sb, Skey, PredefinedObjectGroups, Mod, Time) ->
	authorize_predefined_object_(
		riakacl_entry:find(Pid, Sb, Skey),
		PredefinedObjectGroups,
		Mod,
		Time).

-spec authorize_predefined_object(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), [{binary(), any()}], module()) -> {ok | error, any()}.
authorize_predefined_object(Pid, Sb, Skey, Ob, Okey, PredefinedObjectGroups, Mod) ->
	authorize_predefined_object(Pid, Sb, Skey, Ob, Okey, PredefinedObjectGroups, [], Mod).

-spec authorize_predefined_object(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), [{binary(), any()}], [binary()], module()) -> {ok | error, any()}.
authorize_predefined_object(Pid, Sb, Skey, Ob, Okey, PredefinedObjectGroups, DerivedSubjectGroups, Mod) ->
	authorize_predefined_object(Pid, Sb, Skey, Ob, Okey, PredefinedObjectGroups, DerivedSubjectGroups, Mod, unix_time_us()).

%% If a predefined object's group is matched to subject's group,
%% the result with object's access data will be immediately returned.
%% No future requests to RiakKV will be performed, for perfomance reasons.
%% So that, predefined ACL cannot be merged with object's ACL stored in RiakKV.
%% Derived subject's groups are processed similar as general subject's groups.
-spec authorize_predefined_object(pid(), bucket_and_type(), binary(), bucket_and_type(), binary(), [{binary(), any()}], [binary()], module(), non_neg_integer()) -> {ok | error, any()}.
authorize_predefined_object(Pid, Sb, Skey, Ob, Okey, PredefinedObjectGroups, DerivedSubjectGroups, Mod, Time) ->
	NoAccess = Mod:no_access(),
	case authorize_predefined_object(Pid, Sb, Skey, PredefinedObjectGroups, Mod, Time) of
		{ok, NoAccess} -> authorize(Pid, Sb, Skey, Ob, Okey, DerivedSubjectGroups, Mod, Time);
		MaybeAcl       -> MaybeAcl
	end.

-spec authorize_predefined_subject(pid(), [binary()], bucket_and_type(), binary(), module()) -> {ok | error, any()}.
authorize_predefined_subject(Pid, PredefinedSubjectGroups, Ob, Okey, Mod) ->
	authorize_predefined_subject(Pid, PredefinedSubjectGroups, Ob, Okey, Mod, unix_time_us()).

-spec authorize_predefined_subject(pid(), [binary()], bucket_and_type(), binary(), module(), non_neg_integer()) -> {ok | error, any()}.
authorize_predefined_subject(Pid, PredefinedSubjectGroups, Ob, Okey, Mod, Time) ->
	authorize_predefined_subject_(
		PredefinedSubjectGroups,
		riakacl_entry:find(Pid, Ob, Okey),
		Mod,
		Time).

-spec authorize_predefined_subject(pid(), bucket_and_type(), binary(), [binary()], bucket_and_type(), binary(), module()) -> {ok | error, any()}.
authorize_predefined_subject(Pid, Sb, Skey, PredefinedSubjectGroups, Ob, Okey, Mod) ->
	authorize_predefined_subject(Pid, Sb, Skey, PredefinedSubjectGroups, Ob, Okey, [], Mod, unix_time_us()).

-spec authorize_predefined_subject(pid(), bucket_and_type(), binary(), [binary()], bucket_and_type(), binary(), [binary()], module()) -> {ok | error, any()}.
authorize_predefined_subject(Pid, Sb, Skey, PredefinedSubjectGroups, Ob, Okey, DerivedSubjectGroups, Mod) ->
	authorize_predefined_subject(Pid, Sb, Skey, PredefinedSubjectGroups, Ob, Okey, DerivedSubjectGroups, Mod, unix_time_us()).

%% If a predefined subject's group is matched to object's group,
%% the result with object's access data will be immediately returned.
%% No future requests to RiakKV will be performed, for perfomance reasons.
%% So that, predefined ACL cannot be merged with object's ACL stored in RiakKV.
%% Derived subject's groups are processed similar as general subject's groups.
-spec authorize_predefined_subject(pid(), bucket_and_type(), binary(), [binary()], bucket_and_type(), binary(), [binary()], module(), non_neg_integer()) -> {ok | error, any()}.
authorize_predefined_subject(Pid, Sb, Skey, PredefinedSubjectGroups, Ob, Okey, DerivedSubjectGroups, Mod, Time) ->
	NoAccess = Mod:no_access(),
	case authorize_predefined_subject(Pid, PredefinedSubjectGroups, Ob, Okey, Mod, Time) of
		{ok, NoAccess} -> authorize(Pid, Sb, Skey, Ob, Okey, DerivedSubjectGroups, Mod, Time);
		MaybeAcl       -> MaybeAcl
	end.

-spec put_subject_groups(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}]) -> riakacl_entry:entry().
put_subject_groups(Pid, Bucket, Key, Groups) ->
	riakacl_entry:put_groups(Pid, Bucket, Key, Groups).

-spec remove_subject_groups(pid(), bucket_and_type(), binary(), [binary()]) -> riakacl_entry:entry().
remove_subject_groups(Pid, Bucket, Key, Names) ->
	riakacl_entry:remove_groups(Pid, Bucket, Key, Names).

-spec put_object_acl(pid(), bucket_and_type(), binary(), [{binary(), riakacl_group:group()}]) -> riakacl_entry:entry().
put_object_acl(Pid, Bucket, Key, Groups) ->
	riakacl_entry:put_groups(Pid, Bucket, Key, Groups).

-spec remove_object_acl(pid(), bucket_and_type(), binary(), [binary()]) -> riakacl_entry:entry().
remove_object_acl(Pid, Bucket, Key, Names) ->
	riakacl_entry:remove_groups(Pid, Bucket, Key, Names).

-spec unix_time_us() -> non_neg_integer().
unix_time_us() ->
	unix_time_us(erlang:timestamp()).

-spec unix_time_us(erlang:timestamp()) -> non_neg_integer().
unix_time_us({MS, S, US}) ->
	MS * 1000000000000 + S * 1000000 + US.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec authorize_(MaybeEntry, MaybeEntry, [binary()], module(), non_neg_integer()) -> {ok | error, any()} when MaybeEntry :: {ok, riakacl_entry:entry()} | error.
authorize_({ok, S}, {ok, O}, DerivedSubjectGroups, Mod, Time) ->
	Names =
		gb_sets:intersection(
			gb_sets:union(riakacl_entry:verified_groupset_dt(S, Time), gb_sets:from_list(DerivedSubjectGroups)),
			riakacl_entry:verified_groupset_dt(O, Time)),
	case gb_sets:is_empty(Names) of
		true -> {ok, Mod:no_access()};
		_    -> {ok, max_access_rawdt_(O, Names, Mod)}
	end;
authorize_(error, _MaybeO, _DerivedSubjectGroups, _Mod, _Time) ->
	{error, bad_aclsubject_key};
authorize_(_MaybeS, error, _DerivedSubjectGroups, _Mod, _Time) ->
	{error, bad_aclobject_key}.

-spec authorize_predefined_object_(MaybeEntry, [{binary(), any()}], module(), non_neg_integer()) -> {ok | error, any()} when MaybeEntry :: {ok, riakacl_entry:entry()} | error.
authorize_predefined_object_({ok, S}, PredefinedObjectGroups, Mod, Time) ->
	Names =
		gb_sets:intersection(
			riakacl_entry:verified_groupset_dt(S, Time),
			lists:foldl(fun({Name, _Group}, Acc) -> gb_sets:add_element(Name, Acc) end, gb_sets:new(), PredefinedObjectGroups)),
	case gb_sets:is_empty(Names) of
		true -> {ok, Mod:no_access()};
		_    -> {ok, max_access_(PredefinedObjectGroups, Names, Mod)}
	end;
authorize_predefined_object_(_MaybeS, _PredefinedObjectGroups, _Mod, _Time) ->
	{error, bad_aclsubject_key}.

-spec authorize_predefined_subject_([binary()], MaybeEntry, module(), non_neg_integer()) -> {ok | error, any()} when MaybeEntry :: {ok, riakacl_entry:entry()} | error.
authorize_predefined_subject_(PredefinedSubjectGroups, {ok, O}, Mod, Time) ->
	Names =
		gb_sets:intersection(
			gb_sets:from_list(PredefinedSubjectGroups),
			riakacl_entry:verified_groupset_dt(O, Time)),
	case gb_sets:is_empty(Names) of
		true -> {ok, Mod:no_access()};
		_    -> {ok, max_access_rawdt_(O, Names, Mod)}
	end;
authorize_predefined_subject_(_PredefinedSubjectGroups, _MaybeO, _Mod, _Time) ->
	{error, bad_aclobject_key}.

-spec max_access_([{binary(), any()}], gb_sets:set(binary()), module()) -> any().
max_access_(Groups, Names, Mod) ->
	lists:foldl(
		fun({Name, Access}, Max) ->
			case gb_sets:is_element(Name, Names) of
				true -> try Mod:max_access(Access, Max) catch _:_ -> Max end;
				_    -> Max
			end
		end, Mod:no_access(), Groups).

-spec max_access_rawdt_(riakacl_entry:entry(), gb_sets:set(binary()), module()) -> any().
max_access_rawdt_(E, Names, Mod) ->
	riakacl_entry:fold_groups_dt(
		fun(Name, Raw, Max) ->
			case gb_sets:is_element(Name, Names) of
				true -> try Mod:max_access(Mod:parse_rawdt(riakacl_group:data_rawdt(Raw)), Max) catch _:_ -> Max end;
				_    -> Max
			end
		end, Mod:no_access(), E).
