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

-module(main_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% =============================================================================
%% Common Test callbacks
%% =============================================================================

all() ->
	application:ensure_all_started(riakacl),
	[{group, main}].

groups() ->
	[{main, [parallel], ct_helper:all(?MODULE)}].

init_per_suite(Config) ->
	riakacl_cth:init_config() ++ Config.

%% =============================================================================
%% Tests
%% =============================================================================

subject_group_roundtrip(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt()}]),
	true = riakacl_cth:has_group(Pid, SubBucket, SubKey, Group),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [Group]),
	false = riakacl_cth:has_group(Pid, SubBucket, SubKey, Group),
	true.

object_group_roundtrip(Config) ->
	ObjBucket = ?config(object_bucket, Config),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt()}]),
	true = riakacl_cth:has_group(Pid, ObjBucket, ObjKey, Group),
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	false = riakacl_cth:has_group(Pid, ObjBucket, ObjKey, Group),
	true.

permission_1group(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt(#{read => true})}]),
	{ok, #{read := true}} = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess),
	
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [Group]),
	true.

permission_2group_merge(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	GroupReader = riakacl_cth:make_group(),
	GroupWriter = riakacl_cth:make_group(),
	GroupNobody = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(
		Pid, SubBucket, SubKey,
		[	{GroupReader, riakacl_group:new_dt()},
			{GroupWriter, riakacl_group:new_dt()},
			{GroupNobody, riakacl_group:new_dt()} ]),
	riakacl:put_object_acl(
		Pid, ObjBucket, ObjKey,
		[	{GroupReader, riakacl_rwaccess:new_dt(#{read => true, write => false})},
			{GroupWriter, riakacl_rwaccess:new_dt(#{read => false, write => true})},
			{GroupNobody, riakacl_rwaccess:new_dt(#{read => false, write => false})} ]),
	{ok, #{read := true, write := true}} = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess),
	
	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [GroupReader, GroupWriter, GroupNobody]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [GroupReader, GroupWriter, GroupNobody]),
	true.

permission_2group_best(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	GroupReader = riakacl_cth:make_group(),
	GroupWriter = riakacl_cth:make_group(),
	GroupNobody = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(
		Pid, SubBucket, SubKey,
		[	{GroupReader, riakacl_group:new_dt()},
			{GroupWriter, riakacl_group:new_dt()},
			{GroupNobody, riakacl_group:new_dt()} ]),
	riakacl:put_object_acl(
		Pid, ObjBucket, ObjKey,
		[	{GroupReader, riakacl_rwaccess:new_dt(#{read => true, write => false})},
			{GroupWriter, riakacl_rwaccess:new_dt(#{read => true, write => true})},
			{GroupNobody, riakacl_rwaccess:new_dt(#{read => false, write => false})} ]),
	{ok, #{read := true, write := true}} = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess),
	
	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [GroupReader, GroupWriter, GroupNobody]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [GroupReader, GroupWriter, GroupNobody]),
	true.

permission_2group_brokenformat(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	GroupReader = riakacl_cth:make_group(),
	GroupWriter = riakacl_cth:make_group(),
	GroupNobody = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(
		Pid, SubBucket, SubKey,
		[	{GroupReader, riakacl_group:new_dt()},
			{GroupWriter, riakacl_group:new_dt()},
			{GroupNobody, riakacl_group:new_dt()} ]),
	riakacl:put_object_acl(
		Pid, ObjBucket, ObjKey,
		[	{GroupReader, riakacl_rwaccess:new_dt(#{read => true, write => false})},
			{GroupWriter, riakacl_rwaccess:new_dt(#{read => true, write => true})},
			{GroupNobody, riakacl_rwaccess:new_dt(#{})} ]),
	{ok, #{read := true, write := true}} = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess),
	
	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [GroupReader, GroupWriter, GroupNobody]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [GroupReader, GroupWriter, GroupNobody]),
	true.

permission_denied_subject_0group(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, []),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt()}]),
	error = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess),

	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	true.

permission_denied_object_0group(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, []),
	error = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess),

	%% cleaning up
	riakacl:remove_subject_groups(Pid, ObjBucket, ObjKey, [Group]),
	true.

permission_expired_subject(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	Time = riakacl:unix_time_us(),
	After = Time +1,
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt(#{exp => Time})}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt()}]),
	error = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess, After),

	%% cleaning up
	riakacl:remove_subject_groups(Pid, ObjBucket, ObjKey, [Group]),
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	true.

permission_expired_object(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	Time = riakacl:unix_time_us(),
	After = Time +1,
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt(#{read => true}, #{exp => Time})}]),
	error = riakacl:authorize(Pid, SubBucket, SubKey, ObjBucket, ObjKey, riakacl_rwaccess, After),

	%% cleaning up
	riakacl:remove_subject_groups(Pid, ObjBucket, ObjKey, [Group]),
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	true.

predefined_object(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	PredefinedObjectGroups = [{Group, #{read => true, write => false}}],
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt()}]),
	{ok, #{read := true}} = riakacl:authorize_predefined_object(Pid, SubBucket, SubKey, PredefinedObjectGroups, riakacl_rwaccess),
	
	%% cleaning up
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [Group]),
	true.

predefined_object_0group_then_object(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	PredefinedObjectGroups = [],
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt(#{read => true})}]),
	{ok, #{read := true}} = riakacl:authorize_predefined_object(Pid, SubBucket, SubKey, ObjBucket, ObjKey, PredefinedObjectGroups, riakacl_rwaccess),

	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [Group]),
	true.

predefined_object_1group_then_object(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	GroupReader = riakacl_cth:make_group(),
	GroupNobody = riakacl_cth:make_group(),
	PredefinedObjectGroups = [{GroupNobody, #{read => false, write => false}}],
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{GroupReader, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{GroupReader, riakacl_rwaccess:new_dt(#{read => true})}]),
	{ok, #{read := true}} = riakacl:authorize_predefined_object(Pid, SubBucket, SubKey, ObjBucket, ObjKey, PredefinedObjectGroups, riakacl_rwaccess),

	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [GroupReader]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [GroupReader]),
	true.

predefined_object_1group_brokenformat_then_object(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	GroupReader = riakacl_cth:make_group(),
	GroupNobody = riakacl_cth:make_group(),
	PredefinedObjectGroups = [{GroupNobody, #{}}],
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{GroupReader, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{GroupReader, riakacl_rwaccess:new_dt(#{read => true})}]),
	{ok, #{read := true}} = riakacl:authorize_predefined_object(Pid, SubBucket, SubKey, ObjBucket, ObjKey, PredefinedObjectGroups, riakacl_rwaccess),

	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [GroupReader]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [GroupReader]),
	true.

predefined_subject(Config) ->
	ObjBucket = ?config(object_bucket, Config),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	PredefinedSubjectGroups = [Group],
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt(#{read => true})}]),
	{ok, #{read := true}} = riakacl:authorize_predefined_subject(Pid, PredefinedSubjectGroups, ObjBucket, ObjKey, riakacl_rwaccess),
	
	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	true.

predefined_subject_0group_then_subject(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	Group = riakacl_cth:make_group(),
	PredefinedSubjectGroups = [],
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{Group, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{Group, riakacl_rwaccess:new_dt(#{read => true})}]),
	{ok, #{read := true}} = riakacl:authorize_predefined_subject(Pid, SubBucket, SubKey, PredefinedSubjectGroups, ObjBucket, ObjKey, riakacl_rwaccess),

	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [Group]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [Group]),
	true.

predefined_subject_1group_then_subject(Config) ->
	SubBucket = ?config(subject_bucket, Config),
	ObjBucket = ?config(object_bucket, Config),
	SubKey = riakacl_cth:make_bucket(),
	ObjKey = riakacl_cth:make_bucket(),
	GroupReader = riakacl_cth:make_group(),
	GroupNobody = riakacl_cth:make_group(),
	PredefinedSubjectGroups = [GroupNobody],
	Pid = riakacl_cth:riakc_open(Config),
	riakacl:put_subject_groups(Pid, SubBucket, SubKey, [{GroupReader, riakacl_group:new_dt()}]),
	riakacl:put_object_acl(Pid, ObjBucket, ObjKey, [{GroupReader, riakacl_rwaccess:new_dt(#{read => true})}]),
	{ok, #{read := true}} = riakacl:authorize_predefined_subject(Pid, SubBucket, SubKey, PredefinedSubjectGroups, ObjBucket, ObjKey, riakacl_rwaccess),

	%% cleaning up
	riakacl:remove_object_acl(Pid, ObjBucket, ObjKey, [GroupReader]),
	riakacl:remove_subject_groups(Pid, SubBucket, SubKey, [GroupReader]),
	true.
