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

-module(riakacl_rwaccess).
-behaviour(riakacl).

%% API
-export([
	new_dt/0,
	new_dt/1,
	new_dt/2,
	access/1,
	parse_access/1
]).

%% ACL callbacks
-export([
	parse_rawdt/1,
	max_access/2,
	no_access/0
]).

%% Types
-type access() :: #{read => boolean(), write => boolean()}.

%% =============================================================================
%% API
%% =============================================================================

-spec new_dt() -> riakacl_group:group().
new_dt() ->
	new_dt(no_access(), #{}).

-spec new_dt(access()) -> riakacl_group:group().
new_dt(Access) ->
	new_dt(Access, #{}).

-spec new_dt(access(), riakacl_group:claims()) -> riakacl_group:group().
new_dt(Access, Claims) ->
	riakacl_group:new_dt(
		Claims,
		fun(Data) ->
			riakc_map:update({<<"access">>, register}, fun(Obj) -> riakc_register:set(access(Access), Obj) end, Data)
		end).

-spec access(access()) -> binary().
access(Val) ->
	R = case maps:get(read, Val, false) of true -> <<$r>>; _ -> <<$->> end,
	W = case maps:get(write, Val, false) of true -> <<$w>>; _ -> <<$->> end,
	<<R/binary, W/binary>>.

-spec parse_access(binary()) -> access().
parse_access(<<"r-">>) -> #{read => true, write => false};
parse_access(<<"-w">>) -> #{read => false, write => true};
parse_access(<<"rw">>) -> #{read => true, write => true};
parse_access(<<"--">>) -> #{read => false, write => false}.

%% =============================================================================
%% ACL callbacks
%% =============================================================================

-spec parse_rawdt([riakacl_group:rawdt()]) -> any().
parse_rawdt(Raw) ->
	{_, Val} = lists:keyfind({<<"access">>, register}, 1, Raw),
	parse_access(Val).

-spec max_access(access(), access()) -> access().
max_access(#{read := R1, write := W1}, #{read := R2, write := W2}) ->
	#{read => max(R1, R2),
		write => max(W1, W2)}.

-spec no_access() -> access().
no_access() ->
	#{read => false, write => false}.
