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

-module(riakacl_group).

%% DataType API
-export([
	new_dt/0,
	new_dt/1,
	new_dt/2,
	data_rawdt/1,
	check_rawdt/2,
	parse_rawdt/2
]).

%% Types
-type claims()              :: map().
-type data()                :: riakc_map:crdt_map().
-type group()               :: riakc_map:crdt_map().
-type rawdt()               :: {{binary(), riakc_datatype:datatype()}, any()}.
-type parse_rawdt_handler() :: fun(([riakacl_group:rawdt()], map()) -> map()).

-export_type([claims/0, data/0, group/0, rawdt/0, parse_rawdt_handler/0]).

%% =============================================================================
%% DataType API
%% =============================================================================

%% It would be slightly more efficient to pass `iat` claim explicitly,
%% creating many groups by once.
-spec new_dt() -> group().
new_dt() ->
	new_dt_(#{}).

-spec new_dt(claims()) -> group().
new_dt(Claims) ->
	new_dt_(Claims).

-spec new_dt(claims(), fun((data()) -> data())) -> group().
new_dt(Claims, HandleData) ->
	riakc_map:update({<<"data">>, map}, HandleData, new_dt_(Claims)).

-spec data_rawdt([rawdt()]) -> [rawdt()].
data_rawdt(Raw) ->
	case lists:keyfind({<<"data">>, map}, 1, Raw) of
		{_, Data} -> Data;
		_         -> []
	end.

-spec check_rawdt([rawdt()], non_neg_integer()) -> boolean().
check_rawdt([{{<<"exp">>, register}, Val}|T], Time) ->
	case binary_to_integer(Val) > Time of
		true -> check_rawdt(T, Time);
		_    -> false
	end;
check_rawdt([_|T], Time) -> check_rawdt(T, Time);
check_rawdt([], _Time)   -> true.

-spec parse_rawdt([riakacl_group:rawdt()], riakacl_group:parse_rawdt_handler()) -> map().
parse_rawdt(Raw, HandleData) ->
	parse_rawdt(Raw, HandleData, #{}).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec new_dt_(claims()) -> group().
new_dt_(Claims0) ->
	Claims1 =
		Claims0#{
			iat => maps:get(iat, Claims0, riakacl:unix_time_us())},

	new_dt_(maps:to_list(Claims1), riakc_map:new()).

-spec new_dt_([{atom(), any()}], group()) -> group().
new_dt_([{exp, Val}|T], G) -> new_dt_(T, riakc_map:update({<<"exp">>, register}, fun(Obj) -> riakc_register:set(integer_to_binary(Val), Obj) end, G));
new_dt_([{iat, Val}|T], G) -> new_dt_(T, riakc_map:update({<<"iat">>, register}, fun(Obj) -> riakc_register:set(integer_to_binary(Val), Obj) end, G));
new_dt_([], G)             -> G.

-spec parse_rawdt([riakacl_group:rawdt()], riakacl_group:parse_rawdt_handler(), map()) -> map().
parse_rawdt([{{<<"data">>, map}, L}|T], HandleData, Acc)       -> parse_rawdt(T, HandleData, HandleData(L, Acc));
parse_rawdt([{{<<"iat">>, register}, Val}|T], HandleData, Acc) -> parse_rawdt(T, HandleData, Acc#{iat => Val});
parse_rawdt([{{<<"exp">>, register}, Val}|T], HandleData, Acc) -> parse_rawdt(T, HandleData, Acc#{exp => Val});
parse_rawdt([_|T], HandleData, Acc)                            -> parse_rawdt(T, HandleData, Acc);
parse_rawdt([], _HandleData, Acc)                              -> Acc.
