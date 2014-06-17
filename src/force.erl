% =====================================================================================
% CMSaaS - Content Management System as a Service
%
% @copyright 2011 CMSaaS dev team
% @version 0.1
% All rights reserved.
%
% CC-Attribution License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% =====================================================================================

-module(force).

-export([get_type/1, to_list/1, to_list/2, to_atom/1, to_atom/2, to_binary/1, to_binary/2, to_integer/1, to_integer/2, to_bitstring/1, to_bitstring/2, to_float/1, to_float/2, to_pid/1, to_pid/2, to_tuple/1, to_tuple/2]).

force(Item, New_type) ->
	force(Item, New_type, [])
.

force(Item, New_type, Options) when is_list(Item)-> pass(Item, 'list', New_type, Options);
force(Item, New_type, Options) when is_atom(Item)-> 
	case New_type of
		'list' -> 
			pass(Item, 'atom', New_type, Options)
		;
		'binary' -> 
			New_option=case Options of 
				[] -> [utf8];
				X -> X
			end,
			pass(Item, 'atom', New_type, New_option)
		;
		_ ->
			pass(pass(Item, 'atom', 'list', []), 'list', New_type, Options)
	end
;
force(Item, New_type, Options) when is_binary(Item)-> 
	case New_type of
		'atom' ->
			New_option=case Options of 
				[] -> [utf8];
				X -> X
			end,
			pass(Item, 'binary', New_type, New_option)
		;
		'list' ->
			pass(Item, 'binary', New_type, Options)
		;
		_ ->
			pass(pass(Item, 'binary', 'list', Options), 'list', New_type, [])
	end
;
force(Item, New_type, Options) when is_bitstring(Item)-> 
	case New_type of
		'list' ->
			pass(Item, 'bitstring', New_type, Options)
		;
		_ ->
			pass(pass(Item, 'bitstring', 'list', []), 'list', New_type, Options)
	end
;
force(Item, New_type, Options) when is_boolean(Item)-> 
	case New_type of
		'integer' -> 
			pass(Item, 'boolean', New_type, Options)
		;
		'float' ->
			pass(Item, 'boolean', New_type, Options)
		;
		_ ->
			pass(Item, 'atom', New_type, Options)
	end
;
force(Item, New_type, Options) when is_float(Item)-> 
	case New_type of
		'list' ->
			pass(Item, 'float', New_type, Options)
		;
		_ ->
			pass(pass(Item, 'float', 'list', []), 'list', New_type, Options)
	end
;
force(Item, New_type, Options) when is_integer(Item)-> 
	case New_type of
		'list' ->
			pass(Item, 'integer', New_type, Options)
		;
		_ ->
			pass(pass(Item, 'integer', 'list', []), 'list', New_type, [])
	end
;
force(Item, New_type, Options) when is_pid(Item)-> 
	pass(pass(Item, 'pid', 'list', []), 'list', New_type, Options)
;
force(Item, New_type, Options) when is_port(Item)-> 
	pass(pass(Item, 'port', 'list', []), 'list', New_type, Options)
;
force(Item, New_type, Options) when is_reference(Item)-> 
	pass(pass(Item, 'ref', 'list', []), 'list', New_type, Options)
;
force(Item, New_type, Options) when is_tuple(Item)-> 
	pass(pass(Item, 'tuple', 'list', []), 'list', New_type, Options)
;
force(_Item, New_type, Options) -> 
	pass(pass(undefined, 'atom', 'list', []), 'list', New_type, Options)
.

pass(Item, Old_type, New_type, _Options)  when Old_type==New_type-> Item;
pass(Item, Old_type, New_type, Options) ->
	apply(erlang, list_to_atom(atom_to_list(Old_type)++"_to_"++atom_to_list(New_type)), lists:merge([Item], Options))
.


get_type(Item) when is_list(Item)-> 'list';
get_type(Item) when is_atom(Item)-> 'atom';
get_type(Item) when is_binary(Item)-> 'binary';
get_type(Item) when is_bitstring(Item)-> 'bitstring';
get_type(Item) when is_boolean(Item)-> 'atom';
get_type(Item) when is_float(Item)-> 'float';
get_type(Item) when is_integer(Item)-> 'integer';
get_type(Item) when is_pid(Item)-> 'pid';
get_type(Item) when is_port(Item)-> 'port';
get_type(Item) when is_reference(Item)-> 'ref';
get_type(Item) when is_tuple(Item)-> 'tuple';
get_type(_Item) -> undefined.


to_list(Item) when is_list(Item) -> Item;
to_list(Item)  -> force(Item, 'list').

to_list(Item, _Options) when is_list(Item) -> Item;
to_list(Item, Options) -> force(Item, 'list', Options).

to_atom(Item) when is_atom(Item) -> Item;
to_atom(Item) -> force(Item, 'atom').

to_atom(Item, _Options)  when is_atom(Item) -> Item;
to_atom(Item, Options) -> force(Item, 'atom', Options).

to_binary(Item) when is_binary(Item) -> Item;
to_binary(Item) -> force(Item, 'binary').

to_binary(Item, _Options) when is_binary(Item) -> Item;
to_binary(Item, Options) -> force(Item, 'binary', Options).

to_integer(Item) when is_integer(Item) -> Item;
to_integer(Item) -> force(Item, 'integer').

to_integer(Item, _Options) when is_integer(Item) -> Item;
to_integer(Item, Options) -> force(Item, 'integer', Options).

to_bitstring(Item) when is_bitstring(Item) -> Item;
to_bitstring(Item) -> force(Item, 'bitstring').

to_bitstring(Item, _Options) when is_bitstring(Item) -> Item;
to_bitstring(Item, Options) -> force(Item, 'bitstring', Options).

to_float(Item) when is_float(Item) -> Item;
to_float(Item) -> force(Item, 'float').

to_float(Item, _Options) when is_float(Item) -> Item;
to_float(Item, Options) -> force(Item, 'float', Options).

to_pid(Item) when is_pid(Item) -> Item;
to_pid(Item) -> force(Item, 'pid').

to_pid(Item, _Options)  when is_pid(Item)-> Item;
to_pid(Item, Options) -> force(Item, 'pid', Options).

to_tuple(Item)  when is_tuple(Item)-> Item;
to_tuple(Item) -> force(Item, 'tuple').

to_tuple(Item, _Options)  when is_tuple(Item)-> Item;
to_tuple(Item, Options) -> force(Item, 'tuple', Options).
