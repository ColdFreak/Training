-module(tree).
-compile(export_all).


insert(Key, Val, {node, 'nil'}) -> % base case:the tree is empty
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}}; % insert function returns a node
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}}; % search from root
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) -> % ok, pattern matched
    {node, {Key, Val, Smaller, Larger}}.


% look up method to find a key
lookup( _, { node, 'nil'} ) ->

	undefined;

lookup( Key, { node, { Key, Val, _, _ } } ) ->

	{ ok, Val };

lookup( Key, { node, { NodeKey, _, Smaller, _ } } ) when Key < NodeKey ->
	
	lookup(Key, Smaller);

lookup( Key, { node, { _, _, _, Larger } } ) ->

	lookup(Key, Larger).
	



