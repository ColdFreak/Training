-module(exceptions).
-compile(export_all).

% exceptions:throws(fun() -> throw(thrown) end).
% {throw, caught, thrown}
throws(F) ->
	try F() of 
		_ -> ok % no comma 
	catch 
		Throw ->
			{throw, caught, Throw}
	end.

% exceptions:errors(fun() -> erlang:error("Die") end).
% {error, caught, "Die"}	
errors(F) ->
	try F() of 
		_ -> ok
	catch
		error:Error ->
			{error, caught, Error}
	end.

% exceptions:exits(fun() -> exit(goodbye) end). 
% {exit, caught, goodbye}
exits(F) ->
	try F() of
		_ -> ok
	catch 
		exit:Exit -> 
			{exit, caught, Exit}
	end.

sword(1) ->
	throw(slice);
sword(2) ->
	erlang:error(cut_arm);
sword(3) ->
	exit(cut_leg);
sword(4) ->
	throw(punch);
sword(5) ->
	exit(cross_bridge).

% call the black_knight function,
% then black_knight will evaluate the function Attack
% if function Attack is a normal like talk() and does not have any exception deal,
% that will match "None shall pass."
% else sword()'s exception will be evalucated
black_knight(Attack) when is_function(Attack, 0) ->
	try Attack() of
		_ -> "None shall pass."
	catch
		% if the function has a exception call like
		% exit, throw, error, than try to match it
		throw:slick -> "It is but a scratch.";
		error:cut_arm -> "I've had worse.";
		exit:cut_leg -> "Come on you pansy!";
		% the _:_ pattern is what you need to use to make sure to catch any exception of any type.
		_:_ -> "Just a flesh wound."
	end.

talk() -> 
	"blah blah".

whoa() ->
	try
		talk(),
		_Knight = "None shall Pass!",
		_Doubles = [N*2 || N <- lists:seq(1,100)],
		throw(up),
		_WillReturnThis = tequila
	of
		tequila -> "hey this worked!"
	catch
		Exception:Reason ->
			{caught, Exception, reason}
	end.

% exceptions:catcher(4,0).
% "uh oh" will be displayed.
% when you input catch 4/0 in the shell command
% the error will be matching {'EXIT', {badarith,_}}
catcher(X, Y) ->
	case catch X/Y of
		{'EXIT', {badarith,_}} -> "uh oh";
		N -> N
	end.
