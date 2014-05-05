-module(tuples1).
-compile(export_all).
-record(person, {name, age, phone}).
-record(name, {first, surname}).

% use pattern matching 
birthday(#person{age=Age} = P) ->
	P#person{age=Age+1}.

joe() ->
	#person{name="Joe",
			age=21,
			phone="999-999"}.


showPerson(#person{name=Name, age=Age, phone=Phone}) ->
	io:format("name: ~p \n"
			  "Age: ~p \n"
			  "Phone: ~p",
			  [Name, Age, Phone]).

test1() ->
	showPerson(joe()).

test2() ->
	birthday(joe()).
