-module(record_permit).
-compile(export_all).
-record(user, {id, name, group, age}).

% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
	Name ++ " is allowed!";
admin_panel(#user{name=Name}) -> % groupは提示しない場合not allowed
	Name ++ " is not allowed!".

adult_section( U = #user{} ) when U#user.age >= 18 ->
	allowed;

adult_section(_) -> % ageを教えて教えてくれないとforbidden
	forbidden.
%% 5> c(record_permit).
%% {ok,record_permit}
%% 6> rr(record_permit).  
%% [user]
%% 9> record_permit:adult_section(#user{id=22, name="Noah", group=users, age=13}).
%% forbidden

