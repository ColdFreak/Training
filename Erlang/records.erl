-module(records).
-compile(export_all).
-record(robot, {name, 
				type=industrial,
				hobbies,
				details=[]}).

% declare a record
first_robot() ->
	#robot{name = "Mechatron",
			 type = handmade,
			 details = ["Moved by a small man inside"]}.
	
car_factory(CorpName) ->
	#robot{name = CorpName,
			 hobbies = "building cars"}.
