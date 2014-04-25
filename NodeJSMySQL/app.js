var mysql = require('mysql');

var connection =  mysql.createConnection({
  	host : "localhost",
  	user : "root",
  	password: "wang123"
});

connection.connect();

connection.query("use nodejsmysql");
var strQuery = "select * from nodejs";

connection.query( strQuery, function(err, rows) {
	if(err) {
		throw err;
	}else {
		console.log(rows);
	}
});

connection.end(function(err) {
	console.log("connection destroyed");
});
