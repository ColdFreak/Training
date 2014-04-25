// Somewhere within Node.js lives a module called "http", 
// and we can make use of it in our own code by requiring 
// it and assigning the result of the require to a local variable.
var http = require("http");

function onRequest(request, response) {

	console.log("request received.");
	response.writeHead(200, {"Content-Type":"text/plain"});
	response.write("Hello World");
	response.end();
}

http.createServer(onRequest).listen(8888);
console.log("Server has started.");
